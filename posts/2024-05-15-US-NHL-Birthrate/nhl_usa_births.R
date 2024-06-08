#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list = ls(all = TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# cSpell:disable  disable spell checking for this document

## ---- Setup ----
library(magrittr)
library(ggplot2)
library(ggtext)
library(ggimage)

## ---- USA_Birth_Data ----
usa_raw  <- wonderapi::send_query("D66", here::here("posts", "2024-05-15-US-NHL-Birthrate", "cdc_wonder_request.xml"))

usa_births  <- usa_raw %>%
  dplyr::group_by(Month) %>%
  dplyr::summarise(country_births = sum(Births), .groups = "drop") %>%
  dplyr::mutate(country_pct = country_births / sum(country_births))

## ---- USA_Distro ----

usa_births %>%
  dplyr::mutate(expected_births = dplyr::case_when(
    Month %in% c("April", "June", "September", "November") ~ 30 / 365
    , Month == "February" ~ 28 / 365
    , .default  = 31 / 365
  )
  , difference = country_pct - expected_births
  , dplyr::across(Month, ~factor(., levels = month.name))
  , dplyr::across(c(country_pct, expected_births, difference), ~scales::percent(., accuracy = .1))
  ) %>%
  dplyr::arrange(Month) %>%
  dplyr::rename_with(~stringr::str_replace_all(., "_", " ")) %>%
  dplyr::rename_with(stringr::str_to_title) %>%
  kableExtra::kbl() %>%
  kableExtra::kable_styling()

## ---- Roster_Data ----

teams <- httr::GET("https://api.nhle.com/stats/rest/en/team") %>%
  httr::content() %>%
  .[["data"]] %>%
  tibble::tibble(data = .) %>%
  tidyr::unnest_wider(data)

get_roster  <- function(team){
  httr::GET(glue::glue("https://api-web.nhle.com/v1/roster/{team}/20232024")) %>%
    httr::content() %>%
    purrr::flatten() %>%
    tibble::tibble(data = .) %>%
    tidyr::hoist(
      .col = "data"
      , "firstName" = list("firstName", 1L)
      , "lastName" = list("lastName", 1L)
      , "positionCode"
      , "birthDate"
      , "birthCountry"
    )
}

usa_roster  <- purrr::map(teams$triCode, get_roster) %>%
  purrr::list_rbind() %>%
  dplyr::filter(!is.na(firstName)) %>%
  dplyr::filter(birthCountry == "USA") %>%
  dplyr::mutate(
    mob = lubridate::month(lubridate::ymd(birthDate), label = TRUE, abbr = FALSE)
    , mob_id = lubridate::month(lubridate::ymd(birthDate))
  ) %>%
  dplyr::count(mob_id, mob,  name = "players") %>%
  dplyr::mutate(player_pct = players / sum(players))

## ---- Graph_It ----

nhl_icon  <- "https://pbs.twimg.com/media/F9sTTAYakAAkRv6.png"
usa_icon  <- "https://cdn-icons-png.flaticon.com/512/197/197484.png"

combined  <- usa_roster %>%
  dplyr::left_join(usa_births, by = c("mob" = "Month")) %>%
  dplyr::mutate(
    random = dplyr::case_when(
      mob_id %in% c(4, 6, 9, 11) ~ 30 / 365,
      mob_id %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31 / 365,
      mob_id == 2 ~ 28 / 365
    )
  )

# labels  <- combined %>% glue::glue_data("{mob} <br> n = {players}")

g1  <- combined %>%
  ggplot(aes(x = forcats::fct_reorder(mob, -mob_id))) +
  geom_line(aes(y = random, group = 1), linetype = 2, color = "grey60") +
  geom_linerange(aes(ymin = country_pct, ymax = player_pct)) +
  geom_image(aes(image = nhl_icon, y = player_pct), size = 0.1) +
  geom_image(aes(image = usa_icon, y = country_pct), size = 0.08) +
  geom_text(aes(label = scales::percent(player_pct, accuracy = .1),
      y = dplyr::if_else(player_pct > country_pct, player_pct + .006, player_pct - .006)), size = 5) +
  geom_text(aes(label = scales::percent(country_pct, accuracy = .1),
      y = dplyr::if_else(country_pct > player_pct, country_pct + .006, country_pct - .006)), size = 5) +
  scale_y_continuous(labels = scales::percent) +
  # scale_x_discrete(labels = labels) +
  coord_flip() +
  labs(
    x = "Month of Birth"
    , y = "Percentage of Births"
    , title = "Are United States Born NHL Players More Likely to be Born Early in the Year?"
    , subtitle = "Comparing the distribution of birth months between US NHL players and US in general"
    , caption = glue::glue(
      "<img src = {nhl_icon} width = '15' height=' 15' /> - US NHL Players Birth Month Distribution <br />
      <img src = {usa_icon} width = '15' height=' 15' /> - US Birth Month (2007-2022) Distribution"
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_markdown()
    , plot.title.position = "plot"
    , text = element_text(size = 16)
    , axis.text = element_markdown()
  )


g1

# Stats ----

broom::tidy(chisq.test(x = combined$players, p = combined$country_pct))


## ---- All_Players ----

world_icon  <- "https://cdn-icons-png.flaticon.com/512/921/921490.png"

country_codes_url <- "https://raw.githubusercontent.com/datasets/country-codes/master/data/country-codes.csv"

world_birth_rates_csv  <- here::here("posts", "2024-05-15-US-NHL-Birthrate", "UNdata_Export_20240520_194600031.csv")

country_codes  <- readr::read_csv(country_codes_url) %>%
  dplyr::select(official_name_en, `ISO3166-1-Alpha-3`)

world_roster_raw  <- purrr::map(teams$triCode, get_roster) %>%
  purrr::list_rbind() %>%
  dplyr::filter(!is.na(firstName)) %>%
  dplyr::mutate(
    mob = lubridate::month(lubridate::ymd(birthDate), label = TRUE, abbr = FALSE)
    , mob_id = lubridate::month(lubridate::ymd(birthDate))
  )

codes_filter  <- world_roster_raw %>% dplyr::distinct(birthCountry) %>% dplyr::pull()

country_filter  <- country_codes %>%
  dplyr::filter(`ISO3166-1-Alpha-3` %in% codes_filter) %>%
  dplyr::pull(official_name_en)

world_births  <- readr::read_csv(world_birth_rates_csv, name_repair = janitor::make_clean_names) %>%
  dplyr::filter(country_or_area %in% country_filter) %>%
  dplyr::filter(month %in% month.name) %>%
  dplyr::filter(reliability == "Final figure, complete") %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(world_births = sum(value), .groups = "drop") %>%
  dplyr::mutate(world_pct = world_births / sum(world_births))

combined_world <- world_roster_raw %>%
  dplyr::count(mob_id, mob,  name = "players") %>%
  dplyr::mutate(player_pct = players / sum(players)) %>%
  dplyr::left_join(world_births, by = c("mob" = "month")) %>%
  dplyr::mutate(
    random = dplyr::case_when(
      mob_id %in% c(4, 6, 9, 11) ~ 30 / 365,
      mob_id %in% c(1, 3, 5, 7, 8, 10, 12) ~ 31 / 365,
      mob_id == 2 ~ 28 / 365
    )
  )

# ---- world_graph ----

g2  <- combined_world %>%
  ggplot(aes(x = forcats::fct_reorder(mob, -mob_id))) +
  geom_line(aes(y = random, group = 1), linetype = 2, color = "grey60") +
  geom_linerange(aes(ymin = world_pct, ymax = player_pct)) +
  geom_image(aes(image = world_icon, y = world_pct), size = 0.08) +
  geom_image(aes(image = nhl_icon, y = player_pct), size = 0.1) +
  geom_text(aes(label = scales::percent(player_pct, accuracy = .1),
      y = dplyr::if_else(player_pct > world_pct, player_pct + .006, player_pct - .006)), size = 5) +
  geom_text(aes(label = scales::percent(world_pct, accuracy = .1),
      y = dplyr::if_else(world_pct > player_pct, world_pct + .006, world_pct - .006)), size = 5) +
  scale_y_continuous(labels = scales::percent) +
  coord_flip() +
  labs(
    x = "Month of Birth"
    , y = "Percentage of Births"
    , title = "What Birthday is most liekly to make the NHL?"
    , subtitle = "Comparing the distribution of birth months between All NHL players and the World in general"
    , caption = glue::glue(
      "<img src = {nhl_icon} width = '15' height=' 15' /> - US NHL Players Birth Month Distribution <br />
      <img src = {world_icon} width = '15' height=' 15' /> - World Birth Month (2007-2022) Distribution"
    )
  ) +
  theme_minimal() +
  theme(
    plot.caption = element_markdown()
    , plot.title.position = "plot"
    , text = element_text(size = 16)
    , axis.text = element_markdown()
  )

g2

broom::tidy(chisq.test(x = combined_world$players, p = combined_world$world_pct))
