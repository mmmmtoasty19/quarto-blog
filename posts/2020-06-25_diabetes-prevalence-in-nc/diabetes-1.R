#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- load-packages --------------------------------------------------
# Attach these packages so their functions don't need to be qualified: http://r-pkgs.had.co.nz/namespace.html#search-path
library(magrittr) # enables piping : %>%
library(dplyr)    # data wrangling
library(ggplot2)  # graphs
library(tidyr)    # data tidying
library(maps)
library(mapdata)
library(sf)
library(readr)


# ---- load-sources ---------------------------------------------------



# ---- declare-globals ----------------------------------------------------

#set ggplot theme
ggplot2::theme_set(theme_bw())

# ---- load-data ------------------------------------------------------

# load the data, and have all column names in lowercase

nc_diabetes_data_raw <- read_csv("https://raw.githubusercontent.com/mmmmtoasty19/nc-diabetes-epidemic-2020/62bdaa6971fbdff09214de7c013d40122abbe40d/data-public/derived/nc-diabetes-data.csv") %>% 
  rename_all(tolower)

us_diabetes_data_raw <- read_csv("https://github.com/mmmmtoasty19/nc-diabetes-epidemic-2020/raw/62bdaa6971fbdff09214de7c013d40122abbe40d/data-public/raw/us_diabetes_totals.csv"
                                 ,skip = 2)

rural_counties <- read_csv("https://github.com/mmmmtoasty19/nc-diabetes-epidemic-2020/raw/b29bfd93b20b73a7000d349cb3b55fd0822afe76/data-public/metadata/rural-counties.csv")

county_centers_raw <- read_csv("https://github.com/mmmmtoasty19/nc-diabetes-epidemic-2020/raw/b29bfd93b20b73a7000d349cb3b55fd0822afe76/data-public/raw/nc_county_centers.csv", col_names = c("county", "lat","long"))

diabetes_atlas_data_raw <- read_csv("https://raw.githubusercontent.com/mmmmtoasty19/nc-diabetes-epidemic-2020/b29bfd93b20b73a7000d349cb3b55fd0822afe76/data-public/raw/DiabetesAtlasData.csv"
                                    ,col_types = cols(LowerLimit = col_skip(), 
                                                     UpperLimit = col_skip(),
                                                     Percentage = col_double()), skip = 2)




# ---- load-map-data ----------------------------------------------------------

# load in both US State Map and NC County Map

nc_counties_map_raw <- st_as_sf(map("county",region = "north carolina", plot = FALSE,fill = TRUE)) %>% 
  mutate_at("ID", ~stringr::str_remove(.,"north carolina,"))

state_map_raw <- st_as_sf(map("state",plot = FALSE,fill = TRUE ))

nc_cities <-  st_as_sf(read_csv("https://github.com/mmmmtoasty19/nc-diabetes-epidemic-2020/raw/b29bfd93b20b73a7000d349cb3b55fd0822afe76/data-public/metadata/nc_cities.csv"),
                       coords = c("long", "lat")
                       ,remove = FALSE
                       ,agr = "constant"
                       ,crs = 4326)




# ---- tweak-data --------------------------------------------------------------

county_centers <- county_centers_raw %>% 
  mutate_all(~stringr::str_replace_all(.,
                                       c("\\°"  = ""
                                         ,"\\+" = ""
                                         ,"\\–" = "-"
                                       )
  ) 
  ) %>%
  mutate(across(c("lat","long"), ~iconv(.,from = 'UTF-8', to = 'ASCII//TRANSLIT'))
         ,across(c("lat","long"),~stringr::str_remove_all(.,"\\?"))) %>% 
  mutate_at(c("lat","long"),as.numeric) %>%
  mutate(across("long", ~(. * -1))) %>% 
  mutate_at("county", tolower)




us_diabetes_data <- us_diabetes_data_raw %>% 
  filter(Year >= 2000) %>% 
  select( "Year","Total - Percentage") %>% 
  rename(year = Year , us_pct = `Total - Percentage`)

diabetes_atlas_data <- diabetes_atlas_data_raw %>% 
  mutate_at("State", tolower) %>% 
  filter(Year >= 2000)

state_map_abb <- state_map_raw %>% 
  left_join(read_csv("https://github.com/mmmmtoasty19/nc-diabetes-epidemic-2020/raw/b29bfd93b20b73a7000d349cb3b55fd0822afe76/data-public/metadata/state-abb.csv") %>% 
              mutate_at("state", tolower)
            ,by = c("ID" = "state") )



# ---- merge-data ---------------------------------------------------------

#join US totals to NC data 

nc_diabetes_data <- nc_diabetes_data_raw %>% 
  mutate_at("county", ~stringr::str_replace_all(.,"Mcdowell","McDowell")) %>% 
  mutate(
    rural = county %in% rural_counties$rural_counties
  ) %>% 
  mutate_at("county",tolower) %>% 
  left_join(us_diabetes_data)


nc_counties_map <- nc_counties_map_raw %>% 
  left_join(nc_diabetes_data, by = c("ID" = "county")) %>% 
  left_join(county_centers, by = c("ID" = "county")) %>% 
  rename(
    center_long = long
    ,center_lat = lat)

state_map <- state_map_abb %>% 
  left_join(diabetes_atlas_data, by = c("ID" = "State")) %>% 
  rename_all(tolower)



# ---- o-g1 ------------------------------------------------------------------


us_diabetes_data <- us_diabetes_data %>% 
  mutate(
    change = lead(us_pct) - us_pct
    ,change = if_else(change > 0, TRUE, FALSE)
  ) %>% 
  mutate_at("change", ~stringr::str_replace_na(.,"NA"))



o_g1 <- us_diabetes_data %>% 
  ggplot(aes(x = year, y = us_pct)) +
  geom_line(color= "#D95F02") +
  # geom_line(aes(color = change, group = 1)) +
  geom_point(shape = 21, size = 3,color= "#D95F02") +
  # geom_point(aes(color = change),shape = 21, size = 3) +
  scale_color_manual(values = c(
    "TRUE" = "#D95F02"
    ,"FALSE" = "#7570B3"
  ), guide = FALSE) +
  labs(
    title    = "Percentage of Diagnosed Diabetes in Adults (18+), National Level"
    ,x       = NULL
    ,y       = NULL
    ,caption = "Note: Data from the CDC's National Health Interview Survey (NHIS)"
  )

o_g1



# ---- s-g1 -----------------------------------------------------------------


s_g1 <- state_map %>% 
  st_drop_geometry() %>% 
  ggplot(aes(x = year, y = percentage, color = region)) +
  geom_line(aes(group = id ),alpha = 0.3,na.rm = TRUE) +
  geom_smooth(method = "lm", se = FALSE) +
  ggpmisc::stat_poly_eq(formula = y ~ + x ,
                        aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
                        parse = TRUE) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  scale_color_brewer(palette    = "Dark2"
                     ,direction = -1
                     ,labels    = snakecase::to_title_case
  ) +
  labs(
    title    = "Percentage of Diagnosed Diabetes in Adults (18+) \nby State and Region"
    ,x       = NULL
    ,y       = NULL
    ,color   = "Region"
    ,caption = "Regions from US Census Bureau"
  ) 

s_g1




# ---- s-g2 ---------------------------------------------------------------

s_g2 <- state_map %>% 
  st_drop_geometry() %>% 
  filter(region == "south") %>% 
  mutate_at("id", ~snakecase::to_title_case(.)) %>% 
  ggplot(aes(x = year, y = percentage)) +
  geom_line(aes(group = id ),na.rm = TRUE, color= "#D95F02") +
  gghighlight::gghighlight(id == "North Carolina", label_params = list(vjust = 3)) +
  scale_y_continuous(breaks = seq(5,13,2)) +
  scale_x_continuous(minor_breaks = seq(2000,2016,1)) +
  labs(
    title    = "Percentage of Diagnosed Diabetes in Adults (18+) \nSouth Region"
    ,x       = NULL
    ,y       = NULL
    ,caption = "Regions from US Census Bureau"
  ) +
  theme()

s_g2



# ---- nc-g1 ----------------------------------------------------------------------

d1 <- nc_diabetes_data %>% 
  group_by(year) %>% 
  summarise(
    pct = mean(percentage)
    ,us_pct = mean(us_pct)
  ) %>% 
  pivot_longer(
    cols       = c("pct", "us_pct")
    ,names_to  = "metric"
    ,values_to = "values"
  ) %>% 
  mutate(
    metric = factor(metric
                    ,levels = c("pct","us_pct")
                    ,labels = c("NC", "National"))
  )

nc_g1 <- d1 %>% 
  ggplot(aes(x = year, y = values, color = metric)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x      = NULL
    ,y     = NULL
    ,color = NULL
    ,title = "Percent of Adults (20+) with Diagnosed Diabetes"
  )

nc_g1 

# ---- nc-data-aberration ---------------------------------------------------

nc_g1a <- nc_diabetes_data %>% 
  ggplot(aes(x = year, y = percentage)) +
  geom_line(aes(group = county),alpha = 0.4) +
  labs(
    x = NULL
    ,y = NULL
    ,color = NULL
  )

nc_g1a


# ---- nc-g2 -----------------------------------------------------------------

d2 <- nc_diabetes_data %>% 
  select(-us_pct) %>% 
  mutate(
    pct_rural  = if_else(rural == TRUE, percentage, FALSE)
    ,pct_urban = if_else(rural == FALSE, percentage, FALSE)
  ) %>% 
  select(-countyfips,-percentage) %>% 
  group_by(year) %>% 
  summarise(
    pct_rural = mean(pct_rural,na.rm = TRUE)
    ,pct_urban = mean(pct_urban,na.rm = TRUE)
  ) %>% left_join(us_diabetes_data) %>% 
  pivot_longer(
    cols       = c("us_pct", "pct_rural","pct_urban")
    ,names_to  = "metric"
    ,values_to = "value"
    ,values_drop_na = TRUE
  ) %>% 
  mutate(
    metric = factor(metric,
                    levels  = c("pct_rural","pct_urban","us_pct")
                    ,labels = c("Rural","Urban","US")
    )
  )

nc_g2 <- d2 %>% ggplot(aes(x = year, y = value, color = metric)) +
  geom_line() +
  geom_point(shape = 21, size = 3) +
  geom_vline(xintercept = 2011, linetype = "dashed", color = "gray") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    x      = NULL
    ,y     = NULL
    ,color = NULL
    ,title = "Percent of Adults (20+) with Diagnosed Diabetes \nDisplaying Rural vs Urban"
  )

nc_g2

# ---- spaghetti-plot ----------------------------------------------------

g50 <- nc_diabetes_data %>% 
  filter(year < 2015) %>% 
  mutate(
    rural = factor(rural
                   ,levels = c(TRUE,FALSE)
                   ,labels = c("Rural", "Urban")
    )
  ) %>% 
  ggplot(aes(x = year, y = percentage, color = rural)) +
  geom_line(aes(group = county),alpha = 0.3) +
  geom_smooth(aes(group = rural), method = "loess", se= FALSE, size = 1.1) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Percent of Adults (20+) with Diagnosed Diabetes \nAll North Carolina Counties"
    ,x = NULL
    ,y = NULL
    ,color = NULL
  )

g50


# ---- c-g1 --------------------------------------------------------------


nc_counties_map_binned <- nc_counties_map %>% 
  filter(year < 2015) %>% 
  mutate(
    bin = dlookr::binning(.$percentage, nbins = 6 ,type = "equal")
    ,bin = forcats::fct_recode(bin
                               ,"6.5   - 7.9"  =  "[6.5,7.97]"
                               ,"8.0  - 9.4" =  "(7.97,9.43]" 
                               ,"9.5  - 10.9" =  "(9.43,10.9]" 
                               ,"11.0 - 12.4" =  "(10.9,12.4]"
                               ,"12.5 - 13.8" =  "(12.4,13.8]"  
                               ,"13.9 - 15.3" =  "(13.8,15.3]"
    )
  )

c_g1 <- nc_counties_map_binned %>% 
  filter(year %in% c(2006,2014)) %>% 
  ggplot() +
  geom_sf() + #blank geom_sf keeps gridlines from overlapping map
  geom_sf(aes(fill = bin,color = rural)) +
  geom_sf(data = nc_cities) +
  ggrepel::geom_text_repel(data = nc_cities, 
                           aes(x = long, y = lat, label = city)
                           ,nudge_y = c(-1,1,1,-1,1)
                           ,nudge_x = c(0,0,0,-1,0)
  ) +
  geom_text(data = . %>% filter(rural == TRUE)
            ,aes(x = center_long, y = center_lat)
            ,label = "R"
            ,color = "#696969"
  ) +
  coord_sf(xlim = c(-84.5,-75.5), ylim = c(33.75,37)) +
  facet_wrap(~year) +
  scale_fill_viridis_d(alpha = 0.6, direction = -1) +
  scale_color_manual(
    values = c(
      "FALSE" = "gray"
      ,"TRUE" = "black"
    ),guide = 'none') +
  labs(
    title = "Estimated Diabetes in Adults (20+) by County"
    ,fill = "Percentage"
    ,y    = NULL
    ,x    = NULL
  ) +
  theme(
    panel.background = element_rect(fill = "aliceblue")
    ,panel.grid.major = element_line(color = "#D4D4D4", linetype = "dashed", 
                                     size = 0.5)
    ,legend.position = "bottom"
    ,plot.title = element_text(hjust = 0.5)
  )

c_g1

# ---- county-distribution-histogram ------------------------------------

# Not USED
c_g1a <- nc_counties_map_binned %>% 
  mutate(
    rural = factor(rural
                   ,levels = c(TRUE,FALSE)
                   ,labels = c("Rural", "Urban")
    )
  ) %>% 
  filter(year %in% c(2006,2014)) %>% 
  ggplot(aes(x = bin, fill = rural)) +
  geom_bar(stat = "count"
           ,position = "dodge"
  ) +
  geom_text(aes(label=..count..)
            ,position = position_dodge(width = 1)
            ,stat = "count"
            ,vjust = -0.1
            ,size = 5) +
  facet_wrap(~year) +
  scale_fill_brewer(palette = "Dark2") +
  labs(
    fill = NULL
    ,x   = NULL
    ,y   = NULL 
  )

c_g1a

# ---- county-boxplot ----

c_g1c <- nc_counties_map %>% 
  mutate(
    rural = factor(rural
                   ,levels = c(TRUE,FALSE)
                   ,labels = c("Rural", "Urban")
    )) %>% 
  filter(year < 2015) %>%
  ggplot(aes(x = year, y = percentage, group = interaction(year,rural), fill = rural)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks = seq(2004,2014,2)) +
  labs(
    x      = NULL
    ,y     = NULL
    ,fill  = NULL
    ,title = "Distribution  of Estimated Cases by County 2006 - 2014"
  )

c_g1c




# ---- c-g4 ---------------------------------------------------------------
d3 <- nc_counties_map %>% 
  st_drop_geometry() %>% 
  filter(year %in% c(2006,2014)) %>% 
  select(-countyfips,-us_pct) %>% 
  pivot_wider(names_from = "year"
              ,values_from = "percentage") %>% 
  mutate(
    pct_p  = `2014` - `2006`
    ,pct_c = ((`2014` - `2006`)/`2006`) * 100
  ) %>% 
  left_join(nc_counties_map_raw) %>% 
  st_as_sf()


c_g4 <- d3 %>% 
  ggplot() +
  geom_sf() + #blank geom_sf keeps gridlines from overlapping map
  geom_sf(aes(fill = pct_c ,color = rural)) +
  geom_sf(data = nc_cities) +
  ggrepel::geom_text_repel(data = nc_cities, 
                           aes(x = long, y = lat, label = city)
                           ,nudge_y = c(-1,1,1,-1,1)
                           ,nudge_x = c(0,0,0,-1,0)
  ) +
  geom_text(data = . %>% filter(rural == TRUE)
            ,aes(x = center_long, y = center_lat)
            ,label = "R"
            ,color = "#696969"
  ) +
  # scale_fill_viridis_c(alpha = 0.6, direction = -1) +
  scale_fill_gradient2(
    low = "#d01c8b"
    ,mid = "#f7f7f7"
    ,high = "#4dac26"
    ,midpoint = 0
  ) +
  scale_color_manual(
    values = c(
      "FALSE" = "gray"
      ,"TRUE" = "black"
    ),guide = 'none') +
  labs(
    title = "Percentage Change of Diagnosed Diabetes 2006-2014"
    ,fill = "Percentage"
    ,y    = NULL
    ,x    = NULL
  ) +
  theme(
    panel.background = element_rect(fill = "aliceblue")
    ,panel.grid.major = element_line(color = "#D4D4D4", linetype = "dashed", 
                                     size = 0.5)
  )

c_g4


# ---- pct_p-histogram ----------------------------------------------------------




d4 <- d3 %>% 
  st_drop_geometry() %>% 
  mutate(
    rural = factor(rural
                   ,levels = c(TRUE,FALSE)
                   ,labels = c("Rural", "Urban")
    )
  )


mean_d4 <- d4 %>% 
  group_by(rural) %>% 
  summarise(.groups = "keep"
            ,pct_c = mean(pct_c)
  )

g51 <-  d4 %>% 
  ggplot(aes(x = pct_c, fill = rural, y = ..density.., color = rural)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.3) +
  geom_density(alpha = 0.5) +
  facet_wrap(~rural, ncol = 1)  +
  geom_vline(aes(xintercept = pct_c), data = mean_d4) +
  geom_text(aes(x = pct_c, y = 0.038, label = round(pct_c, 2))
            ,data  = mean_d4
            ,hjust = -0.15
            ,size  = 5
            ,color = "#000000") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#696969") +
  scale_color_brewer(palette = "Dark2", guide = NULL) +
  scale_fill_brewer(palette = "Dark2", guide = NULL) +
  labs(
    x = "Percentage Change"
    ,y = "Density"
    ,fill = NULL
  )
g51





