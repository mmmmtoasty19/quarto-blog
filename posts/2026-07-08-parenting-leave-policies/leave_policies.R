# ---- Clean Environment ----
rm(list = ls())

# Read Data --------------------------------------------------------------

# ds_EU_raw <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2026/2026-06-02/eplp.csv')

ds_US_names <- readxl::read_excel('posts/2026-07-08-parenting-leave-policies/us_policies.xlsx', n_max = 2, col_names = FALSE) |> 
  t() |> 
  tibble::as_tibble(.name_repair = "unique_quiet") |> 
  tidyr::fill("...1") |> 
  tidyr::unite(col = "new_names", "...1", "...2", sep = "_") |> 
  dplyr::pull() |> 
  stringr::str_remove_all("_NA") |>
  janitor::make_clean_names()
  
ds_US_raw <- readxl::read_excel('posts/2026-07-08-parenting-leave-policies/us_policies.xlsx', col_names = ds_US_names, skip = 2)


# US Data Manipulation -------------------------------------------------------

# US data source https://bipartisanpolicy.org/explainer/state-paid-family-leave-laws-across-the-u-s/

ds_US <- ds_US_raw |> 
  dplyr::select(-enacted, -small_business_carveout, -dplyr::starts_with("premium"),
    -wage_replacement_rate, -eligibility_requirement, -max_weekly_benefit
  ) |> 
  dplyr::mutate(
    effective = stringr::str_extract(effective, "\\d{4}") |> as.integer(),
    dplyr::across(dplyr::starts_with("leave_duration"), ~ stringr::str_extract(., "\\d+") |> as.integer()),
    job_protection_notes = dplyr::if_else(
      stringr::str_detect(job_protection, ","), 
      stringr::str_trim(stringr::str_remove(job_protection, "^[^,]+,")),
      NA_character_
    ),
    job_protection = dplyr::if_else(
      stringr::str_starts(job_protection, "Yes"),
      "Yes",
      "No"
    )
  ) |> 
  dplyr::rename_with(
    ~paste0(.x, "_weeks"),
    dplyr::starts_with("leave_duration")
  )



# EU Data Manipulation ---------------------------------------------------

# EU Source https://eplp-dataset.org/




# US Graphs --------------------------------------------------------------

library(ggplot2)

# Cumulative number of paid leave policies in the US over time
ds_US |> 
  dplyr::count(effective) |> 
  dplyr::arrange(effective) |> 
  dplyr::mutate(cumulative_policies = cumsum(n)) |>
  ggplot(aes(x = effective, y = cumulative_policies)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Paid Leave Policies in the United States Over Time",
    x = "Year Effective",
    y = "Cumulative Policies"
  ) +
  theme_minimal()

# Cumulative number of paid leave policies in the US over time by job protection

ds_US |> 
  dplyr::count(effective, job_protection) |> 
  dplyr::arrange(effective) |> 
  dplyr::group_by(job_protection) |> 
  dplyr::mutate(cumulative_policies = cumsum(n)) |>
  ggplot(aes(x = effective, y = cumulative_policies, color = job_protection)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Paid Leave Policies in the United States Over Time by Job Protection",
    x = "Year Effective",
    y = "Cumulative Policies",
    color = "Job Protection"
  ) +
  theme_minimal()

# Bar Chart of Paid Leave Duration 

ds_US |> 
  dplyr::count(
    leave_duration_parental_weeks = dplyr::coalesce(
      as.character(leave_duration_parental_weeks),
      "Set by insurer"
    )
  ) |>
  dplyr::mutate(
    leave_duration_parental_weeks = factor(
      leave_duration_parental_weeks,
      levels = c("2", "6", "8", "12", "Set by insurer")
    )
  ) |> 
  ggplot(aes(x = factor(leave_duration_parental_weeks), y = n)) +
  geom_col() +
  scale_y_continuous(
    n.breaks = 15
  ) +
  labs(
    title = "Distribution of Paid Leave Duration in the United States",
    x = "Leave Duration (Weeks)",
    y = "Number of Policies"
  ) +
  theme_minimal()

