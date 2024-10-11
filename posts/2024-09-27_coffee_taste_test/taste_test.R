# ---- Clean Environment ----
rm(list = ls())

# ---- Load Packages ----

box::use(
  readr[read_csv],
  rlang[`!!`, sym]
)

# ---- Load Data ----

url <- "https://bit.ly/gacttCSV"
ds_raw <- read_csv(url)

# ---- Count NAs ----

nrow(ds_raw)

na_count <- ds_raw |> 
  dplyr::mutate(num_na = rowSums(is.na(ds_raw))) |> 
  dplyr::summarise(
    n = dplyr::n(),
    .by = num_na
  )

# don't show this in blog but note that I did it
ds_raw |> 
  dplyr::summarise(dplyr::across(dplyr::everything(), ~ sum(is.na(.)), .names = "{.col}")) |> 
  tidyr::pivot_longer(tidyr::everything()) |> 
  dplyr::arrange(desc(value)) |> 
  print(n = 50)

# --- Clean Data ----
#' Remove columns with a lot of NA's, selected only columns to allow for a pretty complete data set
#' At the end of selection, drop NA's so that all rows are fully complete.  This will avoid having to infer any data for ML 

ds <- ds_raw |>
  dplyr::select(
    ID = `Submission ID`,
    age = `What is your age?`,
    cups = `How many cups of coffee do you typically drink per day?`,
    where_drink = `Where do you typically drink coffee?`,
    brew_method = `How do you brew coffee at home?`,
    favorite = `What is your favorite coffee drink?`,
    additions = `Do you usually add anything to your coffee?`,
    style = `Before today's tasting, which of the following best described what kind of coffee you like?`,
    strength = `How strong do you like your coffee?`,
    roast_level = `What roast level of coffee do you prefer?`,
    why_drink = `Why do you drink coffee?`,
    taste = `Do you like the taste of coffee?`,
    gender = Gender,
    education_level = `Education Level`,
    ethnicity = `Ethnicity/Race`,
    employment = `Employment Status`,
    political_view = `Political Affiliation`   
  ) |> 
  tidyr::drop_na()

skimr::skim(ds)

# ---- Split Multi choice Columns ---- 

#' Function to split columns that contain multiple choice answers all in one column
#' result of function is each column contains original column name, followed by the answer choice
#' 1 is True, 0 False for the original column containing that choice

multi_choice_split <- function(ds, col) {
  col <- sym(col)

  ds |> 
    dplyr::select(ID, !!col) |> 
    tidyr::separate_longer_delim(!!col, delim = ",") |> 
    dplyr::mutate(dplyr::across(!!col, ~stringr::str_trim(., side = "both"))) |> 
    dplyr::mutate(dplyr::across(!!col, snakecase::to_snake_case)) |> 
    dplyr::group_by(ID, !!col) |> 
    dplyr::summarise(n = dplyr::n()) |> 
    dplyr::ungroup() |> 
    tidyr::pivot_wider(names_from = !!col, values_from = n, names_prefix = glue::glue("{col}_")) |> 
    {\(.) {replace(.,is.na(.),0)}}()  #anonymous function to use native pipe 

}

#' purr map, maps over each column choice returning a data frame for that col, map returns a list of data frames
#' the use of purrr reduce takes the list, and left joins each data frame by the ID col.  

cols_to_split <- c("where_drink", "brew_method", "additions", "why_drink")

ds_ml <- purrr::map(cols_to_split, \(x) multi_choice_split(ds, x)) |> 
  purrr::reduce(dplyr::left_join, by = "ID") |> 
  dplyr::left_join(ds, by = "ID") |> 
  dplyr::select(-dplyr::any_of(cols_to_split)) |> 
  dplyr::mutate(dplyr::across(dplyr::where(is.character) & !ID, as.factor))  # note use of ! to not select the ID column


# ---- Data Summaries ----
#need to expand this section to better explore cleaned data
summarytools::freq(ds_ml) 


# ---- 
