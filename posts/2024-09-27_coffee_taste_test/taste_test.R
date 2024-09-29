# ---- Clean Environment ----
rm(list = ls())

# Load Packages ----

box::use(
  readr[read_csv],
)


# Load Data ----

url <- "https://bit.ly/gacttCSV"
ds_raw <- read_csv(url)

# Count NAs ----

nrow(ds_raw)

na_count <- ds_raw |> 
  dplyr::mutate(num_na = rowSums(is.na(ds))) |> 
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

# Clean Data ----


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



