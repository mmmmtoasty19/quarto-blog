#These first few lines run only when the file is run in RStudio, !!NOT when an Rmd/Rnw file calls it!!
rm(list=ls(all=TRUE))  #Clear the variables from previous runs.
cat("\f") # clear console

# ---- example-data ----

library(readxl)                  # load the readxl library
library(tidyverse)               # load the tidyverse for manipulating the data
file_path <- "example_data.xlsx" # set the file path
ds0 <- read_excel(file_path)     # read the file
ds0


# ---- fix-it-data ----

ds1 <- read_excel(file_path, n_max = 2, col_names = FALSE)
ds1
# ---- fix-it-names ----

names <- ds1 %>%
  t() %>%       #transpose to a matrix
  as_tibble()   #back to tibble
names

# ---- fix-it-names-1 ----

names <- names %>% fill(V1)  #use dplyr fill to fill in the NA's
names
# ---- fix-it-names-2 ----

names <- names %>%
  mutate(
    new_names = paste(V1,V2, sep = "_")
  )
names

# ---- fix-it-names-3 ----
names <- names %>% mutate(across(new_names, ~str_remove_all(.,"_NA")))
names
# ---- fix-it-names-4 ----
names <-  names %>% pull(new_names)
# ---- fix-it-final ----

example_data <- readxl::read_excel(file_path, col_names = names, skip = 2) %>%
  janitor::clean_names()
example_data

