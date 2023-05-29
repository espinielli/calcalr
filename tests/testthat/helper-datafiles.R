library(readr)
library(stringr)
library(zoo)

datafile_col_names <- function(filename) {
  l1 <- read_csv(test_path("testdata", filename),
                 n_max = 0,
                 show_col_types = FALSE,
                 name_repair = "unique_quiet") |>
    colnames() |>
    str_replace("\\.\\.\\.\\d+", NA_character_) |>
    na.locf() |>
    tolower()

  l2 <- read_csv(test_path("testdata", filename),
                 skip = 1,
                 n_max = 0,
                 show_col_types = FALSE,
                 name_repair = "unique_quiet") |>
    colnames() |>
    str_replace("\\.\\.\\.\\d+", "") |>
    tolower()

  paste0(l1, ifelse(l2 == "", "", "_"), l2)
}

read_test_data <- function(data_filename) {
  col_names = datafile_col_names(data_filename)
  read_csv(test_path("testdata", data_filename),
           col_names = col_names,
           skip = 2,
           show_col_types = FALSE)
}
