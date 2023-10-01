library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(piggyback)
library(cli)
library(stringr)

getwd()
setwd('../yelp-business-info')
source('../../R/helpers-geekswhodrink.R')
existing_paths <- list.files(
  '.',
  full.names = FALSE
)

TIMESTAMP <- now()
pb_new_release(repo = 'tonyelhabr/geekswhodrink', tag = 'yelp-business-info')
n_paths <- length(existing_paths)
1:n_paths |>
  iwalk(
    ~{
      path <- existing_paths[.x]
      raw <- read_csv(path, show_col_types = FALSE)
      if (nrow(raw) > 0) {
        raw$updated_at <- TIMESTAMP
      }
      cli_inform('Uploading {.x} of {n_paths} files.')
      write_geekswhodrink_release_csv(
        raw,
        str_remove(path, '\\.csv'),
        tag = 'yelp-business-info'
      )
    }
  )
