library(lubridate)
library(dplyr)
library(readr)
library(purrr)
library(piggyback)
library(cli)
library(stringr)

getwd()
setwd('data/raw')
source('../../R/helpers-geekswhodrink.R')
existing_quiz_result_paths <- list.files(
  '.',
  full.names = FALSE
)

TIMESTAMP <- now()
# pb_new_release(repo = 'tonyelhabr/geekswhodrink', tag = 'venue-quiz-results')
1:length(existing_quiz_result_paths) |>
  iwalk(
    ~{
      path <- existing_quiz_result_paths[.x]
      raw <- read_csv(path, show_col_types = FALSE)
      if (nrow(raw) > 0) {
        raw <- clean_geekswhodrink_quiz_results(raw)
        raw$updated_at <- TIMESTAMP
      }
      cli_inform('Uploading {.x} of {length(existing_quiz_result_paths)} files.')
      write_geekswhodrink_release(
        raw,
        str_remove(path, '\\.csv'),
        tag = 'venue-quiz-results'
      )
    }
  )
