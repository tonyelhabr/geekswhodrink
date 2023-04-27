library(lubridate)
library(cli)
library(dplyr)
library(readr)
library(purrr)

source(file.path('R', 'helpers-geekswhodrink.R'))
RAW_DATA_DIR <- file.path('data', 'raw')
FINAL_DATA_DIR <- file.path('data', 'final')
dir.create(RAW_DATA_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(FINAL_DATA_DIR, showWarnings = FALSE)

TIMESTAMP <- now()
scrape_and_cache_geekswhodrink_venue_quiz_results <- function(venue_id, overwrite = FALSE) {
  path <- file.path(RAW_DATA_DIR, paste0(venue_id, '.csv'))
  if (file.exists(path) & isFALSE(overwrite)) {
    cli_inform('Reading in pre-saved results for {.var venue_id} = {.val {venue_id}}.')
    return(read_csv(path, show_col_types = FALSE))
  }
  res <- possibly_scrape_geekswhodrink_venue_quiz_results(venue_id) |> 
    mutate(
      venue_id = venue_id,
      updated_at = !!TIMESTAMP,
      .before = 1
    )
  write_csv(res, path, na = '')
  res
}

venues <- readr_geekswhodrink_release('venues')
quiz_results <- map(
  venues$venue_id,
  scrape_and_cache_geekswhodrink_venue_quiz_results
)

quiz_results |> 
  discard(~nrow(.x) == 0) |> 
  bind_rows() |> 
  transmute(
    venue_id,
    across(quiz_date, ~mdy(quiz_date)),
    placing = `Place Ranking`,
    team = `Team Name`,
    score = `Score`
  ) |> 
  write_geekswhodrink_release('quiz-results')
