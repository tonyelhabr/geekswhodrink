library(lubridate)
library(dplyr)
library(purrr)
library(cli)
library(tidyr)
library(tibble)

source(file.path('R', 'helpers-geekswhodrink.R'))
TIMESTAMP <- now()

scrape_and_bind_geekswhodrink_venue_quiz_results <- function(venue_id) {
  existing_quiz_results <- possibly_read_geekswhodrink_release(venue_id, tag = 'venue-quiz-results')
  n_quiz_results <- nrow(existing_quiz_results) 
  has_existing_quiz_results <- nrow(existing_quiz_results) > 0
  # cli_inform('{.var venue_id} = {.val {venue_id}} has {n_quiz_results} existing records.')
  cli_inform('Has {n_quiz_results} existing records.')
  max_page <- NULL
  if (isTRUE(has_existing_quiz_results)) {
    max_page <- 1
  }
  res <- possibly_scrape_geekswhodrink_venue_quiz_results(venue_id, max_page = max_page)
  # cli_inform('Got {nrow(res)} new records for {.var venue_id} = {.val {venue_id}}.')
  cli_inform('Got {nrow(res)} new records.')
  if (isTRUE(has_existing_quiz_results)) {
    res$updated_at <- TIMESTAMP
    res <- bind_rows(
      existing_quiz_results,
      res
    ) |> 
      group_by(quiz_date) |> 
      slice_max(updated_at, n = 1, with_ties = TRUE) |> 
      ungroup()
  }
  
  write_geekswhodrink_release(
    res,
    name = venue_id, 
    tag = 'venue-quiz-results'
  )
}

venues <- read_geekswhodrink_release('venues') |> arrange(venue_id)
cli_inform('Scraping quiz results from locations with prior quiz results.')
n_venues <- length(venues$venue_id)
new_quiz_results <- 1:n_venues |> 
  imap_dfr(
    ~{
      cli_inform('Scraping {.x} of {n_venues} previously scraped venues.')
      scrape_and_bind_geekswhodrink_venue_quiz_results(venues$venue_id[.x])
    }
  )
