library(lubridate)
library(dplyr)
library(purrr)
library(cli)
library(tidyr)
library(tibble)

source(file.path('R', 'helpers-geekswhodrink.R'))
TIMESTAMP <- now()

scrape_and_bind_geekswhodrink_venue_quiz_results <- function(
    venue_id, 
    try_if_existing_has_zero_rows = FALSE, 
    try_if_recently_scraped = FALSE,
    recent_scrape_window = duration(1, unit = 'days')
  ) {
  res_existing <- safely_read_geekswhodrink_release(venue_id, tag = 'venue-quiz-results')
  
  release_file_exists <- TRUE
  existing_quiz_results <- res_existing$result
  max_page <- NULL
  if (!is.null(res_existing$error)) {
    cli_warn('No existing release file for {.var venue_id} = {.val {venue_id}} .')
    existing_quiz_results <- tibble()
    has_existing_quiz_results <- FALSE
    release_file_exists <- FALSE
  } else {
    n_quiz_results <- nrow(existing_quiz_results)
    has_existing_quiz_results <- nrow(existing_quiz_results) > 0
    ## Some issue with venue_id...
    cli_inform(c('i' = '{.var venue_id} = {.val {venue_id}} has {n_quiz_results} existing records.'))
    # cli_inform('Has {n_quiz_results} existing records.')
    ## Only try scraping the first quiz page for a venue where we've successfully scraped in the past.
    ##   If we're scraping at a weekly cadence, we won't "miss" any quiz results. And, even if we're scraping
    ##   at a monthly cadence, we should be ok, since venue pages, typically have at least the 5 most recent week's set of scores.
    if (isTRUE(has_existing_quiz_results)) {
      max_page <- 1
      
      ## Temporary fix for files where I forgot to include updated_at
      if (isFALSE(any(colnames(existing_quiz_results) == 'updated_at'))) {
        existing_quiz_results$updated_at <- TIMESTAMP
        write_geekswhodrink_release(
          existing_quiz_results,
          name = venue_id, 
          tag = 'venue-quiz-results'
        )
      }
    }
  }
  
  ## Don't continue to try to scrape if the venue_id is not completely new, didn't have past existing results, and has a release file.
  ##   We should consider setting try_if_existing_has_zero_rows = FALSE occasionally to check back on venues that originally didn't
  ##   have any records (e.g. a venue with "Coming soon!")
  if (isFALSE(has_existing_quiz_results) & isFALSE(try_if_existing_has_zero_rows) & isTRUE(release_file_exists)) {
    return(tibble())
  }
  
  ## Don't try scraping if we've already scraped recently
  if (isTRUE(has_existing_quiz_results) & isFALSE(try_if_recently_scraped)) {
    max_updated_at <- max(existing_quiz_results$updated_at)
    should_try_scraping <- (max_updated_at + recent_scrape_window) < TIMESTAMP
    if (isFALSE(should_try_scraping)) {
      cli_inform(c('i' = 'Returning early since it is within the scrape window.'))
      return(existing_quiz_results)
    }
  }
  
  res <- possibly_scrape_geekswhodrink_venue_quiz_results(venue_id, max_page = max_page)
  n_new_records <- nrow(res)
  cli_inform(c('i' =-'Got {n_new_records} new records for {.var venue_id} = {.val {venue_id}}.'))
  
  if (n_new_records == 0) {
    if (isFALSE(release_file_exists)) {
      cli_inform(c('i' =-'Writing to release file even since no release file existed before.'))
      write_geekswhodrink_release(
        res,
        name = venue_id, 
        tag = 'venue-quiz-results'
      )
      return(res)
    } else {
      return(existing_quiz_results)
    }
  }
  
  res$updated_at <- TIMESTAMP
  res <- bind_rows(
    existing_quiz_results,
    res
  ) |> 
    ## Only update the updated_at field for the new scores on the first page. We'll most likely
    ##   re-scrape results we've already scraped on the first page (past weeks). Keep the updated_at field the
    ##   same for those weeks.
    group_by(quiz_date) |> 
    slice_min(updated_at, n = 1, with_ties = TRUE) |> 
    ungroup()
  
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
      scrape_and_bind_geekswhodrink_venue_quiz_results(
        venues$venue_id[.x], 
        try_if_existing_has_zero_rows = FALSE
      )
    }
  )
