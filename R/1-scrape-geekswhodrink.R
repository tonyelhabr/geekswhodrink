library(lubridate)
library(dplyr)
library(purrr)
library(cli)
library(tidyr)
library(tibble)

source(file.path('R', 'helpers-geekswhodrink.R'))
TIMESTAMP <- now()

scrape_and_bind_geekswhodrink_venue_quiz_results <- function(venue_id, existing_quiz_results = tibble(), max_page = NULL) {
  res <- possibly_scrape_geekswhodrink_venue_quiz_results(venue_id, max_page = max_page) |> 
    mutate(
      venue_id = venue_id,
      updated_at = !!TIMESTAMP,
      .before = 1
    )
  
  if (nrow(res) > 0) {
    res$quiz_date <- mdy(res$quiz_date)
  }
  
  if (nrow(existing_quiz_results) == 0) {
    return(res)
  }
  
  bind_rows(
    existing_quiz_results,
    res
  ) |> 
    group_by(quiz_date) |> 
    slice_max(updated_at, n = 1, with_ties = TRUE) |> 
    ungroup()
}

venues <- readr_geekswhodrink_release('venues')
existing_quiz_results <- readr_geekswhodrink_release('quiz-results')
previously_unscraped_venue_ids <- setdiff(venues$venue_id, unique(existing_quiz_results$venue_id))

cli_inform('Scraping quiz results from locations with prior quiz results.')
nested_existing_quiz_results <- existing_quiz_results |> 
  nest(quiz_results = -c(venue_id)) |> 
  deframe()

new_quiz_results <- nested_existing_quiz_results |> 
  imap_dfr(
    ~{
      i <- which(.y == names(nested_existing_quiz_results))
      cli_inform(c('i' = 'Scraping {i} of {length(nested_existing_quiz_results)} previously scraped venues.'))
      scrape_and_bind_geekswhodrink_venue_quiz_results(
        venue_id = as.integer(.y),
        existing_quiz_results = .x,
        max_page = 1
      )
    }
  )
write_geekswhodrink_release(new_quiz_results, 'quiz-results')

cli_inform('Scraping quiz results from locations without prior quiz results.')
completely_new_quiz_results <- map(
  previously_unscraped_venue_ids,
  ~{
    i <- which(.x == names(previously_unscraped_venue_ids))
    cli_inform(c('i' = 'Trying to scrape {i} of {length(previously_unscraped_venue_ids)} previously unscraped venues.'))
    scrape_and_bind_geekswhodrink_venue_quiz_results(
      venue_id = .x,
      existing_quiz_results = tibble(),
      max_page = NULL
    )
  }
)

bind_rows(
  new_quiz_results,
  completely_new_quiz_results
) |> 
  write_geekswhodrink_release('quiz-results')
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
