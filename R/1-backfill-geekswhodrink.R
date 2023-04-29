library(lubridate)
library(dplyr)
library(readr)
library(purrr)

source(file.path('R', 'helpers-geekswhodrink.R'))

TIMESTAMP <- now()
scrape_and_cache_geekswhodrink_venue_quiz_results <- function(venue_id, overwrite = FALSE) {
  res <- possibly_scrape_geekswhodrink_venue_quiz_results(venue_id)
  if (nrow(res) > 0) {
    res$updated_at = !!TIMESTAMP
  }
  write_geekswhodrink_release(
    res, 
    name = venue_id,
    tag = 'venue-quiz-results'
  )
  res
}

venues <- read_geekswhodrink_release('venues') |> arrange(venue_id)

quiz_results <- map_dfr(
  venues$venue_id,
  scrape_and_cache_geekswhodrink_venue_quiz_results
)

write_geekswhodrink_release(
  quiz_results,
  name = 'quiz-results',
  tag = 'data'
)
