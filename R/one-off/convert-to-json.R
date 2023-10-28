suppressPackageStartupMessages(suppressWarnings({
  library(lubridate)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(jsonlite)
}))

source(file.path('R', 'helpers-geekswhodrink.R'))

TODAY <- Sys.Date()
existing_releases <- get_existing_geekwhodrink_quiz_results_releases()
existing_releases_needing_change <- dplyr::filter(
  existing_releases,
  lubridate::date(timestamp) < TODAY
)

overwrite_existing_venue_quiz_results <- function(venue_id) {
  raw <- tibble::as_tibble(read_geekswhodrink_venue_quiz_results(venue_id))
  raw$iso_quiz_date <- sprintf(
    '%s-W%02d', 
    lubridate::year(raw$quiz_date), 
    lubridate::isoweek(raw$quiz_date)
  )
  res <- convert_quiz_results_df_to_list(raw)
  write_geekswhodrink_release_json(
    x = res,
    name = venue_id,
    tag = 'venue-quiz-results'
  )
}

possibly_overwrite_existing_venue_quiz_results <- purrr::possibly(
  overwrite_existing_venue_quiz_results,
  otherwise = NULL
)

existing_releases_needing_change$venue_id |> 
  purrr::walk(possibly_overwrite_existing_venue_quiz_results)
