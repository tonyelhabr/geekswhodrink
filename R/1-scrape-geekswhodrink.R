suppressPackageStartupMessages(suppressWarnings({
  library(lubridate)
  library(dplyr)
  library(purrr)
  library(cli)
  library(tibble)
}))

source(file.path('R', 'helpers-geekswhodrink.R'))
TIMESTAMP <- lubridate::now()
## Data is considered "stale" / in need of update if it hasn't been updated for x days
STALE_QUIZ_RESULTS_DURATION <- 7
## Choose a number that roughly corresponds to the number of months to do a "lookback" scrape for.
##   If the GitHub Action has been actively running, this should just be 1.
MAX_PAGE <- 1

## setup ----
venues <- read_geekswhodrink_release_csv('venues')

existing_releases <- list_geekswhodrink_releases('venue-quiz-results') |> 
  dplyr::filter(tools::file_ext(file_name) == 'json') |> 
  dplyr::mutate(
    venue_id = as.numeric(tools::file_path_sans_ext(file_name)),
    .before = 1
  ) |> 
  dplyr::arrange(timestamp, venue_id)

existing_releases_needing_update <- dplyr::filter(
  existing_releases,
  timestamp < lubridate::days(STALE_QUIZ_RESULTS_DURATION)
)

new_venue_ids <- setdiff(venues$venue_id, existing_releases$venue_id)
stale_venue_ids <- existing_releases_needing_update$venue_id

## scrape ----
judiciously_scrape_geekswhodrink_venue_quiz_results <- function(
    venue_id, 
    try_if_existing_has_zero_records = FALSE, 
    try_if_recently_scraped = FALSE,
    recent_scrape_window = lubridate::duration(1, unit = 'days')
) {
  res_existing <- safely_read_geekswhodrink_venue_quiz_results(venue_id)
  
  release_file_exists <- TRUE
  existing_quiz_results <- res_existing$result
  max_page <- NULL
  
  existing_has_error <- !is.null(res_existing$error)
  ## TODO: Coalesce logic for file with 0 rows and no existing file.
  ## Note that we use && instead of & to "escape early" in the case that reading in the file
  ##   encountered an error.
  existing_has_zero_records <- isFALSE(existing_has_error) && (nrow(existing_quiz_results) == 0)
  if (existing_has_error | existing_has_zero_records) {
    cli::cli_warn('No existing release file for {.var venue_id} = {.val {venue_id}} .')
    existing_quiz_results <- NULL
    has_existing_quiz_results <- FALSE
  } else {
    existing_quiz_results <- convert_quiz_results_list_to_df(existing_quiz_results_list)
    n_quiz_results <- nrow(existing_quiz_results)
    cli::cli_inform(c('i' = '{.var venue_id} = {.val {venue_id}} has {n_quiz_results} existing records.'))
    has_existing_quiz_results <- TRUE
    ## Only try scraping the first quiz page for a venue where we've successfully 
    ##   scraped in the past.
    ##   If we're scraping at a weekly cadence, we won't "miss" any quiz results. 
    ##   And, even if we're scraping at a monthly cadence, we should be ok, 
    ##   since venue pages, typically have at least the 5 most recent week's set of scores.
    max_page <- MAX_PAGE
  }
  
  ## Don't continue to try to scrape if the venue_id is not completely new, 
  ##   didn't have past existing results, and has a release file.
  ##   We should consider setting try_if_existing_has_zero_records = FALSE 
  ##   occasionally to check back on venues that originally didn't
  ##   have any records (e.g. a venue with "Coming soon!")
  if (
    isTRUE(existing_has_zero_records) & 
    isFALSE(try_if_existing_has_zero_records)
  ) {
    return(data.frame())
  }
  
  ## Don't try scraping if we've already scraped recently
  ##   Note that this may not really be necessary if we're already checking for recency
  ##   outside of this function.
  if (isTRUE(has_existing_quiz_results) & isFALSE(try_if_recently_scraped)) {
    max_updated_at <- max(existing_quiz_results$updated_at)
    
    should_try_scraping <- (max_updated_at + recent_scrape_window) < TIMESTAMP
    if (isFALSE(should_try_scraping)) {
      cli::cli_inform(c('i' = 'Returning early since it is within the scrape window.'))
      return(existing_quiz_results)
    }
  }
  
  res <- possibly_scrape_geekswhodrink_venue_quiz_results(venue_id, max_page = max_page)
  n_new_records <- nrow(res)
  cli::cli_inform(c('i' = 'Got {n_new_records} new records for {.var venue_id} = {.val {venue_id}}.'))
  
  if (n_new_records == 0) {
    if (isFALSE(release_file_exists)) {
      cli::cli_inform(c('i' = 'Writing to release file even though no records exist since no release file existed before.'))
      
      write_geekswhodrink_quiz_results(
        res,
        name = venue_id
      )
      return(res)
    } else {
      ## Figure out what to return if a release exists and there were no new records.
      if (isTRUE(has_existing_quiz_results)) {
        ## if we had non-zero existing records
        return(existing_quiz_results)
      } else {
        ## could just return existing_quiz_results if it were a data.frame
        return(NULL)
      }
    }
  }
  
  res$updated_at <- TIMESTAMP
  
  res <- dplyr::bind_rows(
    existing_quiz_results,
    res
  ) |> 
    ## Only update the updated_at field for the new scores on the first page. We'll most likely
    ##   re-scrape results we've already scraped on the first page (past weeks). Keep the updated_at field the
    ##   same for those weeks.
    dplyr::group_by(quiz_date) |> 
    dplyr::slice_min(updated_at, n = 1, with_ties = TRUE) |> 
    dplyr::ungroup()
  
  write_geekswhodrink_quiz_results(
    res,
    name = venue_id
  )
  invisible(res)
}

n_stale_venues <- length(stale_venue_ids)
cli::cli_inform('Scraping quiz results for {n_stale_venues} locations with "stale" data. (Has not been updated in past {STALE_QUIZ_RESULTS_DURATION} days.)')
purrr::iwalk(
  stale_venue_ids,
  \(venue_id, i) {
    cli::cli_inform('Scraping {i}/{n_stale_venues} stale venues.')
    judiciously_scrape_geekswhodrink_venue_quiz_results(
      venue_id, 
      try_if_existing_has_zero_records = TRUE
    )
  }
)

n_new_venues <- length(new_venue_ids)
cli::cli_inform('Scraping quiz results for {n_new_venues} locations with no prior GitHub release data.')
purrr::iwalk(
  new_venue_ids,
  \(venue_id, i) {
    cli::cli_inform('Scraping {i}/{n_new_venues} new venues.')
    judiciously_scrape_geekswhodrink_venue_quiz_results(
      venue_id, 
      try_if_existing_has_zero_records = TRUE
    )
  }
)
