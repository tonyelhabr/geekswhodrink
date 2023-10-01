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
MAX_PAGE <- 6

## setup ----
venues <- read_geekswhodrink_release('venues')

existing_releases <- list_geekswhodrink_releases('venue-quiz-results') |> 
  dplyr::filter(tools::file_path_sans_ext(file_name) == 'csv') |> 
  dplyr::mutate(
    venue_id = as.numeric(tools::file_path_sans_ext(file_name)),
    .before = 1
  ) |> 
  dplyr::arrange(timestamp, venue_id)

existing_releases_needing_update <- dplyr::filter(
  existing_releases,
  timestamp < (Sys.Date() - lubridate::days(STALE_QUIZ_RESULTS_DURATION))
)

new_venue_ids <- setdiff(venues$venue_id, existing_releases$venue_id)
stale_venue_ids <- existing_releases_needing_update$venue_id

## scrape ----
judiciously_scrape_geekswhodrink_venue_quiz_results <- function(
    venue_id, 
    try_if_existing_has_zero_rows = FALSE, 
    try_if_recently_scraped = FALSE,
    recent_scrape_window = lubridate::duration(1, unit = 'days')
) {
  res_existing <- safely_read_geekswhodrink_release(venue_id, tag = 'venue-quiz-results')
  
  release_file_exists <- TRUE
  existing_quiz_results <- res_existing$result
  max_page <- NULL
  
  existing_has_error <- !is.null(res_existing$error)
  ## TODO: Coalesce logic for file with 0 rows and no existing file.
  existing_has_zero_rows <- isFALSE(existing_has_error) & (nrow(res_existing$result) == 0)
  if (existing_has_error | existing_has_zero_rows) {
    cli::cli_warn('No existing release file for {.var venue_id} = {.val {venue_id}} .')
    existing_quiz_results <- NULL # tibble::tibble() ## 0-row tibbles work with bind_rows, whereas 0-row dataframes may not
    has_existing_quiz_results <- FALSE
    release_file_exists <- FALSE
  } else {
    n_quiz_results <- nrow(existing_quiz_results)
    has_existing_quiz_results <- nrow(existing_quiz_results) > 0
    cli::cli_inform(c('i' = '{.var venue_id} = {.val {venue_id}} has {n_quiz_results} existing records.'))
    ## Only try scraping the first quiz page for a venue where we've successfully 
    ##   scraped in the past.
    ##   If we're scraping at a weekly cadence, we won't "miss" any quiz results. 
    ##   And, even if we're scraping at a monthly cadence, we should be ok, 
    ##   since venue pages, typically have at least the 5 most recent week's set of scores.
    if (isTRUE(has_existing_quiz_results)) {
      max_page <- MAX_PAGE
      ## Temporary fix for files where I forgot to include updated_at
      if (isFALSE(any(colnames(existing_quiz_results) == 'updated_at'))) {
        existing_quiz_results$updated_at <- TIMESTAMP
        write_geekswhodrink_release_csv(
          existing_quiz_results,
          name = venue_id, 
          tag = 'venue-quiz-results'
        )
      }
    }
  }
  
  ## Don't continue to try to scrape if the venue_id is not completely new, 
  ##   didn't have past existing results, and has a release file.
  ##   We should consider setting try_if_existing_has_zero_rows = FALSE 
  ##   occasionally to check back on venues that originally didn't
  ##   have any records (e.g. a venue with "Coming soon!")
  if (
    isFALSE(has_existing_quiz_results) & 
    isFALSE(try_if_existing_has_zero_rows) & 
    isTRUE(release_file_exists)
  ) {
    return(data.frame())
  }
  
  ## Don't try scraping if we've already scraped recently
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
      write_geekswhodrink_release_csv(
        res,
        name = venue_id, 
        tag = 'venue-quiz-results'
      )
      return(res)
    } else {
      ## Figure out what to return if a release exists and there were no new records.
      if (isTRUE(has_existing_quiz_results)) {
        ## if we had non-zero existing records
        return(existing_quiz_results)
      } else {
        ## could just return existing_quiz_results if it were a data.frame
        return(data.frame())
      }
    }
  }
  
  res <- dplyr::mutate(
    res,
    "venue_id" = venue_id,
    .before = 1
  )
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
  
  write_geekswhodrink_release_csv(
    res,
    name = venue_id, 
    tag = 'venue-quiz-results'
  )
  invisible(res)
}

cli::cli_inform('Scraping quiz results for locations with "stale" data. (Has not been updated in past {STALE_QUIZ_RESULTS_DURATION} days.)')
n_stale_venues <- length(stale_venue_ids)
purrr::iwalk(
  stale_venue_ids,
  \(venue_id, i) {
    cli::cli_inform('Scraping {i}/{n_stale_venues} venues.')
    judiciously_scrape_geekswhodrink_venue_quiz_results(
      venue_id, 
      try_if_existing_has_zero_rows = TRUE
    )
  }
)

cli::cli_inform('Scraping quiz results for locations with no prior GitHub release data.')
n_new_venues <- length(new_venue_ids)
if (n_new_venues > 0) {
  purrr::iwalk(
    new_venue_ids,
    \(venue_id, i) {
      cli::cli_inform('Scraping {i}/{n_new_venues} venues.')
      judiciously_scrape_geekswhodrink_venue_quiz_results(
        venue_id, 
        try_if_existing_has_zero_rows = TRUE
      )
    }
  )
}
