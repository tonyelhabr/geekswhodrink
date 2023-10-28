suppressPackageStartupMessages(suppressWarnings({
  library(rvest)
  library(chromote)
  library(purrr)
  library(dplyr)
  library(cli)
  library(readr)
  library(lubridate)
  library(piggyback)
  library(jsonlite)
}))

## Data is considered "stale" / in need of update if it hasn't been updated for x days
STALE_QUIZ_RESULTS_DURATION <- 6
## Choose a number that roughly corresponds to the number of months to do a "lookback" scrape for.
##   If the GitHub Action has been actively running, this should just be 1.
MAX_PAGE <- 1
TIMESTAMP <- lubridate::now()

BASE_URL <- 'https://www.geekswhodrink.com/'
REPO <- 'tonyelhabr/geekswhodrink'
read_geekswhodrink_release <- function(name, ext, f, tag = 'data') {
  url <- sprintf('https://github.com/%s/releases/download/%s/%s.%s', REPO, tag, name, ext)
  f(url)
}

read_geekswhodrink_release_csv <- function(name, ...) {
  read_geekswhodrink_release(
    name = name,
    ext = 'csv',
    f = function(path) { readr::read_csv(path, show_col_types = FALSE) },
    ...
  )
}

read_geekswhodrink_release_json <- function(name, ...) {
  read_geekswhodrink_release(
    name = name,
    ext = 'json',
    f = function(path) { jsonlite::read_json(path) },
    ...
  )
}

convert_quiz_results_list_to_df <- function(x) {
  purrr::map_dfr(
    rlang::set_names(names(x)),
    \(iso_week) {
      by_iso_week <- x[[iso_week]]
      raw_meta <- by_iso_week$meta
      raw_results <- by_iso_week$results
      
      meta <- tibble::tibble(
        'quiz_date' = raw_meta$quiz_date,
        'updated_at' = raw_meta$updated_at
      )
      
      suppressWarnings(
        results <- purrr::map_dfr(
          raw_results,
          \(x) {
            list(
              'placing' = as.integer(x$placing),
              'team' = as.character(x$team),
              'score' = as.integer(x$score)
            )
          }
        )
      )
      
      dplyr::bind_cols(
        meta |> dplyr::select(quiz_date, updated_at),
        results |> dplyr::select(placing, team, score)
      ) |> 
        dplyr::transmute(
          quiz_week = .env$iso_week,
          quiz_date = lubridate::ymd(quiz_date),
          placing,
          team,
          score,
          updated_at = lubridate::ymd_hms(updated_at)
        )
    }
  )
}

read_geekswhodrink_venue_quiz_results <- function(name) {
  res <- read_geekswhodrink_release_json(
    name = name,
    tag = 'venue-quiz-results'
  )
  convert_quiz_results_list_to_df(res)
}

safely_read_geekswhodrink_venue_quiz_results <- purrr::safely(
  read_geekswhodrink_venue_quiz_results,
  otherwise = list()
)

possibly_read_geekswhodrink_venue_quiz_results <- purrr::possibly(
  read_geekswhodrink_venue_quiz_results,
  otherwise = list(),
  quiet = TRUE
)


GITHUB_PAT <- Sys.getenv('GEEKS_WHO_DRINK_TOKEN')
write_geekswhodrink_release <- function(x, name, ext, f, tag = 'data') {
  temp_dir <- tempdir(check = TRUE)
  basename <- sprintf('%s.%s', name, ext)
  temp_path <- file.path(temp_dir, basename)
  f(x, temp_path)
  piggyback::pb_upload(
    temp_path,
    .token = GITHUB_PAT,
    repo = REPO,
    tag = tag
  )
}

write_geekswhodrink_release_csv <- function(x, name, ...) {
  write_geekswhodrink_release(
    x = x,
    name = as.character(name),
    ext = 'csv',
    f = function(x, path) { 
      readr::write_csv(
        x, 
        path, 
        na = ''
      ) 
    },
    ...
  )
}

write_geekswhodrink_release_json <- function(x, name, ...) {
  write_geekswhodrink_release(
    x = x,
    name = as.character(name),
    ext = 'json',
    f = function(x, path) { 
      jsonlite::write_json(
        x, 
        path, 
        auto_unbox = TRUE,
        pretty = TRUE
      ) 
    },
    ...
  )
}

convert_quiz_results_df_to_list <- function(df) {
  df$iso_quiz_date <- sprintf(
    '%s-W%02d', 
    lubridate::year(df$quiz_date), 
    lubridate::isoweek(df$quiz_date)
  )
  res <- split(
    df,
    df$iso_quiz_date
  ) |> 
    purrr::map(
      \(by_iso_week) {
        meta <- list(
          'quiz_date' = by_iso_week$quiz_date[1],
          'updated_at' = by_iso_week$updated_at[1],
          'has_scores' = any(!is.na(by_iso_week$score)),
          'n_teams' = length(by_iso_week$score),
          'max_score' = dplyr::na_if(as.double(max(by_iso_week$score, na.rm = TRUE)), -Inf),
          'min_score' = dplyr::na_if(as.double(min(by_iso_week$score, na.rm = TRUE)), +Inf),
          '3rd_score' = sort(by_iso_week$score, decreasing = TRUE)[3]
        )
        results <- purrr::map(
          1:nrow(by_iso_week),
          \(i) {
            team_result <- by_iso_week[i, ]
            list(
              'placing' = team_result$placing,
              'team' = team_result$team,
              'score' = team_result$score
            )
          }
        )
        list(
          'meta' = meta,
          'results' = results
        )
      }
    )
}


write_geekswhodrink_quiz_results <- function(x, name, ...) {
  res <- convert_quiz_results_df_to_list(x)
  
  write_geekswhodrink_release_json(
    x = res,
    name = as.character(name),
    tag = 'venue-quiz-results'
  )
}

list_geekswhodrink_releases <- function(tag) {
  piggyback::pb_list(
    repo = REPO, 
    tag = tag
  )
}

create_session_for_geekswhodrink_page <- function(venue_id, page = 1) {
  url <- paste0(BASE_URL, 'venues/', venue_id, '/?pag=', page)
  rvest::read_html_live(url)
}

scrape_tables_from_geekswhodrink_venue_page <- function(venue_id, page, session = NULL) {
  if (is.null(session)) {
    session <- create_session_for_geekswhodrink_page(
      venue_id = venue_id,
      page = page
    )
  }
  
  ## I found that there were issues when immediately querying elements from a session.
  ##   This was resolved when adding some brief timeouts.
  ##   (Even 0.5 seconds was too short in some cases, but 1 second seems sufficient.)
  Sys.sleep(stats::runif(1, min = 1, max = 2))
  quiz_dates <- session$html_elements('.quiz__title') |> rvest::html_text2()
  n_quiz_dates <- length(quiz_dates)
  if (n_quiz_dates == 0) {
    session$session$close()
    cli::cli_abort('No quiz dates found for {.var venue_id} = {.val {venue_id}} on {.var page} = {.val {page}}.')
  }
  Sys.sleep(stats::runif(1, min = 1, max = 2))
  all_tbs <- session$html_elements('table') |> rvest::html_table()
  tbs <- purrr::keep(
    all_tbs,
    ~all(c('Place Ranking', 'Team Name', 'Score') %in% colnames(.x))
  ) |> 
    purrr::map(
      \(.x) {
        .x$`Place Ranking` <- as.integer(.x$`Place Ranking`)
        .x$`Team Name` <- as.character(.x$`Team Name`)
        .x$`Score` <- as.integer(.x$Score)
        .x
      }
    )
  
  n_tbs <- length(tbs)
  if (n_tbs == 0) {
    session$session$close()
    cli::cli_abort('No tables found for {.var venue_id} = {.val {venue_id}} on {.var page} = {.val {page}}.')
  }
  
  if (n_tbs != n_quiz_dates) {
    cli::cli_warn('Number of tables ({n_tbs}) is different from number of quiz dates ({n_quiz_dates}).')
    if (n_tbs < n_quiz_dates) {
      quiz_dates <- quiz_dates[1:n_tbs]
    }
  }
  
  res <- dplyr::bind_rows(
    purrr::set_names(tbs, quiz_dates),
    .id = 'quiz_date'
  )
  session$session$close()
  res
}

quietly_scrape_tables_from_geekswhodrink_venue_page <- purrr::quietly(
  scrape_tables_from_geekswhodrink_venue_page
)
safely_quietly_scrape_tables_from_geekswhodrink_venue_page <- purrr::safely(
  quietly_scrape_tables_from_geekswhodrink_venue_page,
  otherwise = data.frame(),
  quiet = FALSE
)

clean_geekswhodrink_quiz_results <- function(df) {
  df |> 
    dplyr::transmute(
      # venue_id,
      dplyr::across(quiz_date, ~lubridate::mdy(.x)),
      placing = `Place Ranking`,
      team = `Team Name`,
      score = `Score`
    )
}

scrape_geekswhodrink_venue_quiz_results <- function(venue_id, max_page = NULL) {
  cli::cli_inform('Scraping {.var venue_id} = {.val {venue_id}}.')
  
  p1_session <- create_session_for_geekswhodrink_page(venue_id, page = 1)
  Sys.sleep(stats::runif(1, min = 2, max = 3))
  page_links <- p1_session$html_elements('.quiz__pag') |>
    rvest::html_children() |>
    rvest::html_text2()
  
  if (length(page_links) == 0) {
    cli::cli_abort("Couldn't find any page links on the first page of the venue.")
    res <- safely_quietly_scrape_tables_from_geekswhodrink_venue_page(session = p1_session)
    if (!is.null(res$result[['warning']])) {
      cli::cli_warn(res$result[['warning']])
    }
    return(clean_geekswhodrink_quiz_results(res$result[['result']]))
  }
  
  last_valid_page <- as.integer(rev(page_links)[2])
  if (is.na(last_valid_page)) {
    p1_session$session$close()
    cli::cli_abort('{.var last_valid_page} is not a number. {.var page_links} has length {length(page_links)}.')
  }
  cli::cli_inform('There are {last_valid_page} pages for {.var venue_id} = {.val {venue_id}}.')
  if (is.null(max_page)) {
    max_page <- last_valid_page
  } else if (max_page > last_valid_page) {
    cli::cli_warn('Setting max page to {last_valid_page} for {.var venue_id} = {.val {venue_id}} (ignoring {.var max_page} = {.val {max_page}}).')
    max_page <- last_valid_page
  }
  
  if (max_page == 1) {
    res <- safely_quietly_scrape_tables_from_geekswhodrink_venue_page(session = p1_session)
    if (!is.null(res$result[['warning']])) {
      cli::cli_warn(res$result[['warning']])
    }
    return(clean_geekswhodrink_quiz_results(res$result[['result']]))
  }
  
  tbs <- vector(mode = 'list', length = max_page)
  for(i in seq_along(1:max_page)) {
    cli::cli_inform(c('i' = 'Scraping page {i} for {.var venue_id} = {.val {venue_id}}.'))
    res <- safely_quietly_scrape_tables_from_geekswhodrink_venue_page(venue_id, page = i)
    if (!is.null(res$error)) {
      cli::cli_warn(res$error$message)
      break
    } 
    if (!is.null(res$result[['warning']])) {
      browser()
      cli::cli_warn(res$result[['warning']])
    }
    if (!is.null(res$result[['result']])) {
      tbs[[i]] <- res$result[['result']]
    }
    Sys.sleep(stats::runif(1, min = 1, max = 3))
  }
  
  res <- dplyr::bind_rows(tbs)
  
  if (nrow(res) == 0) {
    return(res)
  }
  
  clean_geekswhodrink_quiz_results(res)
}

possibly_scrape_geekswhodrink_venue_quiz_results <- purrr::possibly(
  scrape_geekswhodrink_venue_quiz_results,
  otherwise = data.frame(),
  quiet = TRUE
)

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
    res <- data.frame()
    write_geekswhodrink_quiz_results(
      res,
      name = venue_id
    )
    return(res)
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


judiciously_scrape_x_geekswhodrink_venue_quiz_results <- function(venue_ids, descriptor) {
  n_venues <- length(venue_ids)
  msg <- glue::glue('Scraping quiz results for {n_venues} locations with "{descriptor}" data.')
  if (descriptor == 'stale') {
    msg <- glue::glue('{msg} (i.e. venues with quiz results that have not been updated in past {STALE_QUIZ_RESULTS_DURATION} days.)')
  }
  cli::cli_inform(msg)
  purrr::iwalk(
    venue_ids,
    \(venue_id, i) {
      cli::cli_inform('Scraping {i}/{n_venues} {descriptor} venues.')
      judiciously_scrape_geekswhodrink_venue_quiz_results(
        venue_id, 
        try_if_existing_has_zero_records = TRUE
      )
    }
  )
}


## TODO: Can eventually remove the check for JSON files, once CSVs for individual quiz results are removed
get_existing_geekwhodrink_quiz_results_releases <- function() {
  
  list_geekswhodrink_releases('venue-quiz-results') |> 
    dplyr::filter(tools::file_ext(file_name) == 'json') |> 
    dplyr::mutate(
      venue_id = as.numeric(tools::file_path_sans_ext(file_name)),
      .before = 1
    ) |> 
    dplyr::arrange(timestamp, venue_id)
}


judiciously_scrape_stale_geekswhodrink_venue_quiz_results <- function() {
  existing_releases<- get_existing_geekwhodrink_quiz_results_releases()
  existing_releases_needing_update <- dplyr::filter(
    existing_releases,
    timestamp < lubridate::days(STALE_QUIZ_RESULTS_DURATION)
  )
  venue_ids <- existing_releases_needing_update$venue_id
  judiciously_scrape_x_geekswhodrink_venue_quiz_results(
    venue_ids = venue_ids,
    descriptor = 'stale'
  )
}

judiciously_scrape_new_geekswhodrink_venue_quiz_results <- function() {
  venues <- read_geekswhodrink_release_csv('venues')
  existing_releases <- get_existing_geekwhodrink_quiz_results_releases()
  
  venue_ids <- setdiff(venues$venue_id, existing_releases$venue_id)
  
  judiciously_scrape_x_geekswhodrink_venue_quiz_results(
    venue_ids = venue_ids,
    descriptor = 'new'
  )
}
