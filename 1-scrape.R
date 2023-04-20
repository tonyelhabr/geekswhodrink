## pak::pak('tidyverse/rvest@chromote')
library(rvest) ## need chromote version
library(purrr)
library(dplyr)
library(cli)
library(lubridate)
library(readr)

venues <- read_csv('venues.csv')
DATA_DIR <- 'data'
dir.create(DATA_DIR, showWarnings = FALSE)

BASE_URL <- 'https://www.geekswhodrink.com/'
create_session_for_geekswhodrink_page <- function(venue_id, page = 1) {
  url <- paste0(BASE_URL, 'venues/', venue_id, '/?pag=', page)
  
  read_html_live(url)
}

scrape_tables_from_geekswhodrink_venue_page <- function(venue_id, page) {
  session <- create_session_for_geekswhodrink_page(
    venue_id = venue_id,
    page = page
  )
  
  ## I found that there were issues when immediately querying elements from a session.
  ##   This was resolved when adding some brief timeouts.
  ##   (Even 0.5 seconds was too short in some cases, but 1 second seems sufficient.)
  Sys.sleep(runif(1, min = 1, max = 2))
  quiz_dates <- session$html_elements('.quiz__title') |> html_text2()
  n_quiz_dates <- length(quiz_dates)
  if (n_quiz_dates == 0) {
    stop('No quiz dates found.')
  }
  Sys.sleep(runif(1, min = 1, max = 2))
  all_tbs <- session$html_elements('table') |> html_table()
  tbs <- keep(
    all_tbs,
    ~all(c('Place Ranking', 'Team Name', 'Score') %in% colnames(.x))
  )
  
  n_tbs <- length(tbs)
  if (n_tbs == 0) {
    stop('No tables found.')
  }
  
  if (n_tbs != n_quiz_dates) {
    warning(sprintf('Number of tables (%s) is different from number of quiz dates (%s).', n_tbs, n_quiz_dates))
    if (n_tbs < n_quiz_dates) {
      quiz_dates <- quiz_dates[1:n_tbs]
    }
  }
  
  
  bind_rows(
    set_names(tbs, quiz_dates),
    .id = 'quiz_date'
  )
}

very_safely_scrape_tables_from_geekswhodrink_venue_page <- safely(
  quietly(
    scrape_tables_from_geekswhodrink_venue_page
  )
)

scrape_geekswhodrink_venue_quiz_results <- function(venue_id) {
  cli_inform('Scraping {.var venue_id} = {.val {venue_id}}.')
  p1_session <- create_session_for_geekswhodrink_page(venue_id, page = 1)
  Sys.sleep(runif(1, min = 1, max = 2))
  page_links <- p1_session$html_elements('.quiz__pag') |>
    html_children() |>
    html_text2()
  if (length(page_links) == 0) {
    cli_abort("=Couldn't find any page links on the first page of the venue.")
    res <- very_safely_scrape_tables_from_geekswhodrink_venue_page(venue_id, page = 1)
    if (!is.null(res$result[['warning']])) {
      cli_warn(res$result[['warning']])
    }
    return(res)
  }
  last_valid_page <- as.integer(rev(page_links)[2])
  if (is.na(last_valid_page)) {
    cli_abort('{.var last_valid_page} is not a number. {.var page_links} has length {length(page_links)}.')
  }
  cli_inform(c('i' = 'There {?is/are} {last_valid_page} page{?s}.'))
  tbs <- vector(mode = 'list', length = last_valid_page)
  for(i in seq_along(1:last_valid_page)) {
    cli_inform(c('i' = 'Scraping page {i}.'))
    res <- very_safely_scrape_tables_from_geekswhodrink_venue_page(venue_id, page = i)
    if (!is.null(res$error)) {
      cli_warn(res$error)
      break
    } 
    if (!is.null(res$result[['warning']])) {
      cli_warn(res$result[['warning']])
    }
    if (!is.null(res$result[['result']])) {
      tbs[[i]] <- res$result[['result']]
    }
    Sys.sleep(runif(1, min = 1, max = 3))
  }
  bind_rows(tbs)
}

TIMESTAMP <- now()

scrape_and_cache_geekswhodrink_venue_quiz_results <- function(venue_id, overwrite = FALSE) {
  path <- file.path(DATA_DIR, paste0(venue_id, '.csv'))
  if (file.exists(path) & isFALSE(overwrite)) {
    return(read_csv(path))
  }
  res <- scrape_geekswhodrink_venue_quiz_results(venue_id) |> 
    mutate(
      venue_id = venue_id,
      updated_at = !!TIMESTAMP,
      .before = 1
    )
  write_csv(res, path, na = '')
  res
}

safely_scrape_and_cache_geekswhodrink_venue_quiz_results <- safely(
  scrape_and_cache_geekswhodrink_venue_quiz_results,
  otherwise = NULL, 
  quiet = FALSE
)

quiz_results <- map_dfr(
  venues$venue_id,
  safely_scrape_and_cache_geekswhodrink_venue_quiz_results
)

quiz_results |> 
  transmute(
    venue_id,
    across(quiz_date, ~mdy(quiz_date)),
    placing = `Place Ranking`,
    team = `Team Name`,
    score = `Score`
  ) |> 
  write_csv('quiz-results.csv', na = '')
