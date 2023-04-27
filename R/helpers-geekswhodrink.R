library(rvest)
library(chromote)
library(purrr)
library(dplyr)
library(cli)
library(tibble)
library(readr)

BASE_URL <- 'https://www.geekswhodrink.com/'

readr_geekswhodrink_release <- function(name, show_col_types = FALSE) {
  read_csv(
    sprintf('https://github.com/tonyelhabr/geekswhodrink/releases/download/data/%s.csv', name),
    show_col_types = show_col_types,
  )
}

create_session_for_geekswhodrink_page <- function(venue_id, page = 1) {
  url <- paste0(BASE_URL, 'venues/', venue_id, '/?pag=', page)
  read_html_live(url)
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
  Sys.sleep(runif(1, min = 1, max = 2))
  quiz_dates <- session$html_elements('.quiz__title') |> html_text2()
  n_quiz_dates <- length(quiz_dates)
  if (n_quiz_dates == 0) {
    session$session$close()
    cli_abort('No quiz dates found for {.var venue_id} = {.val {venue_id}} on {.var page} = {.val {page}}.')
  }
  Sys.sleep(runif(1, min = 1, max = 2))
  all_tbs <- session$html_elements('table') |> html_table()
  tbs <- keep(
    all_tbs,
    ~all(c('Place Ranking', 'Team Name', 'Score') %in% colnames(.x))
  )
  
  n_tbs <- length(tbs)
  if (n_tbs == 0) {
    session$session$close()
    cli_abort('No tables found for {.var venue_id} = {.val {venue_id}} on {.var page} = {.val {page}}.')
  }
  
  if (n_tbs != n_quiz_dates) {
    cli_warn('Number of tables ({n_tbs}) is different from number of quiz dates ({n_quiz_dates}).')
    if (n_tbs < n_quiz_dates) {
      quiz_dates <- quiz_dates[1:n_tbs]
    }
  }
  
  res <- bind_rows(
    set_names(tbs, quiz_dates),
    .id = 'quiz_date'
  )
  session$session$close()
  res
}

safely_quietly_scrape_tables_from_geekswhodrink_venue_page <- safely(
  quietly(
    scrape_tables_from_geekswhodrink_venue_page
  ),
  otherwise = tibble(),
  quiet = FALSE
)

scrape_geekswhodrink_venue_quiz_results <- function(venue_id, max_page = NULL) {
  cli_inform('Scraping {.var venue_id} = {.val {venue_id}}.')
  p1_session <- create_session_for_geekswhodrink_page(venue_id, page = 1)
  Sys.sleep(runif(1, min = 1, max = 2))
  page_links <- p1_session$html_elements('.quiz__pag') |>
    html_children() |>
    html_text2()
  
  if (length(page_links) == 0) {
    cli_abort("Couldn't find any page links on the first page of the venue.")
    res <- safely_quietly_scrape_tables_from_geekswhodrink_venue_page(session = p1_session)
    if (!is.null(res$result[['warning']])) {
      cli_warn(res$result[['warning']])
    }
    return(res$result[['result']])
  }
  
  last_valid_page <- as.integer(rev(page_links)[2])
  if (is.na(last_valid_page)) {
    p1_session$session$close()
    cli_abort('{.var last_valid_page} is not a number. {.var page_links} has length {length(page_links)}.')
  }
  cli_inform('There {?is/are} {last_valid_page} page{?s} for {.var venue_id} = {.val {venue_id}}.')
  if (is.null(max_page)) {
    max_page <- last_valid_page
  } else if (max_page > last_valid_page) {
    cli_warn('Setting max page to {last_valid_page} for {.var venue_id} = {.val {venue_id}} (ignoring {.var max_page} = {.val {max_page}}).')
    max_page <- last_valid_page
  }
  
  if (max_page == 1) {
    res <- safely_quietly_scrape_tables_from_geekswhodrink_venue_page(session = p1_session)
    if (!is.null(res$result[['warning']])) {
      cli_warn(res$result[['warning']])
    }
    return(res$result[['result']])
  }
  
  tbs <- vector(mode = 'list', length = max_page)
  for(i in seq_along(1:max_page)) {
    cli_inform(c('i' = 'Scraping page {i} for {.var venue_id} = {.val {venue_id}}.'))
    res <- safely_quietly_scrape_tables_from_geekswhodrink_venue_page(venue_id, page = i)
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

possibly_scrape_geekswhodrink_venue_quiz_results <- possibly(
  scrape_geekswhodrink_venue_quiz_results,
  otherwise = tibble(),
  quiet = FALSE
)

write_geekswhodrink_release <- function(x, name, tag = 'data') {
  temp_dir <- tempdir(check = TRUE)
  basename <- sprintf('%s.csv', name)
  temp_path <- file.path(temp_dir, basename)
  write_csv(x, temp_path, na = '')
  pb_upload(
    temp_path,
    repo = 'tonyelhabr/geekswhodrink',
    tag = tag
  )
}
