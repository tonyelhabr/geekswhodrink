suppressPackageStartupMessages(suppressWarnings({
  library(rvest)
  library(chromote)
  library(purrr)
  library(dplyr)
  library(cli)
  library(readr)
  library(lubridate)
  library(piggyback)
}))

BASE_URL <- 'https://www.geekswhodrink.com/'
REPO <- 'tonyelhabr/geekswhodrink'
read_geekswhodrink_release <- function(name, tag = 'data', show_col_types = FALSE) {
  url <- sprintf('https://github.com/%s/releases/download/%s/%s.csv', REPO, tag, name)
  readr::read_csv(
    url,
    show_col_types = show_col_types,
  )
}

safely_read_geekswhodrink_release <- purrr::safely(
  read_geekswhodrink_release,
  otherwise = data.frame()
)

possibly_read_geekswhodrink_release <- purrr::possibly(
  read_geekswhodrink_release,
  otherwise = data.frame(),
  quiet = TRUE
)

write_geekswhodrink_release <- function(x, name, tag = 'data') {
  temp_dir <- tempdir(check = TRUE)
  basename <- sprintf('%s.csv', name)
  temp_path <- file.path(temp_dir, basename)
  readr::write_csv(x, temp_path, na = '')
  piggyback::pb_upload(
    temp_path,
    repo = REPO,
    tag = tag
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
