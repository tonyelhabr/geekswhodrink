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

# safely_read_geekswhodrink_release_csv <- purrr::safely(
#   read_geekswhodrink_release_csv,
#   otherwise = data.frame()
# )
# 
# possibly_read_geekswhodrink_release_csv <- purrr::possibly(
#   read_geekswhodrink_release_csv,
#   otherwise = data.frame(),
#   quiet = TRUE
# )
# 
# safely_read_geekswhodrink_release_json <- purrr::safely(
#   read_geekswhodrink_release_json,
#   otherwise = data.frame()
# )
# 
# possibly_read_geekswhodrink_release_json <- purrr::possibly(
#   read_geekswhodrink_release_json,
#   otherwise = data.frame(),
#   quiet = TRUE
# )

unnest_quiz_results_element <- function(x) {
  res <- purrr::map_dfr(
    names(x),
    \(year) {
      purrr::map_dfr(
        names(x[[year]]),
        \(week) {
          dplyr::mutate(
            dplyr::bind_rows(x[[year]][[week]]),
            year = year,
            week = week,
            .before = 1
          )
        }
      )
    }
  )
  res
}

cleanly_read_geekswhodrink_venue_quiz_results <- function(name) {
  raw <- read_geekswhodrink_release_json(name = name, tag = 'venue-quiz-results')
  results <- raw$results
  meta <- raw$meta
  
  results <- unnest_quiz_results_element(raw$results)
  meta <- unnest_quiz_results_element(raw$meta)
  
  dplyr::inner_join(
    meta,
    results,
    by = dplyr::join_by(year, week)
  )
}

read_geekswhodrink_venue_quiz_results <- function(name) {
  read_geekswhodrink_release_json(name = name, tag = 'venue-quiz-results')
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
    name = name,
    ext = 'csv',
    f = function(x, path) { readr::write_csv(x, path, na = '') },
    ...
  )
}

write_geekswhodrink_release_json <- function(x, name, ...) {
  write_geekswhodrink_release(
    x = x,
    name = name,
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

write_geekswhodrink_quiz_results <- function(x, name, ...) {
  x$year <- lubridate::year(x$quiz_date)
  x$week <- sprintf('%02d', lubridate::week(x$quiz_date))
  results <- split(
    x,
    x$year
  ) |> 
    purrr::map(
      \(x_by_year) {
        split(
          x_by_year,
          x_by_year$week
        ) |> 
          purrr::map(
            \(x_by_year_week) {
              purrr::map(
                x_by_year_week$placing,
                \(placing) {
                  team_quiz_results <- x_by_year_week[
                    x_by_year_week$placing == placing, 
                  ]
                  list(
                    'placing' = team_quiz_results$placing,
                    'team' = team_quiz_results$team,
                    'score' = team_quiz_results$score
                  )
                }
              )
            }
          )
      }
    )
  
  meta <- split(
    x,
    x$year
  ) |> 
    purrr::map(
      \(x_by_year) {
        split(
          x_by_year,
          x_by_year$week
        ) |> 
          purrr::map(
            \(x_by_year_week) {
              list(
                'quiz_date' = x_by_year_week$quiz_date[1],
                'updated_at' = x_by_year_week$updated_at[1],
                'has_scores' = any(!is.na(x_by_year_week$score)),
                'n_teams' = length(x_by_year_week$score),
                'max_score' = dplyr::na_if(max(x_by_year_week$score, na.rm = TRUE), -Inf),
                'min_score' = dplyr::na_if(min(x_by_year_week$score, na.rm = TRUE), +Inf),
                '3rd_score' = sort(x_by_year_week$score, decreasing = TRUE)[3]
              )
            }
          )
      }
    )
  
  res <- list(
    'meta' = meta,
    'results' = results
  )
  
  write_geekswhodrink_release_json(
    x = res,
    name = as.character(venue_id),
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

flatten_by_year_week <- function(x, f) {
  purrr::map_dfr(
    names(x),
    \(year) {
      purrr::map_dfr(
        names(x[[year]]),
        \(week) {
          f(x[[year]][[week]]) |> 
            dplyr::mutate(
              year = .env$year,
              week = .env$week,
              .before = 1
            )
        }
      )
    }
  )
}

convert_quiz_results_list_to_df <- function(x) {
  raw_quiz_results <- x$results
  raw_quiz_meta <- x$meta
  quiz_results <- flatten_by_year_week(
    raw_quiz_results,
    \(x) {
      purrr::map_dfr(
        seq_along(x),
        \(i) {
          row <- x[[i]]
          ## TODO: this is to address a bug with initial storage of data as JSON, where I indexed by placing,
          ##   assuming each venue's weekly quiz results had unique placings. now,
          ##   i just sequence a long records, so in the future, this clause can be taken out
          if (class(row$team) == 'list') {
            return(
              data.frame(
                'placing' = as.integer(unlist(purrr::pluck(row, 'placing'))),
                'team' = unlist(purrr::pluck(row, 'team')),
                'score' = as.integer(unlist(purrr::pluck(row, 'score')), 'NA')
              )
            )
            
          }
          data.frame(
            'placing' = as.integer(row$placing),
            'team' = dplyr::coalesce(row$team, NA_character_),
            'score' = as.integer(dplyr::na_if(row$score, 'NA'))
          )
        }
      )
    }
  )
  
  quiz_meta <- flatten_by_year_week(
    raw_quiz_meta,
    \(x) {
      data.frame(
        'quiz_date' = x$quiz_date,
        'updated_at' = x$updated_at
      )
    }
  )
  
  dplyr::inner_join(
    quiz_meta |> dplyr::select(year, week, quiz_date, updated_at),
    quiz_results |> dplyr::select(year, week, placing, team, score),
    dplyr::join_by(year, week)
  ) |> 
    tibble::as_tibble()
}

split_by_year_week <- function(df, f) {
  split(
    df,
    df$year
  ) |> 
    purrr::map(
      \(by_year) {
        split(
          by_year,
          by_year$week
        ) |> 
          purrr::map(
            \(by_year_week) {
              f(by_year_week)
            }
          )
      }
    )
}

convert_quiz_results_df_to_list <- function(df) {
  df$year <- lubridate::year(df$quiz_date)
  df$week <- sprintf('%02d', lubridate::week(df$quiz_date))
  quiz_results <- split_by_year_week(
    df,
    \(x) {
      purrr::map(
        x$placing,
        \(placing) {
          team_quiz_results <- x[
            x$placing == placing, 
          ]
          list(
            'placing' = team_quiz_results$placing,
            'team' = team_quiz_results$team,
            'score' = team_quiz_results$score
          )
        }
      )
    }
  )
  
  quiz_meta <- split_by_year_week(
    df,
    \(x) {
      list(
        'quiz_date' = x$quiz_date[1],
        'updated_at' = x$updated_at[1],
        'has_scores' = any(!is.na(x$score)),
        'n_teams' = length(x$score),
        ## na_if(x, y) has issues with if xis an integer and y is Inf
        'max_score' = dplyr::na_if(as.double(max(x$score, na.rm = TRUE)), -Inf),
        'min_score' = dplyr::na_if(as.double(min(x$score, na.rm = TRUE)), +Inf),
        '3rd_score' = sort(x$score, decreasing = TRUE)[3]
      )
    }
  )
  
  list(
    'meta' = quiz_meta,
    'results' = quiz_results
  )
}
