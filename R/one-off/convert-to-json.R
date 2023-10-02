suppressPackageStartupMessages(suppressWarnings({
  library(lubridate)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(jsonlite)
}))

source(file.path('R', 'helpers-geekswhodrink.R'))

raw_existing_releases <- list_geekswhodrink_releases('venue-quiz-results')

existing_releases <- raw_existing_releases |> 
  dplyr::as_tibble() |> 
  dplyr::transmute(
    venue_id = as.numeric(tools::file_path_sans_ext(file_name)),
    ext = tools::file_ext(file_name),
    dummy = TRUE
  ) |> 
  tidyr::pivot_wider(
    names_from = ext,
    values_from = dummy
  ) |> 
  dplyr::filter(csv, !json) |> 
  dplyr::arrange(venue_id)

existing_releases$venue_id |> 
  purrr::walk(
    \(venue_id) {
      raw <- read_geekswhodrink_release(tag = 'venue-quiz-results', name = as.character(venue_id))
      raw$year <- lubridate::year(raw$quiz_date)
      raw$week <- sprintf('%02d', lubridate::week(raw$quiz_date))
      quiz_results <- split(
        raw,
        raw$year
      ) |> 
        purrr::map(
          \(raw_by_year) {
            split(
              raw_by_year,
              raw_by_year$week
            ) |> 
              purrr::map(
                \(raw_by_year_week) {
                  purrr::map(
                    raw_by_year_week$placing,
                    \(placing) {
                      team_quiz_results <- raw_by_year_week[
                        raw_by_year_week$placing == placing, 
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
      
      quiz_meta <- split(
        raw,
        raw$year
      ) |> 
        purrr::map(
          \(raw_by_year) {
            split(
              raw_by_year,
              raw_by_year$week
            ) |> 
              purrr::map(
                \(raw_by_year_week) {
                  list(
                    'quiz_date' = raw_by_year_week$quiz_date[1],
                    'updated_at' = raw_by_year_week$updated_at[1],
                    'has_scores' = any(!is.na(raw_by_year_week$score)),
                    'n_teams' = length(raw_by_year_week$score),
                    'max_score' = dplyr::na_if(max(raw_by_year_week$score, na.rm = TRUE), -Inf),
                    'min_score' = dplyr::na_if(min(raw_by_year_week$score, na.rm = TRUE), +Inf),
                    '3rd_score' = sort(raw_by_year_week$score, decreasing = TRUE)[3]
                  )
                }
              )
          }
        )
      
      res <- list(
        'meta' = quiz_meta,
        'results' = quiz_results
      )
      
      write_geekswhodrink_release_json(
        x = res,
        name = as.character(venue_id),
        tag = 'venue-quiz-results'
      )
    }
  )
res
