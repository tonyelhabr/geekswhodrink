suppressPackageStartupMessages(suppressWarnings({
  library(lubridate)
  library(dplyr)
  library(purrr)
  library(tibble)
  library(jsonlite)
}))

source(file.path('R', 'helpers-geekswhodrink.R'))

read_geekswhodrink_release(tag = 'venue-quiz-results', name = '2501863925') -> df
df <- data.frame(
  # venue_id = "foo",
  year = c(rep(2023, 4), rep(2022, 4)),
  week = c("01", "01", "02", "02", "03", "03", "04", "04"),
  team = rep(c("a", "b"), 4),
  score = c(1, 2, 3, 4, 11, 12, 13, 14)
)

split(
  df,
  df$year
) |> 
  map(
    \(.x) {
      split(
        .x,
        .x$week
      ) |> 
        map(
          \(d) {
            map(
              d$team,
              \(team) {
                d[d$team == team, ] |> 
                  dplyr::select(
                    team,
                    score
                  )
              }
            )
          }
        )
    }
  ) |> 
  toJSON(
    auto_unbox = TRUE,
    pretty = TRUE
  )
  
existing_releases <- as_tibble(existing_releases)
res <- existing_releases$venue_id[1] |> 
  map(
    \(venue_id) {
      raw <- read_geekswhodrink_release(tag = 'venue-quiz-results', name = as.character(venue_id))
      raw$year <- year(raw$quiz_date)
      raw$week <- sprintf('%02d', week(raw$quiz_date))
      quiz_results <- split(
        raw,
        raw$year
      ) |> 
        map(
          \(raw_by_year) {
            split(
              raw_by_year,
              raw_by_year$week
            ) |> 
              map(
                \(raw_by_year_week) {
                  map(
                    raw_by_year_week$placing,
                    \(placing) {
                      team_quiz_results <- raw_by_year_week[
                        raw_by_year_week$placing == placing, 
                      ]
                      list(
                        'quiz_date' = team_quiz_results$quiz_date,
                        'placing' = team_quiz_results$placing,
                        'team' = team_quiz_results$team,
                        'score' = team_quiz_results$score,
                        'updated_at' =  team_quiz_results$updated_at
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
        map(
          \(raw_by_year) {
            split(
              raw_by_year,
              raw_by_year$week
            ) |> 
              map(
                \(raw_by_year_week) {
                  list(
                    'quiz_date' = raw_by_year_week$quiz_date[1],
                    'updated_at' = raw_by_year_week$updated_at[1],
                    'has_scores' = any(!is.na(raw_by_year_week$score)),
                    'n_teams' = length(raw_by_year_week$score),
                    # 'n_teams_with_score' = sum(!is.na(raw_by_year_week$score)),
                    'max_score' = na_if(max(raw_by_year_week$score, na.rm = TRUE), -Inf),
                    'min_score' = na_if(min(raw_by_year_week$score, na.rm = TRUE), +Inf)
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
