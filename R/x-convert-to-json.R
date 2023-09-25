read_geekswhodrink_release(tag = 'venue-quiz-results', name = '2501863925') -> df
res <- df |> 
  select(-venue_id, -updated_at) %>% 
  split(.$quiz_date) |> 
  map(
    \(quiz_date_results) {
      map(
        quiz_date_results$placing,
        \(placing) {
          team_quiz_results <- quiz_date_results[quiz_date_results$placing == placing, ]
          list(
            'placing' = team_quiz_results$placing,
            'team' = team_quiz_results$team,
            'score' = team_quiz_results$score
          )
        }
      )
    }
  )

res <- df |> 
  select(-venue_id, -updated_at) %>% 
  split(.$quiz_date) |> 
  map(
    \(quiz_date_results) {
    }
  )
res |> 
  jsonlite::write_json(
    'temp.json',
    pretty = TRUE,
    auto_unbox = TRUE
  )
raw <- jsonlite::read_json('temp.json')
imap_dfr(
  raw,
  \(quiz_date_results, .y) {
    bind_cols(
      tibble(quiz_date = lubridate::ymd(.y)),
      bind_rows(quiz_date_results)
    )
  }
)
bind_rows(raw, .id = 'quiz_date')
