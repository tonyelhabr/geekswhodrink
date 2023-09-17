read_geekswhodrink_release(tag = 'venue-quiz-results', name = '2501863925') -> df
df |> 
  select(-venue_id, -updated_at) %>% 
  split(.$quiz_date) |> 
  map(
    \(quiz_date_results) {
      imap(
        set_names(quiz_date_results$placing),
        \(placing, .y) {
          team_quiz_results <- quiz_date_results[quiz_date_results$placing == placing, ]
          list(
            'team' = team_quiz_results$team,
            'score' = team_quiz_results$score
          )
        }
      )
    }
  ) |> 
  jsonlite::toJSON(
    pretty = TRUE,
    auto_unbox = TRUE
  )
