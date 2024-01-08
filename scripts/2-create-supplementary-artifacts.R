source(file.path('scripts', 'helpers.R'))

cli::cli_inform('Pulling in all quiz results (from CSV).')
all_quiz_results <- read_release_csv(
  name = 'quiz-results',
  tag = 'data'
)

nested_all_quiz_results <- purrr::map(
  split(
    all_quiz_results, 
    all_quiz_results$venue_id
  ),
  \(by_venue) {
    convert_quiz_results_df_to_list(by_venue)
  }
)

cli::cli_inform('Writing all quiz results as JSON.')
write_release_json(
  nested_all_quiz_results,
  name = 'quiz-results',
  tag = 'data'
)
