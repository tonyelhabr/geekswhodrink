source(file.path('scripts', 'helpers'))

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

write_release_json(
  nested_all_quiz_results,
  name = 'quiz-results',
  tag = 'data'
)

austin_venues <- read_release_csv(
  name = 'venues', 
  tag = 'data'
)

all_quiz_results |> 
  dplyr::semi_join(
    austin_venues,
    by = dplyr::join_by(venue_id)
  ) |> 
  write_release_csv(
    name = 'austin-quiz-results',
    tag = 'data'
  )
