source(file.path('R', 'helpers-geekswhodrink.R'))

existing_quiz_result_batches <- list_geekswhodrink_releases('data') |> 
    dplyr::filter(grepl('quiz-results-', file_name)) |> 
    dplyr::arrange(timestamp, file_name)

venue_ids <- existing_releases$venue_id
n_venues <- length(venue_ids)
possibly_read_geekswhodrink_venue_quiz_results <- purrr::possibly(
  read_geekswhodrink_venue_quiz_results,
  otherwise = tibble::tibble()
)
all_quiz_results <- purrr::map_dfr(
  existing_quiz_result_batches$file_name,
  \(.name) {
    read_geekswhodrink_release_csv(
      name = .name,
      tag = 'data'
    )
  }
)

write_geekswhodrink_release_csv(
  all_quiz_results,
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

write_geekswhodrink_release_json(
  nested_all_quiz_results,
  name = 'quiz-results',
  tag = 'data'
)

austin_venues <- read_geekswhodrink_release_csv(
  name = 'venues', 
  tag = 'data'
)
all_quiz_results |> 
  dplyr::semi_join(
    austin_venues,
    by = dplyr::join_by(venue_id)
  ) |> 
  write_geekswhodrink_release_csv(
    name = 'austin-quiz-results',
    tag = 'data'
  )
