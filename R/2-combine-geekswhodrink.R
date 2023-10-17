source(file.path('R', 'helpers-geekswhodrink.R'))

existing_releases <- get_existing_geekwhodrink_quiz_results_releases()
venue_ids <- existing_releases$venue_id
n_venues <- length(venue_ids)
all_quiz_results <- purrr::map_dfr(
  seq_along(venue_ids),
  \(i) {
    venue_id <- venue_ids[i]
    cli::cli_inform('Scraping {i}/{n_venues} venues.')
    read_geekswhodrink_venue_quiz_results(venue_id)
  }
)

