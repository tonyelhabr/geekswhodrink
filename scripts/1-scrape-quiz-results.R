source(file.path('scripts', 'helpers.R'))
venues <- read_release_csv('venues')
stopifnot('No data in venues file!' = nrow(venues) > 0)
filt_venues <- venues |> 
  dplyr::filter(
    !is.na(updated_at)
  ) |> 
  dplyr::filter(
    updated_at == max(updated_at)
  )
judiciously_scrape_new_venue_quiz_results(filt_venues)
judiciously_scrape_stale_venue_quiz_results(filt_venues)
