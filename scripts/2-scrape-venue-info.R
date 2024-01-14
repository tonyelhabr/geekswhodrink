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
judiciously_scrape_venue_info(filt_venues)
