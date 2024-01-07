source(file.path('scripts', 'helpers.R'))
print(Sys.getenv('GEEKS_WHO_DRINK_TOKEN'))
venue_id <- 1002923621
res <- read_venue_quiz_results(venue_id)
write_venue_quiz_results(
  res,
  venue_id
)
