source(file.path('scripts', 'helpers.R'))
venue_id <- 2180352964
res <- read_venue_quiz_results(venue_id)
write_quiz_results(
  res,
  venue_id
)
