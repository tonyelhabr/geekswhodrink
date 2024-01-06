source(file.path('scripts', 'helpers.R'))
venue_id <- 1002923621 # 2180352964
x <- read_venue_quiz_results(venue_id)
write_quiz_results(
  res,
  venue_id
)
# res <- convert_quiz_results_df_to_list(x)
# 
# tag <- 'venue-quiz-results'
# name <- as.character(venue_id)
# ext <- 'json'
# f <- function(x, path) { 
#   jsonlite::write_json(
#     x, 
#     path, 
#     auto_unbox = TRUE,
#     pretty = TRUE
#   ) 
# }
# temp_dir <- tempdir(check = TRUE)
# basename <- sprintf('%s.%s', name, ext)
# temp_path <- file.path(temp_dir, basename)
# f(res, temp_path)
# piggyback:::pb_upload_file(
#   temp_path,
#   .token = GITHUB_PAT,
#   repo = REPO,
#   tag = tag
# )
# 
# # releases <- pb_releases(repo = REPO, .token = GITHUB_PAT)
