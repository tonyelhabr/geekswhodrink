url <- 'https://api.github.com/rate_limit'

GITHUB_PAT <- Sys.getenv('GEEKS_WHO_DRINK_TOKEN')
response <- httr::GET(url, httr::add_headers(Authorization = paste('token', GITHUB_PAT)))
print(response)
rate_limit_info <- httr::content(response)
print(rate_limit_info)
print(rate_limit_info$resources$core$remaining)
