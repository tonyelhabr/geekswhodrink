library(yelp)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
store_access_token(Sys.getenv('YELP_ACCESS_TOKEN'))
store_access_token('DW4rR-HA2A4cRL20zxefH7lADdRb7tQmimexUyW4nB65lPtYpsOcWtB7XqhxPMi3h8yzTAq-uTbRML1KosVqnPqEaHmjloZeG-O2KAvynhqe7mvzESvZP4-ge_gOYXYx')

str_replace_address <- function(x, i) {
  str_replace(x, '(^.*)\\s([A-Za-z]+)\\,\\s([A-Z]{2})\\s([0-9]{5}$)', sprintf('\\%s', i))
}

venues <- read_csv('https://github.com/tonyelhabr/geekswhodrink/releases/download/data/venues.csv') |> 
  transmute(
    venue_id,
    name,
    full_address = address,
    address_no_city_state = str_remove(full_address, '\\,\\s([A-Z]{2})\\s([0-9]{5}$)$'),
    # city = str_replace_address(full_address, 2),
    state = str_replace_address(full_address, 3),
    zip = str_replace_address(full_address, 4),
    lat,
    lon
  )

possibly_business_match <- possibly(
  business_match,
  otherwise = tibble(),
  quiet = FALSE
)

get_yelp_business_id <- function(venue_id, overwrite = FALSE) {
  venue <- venues |> filter(.data$venue_id == .env$venue_id)
  path <- file.path('data', 'yelp-businesses', paste0(venue_id, '.csv'))
  if (file.exists(path) & !overwrite) {
    return(read_csv(path))
  }
  Sys.sleep(runif(1, min = 0.1, max = 1.5))
  cli::cli_inform('Scraping {.var venue_id} = {.val {venue_id}}.')
  res <- possibly_business_match(
    venue$name,
    city = 'foo',
    latitude = venue$lat,
    longitude = venue$lon,
    address1 = venue$address_no_city_state,
    state = venue$state,
    zip_code = venue$zip,
    country = 'US',
    match_threshold = 'default'
  )
  write_csv(res, path, na = '')
  return(res)
}
# 2156400696, 2210632375
yelp_business_ids <- venues$venue_id |> 
  map_dfr(get_yelp_business_id)

tokens <- venues |> 
  mutate(
    address = str_remove(full_address, '\\,\\s([A-Z]{2})\\s([0-9]{5}$)$') |> 
      str_replace('(Los|San|New|Fort|El|Santa)\\s', '\\1') |> 
      str_replace('\\s(City|Springs|Rapids)', '\\1')
  ) |> 
  select(address) |> 
  tidytext::unnest_tokens(address, output = 'token', to_lower = FALSE)
token_counts <- tokens |>
  count(token, sort = TRUE) |> 
  filter(token |> str_detect('^[A-Z]')) |> 
  filter(nchar(token) > 3) |> 
  filter(
    !(token %in% c('Blvd', 'Street', 'Suite', 'Main', 'Drive', 'Avenue', 'Park', 'North', 'South', 'East', 'West', 'Grand', 'Road', 'Creek', 'Lake', 'Center', 'College', 'Pkwy', 'Central', 'Unit', 'Broadway', 'Centennial', 'University', 'Market', 'Overland'))
  ) |> 
  mutate(
    token = str_replace(token, '(^.*[a-z])([A-Z][a-z]+)', '\\1 \\2')
  )
token_counts

venues |> 
  filter(
    full_address |> str_detect('Denver')
  ) |> 
  select(full_address) |> 
  mutate(
    z = str_replace(full_address, '(^.*)\\s([A-Za-z]+(?:\\s[A-Za-z]+))\\,\\s([A-Z]{2})\\s([0-9]{5}$)', '\\2')
  )

venues |> 
  mutate(
    address1_suffix = full_address |> str_detect('\\sBroadway\\s|\\s(St|Ave|Blvd|Dr|Ln|Pkwy|Rd|Ter)[.]?\\s|\\s(ST|AVE|BLVD|DR|LN|PKWY|RD|TER)\\s|\\s(Street|Avenue|Drive|Court|Way)\\s')
  ) |> 
  filter(!address1_suffix) |> 
  select(full_address)

venue <- venues |> filter(name |> str_detect('Red Horn'))
venue <- venues[1, ]
res <- business_match(
  venue$name,
  city = 'foo',
  latitude = venue$lat,
  longitude = venue$lon,
  address1 = venue$address_no_city_state,
  state = venue$state,
  zip_code = venue$zip,
  country = 'US',
  match_threshold = 'default'
)

library(httr)

url <- "https://api.yelp.com/v3/businesses/matches"

queryString <- list(
  limit = "3",
  match_threshold = "default"
)

response <- VERB("GET", url, query = queryString, content_type("application/octet-stream"), accept("application/json"))

content(response, "text")
