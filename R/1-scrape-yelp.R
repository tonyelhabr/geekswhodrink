library(yelp)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(cli)

store_access_token(Sys.getenv('YELP_ACCESS_TOKEN'))

str_replace_address <- function(x, i) {
  str_replace(x, '(^.*),\\s([A-Z]{2})\\s([0-9]{5}$)', sprintf('\\%s', i))
}

venues <- read_csv('https://github.com/tonyelhabr/geekswhodrink/releases/download/data/venues.csv') |> 
  transmute(
    venue_id,
    name,
    full_address = address,
    address_no_city_state = str_remove(full_address, '\\,\\s([A-Z]{2})\\s([0-9]{5}$)$'),
    state = full_address |> str_remove('[-][0-9]{4}$') |> str_replace_address(2),
    zip = str_replace_address(full_address, 3),
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
    return(read_csv(path, show_col_types = FALSE))
  }
  Sys.sleep(runif(1, min = 0.1, max = 1.5))
  cli_inform('Scraping {.var venue_id} = {.val {venue_id}}.')
  res <- possibly_business_match(
    venue$name,
    city = 'foo', ## yelp API works fine without this
    ## I think the API works best if you have lat + lon (and can enter dummy info for several other things)
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

map_dfr_venues <- function(...) {
  map_dfr(..., .id = 'venue_id') |> 
    mutate(across(venue_id, as.numeric))
}

yelp_business_ids <- venues$venue_id |> 
  set_names() |> 
  map(get_yelp_business_id) |> 
  discard(~nrow(.x) == 0) |> 
  map_dfr_venues(
    ~mutate(.x, across(zip_code, as.double))
  )

venues |> 
  filter(venue_id %in% setdiff(venues$venue_id, yelp_business_ids$venue_id)) |> 
  select(name) |> 
  tibble::view()
venue <- venues |> filter(venue_id == 2441104256)
venue
res <- possibly_business_match(
  venue$name,
  # name = 'Atomic Cowboy South Broadway',
  city = 'Temple', ## yelp API works fine without this
  ## I think the API works best if you have lat + lon (and can enter dummy info for several other things)
  latitude = venue$lat,
  longitude = venue$lon,
  address1 = venue$address_no_city_state,
  state = venue$state,
  zip_code = venue$zip,
  country = 'US',
  match_threshold = 'none'
)
write_csv(yelp_business_ids, 'data/yelp-business-ids.csv', na = '')

possibly_business_lookup <- possibly(
  business_lookup,
  otherwise = tibble(),
  quiet = FALSE
)

get_yelp_business_info <- function(business_id, overwrite = FALSE) {
  path <- file.path('data', 'yelp-business-info', paste0(business_id, '.csv'))
  if (file.exists(path) & !overwrite) {
    return(read_csv(path, show_col_types = FALSE))
  }
  Sys.sleep(runif(1, min = 0.1, max = 1.5))
  cli_inform('Scraping {.var business_id} = {.val {business_id}}.')
  res <- possibly_business_lookup(business_id)
  write_csv(res, path, na = '')
  return(res)
}

yelp_business_info <- set_names(
  yelp_business_ids$business_id,
  yelp_business_ids$venue_id
) |> 
  map(get_yelp_business_info) |> 
  discard(~nrow(.x) == 0) |> 
  map_dfr_venues(
    ~mutate(.x, across(c(rating, zip_code), as.double)),
    .id = 'venue_id'
  )

write_csv(yelp_business_info, 'data/yelp-business-info.csv', na = '')
possibly_business_lookup('gUVX7GzRoyuUiJ7CaTIXRw') -> res
