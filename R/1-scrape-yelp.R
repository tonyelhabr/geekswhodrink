library(yelp)
library(readr)
library(stringr)
library(dplyr)
library(purrr)
library(lubridate)
library(cli)
library(tibble)
library(stringdist)

source(file.path('R', 'helpers-geekswhodrink.R'))
store_access_token(Sys.getenv('YELP_ACCESS_TOKEN'))

TIMESTAMP <- now()
ADDRESS_SUFFIX_REGEX <- '\\,\\s([A-Z]{2})\\s([0-9]+$)'
str_replace_address <- function(x, i) {
  str_replace(x, paste0('(^.*)', ADDRESS_SUFFIX_REGEX), sprintf('\\%s', i))
}

venues <- read_geekswhodrink_release('venues', tag = 'data') |> 
  arrange(venue_id) |> 
  transmute(
    venue_id,
    name,
    full_address = address,
    address_no_city_state = str_remove(full_address, ADDRESS_SUFFIX_REGEX),
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

get_yelp_business_match <- function(venue_id, match_threshold = 'default') {
  venue <- venues |> filter(.data$venue_id == .env$venue_id)
  existing_business_id <- possibly_read_geekswhodrink_release(venue_id, tag = 'yelp-businesses')
  if (nrow(existing_business_id) > 0) {
    ## this is for when we've already tried to scrape with both default and none, yet neither worked, so we just saved the venue_id
    ##   in its own file, as a sentinel value to indicate that we shouldn't try scraping it anymore
    if (match_threshold == 'default') {
      cli_inform('Trying to re-scrape {.var venue_id} = {.val {venue_id}} with {.var match_threshold} = {.code "none"}.')
      match_threshold <- 'none'
    } else {
      cli_inform('Returning early for {.var venue_id} = {.val {venue_id}}.')
      return(existing_business_id)
    }
  }
  
  Sys.sleep(runif(1, min = 0.1, max = 1.5))
  cli_inform('Scraping {.var venue_id} = {.val {venue_id}}.')
  ## I think the API works best if you have lat + lon (and can enter dummy info for several other things)
  res <- possibly_business_match(
    ## some places weren't working because the names was too long. after some experimenting it seems that 64 characters is the limit
    substr(venue$name, 1, 64),
    ## It's sort of hard to come up with a set of that is 100% accurate in parsing this
    ##   out from the venue address. Nonetheless, the yelp API seems to work fine without city.
    city = 'foo',
    latitude = venue$lat,
    longitude = venue$lon,
    address1 = venue$address_no_city_state,
    state = venue$state,
    zip_code = venue$zip,
    country = 'US',
    match_threshold = match_threshold
  )
  
  should_try_no_match_threshold <- nrow(res) == 0& match_threshold == 'default'
   if (isTRUE(should_try_no_match_threshold)) {
     res <- tibble(venue_id = venue_id, updated_at = TIMESTAMP) 
   }
  
  res$updated_at <- TIMESTAMP
  write_csv(res, path, na = '')
  if (isTRUE(should_try_no_match_threshold)) {
    get_yelp_business_match(venue_id, match_threshold = 'none')
  }
  res
}

map_dfr_venues <- function(x, ..., venue_ids = x) {
  set_names(x, venue_ids) |> 
    map_dfr(..., .id = 'venue_id') |> 
    mutate(
      across(venue_id, as.numeric)
    )
}

yelp_businesses <- map_dfr_venues(
  venues$venue_id,
  venue_ids = venues$venue_id,
  ~{
    res <- get_yelp_business_match(.x)
    mutate(
      res, 
      across(any_of('zip_code'), as.double),
      across(any_of('phone'), as.character)
    )
  }
)

n_yelp_businesses <- yelp_businesses |> count(venue_id)

disambiguiated_yelp_businesses <- yelp_businesses |> 
  semi_join(
    n_yelp_businesses |> filter(n > 1L),
    by = join_by(venue_id)
  ) |> 
  inner_join(
    venues |> 
      transmute(
        venue_id, 
        venue_name = name |> str_remove('\\(.*$')
      ),
    by = join_by(venue_id)
  ) |> 
  select(venue_id, business_id, yelp_name = name, venue_name) |> 
  mutate(
    lv = stringdist(
      tolower(yelp_name),
      tolower(venue_name),
      method = 'lv'
    )
  ) |> 
  group_by(venue_id) |> 
  slice_min(lv, n = 1, with_ties = FALSE) |> 
  ungroup() |> 
  select(venue_id, business_id) |> 
  inner_join(
    yelp_businesses,
    by = join_by(venue_id, business_id)
  )

chosen_yelp_businesses <- bind_rows(
  yelp_businesses |> 
    semi_join(
      n_yelp_businesses |> filter(n == 1L),
      by = join_by(venue_id)
    ),
  disambiguiated_yelp_businesses
) |> 
  arrange(venue_id)
scrapable_yelp_businesses <- chosen_yelp_businesses |> filter(!is.na(business_id))
scrapable_yelp_businesses$updated_at <- TIMESTAMP
write_geekswhodrink_release(
  scrapable_yelp_businesses,
  name = 'yelp-businesses',
  tag = 'data'
)

possibly_business_lookup <- possibly(
  business_lookup,
  otherwise = tibble(),
  quiet = FALSE
)

get_yelp_business_info <- function(business_id) {
  existing_business_info <- possibly_read_geekswhodrink_release(business_id, tag = 'yelp-business-info')
  if (nrow(existing_business_info) > 0) {
    cli_inform('Returning early for {.var venue_id} = {.val {venue_id}}.')
    return(existing_business_info)
  }
  Sys.sleep(runif(1, min = 0.1, max = 1.5))
  cli_inform('Scraping {.var business_id} = {.val {business_id}}.')
  res <- possibly_business_lookup(business_id)
  res$updated_at <- TIMESTAMP
  write_csv(res, path, na = '')
  res
}

yelp_business_info <- map_dfr_venues(
  scrapable_yelp_businesses$business_id,
  venue_ids = scrapable_yelp_businesses$venue_id,
  ~{
    res <- get_yelp_business_info(.x)
    mutate(
      res,
      across(any_of(c('rating', 'zip_code')), as.double),
      across(any_of(c('phone')), as.character)
    )
  }
)

write_geekswhodrink_release(
  yelp_business_info,
  name = 'yelp-business-info',
  tag = 'data'
)
