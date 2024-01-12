source(file.path('scripts', 'helpers.R'))

convert_10_digit_zip_to_5_digit_zip <- function(x) {
  stringr::str_remove(x, '-[0-9]{4}$')
}

ADDRESS_SUFFIX_REGEX <- '\\,\\s([A-Z]{2})\\s([0-9]+$)'
str_replace_address <- function(x, i) {
  stringr::str_replace(x, paste0('(^.*)', ADDRESS_SUFFIX_REGEX), paste0('\\', i))
}

## pull data ----
cli::cli_inform('Pulling in all venues and venue info.')
raw_venues <- read_release_csv(
  name = 'venues',
  tag = 'data'
)

raw_venue_info <- read_release_csv(
  name = 'venue-info',
  tag = 'data'
)

## combine ----
venue_info <- raw_venue_info |> 
  dplyr::transmute(
    venue_id,
    venue_address_line_1,
    venue_address_line_2  = convert_10_digit_zip_to_5_digit_zip(venue_address_line_2),
    ## TODO: Fix 2336767042 and 1260079721
    venue_address = dplyr::na_if(venue_address, 'NA\nNA'),
    venue_link,
    venue_info_updated_at = updated_at,
  )

dashboard_venues <- raw_venues |> 
  dplyr::mutate(
    address = convert_10_digit_zip_to_5_digit_zip(address)
  ) |> 
  dplyr::left_join(
    venue_info,
    by = dplyr::join_by(venue_id)
  ) |> 
  dplyr::transmute(
    venue_id,
    html_venue_id = sprintf("<a href='https://www.geekswhodrink.com/venues/%s' target='_blank'>%s</a>", venue_id, venue_id),
    name,
    html_name = sprintf(
      '%s%s%s', 
      ifelse(
        !is.na(venue_link),
        sprintf("<a href='%s' target='_blank'>", venue_link),
        ''
      ),
      name,
      ifelse(
        !is.na(venue_link),
        '</a>',
        ''
      )
    ),
    ## venue only is in the venue-info release if it has results
    has_results = !is.na(venue_info_updated_at),
    venue_link,
    full_address = dplyr::coalesce(
      venue_address,
      address
    ),
    address_line_1 = dplyr::coalesce(
      venue_address_line_1, 
      stringr::str_remove(address, ADDRESS_SUFFIX_REGEX)
    ),
    # city = dplyr::coalesce(
    #   str_replace_address(venue_address_line_2, 1),
    #   str_replace_address(address, 1)
    # ),
    city = str_replace_address(venue_address_line_2, 1),
    state = dplyr::coalesce(
      str_replace_address(venue_address_line_2, 2),
      str_replace_address(address, 2)
    ),
    zip = dplyr::coalesce(
      str_replace_address(venue_address_line_2, 3),
      str_replace_address(full_address, 3)
    ),
    lat,
    lon
  ) |> 
  dplyr::arrange(state, name)

dashboard_venues$label <- sprintf(
  '<strong>%s</strong><br/><br/>%s<br/>%s, %s<br/><br/><i>Venue ID: %s</i>',
  dashboard_venues$html_name,
  dashboard_venues$address_line_1,
  dashboard_venues$state,
  dashboard_venues$zip,
  dashboard_venues$html_venue_id
) |>
  purrr::map(htmltools::HTML)

write_release_csv(
  dashboard_venues,
  name = 'dashboard-venues',
  tag = 'data'
)
