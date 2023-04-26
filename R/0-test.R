library(rvest)
library(readr)

BASE_URL <- 'https://www.geekswhodrink.com/'

venues <- read_csv('https://github.com/tonyelhabr/geekswhodrink/releases/download/data/austin-venues.csv')

create_session_for_geekswhodrink_page <- function(venue_id, page = 1) {
  url <- paste0(BASE_URL, 'venues/', venue_id, '/?pag=', page)
  
  read_html_live(url)
}
session <- create_session_for_geekswhodrink_page(74055573)
print('hola')
