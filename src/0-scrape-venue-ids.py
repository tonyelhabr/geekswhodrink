#%%
import os
import time
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
import chromedriver_autoinstaller
from utils import create_or_update_geekswhodrink_release

os.makedirs(OUTPUT_DIR)
chromedriver_autoinstaller.install()
chrome_options = webdriver.ChromeOptions()    
options = [
  '--headless',
]

for option in options:
  chrome_options.add_argument(option)

driver = webdriver.Chrome(options = chrome_options)

def build_geekswhodrink_venue_search_url(location):
  base_url = 'https://www.geekswhodrink.com/venues/'
  query_params = {'search': '', 'location': location}
  query_string = '&'.join([f'{key}={value}' for key, value in query_params.items()])
  url = f'{base_url}?{query_string}'
  return url

def scrape_geekswhodrink_venues_given_location(driver):
  # url = build_geekswhodrink_venue_search_url(location)
  url = 'https://www.geekswhodrink.com/venues/'
  driver.get(url)
  time.sleep(3)
  soup = BeautifulSoup(driver.page_source, 'html.parser')
  results = soup.find_all('div', class_='find__col find__col--list')
  
  data = []
  for result in results:
    quiz_blocks = result.find_all('a', class_='find__block quizBlock quizBlock-returned')
    for quiz_block in quiz_blocks:
      data.append(
        {
          'venue_id': quiz_block['data-podio'],
          'url': quiz_block['href'], 
          'lat': quiz_block['data-lat'], 
          'lon': quiz_block['data-lon'], 
          'name': quiz_block['data-title'],
          'address': quiz_block['data-address']
        }
      )
      
  return(pd.DataFrame(data))

venues = scrape_geekswhodrink_venues_given_location(driver)
create_or_update_geekswhodrink_release(
  df=venues,
  file_name='venues.csv'
)