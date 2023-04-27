#%%
import time
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
import chromedriver_autoinstaller
from datetime import datetime
from utils import create_or_update_geekswhodrink_release

chromedriver_autoinstaller.install()
chrome_options = webdriver.ChromeOptions()    
options = [
  '--headless',
]

for option in options:
  chrome_options.add_argument(option)

driver = webdriver.Chrome(options = chrome_options)

def scrape_geekswhodrink_venues_given_location(driver):
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
current_time = datetime.now()
venues['updated_at'] = current_time.strftime('%Y-%m-%d %H:%M:%S')

create_or_update_geekswhodrink_release(
  df=venues,
  file_name='venues.csv'
)
