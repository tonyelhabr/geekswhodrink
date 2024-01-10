# %%
import pandas as pd
from bs4 import BeautifulSoup
from selenium import webdriver
from selenium.common.exceptions import NoSuchElementException
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import chromedriver_autoinstaller
from datetime import datetime
from urllib.parse import urlencode, urlunparse
from dotenv import load_dotenv
import time

from src.utils import create_or_update_geekswhodrink_release

load_dotenv()

# %%
chromedriver_autoinstaller.install()
chrome_options = webdriver.ChromeOptions()
options = []

for option in options:
    chrome_options.add_argument(option)

#%%
query_params = {"search": "", "location": ""}
encoded_params = urlencode(query_params)
url = urlunparse(
    ("https", "www.geekswhodrink.com", "venues", "", encoded_params, "")
)

driver = webdriver.Chrome(options=chrome_options)
driver.get(url)
soup0 = BeautifulSoup(driver.page_source, "html.parser")
print(str(soup0)[:100])
try:
    # Look for the popup button by its class and type attributes
    popup_button = WebDriverWait(driver, 5).until(
        EC.presence_of_element_located(
            (
                By.XPATH,
                '//button[@class="pum-close popmake-close" and @type="button"]',
            )
        )
    )
    print('popup button found')
    # Close the popup by clicking the button
    popup_button.click()
except NoSuchElementException:
    print("Popup button not found.")
    
#%%
time.sleep(5)
soup = BeautifulSoup(driver.page_source, "html.parser")
print(str(soup)[:200])
#%%
results = soup.find_all("div", class_="find__col find__col--list")

#%%
data = []
for result in results:
    quiz_blocks = result.find_all("a", class_="quizBlock-returned")
    print(len(quiz_blocks))
    for i, quiz_block in enumerate(quiz_blocks):
        data.append(
            {
                "venue_id": quiz_block["data-podio"],
                "url": quiz_block["href"],
                "lat": quiz_block["data-lat"],
                "lon": quiz_block["data-lon"],
                "name": quiz_block["data-title"],
                "address": quiz_block["data-address"],
            }
        )

venues = pd.DataFrame(data)
driver.quit()

current_time = datetime.now()
venues["updated_at"] = current_time.strftime("%Y-%m-%d %H:%M:%S")

create_or_update_geekswhodrink_release(df=venues, file_name="venues.csv")

# %%
print(venues.shape)

# %%
