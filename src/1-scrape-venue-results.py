#%%
import os
import time
import random
import pandas as pd
from bs4 import BeautifulSoup
import requests
from functools import wraps

venues = pd.read_csv('../all-venues.csv')
DATA_DIR = 'data/all'
os.makedirs(DATA_DIR, exist_ok=True)

BASE_URL = 'https://www.geekswhodrink.com/'


def create_session_for_geekswhodrink_page(venue_id, page=1):
    url = f"{BASE_URL}venues/{venue_id}/?pag={page}"
    response = requests.get(url)
    response.raise_for_status()
    return BeautifulSoup(response.content, 'html.parser')


def safely(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return {'result': func(*args, **kwargs), 'error': None}
        except Exception as e:
            return {'result': None, 'error': e}
    return wrapper

@safely
def scrape_tables_from_geekswhodrink_venue_page(venue_id, page):
    session = create_session_for_geekswhodrink_page(venue_id, page)

    time.sleep(random.uniform(1, 2))
    quiz_dates = [elem.get_text(strip=True) for elem in session.select('.quiz__title')]
    n_quiz_dates = len(quiz_dates)
    if n_quiz_dates == 0:
        raise ValueError('No quiz dates found.')

    time.sleep(random.uniform(1, 2))
    tables = session.select('table')
    dfs = []
    for table in tables:
        df = pd.read_html(str(table))[0]
        if set(['Place Ranking', 'Team Name', 'Score']).issubset(df.columns):
            dfs.append(df)

    n_tbs = len(dfs)
    if n_tbs == 0:
        raise ValueError('No tables found.')

    if n_tbs != n_quiz_dates:
        print(f'Number of tables ({n_tbs}) is different from number of quiz dates ({n_quiz_dates}).')
        if n_tbs < n_quiz_dates:
            quiz_dates = quiz_dates[:n_tbs]

    for idx, df in enumerate(dfs):
        df['quiz_date'] = quiz_dates[idx]

    return pd.concat(dfs, ignore_index=True)
  
#%%
@safely
def example_function(x, y):
    return x / y

result = example_function(1, 0)
print(result)
#%%
