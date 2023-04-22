#%%
import os
import time
import random
import pandas as pd
from bs4 import BeautifulSoup
import requests
from functools import wraps
from datetime import datetime

TIMESTAMP = datetime.now()
BASE_URL = 'https://www.geekswhodrink.com/'
FINAL_DATA_DIR = 'data/final'
RAW_DATA_DIR = 'data/raw'
os.makedirs(FINAL_DATA_DIR, exist_ok=True)
os.makedirs(RAW_DATA_DIR, exist_ok=True)

venues = pd.read_csv(os.path.join(FINAL_DATA_DIR, 'all-venues.csv'))

def possibly(otherwise=None):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            try:
                return func(*args, **kwargs)
            except Exception:
                return otherwise
        return wrapper
    return decorator

def safely(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return {'result': func(*args, **kwargs), 'error': None}
        except Exception as e:
            return {'result': None, 'error': e}
    return wrapper

def create_session_for_geekswhodrink_page(venue_id, page=1):
    url = f"{BASE_URL}venues/{venue_id}/?pag={page}"
    response = requests.get(url)
    response.raise_for_status()
    return BeautifulSoup(response.content, 'html.parser')

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

@possibly(otherwise=pd.DataFrame())
def scrape_geekswhodrink_venue_quiz_results(venue_id):
    print(f'Scraping `venue_id` = {venue_id}.')
    p1_session = create_session_for_geekswhodrink_page(venue_id, page=1)
    time.sleep(random.uniform(1, 2))
    page_links = [elem.get_text(strip=True) for elem in p1_session.select('.quiz__pag > *')]

    if len(page_links) == 0:
        print("Couldn't find any page links on the first page of the venue.")
        res = scrape_tables_from_geekswhodrink_venue_page(venue_id, page=1)
        if res['warning'] is not None:
            print(f"Warning: {res['warning']}")
        return res

    last_valid_page = int(page_links[-2])

    if last_valid_page is None:
        print(f"{last_valid_page} is not a number. {page_links} has length {len(page_links)}.")

    print(f"There {'is' if last_valid_page == 1 else 'are'} {last_valid_page} page{'s' if last_valid_page > 1 else ''}.")

    tbs = []
    for i in range(1, last_valid_page + 1):
        print(f'Scraping page {i}.')
        res = scrape_tables_from_geekswhodrink_venue_page(venue_id, page=i)
        if res['error'] is not None:
            print(f"Warning: {res['error']}")
            break
        if res['warning'] is not None:
            print(f"Warning: {res['warning']}")
        if res['result'] is not None:
            tbs.append(res['result'])
        time.sleep(random.uniform(1, 3))

    return pd.concat(tbs, ignore_index=True)

def scrape_and_cache_geekswhodrink_venue_quiz_results(venue_id, overwrite=False):
    path = os.path.join(RAW_DATA_DIR, f'{venue_id}.csv')
    if os.path.exists(path) and not overwrite:
        print(f'Reading in pre-saved results for {venue_id} = {venue_id}.')
        return pd.read_csv(path)

    res = scrape_geekswhodrink_venue_quiz_results(venue_id)
    res['venue_id'] = venue_id
    res['updated_at'] = TIMESTAMP
    res.to_csv(path, index=False)
    return res

#%%
quiz_results = pd.concat(
    [scrape_and_cache_geekswhodrink_venue_quiz_results(venue_id) for venue_id in venues['venue_id']],
    ignore_index=True
)

#%%
quiz_results = quiz_results.rename(columns={
    'Place Ranking': 'placing',
    'Team Name': 'team',
    'Score': 'score'
})

# Assuming you have pandas.to_datetime function imported
quiz_results['quiz_date'] = pd.to_datetime(quiz_results['quiz_date'], format='%m-%d-%Y')

quiz_results.to_csv(os.path.join(FINAL_DATA_DIR, 'quiz-results.csv'), index=False)
