![Lifecycle: work in progress](https://img.shields.io/badge/lifecycle-work%20in%20progress-blue.svg)

## Introduction

This repo stores public [*Geeks Who Drink*](https://www.geekswhodrink.com/) quiz results from teams from all United States locations.

*Geeks Who Drink* is a company that hosts pub quizzes and trivia events across the United States and in some international locations. Founded in 2006, the company hosts weekly quizzes at bars and restaurants, as well as special events and themed quizzes. Their quizzes cover a wide range of topics, including pop culture, history, science, and more, and are designed to be fun and entertaining for participants of all levels of knowledge. *Geeks Who Drink* has gained a reputation for hosting some of the best pub quizzes around and has been featured in media outlets such as The New York Times and NPR.

## Data

| File | Download |
| :----- | :------- |
| `venues.csv` | [Download](https://github.com/tonyelhabr/geekswhodrink/releases/download/data/venues.csv) |
| `quiz-results.json` | Coming soon! |

### GitHub Actions

| Action |
| ------ |
| [![Scrape Geeks Who Drink venues](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-geekswhodrink-venues.yml/badge.svg)](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-geekswhodrink-venues.yml) |
[![Scrape stale Geeks Who Drink quiz results](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-stale-geekswhodrink-quiz-results.yml/badge.svg)](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-stale-geekswhodrink-quiz-results.yml)
[![Scrape new Geeks Who Drink quiz results](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-new-geekswhodrink-quiz-results.yml/badge.svg)](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-new-geekswhodrink-quiz-results.yml)

### Dictionary

#### venues.csv

-   `venue_id`: venue's ID
-   `url`: venue's Geeks Who Drink landing page
-   `lat`: venue's latitude
-   `lon`: venue's longitude
-   `name`: venue's name
-   `address`: venue's address

#### quiz-results/`{venue_id}`.json

Each venue's quiz results come with a `meta` element, that is effectively one array per quiz date. This array stores meta-level information about the results. The `results` element contain the scores for each team for every quiz date. The placing of a team 

```json
{
  "meta": {
    "{quiz year}": {
      "{quiz week}": {
        "n_teams": {n_teams},
        "max_score": {max_score},
        "min_score": {min_score},
        "3rd_score": {3rd_score},
        "updated_at": {updated_at}
      }
    }
  },
  "results": {
    "{quiz year}": {
      "{quiz week}": [
          {
            "placing": {placing},
            "team": "{team}",
            "score": {score}
          }
      ]
    }
  }
}
```

For example, for a quiz run on Feb. 16, 2023, (the seventh week of 2023) where 3 teams competed, the venue's results would look like this

```json
{
  "meta": {
    "2023": {
      "07": {
        "n_teams": 3,
        "max_score": 91,
        "min_score": 83,
        "3rd_score": 83,
        "updated_at": "2023-02-20 10:09:42"
      }
    }
  },
  "results": {
    "2023": {
      "07": [
          {
            "team": "Best Place First Place",
            "score": 91
          },
          {
            "team": "I'm just here so I don't get fined",
            "score": 88
          },
          {
            "team": "That's what she said",
            "score": 83
          }
      ]
    }
  }
}
```

