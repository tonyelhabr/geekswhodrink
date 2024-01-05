![Lifecycle: work in progress](https://img.shields.io/badge/lifecycle-work%20in%20progress-blue.svg)

## Introduction

This repo stores public [*Geeks Who Drink*](https://www.geekswhodrink.com/) quiz results from teams from all United States locations.

*Geeks Who Drink* is a company that hosts pub quizzes and trivia events across the United States and in some international locations. Founded in 2006, the company hosts weekly quizzes at bars and restaurants, as well as special events and themed quizzes. Their quizzes cover a wide range of topics, including pop culture, history, science, and more, and are designed to be fun and entertaining for participants of all levels of knowledge. *Geeks Who Drink* has gained a reputation for hosting some of the best pub quizzes around and has been featured in media outlets such as The New York Times and NPR.

## Repo Structure

1. There are a set of R and python scripts in `scripts/` for data ingestion.
    - `scripts/src/`: Python modules for functions for python script.
    - `scripts/helpers-geekswhodrink`: R functions for R scripts.
    - `scripts/[012]-*[.](R|py)`: Scraping and ingestion
2. There is a [Quarto](https://quarto.org/) [dashboard](https://tonyelhabr.github.io/geekswhodrink/) defined at the top-level of this project.
    - `index.qmd`: Dashboard
    - `_quarto.yml`: Configuration
    - `assets/styles.scss`: Custom theming
3. Data is stored in [GitHub releases](https://github.com/tonyelhabr/geekswhodrink/releases).

Python package management is handled with [poetry](https://python-poetry.org/).
- `pyproject.toml`, `poetry.lock`: Files associated with [poetry](https://python-poetry.org/) for package managements.
- `.venv/` is the virtual environment folder.
`.env` has environment variables for local development 

R package management is handled with [{renv}](https://rstudio.github.io/renv/).
- `renv.lock`, `renv/`: `{renv}` files.
- `"data"` profile for data ingestion; `"viz"` profile for dashboarding.

## Available Data

| File | Download |
| :----| :------- |
| `venues.csv` | [Download](https://github.com/tonyelhabr/geekswhodrink/releases/download/data/venues.csv) |
| `venue-info.csv` | [Download](https://github.com/tonyelhabr/geekswhodrink/releases/download/data/venue-info.csv) |
| `quiz-results.csv` | [Download](https://github.com/tonyelhabr/geekswhodrink/releases/download/data/quiz-results.csv) |

Individual venue quiz results can be downloaded at `https://github.com/tonyelhabr/geekswhodrink/releases/download/venue-quiz-results/{venue_id}.json`

### GitHub Actions

| Action |
| :------|
| [![Scrape venues](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-venues.yml/badge.svg)](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-venues.yml) |
| [![Scrape venue info](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-venue-info.yml/badge.svg)](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-venue-info.yml) |
| [![Scrape quiz results](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-quiz-results.yml/badge.svg)](https://github.com/tonyelhabr/geekswhodrink/actions/workflows/scrape-quiz-results.yml) |


### Dictionary

#### venues.csv

List of venues that have hosted Geeks Who Drink quizzes.

-   `venue_id`: venue's ID
-   `url`: venue's Geeks Who Drink landing page
-   `lat`: venue's latitude
-   `lon`: venue's longitude
-   `name`: venue's name
-   `address`: venue's address

Some locations have not yet hosted a quiz or no longer host quizzes.

#### quiz-results/`{venue_id}`.json

Each venue's quiz results come with the following structure.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "patternProperties": {
    "^[0-9]{4}-W[0-9]{2}$": {
      "type": "object",
      "properties": {
        "meta": {
          "type": "object",
          "properties": {
            "quiz_date": { "type": "string", "format": "date" },
            "n_teams": { "type": "integer" },
            "max_score": { "type": "integer" },
            "min_score": { "type": "integer" },
            "3rd_score": { "type": "integer" },
            "updated_at": { "type": "string", "format": "date-time" }
          },
          "required": [
            "quiz_date",
            "n_teams", 
            "max_score", 
            "min_score", 
            "3rd_score", 
            "updated_at"
          ]
        },
        "results": {
          "type": "array",
          "items": {
            "type": "object",
            "properties": {
              "placing": { "type": "integer" },
              "team": { "type": "string" },
              "score": { "type": "integer" }
            },
            "required": ["placing", "team", "score"]
          }
        }
      },
      "required": ["meta", "results"]
    }
  }
}
```

For example, for a quiz run on Feb. 16, 2023, (the seventh week of 2023) where four teams competed, the venue's results would look like this.

```json
{
  "2023-W07": {
    "meta": {
      "quiz_date": "2023-02-16",
      "n_teams": 4,
      "max_score": 91,
      "min_score": 70,
      "3rd_score": 83,
      "updated_at": "2023-02-20 10:09:42"
    },
    "results": [
      {
        "placing": 1,
        "team": "Best Place First Place",
        "score": 91
      },
      {
        "placing": 2,
        "team": "I'm just here so I don't get fined",
        "score": 88
      },
      {
        "placing": 3,
        "team": "That's what she said",
        "score": 83
      },
      {
        "placing": 3,
        "team": "We're on a boat",
        "score": 70
      }
    ]
  }
}
```

The `meta` element is a dictionary that stores information about the results across all teams. The `results` element is an array of dictionaries, where each dictionary stores information about an individual team's results for the quiz.

