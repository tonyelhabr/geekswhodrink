![Lifecycle: work in progress](https://img.shields.io/badge/lifecycle-work%20in%20progress-blue.svg)

## Introduction

This repo stores public [*Geeks Who Drink*](https://www.geekswhodrink.com/) quiz results from teams from all United States locations.

*Geeks Who Drink* is a company that hosts pub quizzes and trivia events across the United States and in some international locations. Founded in 2006, the company hosts weekly quizzes at bars and restaurants, as well as special events and themed quizzes. Their quizzes cover a wide range of topics, including pop culture, history, science, and more, and are designed to be fun and entertaining for participants of all levels of knowledge. *Geeks Who Drink* has gained a reputation for hosting some of the best pub quizzes around and has been featured in media outlets such as The New York Times and NPR.

## Data Dictionary

Data can be found in `data/final/`

### quiz-results.csv

-   `venue_id`: venue's ID (URL element)
-   `quiz_date`: date that quiz game was played
-   `placing`: team's placing at venue, based on descending score
-   `team`: team's name
-   `score`: team's score

### all-venues.csv

-   `venue_id`: venue's ID (URL element)
-   `url`: venue's Geeks Who Drink landing page
-   `lat`: venue's latitude
-   `lon`: venue's longitude
-   `name`: venue's name
-   `address`: venue's address
