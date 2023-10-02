library(readr)
library(dplyr)
library(tidyr)
library(lubridate)

read_geekswhodrink_release <- function(x) {
  read_csv(sprintf('https://github.com/tonyelhabr/geekswhodrink/releases/download/data/%s.csv', x))
}
## maybe add an 'all_scores_missing' column so that it's easier to filter for it down the line?
raw_quiz_results <- read_geekswhodrink_release('quiz-results') |> 
  mutate(
    week = floor_date(quiz_date, unit = 'week', week_start = 'Monday')
  )
raw_venues <- read_geekswhodrink_release('venues') |> 
  ## don't need url
  select(
    venue_id,
    name,
    address,
    lat,
    lon
  )

raw_austin_venues <- read_geekswhodrink_release('austin-venues') |> 
  select(venue_id) |> 
  inner_join(
    raw_venues,
    by = join_by(venue_id)
  )

missing_quiz_scores <- raw_quiz_results |> 
  group_by(venue_id, week) |> 
  filter(all(is.na(score))) |> 
  ungroup() 

non_missing_quiz_scores <- raw_quiz_results |> 
  group_by(venue_id, week) |> 
  filter(any(!is.na(score))) |> 
  ungroup()

weeks_with_scores <- full_join(
  missing_quiz_scores |> 
    group_by(venue_id) |> 
    summarize(
      first_week_without_scores = min(week),
      last_week_without_scores = max(week)
    ) |> 
    ungroup(),
  non_missing_quiz_scores |> 
    group_by(venue_id) |> 
    summarize(
      first_week_with_scores = min(week),
      last_week_with_scores = max(week)
    ) |> 
    ungroup(),
  by = join_by(venue_id)
) |> 
  arrange(venue_id)

weekly_quiz_score_summary <- raw_quiz_results |> 
  inner_join(
    weeks_with_scores |> 
      select(
        venue_id,
        first_week_with_scores,
        last_week_with_scores
      ),
    by = join_by(venue_id)
  ) |> 
  filter(
    week >= first_week_with_scores, 
    week <= last_week_with_scores
  ) |> 
  group_by(venue_id, week) |> 
  summarize(
    max_score = max(score, na.rm = TRUE),
    min_score = min(score, na.rm = TRUE)
  ) |> 
  ungroup() |> 
  filter(!(is.nan(max_score) & is.nan(min_score)))

weekly_quiz_participation <- raw_quiz_results |> 
  group_by(venue_id, week) |> 
  summarize(
    n_teams = n(),
    n_teams_without_score = sum(is.na(score))
  ) |> 
  ungroup()

score_by_placing <- raw_quiz_results |> 
  semi_join(
    weekly_quiz_score_summary,
    by = join_by(venue_id, week)
  ) |> 
  inner_join(
    weekly_quiz_participation,
    by = join_by(venue_id, week)
  ) |> 
  group_by(placing, n_teams) |> 
  summarize(
    n = n(),
    max_score = max(score, na.rm = TRUE),
    median_score = median(score, na.rm = TRUE)
  ) |> 
  ungroup()
score_by_placing |> 
  filter(placing == 3) |> 
  filter(n_teams >= 5L)


median_score_by_placing_venue <- raw_quiz_results |> 
  semi_join(
    weekly_quiz_score_summary,
    by = join_by(venue_id, week)
  ) |> 
  inner_join(
    weekly_quiz_participation,
    by = join_by(venue_id, week)
  ) |> 
  filter(n_teams >= 5L, n_teams_without_score == 0L) |> 
  select(
    venue_id,
    week,
    placing,
    score
  ) |> 
  group_by(venue_id, placing) |> 
  summarize(
    n_weeks = n(),
    median_score = median(score)
  ) |> 
  ungroup()

median_score_by_placing_venue |> 
  filter(placing == 3L, n_weeks >= 10L) |> 
  arrange(desc(median_score)) |> 
  mutate(
    rank = row_number(desc(median_score))
  ) |> 
  inner_join(
    raw_venues,
    by = join_by(venue_id)
  ) |> 
  left_join(
    raw_austin_venues |> transmute(venue_id, is_austin = TRUE),
    by = join_by(venue_id)
  ) |> 
  mutate(
    is_austin = coalesce(is_austin, FALSE)
  ) |> 
  filter(is_austin) |> 
  tibble::view()
