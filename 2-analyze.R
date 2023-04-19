library(readr)
library(dplyr)
library(tidyr)

recent_quiz_results <- read_csv('quiz-results.csv') |> 
  group_by(venue_id) |> 
  mutate(
    rn = dense_rank(desc(quiz_date))
  ) |> 
  ungroup() |> 
  filter(rn <= 20)

recent_quiz_results |> 
  count(venue_name, quiz_date, name = 'n_teams') |> 
  group_by(venue_name) |> 
  summarize(
    mean_n_teams = mean(n_teams)
  )

recent_quiz_results |> 
  filter(placing <= 5) |> 
  group_by(
    venue_name,
    placing
  ) |> 
  summarize(
    mean_score = mean(score)
  ) |> 
  ungroup() |> 
  pivot_wider(
    names_from = venue_name,
    values_from = mean_score
  )
