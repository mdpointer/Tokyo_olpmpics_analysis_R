library(tidyverse)

##### Explore the data - athletes and
#####

# _____________________________________________________________
### Number of athletes per sport


athletes_by_sport <- dd_athletes %>%
  count(play, sort = TRUE)

head(athletes_by_sport)

# Plot
plot_athletes_per_sport <- athletes_by_sport %>%
  ggplot(aes(x = fct_reorder(play, n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Number of Athletes per Sport",
    x = "Sport",
    y = "Athletes"
  ) +
  theme_bw()


# _____________________________________________________________
### Number of athletes per country

athletes_by_country <- dd_athletes %>%
  count(country, sort = TRUE)
head(athletes_by_country)

# Plot top 30 countries
plot_athletes_per_country <- athletes_by_country %>%
  slice_max(n, n = 30) %>%
  ggplot(aes(x = reorder(country, n), y = n)) +
  geom_col(fill = "forestgreen") +
  coord_flip() +
  labs(
    title = "Countries With the Largest Delegations (Top 30)",
    x = "Country",
    y = "Athletes"
  ) +
  theme_bw()


# _____________________________________________________________
# Sports diversity per country - How many *different sports* each country participates in?

sports_diversity <- dd_athletes %>%
  count(country, play) %>%
  group_by(country) %>%
  summarise(
    sports = n(),
    total_athletes = sum(n)
  ) %>%
  arrange(desc(sports))
head(sports_diversity)

plot_sports_per_country <- sports_diversity %>%
  slice_max(sports, n = 30) %>%
  ggplot(aes(x = fct_reorder(country, sports), y = sports)) +
  geom_col(fill = "purple3") +
  coord_flip() +
  labs(
    title = "Countries With the Largest Sport Diversity (Top 30)",
    x = "Country",
    y = "Number of Sports Represented"
  ) +
  theme_bw()




# _____________________________________________________________
#Coach-to-athlete ratio by country

coach_ratio <- dd_athletes %>%
  count(country, name = "athletes") %>%
  left_join(dd_coaches %>% count(country, name = "coaches")) %>%
  mutate(
    coaches = replace_na(coaches, 0),
    coach_to_athlete = coaches / athletes
  ) %>%
  arrange(desc(coach_to_athlete))
head(coach_ratio)

plot_coach_to_athlete_ratio <- coach_ratio %>%
  slice_max(coach_to_athlete, n = 20) %>%
  ggplot(aes(x = fct_reorder(country, coach_to_athlete), y = coach_to_athlete)) +
  geom_col(fill = "darkcyan") +
  coord_flip() +
  labs(
    title = "Highest Coach-to-Athlete Ratios (Top 20 Countries)",
    x = "Country",
    y = "Coaches per Athlete"
  ) +
  theme_bw()




