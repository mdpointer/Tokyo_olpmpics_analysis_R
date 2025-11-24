library(tidyverse)

## GENDER PARTICIPATION ANALYSIS

# _____________________________________________
# Gender balance per sport

gender_balance <- dd_gender %>%
  mutate(
    female = as.numeric(female),
    male   = as.numeric(male),
    total  = as.numeric(total),
    f_ratio = female / total
  )
head(gender_balance)

# Plot: Gender balance per sport
plot_gender_balance_per_sport <- ggplot(gender_balance,
       aes(x = fct_reorder(play, f_ratio), y = f_ratio)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Female Ratio by Sport",
    x = "Sport",
    y = "Female Ratio"
  ) +
  theme_bw()


# ----------------------------------------------------------
# Gender balance per country (estimated from sport-level ratios)


# attach gender ratio per sport to each athlete
athletes_with_gender <- dd_athletes %>%
  left_join(
    gender_balance %>% 
      dplyr::select(play, f_ratio),
    by = "play"
  )

# compute *estimated* female participation share per country
country_gender_balance <- athletes_with_gender %>%
  group_by(country) %>%
  summarise(
    athletes = n(),
    avg_female_ratio = mean(f_ratio, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_female_ratio))

# Print head
head(country_gender_balance)

# Optional: keep only countries with enough athletes (reduce noise)
country_gender_filtered <- country_gender_balance %>%
  filter(athletes >= 20)  # threshold you can adjust
country_gender_filtered %>%
  filter(country=="Great Britain")



# ----------------------------------------------------------
# Estimated female participation ratio by country

plot_female_prop_by_country <- country_gender_filtered %>% 
  ggplot(
       aes(x = fct_reorder(country, avg_female_ratio),
           y = avg_female_ratio)) +
  geom_col(fill = "orchid") +
  coord_flip() +
  labs(
    title = "Estimated Female Participation Ratio by Country",
    x = "Country",
    y = "Estimated Female Participation Share"
  )
