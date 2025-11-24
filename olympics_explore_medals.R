library(tidyverse)

# ----------------------------------------------------------
# MEDAL ANALYSIS — TOTAL MEDALS, GOLD MEDALS, WEIGHTED SCORE

# Compute all metrics
tokyo_ranked <- dd_tokyo_medals %>%
  mutate(
    gold_medal   = as.numeric(gold_medal),
    silver_medal = as.numeric(silver_medal),
    bronze_medal = as.numeric(bronze_medal),
    total        = as.numeric(total),
    weighted_score = 3*gold_medal + 2*silver_medal + bronze_medal
  )



# ----------------------------------------------------------
##### Compare different metrics by country
tokyo_comparison <- tokyo_ranked %>%
  dplyr::select(
    country,
    total_medals = total,
    gold_medals  = gold_medal,
    weighted_score
  ) %>%
  arrange(desc(weighted_score))

head(tokyo_comparison, 20)



# ----------------------------------------------------------
# Plot comparing metrics

tokyo_long <- tokyo_ranked %>%
  dplyr::select(country, total_medals = total, gold_medals = gold_medal, weighted_score) %>%
  pivot_longer(
    cols = c(total_medals, gold_medals, weighted_score),
    names_to = "metric",
    values_to = "value"
  )

plot_combined_metrics <- tokyo_long %>% 
  ggplot(
    aes(x = reorder(country, value), y = value, fill = metric)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~metric, scales = "free_x") +
  coord_flip() +
  labs(
    title = "Comparison of Total Medals, Gold Medals, and Weighted Scores",
    x = "Country",
    y = "Value"
  ) +
  scale_fill_manual(values = c(
    total_medals = "steelblue",
    gold_medals = "goldenrod",
    weighted_score = "purple3"
  )) +
  theme_bw()

