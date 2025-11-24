library(tidyverse)
library(patchwork)

## LOAD, CLEAN & CHECK STRUCTURE
source("/Users/mdp/Google Drive/WORK/R/olympics_data/olympics_load_and_clean_data.R")

## EXPLORE THE ATHLETES AND COACHES
source("/Users/mdp/Google Drive/WORK/R/olympics_data/olympics_explore_athletes_and_coaches.R")
head(athletes_by_sport)
plot_athletes_per_sport
head(athletes_by_country)
plot_athletes_per_country
head(sports_diversity)
plot_sports_per_country
head(coach_ratio)
plot_coach_to_athlete_ratio

## EXPLORE GENDER PARTICIPATION
source("/Users/mdp/Google Drive/WORK/R/olympics_data/olympics_explore_gender_balance.R")
head(gender_balance)
plot_gender_balance_per_sport
head(country_gender_filtered)
plot_female_prop_by_country

## EXPLORE MEDAL DISTRIBUTION
source("/Users/mdp/Google Drive/WORK/R/olympics_data/olympics_explore_medals.R")
head(tokyo_ranked)
head(tokyo_comparison, 20)
plot_combined_metrics

## MODEL FACTORS LEADING TO MEDAL HAULS
source("/Users/mdp/Google Drive/WORK/R/olympics_data/olympics_model_medal_haul.R")
summary(nb_model)
plot_neg_binom_model
print(rf_model)
rmse_test
r2_test
varImpPlot(rf_model, type=1)  # type=1 = mean decrease in accuracy
varImpPlot(rf_model, type=2)  # type=2 = mean decrease in node impurity
plot_random_forrest_model

