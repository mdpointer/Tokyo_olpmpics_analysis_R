library(tidyverse)
library(DHARMa)
library(MASS)
library(randomForest)
library(Metrics)

# ----------------------------------------------------------
# Construct dataframe for modelling

# Athletes per country
athletes_country <- dd_athletes %>%
  count(country, name = "athletes")

# Coaches per country
coaches_country <- dd_coaches %>%
  count(country, name = "coaches")

# Sports diversity (number of sports entered)
sports_country <- dd_athletes %>%
  count(country, play) %>%
  count(country, name = "sports")

# Gender balance per country (estimated)
gender_country <- dd_athletes %>%
  left_join(dd_gender %>% 
              dplyr::select(play, female, male, total), by = "play") %>%
  mutate(f_ratio = as.numeric(female) / as.numeric(total)) %>%
  group_by(country) %>%
  summarise(
    avg_female_ratio = mean(f_ratio, na.rm = TRUE)
  )

# Teams entered per country
teams_country <- dd_teams %>%
  count(name, name = "teams") %>%
  rename(country = name)

# Medals (targets)
medals <- dd_tokyo_medals %>%
  mutate(
    gold_medal   = as.numeric(gold_medal),
    silver_medal = as.numeric(silver_medal),
    bronze_medal = as.numeric(bronze_medal),
    total        = as.numeric(total)
  ) %>%
  transmute(
    country,
    gold_medal,
    silver_medal,
    bronze_medal,
    total_medals = total,
    weighted_score = 3*gold_medal + 2*silver_medal + bronze_medal
  )

# Combine everything into one modeling frame
model_df <- athletes_country %>%
  full_join(coaches_country, by = "country") %>%
  full_join(sports_country, by = "country") %>%
  full_join(gender_country, by = "country") %>%
  full_join(teams_country, by = "country") %>%
  full_join(medals, by = "country") %>%
  replace_na(list(
    coaches = 0, teams = 0, avg_female_ratio = mean(gender_country$avg_female_ratio, na.rm = TRUE)
  )) %>%
  mutate(
    coach_ratio = coaches / athletes,
    athletes_per_sport = athletes / sports
  )

skimr::skim(model_df)


##### model

# lm_model <- lm(
#   total_medals ~ athletes + coaches + sports + avg_female_ratio +
#     coach_ratio + teams + athletes_per_sport,
#   data = model_df
# )
# sim_res <- simulateResiduals(fittedModel = lm_model, n = 1000)
# plot(sim_res)
# testUniformity(sim_res)
# testDispersion(sim_res)
# summary(lm_model)

# standard lm showed non-uniformity of residuals - KS test: p<0.038
# expected for count data
# using negative binomial GLM fixes the issue

nb_model <- glm.nb(total_medals ~ athletes + sports + avg_female_ratio, data = model_df)
sim_res_nb <- simulateResiduals(nb_model, n = 1000)
#plot(sim_res_nb)
#testUniformity(sim_res_nb)
#testDispersion(sim_res_nb)
#summary(nb_model)


# Get rows used in the model
used_rows <- rownames(nb_model$model)
# Subset dataframe
model_df_used <- model_df[rownames(model_df) %in% used_rows, ]
# Add predictions
model_df_used$pred_nb <- predict(nb_model, type = "response")

# Now you can plot
plot_neg_binom_model <- ggplot(model_df_used, aes(x = pred_nb, y = total_medals)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, linetype="dashed", color="red") +
  labs(title="Observed vs Predicted Medals (Negative Binomial)",
       x="Predicted Medals",
       y="Observed Medals") +
  theme_bw()







set.seed(123)

# Subset to complete cases
rf_df <- model_df %>%
  dplyr::select(total_medals, athletes, coaches, sports, avg_female_ratio,
         coach_ratio, teams, athletes_per_sport) %>%
  na.omit()

# Example 70/30 train/test split
n <- nrow(rf_df)
train_index <- sample(seq_len(n), size = 0.7*n)
train_data <- rf_df[train_index, ]
test_data  <- rf_df[-train_index, ]

# Fit RF on training data
rf_model <- randomForest(total_medals ~ ., data = train_data, ntree = 500, importance = TRUE)

# Predict on test data
pred_test <- predict(rf_model, newdata = test_data)

# Evaluate performance
rmse_test <- rmse(test_data$total_medals, pred_test)
r2_test   <- cor(test_data$total_medals, pred_test)^2

# rmse_test
# r2_test
# print(rf_model)

# varImpPlot(rf_model, type=1)  # type=1 = mean decrease in accuracy
# varImpPlot(rf_model, type=2)  # type=2 = mean decrease in node impurity

test_data$pred <- pred_test

plot_random_forrest_model <- ggplot(test_data, aes(x = pred, y = total_medals)) +
  geom_point() +
  geom_abline(slope=1, intercept=0, color="red", linetype="dashed") +
  labs(title="Observed vs Predicted Medals (Random Forest)",
       x="Predicted Medals",
       y="Observed Medals") +
  theme_bw()
