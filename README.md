# Tokyo_olympics_analysis_R
An analysis of data from the 2021 Tokyo olympics conducted in R v4.3.3.Data obtained from https://github.com/MainakRepositor/Datasets/tree/master/Tokyo-Olympics
View the full report with visualisations in the olympics_report_markdown.html file.

## Files:
Athletes.csv
Coaches.csv
Gender.csv
Medals.csv
Teams.csv
Tokyo_Medals_2021.csv
olympics_report_markdown.html

## Scritps
olympics_analysis_pipeline.R
olympics_load_and_clean_data.R
olympics_explore_athletes_and_coaches.R
olympics_explore_gender_balance.R
olympics_explore_medals.R
olympics_model_medal_haul.R
olympics_report_markdown.Rmd



# REPORT
## SUMMARY
This report examines what factors explain why some countries win more Olympic medals than others. After cleaning and integrating Tokyo 2020 athlete, coach, gender, and medal data, exploratory analysis showed large differences in team sizes and sport diversity across nations. Major delegations such as the United States, Japan, Australia, China, and major European countries competed in the widest range of sports and brought the largest athlete groups.

Weighted medal scoring confirmed the expected performance hierarchy, with the United States and China leading by a wide margin.

Modelling Medal Haul
Two models were used to identify predictors of medal success:
### Negative Binomial Regression
The model showed that:

Number of athletes and

Number of sports entered
are both strong, significant predictors of total medals.
The average female ratio of a team was not statistically significant once those factors were accounted for.

### 2. Random Forest Regression
The random forest achieved strong predictive accuracy (R² = 0.80 on the test set). Variable importance rankings confirmed the regression findings:

Team size and sport diversity were by far the most influential variables.
Gender-related variables played only a minor role in comparison.

Conclusion
Across both modelling approaches, the evidence clearly shows that delegation scale—how many athletes a country sends and how many sports it participates in—is the dominant determinant of medal success. Structural or demographic variables such as gender balance do not meaningfully affect medal totals at the country level. The models provide consistent, robust support for this conclusion.



## 1. Exploring Athletes and Coaches
# 1.1 Athletes by Sport

We first examined the distribution of athletes across different Olympic sports. The six sports with the largest participation were:

Athletics: 2,068 athletes

Swimming: 743 athletes

Football: 567 athletes

Rowing: 496 athletes

Hockey: 406 athletes

Judo: 373 athletes

These results highlight that Athletics dominates in athlete numbers, followed by Swimming and team sports like Football and Hockey. The distribution reflects the popularity and number of events within each sport. A bar plot of athletes per sport further illustrates the steep drop-off after the largest sports, emphasizing that a small number of sports account for a large share of participants.

# 1.2 Athletes by Country

Next, we looked at the number of athletes sent by each country. The countries with the largest delegations are:

United States of America: 615 athletes

Japan: 586 athletes

Australia: 470 athletes

People’s Republic of China: 401 athletes

Germany: 400 athletes

France: 377 athletes

A bar plot of the top 30 countries shows that these six countries are clear leaders in team size, reflecting both the resources and the breadth of their Olympic programs. Most other countries send considerably smaller delegations.

# 1.3. Sports Diversity by Country

We also measured sports diversity, defined as the number of different sports in which a country participates. The top countries by this metric are:

Japan: 46 sports, 586 athletes

United States of America: 44 sports, 615 athletes

Australia: 41 sports, 470 athletes

France: 41 sports, 377 athletes

People’s Republic of China: 41 sports, 401 athletes

Canada: 40 sports, 368 athletes

The data indicate that countries with large delegations also tend to compete across more sports. A corresponding plot of sports diversity shows that participation breadth often aligns with total delegation size, highlighting comprehensive Olympic programs.

# 1.4. Coach-to-Athlete Ratios

Finally, we examined the ratio of coaches to athletes for each country. Some notable examples include:

San Marino: 2 coaches for 4 athletes (0.5 coaches per athlete)

Cambodia: 1 coach for 3 athletes (0.33)

Venezuela: 10 coaches for 43 athletes (0.23)

Liechtenstein: 1 coach for 5 athletes (0.2)

Nigeria: 9 coaches for 59 athletes (0.15)

Chile: 6 coaches for 56 athletes (0.11)

These ratios reveal that smaller delegations tend to invest more heavily in coaching support per athlete, whereas larger delegations have lower coach-to-athlete ratios due to scale. A bar plot of the top 20 countries by coach-to-athlete ratio emphasizes this trend, showing that small nations prioritize individual support.




## 2. Gender Participation Analysis

We examined the distribution of athletes by gender across different sports and countries to understand patterns of female participation at the Tokyo Olympics.

# 2.1. Gender balance per sport
The gender_balance dataset shows the number of female and male athletes in each sport, along with the total and the female ratio (f_ratio). A few highlights:

Sport	Female	Male	Total	Female Ratio
3x3 Basketball	32	32	64	0.50
Archery	64	64	128	0.50
Artistic Gymnastics	98	98	196	0.50
Artistic Swimming	105	0	105	1.00
Athletics	969	1072	2041	0.475
Badminton	86	87	173	0.497

Some sports, such as Artistic Swimming, are exclusively female, while others like Athletics and Badminton show nearly balanced participation.

The plot_gender_balance_per_sport visualizes the female ratio for all sports, highlighting which sports are male-dominated, female-dominated, or balanced.

# 2.2 Gender balance per country
We aggregated athletes by country and computed the average female ratio (avg_female_ratio) for each delegation:

Country	Athletes	Avg. Female Ratio
Bulgaria	41	0.537
Greece	75	0.528
Azerbaijan	41	0.524
Egypt	133	0.523
Ukraine	152	0.520
Israel	85	0.508

Smaller delegations sometimes have slightly higher female representation.

The plot_female_prop_by_country shows the variation in female participation across countries, emphasizing which countries have more gender-balanced teams.

Summary:
Overall, while some sports and countries maintain gender parity, there remain notable differences in female representation. This analysis provides insight into both sport-specific and country-level patterns in gender participation at the Olympics.




## 3. Medal Analysis: Total Medals, Gold Medals, and Weighted Scores

To compare overall Olympic performance across countries, I examined three complementary metrics:

Total medals won

Gold medals specifically

A weighted medal score, where

Gold = 3 points

Silver = 2 points

Bronze = 1 point

This scoring system rewards both the quality and quantity of medals and provides a more nuanced view of competitive success.

Overall Medal Leaders

The United States leads the ranking across all metrics, with 113 total medals, 39 gold medals, and the highest weighted score (232). China follows closely behind with 88 total medals, 38 golds, and a weighted score of 196. These two countries significantly outperform the rest of the field.

Japan, Great Britain, and the ROC round out the top tier, each combining high medal totals with strong gold-medal counts. Their positions shift slightly depending on the metric. For example, ROC places third in weighted score due to a large number of silver and bronze medals, despite ranking lower in golds.

Metric Comparison and Country Profiles

The comparison table highlights important differences in countries’ medal profiles. Some nations—such as Great Britain and Japan—show relatively balanced distributions across gold, silver, and bronze. Others, such as the Netherlands and Germany, achieve strong overall scores despite fewer gold medals, demonstrating depth rather than dominance.

The weighted score amplifies these distinctions. Countries with modest medal totals but high gold efficiency (e.g., Hungary and Cuba) rise in the rankings, while nations relying on broader medal accumulation without many golds tend to fall slightly.

Visualization

The combined metric plot provides a clear visual summary of how countries perform across total medals, gold medals, and the weighted score. Countries with consistently strong values across all three metrics cluster at the top of the chart, whereas countries with unbalanced medal distributions show divergence between the different ranking measures.

Summary

Overall, the analysis shows that:

Countries with large, well-supported delegations dominate across all medal metrics.

Weighted scoring offers a more stable and informative ranking than gold-medal count alone.

The relative performance of countries can vary substantially depending on the chosen metric, highlighting the importance of examining multiple measures of success.




## 4. Modelling Olympic Medal Outcomes

To understand which country-level factors are most strongly associated with medal success at the Tokyo Olympics, I fitted two different predictive models:

A Negative Binomial regression (suitable for count data with over-dispersion)

A Random Forest regression (a flexible non-linear machine-learning model)

Both models use the same set of predictors derived from the athlete and coaching datasets:

Athletes — total number of athletes a country sent

Sports — number of sports the country competed in

Average female ratio — mean proportion of female participation across sports per country

(Random Forest also used additional engineered predictors such as coach ratio, teams, and athletes per sport)

Results from both approaches tell a consistent story about what drives medal hauls.


# 4.1 Negative Binomial Regression

The Negative Binomial model treats total medals as an over-dispersed count outcome. The model converged successfully with a dispersion parameter θ ≈ 5.16, confirming that over-dispersion was present but manageable.

Key Results
Predictor	Estimate	Interpretation
Athletes	0.00393*	More athletes → higher medal count. This effect is highly significant (p < .001).
Sports	0.0405 **	Competing in additional sports is also positively associated with total medals (p < .01).
Avg. female ratio	0.936 (ns)	No strong or significant relationship between gender balance and medal totals.
Interpretation

The model indicates that the size and breadth of a country’s delegation are the dominant predictors of medal success.

Each additional athlete is associated with a multiplicative increase in expected medals.

Similarly, countries competing in more sports see systematically higher medal totals.

Gender balance does not meaningfully predict medal outcomes, once delegation size and sport diversity are accounted for.

Goodness of Fit

Residual deviance = 87.16 on 88 df, suggesting a reasonable fit.

AIC = 505.6, used for model comparison.

Diagnostic plots (DHARMa) indicated that this model handled over-dispersion better than the Poisson model and showed no strong residual structure.


# 4.2 Random Forest Model

To allow for non-linear patterns and complex interactions, I trained a Random Forest model using a 70/30 train–test split.

Test Performance

RMSE: 12.61

R²: 0.804

This means the model explains approximately 80% of the variation in medal totals on unseen data, substantially better than the Negative Binomial model.

Variable Importance

Both Mean Decrease in Accuracy and Mean Decrease in Node Impurity rankings show the same pattern:

Athletes — the strongest single predictor

Sports — also highly influential

Coaches, team count, and athletes per sport — moderate contributions

Average female ratio — consistently among the least important predictors

Interpretation

The Random Forest confirms the regression findings:

Delegation size and sport diversity are by far the most important drivers of medal success.

Countries with deep coaching support and a broad presence across sports tend to outperform smaller or more specialized delegations.

Gender balance again shows little direct predictive value once other variables are considered.
