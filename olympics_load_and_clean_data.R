library(tidyverse)
library(janitor)
library(skimr)

#### CLEAN AND TIDY DATA FOR ANALYSIS

## ASSIGN FILES
file_location <- "/Users/mdp/Google Drive/WORK/R/olympics_data"
athletes_file <- "Athletes.csv"
coaches_file <- "Coaches.csv"
gender_file <- "Gender.csv"
medals_file <- "Medals.csv"
teams_file <- "Teams.csv"
tokyo_medals_file <- "Tokyo_Medals_2021.csv"


## LOAD, CLEAN & CHECK STRUCTURE
dd_athletes <- read_csv(paste(file_location, athletes_file, sep="/")) %>%
  clean_names() %>% 
  mutate(across(everything(), str_squish))
skim(dd_athletes)

dd_coaches <- read_csv(paste(file_location, coaches_file, sep="/")) %>%
  clean_names() %>% 
  mutate(across(everything(), str_squish))
skim(dd_coaches)

dd_gender <- read_csv(paste(file_location, gender_file, sep="/")) %>%
  clean_names() %>% 
  mutate(across(everything(), str_squish))
skim(dd_gender)

dd_medals <- read_csv(paste(file_location, medals_file, sep="/")) %>%
  clean_names() %>% 
  mutate(across(everything(), str_squish))
skim(dd_medals)

dd_teams <- read_csv(paste(file_location, teams_file, sep="/")) %>%
  clean_names() %>% 
  mutate(across(everything(), str_squish))
skim(dd_teams)

dd_tokyo_medals <- read_csv(paste(file_location, tokyo_medals_file, sep="/")) %>%
  clean_names() %>% 
  mutate(across(everything(), str_squish))
skim(dd_tokyo_medals)

