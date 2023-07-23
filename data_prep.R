library(tidyverse)
library(tidymodels)
library(tidypredict)
library(doParallel)
library(vip)
library(yaml)


# Pre-Processing ----

# Parallelize
doParallel::registerDoParallel(cores = 10)

## Read in data ----
cleaned_data <- read_csv("modelling/cleaned_data.csv")

# Set categorical variables as factors
cleaned_data$T1_firstBlood <- as.factor(cleaned_data$T1_firstBlood)
cleaned_data$T1_firstTower <- as.factor(cleaned_data$T1_firstTower)
cleaned_data$T1_win <- as.factor(cleaned_data$T1_win)
cleaned_data$T1_win <- relevel(cleaned_data$T1_win, ref = "win")


## Metrics Set ----
chosen_metrics <- metric_set(accuracy, precision, recall, spec, f_meas)

## Train/ Validation/ Test Split ----
set.seed(12345)
data_split <- initial_split(cleaned_data,
                            strata = T1_win)

training_set <- training(data_split)
test_set <- testing(data_split)

set.seed(12345)
cv_split <- vfold_cv(training_set)



# No Streak
set.seed(12345)
ns_data_split <- cleaned_data %>% 
    select(-streakDiff) %>% 
    initial_split(strata = T1_win)

ns_training_set <- training(ns_data_split)
ns_test_set <- testing(ns_data_split)


# PG ----
set.seed(12345)
pg_data_split <- cleaned_data %>% 
    select(totalGamesDiff, rankDiff, championPointsDiff, totalMasteryDiff,
           nchampsDiff, meanMasteryDiff, streakDiff, T1_win) %>% 
    initial_split(strata = T1_win)

pg_training_set <- training(pg_data_split)
pg_test_set <- testing(pg_data_split)


# IG ----
set.seed(12345)
ig_data_split <- cleaned_data %>% 
    select(contains("At"), contains("By"), starts_with("T1")) %>% 
    initial_split(strata = T1_win)

ig_training_set <- training(ig_data_split)
ig_test_set <- testing(ig_data_split)
