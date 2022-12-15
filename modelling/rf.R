library(tidyverse)
library(tidymodels)
library(tidypredict)


# Read in data
cleaned_data <- read_csv("modelling/cleaned_data.csv")

# Set categorical variables as factors
cleaned_data$T1_firstBlood <- as.factor(cleaned_data$T1_firstBlood)
cleaned_data$T1_firstTower <- as.factor(cleaned_data$T1_firstTower)
cleaned_data$T1_win <- as.factor(cleaned_data$T1_win)
cleaned_data$T1_win <- relevel(cleaned_data$T1_win, ref = "win")

# Train/ Validation/ Test Split
set.seed(12345)

data_split <- initial_split(cleaned_data,
                            strata = T1_win)

training_set <- training(data_split)
test_set <- testing(data_split)
cv_split <- vfold_cv(training_set)

# Metrics
chosen_metrics <- metric_set(accuracy, precision, recall, spec, f_meas)


# Model Spec
rf_model <- rand_forest(
    mtry = tune(),
    trees = tune(),
    min_n = tune()) %>% 
    set_engine("ranger") %>% 
    set_mode("classification")


# Tune Grid




