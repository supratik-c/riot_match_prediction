library(tidyverse)
library(tidymodels)
library(tidypredict)
library(doParallel)
library(vip)

# Parallelize
doParallel::registerDoParallel()


# Read in data ----
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

set.seed(12345)
cv_split <- vfold_cv(training_set)

# Metrics
chosen_metrics <- metric_set(accuracy, precision, recall, spec, f_meas)


# Model Spec ----
lr_model <- logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode("classification")

rf_model <- rand_forest(
    mtry = tune(),
    trees = 256,
    min_n = tune()) %>% 
    set_engine("ranger", num.threads = 12) %>% 
    set_mode("classification")

rf_wf <- 
    workflow() %>% 
    add_model(rf_model) %>% 
    add_formula(T1_win ~ .)

t_ <- rf_wf %>% 
    update_model(mtry = 14)

# RQ1 ----

### LR

# Validation Testing
lr_all_cv_results <- lr_model %>% 
    fit_resamples(T1_win ~ ., cv_split)

lr_all_cv_metrics <- collect_metrics(lr_all_cv_results)


# Final model
lr_all <- lr_model %>% 
    fit(T1_win ~ ., data = training_set)


# Predict test set
lr_all_pred <- predict(lr_all, test_set) %>% 
    bind_cols(predict(lr_all, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))



### Randforest

# Tune grid


# First Pass
set.seed(12345)
rf_tune_first <- tune_grid(
    rf_wf,
    resamples = cv_split,
    grid = grid_regular(
        mtry(range = c(0, 14)),
        min_n(range = c(0, 50)),
        levels = 10
    )
)

rf_param_plot <- rf_tune_first %>% 
    collect_metrics() %>% 
    filter(.metric == "roc_auc") %>% 
    select(mtry, min_n, mean) %>% 
    pivot_longer(1:2,
                 values_to = "param_val",
                 names_to = "param") %>% 
    ggplot(aes(param_val, mean, color = param)) +
    geom_point() +
    facet_wrap(~param)+
    labs(x = NULL, y = "AUC")

# Update rf

# Second Tune
set.seed(12345)
rf_tune <- tune_grid(
    rf_wf,
    resamples = cv_split,
    grid = grid_regular(
        mtry(range = c(0, 14)),
        min_n(range = c(60, 110)),
        levels = 10
    )
)

rf_param_plot2 <- rf_tune %>% 
    collect_metrics() %>% 
    filter(.metric == "roc_auc") %>% 
    select(mtry, min_n, mean) %>% 
    pivot_longer(1:2,
                 values_to = "param_val",
                 names_to = "param") %>% 
    ggplot(aes(param_val, mean, color = param)) +
    geom_point() +
    facet_wrap(~param)+
    labs(x = NULL, y = "AUC")




# Finalize best performing RF model
rf_best <- select_best(rf_tune, "roc_auc")
rf_final_model <- rand_forest(
    engine = "ranger",
    mode = "classification",
    mtry = rf_best$mtry,
    trees = 1500,
    min_n = rf_best$min_n
) %>% 
    fit(T1_win ~., training_set)


rf_all_pred <- predict(rf_final_model, test_set) %>% 
    bind_cols(predict(rf_final_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))

accuracy(rf_all_pred, truth = .actual, estimate = .pred_class)
conf_mat(rf_all_pred, truth = .actual, estimate = .pred_class)

# RQ1 - Performance Metrics ----

# Conf Matrices
rf_all_cm <- conf_mat(rf_all_pred,
                      truth = .actual,
                      estimate = .pred_class)
lr_all_cm <- conf_mat(lr_all_pred,
                   truth = .actual,
                   estimate = .pred_class)

# Conf Matrix Metrics
lr_all_metrics <- chosen_metrics(lr_all_pred, 
                                 truth = .actual, 
                                 estimate = .pred_class) %>% 
    select(Metric = .metric, LR = .estimate)

rf_all_metrics <- chosen_metrics(rf_all_pred, 
                                 truth = .actual, 
                                 estimate = .pred_class) %>% 
    select(RF = .estimate)

all_metrics <- bind_cols(lr_all_metrics, rf_all_metrics) %>% 
    mutate(LR = round(LR, 3),
           RF = round(RF, 3),
           Metric = case_when(
               Metric == "accuracy" ~ "Accuracy",
               Metric == "precision" ~ "Precision",
               Metric == "recall" ~ "Recall",
               Metric == "spec" ~ "Specificity",
               Metric == "f_meas" ~ "F1 Measure"
           )) %>% 
    pivot_longer(2:3,
                 names_to = "Model",
                 values_to = "Score")

all_metrics_plot <- all_metrics %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    geom_text(position = position_dodge(1))


# AUROC
lr_all_auc <- roc_auc(lr_all_pred, truth = .actual, .pred_win)
lr_all_curve <- roc_curve(lr_all_pred, truth = .actual, .pred_win) %>% 
    mutate(model = "Logistic Regression")


rf_all_auc <- roc_auc(rf_all_pred, .actual, .pred_win)
rf_all_curve <- roc_curve(rf_all_pred, .actual, .pred_win) %>% 
    mutate(model = "Random Forest")

RQ1_combined_aucplot <- bind_rows(lr_all_curve, rf_all_curve) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity,
               col = model)) +
    geom_path(linewidth = 1, alpha = 0.8)





#### RQ2 ----



# LR Significant coefficients
pred_sig <- tidy(lr_all, exponentiate = T) %>% 
    filter(p.value < 0.05)


# RF Variable Importance


# No Streak Models ----
set.seed(12345)
ns_set <- cleaned_data %>% 
    select(-streakDiff) %>% 
    initial_split(strata = T1_win)

ns_train <- training(ns_set)
ns_test <- testing(ns_set)

# LR
ns_lr <- lr_model %>% 
    fit(T1_win ~ ., data = ns_train)

lr_ns_pred <- predict(ns_lr, ns_test) %>% 
    bind_cols(predict(ns_lr, ns_test, type = "prob")) %>% 
    bind_cols(ns_test %>% select(.actual = T1_win))






# RF
set.seed(12345)
rf_ns_model <- rand_forest(
    engine = "ranger",
    mode = "classification",
    mtry = rf_best$mtry,
    trees = 1500,
    min_n = rf_best$min_n
) %>% 
    fit(T1_win ~., ns_train)


rf_ns_pred <- predict(rf_ns_model, ns_test) %>% 
    bind_cols(predict(rf_ns_model, ns_test, type = "prob")) %>% 
    bind_cols(ns_test %>% select(.actual = T1_win))



# AUROC Plot without Streak
lr_ns_curve <- roc_curve(lr_ns_pred, .actual, .pred_win) %>% 
    mutate(model = "LR No Streak")

rf_ns_curve <- roc_curve(rf_ns_pred, .actual, .pred_win) %>% 
    mutate(model = "RF No Streak")

ns_aucplot <- bind_rows(lr_all_curve, lr_ns_curve, rf_ns_curve) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity,
               col = model)) +
    geom_path(linewidth = 1, alpha = 0.8)


# Metrics Plot without Streak

lr_ns_metrics <- chosen_metrics(lr_ns_pred,
                                truth = .actual,
                                estimate = .pred_class) %>% 
    select(LR_No_Streak = .estimate)

rf_ns_metrics <- chosen_metrics(rf_ns_pred, 
                                truth = .actual, 
                                estimate = .pred_class) %>% 
    select(RF_No_Streak = .estimate)

ns_all_metrics <- bind_cols(lr_all_metrics, lr_ns_metrics, rf_ns_metrics) %>% 
    mutate(LR_No_Streak = round(LR_No_Streak, 2),
           RF_No_Streak = round(RF_No_Streak, 2),
           LR = round(LR, 2),
           Metric = case_when(
               Metric == "accuracy" ~ "Accuracy",
               Metric == "precision" ~ "Precision",
               Metric == "recall" ~ "Recall",
               Metric == "spec" ~ "Specificity",
               Metric == "f_meas" ~ "F1 Measure"
           )) %>% 
    pivot_longer(2:4,
                 names_to = "Model",
                 values_to = "Score")

ns_metrics_plot <- ns_all_metrics %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    geom_text(position = position_dodge(1))
