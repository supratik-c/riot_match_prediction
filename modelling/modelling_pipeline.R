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

## Train/ Validation/ Test Split ----
set.seed(12345)
data_split <- initial_split(cleaned_data,
                            strata = T1_win)

training_set <- training(data_split)
test_set <- testing(data_split)

set.seed(12345)
cv_split <- vfold_cv(training_set)

## Metrics Set ----
chosen_metrics <- metric_set(accuracy, precision, recall, spec, f_meas)


## Model Default Specifications ----
lr_model <- logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode("classification")

rf_model <- rand_forest(
    mtry = tune(),
    trees = 256,
    min_n = tune()) %>% 
    set_engine("ranger") %>% 
    set_mode("classification")

rf_wf <- 
    workflow() %>% 
    add_model(rf_model) %>% 
    add_formula(T1_win ~ .)



# RQ1 ----

## Logistic Regression Reference Model ----

### Validation Testing ----
lr_all_cv_results <- lr_model %>% 
    fit_resamples(T1_win ~ ., cv_split)

lr_all_cv_metrics <- collect_metrics(lr_all_cv_results)


### Final LR model ----
lr_all <- lr_model %>% 
    fit(T1_win ~ ., data = training_set)




lr_all_pred <- predict(lr_all, test_set) %>% 
    bind_cols(predict(lr_all, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))


lr_all_cm <- conf_mat(lr_all_pred,
                      truth = .actual,
                      estimate = .pred_class)
lr_all_cm


## Random Forest Reference Model ----
### First Pass Tuning ----
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

rf_param_plot_init <- rf_tune_first %>% 
    collect_metrics() %>% 
    filter(.metric == "roc_auc") %>% 
    select(mtry, min_n, mean) %>% 
    pivot_longer(1:2,
                 values_to = "param_val",
                 names_to = "Parameter") %>% 
    ggplot(aes(param_val, mean, color = Parameter)) +
    geom_point() +
    scale_color_manual(values = c("#DE532C", "#2037D8")) +
    facet_wrap(~Parameter)+
    labs(x = NULL, y = "AUC", 
         caption = "Figure 10 – Initial Hyperparameter Ranges for RF") + 
    theme(plot.caption = element_text(face = "bold", hjust = 0, size = 14),
          legend.position = "top")
rf_param_plot_init


### Second Tune ----
set.seed(12345)
rf_tune <- tune_grid(
    rf_wf,
    resamples = cv_split,
    grid = grid_regular(
        mtry(range = c(0, 14)),
        min_n(range = c(60, 125)),
        levels = 10
    )
)

rf_param_plot <- rf_tune %>% 
    collect_metrics() %>% 
    filter(.metric == "roc_auc") %>% 
    select(min_n, mean) %>% 
    ggplot(aes(min_n, mean)) +
    geom_point() +
    labs(x = "min_n", y = "AUC",
         caption = "Figure 11 – Second min_n tuning pass for RF") + 
    theme_classic() +
    theme(plot.caption = element_text(face = "bold", hjust = 0, size = 14))
rf_param_plot



### Finalize best performing RF model ----
rf_best <- select_best(rf_tune, "roc_auc")

set.seed(12345)
rf_final_model <- rand_forest(
    mtry = rf_best$mtry,
    trees = 1500,
    min_n = rf_best$min_n) %>% 
    set_engine("ranger", num.threads = 12, importance = "impurity") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~., training_set)

# Write Reference RF Model YAML file
# write_yaml(parse_model(rf_final_model), "models/reference_RF.yml")

# Predict on test set
rf_all_pred <- predict(rf_final_model, test_set) %>% 
    bind_cols(predict(rf_final_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))

# Confusion Matrix
rf_all_cm <- conf_mat(rf_all_pred,
                      truth = .actual,
                      estimate = .pred_class)
rf_all_cm

## Reference Model Performance Metrics ----

### ROC Curve ----
lr_all_curve <- roc_curve(lr_all_pred, truth = .actual, .pred_win) %>% 
    mutate(model = "Logistic Regression")


rf_all_curve <- roc_curve(rf_all_pred, .actual, .pred_win) %>% 
    mutate(model = "Random Forest")

RQ1_combined_aucplot <- lr_all_curve %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    labs(x = "FP Rate",
         y = "Recall",
         caption = "Figure 9 - An Example ROC Curve Plot") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    theme_classic() +
    theme(plot.caption = element_text(face = "bold", hjust = 0, size = 14),
          legend.position = "top",
          panel.grid.major = element_line(colour = "gray"))
RQ1_combined_aucplot



### Metrics Chart ----
lr_all_auc <- roc_auc(lr_all_pred, truth = .actual, .pred_win)%>% 
    mutate(Model = "LR",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)

rf_all_auc <- roc_auc(rf_all_pred, .actual, .pred_win)%>% 
    mutate(Model = "RF",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)

lr_all_metrics <- chosen_metrics(lr_all_pred, 
                                 truth = .actual, 
                                 estimate = .pred_class) %>% 
    mutate(LR = round(.estimate, 3)) %>% 
    select(Metric = .metric, LR) %>% 
    bind_rows(select(lr_all_auc, Metric, LR = Score))

rf_all_metrics <- chosen_metrics(rf_all_pred, 
                                 truth = .actual, 
                                 estimate = .pred_class) %>% 
    mutate(RF = round(.estimate, 3)) %>% 
    select(RF) %>% 
    bind_rows(select(rf_all_auc, RF = Score))


all_metrics <- bind_cols(lr_all_metrics, rf_all_metrics) %>% 
    mutate(Metric = case_when(
               Metric == "accuracy" ~ "Accuracy",
               Metric == "precision" ~ "Precision",
               Metric == "recall" ~ "Recall",
               Metric == "spec" ~ "Specificity",
               Metric == "f_meas" ~ "F1 Measure",
               T ~ Metric
           )) %>% 
    pivot_longer(2:3,
                 names_to = "Model",
                 values_to = "Score")

all_metrics_plot <- all_metrics %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    labs(caption = "Figure 13 – Comparison of performance metrics for LR and RF reference models") +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14)) +
    geom_text(position = position_dodge(1))
all_metrics_plot



# RQ2 ----



## LR Significant coefficients ----
pred_sig <- tidy(lr_all, exponentiate = T) %>% 
    filter(p.value < 0.05)


## RF Variable Importance ----
vi_ <- vi(rf_final_model) %>% 
    ggplot(aes(x = Importance , y = fct_reorder(Variable, Importance))) +
    geom_bar(stat = "identity") + 
    labs(y = "Predictor",
         caption = "Figure 14 – Comparison of performance metrics for LR and RF reference models") +
    theme_classic() +
    theme(axis.text = element_text(face="bold"),
          plot.caption = element_text(hjust = 0, face = "bold", size = 12)) 
vi_


## No Streak Models ----
set.seed(12345)
ns_set <- cleaned_data %>% 
    select(-streakDiff) %>% 
    initial_split(strata = T1_win)

ns_train <- training(ns_set)
ns_test <- testing(ns_set)

### LR ----
ns_lr <- lr_model %>% 
    fit(T1_win ~ ., data = ns_train)

# Write model
# write_yaml(parse_model(ns_lr), "models/LR_noStreak.yml")


lr_ns_pred <- predict(ns_lr, ns_test) %>% 
    bind_cols(predict(ns_lr, ns_test, type = "prob")) %>% 
    bind_cols(ns_test %>% select(.actual = T1_win))


lr_ns_curve <- roc_curve(lr_ns_pred, .actual, .pred_win) %>% 
    mutate(model = "LR No Streak")

lr_ns_auc <- roc_auc(lr_ns_pred, .actual, .pred_win) %>% 
    mutate(Model = "LR No Streak",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)

lr_ns_metrics <- chosen_metrics(lr_ns_pred,
                                truth = .actual,
                                estimate = .pred_class) %>% 
    mutate(.estimate = round(.estimate, 3)) %>% 
    select("LR No Streak" = .estimate) %>% 
    bind_rows(select(lr_ns_auc, "LR No Streak" = Score))

### RF ----
set.seed(12345)
rf_ns_model <- rand_forest(
    mtry = rf_best$mtry,
    trees = 1500,
    min_n = rf_best$min_n) %>% 
    set_engine("ranger", num.threads = 12, importance = "impurity") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~., ns_train)

# Write model
# write_yaml(parse_model(rf_ns_model), "models/RF_noStreak.yml")

rf_ns_pred <- predict(rf_ns_model, ns_test) %>% 
    bind_cols(predict(rf_ns_model, ns_test, type = "prob")) %>% 
    bind_cols(ns_test %>% select(.actual = T1_win))


rf_ns_curve <- roc_curve(rf_ns_pred, .actual, .pred_win) %>% 
    mutate(model = "RF No Streak")

rf_ns_auc <- roc_auc(rf_ns_pred, .actual, .pred_win) %>% 
    mutate(Model = "RF No Streak",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)

rf_ns_metrics <- chosen_metrics(rf_ns_pred, 
                                truth = .actual, 
                                estimate = .pred_class) %>% 
    mutate("RF No Streak" = round(.estimate, 3)) %>% 
    select("RF No Streak") %>% 
    bind_rows(select(rf_ns_auc, "RF No Streak" = Score))




### No Streak Performance Metrics ----
ns_aucplot <- bind_rows(lr_all_curve, lr_ns_curve, rf_ns_curve) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity,
               col = model)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "FP Rate",
         y = "Recall",
         caption = "Figure 15 – ROC Curves for models without streakDiff, compared to reference LR") +
    theme_classic() +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14),
          panel.grid.major = element_line(color = "gray"))
ns_aucplot


ns_all_metrics <- bind_cols(lr_all_metrics, lr_ns_metrics, rf_ns_metrics) %>% 
    mutate(Metric = case_when(
               Metric == "accuracy" ~ "Accuracy",
               Metric == "precision" ~ "Precision",
               Metric == "recall" ~ "Recall",
               Metric == "spec" ~ "Specificity",
               Metric == "f_meas" ~ "F1 Measure",
               T ~ Metric
           )) %>% 
    pivot_longer(2:4,
                 names_to = "Model",
                 values_to = "Score")

ns_metrics_plot <- ns_all_metrics %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    geom_text(position = position_dodge(1)) +
    labs(caption = "Figure 16 – Performance metrics of reference LR to No Streak models") +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14))
ns_metrics_plot


# RQ3 ----

## VIP Grouping ----
vi_grp <- vi(rf_final_model) %>% 
    mutate("Pred Group" = ifelse(
        grepl("en", Variable) | grepl("T1", Variable),
        "IG",
        "PG"
    )) %>% 
    ggplot(aes(x = Importance , 
               y = fct_reorder(Variable, Importance),
               fill = `Pred Group`)) +
    geom_bar(stat = "identity") + 
    theme_classic() +
    labs(y = "Predictor",
         caption = "Figure 17 – RF Variable Importance by whether Predictor is PG or IG") +
    theme_classic() +
    theme(axis.text = element_text(face="bold"),
          plot.caption = element_text(hjust = 0, face = "bold", size = 12)) 
vi_grp


## PG Model ----

### Train/Test ----
set.seed(123)
pg_data_split <- cleaned_data %>% 
    select(totalGamesDiff, rankDiff, championPointsDiff, totalMasteryDiff,
           nchampsDiff, meanMasteryDiff, streakDiff, T1_win) %>% 
    initial_split()

pg_training <- training(pg_data_split)
pg_test <- testing(pg_data_split)

set.seed(12345)
pg_cv <- vfold_cv(pg_training)

### LR ----
lr_pg_model <- lr_model %>% 
    fit(T1_win ~ ., data = pg_training)

# Write model
# write_yaml(parse_model(lr_pg_model), "models/LR_pg.yml")

lr_pg_pred <- predict(lr_pg_model, pg_test) %>% 
    bind_cols(predict(lr_pg_model, pg_test, type = "prob")) %>% 
    bind_cols(select(pg_test, .actual = T1_win))

lr_pg_auc <- roc_auc(lr_pg_pred, .actual, .pred_win)%>% 
    mutate(Model = "LR PG Only",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)


lr_pg_curve <- roc_curve(lr_pg_pred, .actual, .pred_win) %>% 
    mutate(model = "LR PG Only")


lr_pg_metrics <- chosen_metrics(lr_pg_pred,
                                truth = .actual,
                                estimate = .pred_class) %>% 
    mutate(.estimate = round(.estimate, 3)) %>% 
    select("LR PG Only" = .estimate) %>% 
    bind_rows(select(lr_pg_auc, "LR PG Only" = Score))





### RF ----
set.seed(12345)
rf_pg_tune <- tune_grid(
    rf_wf,
    resamples = pg_cv,
    grid = grid_regular(
        mtry(range = c(0, 6)),
        min_n(range = c(45, 125)),
        levels = 10
    )
)


rf_pg_best <- select_best(rf_pg_tune, "roc_auc")

rf_pg_model <- rand_forest(
    mtry = rf_pg_best$mtry,
    trees = 1500,
    min_n = rf_pg_best$min_n) %>% 
    set_engine("ranger", num.threads = 12, importance = "impurity") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~., pg_training)

# Write model
# write_yaml(parse_model(rf_pg_model), "models/RF_pg.yml")


rf_pg_pred <- predict(rf_pg_model, pg_test) %>% 
    bind_cols(predict(rf_pg_model, pg_test, type = "prob")) %>% 
    bind_cols(pg_test %>% select(.actual = T1_win))


rf_pg_curve <- roc_curve(rf_pg_pred, .actual, .pred_win) %>% 
    mutate(model = "RF PG Only")

rf_pg_auc <- roc_auc(rf_pg_pred, .actual, .pred_win) %>% 
    mutate(Model = "RF PG Only",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)

rf_pg_metrics <- chosen_metrics(rf_pg_pred, 
                                truth = .actual, 
                                estimate = .pred_class) %>% 
    mutate("RF PG Only" = round(.estimate, 3)) %>% 
    select("RF PG Only") %>% 
    bind_rows(select(rf_pg_auc, "RF PG Only" = Score))



### PG Performance Metrics ----
pg_aucplot <- bind_rows(lr_all_curve, lr_pg_curve, rf_pg_curve) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity,
               col = model)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "FP Rate",
         y = "Recall",
         caption = "Figure 18 – ROC Curve comparing PG only models to reference LR model") +
    theme_classic() +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14),
          panel.grid.major = element_line(color = "gray"))
pg_aucplot


pg_all_metrics <- bind_cols(lr_all_metrics, lr_pg_metrics, rf_pg_metrics) %>% 
    mutate(Metric = case_when(
        Metric == "accuracy" ~ "Accuracy",
        Metric == "precision" ~ "Precision",
        Metric == "recall" ~ "Recall",
        Metric == "spec" ~ "Specificity",
        Metric == "f_meas" ~ "F1 Measure",
        T ~ Metric
    )) %>% 
    pivot_longer(2:4,
                 names_to = "Model",
                 values_to = "Score")

pg_metrics_plot <- pg_all_metrics %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    theme(legend.position = "top") +
    geom_text(position = position_dodge(1)) +
    labs(caption = "Figure 19 – Performance metrics for PG only models against reference LR model") +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14))
pg_metrics_plot






## IG Model ----

### Train/Test ----
set.seed(123)
ig_data_split <- cleaned_data %>% 
    select(contains("At"), contains("By"), starts_with("T1")) %>% 
    initial_split()

ig_training <- training(ig_data_split)
ig_test <- testing(ig_data_split)

set.seed(12345)
ig_cv <- vfold_cv(ig_training)



### LR ----
lr_ig_model <- lr_model %>% 
    fit(T1_win ~ ., data = ig_training)

# Write model
# write_yaml(parse_model(lr_ig_model), "models/LR_ig.yml")


lr_ig_pred <- predict(lr_ig_model, ig_test) %>% 
    bind_cols(predict(lr_ig_model, ig_test, type = "prob")) %>% 
    bind_cols(select(ig_test, .actual = T1_win))

lr_ig_auc <- roc_auc(lr_ig_pred, .actual, .pred_win)%>% 
    mutate(Model = "LR IG Only",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)


lr_ig_curve <- roc_curve(lr_ig_pred, .actual, .pred_win)%>% 
    mutate(model = "LR IG Only")


lr_ig_metrics <- chosen_metrics(lr_ig_pred,
                                truth = .actual,
                                estimate = .pred_class) %>% 
    mutate(.estimate = round(.estimate, 3)) %>% 
    select("LR IG Only" = .estimate) %>% 
    bind_rows(select(lr_ig_auc, "LR IG Only" = Score))



### RF ----
set.seed(12345)
rf_ig_tune <- tune_grid(
    rf_wf,
    resamples = ig_cv,
    grid = grid_regular(
        mtry(range = c(0, 7)),
        min_n(range = c(45, 125)),
        levels = 10
    )
)

rf_ig_best <- select_best(rf_ig_tune, "roc_auc")

rf_ig_model <- rand_forest(
    mtry = rf_ig_best$mtry,
    trees = 1500,
    min_n = rf_ig_best$min_n) %>% 
    set_engine("ranger", num.threads = 12, importance = "impurity") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~., ig_training)

# Write model
# write_yaml(parse_model(rf_ig_model), "models/RF_ig.yml")


rf_ig_pred <- predict(rf_ig_model, ig_test) %>% 
    bind_cols(predict(rf_ig_model, ig_test, type = "prob")) %>% 
    bind_cols(ig_test %>% select(.actual = T1_win))


rf_ig_curve <- roc_curve(rf_ig_pred, .actual, .pred_win) %>% 
    mutate(model = "RF IG Only")

rf_ig_auc <- roc_auc(rf_ig_pred, .actual, .pred_win) %>% 
    mutate(Model = "RF IG Only",
           .estimate = round(.estimate, 3),
           .metric = "AUC") %>% 
    select(Metric = .metric , Model, Score = .estimate)

rf_ig_metrics <- chosen_metrics(rf_ig_pred, 
                                truth = .actual, 
                                estimate = .pred_class) %>% 
    mutate("RF IG Only" = round(.estimate, 3)) %>% 
    select("RF IG Only") %>% 
    bind_rows(select(rf_ig_auc, "RF IG Only" = Score))



### IG Performance Metrics ----
ig_aucplot <- bind_rows(lr_all_curve, lr_ig_curve, rf_ig_curve) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity,
               col = model)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    geom_abline(linetype = "dashed", intercept = 0, slope = 1) +
    labs(x = "FP Rate",
         y = "Recall",
         caption = "Figure 20 – ROC Curves for IG only models against reference LR model") +
    theme_classic() +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14),
          panel.grid.major = element_line(color = "gray"))
ig_aucplot



ig_all_metrics <- bind_cols(lr_all_metrics, lr_ig_metrics, rf_ig_metrics) %>% 
    mutate(Metric = case_when(
        Metric == "accuracy" ~ "Accuracy",
        Metric == "precision" ~ "Precision",
        Metric == "recall" ~ "Recall",
        Metric == "spec" ~ "Specificity",
        Metric == "f_meas" ~ "F1 Measure",
        T ~ Metric
    )) %>% 
    pivot_longer(2:4,
                 names_to = "Model",
                 values_to = "Score")

ig_metrics_plot <- ig_all_metrics %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    theme(legend.position = "top") +
    geom_text(position = position_dodge(1)) +
    labs(caption = "Figure 19 – Performance metrics for IG only models against reference LR model") +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14))
ig_metrics_plot
