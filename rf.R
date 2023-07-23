# Hyperparameter tuning ----
rf_model <- rand_forest(
    mtry = tune(),
    trees = 256,
    min_n = tune()) %>% 
    set_engine("ranger", .num_threads = 12) %>% 
    set_mode("classification")

rf_wf <- 
    workflow() %>% 
    add_model(rf_model) %>% 
    add_formula(T1_win ~ .)


start <- Sys.time()
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
print(Sys.time() - start)

rf_best <- select_best(rf_tune, "roc_auc")

### Finalize best performing RF model ----
# Mtry = 10, min_n = 125
set.seed(12345)
rf_final_model <- rand_forest(
    mtry = 10,
    trees = 1500,
    min_n = 125) %>% 
    set_engine("ranger", num.threads = 12, importance = "impurity") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~., training_set)


# Predict on test set
rf_all_pred <- predict(rf_final_model, test_set) %>% 
    bind_cols(predict(rf_final_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))

