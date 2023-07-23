library(xgboost)

# Cross Validation and Hyperparameter tuning ----
xgb_spec <- boost_tree(
    trees = 1000,
    tree_depth = tune(), 
    min_n = tune(),
    loss_reduction = tune(),                     
    sample_size = tune(), 
    mtry = tune(),        
    learn_rate = tune()                          
) %>%
    set_engine("xgboost", .num_threads = 12) %>%
    set_mode("classification")

# Workflow
xgb_wf <- workflow() %>%
    add_formula(T1_win ~ .) %>%
    add_model(xgb_spec)


# LH Hyperparameter Grid Search
xgb_grid <- grid_latin_hypercube(
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    finalize(mtry(), training_set),
    learn_rate(),
    size = 30
)



# Cross Validation
xgb_res <- tune_grid(
    xgb_wf,
    resamples = cv_split,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
)

xgb_best <- select_best(xgb_res, "roc_auc")
# mtry 13
# min_n 27
# tree_depth 12
# learn_rate 0.001613343
# loss_reduction 0.01938399
# sample_size 0.7459859


# Finalize model ----
set.seed(543)
xgb_final_model <- boost_tree(
    trees = 1000,
    tree_depth = 12, 
    min_n = 27,
    loss_reduction = 0.01938399,                     
    sample_size = 0.7459859, 
    mtry = 13,        
    learn_rate = 0.001613343                          
) %>%
    set_engine("xgboost", verbose = 2, importance = "impurity") %>%
    set_mode("classification") %>% 
    fit(T1_win ~ ., training_set)
    

xgb_all_pred <- predict(xgb_final_model, test_set) %>% 
    bind_cols(predict(xgb_final_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))



