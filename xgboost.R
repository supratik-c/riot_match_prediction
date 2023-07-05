# Spec
xgb_spec <- boost_tree(
    trees = 1000,
    tree_depth = tune(), 
    min_n = tune(),
    loss_reduction = tune(),                     
    sample_size = tune(), 
    mtry = tune(),        
    learn_rate = tune()                          
) %>%
    set_engine("xgboost") %>%
    set_mode("classification")


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


# Workflow
xgb_wf <- workflow() %>%
    add_formula(T1_win ~ .) %>%
    add_model(xgb_spec)


# Cross Validation
xgb_res <- tune_grid(
    xgb_wf,
    resamples = cv_split,
    grid = xgb_grid,
    control = control_grid(save_pred = TRUE)
)


# Finalize model
xgb_best <- select_best(xgb_res, "roc_auc")

xgb_final_wf <- finalize_workflow(
    xgb_wf,
    xgb_best
) %>% 
    last_fit(data_split)


# Collect Test Set metrics
collect_metrics(xgb_final_wf)

