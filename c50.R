# C5.0 Gradient Boost Spec
gb_model <- boost_tree(
    trees = 100, 
    min_n = tune(), 
    sample_size = tune()) %>% 
    set_engine("C5.0") %>% 
    set_mode("classification")


# Workflow
gb_wf <- workflow() %>% 
    add_model(gb_model) %>% 
    add_formula(T1_win ~ .)


# Tuning grid
gb_grid <- grid_latin_hypercube(
    min_n(),
    sample_size = sample_prop(),
    size = 30
)

# Tuning
gb_tune <- tune_grid(
    gb_wf,
    resamples = cv_split,
    grid = gb_grid,
)

gb_best <- select_best(gb_tune, "roc_auc")
# min_n = 34
# sample_size = 0.528481

# Finalize Model
set.seed(543)
gb_final_model <- boost_tree(
    trees = 100, 
    min_n = 34, 
    sample_size = 0.528481) %>% 
    set_engine("C5.0", importance = "impurity") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~ ., training_set)


gb_all_pred <- predict(gb_final_model, test_set) %>% 
    bind_cols(predict(gb_final_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))