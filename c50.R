# C5.0 Gradient Boost Spec
gb_model <- boost_tree(
    trees = 256, 
    min_n = tune(), 
    sample_size = 1) %>% 
    set_engine("C5.0") %>% 
    set_mode("classification")


# Workflow
gb_wf <- workflow() %>% 
    add_model(gb_model) %>% 
    add_formula(T1_win ~ .)

# Tuning

