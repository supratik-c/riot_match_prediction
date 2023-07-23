# Model fitting
lr_model <- logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~ ., data = training_set)



# Predict on Test Set
lr_all_pred <- predict(lr_model, test_set) %>% 
    bind_cols(predict(lr_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))


