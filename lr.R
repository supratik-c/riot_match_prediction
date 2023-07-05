# Model fitting
lr_model <- logistic_reg() %>% 
    set_engine("glm") %>% 
    set_mode("classification") %>% 
    fit(T1_win ~ ., data = training_set)



# Predict on Test Set
lr_all_pred <- predict(lr_model, test_set) %>% 
    bind_cols(predict(lr_model, test_set, type = "prob")) %>% 
    bind_cols(test_set %>% select(.actual = T1_win))


# Significant Coefficients
pred_sig <- tidy(lr_model, exponentiate = T) %>% 
    filter(p.value < 0.05)

# Confusion Matrix
lr_all_cm <- conf_mat(lr_all_pred,
                      truth = .actual,
                      estimate = .pred_class)

# ROC Curve
lr_all_curve <- roc_curve(lr_all_pred, truth = .actual, .pred_win) %>% 
    mutate(model = "Logistic Regression")
