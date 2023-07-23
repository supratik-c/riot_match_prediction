### Confusion Matrices ----
lr_all_cm <- conf_mat(lr_all_pred,
                      truth = .actual,
                      estimate = .pred_class)


rf_all_cm <- conf_mat(rf_all_pred,
                      truth = .actual,
                      estimate = .pred_class)

xgb_all_cm <- conf_mat(xgb_all_pred,
                       truth = .actual,
                       estimate = .pred_class)

gb_all_cm <- conf_mat(gb_all_pred,
                      truth = .actual,
                      estimate = .pred_class)


### ROC Charts ----
lr_all_curve <- roc_curve(lr_all_pred, truth = .actual, .pred_win) %>% 
    mutate(Model = "Logistic Regression")

rf_all_curve <- roc_curve(rf_all_pred, .actual, .pred_win) %>% 
    mutate(Model = "Random Forest")

xgb_all_curve <- roc_curve(xgb_all_pred, .actual, .pred_win) %>% 
    mutate(Model = "XGBoost")

gb_all_curve <- roc_curve(gb_all_pred, .actual, .pred_win) %>% 
    mutate(Model = "Gradient Boost")



all_curves <- bind_rows(lr_all_curve, rf_all_curve, xgb_all_curve, gb_all_curve) 


all_auc_plot <- all_curves %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity,
               col = Model)) +
    geom_path(linewidth = 1, alpha = 0.8) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    labs(x = "FP Rate",
         y = "Recall",
         caption = "ROC Curves") +
    theme_classic() +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14),
          panel.grid.major = element_line(color = "gray"))
all_auc_plot




### Metrics Charts ----
lr_all_metrics <- get_metrics(lr_all_pred, "Logistic Regression")

rf_all_metrics <- get_metrics(rf_all_pred, "Random Forest")

xgb_all_metrics <- get_metrics(xgb_all_pred, "XGBoost")

gb_all_metrics <- get_metrics(gb_all_pred, "Gradient Boost")


all_metrics <- bind_rows(lr_all_metrics, rf_all_metrics, xgb_all_metrics, gb_all_metrics)
    
    

all_metrics_plot <- all_metrics %>% 
    filter(Metric %in% c("Accuracy", "AUC")) %>% 
    ggplot(aes(x = Metric, y = Score, fill = Model, label = Score, vjust = -1)) +
    geom_col(position = "dodge") +
    theme_classic() +
    labs(caption = "Comparison of performance metrics") +
    theme(legend.position = "top",
          axis.text = element_text(face="bold"),
          plot.caption = element_text(face = "bold", hjust = 0, size = 14)) +
    geom_text(position = position_dodge(1))
all_metrics_plot



# Variable Importance ----
## LR Significant coefficients 
pred_sig <- tidy(lr_model, exponentiate = T) %>% 
    filter(p.value < 0.05)
pred_sig


## RF Variable Importance 
vi_ <- vi(rf_final_model) %>% 
    ggplot(aes(x = Importance , y = fct_reorder(Variable, Importance))) +
    geom_bar(stat = "identity") + 
    labs(y = "Predictor") +
    theme_classic() +
    theme(axis.text = element_text(face="bold"),
          plot.caption = element_text(hjust = 0, face = "bold", size = 12)) 
vi_

# GB Variable Importance
gb_vip <- vip(gb_final_model, num_features = 10)
gb_vip

# XGB Importance
xgb_vip <- xgboost::xgb.importance(model=xgb_final_model$fit)
xgb_plot <- xgb.ggplot.importance(xgb_vip, top_n = 10, n_clusters = 1)
xgb_plot
