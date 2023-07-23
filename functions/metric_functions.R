get_auc <- function(df, name){
    
    auc <- roc_auc(df, truth = .actual, .pred_win)%>% 
        mutate(Model = name,
               .estimate = round(.estimate, 3),
               .metric = "AUC") %>% 
        select(Model, Metric = .metric, Score = .estimate)
    
}



get_metrics <- function(df, name){
    
    metrics <- chosen_metrics(df, 
                             truth = .actual, 
                             estimate = .pred_class) %>% 
        transmute(Model = name,
                  Metric = case_when(
                      .metric == "accuracy" ~ "Accuracy",
                      .metric == "precision" ~ "Precision",
                      .metric == "recall" ~ "Recall",
                      .metric == "spec" ~ "Specificity",
                      .metric == "f_meas" ~ "F1 Measure",
                      T ~ .metric),
                  Score = round(.estimate, 3)) %>% 
        bind_rows(get_auc(df, name))
    
    
    
}
