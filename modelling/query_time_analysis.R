raw_query_timings <- read_csv("data/record_times.csv", col_names = c("phase", "value"))

averages <- raw_query_timings %>% 
    group_by(phase) %>% 
    summarise(avg = mean(value))
    
