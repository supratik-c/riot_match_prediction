update_champions <- function(conn)
{
    # Extract master champions json
    champs <- fromJSON("http://ddragon.leagueoflegends.com/cdn/12.19.1/data/en_US/champion.json")[["data"]]
    
    # Extract id, names and convert to data frame
    champs_flat <- champs %>% 
        unlist() %>% 
        enframe() %>% 
        filter((grepl(".key$", name) | grepl(".name$", name))) %>% 
        mutate(name = ifelse(grepl(".key$", name), "key", "name")) %>% 
        pivot_wider()
    
    champions <- data.frame(id = as.numeric(unlist(champs_flat$key)), 
                            name = unlist(champs_flat$name))
    
    # Update champions database in azure
    dbWriteTable(conn,
                 name = "D_CHAMPIONS",
                 value = champions,
                 overwrite = TRUE,
                 row.names = FALSE)
} 





