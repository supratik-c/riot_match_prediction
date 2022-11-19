update_items <- function(conn)
{
    items_raw <- fromJSON("http://ddragon.leagueoflegends.com/cdn/12.19.1/data/en_US/item.json")[["data"]]
    items_unlisted <- items_raw %>% unlist() %>% enframe()
    id <- names(items_raw)
    data <- items_unlisted %>% 
        filter((grepl(".name$", name) | grepl(".gold.total$", name))) %>% 
        mutate(name = ifelse(grepl(".name$", name), "name", "gold")) %>% 
        pivot_wider()
    
    items <- data.frame(id = id,
                        name = unlist(data$name),
                        gold = unlist(data$gold)) 
    
    dbWriteTable(conn,
                 name = "D_ITEMS",
                 value = items,
                 overwrite = TRUE,
                 row.names = FALSE)
}

  
