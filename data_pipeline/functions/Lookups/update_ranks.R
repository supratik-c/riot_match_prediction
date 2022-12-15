update_ranks <- function(conn)
{
    divisions <- list(divisions = c("IV", "III", "II", "I"))
    tiers <- list(tiers = c("IRON", "BRONZE", "SILVER", "GOLD", "PLATINUM", "DIAMOND"))
    ranks <- merge(divisions, tiers) %>%
        rename(division = divisions,
               tier = tiers) %>% 
        mutate(rank = paste0(tier, division),
               numeric_rank = row_number())
    
    
    dbWriteTable(conn,
                 "D_RANKS",
                 ranks,
                 row.names = F,
                 overwrite = T)
}