get_player_wl <- function(encryptedSummonerId)
{
    url <- paste("https://euw1.api.riotgames.com/lol/league/v4/entries/by-summoner",
                 encryptedSummonerId,
                 sep = "/")
    
    data <- fromJSON(rawToChar(riot_get(url)$content)) %>% 
        filter(queueType == "RANKED_SOLO_5x5") %>% 
        mutate(rank = paste0(tier, rank)) %>% 
        select(summonerId, wins, losses, rank) %>% 
        merge(select(ranks, c(tier, rank, numeric_rank)),
              by.x = "rank",
              by.y = "rank",
              all.x = TRUE)
    return(data)
    
}

