get_champ_mastery <- function(encryptedSummonerId, championId)
{
    url <- paste("https://euw1.api.riotgames.com/lol/champion-mastery/v4/champion-masteries/by-summoner",
                 encryptedSummonerId,
                 sep = "/")
    
    champ_data <- fromJSON(rawToChar(riot_get(url)$content))
    
    # Calculate mastery metrics
    champMastery <- champ_data %>% 
        filter(championId == .env$championId) %>% 
        select(championPoints)
    
    totalMastery <- champ_data %>% 
        summarize(totalMastery = sum(championPoints),
                  nchamps = n(),
                  meanMastery = mean(championPoints))
    
    # Create return tibble
    champs <- bind_cols(
        list("summonerId" = encryptedSummonerId),
        champMastery,
        totalMastery
    )
    
    return(champs)
}

