get_sample_player <- function(api_key){
    
    # League V4 base URL
    base_url <- "https://euw1.api.riotgames.com/lol/league/v4/entries/RANKED_SOLO_5x5/"
    
    # Randomly choose a division, tier and page to sample
    divisions <- list(division = c("I", "II", "III", "IV"))
    tier <- list(tier = c("IRON", "BRONZE", "SILVER", "GOLD", "PLATINUM", "DIAMOND"))
    choices <- merge(tier, divisions, all = T, sort = T)
    choice <- sample_n(choices, 1) %>% 
        unite(league, sep = "/")
    
    # Choose a random page
    page <- paste0("?page=", sample(seq(1, 30), 1))
    
    # Final URL
    final_url <- paste0(base_url, choice$league, page)
    
    # Request all players in sample league
    league_sample <- httr::GET(final_url,
                               add_headers("X-Riot-Token" = api_key))
    
    
    
    # Response handling
    if(league_sample$status_code != 200) {
        stop(paste0("Error. Status code:", league_sample$status_code))
    }
    
        
    
    # Content extraction
    sample_player <- fromJSON(rawToChar(league_sample$content)) %>% 
        sample_n(1) %>% 
        select(summonerId, wins, losses) %>% 
        return()
    
}