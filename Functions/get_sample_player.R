get_sample_player <- function()
{
    
    # League V4 base URL
    base_url <- "https://euw1.api.riotgames.com/lol/league/v4/entries/RANKED_SOLO_5x5/"
    
    # Randomly choose a division, tier and page to sample
    if(!exists("choices")) {
        divisions <- list(division = c("I", "II", "III", "IV"))
        tier <- list(tier = c("IRON", "BRONZE", "SILVER", "GOLD", "PLATINUM", "DIAMOND"))
        choices <- merge(tier, divisions, all = T, sort = T)
    }
    
    choice <- sample_n(choices, 1) %>% 
        unite(league, sep = "/")
    
    # Choose a random page
    page <- paste0("?page=", sample(seq(1, 30), 1))
    
    # Final URL
    final_url <- paste0(base_url, choice$league, page)
    
    # Request all players in sample league
    league_sample <- riot_get(final_url)

    
    # Content extraction
    sample_player <- fromJSON(rawToChar(league_sample$content)) %>% 
        sample_n(1) %>% 
        select(summonerId, wins, losses) %>% 
        return()
}