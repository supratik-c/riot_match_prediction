get_sample_player <- function()
{
    
    # League V4 base URL
    base_url <- "https://euw1.api.riotgames.com/lol/league/v4/entries/RANKED_SOLO_5x5/"
    
    # Randomly choose a division, tier
    choice <- sample_n(ranks, 1) %>%
        select(tier, division) %>% 
        unite(league, sep = "/")
    
    # Choose a random page
    page <- paste0("?page=", sample(seq(1, 15), 1))
    
    # Final URL
    final_url <- paste0(base_url, choice$league, page)
    
    # Request all players in sample league
    league_sample <- riot_get(final_url)

    
    # Content extraction
    sample_player <- fromJSON(rawToChar(league_sample$content)) %>% 
        sample_n(1)
    return(sample_player$summonerId)
}