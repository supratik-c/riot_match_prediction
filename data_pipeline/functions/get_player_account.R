get_player_account <- function(id, id_type)
{
    base_url <- "https://euw1.api.riotgames.com/lol/summoner/v4/summoners/"
    
    # Case handling for name/ puuid/ encryptedSummonerId querying
    switch(id_type,
           "name" = api_spec <- "by-name/",
           "puuid" = api_spec <- "by-puuid/",
           "encryptedSummonerId" = api_spec <- "")
    
    # Execution
    final_url <- paste0(base_url, api_spec, id)
    player_data <- riot_get(final_url)
    
    # Return data if query successful
    return(fromJSON(rawToChar(player_data$content)))
}

