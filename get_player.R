get_player <- function(id, type, api_key)
{
    base_url <- "https://euw1.api.riotgames.com/lol/summoner/v4/summoners/"
    
    # Case handling for name/ puuid/ encryptedSummonerId querying
    switch(type,
           "name" = api_spec <- "by-name/",
           "puuid" = api_spec <- "by-puuid/",
           "encryptedSummonerId" = api_spec <- "")
    
    # Execution
    final_url <- paste0(base_url, api_spec, id)
    player_data <- httr::GET(final_url, 
                             add_headers("X-Riot-Token" = api_key))
    
    Sys.sleep(1.2)
    
    # Flag if requests receives an error code
    if(player_data$status_code == 429)
    {
        Sys.sleep(10)
    }
    else if(player_data$status_code != 200)
    {
        stop("Error. Status Code:", player_data$status_code)
    }
    
    # Return data if query successful
    return(fromJSON(rawToChar(player_data$content)))
}

