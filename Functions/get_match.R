get_match <- function(match_id)
{
    url <- paste("https://europe.api.riotgames.com/lol/match/v5/matches",
                 match_id,
                 sep = "/")

    match_data <- riot_get(url)
    return(fromJSON(rawToChar(match_data$content)))
    
    
}

