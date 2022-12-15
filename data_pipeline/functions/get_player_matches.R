get_player_matches <- function(player_puuid, queue = 420, start = 0, count = 100)
{
    # Format URL using puuid argument
    base_url <- "https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/"
    queue_url <- ifelse(queue == "all", 
                        "",
                        paste("queue=", queue, sep = ""))
    final_url <- paste(base_url, 
                       player_puuid,
                       "/ids?",
                       queue_url,
                       "&start=",
                       start,
                       "&count=",
                       count,
                       sep = "")
    
    # Get and return data
    matches <- riot_get(final_url)
    return(fromJSON(rawToChar(matches$content)))
    
    
}
