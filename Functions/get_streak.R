get_streak <- function(puuid, matchId)
{
    
    # Create tibble of last 100 matches
    matches <- get_player_matches(puuid, queue = "all")
    if(!(matchId %in% matches)){
        data <- tibble(
            puuid = puuid,
            streak = 0
        )
        
        return(data)
    }
    
    match_tbl <- tibble(
        "id" = seq(length(matches)),
        "matchId" = matches
    )
    
    # Get match row for given id and filter out all matches before it
    match_row <- match_tbl %>% 
        filter(matchId == .env$matchId) %>% 
        pull(id)
    
    prev_matches <- match_tbl %>% 
        filter(id > match_row) %>% 
        pull(matchId)
    
    
    # Set last match result as reference point (win or loss)
    init_result <- get_match(prev_matches[1])$info$participants %>% 
        filter(puuid == .env$puuid) %>% 
        pull(win)
    
    # Set initial streak
    streak <- 1
    
    # Iterate through previous matches and count streak
    for (match in prev_matches[2:length(prev_matches)]){
        next_match <- get_match(match)$info$participants %>% 
            filter(puuid == .env$puuid) %>% 
            pull(win)
        
        if (next_match != init_result) {
            break
        } else {
            streak <- streak + 1
        }
    }
    
    
    # Make streak negative if it's losing
    if(!init_result){
        streak <- streak * -1
    }
    
    data <- tibble(
        puuid = puuid,
        streak = streak
    )
    
    return(data)

}