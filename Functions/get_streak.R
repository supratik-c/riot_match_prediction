get_streak <- function(puuid, matchId)
{
    
    # Create tibble of last 100 matches
    matches <- get_player_matches(puuid, queue = "all")
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

# # Tests
# test_streak <- get_streak(test_puuid, test_match)
# 
# 
# test_puuid <- player_puuid
# test_match <- recent_matchId
# matches <- get_player_matches(test_puuid, queue = "all")
# wins <- c()
# for (match in matches){
#     win <- get_match(match)$info$participants %>%
#         filter(puuid == test_puuid) %>%
#         pull(win)
# 
#     wins <- c(wins, win)
# }
# match_res <- tibble(
#     id = matches,
#     result = wins
# )
# 
# test_mid <- matches[1]
# test_streak <- matches[2:10]
# 
# test_init <- get_match(test_mid)$info$participants %>%
#     filter(puuid == test_puuid) %>%
#     pull(win)
# 
# test_ctr <- ifelse(test_init, 1, -1)
# 
# for (i in 1:length(test_streak)) {
#     next_match <- get_match(test_streak[i])$info$participants %>%
#         filter(puuid == test_puuid) %>%
#         pull(win)
# 
#     if (next_match != test_init) {
#         break
#     } else {
#         test_ctr <- ifelse(next_match, test_ctr + 1, test_ctr - 1)
#     }
# 
#     print(paste0("Streak:", test_ctr))
# 
# }
