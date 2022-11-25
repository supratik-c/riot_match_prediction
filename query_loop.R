query_loop <- function(){
    
}

# Get sample match
player_esi <- get_sample_player()
player_puuid <- get_player_account(player_esi, "encryptedSummonerId")$puuid
recent_matchId <- get_player_matches(player_puuid)[1]
match <- get_match(recent_matchId)


# Extract match data
match_win <- match$info$teams %>% 
    select(teamId, win)

firstBlood <- list("firstBlood" = match$info$teams$objectives$champion$first)
firstTower <- list("firstTower" = match$info$teams$objectives$tower$first)
timestamp <- anydate(as.numeric(substr(match$info$gameCreation, 1, 10)))

match_data <- bind_cols(match_win, firstBlood, firstTower) %>% 
    filter(teamId == 100) %>% 
    mutate(matchId = recent_matchId,
           timestamp = timestamp) %>% 
    select(t1_firstBlood = firstBlood,
           t1_firstTower = firstTower,
           matchId,
           t1_win = win,
           timestamp)




# Extract participant data
data_raw <- match$info$participants
data_reduced <- data_raw %>% 
    select(puuid, summonerId, teamId, teamPosition,
           championId, gameEndedInEarlySurrender) %>% 
    mutate(matchId = recent_matchId) %>% 
    full_join(match_data, by = "matchId")


# Ranked W/L Ratio
winlosses <- tibble()
for(id in data_reduced$summonerId){
    print(paste("Attempting: ", id))
    data_wl <- get_player_wl(id)
    winlosses <- bind_rows(winlosses, data_wl)
}



data_wl <- merge(data_reduced, winlosses, 
                      by = "summonerId",
                      all.x = T)


# Champ Mastery
champ_df <- tibble()
champ_iterable <- tibble(
    "summonerId" = data_reduced$summonerId,
    "championId" = data_reduced$championId
)


for (i in 1:10) {
    print(paste("Attempting: ", champ_iterable$summonerId[i]))
    champ_df <- bind_rows(champ_df,
              get_champ_mastery(champ_iterable$summonerId[i],
                                champ_iterable$championId[i]))
}

data_champs <- merge(data_wl, champ_df, 
                 by = "summonerId",
                 all.x = T)



# Streak
streaks <- tibble()
for(id in data_reduced$puuid){
    print(paste("Attempting:", id))
    streaks <- bind_rows(streaks, get_streak(id, recent_matchId))
}

data_streaks <- merge(data_champs, streaks,
                      by = "puuid",
                      all.x = T)



# Match timeline data
match_timeline <- get_match_timeline(recent_matchId)
data_final <- merge(data_streaks, match_timeline,
                    by = "puuid",
                    all.x = T) %>% 
    select(puuid, matchId, )


# Pivot wider to make match record
data_wide <- data_final %>% 
    mutate(teamId = ifelse(teamId == 100,
                           "T1",
                           "T2")) %>% 
    select(-c(1, 2, 6, 7, 8)) %>% 
    pivot_wider(
        names_from = c(teamId, teamPosition),
        values_from = -c(1, 3),
        names_glue = "{teamId}_{teamPosition}_{.value}")
    



# Misc
fields <- colnames(test_win)
view(fields)
rm(list = ls(pattern = "^test"))
