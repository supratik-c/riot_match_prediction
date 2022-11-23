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

match_data <- bind_cols(match_win, firstBlood, firstTower) %>% 
    filter(teamId == 100) %>% 
    mutate(matchId = recent_matchId) %>% 
    select(t1_firstBlood = firstBlood,
           t1_firstTower = firstTower,
           matchId,
           t1_win = win)

timestamp <- anytime(as.numeric(substr(match$info$gameCreation, 1, 10)))


# Extract participant data
data_raw <- match$info$participants
data_reduced <- data_raw %>% 
    select(puuid, summonerId, teamId, win, teamPosition,
           championId, gameEndedInEarlySurrender)


# Extract W/L ratio for each player
winlosses <- map_dfr(data_reduced$summonerId, get_player_wl)
data_wl <- merge(data_reduced, winlosses, 
                      by = "summonerId",
                      all.x = T)


# Extract & join Champ Mastery
champ_df <- tibble()
champ_iterable <- tibble(
    "summonerId" = data_reduced$summonerId,
    "championId" = data_reduced$championId
)


for (i in 1:10) {
    champ_df <- bind_rows(champ_df,
              get_champ_mastery(champ_iterable$summonerId[i],
                                champ_iterable$championId[i]))
}

data_champs <- merge(data_wl, champ_df, 
                 by = "summonerId",
                 all.x = T)



# Extract & join Streak
streaks <- map_dfr(data_reduced$puuid, get_streak, recent_matchId)

data_streaks <- merge(data_champs, streaks,
                      by = "puuid",
                      all.x = T)



# Pivot wider to make match record
data_wide <- data_streaks %>% 
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
