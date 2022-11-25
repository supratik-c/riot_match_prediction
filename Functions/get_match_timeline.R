get_match_timeline <- function(matchId)
{
    url <- paste("https://europe.api.riotgames.com/lol/match/v5/matches",
                 matchId,
                 "timeline",
                 sep = "/")
    
    tl <- fromJSON(rawToChar(riot_get(url)$content))
    
    # Participant Lookup
    participants <- tl$info$participants
    matchId <- tl$metadata$matchId
    
    # Iteratively collate participant data
    master_data <- tibble()
    for(pframe in tl$info$frames$participantFrames){
        
        # Extract individual records
        goldAtTen <- pframe$totalGold[10]
        goldAtFifteen <- pframe$totalGold[15]
        participantId <- pframe$participantId[1]
        damageTakenByTen <- pframe$damageStats$totalDamageTaken[10]
        damageTakenByFifteen <- pframe$damageStats$totalDamageTaken[15]
        damageDealtByTen <- pframe$damageStats$totalDamageDoneToChampions[10]
        damageDealtByFifteen <- pframe$damageStats$totalDamageDoneToChampions[15]
        
        # Join to master list
        data <- list(
            "goldAtTen" = goldAtTen,
            "goldAtFifteen" = goldAtFifteen,
            "participantId" = participantId,
            "damageTakenByTen" = damageTakenByTen,
            "damageTakenByFifteen" = damageTakenByFifteen,
            "damageDealtByTen" = damageDealtByTen,
            "damageDealtByFifteen" = damageDealtByFifteen
        )
        master_data <- bind_rows(master_data, data)
    }
    
    # Join on puuid
    master_data <- merge(master_data, participants,
                         by = "participantId",
                         all.x = T) %>% 
        select(-c(participantId))
    
    return(master_data)
    
}