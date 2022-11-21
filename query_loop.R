# Misc & Testing
player <- get_sample_player()
player_puuid <- get_player_account(player$summonerId, "encryptedSummonerId")$puuid
recent_matchId <- get_player_matches(player_puuid)[1]
sample_match <- get_match(recent_matchId)

participants

