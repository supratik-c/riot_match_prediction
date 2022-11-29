library(tidyverse)
library(tidymodels)
library(DBI)
library(RODBC)
library(odbc)

azure <- dbConnect(odbc(),
                     Driver = "ODBC Driver 18 for SQL Server",
                     Server = "supratik.database.windows.net",
                     Database = "riot_data",
                     UID = Sys.getenv("AZURE_USER"),
                     PWD = Sys.getenv("AZURE_PASS"),
                     Port = 1433,
                     timeout = 60)

# Extract data
raw_data_matches <- azure %>% 
    tbl("F_MATCHES") %>% 
    collect()



# Duplicates -------------------
matches_deduped <- raw_data_matches %>% 
    mutate(key = paste0(summonerId, matchId)) %>% 
    distinct(key, .keep_all = T) 


# Filter early surrenders ----
data_filtered <- matches_deduped %>% 
    filter(gameEndedInEarlySurrender == F) %>% 
    mutate(teamId = ifelse(teamId == 100, "T1", "T2"),
           totalGames = wins + losses) %>% 
    select(teamId, teamPosition, matchId, totalGames,
           rank, numeric_rank, championPoints, totalMastery,
           nchamps, meanMastery, streak,
           T1_firstBlood = t1_firstBlood, 
           T1_firstTower = t1_firstTower,
           goldAtTen, goldAtFifteen, damageTakenByTen, damageTakenByFifteen,
           damageDealtByTen, damageDealtByFifteen, 
           T1_win = t1_win)

# Missing data -------------------


# Count NAs per column
data_filtered$streak[data_filtered$streak == 0] <- NA
na_count <- enframe(map(data_filtered, ~sum(is.na(.))))

data_clean <- data_filtered %>% 
    drop_na()


# Streaks
na_streaks <- data_filtered %>% 
    filter(is.na(streak))

data_streaks <- data_filtered %>% 
    filter(!is.na(streak)) %>% 
    select(streak) 
    

s_plot <- ggplot(data_streaks,
                 aes(x = streak)) +
    geom_histogram()

s_plot


# Pivot wider to make match records ----
data_wide <- data_filtered %>% 
    head(30) %>% # Testing with a few matches
    arrange(matchId, teamId, teamPosition) %>% 
    group_by(matchId) %>% 
    pivot_wider(
        names_from = c(teamId, teamPosition),
        values_from = -c(teamId, teamPosition, matchId, T1_firstBlood, T1_firstTower, T1_win),
        names_glue = "{teamId}_{teamPosition}_{.value}")







# Class balance -------------------




# Representativeness -------------------




# Collinearity -------------------






