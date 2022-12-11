library(tidyverse)
library(tidymodels)
library(DBI)
library(RODBC)
library(odbc)
library(ggcorrplot)

azure <- dbConnect(odbc(),
                     Driver = "ODBC Driver 18 for SQL Server",
                     Server = "supratik.database.windows.net",
                     Database = "riot_data",
                     UID = Sys.getenv("AZURE_USER"),
                     PWD = Sys.getenv("AZURE_PASS"),
                     Port = 1433,
                     timeout = 60)

# Extract data
raw_ <- azure %>% 
    tbl("F_MATCHES") %>% 
    collect()



# Remove Duplicates -------------------
cleaned_ <- raw_ %>% 
    mutate(key = paste0(summonerId, matchId)) %>% 
    distinct(key, .keep_all = T) 


# Filter out early surrenders ----
cleaned_surr <- cleaned_ %>% 
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

### Missing data -------------------


# Count NAs per column
cleaned_surr$streak[cleaned_surr$streak == 0] <- NA
na_counts_raw <- enframe(unlist(map(cleaned_surr, ~sum(is.na(.))))) %>% 
    mutate(pc = round(as.numeric(value)*100/nrow(cleaned_surr), 3))

# Fill Master Rank
cleaned_rank <- cleaned_surr %>% 
    mutate(numeric_rank = ifelse(rank == "MASTERI", 25, numeric_rank))

na_counts_rank <- enframe(unlist(map(cleaned_rank, ~sum(is.na(.))))) %>% 
    mutate(pc = round(as.numeric(value)*100/nrow(cleaned_rank), 3))

# Filter out matches with NAs in columns except streak
matches_with_na <- cleaned_rank %>% 
    select(-streak) %>% 
    filter_all(any_vars(is.na(.))) %>% 
    distinct(matchId) %>% 
    pull()

cleaned_na <- cleaned_rank %>% 
    filter(!(matchId %in% matches_with_na))

na_counts_cleaned <- enframe(unlist(map(cleaned_na, ~sum(is.na(.))))) %>% 
    mutate(pc = round(as.numeric(value)*100/nrow(cleaned_na), 3))




# Collinearity -------------------
cleaned_strk <- cleaned_na %>% 
    filter(!is.na(streak))

qq_before <- ggplot(cleaned_strk, aes(sample = streak)) +
    stat_qq() +
    stat_qq_line() + theme_classic()

strk_norm <- ks.test(cleaned_strk$streak, "pnorm") # Not normal


corr_data <- cleaned_strk %>% 
    select(streak, championPoints, totalGames, totalMastery,
           nchamps, meanMastery, goldAtTen, damageTakenByTen, damageDealtByTen,
           goldAtFifteen, damageTakenByFifteen, damageDealtByFifteen)

corr <- round(cor(corr_data), 3)
corr_p <- cor_pmat(corr_data)
ggcorrplot(corr, p.mat = corr_p, insig = "blank")



# Impute streaks ----




# Representativeness -------------------



# Pivot wider to make match records ----
data_wide <- clean_strk %>% 
    head(30) %>% # Testing with a few matches
    arrange(matchId, teamId, teamPosition) %>% 
    group_by(matchId) %>% 
    pivot_wider(
        names_from = c(teamId, teamPosition),
        values_from = -c(teamId, teamPosition, matchId, T1_firstBlood, T1_firstTower, T1_win),
        names_glue = "{teamId}_{teamPosition}_{.value}")





# Class balance -------------------












