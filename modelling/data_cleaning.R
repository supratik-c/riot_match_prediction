library(tidyverse)
library(tidymodels)
library(ggcorrplot)
library(fBasics)


# Extract data
raw_ <- read_csv("modelling/raw_data.csv")


# Remove Duplicates -------------------
dups <- raw_ %>% 
    group_by(matchId) %>% 
    count() %>% 
    filter(n > 10) %>% 
    pull(matchId)

cleaned_ <- raw_ %>% 
    filter(!(matchId %in% dups))

# Filter out early surrenders ----
cleaned_surr <- cleaned_ %>% 
    filter(gameEndedInEarlySurrender == F) %>% 
    mutate(teamId = ifelse(teamId == 100, "T1", "T2"),
           totalGames = wins + losses) %>% 
    select(teamId, teamPosition, matchId, totalGames,
           rank, numeric_rank, tier, championPoints, totalMastery,
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



# Fill Ranks
cleaned_rank <- cleaned_surr %>% 
    mutate(numeric_rank = case_when(
        rank == "MASTERI" ~ 25,
        rank == "GRANDMASTERI" ~ 26,
        rank == "CHALLENGERI" ~ 27,
        T ~ numeric_rank
    ),
    tier = case_when(
        rank == "MASTERI" ~ "MASTER",
        rank == "GRANDMASTERI" ~ "GRANDMASTER",
        rank == "CHALLENGERI" ~ "CHALLENGER",
        T ~ tier
    ))

na_counts_rankfix <- enframe(unlist(map(cleaned_rank, ~sum(is.na(.))))) %>% 
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



# Initial Collinearity -------------------
cleaned_strk <- cleaned_na %>% 
    filter(!is.na(streak))


corr_data <- cleaned_strk %>% 
    select(streak, championPoints, totalGames, totalMastery,
           nchamps, meanMastery, goldAtTen, damageTakenByTen, damageDealtByTen,
           goldAtFifteen, damageTakenByFifteen, damageDealtByFifteen)

corr <- round(cor(corr_data), 3)
corr_p <- cor_pmat(corr_data)
init_corr <- ggcorrplot(corr, p.mat = corr_p, insig = "blank", lab = T)



# Impute streaks ----

# Normality
qq_before <- ggplot(cleaned_strk, aes(sample = streak)) +
    stat_qq() +
    stat_qq_line() + 
    theme_classic() +
    labs(y = "Streak")


strk_norm <- dagoTest(cleaned_strk$streak) # Not normal


# Mean Value Streak imputation
stk_win_mean <- mean(cleaned_na$streak[cleaned_na$T1_win], na.rm = T)
stk_loss_mean <- mean(cleaned_na$streak[!cleaned_na$T1_win], na.rm = T) 

cleaned_final <- cleaned_na %>% 
    mutate(streak = case_when(
        is.na(streak) & T1_win ~ stk_win_mean,
        is.na(streak) & !T1_win ~ stk_loss_mean,
        T ~ streak
    ))


# Rank Representativeness -------------------
rank_count <- cleaned_final %>% 
    group_by(tier) %>%
    count() 



# Class balance -------------------
win_count <- cleaned_final %>% 
    group_by(T1_win) %>% 
    count()



# Pivot wider to make match records ----
data_wide <- cleaned_final %>% 
    arrange(matchId, teamId) %>% 
    group_by(matchId, teamId) %>% 
    summarise(totalGames = mean(totalGames),
              numeric_rank = mean(numeric_rank),
              championPoints = mean(championPoints),
              totalMastery = mean(totalMastery),
              nchamps = mean(nchamps),
              meanMastery = mean(meanMastery),
              streak = mean(streak),
              T1_firstBlood = mean(T1_firstBlood),
              T1_firstTower = mean(T1_firstTower),
              goldAtTen = mean(goldAtTen),
              goldAtFifteen = mean(goldAtFifteen),
              damageTakenByTen = mean(damageTakenByTen),
              damageTakenByFifteen = mean(damageTakenByFifteen),
              damageDealtByTen = mean(damageDealtByTen),
              damageDealtByFifteen = mean(damageDealtByFifteen),
              T1_win = mean(T1_win))


# Make predictors for T1 relative to T2
data_final <- data_wide %>%
    arrange(matchId, teamId) %>% 
    group_by(matchId) %>% 
    pivot_wider(
        names_from = c(teamId),
        values_from = -c(teamId, matchId, T1_firstBlood, T1_firstTower, T1_win),
        names_glue = "{teamId}_{.value}") %>% 
    ungroup() %>% 
    mutate(totalGamesDiff = T1_totalGames - T2_totalGames,
           rankDiff = T1_numeric_rank - T2_numeric_rank,
           championPointsDiff = T1_championPoints - T2_championPoints,
           totalMasteryDiff = T1_totalMastery - T2_totalMastery,
           nchampsDiff = T1_nchamps - T2_nchamps,
           meanMasteryDiff = T1_meanMastery - T2_meanMastery,
           streakDiff = T1_streak - T2_streak,
           goldAtTenDiff = T1_goldAtTen - T2_goldAtTen,
           goldAtFifteenDiff = T1_goldAtFifteen - T2_goldAtFifteen,
           damageTakenByTenDiff = T1_damageTakenByTen - T2_damageTakenByTen,
           damageTakenByFifteenDiff = T1_damageTakenByFifteen - T2_damageTakenByFifteen,
           damageDealtByTenDiff = T1_damageDealtByTen - T2_damageDealtByTen,
           damageDealtByFifteenDiff = T1_damageDealtByFifteen - T2_damageDealtByFifteen) %>% 
    select(T1_firstBlood, T1_firstTower, ends_with("Diff"), T1_win)





# Second Collinearity check ----
final_corr_data <- data_final %>% 
    select(-c(T1_win, T1_firstBlood, T1_firstTower))
final_corr <- round(cor(final_corr_data), 3)
final_corr_p <- cor_pmat(final_corr_data)
final_corrplot <- ggcorrplot(final_corr, p.mat = final_corr_p, insig = "blank")

# Normalize numeric predictors
scaled_data <- data_final %>% 
    mutate_at(
        c("totalGamesDiff", "rankDiff", "championPointsDiff", "totalMasteryDiff",
          "nchampsDiff", "meanMasteryDiff", "streakDiff", "goldAtTenDiff", 
          "goldAtFifteenDiff", "damageTakenByTenDiff", "damageTakenByFifteenDiff", 
          "damageDealtByTenDiff", "damageDealtByFifteenDiff"),
        ~(scale(.) %>% as.vector)
    ) %>% 
    mutate(T1_win = ifelse(T1_win, "win", "loss"))



# Write data
write_csv(scaled_data, "modelling/cleaned_data.csv")
