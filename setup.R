# Source packages & functions
mapply(source, list.files("Functions", recursive = TRUE, full.names = TRUE))



# Database
riot_db <- dbConnect(odbc(),
                     Driver = "ODBC Driver 17 for SQL Server",
                     Server = "supratik.database.windows.net",
                     Database = "riot_data",
                     UID = Sys.getenv("AZURE_USER"),
                     PWD = Sys.getenv("AZURE_PASS"),
                     Port = 1433,
                     timeout = 60)

# Lookups
champions <- riot_db %>% 
    tbl("D_CHAMPIONS") %>% 
    collect()


items <- riot_db %>% 
    tbl("D_ITEMS") %>% 
    collect()


ranks <- riot_db %>% 
    tbl("D_RANKS") %>% 
    collect()



# Update Lookups
update_champions(riot_db)
update_items(riot_db)
update_ranks(riot_db)


# API Key Setter
Sys.setenv("RIOT_API_KEY" = "")

