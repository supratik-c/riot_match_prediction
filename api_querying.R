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

# API Key Set and Get

api_key <- Sys.getenv("RIOT_API_KEY")


# Misc & Testing
player <- get_sample_player(api_key)


# Metadata
update_champions(riot_db)
champions <- riot_db %>% 
    tbl("D_CHAMPIONS") %>% 
    collect()


update_items(riot_db)
items <- riot_db %>% 
    tbl("D_ITEMS") %>% 
    collect()


