

# Database
conn <- dbConnect(odbc(),
                  Driver = "ODBC Driver 13 for SQL Server",
                  Server = "supratik.database.windows.net",
                  Database = "riot_data",
                  UID = Sys.getenv("azure_user"),
                  PWD = Sys.getenv("azure_pass"),
                  Port = 1433,
                  timeout = 60)




# Metadata

queue_id <- 420
queue_name <- "RANKED_SOLO_5x5"



#

# Misc Code
cont <- fromJSON(rawToChar(smurfsuo$content))


