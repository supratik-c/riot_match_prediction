riot_get <- function(url)
{
    
    # Manage call rate (Cannot exceed 1 every 1.2 seconds)
    if(!exists("prev_req_time")) {
        
        # Set initial time value for first call
        prev_req_time <<- Sys.time()
        data <- httr::GET(url, add_headers("X-Riot-Token" = Sys.getenv("RIOT_API_KEY")))
        
        if(data$status_code != 200) stop(paste("Error. Status Code:", data$status_code))
        return(data)
        
        
    } else {
        
        # Sleep until 1.2 seconds have elapsed
        time_since_req <- Sys.time() - prev_req_time
        if (time_since_req < 1.2) {
            Sys.sleep(1.2 - time_since_req)
        }
        
        prev_req_time <<- Sys.time()
        data <- httr::GET(url,
                          add_headers("X-Riot-Token" = Sys.getenv("RIOT_API_KEY")))
        if(data$status_code != 200) stop(paste("Error. Status Code:", data$status_code))
        return(data)
    }
    
}