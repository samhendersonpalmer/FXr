# Define functions that return values of EUR exchange rate to trigger email
# notification and to pass to email body

# Load packages
library(httr2)
library(tidyverse)

# Retrieve EUR rate today ------------------------------------------------------------
EUR_rate_today <- function(){
  
  # Define access tokens
  access_token_fx <- Sys.getenv("FX_API_KEY")
  
  # Define API base URL
  base_url_fx <- "https://marketdata.tradermade.com/api/v1/"
  
  # Define API endpoints
  endpoint_fx <- paste0("live?api_key=", access_token_fx, "&currency=GBPEUR")
  
  # Define request
  req_fx <- request(base_url_fx) %>% 
    req_url_path_append(endpoint_fx)
  
  # Perform response
  req_fx_perform <- req_fx |> 
    req_perform()
  
  # Extract results from the response
  results_fx <- req_fx_perform |> 
    resp_body_json()
  
  # Extract rate
  rate_value <- results_fx$quotes[[1]]$mid
  timestamp <- results_fx$requested_time
  
  list("EUR" = rate_value, "time" = timestamp)
  
}


# Retrieve EUR rate past 3 days -------------------------------------------
EUR_rate_prev3 <- function(){
  
  # Define API base URL
  base_url_fx <- "https://api.frankfurter.dev/v1/"
  
  # Define API endpoints
  endpoint_fx <- paste0(Sys.Date()-3,"..", Sys.Date(), "?base=GBP&symbols=EUR")
  
  # Define request
  req_fx <- request(base_url_fx) %>% 
    req_url_path_append(endpoint_fx)
  
  # Perform response
  req_fx_perform <- req_fx |> 
    req_perform()
  
  # Extract results from the response
  results_fx <- req_fx_perform |> 
    resp_body_json()
  
  # Extract rate
  rate_values <- stack(results_fx$rates) |> 
    select(date = ind, rate = values) |> 
    arrange(desc(date)) |> 
    mutate(change_24hr = (rate - lead(rate))/lead(rate))
  
  return(rate_values)
  
}

