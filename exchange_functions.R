# Define functions that return values of EUR exchange rate to trigger email
# notification and to pass to email body

# Load packages
library(httr2)
library(tidyverse)

# Retrieve EUR rate today ------------------------------------------------------------
EUR_rate_today <- function(){
  
  # Define API base URL
  base_url_fx <- "https://api.frankfurter.dev/v1/"
  
  # Define API endpoints
  endpoint_fx <- "latest?from=GBP&to=EUR"
  
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
  rate_value <- results_fx$rates$EUR
  
  return(rate_value)
  
}
