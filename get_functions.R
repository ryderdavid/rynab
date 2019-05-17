library(tidyverse)
library(magrittr)
library(httr)
library(jsonlite)

y_api_url <- 'https://api.youneedabudget.com/v1'
y_api_key <- read_file('keys/y_api_key') %>% str_trim()



## Gets all budgets and returns as data.frame
get_budgets <- function() {
  
  yk <- y_api_key
  
  request <- GET(url = paste(y_api_url, 'budgets', sep = '/'),
                 add_headers(Authorization = paste('Bearer', 
                                                   yk, sep = ' ')))
  
  response <- content(request, as = 'text', encoding = 'UTF-8')
  df <- fromJSON(response, flatten = TRUE) %>% data.frame()
  
  return(df)
  
}


## Gets transactions in a given budget optionally filtered by since_date param
get_transactions <- function(budget_id, since_date = NULL) {
  
  yk <- y_api_key
  d <- since_date
  b <- budget_id
  
  request <- GET(url = paste(y_api_url, 'budgets', b, 'transactions', sep = '/'),
                 query = list(since_date = d),
                 add_headers(Authorization = paste('Bearer', yk, sep = ' ')))
  
  response <- content(request, as = 'text', encoding = 'UTF-8')
  df <- fromJSON(response, flatten = TRUE) %>% data.frame() %>% as_tibble()
  
  return(df)
  
}

