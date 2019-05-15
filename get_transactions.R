install.packages('tidyverse')
library(tidyverse)
library(httr)
library(jsonlite)

y_api_key <- read_file('.keys/y_api_key') %>% str_trim()
budget_id <- read_file('.keys/budget_id') %>% str_trim()

y_api_url <- 'https://api.youneedabudget.com/v1'
since_date <- '2019-05-01'


get_transactions <- function(budget_id, y_api_key, since_date) {
  
  require(tidyverse)
  require(httr)
  require(jsonlite)
  
  request <- GET(url = paste(y_api_url, 'budgets', budget_id, 'transactions', sep = '/'),
                 query = list(since_date = since_date),
                 add_headers(Authorization = paste('Bearer', y_api_key, sep = ' ')))
  
  response <- content(request, as = 'text', encoding = 'UTF-8')
  df <- fromJSON(response, flatten = TRUE) %>% data.frame()
  
  return(df)
  
}

trx <- get_transactions(budget_id = budget_id, y_api_key, since_date = since_date)

