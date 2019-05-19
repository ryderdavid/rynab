library(googlesheets)
library(googlesheets4)

# pull the budget ID from the file stored in the keys dir. 
# TO DO: make this interactive.
budget_id <- read_file('keys/budget_id') %>% str_trim()


# Get the Expenses Google Sheet by its key,
exp_gs <- gs_key('1ZsHw9HhphBLj18kVY7xo861p6H5ABGYfH0UuStyX0Lg')

# Open as DF the first sheet with proper formatting
exp_df <- gs_read(exp_gs, ws = 1, skip = 1) %>% 
  mutate(
    Timestamp = as.POSIXct(Timestamp, format = '%m/%d/%Y %H:%M:%S')
  )

# Get the latest date of a transaction in the most recently archived sheet
# (sheet 2) -- this will be the 'since' date when calling YNAB API for
# transactions


######## NEED TO CREATE FLEXIBLE TIME CASTING TO CORRECT FOR WEIRD DATA ENTRY


last_date <- 
  gs_read(exp_gs, ws = 2, skip = 1) %>% 
  mutate(
    Timestamp = as.Date(Timestamp)  
  ) %>% pull(Timestamp)
# %>% 
#   pull(Timestamp) %>% 
#   max() %>% 
#   as.Date()
#   

ytest <- get_transactions(budget_id = budget_id, since_date = last_date)

ynab_trx <- 
  get_transactions(budget_id = budget_id, since_date = last_date) %>% 
  filter(data.transactions.flag_color %in% c('red', 'purple')) %>%
  transmute(
    Timestamp = as.POSIXct(data.transactions.date),
    `Who is Paying?` = 'Ryder',
    Amount = (data.transactions.amount / 1000 * -1) %>% round(digits = 0),
    Purpose = case_when(.$data.transactions.flag_color == 'red' ~ 'for us',
                        .$data.transactions.flag_color == 'purple' ~ 'for you'),
    Description = paste(data.transactions.payee_name,
                        data.transactions.memo,
                        sep= ' - ')
    )




# union(trx, exp)



