library(googlesheets)
# install.packages("devtools")
# devtools::install_github("tidyverse/googlesheets4")
# install.packages("httpuv")
library(googlesheets4)
library(lubridate)

source('get_functions.R')

# pull the budget ID from the file stored in the keys dir. 
budget_id <- 'last-used'

#read_file('keys/budget_id') %>% str_trim()

# Identify Spender for script - this can be made into a variable later
spender <- 'Ryder'

# Get the Expenses Google Sheet by its key,
exp_gs <- read_file('keys/expenses_sheet_id') %>% str_trim() %>% gs_key()



# Iterate through sheets to find the most recent sheet with at least one
# transaction by the spender to find the spender's last transaction
s <- 1  # sheet counter token
while (s > 0) {
  sht <- gs_read(exp_gs, ws = s, skip = 1) %>% 
    mutate(  # remove formatting from transaction amounts
      `Expense Amount` = as.numeric(str_extract(`Expense Amount`, '\\d+'))
      ) 
  
  # Check to see if spender occurs in currently loaded sheet
  if (!spender %in% unique(sht$`Who is Paying?`)) {  # if no, iterate to next
    s <- s + 1
  } else {break}  # if yes, got the right sheet
}


# Get the latest date of spender's transactions logged in expenses tracker --
# this will be the 'since' date when calling YNAB API for transactions
last_date <- 
  sht %>% 
  filter(`Who is Paying?` == spender) %>% 
  pull(Timestamp) %>% 
  max()


# Get all transactions from selected budget by budget_id and since the selected
# last_date
ynab_trx <- 
  get_transactions(budget_id = budget_id, since_date = last_date) %>% 
  filter(data.transactions.flag_color %in% c('red', 'purple')) %>%
  transmute(
    Timestamp        = ymd_hms(paste(data.transactions.date, '00:00:00')),
    `Who is Paying?` = 'Ryder',
    `Expense Amount` = (data.transactions.amount / 1000 * -1) %>% round(digits = 0),
    `I spent this:`  = case_when(
                         .$data.transactions.flag_color == 'red' ~ 'for us',
                         .$data.transactions.flag_color == 'purple' ~ 'for you'
                         ),
    Description      = ifelse(
                         is.na(.$data.transactions.memo), str_squish(.$data.transactions.payee_name),
                         paste(str_squish(data.transactions.payee_name), 
                               str_squish(data.transactions.memo), sep = ' - ')
                         )
    ) %>% 
  
  # Next two lines remove any transactions that have already been logged on the
  # google sheet for the "since" day in the tracker
  bind_rows(., sht) %>% 
  filter(`Who is Paying?` == spender & Timestamp >= last_date & !duplicated(.))
cat('Got ', nrow(ynab_trx), ' new shared transactions from YNAB.', sep = '')


# append all new transactions from YNAB to expenses sheet
for (r in 1:nrow(ynab_trx)) {
  gs_add_row(exp_gs, ws = 1, input = ynab_trx[r,], verbose = F)
  
  # notify of successful upload to sheet.
  cat('Added: \"$',  
      ynab_trx[r,]$`Expense Amount`, 
      ' - ', 
      ynab_trx[r,]$Description, 
      '\" to tracker.\n', 
      sep = ''
      )
}

cat(nrow(ynab_trx), ' new transactions added to', ' \"', 
    exp_gs$sheet_title, '\" sheet from ', spender, '.', sep=''
    )
cat('Total: $', sum(ynab_trx$`Expense Amount`), sep='')
cat('Total shared costs: $', sum(ynab_trx$`Expense Amount`[ynab_trx$`I spent this:` == 'for us']), sep='')
cat('Total full-payback costs: $', sum(ynab_trx$`Expense Amount`[ynab_trx$`I spent this:` == 'for you']), sep='')
browseURL(exp_gs$browser_url)
