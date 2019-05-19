library(googlesheets)
library(googlesheets4)

source('get_functions.R')

# pull the budget ID from the file stored in the keys dir. 
budget_id <- read_file('keys/budget_id') %>% str_trim()

# Identify Spender for script - this can be made into a variable later
spender <- 'Ryder'

# Get the Expenses Google Sheet by its key,
exp_gs <- read_file('keys/expenses_sheet_id') %>% str_trim() %>% gs_key()



# Iterate through sheets to find the most recent sheet with at least one
# transaction by the spender to find the spender's last transaction
s <- 1  # sheet counter token
while (s > 0) {
  sht <- gs_read(exp_gs, ws = s, skip = 1)
  
  # Check to see if spender occurs in currently loaded sheet
  if (!spender %in% unique(sht$`Who is Paying?`)) {  # if no, iterate to next
    s <- s + 1
  } else {break}  # if yes, got the right sheet
}



# Get the latest date of spender's transactions in the most recently archived
# sheet (sheet 2) -- this will be the 'since' date when calling YNAB API for
# transactions
last_date <- 
  sht %>% 
  # Next 7 rows clean up any hand entered dates not produced by Google Forms
  separate(Timestamp, c('Date', 'Time'), sep = ' ') %>% 
  mutate(
    Date = str_replace_all(Date, '[:punct:]', '/'),
    Time = ifelse(is.na(Time), '0:00:00', Time)
  ) %>% 
  unite(col = 'Timestamp', Date, Time, sep = ' ') %>% 
  mutate(Timestamp = as.POSIXct(Timestamp, format = '%m/%d/%Y %H:%M:%S')) %>% 
  filter(`Who is Paying?` == spender) %>% 
  pull(Timestamp) %>% 
  max()


# Get all transactions from selected budget by budget_id and since the selected last_date
ynab_trx <- 
  get_transactions(budget_id = budget_id, since_date = last_date) %>% 
  filter(data.transactions.flag_color %in% c('red', 'purple')) %>%
  transmute(
    Timestamp        = as.POSIXct(data.transactions.date),
    `Who is Paying?` = 'Ryder',
    Amount           = (data.transactions.amount / 1000 * -1) %>% round(digits = 0),
    Purpose          = case_when(
                         .$data.transactions.flag_color == 'red' ~ 'for us',
                         .$data.transactions.flag_color == 'purple' ~ 'for you'
                         ),
    Description      = ifelse(
                         is.na(.$data.transactions.memo), .$data.transactions.payee_name,
                         paste(data.transactions.payee_name, 
                               data.transactions.memo, sep = ' - ')
                         )
    )
cat('Got ', nrow(ynab_trx), ' new shared transactions from YNAB.', sep = '')


# append all new transactions from YNAB to expenses sheet
for (r in 1:nrow(ynab_trx)) {
  gs_add_row(exp_gs, ws = 1, input = ynab_trx[r,], verbose = F)
  
  # notify of successful upload to sheet.
  cat('Added: \"$',  
      ynab_trx[r,]$Amount, 
      ' - ', 
      ynab_trx[r,]$Description, 
      '\" to tracker.\n', 
      sep = ''
      )
}

cat(nrow(ynab_trx), ' new transactions added to', ' \"', 
    exp_gs$sheet_title, '\" sheet from ', spender, '.', sep=''
    )
cat('Total: $', sum(ynab_trx$Amount), sep='')
cat('Total shared costs: $', sum(ynab_trx$Amount[ynab_trx$Purpose == 'for us']), sep='')
cat('Total full-payback costs: $', sum(ynab_trx$Amount[ynab_trx$Purpose == 'for you']), sep='')
browseURL(exp_gs$browser_url)
