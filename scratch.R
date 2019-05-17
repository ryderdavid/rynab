source('get_transactions.R')



budget_id <- read_file('keys/budget_id') %>% str_trim()





trx <- get_transactions(budget_id = budget_id, since_date = '2015-01-01')

trx_for_export <- 
  trx %>% 
  filter(data.transactions.flag_color %in% c('red', 'purple')) %>% 
  transmute(
    Timestamp = as.Date(data.transactions.date), 
    Payee = 'Ryder',
    Amount = data.transactions.amount / 1000 * -1,
    Purpose = case_when(.$data.transactions.flag_color == 'red' ~ 'for us',
                        .$data.transactions.flag_color == 'purple' ~ 'for you'),
    Description = paste(data.transactions.payee_name, data.transactions.memo, sep= ' - ')
  )

trx_for_export %>% View()


plot(trx_for_export$Timestamp, trx_for_export$Amount)


trx %>% 
  filter(grepl('travel', str_to_lower(data.transactions.category_name))) %>% 
  filter(data.transactions.amount < 0) %>% 
  mutate(data.transactions.amount = data.transactions.amount / -1000,
         data.transactions.date = as.Date(data.transactions.date)) -> t

plot(t$data.transactions.date, t$data.transactions.amount, cex = 0.2)  
  













budget_list <- get_budgets()

paste("You have", nrow(budget_list), "budgets. Which to select?")
for (b in (budget_list$data.budgets.name)) {
  paste()
}
