# libraries and directory set up
setwd("C:/Users/lavil/source/repos/LukVill/MATH-M148")

library(tidyverse)
library(lubridate)
library(hms)
library(arules)
library(arulesViz)
library(data.table)
library(scales)

# event definition set up
event_def <- fread("Event Definitions.csv")

# dataset set up
luke_export_dir <- 'C:/Users/lavil/source/repos/LukVill/Misc Data/export_no_dup.csv'

# this is based off of Luke's directory!!! #
export <- fread(luke_export_dir, nrows = 1000)
export <- export %>% select(-1)
export$Date <- as.Date(export$Date)
glimpse(export)


# DATA PREP

# get max journey length
# group by customer and account, get highest count
max_journey_len <- export %>% group_by(customer_id,account_id) %>% count() %>% ungroup() %>% select(n) %>% max()

# make dataframe of customer/acct and their event lists
# get each unique customer account pairs and make into dataframe
res <- export %>% group_by(customer_id,account_id) %>% 
select(customer_id,account_id) %>% distinct() %>% as.data.frame()
# for each customer/acct, filter data to that pair, arrange data via date and time,
# and extract events in order
for pair in 
{
  print(pair)
}
