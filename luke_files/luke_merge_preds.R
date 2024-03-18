library(tidyverse)

export_path <- "C:/Users/lavil/source/repos/LukVill/Misc Data/export.csv"
preds_path <- "C:/Users/lavil/source/repos/LukVill/MATH-M148/luke_files/luke_data/export_purchased_preds.csv"
save_path <- "C:/Users/lavil/source/repos/LukVill/Misc Data/export_preds.csv"

df <- read.csv(export_path)

preds <- read.csv(preds_path)

preds <- preds %>% select(customer_id,account_id,preds)

res <- df %>% right_join(preds, by = c("customer_id", "account_id"))

write.csv(res, save_path)
