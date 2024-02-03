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
export_head <- fread(luke_export_dir, nrows = 1000)
export_head <- export_head %>% select(-1)
export_head$Date <- as.Date(export_head$Date)

# this is based off of Luke's directory!!! #
export <- fread(luke_export_dir)
export <- export %>% select(-1)
export$Date <- as.Date(export$Date)
glimpse(export)

# INITIAL DATA CLEANUP
# filter out duplicate events on the same event timestamp (based off of same event stamp and customer and account)
# mutate to get date and time and seconds



# based on date, time, customer, and account, filter distinct
#export_head <- export_head %>% distinct(customer_id,account_id,event_timestamp,date,hours,mins,secs, .keep_all = T)
#print(export_head, n = 100)
#print(export_head %>% filter(customer_id == 15849251), n = 100)
# print(export_head %>% distinct(customer_id,account_id,date,hours,mins,secs, .keep_all = T), n = 100)
# print(export_head %>% distinct(customer_id,account_id,date,time), n = 100)
# CLEANED UP DUPLICATES
# EDIT: USED PYTHON TO CLEAN UP DUPLICATES




# INITIAL DATA VISUALIZATIONS
# make time series of growth of events, colored by different events

# vector of all days from earliest to latest (11-03-20 to 09-20-23)
date_vector <- seq(as.Date("2020-11-03"),as.Date("2023-09-20"), by = "days") %>% 
data.frame() %>% rename("dates" = ".")
#date_vector %>% colnames()

# count ed per day, pivot wider to get hot-esque notation, right join into date_vector
# mutate each column to replace na's, mutate each col to cumsum 
indv_df <- export %>% group_by(Date,ed_id) %>% count() %>% 
  pivot_wider(names_from = ed_id, values_from = n, values_fill = 0) %>% 
  right_join(date_vector, by = c("Date" = "dates")) %>% arrange(Date) %>%
  mutate(across(-0, ~replace_na(.x, 0))) %>% 
  pivot_longer(cols = -Date, names_to = "ed_id", values_to = "count") %>% ungroup() %>%
  group_by(ed_id) %>% arrange(Date) %>% mutate(total = cumsum(count))

# make legend based on highest total
legend_ordered <- indv_df %>% filter(Date == as.Date("2023-09-20")) %>% arrange(desc(total)) %>% pull(ed_id)

# update ed_id to be factor based on ordered legen
indv_df$ed_id <- factor(indv_df$ed_id, levels = legend_ordered)

# make graph based on x = date, y = total, color = ed_id
indv_graph <- indv_df %>% ggplot(aes(x = Date, y = total, color = ed_id)) + 
geom_line() + theme_minimal() + scale_x_date(date_breaks = "3 months") + scale_color_manual(
values = c("#b00086",
           "#c4cf17",
           "#ff49f9",
           "#01a261",
           "#ac00bb",
           "#72daa2",
           "#de75ff",
           "#27672b",
           "#0064e6",
           "#ffaf50",
           "#b197ff",
           "#575f14",
           "#ff3890",
           "#28daec",
           "#d20053",
           "#00675e",
           "#ff7cc9",
           "#944a00",
           "#016d9b",
           "#a6310f",
           "#f4aff3",
           "#ebbf85",
           "#834283",
           "#b0ac87",
           "#a52f35",
           "#675b7b",
           "#ff7189",
           "#8f405b"), 
name = "Event IDs (ordered)") + 
labs(title = "Cumulative Total Frequencies of Events", x = "Date", y = "Total", legend = "test") +
theme(legend.spacing.y = unit(0.01, 'in')) + 
guides(fill = guide_legend(byrow = TRUE)) + theme(axis.text.x = element_text(angle = 90)) + 
  scale_y_continuous(labels = label_comma())
indv_graph

# # RANGE TRANSFORM VALUES
# # scaling function
# min_max <- function(x,min,max)
# {(x-min)/(max-min)}
# # change totals to be log transformed and then graph
# indv_trans_df <- indv_df %>% ungroup() %>% mutate(total = min_max(total,0,max(total)))


# # make 2nd graph
# indv_2_graph <- indv_trans_df %>% ggplot(aes(x = date, y = total, color = ed_id)) + 
#   geom_line() + theme_minimal() + scale_x_date(date_breaks = "3 months") + scale_color_manual(values = c("#5d8d00",
#                                                                                                          "#ac67ff",
#                                                                                                          "#00cd2f",
#                                                                                                          "#ff42ae",
#                                                                                                          "#7bdc6e",
#                                                                                                          "#704b89",
#                                                                                                          "#ffb11d",
#                                                                                                          "#0fbcff",
#                                                                                                          "#ff5629",
#                                                                                                          "#00cfc6",
#                                                                                                          "#9d392e",
#                                                                                                          "#006e1b",
#                                                                                                          "#d5b1ff",
#                                                                                                          "#b1d263",
#                                                                                                          "#006e91",
#                                                                                                          "#a27d00",
#                                                                                                          "#c2bce2",
#                                                                                                          "#456239",
#                                                                                                          "#a7d29a"), name = "Event IDs") + 
#   labs(title = "Cumulative Total Frequencies of Events", x = "Date", y = "Total", legend = "test") +
#   theme(legend.spacing.y = unit(0.01, 'in')) + 
#   guides(fill = guide_legend(byrow = TRUE)) + theme(axis.text.x = element_text(angle = 90))
# indv_2_graph



# GOAL 1: optimizing purchasing products, people on journeys sometimes do not purchase products
# what triggers customers to purchase products -- which ordered set of events tend to result in product purchasing
# 

# make dataframe for apriori algorithm
export_head %>% group_by(customer_id, account_id, ed_id) %>% count()


# GOAL 2: optimizing starting journeys
# journeys that have 1 step
apporved_cust <- export_head %>% group_by(customer_id, account_id) %>% filter(sum(journey_steps_until_end) == 1)
export_head %>% filter(customer_id == 1483781929)
apporved_cust %>% group_by(ed_id) %>% count()
# most did not do anything, so look into the next step (i.e. journey steps = 2)


# GOAL 3: optimize the speed of the journeys (most popular and quickest)

# CLICK RATE ANALYSIS
# click amount on different events

# TIME SPENT ON EVENTS
# time spent criteria: if at least TWO EVENTS ARE CONSECUTIVE, same name, same day
# then get difference of time of last elem and first elem
# only work with cust, acct, event name, date, times(hms)

# 
unique(event_def$stage)
event_def %>% filter(stage == "Apply for Credit")

export %>% filter(ed_id == 21)


