
# libraries

library(future)
library(tidymodels)
library(tidyverse)
library(recipes)
library(glmnet)
library(corrplot)
library(regclass)

#-----

set.seed(206039397)

#-----

# parallel computing

# num of cores
core_count <- availableCores() - 1

plan("multicore", workers = core_count)

#-----

# import data
df <- read.csv("C:/Users/lavil/source/repos/LukVill/Misc Data/export_freq.csv")

#----- DATA MANIPULATION

# make response variable column if the variable exists (if either event exists, set to 1), remove id cols, remove 18 and 7

df <- df %>% mutate(purchased = if_else(X18 != 0 | X7 != 0, 1, 0)) %>% select(-c(1,2,9,20,29))

# turn all cols into numeric
df <- apply(df,2,as.numeric) %>% as.data.frame()

# turn response into factor
df$purchased <- as.factor(df$purchased)

#----- EDA FOR MODELING

# multicollinearity
corrplot(cor(df %>% select(-purchased)))

# event 9 is nonexistent for all customers

#----- TRAINING PREP

# split data into training, validation, and testing
split_obj <- df %>% initial_validation_split(prop = c(0.5,0.2))

train <- training(split_obj)

val <- validation(split_obj)

test <- testing(split_obj)

# analyze data to see what to do for recipes
# summary(train)
# apply(df,2,var)

#----- MODEL DECLARATION

log_model <- logistic_reg(penalty = 0) %>% set_mode("classification") %>% set_engine(engine = "glmnet")

#----- LOG REG RECIPE

corr_thresh <- 1

l_norm_rec <- recipe(purchased ~ ., data = train) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors(), threshold = corr_thresh) %>% step_normalize(all_predictors())
# l_log_rec <- recipe(purchased ~ ., data = train) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors(), threshold = 0.6) %>% step_log(all_predictors())
l_cen_rec <- recipe(purchased ~ ., data = train) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors(), threshold = corr_thresh) %>% step_center(all_predictors())
l_norm_cen_rec <- recipe(purchased ~ ., data = train) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors(), threshold = corr_thresh) %>% step_normalize(all_predictors()) %>% step_center(all_predictors())


# rec_list <- list(l_norm_rec = l_norm_rec, l_log_rec = l_log_rec, l_cen_rec = l_cen_rec)

#----- LOG REG MODEL BUILDING

# wf_set <- workflow_set(preproc = rec_list, models = list(log_model), cross = TRUE)

# log_log_wf <- workflow() %>% add_recipe(l_log_rec) %>% add_model(log_model)

log_norm_wf <- workflow() %>% add_recipe(l_norm_rec) %>% add_model(log_model)

log_cen_wf <- workflow() %>% add_recipe(l_cen_rec) %>% add_model(log_model)

log_norm_cen_wf <- workflow() %>% add_recipe(l_norm_cen_rec) %>% add_model(log_model)

# params <- wf_set %>% extract_parameter_set_dials()

# help("extract_parameter_set_dials")
#----- PARAMETER TUNING

wf_metrics <- metric_set(f_meas)
# 
# param_grid <- grid_regular(params)
# 
# log_log_wf_res <- log_log_wf %>% tune_grid(grid = param_grid, metrics = met_set)
# 
# log_cen_wf_res

# wf_set_res <- wf_set %>% workflow_map(fn = "tune_grid", grid = 5, resamples = split_obj, verbose = TRUE, metrics = wf_metrics)
# wf_set_res <- wf_set %>% workflow_map(recipe = rec_list,model = list(log_model))
# results <- workflows_grid %>%
#   map(function(workflow) {
#     workflow %>%
#       fit(data_train) %>%
#       predict(new_data = data_test) %>%
#       bind_cols(data_test) %>%
#       metrics(truth = your_target_variable, estimate = .pred_class)
#   })

#----- FITTING

# log_log_wf_fit <- log_log_wf %>% fit(data = val)

log_norm_wf_fit <- log_norm_wf %>% fit(data = val)

log_cen_wf_fit <- log_cen_wf %>% fit(data = val)

log_norm_cen_wf_fit <- log_norm_cen_wf %>% fit(data = val)


#----- PREDICTION

# log_log_wf_res <- log_log_wf_fit %>% predict(new_data = test) %>% pull(.pred_class)

log_norm_wf_res <- log_norm_wf_fit %>% predict(new_data = test) %>% pull(.pred_class)

log_cen_wf_res <- log_cen_wf_fit %>% predict(new_data = test) %>% pull(.pred_class)

log_norm_cen_wf_res <- log_norm_cen_wf_fit %>% predict(new_data = test) %>% pull(.pred_class)

#----- METRICS

# F SCORE
f_meas_vec(truth = test$purchased, estimate = log_norm_wf_res)
f_meas_vec(truth = test$purchased, estimate = log_cen_wf_res)
f_meas_vec(truth = test$purchased, estimate = log_norm_cen_wf_res)

# ACCURACY (exact matches/total count)
sum(test$purchased == log_norm_wf_res)/length(test$purchased)
sum(test$purchased == log_cen_wf_res)/length(test$purchased)
sum(test$purchased == log_norm_cen_wf_res)/length(test$purchased)

# roc_auc_vec(truth = test$purchased, estimate = as.numeric(log_norm_wf_res))
# roc_auc_vec(truth = test$purchased, estimate = as.numeric(log_cen_wf_res))
# roc_auc_vec(truth = test$purchased, estimate = as.numeric(log_norm_cen_wf_res))

preds <- data.frame(t = test$purchased, e = as.numeric(log_norm_cen_wf_res)-1)

#----- FEATURE EXTRACTION

log_norm_cen_wf_fit %>%  extract_fit_parsnip() %>% tidy() %>% arrange(by = desc(abs(estimate)))

#----- CONCLUSIONS

# glm didn't work that well
# glmnet used
# intercept is log odds of success if 
# correlation threshold seemed to help at first until glmnet used
# normalization/centering have the same outputs

# REVISED MODELING: remove event 28 because it's after purchasing


#----- PREDICTION OUTPUT

# take the top 5 features that positively contribute to a purchase

top_feat_purchase <- log_norm_cen_wf_fit %>%  extract_fit_parsnip() %>% tidy() %>% 
arrange(by = desc(abs(estimate))) %>% filter(estimate >= 0) %>% head(n = 5) %>% pull(term)

df <- read.csv("C:/Users/lavil/source/repos/LukVill/Misc Data/export_freq.csv")

preds <- log_norm_cen_wf_fit %>% predict(new_data = df) %>% pull(.pred_class)

df <- df %>% mutate(preds = preds)
 
# df <- df %>% mutate(purchased = if_else(X18 != 0 | X7 != 0, 1, 0)) %>%
# filter(purchased == 1)

write.csv(df, file = "export_purchased_preds.csv")

#----- GRAPHICS

top_feat <- log_norm_cen_wf_fit %>%  extract_fit_parsnip() %>% tidy() %>% 
  arrange(by = desc(abs(estimate)))

top_feat_sum <- top_feat %>% slice(-2) %>% select(estimate) %>% mutate(estimate = abs(estimate)) %>% sum()

top_feat_pct <- top_feat %>% select(term,estimate) %>% 
mutate(estimate = abs(estimate)/top_feat_sum) %>% slice(-2)



