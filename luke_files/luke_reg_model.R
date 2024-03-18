library(tidymodels)
library(tidyverse)
library(recipes)
library(corrplot)
library(regclass)


#----- OPTIMIZE THE JOURNEY
# count amount of journey steps and make as response variable


# filter to customers only with success and activation

# import data
df <- read.csv("C:/Users/lavil/source/repos/LukVill/Misc Data/export_freq.csv")

# if 18, 7, 12,15,29 are all existing then filter to those, remove id cols, remove those 
# prior columns because they are on the ends of the journey
df <- df %>% filter((X7 > 0 | X18 > 0) & (X12 > 0 | X15 | 0 | X29 > 0)) %>% 
select(-c(customer_id,account_id,X7,X18,X12,X15,X29))

# sum up journey steps for each customer
df$journey_steps <- apply(df, 1, sum)

#----- RECIPE ANALYSIS

corrplot(cor(df %>% select(-journey_steps)))
# 27 and 28 are highly collinear, remove from corrplot, remove 0 cols
corrplot(cor(df %>% select(-journey_steps,-X27,-X28,-X9,-X17)))

# check vif within the predictors, remove 0 vars X9 X17
car::vif(lm(journey_steps ~ . -X9 -X17, data = df))
# remove highly collinear vars
car::vif(lm(journey_steps ~ . -X9 -X17 -X5 -X27 -X28, data = df))

# CONCLUSION: REMOVE VARS 5 27 28 FOR MULTI COLLINEARITY

#----- TRAINING PREP

# split data into training, validation, and testing
split_obj <- df %>% initial_validation_split(prop = c(0.5,0.2))

train <- training(split_obj)

val <- validation(split_obj)

test <- testing(split_obj)

#----- MODEL DECLARATION

lin_model <- linear_reg() %>% set_mode("regression")
  
#----- LOG REG RECIPE

corr_thresh <- 0.8

l_norm_rec <- recipe(journey_steps ~ ., data = train) %>% step_nzv(all_predictors()) %>% step_corr(all_predictors(), threshold = tune()) %>% step_normalize(all_predictors())

#----- LOG REG MODEL BUILDING

lin_norm_wf <- workflow() %>% add_recipe(l_norm_rec) %>% add_model(lin_model)


# # TUNE CORRELATION THRESHOLD
# train_folds <- vfold_cv(train,10)
# metrics <- metric_set(rmse,rsq)
# params <- lin_norm_wf %>% extract_parameter_set_dials()
# corr_grid <- grid_regular(params, levels = 10)
# tune_res <- lin_norm_wf %>% tune_grid(resamples = train_folds, grid = corr_grid)
# # rmse results
# tune_res %>% collect_metrics() %>% filter(.metric == "rmse") %>% arrange(mean)
# # rsq results
# tune_res %>% collect_metrics() %>% filter(.metric == "rsq") %>% arrange(desc(mean))

# CONCLUSION: NO CORR FILTER, KEEP ALL PREDICTORS SO FAR

#----- UPDATED WORKFLOW

# EXCLUDE HIGHLY COLLINEAR VARS
df <- df %>% select(-X9, -X17, -X5, -X27, -X28)
train <- train %>% select(-X9, -X17, -X5, -X27, -X28)
val <- val %>% select(-X9, -X17, -X5, -X27, -X28)
test <- test %>% select(-X9, -X17, -X5, -X27, -X28)

# recipe removes X10 X20 for they are 0 variance
l_norm_rec <- recipe(journey_steps ~ ., data = train) %>% step_nzv(all_predictors()) %>% step_normalize(all_predictors())
lin_norm_wf <- workflow() %>% add_recipe(l_norm_rec) %>% add_model(lin_model)

#----- FITTING

lin_norm_wf_fit <- lin_norm_wf %>% fit(data = val)

#----- PREDICTION

lin_norm_wf_res <- lin_norm_wf_fit %>% predict(new_data = test) %>% pull(.pred)

#----- METRICS

metric_df <- bind_cols(t = test$journey_steps, e = lin_norm_wf_res) %>% as.data.frame()

# MAE (mean absolute error - in units)
mae(data = metric_df, truth = t, estimate = e)

# RSQ
rsq(metric_df, truth = t, estimate = e)

# RMSE
rmse(data = metric_df, truth = t, estimate = e)

# MASE and RSQ are high which means results are predictable and accurate
# however, rmse is high which means when the model is inaccurate, the mistakes are large
# i.e. errors are very detrimental, possibly due to outlier journeys in the data

# compare with train data

# TRAIN
# lin_norm_wf_res <- lin_norm_wf_fit %>% predict(new_data = train) %>% pull(.pred)
# 
# metric_df <- bind_cols(t = train$journey_steps, e = lin_norm_wf_res) %>% as.data.frame()
# 
# # RMSE
# rmse(data = metric_df, truth = t, estimate = e)
# 
# # RSQ
# rsq(metric_df, truth = t, estimate = e)
# 
# # MASE
# mase(data = metric_df, truth = t, estimate = e)

# conclusion: since training data RMSE is greater, means model is underfitting, which is okay

#----- FEATURE EXTRACTION

top_linreg_feat <- lin_norm_wf_fit %>%  extract_fit_parsnip() %>% tidy() %>% arrange(by = desc(abs(estimate)))
top_linreg_feat

#----- GRAPHICS

df <- read.csv("C:/Users/lavil/source/repos/LukVill/Misc Data/export_freq.csv")
df <- df %>% filter((X7 > 0 | X18 > 0) & (X12 > 0 | X15 | 0 | X29 > 0)) %>% 
  select(-c(customer_id,account_id,X7,X18,X12,X15,X29))

# sum up journey steps for each customer
df$journey_steps <- apply(df, 1, sum)

# df %>% select(journey_steps,X1,X2,X3,X4,X6)

# COR PLOT FOR COLLINEAR VARIABLES
corrplot(cor(df %>% select(journey_steps, X5, X27, X28)))

# lin regression features
# exclude intercept
top_linreg_feat <- top_linreg_feat[-1,] %>% select(term,estimate) %>% rename(Event = term, Influence = estimate)
feat_sum <- top_linreg_feat$Influence %>% sum()
top_linreg_feat <- top_linreg_feat %>% mutate(Influence = Influence/feat_sum)
write.csv(top_linreg_feat, file = "top_linreg_feat.csv")
