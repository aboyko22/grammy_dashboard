# load packages ----
library(tidyverse)
library(tidymodels)
library(here)
library(doMC)

# handle common conflicts
tidymodels_prefer()

# parallel processing
registerDoMC(cores = 6)

# load data ----
data <- read_csv("data/modeling_data.csv")

data <- data %>%
  select(-c(artist, song)) %>%
  mutate(explicit = factor(explicit),
         charted_q1 = factor(charted_q1),
         charted_q2 = factor(charted_q2),
         charted_q3 = factor(charted_q3),
         charted_q4 = factor(charted_q4))

# folding data ----
data_folds <- data %>%
  vfold_cv(v = 10, repeats = 5, strata = won_grammy)

# recipe building ----
standard_recipe <- recipe(won_grammy ~., data) %>%
  step_zv(all_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_normalize(all_predictors())

# model specifications ----
boosted_spec <- boost_tree(mtry = tune(), min_n = tune(), learn_rate = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# define workflow ----
boosted_wflow <- workflow() %>%
  add_recipe(standard_recipe) %>%
  add_model(boosted_spec)

# hyperparameter tuning values ----
hardhat::extract_parameter_set_dials(boosted_spec)

boosted_params <- parameters(boosted_spec) %>% 
  update(mtry = mtry(c(1, 8)),
         learn_rate = learn_rate(c(-5, -0.2))) 

boosted_grid <- grid_regular(boosted_params, levels = 5)

# fit model ----
set.seed(6103)

boosted_tuned <- boosted_wflow %>%
  tune_grid(
    data_folds,
    grid = boosted_grid,
    control = control_grid(save_workflow = TRUE, save_pred = TRUE))

# check results ----
best_params <- select_best(boosted_tuned, metric = "accuracy")

best_predictions <- boosted_tuned %>%
  collect_predictions(parameters = best_params)

best_predictions %>%
  count(.pred_class, won_grammy)

# re do model checks ----
# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(xgboost)
library(lubridate)
library(vip)

# Read data
modeling_data <- read_csv("data/modeling_data.csv")
test_data <- read_csv("data/testing_data.csv")

# Boolean conversion
cols <- c('charted_q1', 'charted_q2', 'charted_q3', 'charted_q4', 'explicit')

modeling_data <- modeling_data %>%
  mutate(across(all_of(cols), ~ as.integer(.x)))

test_data <- test_data %>%
  mutate(across(all_of(cols), ~ as.integer(.x)))

# Composite column (not used in training)
modeling_data <- modeling_data %>%
  mutate(`song+artist` = paste(song, artist, sep = ""))

# Response variable
modeling_data <- modeling_data %>%
  mutate(won_grammy = if_else(won_grammy == "Yes", 1, 0))

# Remove unneeded columns
X <- modeling_data %>%
  select(-won_grammy, -artist, -song, -started_charting, -`song+artist`)

y <- modeling_data$won_grammy

# Convert categorical features
X <- X %>%
  mutate(across(where(is.character), as.factor))

test_data <- test_data %>%
  mutate(across(where(is.character), as.factor))

# Prepare test set
X_test <- test_data %>%
  filter(ymd(release_data) > ymd("2023-09-01")) %>%
  select(-artist, -song, -started_charting, -spotify_id, -release_data)

indexer <- test_data %>%
  filter(ymd(release_data) > ymd("2023-09-01")) %>%
  select(song, artist)

# Combine training data for recipe
train_data <- bind_cols(X, won_grammy = y) %>%
  mutate(won_grammy = factor(won_grammy))

# Recipe
rec <- recipe(won_grammy ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())

# Model spec
xgb_spec <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  stop_iter = 10
) %>%
  set_engine("xgboost", scale_pos_weight = 5149/61) %>%
  set_mode("classification")

# Workflow
wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(rec)

# Tuning
set.seed(40)
folds <- vfold_cv(train_data, v = 5)

library(finetune)

# First, count predictors AFTER preprocessing
rec_prep <- prep(rec)
num_predictors <- ncol(bake(rec_prep, new_data = NULL))

grid <- grid_latin_hypercube(
  trees(range = c(200L, 500L)),
  learn_rate(range = c(0.01, 0.5)),
  tree_depth(range = c(3L, 12L)),
  min_n(range = c(1L, 7L)),
  loss_reduction(range = c(0, 0.2)),
  sample_size = sample_prop(range = c(0.5, 1)),
  mtry(range = c(1L, num_predictors)),
  size = 75
)

res <- tune_grid(
  wf,
  resamples = folds,
  grid = grid,
  metrics = metric_set(precision)
)

best_params <- select_best(res, metric = "precision")

# Final model
final_model <- finalize_workflow(wf, best_params) %>%
  fit(data = train_data)

# Predictions
train_probs <- predict(final_model, train_data, type = "prob")
test_probs <- predict(final_model, X_test, type = "prob")

# Display top predictions
head(bind_cols(indexer, Grammy_Prob = test_probs$.pred_1) %>%
       arrange(desc(Grammy_Prob)), 15)

# Metrics
threshold <- 0.8
train_pred_binary <- if_else(train_probs$.pred_1 >= threshold, 1, 0)

metrics_data <- tibble(
  truth = factor(y),
  estimate = factor(train_pred_binary),
  .pred = train_probs$.pred_1
)

metrics(metrics_data, truth, estimate)

conf_mat(metrics_data, truth, estimate) %>%
  autoplot(type = "heatmap")

# Feature importance
final_model %>%
  extract_fit_parsnip() %>%
  vip(geom = "col", num_features = 25)

# downsampled attempt ----
# Load necessary libraries
library(tidyverse)
library(tidymodels)
library(xgboost)
library(lubridate)
library(vip)
library(finetune)

# Read data
modeling_data <- read_csv("data/modeling_data.csv")
test_data <- read_csv("data/testing_data.csv")

# Boolean conversion
cols <- c('charted_q1', 'charted_q2', 'charted_q3', 'charted_q4', 'explicit')

modeling_data <- modeling_data %>%
  mutate(across(all_of(cols), ~ as.integer(.x)))

test_data <- test_data %>%
  mutate(across(all_of(cols), ~ as.integer(.x)))

# Composite column (not used in training)
modeling_data <- modeling_data %>%
  mutate(`song+artist` = paste(song, artist, sep = ""))

# Response variable
modeling_data <- modeling_data %>%
  mutate(won_grammy = if_else(won_grammy == "Yes", 1, 0))

# Remove unneeded columns
X <- modeling_data %>%
  select(-won_grammy, -artist, -song, -started_charting, -`song+artist`)

y <- modeling_data$won_grammy

# Convert categorical features
X <- X %>%
  mutate(across(where(is.character), as.factor))

test_data <- test_data %>%
  mutate(across(where(is.character), as.factor))

# Prepare test set
X_test <- test_data %>%
  filter(ymd(release_data) > ymd("2023-09-01")) %>%
  select(-artist, -song, -started_charting, -spotify_id, -release_data)

indexer <- test_data %>%
  filter(ymd(release_data) > ymd("2023-09-01")) %>%
  select(song, artist)

# Combine training data and compute case weights
minority_weight <- sum(y == 0) / sum(y == 1)
train_data <- bind_cols(X, won_grammy = y) %>%
  mutate(
    won_grammy = factor(won_grammy),
    weight = if_else(won_grammy == 1, minority_weight, 1)
  )

# Recipe
rec <- recipe(won_grammy ~ ., data = train_data) %>%
  update_role(weight, new_role = "case_weight") %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_zv(all_predictors())

# Model spec (no scale_pos_weight)
xgb_spec <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune(),
  mtry = tune(),
  stop_iter = 10
) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Workflow
wf <- workflow() %>%
  add_model(xgb_spec) %>%
  add_recipe(rec)

# Tuning
set.seed(40)
folds <- vfold_cv(train_data, v = 5, strata = won_grammy)

# Count predictors after prep
rec_prep <- prep(rec)
num_predictors <- ncol(bake(rec_prep, new_data = NULL))

grid <- grid_latin_hypercube(
  trees(range = c(200L, 500L)),
  learn_rate(range = c(0.01, 0.5)),
  tree_depth(range = c(3L, 12L)),
  min_n(range = c(1L, 7L)),
  loss_reduction(range = c(0, 0.2)),
  sample_size = sample_prop(range = c(0.5, 1)),
  mtry(range = c(1L, num_predictors)),
  size = 75
)

res <- tune_grid(
  wf,
  resamples = folds,
  grid = grid,
  metrics = metric_set(precision)
)

best_params <- select_best(res, metric = "precision")

# Final model
final_model <- finalize_workflow(wf, best_params) %>%
  fit(data = train_data)

# Predictions
train_data_pred <- train_data %>%
  mutate(weight = 1)

X_test_pred <- X_test %>%
  mutate(weight = 1) 

train_probs <- predict(final_model, train_data_pred, type = "prob")
test_probs <- predict(final_model, X_test_pred, type = "prob")

# Display top predictions
head(bind_cols(indexer, Grammy_Prob = test_probs$.pred_1) %>%
       arrange(desc(Grammy_Prob)), 15)

# Metrics
threshold <- 0.8
train_pred_binary <- if_else(train_probs$.pred_1 >= threshold, 1, 0)

metrics_data <- tibble(
  truth = train_data$won_grammy,
  estimate = factor(train_pred_binary),
  .pred = train_probs$.pred_1
)

metrics(metrics_data, truth, estimate)

conf_mat(metrics_data, truth, estimate) %>%
  autoplot(type = "heatmap")

# Feature importance
final_model %>%
  extract_fit_parsnip() %>%
  vip(geom = "col", num_features = 25)

