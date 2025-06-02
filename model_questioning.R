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
