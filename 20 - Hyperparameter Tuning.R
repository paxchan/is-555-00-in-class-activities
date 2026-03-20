library(tidyverse)
library(tidymodels)


# Setup w/ Credit Data --------------------------------------------------------

cr_data <- read_csv('https://www.dropbox.com/scl/fi/vykejw5ud9ejjvcc442gd/credit_small.csv?rlkey=zuyurxikxickgdjchh6681j91&dl=1') %>% 
  mutate(status = as.factor(status))

cr_data %>% glimpse()

# Model setup:
set.seed(42)
cr_split <- initial_split(cr_data, strata = status)
cr_training <- cr_split %>% training()
cr_testing <- cr_split %>% testing()

# Let's create a recipe that:
#    - imputes missing numeric values
#    - log transforms assets, debt, income, price, expenses
#    - normalizes all numeric predictors
#    - dummy codes all categories
cr_rec <- recipe(status ~ ., 
                 data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = 1)

cr_training %>% count(status)
cr_rec %>% prep() %>% juice() %>% count(status)

# Now let's setup a model spec (rpart decision tree), workflow, and do a cross validation.

dt_spec <- decision_tree() %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_wkfl <- workflow() %>% 
  add_model(dt_spec) %>% 
  add_recipe(cr_rec)

set.seed(42)
cr_folds <- vfold_cv(cr_training, strata = status)

cv_fit <- cr_wkfl %>% 
  fit_resamples(resamples = cr_folds)

cv_fit %>% collect_metrics(summarize = F)

# Next, a tunable model specification, recipe, and workflow:

dt_spec_tune <- decision_tree(
  cost_complexity = tune(),
  min_n = tune()
) %>% 
  set_engine('rpart') %>% 
  set_mode('classification')

cr_rec_tune <- recipe(status ~ ., 
                      data = cr_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_log(assets, debt, income, price, expenses, offset = 1) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  themis::step_downsample(status, under_ratio = tune())

cr_wkfl_tune <- workflow() %>% 
  add_model(dt_spec_tune) %>% 
  add_recipe(cr_rec_tune)

# Extract parameters, create a grid to search, then search:

cr_grid <- grid_random(extract_parameter_set_dials(cr_wkfl_tune), size = 50)

doParallel::registerDoParallel(cores = 7)

grid_search_result <- cr_wkfl_tune %>% 
  tune_grid(resamples = cr_folds,
            grid = cr_grid)


# Look at the results, selecting the best one and using those to finalize.
grid_search_result %>% collect_metrics()
grid_search_result %>% collect_metrics(summarize = F)

grid_search_result %>% show_best(metric = 'roc_auc')

winners <- grid_search_result %>% select_best(metric = 'roc_auc')

finalized_wkfl <- cr_wkfl_tune %>% 
  finalize_workflow(winners)

all_done <- finalized_wkfl %>% 
  last_fit(split = cr_split)

all_done %>% collect_metrics()






# Other Datasets ----------------------------------------------------------




cars <- read_csv('https://www.dropbox.com/scl/fi/xavej23qpauvx3xfdq7zh/car_sales.csv?rlkey=4mfp6tpia0uqkcoiqf9jleau3&dl=1')

cars %>% glimpse

set.seed(42)
cars_split <- initial_split(cars, strata = sellingprice_log)
cars_training <- cars_split %>% training()
cars_testing <- cars_split %>% testing()

# Let's briefly revisit this recipe and the various steps:
cars_rec <- recipe(sellingprice_log ~.,
                   data = cars_training) %>% 
  step_impute_median(all_numeric_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_novel(make, model) %>%              # <--- These are new. :)
  step_unknown(make, model) %>%            # <--- These are new. :) 
  step_other(make, threshold = 0.03) %>%   # <--- These are new. :)  
  step_other(model, threshold = 0.01) %>%  # <--- These are new. :) 
  step_dummy(all_nominal_predictors())

# cars_rec %>% prep() %>% juice() %>% glimpse()



# Now we'll make a workflow:
xgb_spec <- boost_tree() %>% 
  set_mode('regression')

cars_wkfl <- workflow() %>% 
  add_model(xgb_spec) %>% 
  add_recipe(cars_rec)


# And we'll use dedicated functions to get a crossvalidation object and
# perform the fit










# Final Example - Zillions of Features ------------------------------------


claims_raw <- read_csv('https://www.dropbox.com/scl/fi/yak22stqfsq3aaz4qvxn1/claims.csv?rlkey=hj42vra7wpi6odnqvmrgxb797&dl=1')

# A handful of numeric features:
claims_raw %>% 
  select(where(is.numeric)) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(x = value, fill = name)) +
  geom_density(alpha = .4) +
  facet_wrap(~name, scales = 'free')

# But over 100 categoricals:
claims_raw %>% 
  select(where(is.character)) %>% glimpse()

# Note the highly skewed outcome (claim loss amount):
claims_raw %>%
  ggplot(aes(x = loss)) +
  geom_histogram(alpha = .4, fill = 'blue') +
  theme_bw() +
  labs(title = 'Distribution of Claim Loss Amount (Outcome)')

# It's recommended to handle any transformations of the outcome BEFORE entering
# the model flow. (In case you're curious, here's a brief explanation: 
# https://stackoverflow.com/questions/75762005/error-in-step-log-when-trying-to-make-predictions-with-my-model)
claims <- claims_raw %>% 
  mutate(log_loss = log(loss)) %>% 
  select(-loss)

# Model setup
set.seed(42)
claims_split <- initial_split(claims, strata = log_loss)

claims_training <- claims_split %>% training()
claims_testing <- claims_split %>% testing()

# Create a recipe that applies best-practice pre-processing operations:
claims_rec <- recipe(log_loss ~.,
                     data = claims_training) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_nzv(all_predictors())

claims_rec %>% prep() %>% juice() %>% glimpse()


# And setup and train a model:
linreg_spec <- linear_reg()

claims_wkfl <- workflow() %>% 
  add_recipe(claims_rec) %>% 
  add_model(linreg_spec)


#Lastly, cross-validation!




