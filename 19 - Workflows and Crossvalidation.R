library(tidymodels)
# install.packages('ranger')

data(ames)
housing <- ames %>% janitor::clean_names()
housing %>% glimpse

set.seed(42)
housing_split <- initial_split(housing, strata = sale_price)

housing_training <- housing_split %>% training()
housing_testing <- housing_split %>% testing()

# Build feature engineering pipeline
housing_rec <- recipe(sale_price ~ .,
                      data = housing_training) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.85) %>% 
  step_nzv(all_predictors()) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_dummy(all_nominal_predictors())

housing_rf_model <- rand_forest() %>% 
  set_engine('ranger') %>% 
  set_mode('regression')

# show_engines('rand_forest')

# create a reusable workflow

housing_wkfl <- workflow() %>% 
  add_model(housing_rf_model) %>% 
  add_recipe(housing_rec)


# First, just do an overall shortcut to fit/evaluate

single_result <- housing_wkfl %>% 
  last_fit(split = housing_split)

single_result %>% collect_metrics()
single_result %>% collect_predictions()

# Now some k-fold validation
set.seed(42)
housing_folds <- vfold_cv(housing_training, v = 10, strata = sale_price)


cv_results <- housing_wkfl %>% 
  fit_resamples(resamples = housing_folds)


# Use collect_metrics with and without summarization
cv_results %>% collect_metrics()
cv_results %>% collect_metrics(summarize = F)





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







