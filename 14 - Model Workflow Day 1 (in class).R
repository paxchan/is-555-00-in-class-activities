library(tidyverse)
library(tidymodels)

# Okay let's work through the (cleaned) titanic data from last week:
titanic_raw <- read_csv('https://www.dropbox.com/s/92funarubgk5rzh/titanic_clean.csv?dl=1')


# First let's make sure factors are factors
titanic <- titanic_raw |> 
  mutate(survived = as_factor(survived),
         had_cabin = as_factor(had_cabin),
         pclass = as_factor(pclass))



# Now let's do a train/test split
leo_split <- initial_split(
  data = titanic,
  prop = .75,
  strata = survived
)
leo_split

leo_training <- leo_split |> training()
leo_testing <- leo_split |> testing()

# Plan the model setup, including the engine and mode
leo_spec <- logistic_reg() |> 
  set_engine('glm') |> 
  set_mode('classification')

show_engines('logistic_reg')

# relevant model types: logistic_reg(), linear_reg(), decision_tree(), rand_forest(), boost_tree()
# show_engines('logistic_reg')




# Now fit a model, look at output with tidy()
leo_fit <- leo_spec |> 
  fit(survived ~ .,
      data = leo_training)

leo_fit

# Calculate predictions, 
# including class predictions _and_ probabilities





  

# Now let's build a confusion matrix and explore a few of the related metrics.
# conf_mat(), sens()






# Let's get fancy:
# roc_curve(), roc_auc(), autoplot()
roc_data <- leo_results %>% 
  roc_curve(truth = survived, .pred_0)

roc_data %>% 
  autoplot()

leo_results %>% 
  roc_auc(truth = survived, .pred_0)

# Finalize the model with last_fit()




# finalized object, extract predictions, metrics 
# with dedicated collect_* functions:





