library(tidyverse)

# Here's a loan dataset (defined with the ever-handy tribble() function)
loan_data <- tribble(
  ~applicant_id, ~stable_job,  ~outstanding_debt, ~loan_status,
   1,             0,            0,                 'WriteOff',
   2,             1,            0,                 'Paid',
   3,             1,            1,                 'WriteOff',
   4,             0,            1,                 'Paid',
   5,             0,            0,                 'WriteOff',
   6,             1,            0,                 'Paid',
   7,             0,            1,                 'WriteOff',
   8,             1,            0,                 'Paid',
   9,             0,            1,                 'WriteOff',
   10,            1,            1,                 'WriteOff'
)


# This function calculates the "loss" for our model training, defined in this
# case as the percent of predictions that are incorrect.
calculate_loss <- function(predictions) {
  total_cases <- nrow(predictions)
  incorrect_predictions <- sum(predictions$predicted_loan_status != predictions$loan_status)
  loss <- incorrect_predictions / total_cases
  
  print(paste0('Loss: ', round(loss*100,2),'% incorrect predictions.'))
}


loan_data %>% 
  arrange(loan_status)

# This is a simple function that uses a case_when() to apply case logic to decide
# whether a given loan should be approved. You can think of the "rules" inside
# of the case_when() function as the "model" we're training.
apply_rules <- function(data) {
  rules_applied <- data %>%
    mutate(predicted_loan_status = case_when(
      stable_job == 1 ~ 'Paid',  # First rule: approve if they have a stable job
      .default = 'WriteOff'          # the .default parameter is the "else" in a case_when()
    ))
  return(rules_applied)
}



# Now apply your "model" and calculate the loss (which starts at 30%)
loan_predictions <- apply_rules(loan_data)
calculate_loss(loan_predictions)


loan_predictions







# What about next week's loans, though?

new_applications <- tribble(
  ~applicant_id,  ~stable_job,  ~outstanding_debt, ~loan_status,
  101,             1,            0,                 'Paid',
  102,             0,            0,                 'WriteOff',
  103,             0,            1,                 'Paid',
  104,             0,            0,                 'WriteOff',
  105,             1,            1,                 'WriteOff'
)
new_predictions <- apply_rules(new_applications)
calculate_loss(new_predictions)
