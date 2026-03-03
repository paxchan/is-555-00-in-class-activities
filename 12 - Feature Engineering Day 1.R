library(tidyverse)

df <- read_csv('https://www.dropbox.com/s/petmujrpxa3qn3p/titanic.csv?dl=1') %>% 
  janitor::clean_names()

df %>% glimpse

# Data Dictionary:
# 
# passenger_id = passenger identifier
# survived     = did the passenger survive?
# pclass       = passenger class (1st, 2nd, 3rd)
# name         = passenger name
# sex          = passenger sex
# age          = passenger age
# sib_sp       = number of this passenger's siblings or spouses on board
# parch        = number of this passenger's parents or children on board
# ticket       = ticket number
# fare         = ticket cost/fare
# cabin        = cabin number
# embarked     = Port of embarkation (C=Cherbourg, Q=Queenstown, S=Southampton)


# Missingness -------------------------------------------------------------------------------------------
# Algorithms don't like missing values. It messes with the math.

# Get a feel for the missingness
df |> 
  summarise(across(everything(), ~sum(is.na(.x))))


# first check: is the missingness relevant?
# use summarize across
df |> 
  group_by(is.na(age)) |> 
  summarize(across(everything(), ~sum(is.na(.x))))

df |> 
  group_by(survived) |> 
  summarize(across(everything(), ~sum(is.na(.x))))
df |> count(survived)

# fill in missing age values, check our work
df <- df |> 
  mutate(age = if_else(is.na(age), mean(age, na.rm = T), age))

# now handle embarked, this time using replace_na()
# Again, check our work
df <- df |> 
  mutate(embarked = replace_na(embarked, '0'))

df |> count(embarked)

# What about cabin missingness? Random?
# use summarize across again.
# context: private cabins were assigned for some but not all.
df |> 
  group_by(is.na(cabin)) |> 
  summarize(across(everything(), ~mean(.x, na.rm = T)))


# Outlier Treatments ------------------------------------------------------------------------------------


# Pass the four columns to summary() to check means, maxes
 outlier_candidates <- c('age', 'sib_sp', 'parch', 'fare')

df |> 
  select(all_of(outlier_candidates)) |> 
  pivot_longer(
    everything(),
    names_to = 'feature',
    values_to = 'value'
  ) |> 
  ggplot(aes(x = feature, y = value, fill = feature)) +
  geom_boxplot() +
  facet_wrap(~feature, nrow = 1, scales = 'free') +
  ggthemes::theme_clean()

# calculate extreme threshold caps based on 99th percentile
age_cap <- quantile(df$age, .95, na.rm = T)
sib_sp_cap <- quantile(df$sib_sp, .95, na.rm = T)
parch_cap <- quantile(df$parch, .95, na.rm = T)
fare_cap <- quantile(df$fare, .95, na.rm = T)

# Now check how many are beyond the percentie caps
df |> 
  mutate(
    age = if_else(age > age_cap, age_cap, age),
    sib_sp = if_else(sib_sp > sib_sp_cap, sib_sp_cap, sib_sp),
    parch = if_else(parch > parch_cap, parch_cap, parch),
    fare = if_else(fare > fare_cap, fare_cap, fare)
  ) |> 
  select(all_of(outlier_candidates)) |> 
  pivot_longer(
    everything(),
    names_to = 'feature',
    values_to = 'value'
  ) |> 
  ggplot(aes(x = feature, y = value, fill = feature)) +
  geom_boxplot() +
  facet_wrap(~feature, nrow = 1, scales = 'free') +
  ggthemes::theme_clean()


# cap age and fare, and check work before saving


# save the result to df




