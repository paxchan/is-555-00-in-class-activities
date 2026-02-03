# FACETS
#   Facets allow you to use categories within your data to line up similar
#   charts next to each other, which can be a really useful way to highlight
#   differences between groups.
# 
# The two main facet functions are:
#   facet_wrap() <- usually best used to show a series of plots across a 
#                  single category variable, with you specify with `~varable`.
#                  This will show one plot per level of the category.
#   facet_grid() <- My favorite to show a "grid" of facets, with rows and 
#                  columns defined by two different variables. This is specified
#                  using the facet formula `var1~var2`
#       Note: Pay attention to the `scales` parameter in these facet functions, 
#             where you can specify whether you want to hold the x and/or y
#             axis scales constant across all of the facets.


# Facets and multi-lines --------------------------------------------------------------------------------
library(tidyverse)

tips <- read_csv('https://www.dropbox.com/s/rydxlxdarjdoj7a/tips.csv?dl=1')

# Let's plot tip_percentage vs. total_bill,
# then split that across lots of categories
tips |> 
  ggplot(aes(x = tip, y = total_bill, color = time)) +
  geom_point(size = 2) +
  # ggthemes::theme_clean() +
  facet_wrap(size~smoker, ncol = 2)

tips |> 
  ggplot(aes(x = tip, y = total_bill, color = time)) +
  geom_point(size = 1) +
  ggthemes::theme_clean() +
  facet_grid(size~smoker)

tips |> 
  count(time)

tips |> 
  mutate(tip_c = if_else(smoker == 'Yes', tip*100, tip)) |> 
  ggplot(aes(x = tip_c, y = total_bill, color = time)) +
  geom_point(size = 3) + 
  facet_wrap(~smoker, scales = 'free_x')

tips |> 
  mutate(tip_c = if_else(smoker == 'Yes', tip*100, tip)) |> 
  ggplot(aes(x = tip_c, fill = time)) +
  geom_histogram(position = 'dodge')


econ <- read_csv('https://www.dropbox.com/s/8bq9rw0rk46hru2/econ.csv?dl=1')

# Let's plot two measures over time: savings rate & unemployment weeks
# It's easiest if we pivot to make this work
econ |> 
  # filter(date > ymd('2011-01-01')) |> 
  select(date, savings_rate, unempl_weeks) |> 
  pivot_longer(
    cols = !date,
    names_to = 'measure',
    values_to = 'value'
  ) |> 
  ggplot(aes(x = date, y = value, color = measure)) + 
  geom_line()




