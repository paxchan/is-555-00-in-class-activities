library(tidyverse)

# Because `starwars` is a "hidden" dataset in memory for demonstration, it won't 
# show up in our environment at first, and it'll also be hard to reset it if we 
# make a mistake. So assign it to `df` to make sure you can work with it.
df <- starwars

# glimpse turns a data frame on its side for viewing. Super useful.
df %>%
  glimpse()



# iteratively add operations with the pipe operator: 
# height > 100, sex == female, 
# choose name, height, mass, species, films, 
# mass > 50, 
# arrange by mass
# note: columns can contain lists; more on that later
# note: filtering on some logical excludes NAs
df %>% 
  filter(height > 100,
         sex == 'female') %>% 
  select(name, height, mass, species) %>% 
  filter(mass > 55) %>% 
  arrange(desc(mass))

# calculate a new column,weight_lbs = mass * 2.204623
# Make sure it gets saved to the tibble...
df <- df %>% 
  mutate(weight_lbs = mass * 2.2046)


# group and summarize. Pay attention to NAs
# get a count and mean mass by species
df %>% 
  group_by(species) %>% 
  summarize(count = n(),
            mass_avg = mean(mass)) %>% 
  arrange(desc(count))

# Lots of NAs for average... why? Even for large groups it's NA...
df %>% 
  group_by(species) %>% 
  summarize(count = n(),
            mass_avg = mean(mass)) %>% 
  arrange(desc(count))

# Humans definitely have SOME mass data...
df %>% 
  filter(species == 'Human') %>% 
  select(name, species, mass)

# so let's exclude NAs from the mean aggregation:
df %>% 
  group_by(species) %>% 
  summarize(count = n(),
            mass_avg = mean(mass, na.rm = TRUE)) %>% 
  arrange(desc(count))


# I added this
df %>% 
  select(name, species, mass) %>% 
  group_by(species) %>% 
  slice_max(mass, n = 1)



# top 5 tallest people overall - without grouping this is simply finding the top five across all characters
df %>% 
  arrange(desc(height)) %>% 
  slice_head(n=5)

# Here's another way to accomplish the same thing (note that slice_max/slice_min allow ties)
df %>% 
  slice_max(height, n = 5)


# vs. shortest 2 of each species. 
# When doing these operations within the grouping, we get very different 
# results. You can play around a bit with the ordering of the piped commands 
# to make sure you get a feel for what is going on.
df %>% 
  select(name, species, height) %>%
  group_by(species) %>% 
  mutate(species_count = n(),
         species_max = max(height, na.rm = T)) %>% 
  mutate(difference_from_max = species_max - height) %>% 
  slice_min(height, n=2) %>% 
  arrange(species, height) %>% 
  print(n=20)


# Grouping by multiple categoricals
# Was is the average birth year for each gender from each homeworld
# This is a simple example, but it groups first by homeworld, 
# then within each homeworld it groups by gender
df %>% 
  group_by(homeworld,gender) %>% 
  summarize(ave_birth_year = mean(birth_year, na.rm = T))



# Grouping, then ungrouping, then grouping again:
# find average mass for each sex, but only for the tallest of each species.
# Again, a very contrived example, but understanding what's happening here will 
# be a good way for you to convince yourself that you've got a good handle on 
# the idea of grouping.
df %>% 
  group_by(species) %>% 
  slice_max(height) %>% 
  ungroup() %>% 
  group_by(sex) %>% 
  summarize(avg_mass = mean(mass, na.rm = T))




# Other useful things to play with ----------------------------------------------------------------------

# Sometimes it's useful to pull a set of values out into a vector for
# use later in some filtering logic. 
# Example: we want to do a bunch of checks or logic with just humans from Tatooine.
tatooine_humans <- starwars %>% 
  filter(homeworld == 'Tatooine',
         species == 'Human') %>%
  pull(name)
# I'll often do this when cleaning data, sometimes to denote a subset of the data that
# I want to check later or that needs cleaning, or that I want to piece together with
# another set of data down the line.

# Vectors like these are especially useful in filtering functions and can help you
# avoid a big chain of if_else() statements. Here's a few (silly) examples:

# filter to homeworlds that start with s (this is much simpler than a bunch of if_else()'s)
starwars %>% 
  filter(homeworld %in% c("Stewjon", "Socorro", "Sullust", "Serenno", "Skako", "Shili"))

# assign a binary flag for "is this character at risk for sunburn" under the basic assumption
# that humans on the desert planet will all eventually get sunburned.
starwars %>% 
  mutate(sunburn_risk = if_else(name %in% tatooine_humans, 1, 0))


