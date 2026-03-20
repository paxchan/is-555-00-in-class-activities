library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')


# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate
clean <- raw |> 
  janitor::clean_names()

clean |> 
  select(star_rating)

clean |> 
  count(star_rating) |> 
  print(n=40)

clean |> 
  mutate(star_rating = parse_number(star_rating, na = c('Product rating is not available'))) |> 
  mutate(price = parse_number(price)) |> 
  select(unit_size) |> 
  mutate(unit_type_each = if_else(str_detect(unit_size, 'each'), 1, 0))

clean |> 
  select(unit_size) |> 
  filter(str_detect(unit_size, 'Unit Size not available'))

clean |> 
  mutate(star_rating = parse_number(star_rating, na = c('Product rating is not available'))) |> 
  mutate(price = parse_number(price)) |> 
  select(unit_size) |> 
  mutate(unit_size = na_if(unit_size, 'Unit Size not available')) |> 
  # slice(1:10) |> 
  separate_wider_delim(unit_size, delim = ' ', names = c('unit_num', 'unit_type'), cols_remove = FALSE) 

  # mutate(star_rating_c = na_if(star_rating, 'Product rating is not available')) |> 
  # filter(star_rating == 'Product rating is not available')
  # mutate(star_rating_c = replace_na(star_rating_c, 0))
clean |> 
  select(product_info)
replace_na()
na_if()


clean |> 
  select(date_added, first_sold_day) |> 
  mutate(date_added_c = dmy(date_added)) |> 
  mutate(display = year(date_added_c)) |> 
  mutate(first_sold_day_c = mdy(first_sold_day))



# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.


