# load necessary packages
library(tidyverse)
library(lubridate)
library(readxl)
# read in data
bikes <- read_excel(path = "00_data/01_bike_sales/01_raw_data/bikes.xlsx")
bikeshops <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
orderlines <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")

# Wrangle data
bike_orderlines_wrangled_tbl <- orders %>%
  # Join orders with bikes dataset using product.id
  inner_join(bikes, by = "product.id") %>%
  # Add total column
  mutate(total = price * quantity) %>%
  # Join with bikeshops dataset using bikeshop.id
  inner_join(bikeshops, by = "bikeshop.id") %>%
  # Extract year from order.date
  mutate(year = year(order.date),
         month = month(order.date),
         day = day(order.date),
         state = str_split(location, ", ")[[1]][2],
         city = str_split(location, ", ")[[1]][1])

# Plot sales by location (state)
bike_orderlines_wrangled_tbl %>%
  group_by(state) %>%
  summarise(total_sales = sum(total)) %>%
  ggplot(aes(x = state, y = total_sales)) +
  geom_col(fill = "dodgerblue") +
  labs(title = "Total Sales by State",
       x = "State",
       y = "Total Sales ($)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))