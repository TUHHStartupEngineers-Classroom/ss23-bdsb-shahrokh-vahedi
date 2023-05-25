# Data Visualization 

# Challenge 1

library(tidyverse)
library(ggrepel)
library(scales)
library(lubridate)
Sys.setlocale("LC_TIME", "English")

world_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Select only the relevant columns
world_data <- world_data %>% 
  select(date, continent, location, total_cases) %>% 
  filter(!is.na(total_cases))

# Convert date column to Date format
world_data$date <- as.Date(world_data$date)

# Select data for desired countries
desired_countries <- c("Europe", "France", "Germany", "Spain", "United Kingdom", "United States")
selected_data <- world_data %>% filter(location %in% desired_countries)

# Create the plot
ggplot(selected_data, aes(x = date, y = total_cases, color = location , group = location)) +
  geom_line(size = 1) +
  geom_label_repel(data = selected_data %>% filter(date == max(date)), aes(label = location, x = date, y = total_cases), size = 3, box.padding = unit(0.35, "lines"), point.padding = unit(0.5, "lines"), show.legend = FALSE) +
  labs(title = "Covid-19 Confirmed Cases Worldwide", subtitle = "As of 19/04/2022", y = "Cumulative Cases", x = "Continent/Country") +
  scale_y_continuous(labels = scales::comma_format()) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B '%y") +
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  guides(color = guide_legend(title = " "), shape = guide_legend(title = "Country", override.aes = list(size = 6))) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))

