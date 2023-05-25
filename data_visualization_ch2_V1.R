# Data Visualization
## Chalenge 2


# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(maps)       # For map data and plotting geographic maps
library(mapdata)    # Additional map datasets for use with maps package
library(ggplot2)    # For creating visually appealing and customizable graphics
library(scales)     # Customizing scale labels in plots


# Read the COVID-19 data from the provided URL
covid_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Select only the relevant columns:  date, location, population, total_cases, total_deaths
covid_data <- covid_data %>%
  select(date, location, population, total_cases, total_deaths) %>%
  filter(!is.na(total_cases), !is.na(total_deaths))

# calculating mortality rate by dividing the total death to the population
covid_data$mortality_rate <- covid_data$total_deaths / covid_data$population

# Select the most recent data for each location
latest_data <- covid_data %>%
  group_by(location) %>%
  slice_tail(n = 1)

# Align country names to match the names used in the map data
latest_data <- latest_data %>%
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>%
  distinct()

# Merge the data with map data
world <- map_data("world")
map_data <- right_join(world, latest_data, by = c("region" = "location"))

# plotting the map
plot_data <- ggplot() +
  geom_map(data = map_data, map = map_data, aes(map_id = region, fill = mortality_rate),
           color = "white", size = 0.1) +
  expand_limits(x = world$long, y = world$lat) +
  scale_fill_gradient(low = "red", high = "black", name = "Mortality Rate",
                      labels = scales::percent_format()) +
  labs(title = "Confirmed COVID-19 deaths relative to the size of the population",
       subtitle = "Around 6.9 Million confirmed COVID-19 deaths worldwide") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        legend.position = "right",
        legend.title.align = 0.5,
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        panel.background = element_rect(fill = "transparent", color = "white"))

print(plot_data)
