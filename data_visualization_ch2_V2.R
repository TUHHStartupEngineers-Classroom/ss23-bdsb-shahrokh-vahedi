# Data Visualization
## Challenge 2

# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(maps)       # For map data and plotting geographic maps
library(mapdata)    # Additional map datasets for use with maps package
library(ggplot2)    # For creating visually appealing and customizable graphics
library(scales)     # Customizing scale labels in plots

# Read the COVID-19 data from the provided URL
world_cov_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Select only the relevant columns: date, location, population, total_cases, total_deaths
world_cov_data <- world_cov_data %>% 
  select(date, location, population, total_cases, total_deaths) %>% 
  filter(!is.na(total_cases), !is.na(total_deaths))

# Calculate mortality rate by dividing the total deaths by the population
mort_rate <- world_cov_data$total_deaths / world_cov_data$population
world_cov_data <- world_cov_data %>% mutate(mortality_rate = mort_rate)

# Select the most recent data for each location
recent_data <- world_cov_data %>% 
  group_by(location) %>% 
  slice_tail(n = 1)

# Align country names to match the names used in the map data
recent_data <- recent_data %>% 
  mutate(location = case_when(
    location == "United Kingdom" ~ "UK",
    location == "United States" ~ "USA",
    location == "Democratic Republic of Congo" ~ "Democratic Republic of the Congo",
    TRUE ~ location
  )) %>% 
  distinct()

# Merge the data with map data
world_mp_data <- map_data("world")
world_map_data <- right_join(world_mp_data, recent_data, by = c("region" = "location"))

# Plotting the map
plot_data <- ggplot() +
  geom_map(data = world_map_data, map = world_map_data, aes(map_id = region, fill = mortality_rate),
           color = "white", size = 0.1) +
  expand_limits(x = world_mp_data$long, y = world_mp_data$lat) +
  scale_fill_gradient(low = "red", high = "black", name = "Mortality Rate",
                      labels = scales::percent_format()) +
  labs(title = "Confirmed COVID-19 deaths relative to the size of the population",
       subtitle = "Around 6.9 Million confirmed COVID-19 deaths worldwide (May 2023)") +
  theme_void() +
  theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 9),
        legend.position = "right",
        legend.title.align = 0.5,
        legend.text = element_text(size = 7),
        legend.title = element_text(size = 9),
        panel.background = element_rect(fill = "transparent", color = "white"))

print(plot_data)
