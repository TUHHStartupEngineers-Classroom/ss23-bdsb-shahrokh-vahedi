# Data Visualization

# Challenge 1

# Load required libraries
library(tidyverse)   # For data manipulation and visualization
library(ggrepel)    # For label repulsion in plots
library(scales)      # For customizing scale labels
library(lubridate)   # For working with dates
Sys.setlocale("LC_TIME", "English")  # Set English locale for date formatting

# Read the COVID-19 data from the provided URL
world_cov_data <- read_csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")

# Select only the relevant columns: date, continent, location, and total_cases
world_cov_data <- world_cov_data %>% 
  select(date, continent, location, total_cases) %>% 
  filter(!is.na(total_cases))

# Convert the date column to Date format
world_cov_data$date <- as.Date(world_cov_data$date)

# Select data for chosen countries
chosen_count <- c("Europe", "France", "Germany", "Spain", "United Kingdom", "United States")
chosen_data <- world_cov_data %>% filter(location %in% chosen_count)

# Create a vector of distinct colors
line_colors <- c("#FF0000", "#00FF00", "#0000FF", "#FF00FF", "#FFFF00", "#00FFFF")

# Create the plot
ggplot(chosen_data, aes(x = date, y = total_cases, color = location, group = location)) +
  geom_line(size = 1, show.legend = FALSE) +
  geom_label_repel(data = chosen_data %>% filter(date == max(date)), aes(label = location, x = date, y = total_cases),
                   size = 3, box.padding = unit(0.35, "lines"), point.padding = unit(0.5, "lines"), show.legend = FALSE) +
  labs(title = "Covid-19 Confirmed Cases Worldwide", subtitle = "Data as of 19/04/2022", y = "Cumulative Cases", x = "Continent/Country") +
  scale_y_continuous(labels = function(x) format(x/1e6, big.mark = " ", scientific = FALSE) %>% paste0(" ", "M")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B '%y") +
  scale_color_manual(values = line_colors) +  # Set the color palette for the lines
  theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  guides(color = guide_legend(title = "Country"), shape = guide_legend(title = "")) +
  theme(legend.title = element_text(size = 12), legend.text = element_text(size = 10))
