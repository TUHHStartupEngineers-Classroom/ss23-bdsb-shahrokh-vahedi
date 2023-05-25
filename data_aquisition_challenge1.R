# Load required libraries
library(httr)
library(jsonlite)

# Define API endpoint and parameters
api_url <- "https://api.gbif.org/v1/occurrence/search"
params <- list(
  taxonKey = 2435098, # Taxon key for the species (e.g., 2435098 for "Apis mellifera" - the honeybee)
  limit = 300         # Number of records to fetch (max 300)
)

# Send GET request to the API
response <- GET(api_url, query = params)



# Check the HTTP status code
status_code <- status_code(response)
if (status_code == 200) {
  # Successful response
  print("Request successful!")
  # Parse JSON response
  json_data <- content(response, as = "text", encoding = "UTF-8")
  data <- fromJSON(json_data, flatten = TRUE)
  
  # Convert the results to a dataframe
  occurrences_df <- as.data.frame(data$results)
  
  
} else {
  # Unsuccessful response
  print(paste("Request failed with status code:", status_code))
}



# Load required libraries
library(ggplot2)
library(ggmap)
library(maps)

# Remove rows with missing latitude or longitude
occurrences_df <- occurrences_df[!is.na(occurrences_df$decimalLatitude) & !is.na(occurrences_df$decimalLongitude),]

# Get the world map data
world_map <- map_data("world")

# Create a ggplot object with the world map
map_plot <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "gray80", color = "gray50") +
  theme_minimal()

# Add the scatter plot of latitude and longitude on top of the map
map_plot + geom_point(data = occurrences_df, aes(x = decimalLongitude, y = decimalLatitude), color = "blue", alpha = 0.5) +
  labs(title = "Bio-diversity of Honey Bees", x = "Longitude", y = "Latitude")
