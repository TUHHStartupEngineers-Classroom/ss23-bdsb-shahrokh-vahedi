##################################################################

library(httr)
library(jsonlite)

# Set your OpenWeatherMap API key as an environment variable
Sys.setenv(OPENWEATHERMAP_API_KEY = "7471c3bd36b596b3b489afb5e04d2a61")

# Set the location for which you want to get the weather forecast
city <- 'London'

# Create the API endpoint URL
endpoint <- paste0('http://api.openweathermap.org/data/2.5/weather?q=', city, '&appid=', Sys.getenv("OPENWEATHERMAP_API_KEY"))

# Make the API request
response <- GET(endpoint)
print(status_code(response))
# Check if the request was successful
if (status_code(response) == 200) {
  # Extract the raw response content
  raw_content <- content(response, as = "raw")
  print(raw_content)
  # Convert raw content to character
  char_content <- rawToChar(raw_content)
  
  # Parse the JSON data
  data <- fromJSON(char_content)
  
  # Extract the relevant information from the data
  temperature <- data$main$temp
  humidity <- data$main$humidity
  description <- data$weather[1]$description
  
  # Convert the description to a readable format
  description <- tolower(description)
  
  # Print the weather forecast
  cat("City:", city, "\n")
  cat("Temperature:", temperature, "K\n")
  cat("Humidity:", humidity, "%\n")
  cat("Description:", description, "\n")
} else {
  # Request was not successful
  print("Request failed.")
}





###################################################################################