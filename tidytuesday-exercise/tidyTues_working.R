# Install devtools if you haven't already
install.packages("devtools")

# Use devtools to install rnaturalearthhires from GitHub
devtools::install_github("ropensci/rnaturalearthhires", force = TRUE)


library(devtools)
library(tidyverse)
library(jsonlite)
library(janitor)
library(here)
library(fs)
library(sf)
library(ggplot2)
library(dplyr)
library(ggspatial)
library(ggmap)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(httr)
library(geonames)

tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

ea_2023 <- tuesdata$eclipse_annular_2023
et_2024 <- tuesdata$eclipse_total_2024
ea_part_2023 <- tuesdata$eclipse_partial_2023
ea_part_2024 <- tuesdata$eclipse_partial_2024


wd = here::here("tidytuesday-exercise", "data")

# Specify the URL for the JSON data
json_url <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2023.json"

# Perform the HTTP GET request to fetch the JSON data
response <- GET(json_url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON content
  data <- content(response, "text", encoding = "UTF-8")
  eclipse_data <- fromJSON(data) # This should work now with jsonlite loaded
  
  # Print the data to see its structure
  print(eclipse_data)
} else {
  cat("Failed to download the data. Status code:", status_code(response), "\n")
}
# URLs for the JSON data
eclipse_cities_url_2024 <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2024.json"
eclipse_cities_url_2023 <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2023.json"

# Function to process the JSON data and return a tibble
process_eclipse_data <- function(url) {
  json_data <- fromJSON(url)
  df <- as_tibble(json_data) %>% 
    clean_names() %>% 
    unnest_wider(eclipse, names_sep = "_")
  return(df)
}

# Process the 2024 eclipse data
eclipse_cities_2024 <- process_eclipse_data(eclipse_cities_url_2024)

# Separate into 'total' and 'partial'
eclipse_total_2024 <- filter(eclipse_cities_2024, !is.na(eclipse_6))
eclipse_partial_2024 <- filter(eclipse_cities_2024, is.na(eclipse_6)) %>% 
  select(-eclipse_6)

# Write the 2024 data to CSV files
write_csv(eclipse_total_2024, path(wd, "eclipse_total_2024.csv"))
write_csv(eclipse_partial_2024, path(wd, "eclipse_partial_2024.csv"))

# Process the 2023 eclipse data
eclipse_cities_2023 <- process_eclipse_data(eclipse_cities_url_2023)

# Separate into 'annular' and 'partial'
eclipse_annular_2023 <- filter(eclipse_cities_2023, !is.na(eclipse_6))
eclipse_partial_2023 <- filter(eclipse_cities_2023, is.na(eclipse_6)) %>% 
  select(-eclipse_6)

# Write the 2023 data to CSV files
write_csv(eclipse_annular_2023, path(wd, "eclipse_annular_2023.csv"))
write_csv(eclipse_partial_2023, path(wd, "eclipse_partial_2023.csv"))
  

# Assuming ea_2023 is already loaded and contains the 'eclipse_3' and 'eclipse_4' columns
ea_2023 <- ea_2023 %>%
  mutate(start_time = hms(eclipse_3),
         end_time = hms(eclipse_4),
         # Calculate duration in minutes
         duration_minutes = as.numeric(end_time - start_time, units = "mins"))

# Plot the points on a map with a color gradient based on the duration
ggplot(ea_2023, aes(x = lon, y = lat)) +
  geom_point(aes(color = duration_minutes), alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red", name = "Duration of Annularity (minutes)") +
  labs(x = "Longitude", y = "Latitude", title = "Annular Eclipse Duration") +
  theme_minimal() +
  coord_fixed(1.3)  # Ensuring the aspect ratio is fixed for map accuracy

# Load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load US states
states <- ne_states(country = "united states of america", returnclass = "sf")

# Define the continental US bounding box for plotting focus
us_bbox <- c(-125, 24, -66, 50) # Continental US approx. bounding box


# Plotting
#ggplot() +
 # geom_sf(data = world, fill = "antiquewhite", color = "gray") + # Plot the world map
 # geom_sf(data = states, color = "blue", fill = NA) + # Add US states with borders
 # geom_point(data = ea_2023, aes(x = lon, y = lat, color = duration_minutes), size = 2, alpha = 0.6) +
#  geom_point(data = eclipse_cities_2023, aes(x = lon, y = lat), color = "black", size = 1, alpha = 0.4) +
#  scale_color_gradient(low = "yellow", high = "red", name = "Duration of Annularity (minutes)") +
#  labs(x = "Longitude", y = "Latitude", title = "Eclipse Paths over the Continental US") +
#  theme_minimal() +
 # coord_sf(xlim = c(us_bbox[1], us_bbox[3]), ylim = c(us_bbox[2], us_bbox[4]), expand = FALSE) # Focus on the continental US

# Plotting
ggplot() +
  geom_sf(data = world) + # Plot the world map
  geom_sf(data = states, color = "grey70", fill = NA) + # Add US states with borders
  geom_point(data = ea_2023, aes(x = lon, y = lat, color = duration_minutes), size = .75, alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red", name = "Duration of Annularity (minutes)") +
  labs(x = "Longitude", y = "Latitude", title = "Annular Eclipse Duration over the Continental US") +
  theme_minimal() +
  coord_sf(xlim = c(us_bbox[1], us_bbox[3]), ylim = c(us_bbox[2], us_bbox[4]), expand = FALSE) # Focus on the continental US










download_process_rename_zip <- function(year) {
  base_data_path <- here("tidytuesday-exercise", "data")
  subfolder_name <- sprintf("eclipse_shapefiles_%s", year)
  subfolder_path <- file.path(base_data_path, subfolder_name)
  
  dir.create(subfolder_path, showWarnings = FALSE)
  
  shapefile_zip_url <- sprintf("https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/%seclipse_shapefiles.zip", year)
  dest_shapefile_zip <- file.path(subfolder_path, sprintf("%seclipse_shapefiles.zip", year))
  
  # Download and unzip
  download.file(shapefile_zip_url, destfile = dest_shapefile_zip)
  unzip(dest_shapefile_zip, exdir = subfolder_path)
  unlink(dest_shapefile_zip)  # Delete the zip file after extraction
  
  # List all files in the directory
  files <- list.files(path = subfolder_path, full.names = TRUE)
  
  # Specify patterns of files to keep
  patterns_to_keep <- c("ppath", "upath_lo")
  
  # Filter files based on patterns to keep
  files_to_keep <- Filter(function(file) {
    any(sapply(patterns_to_keep, function(pattern) grepl(pattern, basename(file))))
  }, files)
  
  # Rename and keep only the filtered files
  lapply(files_to_keep, function(file_path) {
    file_ext <- tools::file_ext(file_path)
    base_name <- tools::file_path_sans_ext(basename(file_path))
    new_base_name <- sprintf("%s_%s", base_name, substr(year, 3, 4))
    new_file_path <- file.path(dirname(file_path), sprintf("%s.%s", new_base_name, file_ext))
    
    file.rename(file_path, new_file_path)
  })
  
  # Optionally, remove files not matching the keep criteria
  files_to_delete <- setdiff(files, files_to_keep)
  sapply(files_to_delete, unlink)
}


download_process_rename_zip("2023")
download_process_rename_zip("2024")

options(geonamesUsername = 'anjoru')



