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
library(httr)
library(geonames)

tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

eclipse_annular_2023 <- tuesdata$eclipse_annular_2023
eclipse_total_2024 <- tuesdata$eclipse_total_2024
eclipse_partial_2023 <- tuesdata$eclipse_partial_2023
eclipse_partial_2024 <- tuesdata$eclipse_partial_2024


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
eclipse_cities_url_2024 <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2024.json"
eclipse_cities_url_2023 <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2023.json"

eclipse_cities_2024 <- jsonlite::fromJSON(eclipse_cities_url_2024) |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  tidyr::unnest_wider(eclipse, names_sep = "_")


eclipse_total_2024 <- eclipse_cities_2024 |> 
  dplyr::filter(!is.na(eclipse_6))

eclipse_partial_2024 <- eclipse_cities_2024 |> 
  dplyr::filter(is.na(eclipse_6)) |> 
  dplyr::select(-eclipse_6)

eclipse_cities_2023 <- jsonlite::fromJSON(eclipse_cities_url_2023) |> 
  tibble::as_tibble() |> 
  janitor::clean_names() |> 
  tidyr::unnest_wider(eclipse, names_sep = "_")

eclipse_annular_2023 <- eclipse_cities_2023 |> 
  dplyr::filter(!is.na(eclipse_6))

eclipse_partial_2023 <- eclipse_cities_2023 |> 
  dplyr::filter(is.na(eclipse_6)) |> 
  dplyr::select(-eclipse_6)

readr::write_csv(
  eclipse_total_2024,
  fs::path(wd, "eclipse_total_2024.csv")
)
readr::write_csv(
  eclipse_partial_2024,
  fs::path(wd, "eclipse_partial_2024.csv")
)

readr::write_csv(
  eclipse_annular_2023,
  fs::path(wd, "eclipse_annular_20231.csv")
)
readr::write_csv(
  eclipse_partial_2023,
  fs::path(wd, "eclipse_partial_2023.csv")

  

# First, we convert the times from H:M:S format to period objects, then calculate the duration
ea_2023 <- eclipse_annular_2023 %>%
  mutate(start_time = hms(eclipse_3),
         end_time = hms(eclipse_4),
         duration_minutes = as.numeric(end_time - start_time, units = "mins"))

# Now plot the points on a map with a color gradient from yellow to red based on duration
ggplot(ea_2023, aes(x = lon, y = lat)) +
  geom_point(aes(color = duration_minutes), alpha = 0.6) +  # Use color to represent duration
  scale_color_gradient(low = "yellow", high = "red", name = "Duration of Annularity (minutes)") +  # Define color scale
  labs(x = "Longitude", y = "Latitude", title = "Annular Eclipse Duration") +
  theme_minimal() +
  coord_fixed(1.3)  # Use a fixed aspect ratio



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



# Assuming ea_2023 has columns named latitude and longitude
# Step 3: Create a Spatial Points Data Frame
points_sf <- st_as_sf(ea_2023, coords = c("lon", "lat"), crs = 4326)

# Step 4: Create the Polygon
# Using the convex hull method to create a polygon that covers the points
points_convex_hull <- st_convex_hull(st_union(points_sf))

# Step 5: Plot the Polygon and Points
ggplot() +
  geom_sf(data = points_convex_hull, fill = "blue", alpha = 0.3) + # Polygon with semi-transparent fill
  geom_sf(data = points_sf, color = "red") + # Points as red dots
  labs(title = "Path of the Eclipse", x = "Longitude", y = "Latitude") +
  theme_minimal()






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



