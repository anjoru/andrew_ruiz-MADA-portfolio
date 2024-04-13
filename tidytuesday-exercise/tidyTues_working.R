packages <- c("devtools", "tidyverse", "jsonlite", "janitor", "here", "fs", "sf", 
              "ggplot2", "dplyr", "ggspatial", "ggmap", "rnaturalearth", "rnaturalearthdata", "httr", "geonames", 
              "patchwork", "tidycensus", "tigris")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}


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
library(rnaturalearth)
library(rnaturalearthhires)
library(rnaturalearthdata)
library(httr)
library(tidycensus)
library(patchwork)
library(tidycensus)
library(tigris)

# Use devtools to install rnaturalearthhires from GitHub
devtools::install_github("ropensci/rnaturalearthhires", force = TRUE)

tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

ea_2023 <- tuesdata$eclipse_annular_2023
et_2024 <- tuesdata$eclipse_total_2024
ea_part_2023 <- tuesdata$eclipse_partial_2023
ea_part_2024 <- tuesdata$eclipse_partial_2024


wd = here::here("tidytuesday-exercise", "data")

library(sf)
library(tigris)
###THIS WILL GET ALL THE FIPS CODE FOR COUNTY##
# Assuming 'ea_2023' is your data frame with 'lat' and 'lon' columns
pts_sf <- st_as_sf(ea_2023, coords = c("lon", "lat"), crs = 4326)

# Reproject your points to EPSG:4269 to match the CRS of shapefiles from tigris
pts_sf <- st_transform(pts_sf, 4269)

# Load states and counties shapefiles
states <- states(cb = TRUE)
counties <- counties(cb = TRUE)

# Now, perform the spatial joins
# First join with counties to get the county FIPS code
pts_sf <- st_join(pts_sf, counties)

# Then join with states to get the state FIPS code
pts_sf <- st_join(pts_sf, states)

# If you want to check the results or convert back to a data frame:
ea_2023_with_fips <- as.data.frame(pts_sf)

# This data frame now includes columns for county and state FIPS codes, along with other attributes from the shapefiles.



<<<<<<< HEAD
# Specify the URL for the JSON data
json_url <- "https://svs.gsfc.nasa.gov/vis/a000000/a005000/a005073/cities-eclipse-2023.json"
=======
# Print the resulting dataframe that has unique 'state' and 'name' pairs
View(ea_2023)
>>>>>>> parent of a71cfbc (Ex 13)

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

<<<<<<< HEAD
# Separate into 'total' and 'partial'
eclipse_total_2024 <- filter(eclipse_cities_2024, !is.na(eclipse_6))
eclipse_partial_2024 <- filter(eclipse_cities_2024, is.na(eclipse_6)) %>% 
  select(-eclipse_6)
=======
# Print the resulting dataframe that has unique 'state' and 'name' pairs
View(et_2024)
>>>>>>> parent of a71cfbc (Ex 13)

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
  

# use the 2023 annular eclipse data to visualize the duration of the eclipse at different locations.
ea_2023 <- ea_2023 %>%
  mutate(start_time = hms(eclipse_3),
         end_time = hms(eclipse_4),
         # Calculate duration in minutes
         duration_minutes = as.numeric(end_time - start_time, units = "mins"))

# use the 2024 total eclipse data to visualize the duration of the eclipse at different locations.
et_2024 <- et_2024 %>%
  mutate(
    start_time = hms(eclipse_3),
    end_time = hms(eclipse_4),
    duration_minutes = as.numeric(end_time - start_time, units = "mins")
  )
# combine the 2023 annular and 2024 total eclipse data for visualization.
ea_2023 <- ea_2023 %>% mutate(year = 2023)
et_2024 <- et_2024 %>% mutate(year = 2024)

combined_eclipse_data <- bind_rows(ea_2023, et_2024)

<<<<<<< HEAD
=======
# Find duplicate rows based on 'state' and 'name' columns
duplicatesall <- combined_eclipse_data[duplicated(combined_eclipse_data[c("state", "name")]) | duplicated(combined_eclipse_data[c("state", "name")], fromLast = TRUE), ]

# Print the rows where 'state' and 'name' are duplicated
View(duplicatesall)
# Because both path corss TX, there will be duplicates here and we will leave them. 
>>>>>>> parent of a71cfbc (Ex 13)

#get shapefiles associated with the 2023 and 2024 eclipse paths
# these are listed in the article that goes with the data release
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


# Plot the 2023 points on a map with a color gradient based on the duration
ggplot(ea_2023, aes(x = lon, y = lat)) +
  geom_point(aes(color = duration_minutes), alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "red", name = "Duration of Annularity (minutes)") +
  labs(x = "Longitude", y = "Latitude", title = "Annular Eclipse Duration") +
  theme_minimal() +
  coord_fixed(1.3)  # Ensuring the aspect ratio is fixed for map accuracy

# Plot the 2024 points on a map with a color gradient based on the duration
ggplot(et_2024, aes(x = lon, y = lat)) +
  geom_point(aes(color = duration_minutes), alpha = 0.6) +
  scale_color_gradient(low = "skyblue", high = "navy", name = "Duration of Totality (minutes)") +
  labs(x = "Longitude", y = "Latitude", title = "Total Eclipse Duration") +
  theme_minimal() +
  coord_fixed(1.3)  # Ensuring the aspect ratio is fixed for map accuracy

# Load a world map
world <- ne_countries(scale = "medium", returnclass = "sf")

# Load US states
states <- ne_states(country = "united states of america", returnclass = "sf")

# Define the continental US bounding box for plotting focus
us_bbox <- c(-125, 24, -66, 50) # Continental US approx. bounding box

<<<<<<< HEAD

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
=======

>>>>>>> parent of a71cfbc (Ex 13)

# Plotting
ggplot() +
  geom_sf(data = world) + # Plot the world map
  geom_sf(data = states, color = "grey70", fill = NA) + # Add US states with borders
  geom_point(data = ea_2023, aes(x = lon, y = lat, color = duration_minutes), size = .4, alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "grey30", name = "Duration of Annularity (minutes)") +
  labs(x = "Longitude", y = "Latitude", title = "2023 Annular Eclipse Duration over the Continental US") +
  theme_minimal() +
  coord_sf(xlim = c(us_bbox[1], us_bbox[3]), ylim = c(us_bbox[2], us_bbox[4]), expand = FALSE) # Focus on the continental US

# Plotting
ggplot() +
  geom_sf(data = world) + # Plot the world map
  geom_sf(data = states, color = "grey70", fill = NA) + # Add US states with borders
  geom_point(data = et_2024, aes(x = lon, y = lat, color = duration_minutes), size = .4, alpha = 0.6) +
  scale_color_gradient(low = "yellow", high = "grey30", name = "Duration of Totality (minutes)") +
  labs(x = "Longitude", y = "Latitude", title = "2024 Annular Eclipse Duration over the Continental US") +
  theme_minimal() +
  coord_sf(xlim = c(us_bbox[1], us_bbox[3]), ylim = c(us_bbox[2], us_bbox[4]), expand = FALSE) # Focus on the continental US


# Read the shapefiles
path_2023 <- st_read(here("tidytuesday-exercise", "data", "eclipse_shapefiles_2023", "upath_lo_23.shp"))
path_2024 <- st_read(here("tidytuesday-exercise", "data", "eclipse_shapefiles_2024", "upath_lo_24.shp"))

# Define the continental US bounding box for plotting focus
us_bbox <- c(-125, 24, -66, 50) # Continental US approx. bounding box

# Example coordinates for labels - adjust these to your preferred locations
label_coords <- data.frame(
  year = c("2023", "2024"),
  lon = c(-114, -86.7),  # Example longitude coordinates for the labels
  lat = c(36.1, 36.1)       # Example latitude coordinates for the labels
)

# Plotting with year labels
ggplot() +
  geom_sf(data = world) +
  geom_sf(data = states, color = "grey70", fill = NA) +
  # geom_sf(data = path_2023, color = "black", fill= NA, size = 0.8, alpha = 0.5) +
  # geom_sf(data = path_2024, color = "black", fill=NA, size = 0.8, alpha = 0.5) +
  geom_point(data = combined_eclipse_data, aes(x = lon, y = lat, color = duration_minutes, shape = as.factor(year)), size = .9, alpha = 1) +
  scale_color_gradientn(colors = c("yellow", "gray30"), name = "Duration (minutes)") +
  geom_sf(data = path_2023, color = "black", fill= NA, size = 0.8, alpha = 0.5) +
  geom_sf(data = path_2024, color = "black", fill=NA, size = 0.8, alpha = 0.5) +
    geom_text(data = label_coords, aes(x = lon, y = lat, label = year), size = 5, color = "black") +  # Add year labels
  labs(x = "Longitude", y = "Latitude", title = "Eclipse Duration over the Continental US") +
  theme_minimal() +
  coord_sf(xlim = c(us_bbox[1], us_bbox[3]), ylim = c(us_bbox[2], us_bbox[4]), expand = FALSE)


#COUNTY

# Make copies of lat and lon before converting to sf object
ea_2023$latitude = ea_2023$lat
ea_2023$longitude = ea_2023$lon

# Convert to sf object using lat and lon for geometry
pts_sf_2023 <- st_as_sf(ea_2023, coords = c("longitude", "latitude"), crs = 4326)
pts_sf_2023 <- st_transform(pts_sf_2023, 4269)

# Perform spatial joins as before
states_sf <- states(cb = TRUE)
counties_sf <- counties(cb = TRUE)

ea_2023_with_county <- st_join(pts_sf_2023, counties_sf)
ea_2023_with_fips <- st_join(ea_2023_with_county, states_sf)

# Convert back to a dataframe, preserving lat and lon as separate columns
ea_2023_with_fips_df <- as.data.frame(ea_2023_with_fips)

# Repeat the process for 2024 total data
# Make copies of lat and lon before converting to sf object
et_2024$latitude = et_2024$lat
et_2024$longitude = et_2024$lon

# Convert to sf object using lat and lon for geometry
pts_sf_2024 <- st_as_sf(et_2024, coords = c("longitude", "latitude"), crs = 4326)
pts_sf_2024 <- st_transform(pts_sf_2024, 4269)

# Perform spatial joins as before
et_2024_with_county <- st_join(pts_sf_2024, counties_sf)
et_2024_with_fips <- st_join(et_2024_with_county, states_sf)

# Convert back to a dataframe, preserving lat and lon as separate columns
et_2024_with_fips_df <- as.data.frame(et_2024_with_fips)


## ADD CITY FIPS CODES

<<<<<<< HEAD
if (!requireNamespace("tigris", quietly = TRUE)) install.packages("tigris")
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(tigris)
library(dplyr)




# Assuming 'ea_2023' is your dataframe

# Example for one state, replace "AZ" with dynamic state codes for full implementation
places_data <- places(state = "AZ", class = "sf")

# Convert sf data to a regular dataframe and select relevant columns
places_df <- as.data.frame(places_data) %>% 
  select(NAME, GEOID)

# Add a state column for merging purposes
places_df$state <- "AZ"

# Renaming columns for a clear merge
colnames(places_df) <- c("name", "FIPS", "state")

# Merging FIPS codes into your dataframe based on city names and state
ea_2023_with_fips <- ea_2023 %>% 
  left_join(places_df, by = c("name", "state"))

# Check the first few rows to verify the merge
head(ea_2023_with_fips)




# Get unique values in the 'state' column
unique_states <- unique(ea_2023$state)
=======
# # Get unique values in the 'state' column
unique_states23 <- unique(ea_2023_with_fips_df$state)
>>>>>>> parent of a71cfbc (Ex 13)

# Print the unique states
print(unique_states)

<<<<<<< HEAD
states_list <- c("AZ", "NM", "CA", "CO", "NV", "OR", "TX", "UT")
=======
# Since ea_2023_with_fips_df is already defined, we'll start from there

states_list23 <- c("AZ", "NM", "CA", "CO", "NV", "OR", "TX", "UT")
>>>>>>> parent of a71cfbc (Ex 13)

# Initialize an empty dataframe to collect places data for all states
all_places_df <- data.frame(name = character(), FIPS = character(), state = character(), stringsAsFactors = FALSE)

# Loop through states and retrieve places data
for (state_code in states_list) {
  places_data <- places(state = state_code, class = "sf") %>% 
    as.data.frame() %>% 
    select(NAME, GEOID)
  
  # Add a state column for each state's places data
  places_data$state <- state_code
  
  # Rename columns for consistency
  colnames(places_data) <- c("name", "FIPS", "state")
  
  # Combine this state's places data with the accumulating dataframe
  all_places_df <- rbind(all_places_df, places_data)
}

# Ensure the names in your main dataframe are formatted for matching
ea_2023$name <- toupper(ea_2023$name)
all_places_df$name <- toupper(all_places_df$name)

<<<<<<< HEAD
# Merge FIPS codes into your main dataframe based on city names and state
ea_2023_with_fips <- ea_2023 %>%
  left_join(all_places_df, by = c("name", "state"))

# Verify the merge
head(ea_2023_with_fips)

=======
# Prepare ea_2023_with_fips_df for joining by ensuring name case matches
ea_2023_with_fips_df_prepared <- ea_2023_with_fips_df %>%
  mutate(name = toupper(name), state = toupper(state))

all_places_df_2023_prepared <- all_places_df_2023 %>%
  mutate(name = toupper(name), state = toupper(state))

# Merge FIPS codes into your main dataframe based on city names and state for 2023
ea_2023_final_fips <- left_join(ea_2023_with_fips_df_prepared, all_places_df_2023_prepared, by = c("name", "state"))

# Verify the merge
head(ea_2023_final_fips)
>>>>>>> parent of a71cfbc (Ex 13)


# now for 2024
# Get unique values in the 'state' column
unique_states24 <- unique(et_2024_with_fips_df$state)

# Print the unique states
print(unique_states24)

states_list24 <- c("AR", "IL", "IN", "KY", "ME", "MI", "MO", "NH", "NY", "OH", "OK", "PA", "TX", "VT")

# Initialize an empty dataframe to collect places data for all states
all_places_df <- data.frame(name = character(), FIPS = character(), state = character(), stringsAsFactors = FALSE)

# Loop through states and retrieve places data
for (state_code in states_list24) {
  places_data <- places(state = state_code, class = "sf") %>% 
    as.data.frame() %>% 
    select(NAME, GEOID)
  
  # Add a state column for each state's places data
  places_data$state <- state_code
  
  # Rename columns for consistency
  colnames(places_data) <- c("name", "FIPS", "state")
  
  # Combine this state's places data with the accumulating dataframe
  all_places_df <- rbind(all_places_df, places_data)
}

# Ensure the names in your main dataframe are formatted for matching
et_2024$name <- toupper(et_2024$name)
all_places_df$name <- toupper(all_places_df$name)

<<<<<<< HEAD
# Merge FIPS codes into your main dataframe based on city names and state
et_2024_with_fips <- et_2024 %>%
  left_join(all_places_df, by = c("name", "state"))

# Verify the merge
head(et_2024_with_fips)




# Replace 'your_api_key_here' with your actual Census API key
census_api_key("582d18c6dbbc7a925b8142f2730cdf88ddfeb461")

get_population_by_fips <- function(fips_code) {
  # No need to reformat as a string with leading zeros here, assuming input is correct
  fips_code_str <- as.character(fips_code)
=======
# Prepare et_2024_with_fips_df for joining by ensuring name case matches
et_2024_with_fips_df_prepared <- et_2024_with_fips_df %>%
  mutate(name = toupper(name), state = toupper(state))

all_places_df_2024_prepared <- all_places_df_2024 %>%
  mutate(name = toupper(name), state = toupper(state))

# Merge FIPS codes into your main dataframe based on city names and state for 2024
et_2024_final_fips <- left_join(et_2024_with_fips_df_prepared, all_places_df_2024_prepared, by = c("name", "state"))

# Verify the merge
head(et_2024_final_fips)


## COUNTY DEMOGRAPHICS###########

# Assuming you've set your Census API key
census_api_key("582d18c6dbbc7a925b8142f2730cdf88ddfeb461")
get_demographics_by_county <- function(unique_county_fips) {
  # Initialize an empty data frame to store results
  results <- data.frame()
  
  # Loop through each unique county FIPS code to fetch demographic data
  for (fips in unique_county_fips) {
    demographics <- tryCatch({
      get_acs(geography = "county",
              variables = c("B01003_001E", "B02001_002E"), # Correct variables for total and white population
              year = 2019,
              survey = "acs5",
              geoid = fips,
              output = "wide") # Using wide format to directly get variables as columns
    }, error = function(e) {
      message("Error fetching demographic data for FIPS ", fips, ": ", e$message)
      return(data.frame(GEOID = fips, B01003_001E = NA, B02001_002E = NA)) # Return NA values in case of error
    })
    
    # Append the fetched data to the results data frame
    results <- rbind(results, demographics)
  }
  
  # Rename the columns for clarity
  results <- results %>%
    rename(TotalPopulation = B01003_001E, WhitePopulation = B02001_002E) %>%
    select(GEOID, TotalPopulation, WhitePopulation)
  
  return(results)
}

# Now, fetch demographics for each unique list of county FIPS codes
demographics_2023 <- get_demographics_by_county(unique_county_fips_2023)
demographics_2024 <- get_demographics_by_county(unique_county_fips_2024)


demographics_2023 <- demographics_2023 %>%
  group_by(GEOID) %>%
  summarise(TotalPopulation = first(TotalPopulation),
            WhitePopulation = first(WhitePopulation))

demographics_2024 <- demographics_2024 %>%
  group_by(GEOID) %>%
  summarise(TotalPopulation = first(TotalPopulation),
            WhitePopulation = first(WhitePopulation))

# Join the demographics back to the original dataframe based on county FIPS code
ea_2023_co_demo <- left_join(ea_2023_final_fips, demographics_2023, by = c("GEOID.x" = "GEOID"))
et_2024_co_demo <- left_join(et_2024_final_fips, demographics_2024, by = c("GEOID.x" = "GEOID"))


#CITY DEMOGRAPHICS

# join the by city fips code to get city level demographics
# the us_cities data has information on cities that exceed ~7000 people
# All records from the eclipse data may not have a corresponding city in the us_cities data they will be filled with NA
us_cities <- read_csv(here("tidytuesday-exercise", "data", "USA_Major_Cities.csv"))
us_cities <- us_cities %>% mutate(PLACEFIPS = as.character(PLACEFIPS))

ea_2023_co_demo <- ea_2023_co_demo %>% mutate(fips_city = as.character(fips_city))


ea_2023_all_demo <- left_join(ea_2023_co_demo, us_cities, by = c("fips_city" = "PLACEFIPS"))

# Repeat the process for 2024 total data
et_2024_co_demo <- et_2024_co_demo %>% mutate(fips_city = as.character(fips_city))

et_2024_all_demo <- left_join(et_2024_co_demo, us_cities, by = c("fips_city" = "PLACEFIPS"))

##DISTANCE TO CITIES


# Convert your dataframe to an sf object with geographical coordinates
ea_2023_all_demo_sf <- st_as_sf(ea_2023_all_demo, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Calculate the distance matrix in meters
distance_matrix <- st_distance(ea_2023_all_demo_sf)

# Replace diagonal with NA to ignore distance to itself
diag(distance_matrix) <- NA

# For each city, find the indices of the closest and second closest cities
closest_indices <- apply(distance_matrix, 1, function(x) order(x, na.last = NA)[1])
second_closest_indices <- apply(distance_matrix, 1, function(x) order(x, na.last = NA)[2])

# Create columns for the closest and second closest cities and their distances
ea_2023_all_demo$closest_city <- ea_2023_all_demo$name[closest_indices]
ea_2023_all_demo$closest_distance <- as.numeric(distance_matrix[cbind(1:nrow(distance_matrix), closest_indices)])
ea_2023_all_demo$second_closest_city <- ea_2023_all_demo$name[second_closest_indices]
ea_2023_all_demo$second_closest_distance <- as.numeric(distance_matrix[cbind(1:nrow(distance_matrix), second_closest_indices)])

# Convert meters to desired units if necessary, e.g., meters to kilometers by dividing by 1000
ea_2023_all_demo$closest_distance <- ea_2023_all_demo$closest_distance / 1000
ea_2023_all_demo$second_closest_distance <- ea_2023_all_demo$second_closest_distance / 1000



# Print the first few rows to verify
str(ea_2023_all_demo)


# Repeat the process for 2024 total data
# Convert your dataframe to an sf object with geographical coordinates
et_2024_all_demo_sf <- st_as_sf(et_2024_all_demo, coords = c("lon", "lat"), crs = 4326, agr = "constant")

# Calculate the distance matrix in meters
distance_matrix24 <- st_distance(et_2024_all_demo_sf)

# Replace diagonal with NA to ignore distance to itself
diag(distance_matrix24) <- NA

# For each city, find the indices of the closest and second closest cities
closest_indices24 <- apply(distance_matrix24, 1, function(x) order(x, na.last = NA)[1])
second_closest_indices24 <- apply(distance_matrix24, 1, function(x) order(x, na.last = NA)[2])

# Create columns for the closest and second closest cities and their distances for the 2024 data
et_2024_all_demo$closest_city <- et_2024_all_demo$name[closest_indices24]
et_2024_all_demo$closest_distance <- as.numeric(distance_matrix24[cbind(1:nrow(et_2024_all_demo), closest_indices24)])
et_2024_all_demo$second_closest_city <- et_2024_all_demo$name[second_closest_indices24]
et_2024_all_demo$second_closest_distance <- as.numeric(distance_matrix24[cbind(1:nrow(et_2024_all_demo), second_closest_indices24)])

# Convert meters to desired units if necessary, e.g., meters to kilometers by dividing by 1000
et_2024_all_demo$closest_distance <- et_2024_all_demo$closest_distance / 1000
et_2024_all_demo$second_closest_distance <- et_2024_all_demo$second_closest_distance / 1000
et_2024_all_demo


# Print the first few rows to verify
head(et_2024_all_demo)


####white population

calculate_white_percentage <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      WhitePercentage = ifelse(!is.na(WHITE) & !is.na(POPULATION) & POPULATION > 0,
                               (WHITE / POPULATION) * 100,
                               ifelse(!is.na(WhitePopulation) & !is.na(TotalPopulation) & TotalPopulation > 0,
                                      (WhitePopulation / TotalPopulation) * 100, NA)),
      Source = ifelse(!is.na(WHITE) & !is.na(POPULATION) & POPULATION > 0, "City", "County")
    ) %>%
    ungroup()
}

# Apply the function to ea_2023_all_demo
ea_2023_all_demo <- calculate_white_percentage(ea_2023_all_demo)

# Apply the function to et_2024_all_demo
et_2024_all_demo <- calculate_white_percentage(et_2024_all_demo)

# To view the results
head(ea_2023_all_demo)
head(et_2024_all_demo)

# For ea_2023_all_demo dataframe
ea_2023_cleanfinal <- ea_2023_all_demo %>%
  select(
    state, name, lat, lon, eclipse_1, eclipse_2, eclipse_3, eclipse_4, eclipse_5, eclipse_6,
    start_time, end_time, duration_minutes, TotalPopulation, ALAND.x, fips_city,
    closest_city, closest_distance, second_closest_city, second_closest_distance,
    WhitePercentage
  )

# For et_2024_all_demo dataframe
et_2024_cleanfinal <- et_2024_all_demo %>%
  select(
    state, name, lat, lon, eclipse_1, eclipse_2, eclipse_3, eclipse_4, eclipse_5, eclipse_6,
    start_time, end_time, duration_minutes, TotalPopulation, ALAND.x, fips_city,
    closest_city, closest_distance, second_closest_city, second_closest_distance,
    WhitePercentage
  )


# Check the results
str(ea_2023_cleanfinal)
str(et_2024_cleanfinal)


# Save ea_2023_cleanfinal as CSV
write_csv(ea_2023_cleanfinal, here("tidytuesday-exercise", "data", "ea_2023_cleanfinal.csv"))

# Merge the two dataframes
eclipse_merged <- bind_rows(ea_2023_cleanfinal, et_2024_cleanfinal)

eclipse_merged <- eclipse_merged %>%
  mutate(pop_density = TotalPopulation / (ALAND.x / 1000000))

str(eclipse_merged)

eclipse_merged <- eclipse_merged %>%
  mutate(pop_density = TotalPopulation / (ALAND.x / 1000000))

eclipse_merged <- eclipse_merged %>%
  select(
    state, name, lat, lon, duration_minutes, closest_city, closest_distance, second_closest_city, second_closest_distance,
    WhitePercentage, pop_density
  )
# Save et_2024_cleanfinal as CSV
write_csv(et_2024_cleanfinal, here("tidytuesday-exercise", "data", "et_2024_cleanfinal.csv"))

# Plotting the mean closest distance by population density
ggplot(eclipse_merged, aes(x = closest_distance, y = pop_density)) +
  geom_point() +
  labs(x = "Closest Distance", y = "Population Density", title = "Scatter Plot of County Population Density vs Closest Distance")


# Create the scatter plot
ggplot(eclipse_merged, aes(x = WhitePercentage, y = pop_density)) +
  geom_point() +  # Add points for each observation
  labs(x = "White Population", y = "Population Density", title = "Scatter Plot of Population Density vs White Population")

# Plotting the mean closest distance by population density
ggplot(eclipse_merged, aes(x = distance_difference, y = pop_density)) +
  geom_point() +
  labs(x = "Closest Distance", y = "Population Density", title = "Scatter Plot of County Population Density vs Closest Distance")


continuous_vars_df <- eclipse_merged %>%
  select(closest_distance, second_closest_distance, WhitePercentage, duration_minutes, pop_density)

# Compute the correlation matrix
correlation_matrix <- cor(continuous_vars_df, use = "complete.obs")

# Print the correlation matrix
print(correlation_matrix)

# the distances are highlt correlated, so we will create a new varibale that is the difference between the 2 distances
eclipse_merged$distance_difference <- eclipse_merged$second_closest_distance - eclipse_merged$closest_distance
head(eclipse_merged)
# Save the merged dataframe as CSV
write_csv(eclipse_merged, here("tidytuesday-exercise", "data", "eclipse_merged.csv"))


####MODELING
# Now we will begin the modeling. 

# First set the randon seed for reproducibility
rndseed = 4321

# Now we will split the data into training and testing sets
set.seed(rndseed)

# Splitting the dataset into training and testing sets
data_split <- initial_split(eclipse_merged, prop = 0.75)
train_data <- training(data_split)
test_data <- testing(data_split)

# Linear Regression Model Specification
lm_spec <- linear_reg() %>%
  set_engine("lm")

# Recipe for Model 1 (Using distance_difference as the only predictor)
recipe_1 <- recipe(pop_density ~ distance_difference, data = train_data)

# Recipe for Model 2 (Using all predictors)
recipe_2 <- recipe(pop_density ~ WhitePercentage + duration_minutes + distance_difference, data = train_data)

# Workflow for Model 1
workflow_1 <- workflow() %>%
  add_recipe(recipe_1) %>%
  add_model(lm_spec)

# Workflow for Model 2
workflow_2 <- workflow() %>%
  add_recipe(recipe_2) %>%
  add_model(lm_spec)

# Fit Model 1 on Training Data
fit_1 <- fit(workflow_1, data = train_data)

# Fit Model 2 on Training Data
fit_2 <- fit(workflow_2, data = train_data)

# predict
# Predict with Model 1
predictions_1 <- predict(fit_1, new_data = train_data) %>%
  bind_cols(train_data %>% select(pop_density))

# Calculate RMSE for Model 1
rmse_results_1 <- rmse(predictions_1, truth = pop_density, estimate = .pred)

# Predict with Model 2
predictions_2 <- predict(fit_2, new_data = train_data) %>%
  bind_cols(train_data %>% select(pop_density))

# Calculate RMSE for Model 2
rmse_results_2 <- rmse(predictions_2, truth = pop_density, estimate = .pred)

# Print RMSE values for both models
print(rmse_results_1)
print(rmse_results_2)

# computes the RMSE for three different models: the 'distance_differnce' model,
#the 'All Predictors' model, and a Null model. It then prints the RMSE values 
#to compare the performance of each model.

# Manually calculate the mean of pop_density from the training data for the Null model
mean_pop_density <- mean(train_data$pop_density, na.rm = TRUE)
null_predictions <- rep(mean_pop_density, nrow(train_data))
null_rsme <- sqrt(mean((train_data$pop_density - null_predictions)^2, na.rm = TRUE))

# Print RMSE values to compare model performances
cat("RMSE for distance_difference model:", rmse_results_1$.estimate, "\n")
cat("RMSE for All Predictors model:", rmse_results_2$.estimate, "\n")
cat("RMSE for Null model (manual):", null_rsme, "\n")

## Cross validation of linear model
set.seed(rndseed)

cv_folds10 <- vfold_cv(train_data, v = 10)

# Linear Regression Model Specification
model_spec_distancediff <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

model_spec_allpredictors <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# define workflow for distance difference model
workflow_distancediff <- workflow() %>%
  add_model(model_spec_distancediff) %>%
  add_formula(pop_density ~ distance_difference)

# define workflow for all predictors model
workflow_allpredictors <- workflow() %>%
  add_model(model_spec_allpredictors) %>%
  add_formula(pop_density ~ WhitePercentage + duration_minutes + distance_difference)

# Perform CV for distance difference model
cv_results_distancediff <- fit_resamples(
  workflow_distancediff,
  cv_folds10,
  metrics = metric_set(rmse))

# Perform CV for all predictors model
cv_results_allpredictors <- fit_resamples(
  workflow_allpredictors,
  cv_folds10,
  metrics = metric_set(rmse))

# cv_results_distancediff and cv_results_all contain the cross-validation results

# Extract and average RMSE for the distancediff-only model
cv_summary_distancediff <- cv_results_distancediff %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  summarise(mean_rmse_cv = mean(mean, na.rm = TRUE)) %>%
  pull(mean_rmse_cv)
>>>>>>> parent of a71cfbc (Ex 13)
  
  # Extract state and place codes from the FIPS code
  state_code <- substr(fips_code_str, 1, 2)
  place_code <- substr(fips_code_str, 3, 7)
  
  # Construct the API query URL using state and place codes
  url <- sprintf("https://api.census.gov/data/2019/acs/acs5?get=B01003_001E&for=place:%s&in=state:%s&key=%s", 
                 place_code, state_code, "your_census_api_key_here")
  
  # Perform the API request
  response <- httr::GET(url)
  
  # Process the response
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    parsed_content <- jsonlite::fromJSON(content)
    if (length(parsed_content) > 1 && !is.null(parsed_content[[1]][, "B01003_001E"])) {
      population <- as.numeric(parsed_content[[2]][, "B01003_001E"])
      return(population)
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

<<<<<<< HEAD
# Remember to replace "your_census_api_key_here" with your actual API key
get_population_by_fips <- function(fips_code) {
  fips_code_str <- sprintf("%07s", as.character(fips_code))
  url <- sprintf("https://api.census.gov/data/2019/acs/acs5?get=B01003_001E&for=place:%s&in=state:%s&key=582d18c6dbbc7a925b8142f2730cdf88ddfeb461", substr(fips_code_str, 3, 7), substr(fips_code_str, 1, 2))
  
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
=======
# summarize RSME for the distance difference model
summary_distancediff <- cv_results_distancediff %>%
  collect_metrics() 

# summarize RSME for the all predictors model
summary_allpredictors <- cv_results_allpredictors %>%
  collect_metrics()

#combine the two summaries
combined_summary <- bind_rows(
  data.frame(model = "Distance Difference", summary_distancediff),
  data.frame(model = "All Predictors", summary_allpredictors)
)

# print the combined summary
print(combined_summary)

## Put the observed values and the predicted values from your 3 original model fits to all of the training data (the ones without the CV) into a data frame.
# Create a data frame containing all predictions and the observed values
null_predictions <- rep(mean(train_data$pop_density, na.rm = TRUE), nrow(train_data))

# Create a data frame containing all predictions and the observed values
predictions_df <- bind_rows(
  data.frame(model = "Model 1 (Distance Difference)", observed = train_data$pop_density, predicted = predictions_1$.pred),
  data.frame(model = "Model 2 (All Predictors)", observed = train_data$pop_density, predicted = predictions_2$.pred),
  data.frame(model = "Null Model", observed = train_data$pop_density, predicted = null_predictions)
)

# Generate predictions for the null model
null_predictions <- rep(mean(train_data$pop_density, na.rm = TRUE), nrow(train_data))

# Create a data frame containing all predictions and the observed values
predictions_df <- bind_rows(
  data.frame(model = "Model 1 (Distance Difference)", observed = train_data$pop_density, predicted = predictions_1$.pred),
  data.frame(model = "Model 2 (All Predictors)", observed = train_data$pop_density, predicted = predictions_2$.pred),
  data.frame(model = "Null Model", observed = train_data$pop_density, predicted = null_predictions)
)

# Plot observed vs predicted values
ggplot(predictions_df, aes(x = observed, y = predicted, color = model)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "blue") +
  xlim(0, 1500) +
  ylim(0, 1500) +
  labs(x = "Observed Values", y = "Predicted Values", title = "Observed vs. Predicted Pop Density") +
  theme_minimal()

## REsiduals

# Calculate residuals for Model 2
residuals_2 <- predictions_2$.pred - train_data$pop_density

# Create a data frame for plotting
plot_data <- data.frame(predicted = predictions_2$.pred, residuals = residuals_2)

# Find the maximum absolute value of residuals to set equal limits for y-axis
max_abs_residual <- max(abs(plot_data$residuals))

# Plot predicted vs residuals for Model 2
ggplot(plot_data, aes(x = predicted, y = residuals)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  ylim(-max_abs_residual, max_abs_residual) +
  labs(x = "Predicted Values", y = "Residuals", title = "Predicted vs Residuals for Model 2") +
  theme_minimal()


## BOOTSTRAPPING
# Initialize bootstrapping
set.seed(4321)
boot_samples <- bootstraps(train_data, times = 100)

# Fit Model 2 on each bootstrap sample and predict on original training data
predictions_list <- boot_samples$splits %>%
  map(~ {
    # Extract the analysis (training) set from the current bootstrap sample
    bs_data <- analysis(.x)
>>>>>>> parent of a71cfbc (Ex 13)
    
    # Attempt to parse JSON content
    tryCatch({
      parsed_content <- jsonlite::fromJSON(content)
      # Check if the parsed content has the expected structure
      if (!is.null(parsed_content) && length(parsed_content) >= 1 && "B01003_001E" %in% names(parsed_content[[1]])) {
        population <- as.numeric(parsed_content[[2]][, "B01003_001E"])
        return(population)
      } else {
        print("Unexpected JSON structure.")
        return(NA)
      }
    }, error = function(e) {
      cat("Error parsing JSON:\n", e$message, "\n")
      return(NA)
    })
  } else {
    cat("HTTP request failed. Status code:", httr::status_code(response), "\n")
    return(NA)
  }
}


# Apply the population fetching function to the first 5 rows
first_five_population1 <- ea_2023_with_fips %>%
  slice(1:5) %>%
  rowwise() %>%
  mutate(population = get_population_by_fips(FIPS)) %>%
  ungroup()

get_population_by_fips <- function(fips_code) {
  fips_code_str <- sprintf("%07s", as.character(fips_code))
  url <- sprintf("https://api.census.gov/data/2019/acs/acs5?get=B01003_001E&for=place:%s&in=state:%s&key=582d18c6dbbc7a925b8142f2730cdf88ddfeb461", substr(fips_code_str, 3, 7), substr(fips_code_str, 1, 2))
  
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    cat("Raw JSON response:\n", content, "\n\n")  # Print raw JSON for debugging
    
<<<<<<< HEAD
    # Attempt to parse JSON content
    tryCatch({
      parsed_content <- jsonlite::fromJSON(content)
      print(parsed_content)  # Print parsed JSON for further debugging
      
      # Check if the parsed content has at least one row and the expected field
      if (!is.null(parsed_content) && nrow(parsed_content) > 0 && "B01003_001E" %in% names(parsed_content)) {
        population <- as.numeric(parsed_content$B01003_001E[1])
        return(population)
      } else {
        print("Unexpected JSON structure.")
        return(NA)
      }
    }, error = function(e) {
      cat("Error parsing JSON:\n", e$message, "\n")
      return(NA)
    })
  } else {
    cat("HTTP request failed. Status code:", httr::status_code(response), "\n")
    return(NA)
  }
}





get_population_by_fips <- function(fips_code) {
  fips_code_str <- as.character(fips_code)
  state_code <- substr(fips_code_str, 1, 2)
  place_code <- substr(fips_code_str, 3, 7)
  api_key <- "582d18c6dbbc7a925b8142f2730cdf88ddfeb461" # Replace with your actual API key
  
  url <- sprintf("https://api.census.gov/data/2019/acs/acs5?get=B01003_001E&for=place:%s&in=state:%s&key=%s", place_code, state_code, api_key)
  
  print(url) # Debugging: Print the URL to check its correctness
  
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    
    # Attempt to parse JSON content
    tryCatch({
      parsed_content <- jsonlite::fromJSON(content)
      if (length(parsed_content) > 1 && !is.null(parsed_content[[1]][, "B01003_001E"])) {
        population <- as.numeric(parsed_content[[2]][, "B01003_001E"])
        return(population)
      } else {
        return(NA)
      }
    }, error = function(e) {
      cat("Error parsing JSON:\n", e$message, "\n")
      return(NA)
    })
  } else {
    cat("HTTP request failed. Status code:", httr::status_code(response), "\n")
    return(NA)
  }
}

=======
    # Predict on the original training data
    predict(bs_fit, new_data = train_data)$`.pred`
  })

# Convert the list of predictions to a matrix
pred_matrix <- do.call(cbind, predictions_list)

# Compute median and 89% confidence intervals for each observation
pred_stats <- apply(pred_matrix, 1, function(x) quantile(x, probs = c(0.055, 0.5, 0.945), na.rm = TRUE))

# Assuming `predictions_2` has been previously calculated as predictions from Model 2 without bootstrapping
# Ensure that predictions_2 contains `.pred` column for predictions
original_predictions <- predictions_2$.pred

# Merge original predictions with calculated statistics
merged_predictions <- tibble(
  Observed = train_data$pop_density,
  Predicted = original_predictions,
  Lower_CI = pred_stats[1, ],
  Median = pred_stats[2, ],
  Upper_CI = pred_stats[3, ]
)

# Plotting Observed vs. Predicted with Confidence Intervals
ggplot(merged_predictions, aes(x = Observed, y = Predicted)) +
  geom_point(alpha = 0.6) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.2, color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Observed vs Predicted Pop Density with Bootstrap CIs",
       x = "Observed Pop Density", y = "Predicted Pop Density") +
  theme_minimal()


# Use the test data to evaluate the model

# Assuming fit_2 is your fitted model 2 object
# Predict on Training Data
predictions_train <- predict(fit_2, new_data = train_data) %>%
  bind_cols(train_data %>% select(pop_density)) %>%
  mutate(dataset = "Training")


# Predict on Test Data
predictions_test <- predict(fit_2, new_data = test_data) %>%
  bind_cols(test_data %>% select(pop_density)) %>%
  mutate(dataset = "Test")


# Combine Training and Test Predictions
combined_predictions <- bind_rows(predictions_train, predictions_test)

# Plot

ggplot(combined_predictions, aes(x = pop_density, y = .pred, color = dataset)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Observed Pop Density", y = "Predicted Pop Density", title = "Predicted vs Observed Pop Density") +
  theme_minimal() +
  scale_color_manual(values = c("Training" = "blue", "Test" = "red"))


####MODEL SPECS####
# Linear Model Specification
lm_spec <- linear_reg() %>%
  set_engine("lm")

# LASSO Model Specification
lasso_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Random Forest Model Specification
random_forest_spec <- rand_forest() %>%
  set_engine("ranger", seed = 4321) %>%
  set_mode("regression")
# there is one missing record in the pop_density column, we will remove it
train_data_clean <- train_data %>% 
  filter(!is.na(pop_density))


# Assuming 'training_data' is your training dataset and 'pop_density' is the outcome you're predicting
recipe_all <- recipe(pop_density ~ distance_difference + WhitePercentage + duration_minutes, 
                     data = train_data_clean) %>%
  step_dummy(all_nominal(), -all_outcomes()) %>% # Convert all nominal predictors to dummy variables
  step_impute_median(all_numeric(), -all_outcomes()) %>% # Impute missing values for numeric predictors
  step_impute_mode(all_nominal(), -all_outcomes()) # Impute missing values for nominal predictors

# Workflow for Linear Model
workflow_lm <- workflow() %>%
  add_recipe(recipe_all) %>%
  add_model(lm_spec)

# Workflow for LASSO Model
workflow_lasso <- workflow() %>%
  add_recipe(recipe_all) %>%
  add_model(lasso_spec)

# Workflow for Random Forest Model
workflow_rf <- workflow() %>%
  add_recipe(recipe_all) %>%
  add_model(random_forest_spec)

# Fit Linear Model
fit_lm <- fit(workflow_lm, data = train_data_clean)

# Fit LASSO Model
fit_lasso <- lasso_spec %>%
  fit(pop_density ~ ., data = train_data_clean)

# Fit Random Forest Model
fit_rf <- fit(workflow_rf, data = train_data_clean)


# Make Predictions
predictions_lm <- predict(fit_lm, new_data = train_data_clean)
predictions_lasso <- predict(fit_lasso, new_data = train_data_clean)
predictions_rf <- predict(fit_rf, new_data = train_data_clean)

# Bind predictions back to original data for RMSE calculation and plotting
train_data_clean <- train_data_clean %>%
  bind_cols(
    lm_pred = predictions_lm$.pred,
    lasso_pred = predictions_lasso$.pred,
    rf_pred = predictions_rf$.pred
  )

# Calculate RMSE for each model
rmse_lm <- rmse(train_data_clean, truth = pop_density, estimate = lm_pred)
rmse_lasso <- rmse(train_data_clean, truth = pop_density, estimate = lasso_pred)
rmse_rf <- rmse(train_data_clean, truth = pop_density, estimate = rf_pred)

# Print RMSE results
print(rmse_lm)
print(rmse_lasso)
print(rmse_rf)

# Observed vs Predicted Plots
ggplot(train_data_clean) +
  geom_point(aes(x = pop_density, y = lm_pred), color = 'blue', alpha = 0.5) +
  geom_point(aes(x = pop_density, y = lasso_pred), color = 'red', alpha = 0.5) +
  geom_point(aes(x = pop_density, y = rf_pred), color = 'green', alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  labs(x = "Observed", y = "Predicted", title = "Observed vs Predicted Pop Density") +
  theme_minimal() +
  scale_color_manual(values = c("Linear Model" = "blue", "LASSO" = "red", "Random Forest" = "green")) +
  guides(color = guide_legend(title = "Model Type"))



#TUNING
set.seed(4321)

# Assuming the linear_reg() specification is stored in `lasso_spec`
lasso_spec <- linear_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet") %>% 
  set_mode("regression")

lasso_grid <- grid_regular(penalty(range = log(c(1e-5, 1e+2))), levels = 50)


# Define the 5-fold cross-validation object, repeated 5 times
cv_folds_lasso <- vfold_cv(train_data_clean, v = 5, repeats = 5)
>>>>>>> parent of a71cfbc (Ex 13)

# Apply the population fetching function to the first 5 rows
ea_2023_with_fips <- ea_2023_with_fips %>%
  rowwise() %>%
  mutate(population = get_population_by_fips(FIPS)) %>%
  ungroup()

<<<<<<< HEAD
et_2024_with_fips <- et_2024_with_fips %>%
  rowwise() %>%
  mutate(population = get_population_by_fips(FIPS)) %>%
  ungroup()





=======
# Tune the LASSO model with the defined grid and cross-validation folds
lasso_results <- tune_grid(
  workflow_lasso,
  resamples = cv_folds_lasso,
  grid = lasso_grid,
  metrics = metric_set(rmse)
)
collect_metrics(lasso_results)


rf_model_spec <- rand_forest(trees = 300, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = 4321) %>%
  set_mode("regression")

# Define the recipe
rf_recipe <- recipe(pop_density ~ WhitePercentage + duration_minutes + distance_difference, data = train_data_clean)

# Define the tuning grid
tuning_grid <- grid_regular(
  mtry(range = c(1, 3)),
  min_n(range = c(1, 21)),
  levels = 7
)
# Define resampling method - using the full dataset through apparent resampling
rf_cv_resamples <- vfold_cv(data = train_data_clean)

# Combine the model and recipe into a workflow
rf_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_recipe)

# Tune the model using the workflow
rf_cvtuned_results <- tune_grid(
  rf_workflow,
  resamples = rf_cv_resamples,
  grid = tuning_grid
)

# Set the seed for reproducibility
set.seed(4321)


# Create 5-fold CV resamples, repeated 5 times
rf_cv_resamples <- vfold_cv(data = train_data_clean, v = 5, repeats = 5)

# Tune the model using the workflow with the corrected resamples variable
rf_cvtuned_results <- tune_grid(
  rf_workflow,
  resamples = rf_cv_resamples,
  grid = tuning_grid
)

# Visualize the tuning results
autoplot(rf_cvtuned_results)
>>>>>>> parent of a71cfbc (Ex 13)
