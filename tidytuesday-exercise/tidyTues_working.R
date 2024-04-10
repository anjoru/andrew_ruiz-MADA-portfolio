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

# Print the unique states
print(unique_states)

states_list <- c("AZ", "NM", "CA", "CO", "NV", "OR", "TX", "UT")

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

# Merge FIPS codes into your main dataframe based on city names and state
ea_2023_with_fips <- ea_2023 %>%
  left_join(all_places_df, by = c("name", "state"))

# Verify the merge
head(ea_2023_with_fips)



# now for 2024
# Get unique values in the 'state' column
unique_states24 <- unique(et_2024$state)

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

# Remember to replace "your_census_api_key_here" with your actual API key
get_population_by_fips <- function(fips_code) {
  fips_code_str <- sprintf("%07s", as.character(fips_code))
  url <- sprintf("https://api.census.gov/data/2019/acs/acs5?get=B01003_001E&for=place:%s&in=state:%s&key=582d18c6dbbc7a925b8142f2730cdf88ddfeb461", substr(fips_code_str, 3, 7), substr(fips_code_str, 1, 2))
  
  response <- httr::GET(url)
  
  if (httr::status_code(response) == 200) {
    content <- httr::content(response, "text", encoding = "UTF-8")
    
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



ea_2023_with_fips <- ea_2023_with_fips %>%
  rowwise() %>%
  mutate(population = get_population_by_fips(FIPS)) %>%
  ungroup()

et_2024_with_fips <- et_2024_with_fips %>%
  rowwise() %>%
  mutate(population = get_population_by_fips(FIPS)) %>%
  ungroup()





