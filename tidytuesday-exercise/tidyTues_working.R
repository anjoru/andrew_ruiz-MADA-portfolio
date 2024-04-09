library(tidyverse)
library(jsonlite)
library(janitor)
library(here)
library(fs)
library(sf)
library(ggplot2)
library(dplyr)

tuesdata <- tidytuesdayR::tt_load(2024, week = 15)

eclipse_annular_2023 <- tuesdata$eclipse_annular_2023
eclipse_total_2024 <- tuesdata$eclipse_total_2024
eclipse_partial_2023 <- tuesdata$eclipse_partial_2023
eclipse_partial_2024 <- tuesdata$eclipse_partial_2024


wd = here::here("tidytuesday-exercise", "data")

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
  fs::path(wd, "eclipse_annular_2023.csv")
)
readr::write_csv(
  eclipse_partial_2023,
  fs::path(wd, "eclipse_partial_2023.csv")


# First, we convert the times from H:M:S format to period objects, then calculate the duration
ea_2023 <- ea_2023 %>%
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


if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(sf)
library(ggplot2)
library(dplyr)

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
