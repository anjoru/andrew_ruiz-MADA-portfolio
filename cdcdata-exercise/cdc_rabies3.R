library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forecast)

# Read the CSV file using here()
rabies2020 <- read_csv(here("cdcdata-exercise", "Rabies2020.csv"))

## Let's look at the first few rows

head(rabies2020)


# Clean column names
names(rabies2020) <- gsub("[^[:alnum:] ]", "", names(rabies2020))
names(rabies2020) <- gsub(" ", "_", names(rabies2020))

# Select relevant columns
rabies2020_selected <- rabies2020 %>%
  select(
    Reporting_Area,
    MMWR_Week,
    Rabies_Animal_Current_week = Rabies_Animal_Current_week,
    Rabies_Animal_Cum_2019,
    Location_1,
    Location_2,
      ) %>%
  # Replace NA values with 0
  mutate(
    Rabies_Animal_Current_week = replace_na(Rabies_Animal_Current_week, 0),
    Rabies_Animal_Cum_2019 = replace_na(Rabies_Animal_Cum_2019, 0),
    Rabies_Human_Current_week = replace_na(Rabies_Human_Current_week, 0),
    Rabies_Human_Cum_2019 = replace_na(Rabies_Human_Cum_2019, 0)
  )

# Focus on South Atlantic state for the rest of the analysis
south_atlantic_data <- rabies2020_selected %>%
  filter(Location_2 == "SOUTH ATLANTIC") %>%
  # Calculate incident cases for 2019
  arrange(Reporting_Area, MMWR_Week) %>%
  group_by(Reporting_Area) %>%
  mutate(Incident_Cases_2019 = Rabies_Animal_Cum_2019 - lag(Rabies_Animal_Cum_2019, default = 0)) %>%
  ungroup()
str(south_atlantic_data)

# Prepare data for graphing: Pivot to long format for both 2019 and 2020
south_atlantic_long <- south_atlantic_data %>%
  # Ensure MMWR_Week and Reporting_Area are retained for grouping in the long format
  pivot_longer(cols = c(Incident_Cases_2019, Rabies_Animal_Current_week), 
               names_to = "Year", 
               values_to = "Cases") %>%
  # Correct the Year column to reflect actual years
  mutate(Year = recode(Year, 
                       Incident_Cases_2019 = "2019", 
                       Rabies_Animal_Current_week = "2020"))

# Graph the incident animal cases from 2019 and 2020 by MMWR week
ggplot(south_atlantic_long, aes(x = MMWR_Week, y = Cases, color = Year, group = Year)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Rabies Cases by MMWR Week for 2019 and 2020 in South Atlantic",
       x = "MMWR Week",
       y = "Number of Cases",
       color = "Year") +
  scale_color_manual(values = c("2019" = "blue", "2020" = "red"))


# Lets compare the two years with a box plot
ggplot(south_atlantic_long, aes(x = Year, y = Cases, color = Year)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribution of Weekly Rabies Cases for 2019 and 2020",
       x = "Year",
       y = "Number of Cases")


# Prepare the time series data
time_series_data <- south_atlantic_data %>%
  select(MMWR_Week, Incident_Cases_2019) %>%
  mutate(MMWR_Week = as.Date(paste0("2020-", MMWR_Week, "-1"), format = "%Y-%U-%u"))

# Convert to time series object
ts_data <- ts(time_series_data$Incident_Cases_2019, frequency = 53)

# Time Series Visualization
plot(ts_data, main = "Time Series of Rabies Animal Cases by MMWR Week for 2019")

# Modeling (Auto ARIMA)
arima_model <- auto.arima(ts_data)

# Diagnostic Checking
checkresiduals(arima_model)

# The Ljung-Box test examined the residuals from an ARIMA(2,1,1) model 
#to see if they are correlated with each other. The test statistic (Q*) 
#was 16.92 with 8 degrees of freedom, resulting in a p-value of 0.03096. 
#This suggests that there is evidence of autocorrelation in the residuals, 
#indicating that the ARIMA model may not fully capture the underlying patterns 
#in the data.

# Forecasting
forecast_values <- forecast(arima_model, h = 52)

# Plot Forecast
#plot(forecast_values, main = "Forecast of Rabies Animal Cases for the Next 52 Weeks")

# Plot Forecast and Actual 2020 Cases
# Assuming you have the actual 2020 cases data stored in a variable called actual_2020_cases

# Plot Forecast and Actual 2020 Cases
plot(forecast_values, main = "Forecast vs Actual Rabies Animal Cases for 2020")
lines(south_atlantic_data$Rabies_Animal_Current_week, col = "blue", lty = 2, lwd = 2)  # Add actual 2020 cases to the plot

# Add legend
legend("topright", legend = c("Forecast", "Actual 2020 Cases"), col = c("black", "blue"), lty = c(1, 2), lwd = c(1, 2))

