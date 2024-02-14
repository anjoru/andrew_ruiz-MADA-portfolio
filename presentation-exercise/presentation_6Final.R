library(readxl)
library(dplyr)
library(here)
library(ggplot2)
library(classInt)
library(scales)

# Set the correct path to the Excel file
file_path <- here("presentation-exercise", "EX6_mod.xlsx")

# Read the data from the Excel file
df <- read_excel(file_path)

# Ensure 'football_years' is numeric
df$football_years <- as.numeric(df$football_years)

# Define the breaks manually, ensuring the last break is 33 to create its own category
# Adjust the breaks as necessary to fit the categorization you observed
max_years <- max(df$football_years, na.rm = TRUE)
n_groups <- 13 # One less than before since 33 will be its own group
jenks_breaks <- classIntervals(df$football_years[df$football_years < max_years], n = n_groups, style = "jenks")$brks

# Ensure 33 is its own category
final_breaks <- c(jenks_breaks, max_years-1, max_years)

# Group 'football_years' using these breaks
df$year_group <- cut(df$football_years, breaks = final_breaks, include.lowest = TRUE, labels = FALSE)

# Convert 'CTEStage' to a factor with correct levels
df$CTEStage <- factor(df$CTEStage, levels = c(0, 1, 2, 3, 4))

# Proceed with summarizing and plotting as before
df_summary <- df %>%
  group_by(year_group, CTEStage) %>%
  summarise(count = n(), .groups = "drop") %>%
  left_join(df %>%
              group_by(year_group) %>%
              summarise(total = n(), .groups = "drop"), by = "year_group") %>%
  mutate(percentage = count / total)

# Calculate the count for each year group and CTE stage
df_summary <- df %>%
  group_by(year_group, CTEStage) %>%
  summarise(count = n(), .groups = "drop")

# Calculate the percentage for each year group and CTE stage
total_counts <- df_summary %>%
  group_by(year_group) %>%
  summarise(total = sum(count), .groups = "drop")

df_summary <- df_summary %>%
  left_join(total_counts, by = "year_group") %>%
  mutate(percentage = count / total)

# Plot the percentages as a stacked bar graph scaled to the same height
ggplot_object <- ggplot(df_summary, aes(x = as.factor(year_group), y = percentage, fill = as.factor(CTEStage))) +
  geom_bar(stat = "identity", position = "fill", width = 1) +
  scale_fill_manual(values = c("0" = "#D0D8DA",
                               "1" = "#F6D3AA",
                               "2" = "#EFB47D",
                               "3" = "#DC8445",
                               "4" = "#BA4B32"),
                    labels = c("No CTE", "Stage 1", "Stage 2", "Stage 3", "Stage 4")) +
  scale_y_continuous(labels = percent) +
  labs(x = NULL, # Remove default x-axis title
       y = "Percentage of Athletes", 
       fill = "Stage of CTE",
       title = "Estimated cumulative force of head hits for 631 former football players") +
  theme_minimal() +
  theme(legend.position = "top", # Move legend to the top
        axis.ticks.x = element_blank(), # Remove x-axis ticks
        axis.text.x = element_blank(), # Remove x-axis text
        plot.title = element_text(hjust = 0.5), # Center the plot title
        panel.grid.major = element_blank(), # Remove major grid lines
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank()) + # Remove panel background
  annotate("text", x = Inf, y = -0.07, label = "Increasing Cumulative Force", hjust = 2.45, vjust = .5, size = 4, fontface = "italic") +
  annotate("segment", x = -Inf, xend = Inf, y = -0.05, yend = -0.05, arrow = arrow(type = "open", ends = "last", length = unit(0.15, "inches"))) # Adjusted length

	#annotate("segment", x = -Inf, xend = Inf y = -0.05, yend = -0.05, arrow = arrow(type = "open", ends = "last", length = unit(0.5, "inches")))

# Plot the graph
print(ggplot_object)

