##Arlyn Santiago contributed to this exercise

###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving

#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("starter-analysis-exercise","data","processed-data","processeddata2.rds")

#load data. 
mydata <- readRDS(data_location)


######################################
#Data fitting/statistical analysis
######################################

############################
############################

#### First model fit
# fit linear model using height as outcome, weight as predictor

lmfit1 <- lm(Height ~ Weight, mydata)  

# place results from fit into a data frame with the tidy function
lmtable1 <- broom::tidy(lmfit1)

#look at fit results
print(lmtable1)

# save fit results table  
table_file1 = here("starter-analysis-exercise","results", "tables-files", "resulttable1.rds")
saveRDS(lmtable1, file = table_file1)

# Plotting the graph
height_weight_plot <-ggplot(mydata, aes(x = Weight, y = Height)) +
  geom_point() +  # Add points
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  labs(title = "Relationship between Height and Weight", 
       x = "Weight", 
       y = "Height") +
  theme_minimal()

# Save the plot as a PNG file
plot_file_name = here("starter-analysis-exercise", "results", "figures", "height_weight_relationship1.png")
ggsave(plot_file_name, height_weight_plot, width = 10, height = 6, dpi = 300)

############################
#### Second model fit
# fit linear model using height as outcome, weight and gender as predictor

lmfit2 <- lm(Height ~ Weight + Gender, mydata)  

# place results from fit into a data frame with the tidy function
lmtable2 <- broom::tidy(lmfit2)

#look at fit results
print(lmtable2)

# save fit results table  
table_file2 = here("starter-analysis-exercise","results", "tables-files", "resulttable2.rds")
saveRDS(lmtable2, file = table_file2)

# Plotting the graph with Gender as a factor
height_weight_gender_plot <- ggplot(mydata, aes(x = Weight, y = Height)) +
  geom_point(aes(color = Gender)) +  # Add points with color representing Gender
  geom_smooth(method = "lm", se = FALSE, aes(color = Gender, group = Gender)) +  # Add linear regression lines for each Gender group
  labs(title = "Relationship between Height, Weight, and Gender", 
       x = "Weight", 
       y = "Height") +
  theme_minimal()


# look at plot
print(height_weight_gender_plot)

# Save the plot as a PNG file
plot_file_name = here("starter-analysis-exercise", "results", "figures", "height_weight_gender_relationship.png")
ggsave(plot_file_name, height_weight_gender_plot, width = 10, height = 6, dpi = 300)

#########################
#### Third model

# Convert SR_health to a factor
mydata$SR_health <- factor(mydata$SR_health)

# linear model with Height as outcome and self-reported health status and travel time to nearest healthcare facility as predictors

lmfit3 <- lm(Height ~ SR_health + HC_time, data = mydata)

# place results from fit into a data frame with the tidy function
lmtable3 <- broom::tidy(lmfit3)

# save fit results table  
table_file3 = here::here('starter-analysis-exercise','results', 'tables-files', 'resulttable3.rds')
saveRDS(lmtable3, file = table_file3)

print(lmtable3)




  