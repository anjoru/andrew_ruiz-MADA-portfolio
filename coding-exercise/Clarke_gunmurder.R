library(dslabs)
library(dplyr)
library(ggplot2)
library(maps)

#examine the dslabs dataset 'murders'
head(murders)
str(murders)

#create new table that includes the incident rate (per 100.000) by state for gun murders in 2010
gun_murders <- murders %>%
  mutate(IR_state_var = (total / population) * 100000)

#examine newly created table
head(gun_murders)

#create object that includes state abbreviation and incident rate
incident_state_obj <- gun_murders %>%
  select(state, IR_state_var)

#examine new object
View(incident_state_obj)

#create new table that groups by region
regional_data <- murders %>%
  group_by(region) %>%
  summarise(
    total_population = sum(population),
    total_murders = sum(total)
  )	

#calculate IR by region
region_IR <- regional_data %>%
  mutate(region_IR_var = (total_murders / total_population) * 100000)	
View(region_IR)


#create object that includes state abbreviation and incident rate
incident_region_obj <- region_IR %>%
  select(region_IR_var, region)

View(incident_region_obj)

#create map by state
states_map <- map_data("state")
incident_state_obj$state <- tolower(incident_state_obj$state)
merged_data <- merge(states_map, incident_state_obj, by.x = "region", by.y = "state", all.x = TRUE)
head(merged_data)
ggplot(data = merged_data) +
  geom_polygon(aes(x = long, y = lat, group = group, fill = IR_state_var), 
               color = "darkgray", size = 0.25) +
  coord_fixed(1.3) +
  scale_fill_gradient(low = "yellow", high = "red", name = "Incidence Rate",
                      limits = c(0, 8), 
                      breaks = c(0, 2, 4, 6, 8),
                      labels = c("0", "2", "4", "6", "8")) +
  labs(title = "Gun Murder Incidence Rates by State") +
  theme_minimal()



head(states_map)
head(incident_state_obj)



library(dplyr)
library(ggplot2)

ggplot(gun_murders, aes(x = region, y = IR_state_var, fill = region)) +
  geom_boxplot() +
  labs(title = "Incidence Rates of Gun Murders by Region",
       x = "Region",
       y = "Incidence Rate (per 100,000)") +
  theme_minimal()

# ANOVA
# Assuming your data frame is named 'gun_murders'
anova_result <- aov(IR_state_var ~ region, data = gun_murders)
summary(anova_result)



