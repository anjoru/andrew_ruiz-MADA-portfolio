# This is the working file ex_11

library(here)
library(tidymodels)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(glmnet)
library(ranger)
library(dplyr)

#Set the random seed to 1234
ex11_seed = 1234
set.seed(ex11_seed)

#load rds from the fitting exercise
# Construct the path to the RDS file using here()
file_path_mav <- here("ml-models-exercise", "mav_clean_ex11.rds")

# Load the mav_clean dataframe from the specified RDS file
mav_ex11 <- readRDS(file_path_mav)

# examine the dataset
head(mav_ex11)
dim(mav_ex11)

# Count occurrences of each category in the RACE variable
race_counts <- table(mav_ex11$RACE)

# Calculate percentages
race_percentages <- (race_counts / sum(race_counts)) * 100

# Combine counts and percentages into a data frame for better readability
race_summary <- data.frame(
  Race = names(race_counts),
  Counts = as.integer(race_counts), # Ensure counts are in integer format
  Percentages = race_percentages
)
# Print the summary data frame
print(race_summary)


# According to Table 1 of Wendling, T. et al. 
#Model-Based Evaluation of the Impact of Formulation
# and Food Intake on the Complex Oral Absorption of 
# Mavoglurant in Healthy Subjects. Pharm Res 32, 1764–1778 (2015). 
# https://doi.org/10.1007/s11095-014-1574-1
# race break down was: Caucasian (61.7), Black (30), 
# Native American (1.7) and Other (6.7)

# So, for Race
  # 1=Caucasian
  # 2=Black
  # 7=Native American
  # 88=Other

# create aliases for the RACE variable to improve readability
# Assuming 'data' is your dataframe and 'race' is the variable you want to alias
mav$RACE <- factor(mav$RACE,
                    levels = c(1, 2, 7, 88),
                    labels = c("Caucasian", "Black", "Native American", "Other"))
print(levels(mav$RACE))
print(levels(mav_ex11$RACE))

# make a pairwise correlation plot for the continuous variables.
# If we were to find any very strong correlations, we might want
#to remove those.

# Selecting only continuous variables
continuous_vars <- mav_ex11[, c("AGE", "WT", "HT")]

# Computing the correlation matrix
cor_matrix <- cor(continuous_vars, use = "complete.obs")

# Visualizing the correlation matrix
ggcorrplot(cor_matrix, method = "circle", hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, colors = c("gold", "snow", "tomato"))
print(cor_matrix)

# calculating new variable, BMI, from HT(m) and WT(kg).

mav_ex11$BMI <- mav_ex11$WT / (mav_ex11$HT^2)
str(mav_ex11)

mav_ex11 <- mav_ex11 %>%
  mutate(
    DOSE = as.factor(DOSE),
    RACE = as.factor(RACE),
    SEX = as.factor(SEX)
  )


# linear model with all predictors.
# Define a linear regression model
linear_model_spec <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

# Prepare the recipe for the linear model, specifying categorical variables
linear_model_recipe <- recipe(Y ~ ., data = mav_ex11) %>%
  step_dummy(all_nominal(), -all_outcomes()) # Convert categorical variables to dummy variables

# Combine the model and recipe into a workflow, then fit it to the data
linear_model_workflow <- workflow() %>%
  add_model(linear_model_spec) %>%
  add_recipe(linear_model_recipe) %>%
  fit(data = mav_ex11)

# LASSO
# LASSO Regression Model Specification with glmnet engine

# Then define the recipe for LASSO
lasso_model_recipe <- recipe(Y ~ ., data = mav_ex11) %>%
  step_dummy(all_nominal(), -all_outcomes())

lasso_model_spec <- linear_reg(penalty = 0.1, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Recipe for LASSO, converting categorical variables to dummy variables
lasso_model_recipe <- recipe(Y ~ ., data = mav_ex11) %>%
  step_dummy(all_nominal(), -all_outcomes())
  

# Workflow for LASSO, combining model and recipe, then fitting to data
lasso_model_workflow <- workflow() %>%
  add_model(lasso_model_spec) %>%
  add_recipe(lasso_model_recipe) %>%
  fit(data = mav_ex11)

# Random Forest
# Random Forest Model Specification with ranger engine
rf_model_spec <- rand_forest() %>%
  set_engine("ranger", seed = ex11_seed) %>%
  set_mode("regression")

# Recipe for Random Forest, ensuring categorical variables are treated correctly
# No need for step_dummy() as random forest can handle categorical variables directly
rf_model_recipe <- recipe(Y ~ ., data = mav_ex11)

# Workflow for Random Forest, combining model and recipe, then fitting to data
rf_model_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_model_recipe) %>%
  fit(data = mav_ex11)

# Linear model predections

# Predictions
lm_predictions_ex11 <- predict(linear_model_workflow, new_data = mav_ex11) %>%
  bind_cols(mav_ex11)

# Calculate RMSE
lm_rmse_ex11 <- rmse(lm_predictions_ex11, truth = Y, estimate = .pred)

# Observed vs Predicted Plot
ggplot(lm_predictions_ex11, aes(x = Y, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Observed", y = "Predicted", title = "Linear Model: Observed vs Predicted")

# Lasso model predictions

# Predictions
lasso_predictions_ex11 <- predict(lasso_model_workflow, new_data = mav_ex11) %>%
  bind_cols(mav_ex11)

# Calculate RMSE
lasso_rmse_ex11 <- rmse(lasso_predictions_ex11, truth = Y, estimate = .pred)

# Observed vs Predicted Plot
ggplot(lasso_predictions_ex11, aes(x = Y, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Observed", y = "Predicted", title = "LASSO Model: Observed vs Predicted")

# Random Forest Predctions

# Predictions
rf_predictions_ex11 <- predict(rf_model_workflow, new_data = mav_ex11) %>%
  bind_cols(mav_ex11)

# Calculate RMSE
rf_rmse_ex11 <- rmse(rf_predictions_ex11, truth = Y, estimate = .pred)

# Observed vs Predicted Plot
ggplot(rf_predictions_ex11, aes(x = Y, y = .pred)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(x = "Observed", y = "Predicted", title = "Random Forest Model: Observed vs Predicted")

#RMSE
# Create a dataframe to hold the RMSE values
rmse_summary_ex11 <- tibble(
  Model = c("Linear", "LASSO", "Random Forest"),
  RMSE = c(lm_rmse_ex11$.estimate, lasso_rmse_ex11$.estimate, rf_rmse_ex11$.estimate)
)

# Print the summary table
print(rmse_summary_ex11)


# Tuning the LASSO model wihtout CV

# Define the range of penalty values
penalty_grid <- 10^seq(-5, 2, length.out = 50)

# Update the LASSO model spec to use tune() for the penalty
lasso_model_spec_tuned <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Use the same recipe as before
lasso_model_recipe <- recipe(Y ~ ., data = mav_ex11) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Define the tuning workflow
lasso_tuning_workflow <- workflow() %>%
  add_model(lasso_model_spec_tuned) %>%
  add_recipe(lasso_model_recipe)

# Create a tibble with penalty values for tuning
penalty_tibble <- tibble(penalty = penalty_grid)

# Use apparent resampling for tuning (not recommended in practice)
lasso_resamples <- apparent(mav_ex11)

# Tune the model
lasso_tuned_results <- tune_grid(
  lasso_tuning_workflow,
  resamples = lasso_resamples,
  grid = penalty_tibble
)

# Evaluate the tuned model
best_lasso <- select_best(lasso_tuned_results, "rmse")

# Print the best penalty value
print(best_lasso)

# Visualize tuning diagnostics for LASSO
lasso_tuned_results %>% autoplot()

# as the penalty parameter increases,
# LASSO regression can drive more coefficients
# to zero, effectively removing them from the model.
# If the penalty is too large, it may remove too many 
# features, leading the model towards a null model, 
# which is a model with no predictors.

# The unpenalized linear model is fully optimized to fit 
# the training data without any restraint, possibly 
# capturing noise and overfitting. When LASSO introduces
# a penalty for complexity, it trades off some of the 
# training data fit to achieve a model that should 
# generalize better. However, since we are only evaluating 
# on the same data used to tune the penalty, we don't see 
# the benefit of this trade-off. In fact, due to this 
# evaluation approach, we may see an increase in RMSE 
# as we overly simplify the model, potentially leading 
# to underfitting when the penalty is too high





# Define the Random Forest model with specific tuning indications
rf_model_spec <- rand_forest(trees = 300, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = 123) %>%
  set_mode("regression")

# Define the recipe
rf_recipe <- recipe(Y ~ ., data = mav_ex11)

# Define the tuning grid
tuning_grid <- grid_regular(
  mtry(range = c(1, 7)),
  min_n(range = c(1, 21)),
  levels = 7
)

# Define resampling method - using the full dataset through apparent resampling
resamples <- apparent(data = mav_ex11)

# Combine the model and recipe into a workflow
rf_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_recipe)

# Tune the model using the workflow
rf_tuned_results <- tune_grid(
  rf_workflow,
  resamples = resamples,
  grid = tuning_grid
)

# Visualize the tuning results
autoplot(rf_tuned_results)

# Tuning with CV

# Set the seed for reproducibility
set.seed(ex11_seed)

# Define the range of penalty values for the grid
penalty_grid <- 10^seq(-5, 2, length.out = 50)

# Update the LASSO model spec to use tune() for the penalty
lasso_model_spec_tuned <- linear_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Define the recipe, including dummy variables as needed
lasso_model_recipe <- recipe(Y ~ ., data = mav_ex11) %>%
  step_dummy(all_nominal(), -all_outcomes())

# Define a workflow that includes the model spec and the recipe
lasso_tuning_workflow <- workflow() %>%
  add_model(lasso_model_spec_tuned) %>%
  add_recipe(lasso_model_recipe)

# Create the cross-validation resamples
cv_resamples <- vfold_cv(mav_ex11, v = 5, repeats = 5)

# Tune the model with cross-validation
lasso_tuned_cv_results <- tune_grid(
  lasso_tuning_workflow,
  resamples = cv_resamples,
  grid = penalty_tibble
)

# Visualize tuning diagnostics
autoplot(lasso_tuned_cv_results)

# RF

rf_model_spec <- rand_forest(trees = 300, mtry = tune(), min_n = tune()) %>%
  set_engine("ranger", seed = ex11_seed) %>%
  set_mode("regression")

# Define the recipe
rf_recipe <- recipe(Y ~ ., data = mav_ex11)

# Define the tuning grid
tuning_grid <- grid_regular(
  mtry(range = c(1, 7)),
  min_n(range = c(1, 21)),
  levels = 7
)

# Define resampling method - using the full dataset through apparent resampling
rf_cv_resamples <- vfold_cv(data = mav_ex11)

# Combine the model and recipe into a workflow
rf_workflow <- workflow() %>%
  add_model(rf_model_spec) %>%
  add_recipe(rf_recipe)

# Tune the model using the workflow
rf_cvtuned_results <- tune_grid(
  rf_workflow,
  resamples = resamples,
  grid = tuning_grid
)

# Set the seed for reproducibility
set.seed(ex11_seed)

# Create 5-fold CV resamples, repeated 5 times
rf_cv_resamples <- vfold_cv(data = mav_ex11, v = 5, repeats = 5)

# Tune the model using the workflow with the corrected resamples variable
rf_cvtuned_results <- tune_grid(
  rf_workflow,
  resamples = rf_cv_resamples,
  grid = tuning_grid
)

# Visualize the tuning results
autoplot(rf_cvtuned_results)


# Collect tuning results
results <- collect_metrics(rf_cvtuned_results)

# Arrange results in ascending order of RMSE
results <- results %>% 
  filter(.metric == "rmse") %>% 
  arrange(mean)

# Create a table of results for the top performing models
top_results <- results %>%
  head(10) %>%
  select(mtry, min_n, mean, std_err) %>%
  rename(MSE = mean, Std_Error = std_err)

# Print the table
print(top_results)


## if using RSME as the metric, then the RF model with
## mtry set to 5 and min_n set to 21 has the lowest
## RMSE of 671, making it the top-performing model 
## according to this metric.

## This result suggests that for this particular dataset
## and problem, a simpler Random Forest model that does
## not go into very deep trees (hence larger minimal node size)
## and considers a moderate number of predictors (mtry = 5) 
## for splitting nodes is more likely to generalize well.


