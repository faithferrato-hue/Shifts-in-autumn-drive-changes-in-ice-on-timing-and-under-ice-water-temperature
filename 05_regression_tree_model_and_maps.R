-------------------------------------------------------------------
# Script: 05_regression_tree_model_and_maps.R
# Author: Faith Ferrato
# Purpose: create regression tree model and maps below nodes 
-------------------------------------------------------------------

# Load libraries ----
library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(sf)
library(gridExtra)
library(here)

# Load and Prepare Data ----
# Data frame below is from scriot "04_predictor_varibles_for_models.R"
regression_tree_data <- read.csv("C:/location/regression_tree_data.csv", header = TRUE)

# Remove rows with missing values in relevant variables
regression_tree_data_NARM <- regression_tree_data %>% drop_na(avg_under_ice_temp, 
                                                              ice_on_doy, fall_length_min_15, fall_length, 
                                                              mean_temp_2week, mean_temp_1month, mean_temp_dec, mean_temp_ond, 
                                                              wind_speed_2week, wind_speed_1month, wind_speed_dec, wind_speed_ond, 
                                                              precipitation_2week, precipitation_1month, precipitation_dec, precipitation_ond, 
                                                              log_snow_depth_2week, log_snow_depth_1month, snow_depth_dec, snow_depth_ond, 
                                                              min_temp_2week, min_temp_1month, min_temp_dec, min_temp_ond, 
                                                              log_shortwave_2week, log_shortwave_1month, shortwave_dec, shortwave_ond, 
                                                              Lake_area, Shore_len, Vol_total, Depth_avg, Elevation, latitude, longitude)

# Split data into training and testing sets ----

# Set seed to ensure replicability
set.seed(333)

# Create a smaller data frame with only predictors
predictors <- regression_tree_data_NARM %>% dplyr::select(ice_on_doy, fall_length_min_15, fall_length, 
                                                          mean_temp_2week, mean_temp_1month, mean_temp_dec, mean_temp_ond, 
                                                          wind_speed_2week, wind_speed_1month, wind_speed_dec, wind_speed_ond, 
                                                          precipitation_2week, precipitation_1month, precipitation_dec, precipitation_ond, 
                                                          log_snow_depth_2week, log_snow_depth_1month, snow_depth_dec, snow_depth_ond, 
                                                          min_temp_2week, min_temp_1month, min_temp_dec, min_temp_ond, 
                                                          log_shortwave_2week, log_shortwave_1month, shortwave_dec, shortwave_ond, 
                                                          Lake_area, Shore_len, Vol_total, Depth_avg, Elevation)

str(predictors)

# Convert all columns in the predictors data frame to numeric
predictors <- predictors %>% 
  mutate(across(everything(), as.numeric))


# Creates a data frame with target variable
targets <- regression_tree_data_NARM$avg_under_ice_temp

# This code uses training and testing sets
# In this case, we use 80% of our data (0.8) as our training set
train_index_ui_temp <- createDataPartition(targets, p = 0.8, list = FALSE)

# Create a data frame that contains both predictors and targets, this is the training set
train_ui_temp <- data.frame(predictors[train_index_ui_temp, ], target = targets[train_index_ui_temp])

# Same as above but this is the testing set
testing_ui_temp <- data.frame(predictors[-train_index_ui_temp, ], target = targets[-train_index_ui_temp])

# Train Regression Tree Model ----
# Controls tree creation through cross-validation to ensure a certain # of trees match.
train_control1 <- trainControl(method = "CV", number = 20)  # 20 or more folds

# Controls minimum allowed complexity parameter 
tune_grid1 <- expand.grid(cp = seq(0.0015, 0.1, by = 0.015))

# This one line of code does all our modeling for us. The . tells the tree that it can try every available predictor variable.
model_ui_temp <- train(target ~ ., data = train_ui_temp, method = "rpart", trControl = train_control1, tuneGrid =  tune_grid1, control = rpart.control(maxdepth = 20))

# Model evaluation ----
rpart.plot(model_ui_temp$finalModel,
           box.palette = "BuRd",  # Blue to Red
           split.fun = split.fun)

print(model_ui_temp)
summary(model_ui_temp)

# Charting predictions versus trained data
predictions_ui_temp <- predict(model_ui_temp, newdata = testing_ui_temp)

# Compare observed and predicted values
dat1 <- data.frame(Model = predictions_ui_temp, Observed = testing_ui_temp$target )

# Plot model fit
dat1 %>% ggplot(aes(x = Observed, y = Model)) +
  geom_point() + geom_abline() + geom_smooth(method = "lm", se = FALSE)

# Calculate R^2
mod <- lm(Observed ~ Model, data = dat1)
s <- summary(mod)
s$r.squared # how to get the model's r^2

r2 <- s$r.squared
print(r2)

#Map Terminal Nodes (Spatial Visualization) ----

# Assign terminal node IDs to each observation 
regression_tree_data_NARM$node_id <- predict(model_ui_temp$finalModel, 
                                             newdata = regression_tree_data_NARM, 
                                             type = "vector")

# Summarize average temperature per terminal node
terminal_nodes <- regression_tree_data_NARM %>%
  group_by(node_id) %>%
  summarize(avg_temp = mean(avg_under_ice_temp),
            n = n()) %>%
  arrange(desc(avg_temp))  # Sort from warmest to coldest

# Assign node colors (manual, ordered from warmest to coldest)
node_colors <- c("red",        # For first 3.4°C node
                 "red",        # For second 3.4°C node
                 "#4682B4",    # For 2.5°C node (steel blue)
                 "#1E90FF",    # For 2.4°C node (cadet blue)
                 "#E6F5FF")    # For 1.7°C node (dodger blue)

# Map creation function
create_node_map <- function(node_data, color, n) {
  finland <- map_data("world", region = "Finland")
  
  ggplot() +
    geom_polygon(data = finland, aes(x = long, y = lat, group = group), 
                 fill = "lightgray", color = "black") +
    geom_point(data = node_data, aes(x = longitude, y = latitude), 
               color = color, size = 3) +
    coord_fixed(ratio = 1.3, xlim = c(19, 32), ylim = c(59, 71)) +
    theme_void() +
    theme(plot.margin = unit(c(0, 0, 0, 0), "cm"),
          plot.title = element_text(size = 15)) +
    labs(title = paste0("n: ", n))
}

# Generate one map per node with colours
node_maps <- list()
for (i in 1:nrow(terminal_nodes)) {
  n <- terminal_nodes$node_id[i]
  node_data <- regression_tree_data_NARM[regression_tree_data_NARM$node_id == n, ]
  node_maps[[i]] <- create_node_map(node_data, node_colors[i], terminal_nodes$n[i])
}

# Combine and export maps
maps_only <- grid.arrange(grobs = node_maps, ncol = length(node_maps))
print(maps_only)

# Save just the maps
ggsave("terminal_node_maps.png", maps_only, width = 15, height = 4, dpi = 300)

# NOTE: This was the final regression tree model within the paper
# maps for below terminal nodes were downloaded and the regression tree was created 
# within powerpoint for better visualization 
