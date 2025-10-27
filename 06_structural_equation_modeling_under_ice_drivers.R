-----------------------------------------------------------
# Script: 06_structural_equation_modeling_under_ice_drivers
# Author: Faith Ferrato
# Purpose: Explore how fall meteorological and morphological
# variables influence ice-on timing and under-ice temperature
-------------------------------------------------------------
#*NOTE: close to 60 models were created to construct the final 
  # SEM withing the paper. Below are 10 models showing the processes 
  # of trial and error to find the best fitting model. 
  # The 10th model is the SEM included within the paper. 
  
# Load required libraries and data ----
library(lavaan)
library(ggcorrplot)
library(car)
library(semPlot)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(patchwork)

# Read and prepare data
# *NOTE: Dataset used within this script was created in script "05_regression_tree_model_and_maps.R"
sem_data <- read.csv("C:/location/regression_tree_data.csv", header = TRUE)

# Subset relevant variables for SEM testing
sem_data_subset <- sem_data %>%
  select(avg_under_ice_temp, # response variable 
         ice_on_doy, fall_length_min_15, fall_length, fall_length_min, fall_length_15, 
         mean_temp_2week, mean_temp_1month, mean_temp_dec, mean_temp_ond, 
         wind_speed_2week, wind_speed_1month, wind_speed_dec, wind_speed_ond, 
         precipitation_2week, precipitation_1month, precipitation_dec, precipitation_ond, 
         log_snow_depth_2week, log_snow_depth_1month, snow_depth_dec, snow_depth_ond, 
         min_temp_2week, min_temp_1month, min_temp_dec, min_temp_ond, 
         log_shortwave_2week, log_shortwave_1month, shortwave_dec, shortwave_ond, 
         Lake_area, Shore_len, Vol_total, Depth_avg, Elevation, latitude)

# Standardize all variables (mean = 0, sd = 1) to handle differences in units
sem_data_subset <- sem_data_subset %>%
  mutate(across(everything(), scale))

# Model 1 ----

model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_ond
  ice_on_doy ~ shortwave_ond + min_temp_ond
  fall_length_min_15 ~ min_temp_ond
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit and summarize the model
sem_fit <- sem(model, data = sem_data_subset)
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (OND)",
  "shortwave_ond" = "Shortwave (OND)",
  "min_temp_ond" = "Min Temp (OND)"
)

# Visualize the SEM model with larger text in boxes
# Extract standardized coefficients
edges <- standardizedSolution(sem_fit)[, c("lhs", "rhs", "est.std")]
edges$label <- round(edges$est.std, 2)

# Plot with explicit labels
semPaths(
  sem_fit,
  what = "std",
  edgeLabels = edges$label,  # Manually set edge labels
  layout = "tree",
  edge.label.cex = 1,
  sizeMan = 12,
  label.cex = 1.0,
  fade = FALSE,
  asize = 2,
  residScale = 6,
  nodeLabels = unname(custom_labels)
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 2 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_dec
  ice_on_doy ~ shortwave_dec + min_temp_ond
  fall_length_min_15 ~ min_temp_ond 
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min(_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (OND)",
  "shortwave_dec" = "Shortwave (dec)",
  "min_temp_ond" = "Min Temp (OND)"
)

# Visualize the SEM model with larger text in boxes
semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 3 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_dec + Lake_area
  ice_on_doy ~ shortwave_dec + min_temp_ond  
  fall_length_min_15 ~ min_temp_ond 
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_dec" = "Shortwave (dec)",
  "Lake_area" = "Lake Area",
  "min_temp_ond" = "Min temp (OND)"
)

semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 4 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_ond + Lake_area
  ice_on_doy ~ shortwave_ond + min_temp_ond + Lake_area  
  fall_length_min_15 ~ min_temp_ond 
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_ond" = "Shortwave (ond)",
  "Lake_area" = "Lake Area",
  "min_temp_ond" = "Min temp (OND)"
)

semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 5 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_dec
  ice_on_doy ~ shortwave_dec + min_temp_ond + Vol_total
  fall_length_min_15 ~ min_temp_ond + Vol_total
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_dec" = "Shortwave (dec)",
  "min_temp_ond" = "Min temp (OND)",
  "Vol_total" = "Total Volume"
)

# Visualize the SEM model with larger text in boxes
semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 6 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_ond + Vol_total
  ice_on_doy ~ shortwave_ond + min_temp_ond + Vol_total
  fall_length_min_15 ~ min_temp_ond 
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_ond" = "Shortwave (ond)",
  "Vol_total" = "Total volume",
  "min_temp_ond" = "Min Temp (OND)"
)

# Visualize the SEM model with larger text in boxes
semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 7 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + latitude
  ice_on_doy ~ min_temp_ond + latitude 
  fall_length_min_15 ~ min_temp_ond + latitude
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "latitude" = "Latitude",
  "min_temp_ond" = "Min temp (OND)"
)

# Visualize the SEM model with larger text in boxes
semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 8 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_ond + Depth_avg
  ice_on_doy ~ shortwave_ond + min_temp_ond + Depth_avg  
  fall_length_min_15 ~ min_temp_ond 
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_ond" = "Shortwave (ond)",
  "Depth_avg" = "Lake depth ",
  "min_temp_ond" = "Min temp (OND)"
)

semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

# Model 9 ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_ond + Shore_len
  ice_on_doy ~ shortwave_ond + min_temp_ond + Shore_len  
  fall_length_min_15 ~ min_temp_ond
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length_min_15 ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length_min_15" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_ond" = "Shortwave (ond)",
  "Shore_len" = "Shore length",
  "min_temp_ond" = "Min temp (OND)"
)

semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Model 10: SEM in paper ----
model <- '
  # Direct effects
  avg_under_ice_temp ~ ice_on_doy + wind_speed_ond + shortwave_ond + Lake_area
  ice_on_doy ~ shortwave_ond + min_temp_ond + Lake_area  
  fall_length ~ min_temp_ond
  
  # Covariances (relationships that are correlated but not causally related)
  fall_length ~~ ice_on_doy
  
'

# Fit the Model
sem_fit <- sem(model, data = sem_data_subset)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)

# Check modification indices for potential improvements
mod_indices <- modindices(sem_fit)
print(mod_indices)

# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_on_doy" = "Ice-On DOY",
  "fall_length" = "Fall Length",
  "wind_speed_ond" = "Wind Speed (ond)",
  "shortwave_ond" = "Shortwave (ond)",
  "Lake_area" = "Lake Area",
  "min_temp_ond" = "Min temp (OND)"
)

semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "tree",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

standardized_solution <- standardizedSolution(sem_fit)
print(standardized_solution)

# Visualizations for figures in paper ----
# *NOTE: the SEM visualization was created using powerpoint
# Below are the plots of correlations between variables 
# featured in the final SEM figure in the paper 

# Correlation of ice_on_doy vs avg_under_ice_temp
ice_under_ice_plot <- ggplot(sem_data_subset, aes(x = ice_on_doy, y = avg_under_ice_temp)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, fill = "gray80") +
  stat_cor(
    method = "spearman",
    label.x.npc = 0,   # Near the top-right, but inside
    label.y.npc = 0.95,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Ice-on day of year", 
    y = "Average under-ice bottom water temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(ice_under_ice_plot)

# Correlation of fall_length vs avg_under_ice_temp
fall_length_under_ice_plot <- ggplot(sem_data_subset, aes(x = fall_length, y = avg_under_ice_temp)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", linetype = "dashed", se = TRUE, fill = "gray80") +
  stat_cor(
    method = "spearman",
    label.x.npc = 0,   # Near the top-right, but inside
    label.y.npc = 0.95,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Autumn length (days)", 
    y = "Average under-ice bottom water temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(fall_length_under_ice_plot)

# Correlation of wind_speed_ond vs avg_under_ice_temp
wind_under_ice_plot <- ggplot(sem_data_subset, aes(x = wind_speed_ond, y = avg_under_ice_temp)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, fill = "gray80")  +
  stat_cor(
    method = "spearman",
    label.x.npc = 0,   # Near the top-right, but inside
    label.y.npc = 0.95,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Average wind speed OND (m/s)", 
    y = "Average under-ice bottom water temperature (°C)"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(wind_under_ice_plot)

# Correlation of fall_length vs ice_on_doy
fall_length_ice_plot <- ggplot(sem_data_subset, aes(x = fall_length, y = ice_on_doy)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  
  # Add dashed regression line with confidence interval
  geom_smooth(method = "lm", se = TRUE, color = "black", linetype = "dashed") +
  
  stat_cor(
    method = "spearman",
    label.x.npc = 0,   # Near the top-right, but inside
    label.y.npc = 0.95,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Autumn length (days)", 
    y = "Ice-on day of year"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

print(fall_length_ice_plot)

# Create panel of correlations for final figure ----
# Explicitly specify grid layout
panel_fig <- fall_length_ice_plot + fall_length_under_ice_plot + ice_under_ice_plot + wind_under_ice_plot + 
  plot_layout(nrow = 2, ncol = 2)  # 2 rows, 2 columns
print(panel_fig)

setwd("C:/location")
ggsave("panel_fig.png", panel_fig, width = 22, height = 13, units = "in", dpi = 300)
