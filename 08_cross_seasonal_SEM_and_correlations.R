-----------------------------------------------------------
  # Script: 08_cross_seasonal_SEM_and_correlations
  # Author: Faith Ferrato
  # Purpose: Identify lakes of interest, create additional summer variables (e.g. max summer swt)
  # 
-------------------------------------------------------------
# Load Libraries ----
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(janitor)
library(readxl)
library(lavaan)
library(ggcorrplot)
library(car)
library(semPlot)
library(ggpubr)
library(ggplot2)

# Read surface water temperature data ----
# This df was directly from Syke
swt_data <- read_excel("C:/location/SWT Finland 1916-2023.xlsx")%>%
  clean_names() 

#select columns of interest
swt_data_clean <- swt_data[, c("station_name", "time", "temperature_co")]

# Only 5 lake sites were chosen for this analysis
# Needed complete pairwise observation of under-ice water temperature, ice phenology, and autumn SWT
# Only 5 lakes met this criteria

# Define the lake names of interest
selected_lakes <- c("Kallavesi, Kuopio", 
                    "Pielinen, Nurmes", 
                    "Konnevesi, etelä", 
                    "Säkylän Pyhäjärvi", 
                    "Inari, Nellim")

# Filter the dataset for only these lakes
swt_data_filtered <- swt_data_clean %>% 
  filter(station_name %in% selected_lakes)

# Sort by year and day etc. 
# Convert the 'time' column to Date format if it's not already
swt_data_filtered <- swt_data_filtered %>%
  mutate(time = as.Date(time),  # Ensure it's in Date format
         year = year(time), 
         month = month(time), 
         day = day(time))

# Remove the original 'time' column 
swt_data_filtered <- swt_data_filtered %>%
  select(-time)
print(swt_data_filtered)

# Rename station names to join with ice and under-ice water temperature data 
swt_data_filtered <- swt_data_filtered %>%
  mutate(station_name = case_when(
    station_name == "Kallavesi, Kuopio" ~ "Kallavesi Kayneensalo",
    station_name == "Pielinen, Nurmes" ~ "Pielinen Nurmes_ui",
    station_name == "Konnevesi, etelä" ~ "Konnevesi Nareselka",
    station_name == "Säkylän Pyhäjärvi" ~ "Sakylan Pyhajarvi",
    station_name == "Inari, Nellim" ~ "Inari Paksuvuono",
    TRUE ~ station_name
  )) %>%
  rename(lake = station_name)
print(swt_data_filtered)

# Make columns numeric
swt_data_filtered <- swt_data_filtered %>%
  mutate(
    year = as.numeric(year),   # Ensure year is numeric
    month = as.numeric(month)  # Ensure month is numeric
  )

# Add water year 
swt_data_adjusted <- swt_data_filtered %>%
  mutate(water_year = ifelse(month >= 10, year + 1, year)) 

# Create columns for max summer temperature and max swt doy 
swt_data_adjusted <- swt_data_adjusted %>%
  group_by(lake, water_year) %>%
  # Find the maximum temperature for each group
  mutate(swt_max_temp = max(temperature_co, na.rm = TRUE)) %>%
  # Identify the date of max temperature (this will give you the row with the max temperature)
  mutate(swt_max_doy = ifelse(temperature_co == swt_max_temp, 
                              yday(make_date(year, month, day)), NA_real_)) %>%
  # Now propagate the swt_max_doy value to all rows within the group
  mutate(swt_max_doy = max(swt_max_doy, na.rm = TRUE)) %>% 
  ungroup()

# Merge swt_data_merged to regression_tree_data
# NOTE: regression_tree_data was curated within "04_predictor_varibles_for_models.R" script within the repository 
regression_tree_data <- read.csv("C:/location/regression_tree_data.csv", header = TRUE)

# Merge selecting only lake, water_year, ice_on_doy, and avg_under_ice_temp from regression data
swt_data_merged <- swt_data_adjusted %>%
  left_join(regression_tree_data %>% select(lake, water_year, ice_on_doy, avg_under_ice_temp),
            by = c("lake", "water_year"))

# Make As.date
swt_data_merged <- swt_data_merged %>%
  mutate(ice_on_date = as.Date(ice_on_doy, origin = paste0(water_year - 1, "-01-01")))

# Calculate Autumn swt average
swt_fall_avg <- swt_data_merged %>%
  filter(as.Date(paste0(water_year - 1, "-", month, "-", day)) >= as.Date(paste0(water_year - 1, "-10-01")) &
           as.Date(paste0(water_year - 1, "-", month, "-", day)) <= ice_on_date) %>%
  group_by(lake, water_year) %>%
  summarise(swt_fall_avg = mean(temperature_co, na.rm = TRUE), .groups = "drop")

# Calculate summer SWT average
swt_summer_avg <- swt_data_merged %>%
  filter(month %in% c(6, 7, 8)) %>%  # Filter for June, July, August
  group_by(lake, water_year) %>%
  summarise(swt_summer_avg = mean(temperature_co, na.rm = TRUE), .groups = "drop")

# Adding averages to final dataframe
final_swt_assessment <- swt_data_merged %>%
  left_join(swt_fall_avg, by = c("lake", "water_year")) %>%
  left_join(swt_summer_avg, by = c("lake", "water_year"))

# Remove NA 
final_swt_assessment <- final_swt_assessment %>%
  filter(!is.na(ice_on_doy))

# Select variables
final_swt_assessment <- final_swt_assessment %>%
  select(lake, water_year, ice_on_date, ice_on_doy, avg_under_ice_temp, swt_max_temp, swt_fall_avg, swt_max_doy, swt_summer_avg )

# make sure all distinct before proceeding 
final_swt_assessment <- final_swt_assessment %>% distinct()

# Read ice phenology data ----
# The df below was curated in the script "02_under_ice_water_temperature_calculations.R"
all_combined_data_under_ice_deep <- read.csv("C:/location/all_combined_data_under_ice_deep.csv", header = TRUE)

# Rename and mutate as.date
cor_with_ice_off <- all_combined_data_under_ice_deep %>%
  rowwise() %>%
  mutate(
    ice_on_day_of_year = yday(as.Date(`ice_on`)),
    ice_off_day_of_year = yday(as.Date(`ice_off`)),
    
    # Adjust ice-on DOY for cases where ice-on occurs in Jan-Apr (previous water year)
    ice_on_doy = ifelse(
      month(as.Date(`ice_on`)) %in% 1:4,
      ice_on_day_of_year + ifelse(leap_year(year(as.Date(`ice_on`)) - 1), 366, 365),
      ice_on_day_of_year
    ),
    
    # Keep ice-off DOY as-is, no special adjustments
    ice_off_doy = ice_off_day_of_year
  ) %>%
  ungroup() %>%
  group_by(lake, water_year, ice_on_doy, ice_off_doy, ice_on, ice_off) %>%  # Keep ice_on and ice_off
  filter(date >= as.Date(`ice_on`) & date <= as.Date(ice_off)) %>%
  summarize(avg_temp = mean(temp_C, na.rm = TRUE))

# Rename lake ids to lake names
lake_mapping <- data.frame(
  lake = as.character(1:9),  # Original lake names as strings
  observation_station = c(
    "Kallavesi Kayneensalo",
    "Pielinen Nurmes_ui",
    "Paijanne Linnasaari",
    "Paijanne Paijatsalo",
    "Konnevesi Nareselka",
    "Konnevesi Pynnolanniemi",
    "Sakylan Pyhajarvi",
    "Pesiojarvi",
    "Inari Paksuvuono"
  )
)

# Make lake a Character
cor_with_ice_off <- cor_with_ice_off %>%
  mutate(lake = as.character(lake))

# Replace the lake column with corresponding observation_station names but keep the column name as "lake"
cor_with_ice_off <- cor_with_ice_off %>%
  left_join(lake_mapping, by = "lake") %>%  # Join to get the observation_station names
  mutate(lake = observation_station) %>%  # Update the lake column with observation_station names
  select(-observation_station)  # Remove the extra column if not needed

# Define the lake names of interest (same 5 lakes as above for SWT df)
selected_lakes <- c("Kallavesi Kayneensalo",
                    "Pielinen Nurmes_ui",
                    "Konnevesi Nareselka",
                    "Sakylan Pyhajarvi",
                    "Inari Paksuvuono")

# Filter the dataset for only these lakes
cor_with_ice_off_filtered <- cor_with_ice_off %>% 
  filter(lake %in% selected_lakes)

# Select variables of interest
cor_with_ice_off_filtered <- cor_with_ice_off_filtered %>%
  select(lake, water_year, ice_off_doy, ice_off)

# Combine df together 
final_swt_assessment <- final_swt_assessment %>%
  left_join(cor_with_ice_off_filtered, by = c("lake", "water_year")) 

# Rename column from joining
final_swt_assessment <- final_swt_assessment %>%
  rename(ice_on_doy = ice_on_doy.x) %>%
  select(-ice_on_doy.y)

# Remove duplicate column 
final_swt_assessment <- final_swt_assessment %>%
  select(-ice_on)

# Add duration as a variable 
final_swt_assessment <- final_swt_assessment %>%
  mutate(
    ice_duration = as.integer(difftime(as.Date(ice_off), as.Date(ice_on_date), units = "days"))
  )
print(final_swt_assessment)

# Load climate data ----
# This df below is curated within the script "03_combined_climate_variables.R"
ui_climate_variable_data_cleaned <- read.csv("C:/location/ui_climate_variable_data_cleaned.csv", header = TRUE)

# View climate data frame
view(ui_climate_variable_data_cleaned)

# Select variables of interest 
ui_climate_variable_data_summer <- ui_climate_variable_data_cleaned %>%
  select(observation_station, mean_temp_c, min_temp_c, wind_speed_ms, year, month, day, downward_shortwave_radiation_w_m2)

# Rename column 
ui_climate_variable_data_summer <- ui_climate_variable_data_summer %>%
  rename(lake = observation_station)

# Creating summer predictor varibles 
ui_climate_variable_data_summer <- ui_climate_variable_data_summer %>%
  group_by(lake, year) %>%
  mutate(
    # june july august variables
    mean_temp_jja = mean(mean_temp_c[month %in% c(6, 7, 8)], na.rm = TRUE), #months june july and august
    min_temp_jja = mean(min_temp_c[month %in% c(6, 7, 8)], na.rm = TRUE),
    shortwave_jja = mean(downward_shortwave_radiation_w_m2[month %in% c(6, 7, 8)], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(lake, year, starts_with("mean_temp"), starts_with("min_temp"), starts_with("shortwave"))

# Remove origional columns 
ui_climate_variable_data_summer <- ui_climate_variable_data_summer %>%
  select(
    -mean_temp_c,           # Remove the original mean_temp_c column
    -min_temp_c,            # Remove the original min_temp_c column
  )

# Remove duplicate rows
ui_climate_variable_data_summer <- ui_climate_variable_data_summer %>%
  distinct()

# Rename column
ui_climate_variable_data_summer <- ui_climate_variable_data_summer %>%
  rename(water_year = year)

# Combine all dataframes for cross-seasonal SEM ----

# Load data set from our original SEM
# NOTE: regression_tree_data was curated within "04_predictor_varibles_for_models.R" script within the repository 

sem_data <- read.csv("C:/location/regression_tree_data.csv", header = TRUE)

# Select variables 
sem_data_full <- sem_data %>%
  select(lake, water_year, fall_length_min_15, fall_length, fall_length_min, fall_length_15, 
         mean_temp_2week, mean_temp_1month, mean_temp_dec, mean_temp_ond, 
         wind_speed_2week, wind_speed_1month, wind_speed_dec, wind_speed_ond, 
         precipitation_2week, precipitation_1month, precipitation_dec, precipitation_ond, 
         log_snow_depth_2week, log_snow_depth_1month, snow_depth_dec, snow_depth_ond, 
         min_temp_2week, min_temp_1month, min_temp_dec, min_temp_ond, 
         log_shortwave_2week, log_shortwave_1month, shortwave_dec, shortwave_ond, 
         Lake_area, Shore_len, Vol_total, Depth_avg, Elevation, latitude)

# Perform the right join with SWT dataframe above
merged_swt_df <- final_swt_assessment %>%
  right_join(sem_data_full, by = c("lake", "water_year"))

# Remove NA values
merged_swt_df <- merged_swt_df %>%
  drop_na(avg_under_ice_temp)

# Final merge with summer climate variable dataframe above 
final_merged_swt_df <- ui_climate_variable_data_summer %>%
  right_join(merged_swt_df, by = c("lake", "water_year"))

# Cross Seasonal Structural equation model ----
# Similar to the first SEM, there were multiple models made 
# This was the only model that preformed to standard (based on indices selected) with the variables we had. 

# Rename and standardize variables to handle different units
final_swt_assessment_1 <- final_merged_swt_df %>%
  mutate(across(where(is.numeric), scale))

# Model Featured within the final paper ----
model <- '
  # Direct effects
  swt_max_temp ~ ice_off_doy + avg_under_ice_temp + ice_on_doy
  ice_on_doy ~ swt_fall_avg
  avg_under_ice_temp ~ ice_on_doy
  ice_off_doy ~ ice_on_doy
'

# Fit the Model
sem_fit <- sem(model, data = final_swt_assessment_1)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)


# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "swt_max_temp" = "Max SWT",
  "ice_on_doy" = "Ice-On DOY",
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_off_doy" = "Ice-off DOY",
  "swt_fall_avg" = "Fall SWT"
)

# Visualize the SEM model with larger text in boxes
semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "spring",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6,             # Residual size scaling
  nodeLabels = unname(custom_labels) # Explicitly map custom labels
)

# NOTE: Final model was made in powerpoint for visualization 

# Supplementary Structural equation model including average JJA air temperature (Supplementary)-------------
model <- '
  # Direct effects
  ice_on_doy ~ swt_fall_avg 
  avg_under_ice_temp ~ ice_on_doy 
  ice_off_doy ~ ice_on_doy
  swt_max_temp ~ ice_off_doy + avg_under_ice_temp + ice_on_doy + mean_temp_jja
  
#remove covarience 
swt_max_temp ~~ 0*mean_temp_jja

'

# Fit the Model
sem_fit <- sem(model, data = final_swt_assessment_1)

# Evaluate the Model
# View the model summary
summary(sem_fit, fit.measures = TRUE)


# Get the R2 values for all endogenous variables
r2_values <- inspect(sem_fit, "r2")
print(r2_values)

custom_labels <- c(
  "swt_max_temp" = "Max SWT",
  "ice_on_doy" = "Ice-On DOY",
  "avg_under_ice_temp" = "Under-Ice Temp",
  "ice_off_doy" = "Ice-off DOY",
  "mean_temp_jja" = "mean temp JJA",
  "swt_fall_avg" = "Fall SWT"
  
)

# Visualize the SEM model with larger text in boxes
semPaths(
  sem_fit, 
  what = "std",               # Standardized estimates
  layout = "spring",            # Choose layout type, e.g., "tree" for hierarchical view
  edge.label.cex = 1,         # Size of path coefficients
  sizeMan = 12,                # Size of variable nodes (box size)
  label.cex = 1.0,            # Size of the text inside the boxes
  fade = FALSE,               # Do not fade small paths
  asize = 2,                  # Arrow size
  residScale = 6) # Explicitly map custom labels

# NOTE: Supplementary model was also visualized using powerpoint in paper 

# Correlations between variables within the SEM (as seen in paper) ----
ice_off_max_temp_plot <- ggplot(final_merged_swt_df, aes(x = ice_off_doy, y = swt_max_temp)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "black", se = TRUE, fill = "gray80") +
  stat_cor(
    method = "spearman",
    label.x.npc = 0.75,   # Near the top-right, but inside
    label.y.npc = 0.98,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Ice-off day of year", 
    y = "Maximum summer surface water temperature (°C)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

plot(ice_off_max_temp_plot)


ice_on_ice_off_plot <- ggplot(final_merged_swt_df, aes(x = ice_on_doy, y = ice_off_doy)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "black", se = TRUE, fill = "gray80") +
  stat_cor(
    method = "spearman",
    label.x.npc = 0.75,   # Near the top-right, but inside
    label.y.npc = 0.98,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Ice-on day of year", 
    y = "Ice-off day of year"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

plot(ice_on_ice_off_plot)

ice_on_max_temp_plot <- ggplot(final_merged_swt_df, aes(x = ice_on_doy, y = swt_max_temp)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, 
              color = "black", se = TRUE, fill = "gray80") +
  stat_cor(
    method = "spearman",
    label.x.npc = 0.75,   # Near the top-right, but inside
    label.y.npc = 0.98,   # Near the top
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Ice-on day of year", 
    y = "Maximum summer surface water temperature (°C)"
  ) +
  theme_classic(base_size = 12) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 18),
    axis.text = element_text(size = 16)
  )

plot(ice_on_max_temp_plot)


# Make Pannel for cross-seasonal SEM in paper ----
panel_fig_2 <- (ice_off_max_temp_plot + ice_on_ice_off_plot + ice_on_max_temp_plot) + 
  plot_layout(ncol = 3) 

print(panel_fig_2)

# Save the final panel
ggsave("corr_panel.png", panel_fig_2, width = 20, height = 7, dpi = 300)