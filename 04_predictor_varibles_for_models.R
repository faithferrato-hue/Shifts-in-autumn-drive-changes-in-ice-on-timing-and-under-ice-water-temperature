-------------------------------------------------------------------
# Script: 04_predictor_varibles_for_models.R
# Author: Faith Ferrato
# Purpose: Combine ice-on, under-ice temperature, morphology, and climate variables
# to derive fall transition metrics and predictors for regression tree and SEM models
-------------------------------------------------------------------

# Load required libraries ----
library(dplyr)
library(lubridate)
library(zoo)
library(ggplot2)
library(corrplot)

# For hydrolakes data
library(sf)
library(readxl)
library(here)

# Load and prepare under-ice temperature and ice phenology data ----
# This data frame was curated in the script "02_under_ice_water_temperature_calculations.R" 
all_combined_data_under_ice_deep <- read.csv("C:/location/all_combined_data_under_ice_deep.csv", header = TRUE)

# Select the specified columns
all_combined_data_under_ice_deep <- all_combined_data_under_ice_deep %>%
  select(date, depth, temp_C, water_year, water_year_temp_date, ice_on, ice_off, lake)

# Mutating ice-on date to doy and summarizing average under-ice temperatures by ice season 
all_combined_data_under_ice_deep_cor <- all_combined_data_under_ice_deep %>%
  rowwise() %>%
  mutate(
    ice_on_doy = yday(as.Date(`ice_on`)),
    ice_off_doy = yday(as.Date(`ice_off`)),
    ice_on_doy = ifelse(
      month(as.Date(`ice_on`)) %in% 1:4, #adjust ice-on dates that fall in Jan–Apr
      ice_on_doy + ifelse(leap_year(year(as.Date(`ice_on`)) - 1), 366, 365),
      ice_on_doy
    )  # Adjust Julian date for ice-on
  ) %>%
  ungroup() %>%
  filter(date >= as.Date(`ice_on`) & date <= as.Date(`ice_off`)) %>%
  group_by(lake, water_year, ice_on, ice_off, ice_on_doy) %>%
  summarize(avg_under_ice_temp = mean(temp_C, na.rm = TRUE), .groups = "drop")  # Drop extra grouping

# View the final DataFrame
View(all_combined_data_under_ice_deep_cor)

# Ensure the lake column in all_combined_data_under_ice_deep_cor is of type character
all_combined_data_under_ice_deep_cor <- all_combined_data_under_ice_deep_cor %>%
  mutate(lake = as.character(lake))

# Rename lake IDs to descriptive names ----
# Create a mapping of lake names
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

# Replace the lake column with corresponding observation_station names 
# Keep the column name as "lake"
all_combined_data_under_ice_deep_cor <- all_combined_data_under_ice_deep_cor %>%
  left_join(lake_mapping, by = "lake") %>%  # Join to get the observation_station names
  mutate(lake = observation_station) %>%  # Update the lake column with observation_station names
  select(-observation_station)  # Remove the extra column if not needed

# Loading climate variables ----
# The following dataframe was created in the script "03_combined_climate_variables.R"
ui_climate_variable_data_cleaned <- read.csv("C:/location/ui_climate_variable_data_cleaned.csv", header = TRUE)

# Make water year column so that we can combine it to the previous df
ui_climate_variable_data_cleaned_cor <- ui_climate_variable_data_cleaned %>%
  mutate(
    date = as.Date(paste(year, month, day, sep = "-")),  # Combine year, month, day into a single date
    water_year = ifelse(month >= 10, year + 1, year)    # Assign water_year (shift year on Oct 1)
  )

# Renaming columns in dataframe to join together 
ui_climate_variable_data_cleaned_cor <- ui_climate_variable_data_cleaned_cor %>%
  rename(lake = observation_station)

# Combining both dataframes by lake and water year 
combined_data_final_boss <- all_combined_data_under_ice_deep_cor %>%
  left_join(ui_climate_variable_data_cleaned_cor, by = c("lake", "water_year"))


# Deriving fall fransition metrics ----
# Creating 31-day rolling average for mean and minimum temperature
ui_climate_variable_data_cleaned_cor <- ui_climate_variable_data_cleaned_cor %>%
  group_by(lake) %>%
  arrange(date) %>%
  mutate(
    rolling_mean_temp_c = rollmean(mean_temp_c, 31, fill = NA, align = "right"),
    rolling_mean_min_temp_c = rollmean(min_temp_c, 31, fill = NA, align = "right")
  ) %>%
  ungroup()

# 1. Fall start based on 31-day rolling mean temperature dropping below 4°C (for both min and mean air temp)
fall_start <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_temp_c < 4) %>%
  group_by(lake, water_year) %>%
  summarize(fall_start_date = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_start_doy = as.numeric(format(fall_start_date, "%j")))

fall_start_min <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_min_temp_c < 4) %>%
  group_by(lake, water_year) %>%
  summarize(fall_start_min_date = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_start_min_doy = as.numeric(format(fall_start_min_date, "%j")))

# 2. Fall isotherm based on 31-day rolling mean temperature dropping below 0°C (for both min and mean air temp)
fall_isotherm <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_temp_c < 0) %>%
  group_by(lake, water_year) %>%
  summarize(fall_isotherm_date = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_isotherm_doy = as.numeric(format(fall_isotherm_date, "%j")))

fall_isotherm_min <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_min_temp_c < 0) %>%
  group_by(lake, water_year) %>%
  summarize(fall_isotherm_min_date = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_isotherm_min_doy = as.numeric(format(fall_isotherm_min_date, "%j")))

# Create 15-day rolling average for mean and minimum air temperature
ui_climate_variable_data_cleaned_cor <- ui_climate_variable_data_cleaned_cor %>%
  group_by(lake) %>%
  arrange(date) %>%
  mutate(
    rolling_mean_temp_c_15 = rollmean(mean_temp_c, 15, fill = NA, align = "right"),
    rolling_mean_min_temp_c_15 = rollmean(min_temp_c, 15, fill = NA, align = "right")
  ) %>%
  ungroup()

# 3. Fall start based on 15-day rolling mean temperature dropping below 4°C (for both min and mean air temp)
fall_start_15 <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_temp_c_15 < 4) %>%
  group_by(lake, water_year) %>%
  summarize(fall_start_date_15 = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_start_doy_15 = as.numeric(format(fall_start_date_15, "%j")))

fall_start_min_15 <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_min_temp_c_15 < 4) %>%
  group_by(lake, water_year) %>%
  summarize(fall_start_min_date_15 = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_start_min_doy_15 = as.numeric(format(fall_start_min_date_15, "%j")))

# 4. Fall isotherm based on 15-day rolling mean temperature dropping below 0°C (for both min and mean air temp)
fall_isotherm_15 <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_temp_c_15 < 0) %>%
  group_by(lake, water_year) %>%
  summarize(fall_isotherm_date_15 = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_isotherm_doy_15 = as.numeric(format(fall_isotherm_date_15, "%j")))

fall_isotherm_min_15 <- ui_climate_variable_data_cleaned_cor %>%
  filter(rolling_mean_min_temp_c_15 < 0) %>%
  group_by(lake, water_year) %>%
  summarize(fall_isotherm_min_date_15 = min(date, na.rm = TRUE), .groups = "drop") %>%
  mutate(fall_isotherm_min_doy_15 = as.numeric(format(fall_isotherm_min_date_15, "%j")))

# Combine all calculated variables into the final dataset
final_data_boss <- combined_data_final_boss %>%
  # Merge fall start and isotherm for 31-day rolling mean
  left_join(fall_start, by = c("lake", "water_year")) %>%
  left_join(fall_isotherm, by = c("lake", "water_year")) %>%
  left_join(fall_start_min, by = c("lake", "water_year")) %>%
  left_join(fall_isotherm_min, by = c("lake", "water_year")) %>%
  # Merge fall start and isotherm for 15-day rolling mean
  left_join(fall_start_15, by = c("lake", "water_year")) %>%
  left_join(fall_isotherm_15, by = c("lake", "water_year")) %>%
  left_join(fall_start_min_15, by = c("lake", "water_year")) %>%
  left_join(fall_isotherm_min_15, by = c("lake", "water_year")) %>%
  # Adjust isotherm DOY for January–April dates (account for previous year)- eg. if fall isotherm date is January 1st (365+1) so we can calculate length of fall
  mutate(
    fall_isotherm_doy_adjusted = case_when(
      month(fall_isotherm_date) %in% 1:4 ~ fall_isotherm_doy + ifelse(leap_year(water_year - 1), 366, 365),
      TRUE ~ fall_isotherm_doy
    ),
    fall_isotherm_min_doy_adjusted = case_when(
      month(fall_isotherm_min_date) %in% 1:4 ~ fall_isotherm_min_doy + ifelse(leap_year(water_year - 1), 366, 365),
      TRUE ~ fall_isotherm_min_doy
    ),
    fall_isotherm_doy_15_adjusted = case_when(
      month(fall_isotherm_date_15) %in% 1:4 ~ fall_isotherm_doy_15 + ifelse(leap_year(water_year - 1), 366, 365),
      TRUE ~ fall_isotherm_doy_15
    ),
    fall_isotherm_min_doy_15_adjusted = case_when(
      month(fall_isotherm_min_date_15) %in% 1:4 ~ fall_isotherm_min_doy_15 + ifelse(leap_year(water_year - 1), 366, 365),
      TRUE ~ fall_isotherm_min_doy_15
    ),
    # Calculate fall lengths (subtracting fall start from ice-on doy)
    fall_length = ice_on_doy - fall_start_doy,
    fall_length_min = ice_on_doy - fall_start_min_doy,
    fall_iso_length = ice_on_doy - fall_isotherm_doy_adjusted,
    fall_iso_length_min = ice_on_doy - fall_isotherm_min_doy_adjusted,
    fall_length_15 = ice_on_doy - fall_start_doy_15,
    fall_length_min_15 = ice_on_doy - fall_start_min_doy_15,
    fall_iso_length_15 = ice_on_doy - fall_isotherm_doy_15_adjusted,
    fall_iso_length_min_15 = ice_on_doy - fall_isotherm_min_doy_15_adjusted
  )

# Print final dataset
print(final_data_boss)

# Filter out the first water year (1970) because missing data
final_data_boss <- final_data_boss %>%
  filter(water_year > 1970)

# Make as.date 
final_data_boss <- final_data_boss %>%
  mutate(ice_on = as.Date(ice_on))

# Create final climate predictors ----
# For OND, december, 2 weeks, and 1 month before freeze-up  
final_data_boss <- final_data_boss %>%
  group_by(lake, water_year) %>%
  mutate(
    # 2 weeks before ice-on (14 days before)
    mean_temp_2week = mean(mean_temp_c[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    wind_speed_2week = mean(wind_speed_ms[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    precipitation_2week = mean(precipitation_mm[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    snow_depth_2week = mean(snow_depth_mm[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    min_temp_2week = mean(min_temp_c[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    humidity_2week = mean(avg_rel_humidity_percent[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    shortwave_2week = mean(downward_shortwave_radiation_w_m2[date >= (ice_on - days(14)) & date < ice_on], na.rm = TRUE),
    
    # 1 month before ice-on (30 days before) using add_with_rollback()
    start_date_1month = add_with_rollback(ice_on, months(-1)),
    mean_temp_1month = mean(mean_temp_c[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    wind_speed_1month = mean(wind_speed_ms[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    precipitation_1month = mean(precipitation_mm[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    snow_depth_1month = mean(snow_depth_mm[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    min_temp_1month = mean(min_temp_c[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    humidity_1month = mean(avg_rel_humidity_percent[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    shortwave_1month = mean(downward_shortwave_radiation_w_m2[date >= start_date_1month & date < ice_on], na.rm = TRUE),
    
    # December of each water year
    mean_temp_dec = mean(mean_temp_c[month == 12], na.rm = TRUE),
    wind_speed_dec = mean(wind_speed_ms[month == 12], na.rm = TRUE),
    precipitation_dec = mean(precipitation_mm[month == 12], na.rm = TRUE),
    snow_depth_dec = mean(snow_depth_mm[month == 12], na.rm = TRUE),
    min_temp_dec = mean(min_temp_c[month == 12], na.rm = TRUE),
    humidity_dec = mean(avg_rel_humidity_percent[month == 12], na.rm = TRUE),
    shortwave_dec = mean(downward_shortwave_radiation_w_m2[month == 12], na.rm = TRUE),
    
    # October-November-December (OND)
    mean_temp_ond = mean(mean_temp_c[month %in% c(10, 11, 12)], na.rm = TRUE),
    wind_speed_ond = mean(wind_speed_ms[month %in% c(10, 11, 12)], na.rm = TRUE),
    precipitation_ond = mean(precipitation_mm[month %in% c(10, 11, 12)], na.rm = TRUE),
    snow_depth_ond = mean(snow_depth_mm[month %in% c(10, 11, 12)], na.rm = TRUE),
    min_temp_ond = mean(min_temp_c[month %in% c(10, 11, 12)], na.rm = TRUE),
    humidity_ond = mean(avg_rel_humidity_percent[month %in% c(10, 11, 12)], na.rm = TRUE),
    shortwave_ond = mean(downward_shortwave_radiation_w_m2[month %in% c(10, 11, 12)], na.rm = TRUE)
  ) %>%
  ungroup() %>%
  select(lake, water_year, ice_on_doy, avg_under_ice_temp, fall_length, fall_iso_length, fall_length_15, fall_iso_length_15, fall_length_min, fall_iso_length_min, fall_length_min_15, fall_iso_length_min_15, date, starts_with("mean_temp"), starts_with("wind_speed"), starts_with("precipitation"), starts_with("snow_depth"), starts_with("min_temp"), starts_with("humidity"), starts_with("shortwave"))

# Now print the updated dataframe
print(final_data_boss)

# Now remove origional daily variables 
final_data_boss <- final_data_boss %>%
  select(-date,                  # Remove the date column
         -mean_temp_c,           # Remove the original mean_temp_c column
         -min_temp_c,            # Remove the original min_temp_c column
         -wind_speed_ms,         # Remove the original wind_speed_ms column
         -precipitation_mm,      # Remove the original precipitation_mm column
         -snow_depth_mm,         # Remove the original snow_depth_mm column
  ) 

# Remove duplicate rows (if nessesary)
final_data_boss <- final_data_boss %>%
  distinct()

# Check final dataframe after removing duplicate rows
print(final_data_boss)


# Read and prepare HydroLAKES shapefile ----

# Load the libraries
library(sf)
library(dplyr)
library(readxl)
library(here)

# Shapefile was downloaded from "https://www.hydrosheds.org/products/hydrolakes"
# Read the shape file (replace with your actual shape file path)
shapefile <- st_read("C:/location/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10_shp/HydroLAKES_polys_v10.shp")

# Filtering for Finland
shapefile <- shapefile %>% filter(Country %in% "Finland") 

# Making the boarders valid 
shapefile <- st_make_valid(shapefile)

# Convert Hylak_id in shapefile to character
shapefile <- shapefile %>%
  mutate(Hylak_id = as.character(Hylak_id))

# Identify which study sites fall within HydroLAKES polygons
# (Optional) Create an sf object from your lake coordinate data, assuming WGS84 coordinate system (EPSG:4326)
# coords_df should include columns: latitude, longitude, and lake name
coords_sf <- st_as_sf(coords_df, coords = c("longitude", "latitude"), crs = 4326)

# Perform a spatial join to attach polygon attributes to the points
# Set `left = TRUE` to keep all points, even if they do not fall inside a polygon
joined_sf <- st_join(coords_sf, shapefile, join = st_within)

hydrolakes_df <- joined_sf
view(hydrolakes_df)

# Check lakes to identify if they were assigned correctly to polygons

# Note:
# In this case, some lakes were not matched automatically because their
# coordinates (e.g., sampling sites near shorelines) fell
# slightly outside the polygon boundaries. These were verified
# manually using a GIS viewer, and the correct Hylak_id values
# were assigned in an external Excel file for accuracy.

# Import verified coordinate and Hylak ID table (if necessary)
# Data frame of latitude and longitude points for each of my lakes, with hylak ids
coords_df  <- readxl::read_excel(here("C:/loaction/lake_cordinate_and_hylak_id.xlsx"), sheet = 4) %>% select('Site Name','latitude','longitude', 'Hylak_id')  

# Join HydroLAKES attributes to verified lake list
combined_hylak_id <- coords_df %>%
  left_join(shapefile, by = "Hylak_id")

# View the combined data frame
View(combined_hylak_id)

# Dropping geometry before downloading 
combined_hylak_id_clean <- combined_hylak_id %>%
  select(-geometry)

View(combined_hylak_id_clean)

# Download 
write.csv(
  combined_hylak_id_clean,
  file = "C:/location/all_hylak_id_and_morphology.csv",
  row.names = FALSE
)

Combined_hylak_id <- read.csv("C:/location/all_hylak_id_and_morphology.csv", header = TRUE)

# Selecting for rows 50 to 58, corresponding with under-ice water temperature sites.
ui_hylak_id <- combined_hylak_id %>%
  slice(50:58)

# Rename "Pielinen Nurmes" to "Pielinen Nurmes_ui" under the "site name" column
# Rename the "site name" column to "lake"
# Select the specific columns
# Add it to the final dataframe by "lake"

ui_hylak_id <- ui_hylak_id %>%
  mutate(`Site Name` = if_else(`Site Name` == "Pielinen Nurmes", "Pielinen Nurmes_ui", `Site Name`)) %>%
  mutate(`Site Name` = if_else(`Site Name` == "Konneves Nareselka", "Konnevesi Nareselka", `Site Name`)) %>% # spelling mistake in previous df
  rename(lake = `Site Name`) %>%
  select(lake, latitude, longitude, Lake_area, Shore_len, Vol_total, Depth_avg, Elevation)

# Now join with the final boss dataframe
final_data_boss <- final_data_boss %>%
  left_join(ui_hylak_id, by = "lake")

# View the updated final dataframe
print(final_data_boss)

# Check distributions of ALL variables ----
data <- final_data_boss

# List of numeric columns to plot
numeric_columns <- c("ice_on_doy", "avg_under_ice_temp", "fall_length_15", "fall_length_min_15", "fall_length", 
                     "mean_temp_2week", "mean_temp_1month", "mean_temp_dec", "mean_temp_ond", 
                     "wind_speed_2week", "wind_speed_1month", "wind_speed_dec", "wind_speed_ond", 
                     "precipitation_2week", "precipitation_1month", "precipitation_dec", "precipitation_ond", 
                     "snow_depth_2week", "snow_depth_1month", "snow_depth_dec", "snow_depth_ond", 
                     "min_temp_2week", "min_temp_1month", "min_temp_dec", "min_temp_ond", 
                     "humidity_2week", "humidity_1month", "humidity_dec", "humidity_ond", 
                     "shortwave_2week", "shortwave_1month", "shortwave_dec", "shortwave_ond", 
                     "Lake_area", "Shore_len", "Vol_total", "Depth_avg", "Elevation")

# Filter numeric columns dynamically to exclude invalid ones
valid_numeric_columns <- numeric_columns[sapply(data[numeric_columns], function(x) {
  is.numeric(x) && !all(is.na(x))
})]

# Loop through valid numeric columns and plot histograms
for (col in valid_numeric_columns) {
  print(
    ggplot(data, aes(x = .data[[col]])) +
      geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +
      labs(title = paste("Distribution of", col), x = col, y = "Frequency") +
      theme_minimal()
  )
}

# Log transform variables that are skewed for regression tree ----
regression_tree_data <- final_data_boss

# List of variables to log-transform
vars_to_log <- c("shortwave_1month", "shortwave_2week", "snow_depth_1month", "snow_depth_2week")

# Add log-transformed versions of the variables to the dataset
regression_tree_data <- regression_tree_data %>%
  mutate(across(all_of(vars_to_log), ~ log(. + 1), .names = "log_{.col}"))

#plot new log columns to check 
log_vars <- paste0("log_", vars_to_log)

for (col in log_vars) {
  print(
    ggplot(regression_tree_data, aes(x = .data[[col]])) +
      geom_histogram(binwidth = 0.1, fill = "green", color = "black", alpha = 0.7) +
      labs(title = paste("Log-Transformed Distribution of", col), x = col, y = "Frequency") +
      theme_minimal()
  )
}

# Save final data frame for regression tree as well as SEM ----
# This df was used for analysis in scripts "05_regression_tree_model_and_maps.R" and "06_structural_equation_modeling_under_ice_drivers"
write.csv(
  regression_tree_data,
  file = "C:/location/regression_tree_data.csv",
  row.names = FALSE
)

# Correlation matrix to look at simple relationships before regression tree and SEM ----
# Identifying simile relationships present 

# Load necessary libraries
library(corrplot)
library(ggplot2)
library(dplyr)

# Select the relevant columns for correlation analysis
correlation_data <- regression_tree_data %>%
  select(avg_under_ice_temp, 
         ice_on_doy, fall_length_min_15, fall_length, 
         mean_temp_2week, mean_temp_1month, mean_temp_dec, mean_temp_ond, 
         wind_speed_2week, wind_speed_1month, wind_speed_dec, wind_speed_ond, 
         precipitation_2week, precipitation_1month, precipitation_dec, precipitation_ond, 
         log_snow_depth_2week, log_snow_depth_1month, snow_depth_dec, snow_depth_ond, 
         min_temp_2week, min_temp_1month, min_temp_dec, min_temp_ond, 
         humidity_2week, humidity_1month, humidity_dec, humidity_ond, 
         log_shortwave_2week, log_shortwave_1month, shortwave_dec, shortwave_ond, 
         Lake_area, Shore_len, Vol_total, Depth_avg, Elevation, latitude)


# Calculate Spearman correlation matrix
cor_matrix <- cor(correlation_data, method = "spearman", use = "complete.obs")

# Extract the first row (correlation with avg_under_ice_temp)
cor_first_row <- cor_matrix[1, ]

# Convert to data frame for easier viewing
cor_first_row_df <- data.frame(Variable = names(cor_first_row), Correlation = cor_first_row)

# Print the correlation data for better readability
print(cor_first_row_df)

# Visualize the first row of the correlation matrix (with avg_under_ice_temp)
ggplot(cor_first_row_df, aes(x = reorder(Variable, Correlation), y = Correlation)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +  # Flip the plot for better readability
  labs(title = "Correlation with avg_under_ice_temp", x = "Variable", y = "Spearman Correlation") +
  theme_minimal()
