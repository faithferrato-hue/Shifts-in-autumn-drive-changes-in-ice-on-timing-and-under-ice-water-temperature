-------------------------------------------------------------------
  # Script: 03_combined_climate_variables.R
  # Author: Faith Ferrato
  # Purpose: cleaning and combining climate variables associated with Under-ice water temperature sites into one dataframe
-------------------------------------------------------------------
# NOTE: all files were downloaded from climate engine as ERA 5 data (11km)
# They were renamed for the purpose of joining files  

  # Clean climate variables 
# Packages ------------
library(dplyr)
library(readr)
library(here)
library(readxl)
library(janitor)

# Solar rad data (W/m2) -------------------------

# Set the working directory
# Change to actual location of files
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Print the list of files to verify
print("Files in the directory:")
print(file_list)

# Initialize a list to store merged data frames
merged_data_list <- list()

# In this case, file sizes were too large to and there were multiple files that needed to be downloaded for a single sites
# So in the section below, we are merging the 2 seperate files with the same base names so they are combined

# Loop through unique base names (removing the suffix, but keeping .csv)
unique_basenames <- unique(gsub(" \\(\\d\\)\\.csv$", "", file_list))

for (basename in unique_basenames) {
  # Construct filenames for (1) and (2)
  file1 <- paste0(basename, " (1).csv")
  file2 <- paste0(basename, " (2).csv")
  
  # Print constructed file names for debugging
  print(paste("Checking for files:", file1, "and", file2))
  
  # Check if both files exist
  if (file1 %in% file_list && file2 %in% file_list) {
    # Read the files
    data1 <- read_csv(file1)
    data2 <- read_csv(file2)
    
    # Standardize the column names
    colnames(data1)[2] <- "Downward_Shortwave_Radiation_W_m2"
    colnames(data2)[2] <- "Downward_Shortwave_Radiation_W_m2"
    
    # Merge the data frames (assuming they have the same structure)
    merged_data <- bind_rows(data1, data2)
    
    # Convert the first column (date) to Date type
    merged_data[[1]] <- as.Date(merged_data[[1]])
    
    # Arrange the data by date in ascending order
    merged_data <- merged_data %>% arrange(.[[1]])
    
    # Add a new column for the Observation station name (using the basename)
    merged_data <- merged_data %>% mutate(Observation_station = basename)
    
    # Store the merged and sorted data in the list
    merged_data_list[[basename]] <- merged_data
  }
}

# Combine all the merged data frames into one large data frame
combined_rad_data <- bind_rows(merged_data_list)

# Convert date column to Year, Month, Day
combined_rad_data <- combined_rad_data %>%
  mutate(Year = lubridate::year(...1),
         Month = lubridate::month(...1),
         Day = lubridate::day(...1)) %>%
  select(-...1)  # Drop the original date column

# Get unique station names from the final dataframe (ensuring all stations are present)
unique_stations <- unique(combined_rad_data$`Observation_station`)

# Count of unique station names
print(unique_stations)

# Daily air temperature (C) ------------------------------------------------- 
# Repeat steps for all variables 

# Set the working directory
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Print the list of files to verify
print("Files in the directory:")
print(file_list)

# Initialize a list to store merged data frames
merged_data_list <- list()

# Loop through unique base names (removing the suffix, but keeping .csv)
unique_basenames <- unique(gsub(" \\(\\d\\)\\.csv$", "", file_list))

for (basename in unique_basenames) {
  # Construct filenames for (1) and (2)
  file1 <- paste0(basename, " (1).csv")
  file2 <- paste0(basename, " (2).csv")
  
  # Print constructed file names for debugging
  print(paste("Checking for files:", file1, "and", file2))
  
  # Check if both files exist
  if (file1 %in% file_list && file2 %in% file_list) {
    # Read the files
    data1 <- read_csv(file1)
    data2 <- read_csv(file2)
    
    # Standardize the column names
    colnames(data1)[2] <- "mean_temp_C"
    colnames(data2)[2] <- "mean_temp_C"
    
    # Merge the data frames (assuming they have the same structure)
    merged_data <- bind_rows(data1, data2)
    
    # Convert the first column (date) to Date type
    merged_data[[1]] <- as.Date(merged_data[[1]])
    
    # Arrange the data by date in ascending order
    merged_data <- merged_data %>% arrange(.[[1]])
    
    # Add a new column for the Observation station name (using the basename)
    merged_data <- merged_data %>% mutate(Observation_station = basename)
    
    # Store the merged and sorted data in the list
    merged_data_list[[basename]] <- merged_data
  }
}

# Combine all the merged data frames into one large data frame
combined_air_temp_data <- bind_rows(merged_data_list)

# Convert date to Year, Month, Day
combined_air_temp_data <- combined_air_temp_data %>%
  mutate(Year = lubridate::year(...1),
         Month = lubridate::month(...1),
         Day = lubridate::day(...1)) %>%
  select(-...1)  # Drop the original date column

# Get unique station names from the final dataframe
unique_stations <- unique(combined_air_temp_data$`Observation_station`)

# Count of unique station names
print(unique_stations)

# This specific dataframe was downloaded and used in analysis in 
# file "02_under_ice_water_temperature_calculations.R" 
# Change location to specified file location 
write.csv(
  combined_air_temp_data,
  file = "C:/location/combined_air_temp_data.csv",
  row.names = FALSE
)

# Minimum daily air temperature (C) ------------------------------------------------

# Set the working directory
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Print the list of files to verify
print("Files in the directory:")
print(file_list)

# Initialize a list to store merged data frames
merged_data_list <- list()

# Loop through unique base names (removing the suffix, but keeping .csv)
unique_basenames <- unique(gsub(" \\(\\d\\)\\.csv$", "", file_list))

for (basename in unique_basenames) {
  # Construct filenames for (1) and (2)
  file1 <- paste0(basename, " (1).csv")
  file2 <- paste0(basename, " (2).csv")
  
  # Print constructed file names for debugging
  print(paste("Checking for files:", file1, "and", file2))
  
  # Check if both files exist
  if (file1 %in% file_list && file2 %in% file_list) {
    # Read the files
    data1 <- read_csv(file1)
    data2 <- read_csv(file2)
    
    # Standardize the column names
    colnames(data1)[2] <- "min_temp_C"
    colnames(data2)[2] <- "min_temp_C"
    
    # Merge the data frames (assuming they have the same structure)
    merged_data <- bind_rows(data1, data2)
    
    # Convert the first column (date) to Date type
    merged_data[[1]] <- as.Date(merged_data[[1]])
    
    # Arrange the data by date in ascending order
    merged_data <- merged_data %>% arrange(.[[1]])
    
    # Add a new column for the Observation station name (using the basename)
    merged_data <- merged_data %>% mutate(Observation_station = basename)
    
    # Store the merged and sorted data in the list
    merged_data_list[[basename]] <- merged_data
  }
}

# Combine all the merged data frames into one large data frame
combined_min_air_temp_data <- bind_rows(merged_data_list)

# Convert date to Year, Month, Day
combined_min_air_temp_data <- combined_min_air_temp_data %>%
  mutate(Year = lubridate::year(...1),
         Month = lubridate::month(...1),
         Day = lubridate::day(...1)) %>%
  select(-...1)  # Drop the original date column

# Get unique station names from the final dataframe
unique_stations <- unique(combined_min_air_temp_data$`Observation_station`)

# Count of unique station names
print(unique_stations)


# Daily precipitation (mm) ---------------------------------------------------------

# Set the working directory
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Print the list of files to verify
print("Files in the directory:")
print(file_list)

# Initialize a list to store merged data frames
merged_data_list <- list()

# Loop through unique base names (removing the suffix, but keeping .csv)
unique_basenames <- unique(gsub(" \\(\\d\\)\\.csv$", "", file_list))

for (basename in unique_basenames) {
  # Construct filenames for (1) and (2)
  file1 <- paste0(basename, " (1).csv")
  file2 <- paste0(basename, " (2).csv")
  
  # Print constructed file names for debugging
  print(paste("Checking for files:", file1, "and", file2))
  
  # Check if both files exist
  if (file1 %in% file_list && file2 %in% file_list) {
    # Read the files
    data1 <- read_csv(file1)
    data2 <- read_csv(file2)
    
    # Standardize the column names
    colnames(data1)[2] <- "precipitation_mm"
    colnames(data2)[2] <- "precipitation_mm"
    
    # Merge the data frames (assuming they have the same structure)
    merged_data <- bind_rows(data1, data2)
    
    # Convert the first column (date) to Date type
    merged_data[[1]] <- as.Date(merged_data[[1]])
    
    # Arrange the data by date in ascending order
    merged_data <- merged_data %>% arrange(.[[1]])
    
    # Add a new column for the Observation station name (using the basename)
    merged_data <- merged_data %>% mutate(Observation_station = basename)
    
    # Store the merged and sorted data in the list
    merged_data_list[[basename]] <- merged_data
  }
}

# Combine all the merged data frames into one large data frame
combined_precipitation_data <- bind_rows(merged_data_list)

# Convert ...1 to Year, Month, Day
combined_precipitation_data <- combined_precipitation_data %>%
  mutate(Year = lubridate::year(...1),
         Month = lubridate::month(...1),
         Day = lubridate::day(...1)) %>%
  select(-...1)  # Drop the original date column

# Get unique station names from the final dataframe
unique_stations <- unique(combined_precipitation_data$`Observation_station`)

# Count of unique station names
print(unique_stations)

# Hourly humidity data (%) ----------------------------------------

# Set the working directory to where the air temperature data files are located
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.xlsx$")

# Initialize a list to store cleaned and merged data frames
merged_data_list <- list()

# Loop through each file and clean the column names
for (file in file_list) {
  # Read each file
  data <- read_excel(file)
  
  # Clean the column names for consistency
  clean_colnames <- c("Observation_station", "Year", "Month", "Day", "Time_Local", "average_relative_humidity_percent")
  colnames(data) <- clean_colnames
  
  # Add the file's name as a column "Observation_station" if it's not already present
  if (!"Observation_station" %in% colnames(data)) {
    data <- data %>% mutate(Observation_station = gsub("\\.xlsx$", "", file))  # Remove .xlsx from filename
  }
  
  # Store the cleaned data in the list
  merged_data_list[[file]] <- data
}

# Combine all cleaned data frames into one large data frame
combined_humidity_data <- bind_rows(merged_data_list)

# Aggregating humidity data to daily average
combined_humidity_data <- combined_humidity_data %>%
  group_by(Observation_station, Year, Month, Day) %>%
  summarise(avg_rel_humidity_percent = mean(average_relative_humidity_percent, na.rm = TRUE))

# Get unique station names from the final dataframe
unique_stations <- unique(combined_humidity_data$`Observation_station`)

# Count of unique station names
print(unique_stations)

# Wind speed (m/s) ---------------------------------------------------

# Set the working directory
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Print the list of files to verify
print("Files in the directory:")
print(file_list)

# Initialize a list to store merged data frames
merged_data_list <- list()

# Loop through unique base names (removing the suffix, but keeping .csv)
unique_basenames <- unique(gsub(" \\(\\d\\)\\.csv$", "", file_list))

for (basename in unique_basenames) {
  # Construct filenames for (1) and (2)
  file1 <- paste0(basename, " (1).csv")
  file2 <- paste0(basename, " (2).csv")
  
  # Print constructed file names for debugging
  print(paste("Checking for files:", file1, "and", file2))
  
  # Check if both files exist
  if (file1 %in% file_list && file2 %in% file_list) {
    # Read the files
    data1 <- read_csv(file1)
    data2 <- read_csv(file2)
    
    # Standardize the column names
    colnames(data1)[2] <- "wind_speed_ms"
    colnames(data2)[2] <- "wind_speed_ms"
    
    # Merge the data frames (assuming they have the same structure)
    merged_data <- bind_rows(data1, data2)
    
    # Convert the first column (date) to Date type
    merged_data[[1]] <- as.Date(merged_data[[1]])
    
    # Arrange the data by date in ascending order
    merged_data <- merged_data %>% arrange(.[[1]])
    
    # Add a new column for the Observation station name (using the basename)
    merged_data <- merged_data %>% mutate(Observation_station = basename)
    
    # Store the merged and sorted data in the list
    merged_data_list[[basename]] <- merged_data
  }
}

# Combine all the merged data frames into one large data frame
combined_wind_speed_data <- bind_rows(merged_data_list)

# Convert date to Year, Month, Day
combined_wind_speed_data <- combined_wind_speed_data %>%
  mutate(Year = lubridate::year(...1),
         Month = lubridate::month(...1),
         Day = lubridate::day(...1)) %>%
  select(-...1)  # Drop the original date column

# Get unique station names from the final dataframe
unique_stations <- unique(combined_wind_speed_data$`Observation_station`)

# Count of unique station names
print(unique_stations)

# Snow depth (mm) ------------------------------------------------

# Set the working directory
setwd("C:/location_of_files")

# List all files in the directory
file_list <- list.files(pattern = "\\.csv$")

# Print the list of files to verify
print("Files in the directory:")
print(file_list)

# Initialize a list to store merged data frames
merged_data_list <- list()

# Loop through unique base names (removing the suffix, but keeping .csv)
unique_basenames <- unique(gsub(" \\(\\d\\)\\.csv$", "", file_list))

for (basename in unique_basenames) {
  # Construct filenames for (1) and (2)
  file1 <- paste0(basename, " (1).csv")
  file2 <- paste0(basename, " (2).csv")
  
  # Print constructed file names for debugging
  print(paste("Checking for files:", file1, "and", file2))
  
  # Check if both files exist
  if (file1 %in% file_list && file2 %in% file_list) {
    # Read the files
    data1 <- read_csv(file1)
    data2 <- read_csv(file2)
    
    # Standardize the column names
    colnames(data1)[2] <- "snow_depth_mm"
    colnames(data2)[2] <- "snow_depth_mm"
    
    # Merge the data frames (assuming they have the same structure)
    merged_data <- bind_rows(data1, data2)
    
    # Convert the first column (date) to Date type
    merged_data[[1]] <- as.Date(merged_data[[1]])
    
    # Arrange the data by date in ascending order
    merged_data <- merged_data %>% arrange(.[[1]])
    
    # Add a new column for the Observation station name (using the basename)
    merged_data <- merged_data %>% mutate(Observation_station = basename)
    
    # Store the merged and sorted data in the list
    merged_data_list[[basename]] <- merged_data
  }
}

# Combine all the merged data frames into one large data frame
combined_snow_depth_data <- bind_rows(merged_data_list)

# Convert date to Year, Month, Day
combined_snow_depth_data <- combined_snow_depth_data %>%
  mutate(Year = lubridate::year(...1),
         Month = lubridate::month(...1),
         Day = lubridate::day(...1)) %>%
  select(-...1)  # Drop the original date column

# Get unique station names from the final dataframe
unique_stations <- unique(combined_snow_depth_data$`Observation_station`)

# Count of unique station names
print(unique_stations)

# Combine all climate variables into master df ---------------------------------------------

# Perform a full join on all data frames by Observation_station, Year, Month, Day
ui_climate_variable_data <- combined_air_temp_data %>%
  full_join(combined_wind_speed_data, by = c("Observation_station", "Year", "Month", "Day")) %>%
  full_join(combined_snow_depth_data, by = c("Observation_station", "Year", "Month", "Day")) %>%
  full_join(combined_min_air_temp_data, by = c("Observation_station", "Year", "Month", "Day")) %>%
  full_join(combined_humidity_data, by = c("Observation_station", "Year", "Month", "Day")) %>%
  full_join(combined_precipitation_data, by = c("Observation_station", "Year", "Month", "Day")) %>%
  full_join(combined_rad_data, by = c("Observation_station", "Year", "Month", "Day"))

# Clean the column names in ice_climate_variable_data_cleaned
ui_climate_variable_data_cleaned <- ui_climate_variable_data %>%
  clean_names()

# Get unique station names from the final dataframe
unique_stations <- unique(ui_climate_variable_data_cleaned$`observation_station`)

# Count of unique station names (ensuring everything joined properly)
print(unique_stations)

# Remove duplicate dates from merging 
ui_climate_variable_data_cleaned <- distinct(ui_climate_variable_data_cleaned)
print(ui_climate_variable_data_cleaned)


# Check data points - using air temperature to check if there are 365 observation in each year 
# Count non-missing data points for average_temperature_c by observation station and year
data_points_per_year <- ui_climate_variable_data_cleaned %>%
  group_by(observation_station, year) %>%
  summarise(N = sum(!is.na(mean_temp_c)), .groups = 'drop')

# View the results
print(data_points_per_year)


# Save final climate variable dataframe for under-ice water temperature sites -----------------------
# This df was used in script "04_predictor_varibles_for_models.R"

write.csv(
  ui_climate_variable_data_cleaned,
  file = "C:/location/ui_climate_variable_data_cleaned.csv",
  row.names = FALSE
)
