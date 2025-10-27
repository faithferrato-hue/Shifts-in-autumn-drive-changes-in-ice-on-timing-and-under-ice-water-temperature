-------------------------------------------------------------------
  # Script: 02_under_ice_water_temperature_calculations.R
  # Author: Faith Ferrato
  # Purpose: cleaning and calculating under-ice bottom water temperature averages
  # calculating general statistics, sen'slope, anomaly and breakpoint analysis for under-ice bottom water temperature
-------------------------------------------------------------------
# Load Required Packages ----
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(janitor)
library(readxl)

# Process ice data and under-ice data manually by lake ----

# Overall Goal:
# Combine ice phenology data with under-ice water temperature data 
# to calculate winter average temperatures for the period when the lake is frozen
# The deepest consistent temperature measurement for each lake is used for analysis

# Detailed annotations given for first lake bellow
# Steps were then repeated for remaining lakes

# Lake 1 (deepest consistent measurement = 44 (max= 48))----------------

# 1. Process ice data ---------- 
# Loading ice data
# change "'C:/location/.xlsx'" to the location and name of file  
kallavesi_kuopio <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 5) %>% #location of lake data on sheet
  clean_names() %>% 
  select(ib_0407920, lippu) 

# Renaming date column and updating date format
names(kallavesi_kuopio)[names(kallavesi_kuopio) == "ib_0407920"] <- "date" 
kallavesi_kuopio$date <- as.Date(kallavesi_kuopio$date, format = "%Y%m%d")
kallavesi_kuopio$date <- format(kallavesi_kuopio$date, "%Y-%m-%d") 

# Convert date column to as.date 
kallavesi_kuopio$date <- as.Date(kallavesi_kuopio$date, format = "%Y-%m-%d")

# Add a column for year
kallavesi_kuopio$year <- format(kallavesi_kuopio$date, "%Y")

# 1.a Define water year: begining October 1st
# This groups the entire winter season
# even when lakes freeze in January, they are classified as the same year/season. 
kallavesi_kuopio <- kallavesi_kuopio %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# 1.b Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
# Extract ice-on date
ice_on_data1 <- kallavesi_kuopio %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

# Extract ice-off
ice_off_data1 <- kallavesi_kuopio %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# 1.c Combine ice phenology into one df
ice_data_wide1 <- full_join(ice_on_data1, ice_off_data1, by = "water_year") %>%
  arrange(water_year)

# Keep only years where ice-on and ice-off dates are present
ice_data_wide1 <- ice_data_wide1 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# 2. Load and process under-ice water temperature data ------- 
# Depths were chosen based on most consistent deep under-ice water temperature measurements
# Change "C:/location/Syke TS 1900-2022.xlsx" to the location and file name to match your file 
df1 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 2) %>% #sheet associated with specific lake data
  clean_names() 

# Create a vector of clearer column names corresponding to the 42 columns in df1
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m', 'ts_21m', 'ts_22m', 'ts_24m', 'ts_25m', 'ts_26m', 'ts_28m', 'ts_30m', 'ts_32m', 'ts_34m', 'ts_36m', 'ts_38m', 'ts_40m', 'ts_42m', 'ts_44m', 'ts_45m', 'ts_46m', 'ts_48m') 

# Clean the dataframe: select first 42 cols, apply new names, and add lake factor
df1_clean <- df1 %>%
  select(1:42) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(0428100)
  )

# 2a. Isolate data for specific depth
# Count non-NA observations in each column of df1_clean (identifying most consitant depth measurment overtime)
obs_counts <- df1_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting for specific depth (in this case, 44m) 
# Then convert to long format
kallavesi_sayneensalo_44m <- df1_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_44m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Enure Date is in correct format
kallavesi_sayneensalo_44m$date <- as.Date(kallavesi_sayneensalo_44m$date)

# Filter out years with missing water temperature data
kallavesi_sayneensalo_44m_filtered <- kallavesi_sayneensalo_44m %>%
  filter(!is.na(temp_C))

# 2.b Assign water year to water temperature data (to join dfs)
kallavesi_sayneensalo_44m_filtered <- kallavesi_sayneensalo_44m_filtered %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

# 3. Combine ice and water temperature data by water year ------
combined_under_ice_data_deep1 <- inner_join(kallavesi_sayneensalo_44m_filtered, ice_data_wide1, by = "water_year")


# Lake 2 (deepest consistent measurement = 40m (max 48/50))-----------------------------------

# Loading ice data
pielinen_nurmes <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 3) %>% 
  clean_names() %>% 
  select(ib_0401410, lippu) 

names(pielinen_nurmes)[names(pielinen_nurmes) == "ib_0401410"] <- "date"
pielinen_nurmes$date <- as.Date(pielinen_nurmes$date, format = "%Y%m%d")
pielinen_nurmes$date <- format(pielinen_nurmes$date, "%Y-%m-%d") 

# Convert Date column to Date format
pielinen_nurmes$date <- as.Date(pielinen_nurmes$date, format = "%Y-%m-%d")

# Add a column for year
pielinen_nurmes$year <- format(pielinen_nurmes$date, "%Y")

# Convert dates into water year format
pielinen_nurmes <- pielinen_nurmes %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data2 <- pielinen_nurmes %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data2 <- kallavesi_kuopio %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide2 <- full_join(ice_on_data2, ice_off_data2, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide2 <- ice_data_wide2 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# Processing Under-ice water temperature data (40m depth) 
df2 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 3) %>% 
  clean_names() 

# Print all column names
print(colnames(df2))

# New names
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m', 'ts_22m', 'ts_24m', 'ts_26m', 'ts_28m', 'ts_30m', 'ts_32m', 'ts_34m', 'ts_35m', 'ts_36m', 'ts_38m', 'ts_39m', 'ts_40m', 'ts_42m', 'ts_42_4m', 'ts_42_5m', 'ts_42_6m', 'ts_42_7m', 'ts_42_8m', 'ts_43m', 'ts_43_1m', 'ts_43_2m', 'ts_43_3m', 'ts_43_4m', 'ts_43_5m', 'ts_43_6m','ts_44m', 'ts_46m', 'ts_48m', 'ts_50m') 

# Setting names and factor of lake
df2_clean <- df2 %>%
  select(1:54) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(0441100)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df2_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting 40m depth
pieline_nurmes_40m <- df2_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_40m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Convert to as.date 
pieline_nurmes_40m$date <- as.Date(pieline_nurmes_40m$date)

# Filter out years with missing water temperature data
pieline_nurmes_40m_filtered <- pieline_nurmes_40m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
pieline_nurmes_40m_filtered <- pieline_nurmes_40m_filtered %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

# Combine ice and water temperature data 
combined_under_ice_data_deep2<- inner_join(pieline_nurmes_40m_filtered, ice_data_wide2, by = "water_year")


# Lake 3 (deepest consistent measurement = 50m (max 65 m))--------------------------

#loading ice data
paijanne_tehi <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 22) %>% 
  clean_names() %>% 
  select(ib_1422120, lippu) 

names(paijanne_tehi)[names(paijanne_tehi) == "ib_1422120"] <- "date"
paijanne_tehi$date <- as.Date(paijanne_tehi$date, format = "%Y%m%d")
paijanne_tehi$date <- format(paijanne_tehi$date, "%Y-%m-%d") 

# Convert Date column to Date format
paijanne_tehi$date <- as.Date(paijanne_tehi$date, format = "%Y-%m-%d")

# Add a column for year
paijanne_tehi$year <- format(paijanne_tehi$date, "%Y")

# Convert dates into water year format
paijanne_tehi <- paijanne_tehi %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data3 <- paijanne_tehi %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data3 <- paijanne_tehi %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide3 <- full_join(ice_on_data3, ice_off_data3, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide3 <- ice_data_wide3 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# Processing water temperature data (50m depth) 
df3 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 4) %>% 
  clean_names() 

# Print all column names
print(colnames(df3))

# Renaming columns 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m','ts_21m', 'ts_22m', 'ts_24m', 'ts_26m', 'ts_28m', 'ts_30m', 'ts_32m', 'ts_34m', 'ts_36m', 'ts_38m', 'ts_40m', 'ts_42m', 'ts_44m', 'ts_46m', 'ts_48m', 'ts_50m', 'ts_54_5m', 'ts_55m', 'ts_59_5m', 'ts_60m', 'ts_60_5m', 'ts_61m', 'ts_61_5m', 'ts_62m','ts_62_5m', 'ts_63m', 'ts_64_5m', 'ts_65m') 

# Setting new names and lake as factor 
df3_clean <- df3 %>%
  select(1:53) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(1422110)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df3_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting depth and piviot long 
paijanne_linnasaari_50m <- df3_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_50m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Make as.date 
paijanne_linnasaari_50m$date <- as.Date(paijanne_linnasaari_50m$date)

# Filter out years with missing water temperature data
paijanne_linnasaari_50m_filtered <- paijanne_linnasaari_50m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
paijanne_linnasaari_50m_filtered <- paijanne_linnasaari_50m_filtered %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

#combine ice and water temperature data
combined_under_ice_data_deep3 <- inner_join(paijanne_linnasaari_50m_filtered, ice_data_wide3, by = "water_year")


# Lake 4 (deepest consistent measurement = 36m (max 50))---------------------------------------
# Loading ice data
paijanne_sysma <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 17) %>% 
  clean_names() %>% 
  select(ib_1406000, lippu) 

# Changing column names and format 
names(paijanne_sysma)[names(paijanne_sysma) == "ib_1406000"] <- "date"
paijanne_sysma$date <- as.Date(paijanne_sysma$date, format = "%Y%m%d")
paijanne_sysma$date <- format(paijanne_sysma$date, "%Y-%m-%d") 

# Convert Date column to Date format
paijanne_sysma$date <- as.Date(paijanne_sysma$date, format = "%Y-%m-%d")

# Add a column for year
paijanne_sysma$year <- format(paijanne_sysma$date, "%Y")

# Convert dates into water year format
paijanne_sysma <- paijanne_sysma %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data4 <- paijanne_sysma %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data4 <- paijanne_sysma %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide4 <- full_join(ice_on_data4, ice_off_data4, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide4 <- ice_data_wide4 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# process under-ice water temperature data (36m depth) 
df4 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 5) %>% 
  clean_names() 

# Print all column names
print(colnames(df4))

# Renaming columns 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m', 'ts_21m', 'ts_22m', 'ts_24m', 'ts_26m', 'ts_28m', 'ts_30m', 'ts_32m', 'ts_34m', 'ts_35_5m', 'ts_36m', 'ts_36_5m', 'ts_37_5m', 'ts_38m', 'ts_38_5m', 'ts_39m', 'ts_39_5m', 'ts_40m', 'ts_40_5m', 'ts_41m', 'ts_41_5m', 'ts_42m','ts_50m')

# Setting new names and lake as factor 
df4_clean <- df4 %>%
  select(1:47) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(1422120)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df4_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Select depth and piviot long
paijanne_paijatsalo_36m <- df4_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_36m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Setting as date 
paijanne_paijatsalo_36m$date <- as.Date(paijanne_paijatsalo_36m$date)

# Filter out years with missing water temperature data
paijanne_paijatsalo_36m_filtered <- paijanne_paijatsalo_36m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
paijanne_paijatsalo_36m_filtered <- paijanne_paijatsalo_36m_filtered %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

#combine ice and water temperature data
combined_under_ice_data_deep4 <- inner_join(paijanne_paijatsalo_36m_filtered, ice_data_wide4, by = "water_year")


# Lake 5 (deepest consistent measurement = 46 (max 55))------------------------------
# Loading ice data
konnevesi_etela <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 14) %>% 
  clean_names() %>% 
  select(ib_1403900, lippu) 

# Renaming columns and format 
names(konnevesi_etela)[names(konnevesi_etela) == "ib_1403900"] <- "date"
konnevesi_etela$date <- as.Date(konnevesi_etela$date, format = "%Y%m%d")
konnevesi_etela$date <- format(konnevesi_etela$date, "%Y-%m-%d") 

# Convert Date column to Date format
konnevesi_etela$date <- as.Date(konnevesi_etela$date, format = "%Y-%m-%d")

# Add a column for year
konnevesi_etela$year <- format(konnevesi_etela$date, "%Y")

# Convert dates into water year format
konnevesi_etela <- konnevesi_etela %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data5 <- konnevesi_etela %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data5 <- konnevesi_etela %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide5 <- full_join(ice_on_data5, ice_off_data5, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide5 <- ice_data_wide5 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# Process under-ice water temperature data (44m depth) 
df5 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 6) %>% 
  clean_names() 

# Print all column names
print(colnames(df5))

# Renaming and setting new names 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m', 'ts_22m', 'ts_24m', 'ts_26m', 'ts_28m', 'ts_30m', 'ts_32m', 'ts_34m', 'ts_36m', 'ts_38m', 'ts_40m', 'ts_42m', 'ts_42_5m', 'ts_43m', 'ts_43_5m', 'ts_44m', 'ts_44_5m', 'ts_45m', 'ts_45_5m', 'ts_46m', 'ts_46_5m', 'ts_47m', 'ts_47_5m', 'ts_47_9m', 'ts_48m', 'ts_48_4m', 'ts_48_5m', 'ts_49m', 'ts_49_5m', 'ts_50m', 'ts_50_5m', 'ts_51m', 'ts_51_5m', 'ts_52m', 'ts_52_5m', 'ts_55m')

df5_clean <- df5 %>%
  select(1:60) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(1471110)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df5_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting depth and pivoting longer
konnevesi_nareselka_46m <- df5_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_46m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Make as.date
konnevesi_nareselka_46m$date <- as.Date(konnevesi_nareselka_46m$date)

# Filter out years with missing water temperature data
konnevesi_nareselka_46m_filtered <- konnevesi_nareselka_46m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
konnevesi_nareselka_46m_filtered <- konnevesi_nareselka_46m_filtered %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

# Combine ice and water temperature data
combined_under_ice_data_deep5 <- inner_join(konnevesi_nareselka_46m_filtered, ice_data_wide5, by = "water_year")


# Lake 6 (deepest consistent measurement = 9m (max 10.5m))-----------------------------------------------
# *NOTE*: the lake ice data below is the same as above
# This is because two under-ice water temperature sites are located in oposite end of the same lake)

# Loading ice data
konnevesi_etela <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 14) %>% 
  clean_names() %>% 
  select(ib_1403900, lippu) 

names(konnevesi_etela)[names(konnevesi_etela) == "ib_1403900"] <- "date"
konnevesi_etela$date <- as.Date(konnevesi_etela$date, format = "%Y%m%d")
konnevesi_etela$date <- format(konnevesi_etela$date, "%Y-%m-%d") 

# Convert Date column to Date format
konnevesi_etela$date <- as.Date(konnevesi_etela$date, format = "%Y-%m-%d")

# Add a column for year
konnevesi_etela$year <- format(konnevesi_etela$date, "%Y")

# Convert dates into water year format
konnevesi_etela <- konnevesi_etela %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data5 <- konnevesi_etela %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data5 <- konnevesi_etela %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide5 <- full_join(ice_on_data5, ice_off_data5, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide5 <- ice_data_wide5 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# Process under-ice water temperature data (9m)
df6 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 7) %>% 
  clean_names() 

# Print all column names
print(colnames(df6))

# Setting new names 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_8_5m', 'ts_8_7m', 'ts_8_8m', 'ts_9m', 'ts_9_1m', 'ts_9_2m', 'ts_9_3m', 'ts_9_5m', 'ts_9_6m', 'ts_9_7m','ts_9_9m', 'ts_10m', 'ts_10_4m', 'ts_10_5m')

df6_clean <- df6 %>%
  select(1:27) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(1471120)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df6_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting depth and pivioting longer format 
konnevesi_pynnolanniemi_9m <- df6_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_9m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Make as.date
konnevesi_pynnolanniemi_9m$date <- as.Date(konnevesi_pynnolanniemi_9m$date)

# Filter out years with missing water temperature data
konnevesi_pynnolanniemi_9m_filtered <- konnevesi_pynnolanniemi_9m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
konnevesi_pynnolanniemi_9m_filtered <- konnevesi_pynnolanniemi_9m_filtered %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

# Combine ice and water temperature data
combined_under_ice_data_deep6 <- inner_join(konnevesi_pynnolanniemi_9m_filtered, ice_data_wide5, by = "water_year")


# Lake 7 (deepest consistent measurement = 19m (max =20))---------------------------
#Loading ice data
pyhajarvi_kauttua <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 24) %>% 
  clean_names() %>% 
  select(ib_3400100, lippu) 

# Setting new column names and format 
names(pyhajarvi_kauttua)[names(pyhajarvi_kauttua) == "ib_3400100"] <- "date"
pyhajarvi_kauttua$date <- as.Date(pyhajarvi_kauttua$date, format = "%Y%m%d")
pyhajarvi_kauttua$date <- format(pyhajarvi_kauttua$date, "%Y-%m-%d") 

# Convert Date column to Date format
pyhajarvi_kauttua$date <- as.Date(pyhajarvi_kauttua$date, format = "%Y-%m-%d")

# Add a column for year
pyhajarvi_kauttua$year <- format(pyhajarvi_kauttua$date, "%Y")

# Convert dates into water year format
pyhajarvi_kauttua <- pyhajarvi_kauttua %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data7 <- pyhajarvi_kauttua %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data7 <- pyhajarvi_kauttua %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide7 <- full_join(ice_on_data7, ice_off_data7, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide7 <- ice_data_wide7 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# Process under-ice water temperature data (19m)
df7 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 8) %>% 
  clean_names() 

# Print all column names
print(colnames(df7))

# Setting new names 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m')

df7_clean <- df7 %>%
  select(1:25) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(3403100)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df7_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting depth and pivoting data long
sakylan_pyhajarvi_19m <- df7_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_19m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Set as.date 
sakylan_pyhajarvi_19m$date <- as.Date(sakylan_pyhajarvi_19m$date)

# Filter out years with missing water temperature data
sakylan_pyhajarvi_19m <- sakylan_pyhajarvi_19m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
sakylan_pyhajarvi_19m_filtered <- sakylan_pyhajarvi_19m %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

# Combine ice and under-ice water temperature data
combined_under_ice_data_deep7 <- inner_join(sakylan_pyhajarvi_19m_filtered, ice_data_wide7, by = "water_year")


#Lake 8 (consistent = 12m (max = 14m) -------------------------------------------------
#*NOTE*: The ice phenology data for this lake was processed differently 
# Ice data did not exist for this lake
# Inputted satellite data and water temperature measurements were used to estimate ice-on and ice-off dates

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Estimating ice-off dates ------------ 

# Load the data
# Adjust the file path to match the correct location on your computer
file_path <- "C:/location/.csv" # This data was satellite imagery for coordinates of the lake downloaded from climate engine 
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Inspect the structure of the dataset
str(data)

# Filter the data to only include rows where cloudModis < 0.1
filtered_data <- data %>% filter(cloudModis < 0.1)

# Convert the date column to Date format for better plotting
filtered_data$date <- as.Date(filtered_data$date)

# Separate Modis Terra and Aqua data
terra_data <- filtered_data %>% filter(source == "MODISTerra")

terra_data <- terra_data %>%
  # Remove rows where `date` or `iceModis` is NA
  filter(!is.na(date), !is.na(iceModis)) %>%
  # Add a year column
  mutate(year = year(date))
print(terra_data)

find_ice_off_range <- function(data) {
  data %>%
    arrange(date) %>%  # Ensure data is sorted by date
    mutate(month = month(date)) %>%  # Extract the month from the date
    filter(month >= 3 & month <= 6) %>%  # Focus on relevant months (March to June)
    group_by(year) %>%
    summarise(
      ice_off_date = first(date[iceModis < 0.1]),  # First date with < 0.1 ice
      last_ice_date = last(date[iceModis >= 0.1 & date <= first(date[iceModis < 0.1])]),  # Last date before ice-off
      .groups = "drop"
    ) %>%
    left_join(data, by = c("year"))  # Keep the original data to retain `iceModis`
}

# Apply the function to Terra data
terra_ice_off_range <- find_ice_off_range(terra_data)

# Select relevant columns (year, ice_off_date, and last_ice_date) and remove duplicates
terra_ice_off_range_clean <- terra_ice_off_range %>%
  select(year, ice_off_date, last_ice_date) %>%
  distinct()  # Remove duplicates

# Manually adding data from sentinel pub EO that was determined (last sight of ice, and when no ice was apparent) landsat 4-5
# This was done because Modis data was not available before 2000. 
manual_data <- tibble(
  year = c(1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999),
  last_ice_date = as.Date(c("1986-05-02", "1987-05-21", "1988-05-14", "1989-05-01", "1990-03-25",
                            "1991-05-07", "1992-05-09", "1993-05-03", "1994-05-08", "1995-05-11",
                            "1996-05-20", "1997-06-01", "1998-05-10", "1999-05-01")),                   # when ice was last seen on satalite image
  ice_off_date = as.Date(c("1986-05-18", "1987-05-28", "1988-05-21", "1989-05-16", "1990-05-13",
                           "1991-05-23", "1992-05-25", "1993-05-19", "1994-05-24", "1995-05-25",
                           "1996-06-12", "1997-06-08", "1998-05-17", "1999-06-07"))                     # when ice was no longer visable on satalite image
)


# Join the manual data with the cleaned Terra data
full_data <- terra_ice_off_range_clean %>%
  full_join(manual_data, by = c("year", "last_ice_date", "ice_off_date"))

# Arrange the data by 'year' to ensure it's ordered
full_data <- full_data %>%
  arrange(year)

# Load the water temperature data ranges into a data frame (this was done manually using original df from SYKE)
# These are water temperature measurements, where we assume the lake was frozen vs thawed 
# e.g. surface temperature were 0.2C and the next measurement was 1.0C 
water_temp_data <- data.frame(
  year = 1986:2022,
  start_frozen_water = c("28-Feb", "10-Mar", "30-Apr", "20-Apr", "20-Mar", "20-Apr", "30-Apr", "30-Apr", "20-Apr", "30-Apr", 
                         "10-Apr", "20-Apr", "10-Apr", "10-Apr", "20-Apr", "30-Apr", "30-Apr", "20-Apr", "20-Apr", "10-Apr", 
                         "20-Apr", "20-Apr", "30-Apr", "30-Apr", "30-Apr", "12-Apr", "10-Apr", "30-Apr", "29-Apr", "30-Apr", 
                         "21-Apr", "13-Apr", "23-Apr", "22-Apr", "01-Apr", "21-Apr", "11-Apr"),
  end_not_frozen_water = c("20-Jun", "20-Jun", "30-May", "20-May", "20-May", "30-Apr", "30-May", "20-May", "20-May", "30-May", 
                           "30-May", "20-Jun", "20-May", "30-May", "20-May", "30-May", "20-May", "30-May", "20-May", "30-Apr", 
                           "20-May", "20-May", "30-May", "20-May", "17-May", "09-May", "16-May", "14-May", "20-May", "19-May", 
                           "09-May", "10-Jun", "14-May", "15-May", "31-May", "20-May", "21-May")
)

# Convert the `start_frozen_water` and `end_not_frozen_water` columns to Date format (combine year with the month-day)
water_temp_data$start_frozen_water <- as.Date(paste(water_temp_data$year, water_temp_data$start_frozen_water, sep="-"), format="%Y-%d-%b")
water_temp_data$end_not_frozen_water <- as.Date(paste(water_temp_data$year, water_temp_data$end_not_frozen_water, sep="-"), format="%Y-%d-%b")

# Join the water temperature data with the full data frame by 'year'
full_data_water <- full_data %>%
  left_join(water_temp_data, by = "year")

# Calculate the ice-off
# This was done by taking the median overlap of the two ranges (satalite image range and water temperature range)

calculate_ice_off <- function(df) {
  df %>%
    rowwise() %>%
    mutate(
      # Convert the date columns to Date type
      modis_start_date = as.Date(last_ice_date),
      modis_end_date = as.Date(ice_off_date),
      temp_start_date = as.Date(start_frozen_water),  # Assuming this is the column for water freeze
      temp_end_date = as.Date(end_not_frozen_water),  # Assuming this is the column for water thaw
      
      # Calculate the overlap range
      overlap_start = max(modis_start_date, temp_start_date),
      overlap_end = min(modis_end_date, temp_end_date),
      
      # If there is an overlap, calculate the median of the overlap range
      ice_off = ifelse(
        overlap_start <= overlap_end, 
        overlap_start + (overlap_end - overlap_start) / 2,  # Midpoint of overlap range
        # If no overlap, calculate the midpoint of temp_start_date and temp_end_date (this is because water temperature was more reliable - see suplimentary)
        as.Date((as.numeric(temp_start_date) + as.numeric(temp_end_date)) / 2, origin = "1970-01-01")
      )
    ) %>%
    ungroup() %>%
    mutate(
      # Ensure ice_off is a Date type (in case it's still stored as numeric)
      ice_off = as.Date(ice_off, origin = "1970-01-01")
    )
}

# Apply the function to your full_data_water
full_data_with_ice_off <- calculate_ice_off(full_data_water)

# View the result
print(full_data_with_ice_off)

# Remove the duplicated columns (modis_start_date, modis_end_date, temp_start_date, temp_end_date)
full_data_with_ice_off <- full_data_with_ice_off %>%
  select(year, ice_off, modis_start_date, modis_end_date, temp_start_date, temp_end_date, overlap_start, overlap_end)  # Keep only the relevant columns

# View the cleaned data
print(full_data_with_ice_off)


# Estimating ice-on dates --------------- 
# No satellite data was available for ice-on due to polar night in Finland 
# FDD and water temperatures were used as an estimate for ice-on

# Load data
# The df "combined_air_temp_data" of climate variables was compiled in the file titled "03_combined_climate_variables" within the repository
# For details, access this file
combined_air_temp_data <- read.csv("C:/location/combined_air_temp_data.csv", header = TRUE)

# Filter for only Pesiojarvi
pesiojarvi_data <- combined_air_temp_data %>%
  filter(Observation_station == "Pesiojarvi") %>%
  mutate(
    # Convert day, month, and year into a proper date format
    date = as.Date(paste(Year, Month, Day, sep = "-")),
    
    # Calculate daily freezing degree days (negative temp only, otherwise 0)
    daily_fdd = ifelse(mean_temp_C < 0, abs(mean_temp_C), 0),
    
    # Create a 'season' column that combines the year and the freezing season (Oct-Jan)
    season = case_when(
      Month >= 10 ~ Year,  # For Oct-Dec, assign to the current year
      Month <= 1 ~ Year - 1,  # For Jan, assign to the previous year
      TRUE ~ NA_real_  # Default case (should not happen for valid dates)
    )
  )

# Focus on the freezing season (October - January next year)
pesiojarvi_fdd <- pesiojarvi_data %>%
  filter(!is.na(season)) %>%  # Filter out rows where season is NA (non-freezing months)
  group_by(season) %>%  # Group by season
  mutate(
    # Detect negative temperatures 
    sustained_neg_temp = cumsum(mean_temp_C < 0) >= 0,  #
    start_fdd_date = date[min(which(sustained_neg_temp))],  # Get the start date for FDD
    cumulative_fdd = cumsum(daily_fdd)  # Calculate cumulative FDD
  ) %>%
  ungroup()

print(pesiojarvi_fdd)

# Select the relevant FDD threshold and get the ice-on estimate
fdd_results <- pesiojarvi_fdd %>%
  group_by(season) %>%
  filter(cumulative_fdd >= 78) %>%  # we set our FDD threshold based on research; see suplimentary)
  summarise(
    ice_on_estimate = first(date),  # Get the first date when FDD exceeds threshold
    total_fdd = first(cumulative_fdd)  # Total FDD at that point
  )

# View the refined results
print(fdd_results)

# Water temperature ranges, created manually from SYKE dataset (eg. when surface temperature went from 1.0C to 0.2C)
water_temp_freeze_ranges <- tibble(
  freeze_year = c(1985, 1986, 1987, 1988, 1989, 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999, 
                  2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 
                  2015, 2016, 2017, 2018, 2019, 2020, 2021),
  ice_on_start = as.Date(c("1985-10-20", "1986-12-10", "1987-10-30", "1988-10-20", "1989-10-20", "1990-10-10", 
                           "1991-10-01", "1992-10-10", "1993-10-31", "1994-10-10", "1995-10-10", "1996-09-20", 
                           "1997-09-30", "1998-10-10", "1999-11-30", "2000-10-20", "2001-10-20", "2002-11-20", 
                           "2003-10-20", "2004-11-10", "2005-11-10", "2006-10-20", "2007-10-30", "2008-11-10", 
                           "2009-10-21", "2010-10-31", "2011-12-11", "2012-10-22", "2013-11-11", "2014-10-11", 
                           "2015-11-11", "2016-10-30", "2017-10-30", "2018-11-12", "2019-10-20", "2020-11-20", 
                           "2021-10-31")),
  ice_on_end = as.Date(c("1985-11-30", "1987-01-10", "1987-12-10", "1988-11-10", "1989-12-20", "1990-11-20", 
                         "1992-01-20", "1992-11-10", "1993-11-30", "1994-11-10", "1995-11-30", "1996-12-20", 
                         "1997-12-10", "1998-11-20", "2000-01-30", "2000-12-10", "2001-11-20", "2003-01-30", 
                         "2003-12-20", "2004-12-20", "2005-12-20", "2006-11-16", "2008-01-10", "2009-01-10", 
                         "2009-12-08", "2010-11-24", "2011-12-22", "2012-12-06", "2013-12-10", "2014-11-25", 
                         "2016-01-11", "2016-12-01", "2018-01-31", "2018-12-20", "2019-12-04", "2021-01-06", 
                         "2021-12-22"))
)

print(water_temp_freeze_ranges)

# Cross-reference the FDD ice-on estimates with the water temperature ranges
fdd_results <- fdd_results %>%
  left_join(water_temp_freeze_ranges, by = c("season" = "freeze_year")) %>%  # Join the data frames by season (year)
  mutate(
    ice_on_within_range = if_else(
      ice_on_estimate >= ice_on_start & ice_on_estimate <= ice_on_end, 
      "Within range", 
      "Outside range"
    )
  )

# View the results
print(fdd_results)

# For estimates that fall outside of the range, we take the median value of the water temp range instead (see suplimentary) 
# Update fdd_results with the median for cases outside the range 
fdd_results <- fdd_results %>%
  mutate(
    # Calculate the median date of the range
    median_range_date = ice_on_start + (as.numeric(ice_on_end - ice_on_start) / 2),
    # Replace the ice_on_estimate if it's outside the range
    ice_on_refined = if_else(
      ice_on_within_range == "Outside range",
      median_range_date,
      ice_on_estimate
    )
  )

# View the updated results
print(fdd_results)

# Mutating to water year to join df together 
fdd_results <- fdd_results %>%
  mutate(
    water_year = case_when(
      month(ice_on_refined) >= 10 ~ season + 1,  # Oct-Dec belongs to the next water year
      TRUE ~ season  # Jan-Sep belongs to the current year
    )
  )

# Renaming to join (all ice off years represent water year, no need to change) 
full_data_with_ice_off <- full_data_with_ice_off %>%
  rename(water_year = year)

# Combine ice estimates together
ice_data_wide8 <- fdd_results %>%
  select(water_year, ice_on = ice_on_refined) %>%  # Select relevant columns from fdd_results
  left_join(full_data_with_ice_off %>% select(water_year, ice_off), by = "water_year")  # Join with ice-off data

ice_data_wide8 <- na.omit(ice_data_wide8)

# Processing under-ice water temperature data   
df8 <- readxl::read_excel(here("C:/location/.xlsx"), sheet = 9) %>% 
  clean_names()

# Print all column names
print(colnames(df8))

# Set new names and lake as factor 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_11_5m', 'ts_12m', 'ts_12_3m', 'ts_12_4m', 'ts_12_5m', 'ts_12_8m', 'ts_13m', 'ts_14m')

df8_clean <- df8 %>%
  select(1:24) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(5954100)
  )


# Count non-NA observations in each column of df1_clean
obs_counts <- df8_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Select depth and pivot df 
pesiojarvi_12m <- df8_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_12m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

pesiojarvi_12m$date <- as.Date(pesiojarvi_12m$date)

# Filter out years with missing water temperature data
pesiojarvi_12m <- pesiojarvi_12m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
pesiojarvi_12m_filtered <- pesiojarvi_12m %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

#combine ice estimates and water temperature data 
combined_under_ice_data_deep8 <- inner_join(pesiojarvi_12m_filtered, ice_data_wide8, by = "water_year")


# Lake 9 (deepest consistent measurement = 40m (max = 50 m))---------------------------------------
# Loading ice data
inari_nellim <- readxl::read_excel(here('C:/location/Syke IB 1800-2022 .xlsx'), sheet = 49) %>% 
  clean_names() %>% 
  select(ib_7101610, lippu) 

# New column names and format 
names(inari_nellim)[names(inari_nellim) == "ib_7101610"] <- "date"
inari_nellim$date <- as.Date(inari_nellim$date, format = "%Y%m%d")
inari_nellim$date <- format(inari_nellim$date, "%Y-%m-%d") 

# Convert Date column to Date format
inari_nellim$date <- as.Date(inari_nellim$date, format = "%Y-%m-%d")

# Add a column for year
inari_nellim$year <- format(inari_nellim$date, "%Y")

# Convert dates into water year format
inari_nellim <- inari_nellim %>%
  mutate(
    date = as.Date(date),
    # Define water year: if month is Oct or later, assign to the following year
    water_year = ifelse(month(date) > 9, year(date) + 1, year(date))
  )

# Filter and prioritize for ice-on (x7 > x5) and ice-off (b4 > b5) within each water year
ice_on_data9 <- inari_nellim %>%
  filter(lippu %in% c("x7", "x5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_on = case_when(
      any(lippu == "x7") ~ date[lippu == "x7"][1], # If there's an x7, take it
      any(lippu == "x5") ~ date[lippu == "x5"][1]  # Otherwise, take x5
    ),
    .groups = "drop"
  )

ice_off_data9 <- inari_nellim %>%
  filter(lippu %in% c("b4", "b5")) %>%
  group_by(water_year) %>%
  summarize(
    ice_off = case_when(
      any(lippu == "b4") ~ date[lippu == "b4"][1], # If there's a b4, take it
      any(lippu == "b5") ~ date[lippu == "b5"][1]  # Otherwise, take b5
    ),
    .groups = "drop"
  )

# Join the datasets by water year to align ice_on and ice_off
ice_data_wide9 <- full_join(ice_on_data9, ice_off_data9, by = "water_year") %>%
  arrange(water_year)

# Remove misisng data from rows that have both ice-on and ice-off data.
ice_data_wide9 <- ice_data_wide9 %>%
  filter(!is.na(ice_on) & !is.na(ice_off))


# Process under-ice water temperature data 
df9 <- readxl::read_excel(here('C:/location/Syke TS 1900-2022.xlsx'), sheet = 10) %>% 
  clean_names() 

# Print all column names
print(colnames(df9))

# Setting new names and lake as factor 
new_names <- c('date', 'ts_mean', 'ts_min', 'ts_max', 'ts_0m', 'ts_1m', 'ts_2m', 'ts_3m', 'ts_4m', 'ts_5m', 'ts_6m', 'ts_7m', 'ts_8m', 'ts_9m', 'ts_10m', 'ts_11m', 'ts_12m', 'ts_13m', 'ts_14m', 'ts_15m', 'ts_16m', 'ts_17m', 'ts_18m', 'ts_19m', 'ts_20m', 'ts_22m', 'ts_24m', 'ts_25m', 'ts_26m', 'ts_28m', 'ts_30m', 'ts_32m', 'ts_34m', 'ts_36m', 'ts_38m', 'ts_40m', 'ts_40_8m', 'ts_41m', 'ts_41_3m', 'ts_41_5m', 'ts_42m', 'ts_43m', 'ts_43_5m', 'ts_44m', 'ts_45m', 'ts_46m', 'ts_47m', 'ts_48m', 'ts_49m', 'ts_50m', 'ts_51m', 'ts_52m', 'ts_55m')

df9_clean <- df9 %>%
  select(1:53) %>%  
  setNames(new_names) %>% 
  mutate(
    lake = as.factor(7111100)
  )

# Count non-NA observations in each column of df1_clean
obs_counts <- df9_clean %>%
  summarize_all(~ sum(!is.na(.)))

# Selecting depth and pivoting long  
inari_paksuvuono_40m <- df9_clean %>% 
  select(
    # lake, date, ts_0m, ts_1m, ts_2m 
    'date', 'ts_40m'
  ) %>% 
  pivot_longer(
    !c(date), 
    names_to = 'depth',
    values_to = 'temp_C') 

# Set as.date 
inari_paksuvuono_40m$date <- as.Date(inari_paksuvuono_40m$date)

# Filter out years with missing water temperature data
inari_paksuvuono_40m <- inari_paksuvuono_40m %>%
  filter(!is.na(temp_C))

# Sort by water year to join data
inari_paksuvuono_40m_filtered <- inari_paksuvuono_40m %>%
  mutate(
    water_year = ifelse(lubridate::month(date) > 9, lubridate::year(date) + 1, lubridate::year(date)),
    water_year_temp_date = as.Date(paste(water_year, format(date, "%m-%d"), sep = "-"))
  )

#combine ice and water temperature data
combined_under_ice_data_deep9 <- inner_join(inari_paksuvuono_40m_filtered, ice_data_wide9, by = "water_year")


# Combine all lake data into one data frame ---------------------------------------
combined_under_ice_data_deep1 <- combined_under_ice_data_deep1 %>%
  mutate(lake = "Lake_1")

combined_under_ice_data_deep2 <- combined_under_ice_data_deep2 %>%
  mutate(lake = "Lake_2")

combined_under_ice_data_deep3 <- combined_under_ice_data_deep3 %>%
  mutate(lake = "Lake_3")

combined_under_ice_data_deep4 <- combined_under_ice_data_deep4 %>%
  mutate(lake = "Lake_4")

combined_under_ice_data_deep5 <- combined_under_ice_data_deep5 %>%
  mutate(lake = "Lake_5")

combined_under_ice_data_deep6 <- combined_under_ice_data_deep6 %>%
  mutate(lake = "Lake_6")

combined_under_ice_data_deep7 <- combined_under_ice_data_deep7 %>%
  mutate(lake = "Lake_7")

combined_under_ice_data_deep8 <- combined_under_ice_data_deep8 %>%
  mutate(lake = "Lake_8")

combined_under_ice_data_deep9 <- combined_under_ice_data_deep9 %>%
  mutate(lake = "Lake_9")

# Combine all your datasets into one
all_combined_data_under_ice_deep <- bind_rows(combined_under_ice_data_deep1, combined_under_ice_data_deep2, combined_under_ice_data_deep3, combined_under_ice_data_deep4, combined_under_ice_data_deep5, combined_under_ice_data_deep6, combined_under_ice_data_deep7, combined_under_ice_data_deep8, combined_under_ice_data_deep9, .id = "lake")
print(all_combined_data_under_ice_deep)

# This specific dataframe was downloaded and used in analysis in 
# file "04_predictor_varibles_for_models.R" and "08_cross_seasonal_SEM_and_correlations" 

# This df contains all raw values of under-ice and ice-on doy
# Change location to specified file location 
write.csv(
  combined_air_temp_data,
  file = "C:/location/all_combined_data_under_ice_deep.csv",
  row.names = FALSE
)

# Averaging under-ice water temperature data for each season in each lake ----------------------------------

# Calculate average under-ice temperature between ice-on and ice-off dates
all_combined_data_under_ice_deep_final <- all_combined_data_under_ice_deep %>%
  rowwise() %>%
  mutate(
    ice_on_day_of_year =  yday(as.Date(`ice_on`)),
    ice_off_day_of_year = yday(as.Date(`ice_off`)),
    ice_on_doy = ifelse(month(as.Date(`ice_on`)) %in% 1:4,
                        ice_on_day_of_year + ifelse(leap_year(year(as.Date(`ice_on`)) - 1), 366, 365),
                        ice_on_day_of_year),  # Add julian_date column
  ) %>%
  ungroup() %>%
  group_by(lake, water_year, ice_on_doy) %>%
  filter(date >= as.Date(`ice_on`) & 
           date <= as.Date(ice_off)) %>%
  summarize(avg_temp = mean(temp_C, na.rm = TRUE))

view(all_combined_data_under_ice_deep_final)
print(all_combined_data_under_ice_deep_final)


# Spearman's correlation: Under-ice water temperature and ice-on day of year --------------------------------------------
library(ggplot2)
library(ggpubr)

# Spearman's plot in paper 

under_ice_water_ice_on_cor <- ggplot(all_combined_data_under_ice_deep_final, aes(x = ice_on_doy, y = avg_temp)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", formula = y ~ x, color = "black", se = TRUE, fill = "gray80") +
  geom_smooth(method = "loess", se = TRUE, color = "blue", linetype = "dashed", fill = "lightblue") +
  stat_cor(
    method = "spearman",
    label.x.npc = "left",
    label.y.npc = "top",
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
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

print(under_ice_water_ice_on_cor)

# NOTE: THIS WAS FINAL FIGURE IN PANEL WITHIN PAPER
file_path <- "C:/location/under_ice_water_ice_on_cor.png"

# Save the plot
ggsave(filename = file_path, plot = under_ice_water_ice_on_cor, width = 8, height = 6, dpi = 300)

# Sen's slope calculation for under-ice water temperature (1997-2021) ---------------

# Function for time frame (1998 - 2022 because "year" is represented as water year)
years <- 1998:2022

# Filter the data for the years 1997 to 2021
all_combined_data_under_ice_final_sens <- all_combined_data_under_ice_deep_final %>%
  filter(water_year >= 1998 & water_year <= 2022)

# Function to ensure each observation station has complete time series
complete_time_series <- function(data, years) {
  expand.grid(lake = unique(data$lake), water_year = years) %>%
    left_join(data, by = c("lake", "water_year"))
}

# Apply complete_time_series function to the filtered data
all_combined_data_under_ice_final_sens <- complete_time_series(all_combined_data_under_ice_final_sens, years)

# Calculate number of missing temp values for each station
missing_temp_val_ui <- all_combined_data_under_ice_final_sens %>%
  group_by(`lake`) %>%
  summarize(missing_temp_val = sum(is.na(`avg_temp`))) %>%
  ungroup()

# Merge missing temp values back into the main data
all_combined_data_under_ice_final_sens <- all_combined_data_under_ice_final_sens %>%
  left_join(missing_temp_val_ui, by = "lake")

# Calculate mean temperature for each lake and replace NA values
all_combined_data_under_ice_final_sens <- all_combined_data_under_ice_final_sens %>%
  group_by(`lake`) %>%
  mutate(mean_temp = mean(`avg_temp`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`avg_temp` = ifelse(is.na(`avg_temp`), mean_temp, `avg_temp`)) %>%
  select(-mean_temp)

# Sort the data by lake and water year
all_combined_data_under_ice_final_sens <- all_combined_data_under_ice_final_sens %>%
  arrange(lake, water_year)

# Calculate Sen's slope for each lake
results_ui <- all_combined_data_under_ice_final_sens %>%
  group_by(lake) %>%
  summarize(
    sens_slope = sens.slope(`avg_temp`)$estimates,
    p_value = sens.slope(`avg_temp`)$p.value
  )

# Sens slope calculation for under-ice water temperature (1985 to 2021) -------------------------

# Function for time frame (1986 - 2022 because "year" is represented as water year)
years <- 1986:2022

# Filter the data for the years 1997 to 2021
all_combined_data_under_ice_final_sens_1985 <- all_combined_data_under_ice_deep_final %>%
  filter(water_year >= 1986 & water_year <= 2022)

# Function to ensure each observation station has complete time series
complete_time_series <- function(data, years) {
  expand.grid(lake = unique(data$lake), water_year = years) %>%
    left_join(data, by = c("lake", "water_year"))
}

# Apply complete_time_series function to the filtered data
all_combined_data_under_ice_final_sens_1985 <- complete_time_series(all_combined_data_under_ice_final_sens_1985, years)

# Calculate number of missing temp values for each station
missing_temp_val_ui <- all_combined_data_under_ice_final_sens_1985 %>%
  group_by(`lake`) %>%
  summarize(missing_temp_val = sum(is.na(`avg_temp`))) %>%
  ungroup()

# Merge missing temp values back into the main data
all_combined_data_under_ice_final_sens_1985 <- all_combined_data_under_ice_final_sens_1985 %>%
  left_join(missing_temp_val_ui, by = "lake")

# Calculate mean temperature for each lake and replace NA values
all_combined_data_under_ice_final_sens_1985 <- all_combined_data_under_ice_final_sens_1985 %>%
  group_by(`lake`) %>%
  mutate(mean_temp = mean(`avg_temp`, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(`avg_temp` = ifelse(is.na(`avg_temp`), mean_temp, `avg_temp`)) %>%
  select(-mean_temp)

# Sort the data by lake and water year
all_combined_data_under_ice_final_sens_1985 <- all_combined_data_under_ice_final_sens_1985 %>%
  arrange(lake, water_year)

# Calculate Sen's slope for each lake
results_ui_1985 <- all_combined_data_under_ice_final_sens_1985 %>%
  group_by(lake) %>%
  summarize(
    sens_slope = sens.slope(`avg_temp`)$estimates,
    p_value = sens.slope(`avg_temp`)$p.value
  )

# Combine the two data frames (25 year and 37 year) with full_join
combined_results <- full_join(
  results_ui %>% rename(sens_slope_1997_2021 = sens_slope, p_value_1997_2021 = p_value),
  results_ui_1985 %>% rename(sens_slope_1985_2021 = sens_slope, p_value_1981_2021 = p_value),
  by = "lake"
)

# Anomaly graph for Under-ice water temperature ----------------------------------------------

#packages
library(ggplot2)
library(dplyr)
library(strucchange)

# Adding year column back for visualization of graph
filtered_temp_data <- all_combined_data_under_ice_deep_final %>%
  mutate(year = water_year - 1)

# Filter out years of interest
filtered_temp_data <- filtered_temp_data %>%
  filter(year >= 1985 & year <= 2021)

# Remove lake grouping to calculate a single baseline
baseline_avg_temp <- filtered_temp_data %>%
  ungroup() %>%  # Remove grouping by lake
  summarise(baseline_temp = mean(avg_temp, na.rm = TRUE)) %>%
  pull(baseline_temp)

# Calculate yearly average under-ice water temperature across all lakes
yearly_avg_temp <- filtered_temp_data %>%
  group_by(year) %>%
  summarise(yearly_avg_temp = mean(avg_temp, na.rm = TRUE))

# Calculate anomalies (difference from the baseline)
yearly_avg_temp <- yearly_avg_temp %>%
  mutate(anomaly = yearly_avg_temp - baseline_avg_temp)

# Create anomaly data frame with "Warming" or "Cooling"
anomaly_df <- filtered_temp_data %>%
  group_by(year) %>%
  summarise(avg_temp = mean(avg_temp, na.rm = TRUE)) %>%
  mutate(
    baseline_avg_temp = mean(avg_temp, na.rm = TRUE),
    anomaly = avg_temp - baseline_avg_temp,
    temp_trend = if_else(anomaly > 0, "Warming", "Cooling") %>% factor()
  )

# Plot under-ice water temperature anomalies
under_ice_anom <- ggplot(anomaly_df, aes(x = year, y = anomaly, fill = temp_trend)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Cooling" = "blue", "Warming" = "red"), 
                    name = "Temperature Trend") +  # Ensure legend title is set
  labs(
    x = "Year",
    y = "Under-ice bottom water temperature anomaly (°C)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # move legend below plot
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    
    # >>> Add these lines <<<
    axis.title = element_text(size = 16),  # axis titles
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)# axis tick labels
  )

print(under_ice_anom)

# FIGURE USED IN PANEL WITHIN PAPER
file_path <- "C:/location/under_ice_anom.png"

# Save the plot
ggsave(filename = file_path, plot = under_ice_anom, width = 8, height = 6, dpi = 300)

# Breakpoint calculation for under-ice water temperature ------------------

# Prepare the time series for breakpoints
temp_ts <- ts(yearly_avg_temp$anomaly, start = min(yearly_avg_temp$year), frequency = 1)

# Identify breakpoints
breakpoints_result <- breakpoints(temp_ts ~ 1)  # Intercept-only model
print(breakpoints_result)

# Extract breakpoints
start_year <- min(yearly_avg_temp$year)
breakpoint_years <- start_year + breakpoints_result$breakpoints - 1
first_breakpoint_year <- breakpoint_years[1]  # This is your identified breakpoint, if applicable

# Get the confidence intervals for the breakpoints
ci_breakpoints <- confint(breakpoints_result)
ci_first_breakpoint <- ci_breakpoints$confint[1, ]  # Assuming it's the first breakpoint, adjust index if needed

# Convert indices to years
ci_lower_year <- start_year + ci_first_breakpoint["2.5 %"] - 1
ci_upper_year <- start_year + ci_first_breakpoint["97.5 %"] - 1

# Print the confidence interval as years
print(paste("Confidence interval for first breakpoint: ", ci_lower_year, "-", ci_upper_year))

# Define segments (for averages)
pre_breakpoint <- yearly_avg_temp %>%
  filter(year < first_breakpoint_year)
post_breakpoint <- yearly_avg_temp %>%
  filter(year >= first_breakpoint_year)

# Calculate segment-specific means
pre_breakpoint_mean <- mean(pre_breakpoint$anomaly, na.rm = TRUE)
post_breakpoint_mean <- mean(post_breakpoint$anomaly, na.rm = TRUE)

# Create a summary data frame for segment averages
avg_data <- data.frame(
  segment_start = c(min(pre_breakpoint$year), first_breakpoint_year),
  segment_end = c(first_breakpoint_year - 1, max(post_breakpoint$year)),
  segment_mean = c(pre_breakpoint_mean, post_breakpoint_mean)
)

# Calculate mean anomaly for visualization
mean_anomaly <- mean(anomaly_df$anomaly, na.rm = TRUE)

# create CI data frame for ploting
ci_df <- data.frame(
  x = ci_lower_year,
  xend = ci_upper_year,
  y = -0.7,
  yend = -0.7
)

# Plotting anomaly and breakpoint for under-ice water temperature 
under_ice_break <- ggplot(anomaly_df, aes(x = year, y = anomaly)) +
  # Time series line
  geom_line(color = "black") +
  
  # Vertical breakpoint line
  geom_vline(xintercept = first_breakpoint_year, linetype = "dashed", color = "black") +
  
  geom_segment(
    data = ci_df,
    aes(x = x, xend = xend, y = y, yend = yend, color = "95% CI"),
    size = 0.5,
    inherit.aes = FALSE
  ) +
  
  geom_segment(
    data = avg_data,
    aes(x = segment_start, xend = segment_end,
        y = segment_mean, yend = segment_mean, color = "Mean of Segments"),
    linetype = "solid", size = 0.5, inherit.aes = FALSE
  ) +
  
  # Mean anomaly line using both color and linetype – suppress duplicate linetype legend
  geom_hline(aes(yintercept = mean_anomaly, 
                 color = "Mean of Anomalies", 
                 linetype = "Mean of Anomalies"), 
             size = 0.5) +
  
  # Unified manual legends
  scale_color_manual(
    values = c(
      "Mean of Segments" = "blue",
      "95% CI" = "red",
      "Mean of Anomalies" = "green"
    ),
    name = "Legend"
  ) +
  scale_linetype_manual(
    values = c("Mean of Anomalies" = "solid"),
    name = "Legend"
  ) +
  
  # Axis labels
  labs(x = "Year", y = "Under-ice bottom water temperature anomaly (°C)") +
  
  # Theme and legend position
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "bottom",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  ) +
  guides(
    color = guide_legend(title = "Legend"),
    linetype = "none"
  ) 

print(under_ice_break)

#PLOT FOR FINAL FIGURE WITHIN PANEL IN PAPER
file_path <- "C:/location/under_ice_break.png"

# Save the plot
ggsave(filename = file_path, plot = under_ice_break, width = 10, height = 8, dpi = 300)

# Trend map for under-ice bottom water temperature change (1985-2021) ------------------------
# Packages
install.packages("devtools")
devtools::install_github("ropensci/rnaturalearthhires")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library("readxl")
library("ggspatial")
library(readxl)
library(gridExtra)

# Read in your data

# This was just a summary table of the coordinates, sens slope trends, and significance of trends
UI_temp_loc_1985_2021 <- read_excel("C:/location/Summary_under_ice_trends_and_locations.xlsx")

# Remove row 6 (insufficient data (<10 years) at the site)
UI_temp_loc_1985_2021 <- UI_temp_loc_1985_2021[-6, ]

# Assuming world is defined as the spatial data for Finland
world <- ne_states(country = "Finland", returnclass = "sf")

# Trend map
UI_temp_map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = UI_temp_loc_1985_2021 %>% na.omit(), aes(x = long, y = lat, color = trend, shape = sig), size = 3) +
  scale_color_gradient(low = "blue", high = "red", name = "°C/decade", limits = c(-0.28, 0.33)) +
  labs(x = "Longitude", y = "Latitude", color = "°C/decade", shape = "Significant", subtitle = "1985-2021") +
  ggtitle("Trends in under-ice bottom water temperature") +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "npc"), pad_y = unit(0.01, "npc"),
                         height = unit(0.08, "npc"), width = unit(0.13, "npc"))

print(UI_temp_map)

# NOTE this is final figure in panel within the paper 