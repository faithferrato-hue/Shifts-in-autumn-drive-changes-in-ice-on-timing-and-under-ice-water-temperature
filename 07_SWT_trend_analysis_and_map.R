-----------------------------------------------------------
# Script: 07_SWT_trend_analysis_and_map
# Author: Faith Ferrato
# Purpose: identifying specific lakes of interest and averaging SWT for each lake 
# for trend analysis 
-------------------------------------------------------------
# Load packages ----
library(dplyr)
library(tidyr)
library(lubridate)
library(here)
library(janitor)
library(readxl)
library(dplyr)
library(trend)
library(tidyverse)
library(ggplot2)
library(ggpubr)
  
# Load and processes data ----
# Read data (this data set was directly from SYKE)
swt_data <- read_excel("C:/location/SWT Finland 1916-2023.xlsx")%>%
  clean_names() 

# Select for specific variables 
swt_data_clean <- swt_data[, c("station_name", "time", "temperature_co")]

# Select for specific lakes that have ice phenology data (this was cross referenced manually)
# Essentially identifying which lakes within the surface water temperature df above 
# had associated ice phenology data from the file "Syke IB 1800-2022 .xlsx"

# Define the lake names of interest (the selected lakes below had ice phenology data associated with the locations)
selected_lakes <- c("Haukivesi, Oravi",
                    "Nuasjärvi,Vuokatti",
                    "Pielinen, Nurmes",
                    "Kallavesi, Kuopio",
                    "Saimaa, Lauritsala",
                    "Jääsjärvi, Hartola",
                    "Päijänne, Sysmä",
                    "Pielavesi, Säviä",
                    "Lappajärvi, Halkosaari",
                    "Oijärvi",
                    "Inari, Nellim",
                    "Kevojärvi, Kevoniemi",
                    "Konnevesi, etelä", 
                    "Säkylän Pyhäjärvi")


# Filter the dataset for only these lakes
swt_data_filtered <- swt_data_clean %>% 
  filter(station_name %in% selected_lakes)
unique(swt_data_filtered$station_name)

# Sort by date 
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

# Renaming stations name to matching ice phenology dataset, so we can join 
swt_data_filtered <- swt_data_filtered %>%
  mutate(station_name = case_when(
    station_name == "Haukivesi, Oravi" ~ "Haukivesi Oravi",
    station_name == "Nuasjärvi,Vuokatti" ~ "Nuasjarvi Vuokatti",
    station_name == "Pielinen, Nurmes" ~ "Pielinen Nurmes",
    station_name == "Kallavesi, Kuopio" ~ "Kallavesi Kuopio",
    station_name == "Saimaa, Lauritsala" ~ "Saimaa Lauritsala",
    station_name == "Jääsjärvi, Hartola" ~ "Jaasjarvi Hartola",
    station_name == "Päijänne, Sysmä" ~ "Paijanne Sysma",
    station_name == "Pielavesi, Säviä" ~ "Pielavesi Savia",
    station_name == "Lappajärvi, Halkosaari" ~ "Lappajarvi",
    station_name == "Oijärvi" ~ "Oijarvi Matilanjarvi",
    station_name == "Inari, Nellim" ~ "Inari Nellim",
    station_name == "Kevojärvi, Kevoniemi" ~ "Kevojarvi Kevoniemi",
    station_name == "Konnevesi, etelä" ~ "Konnevesi Etela",
    station_name == "Säkylän Pyhäjärvi" ~ "Pyhajarvi Kauttua",
    TRUE ~ station_name
  )) %>%
  rename(lake = station_name)
print(swt_data_filtered)

# Ensure numeric
swt_data_filtered <- swt_data_filtered %>%
  mutate(
    year = as.numeric(year),   # Ensure year is numeric
    month = as.numeric(month)  # Ensure month is numeric
  )

# Mutate to water year
swt_data_adjusted <- swt_data_filtered %>%
  mutate(water_year = ifelse(month >= 10, year + 1, year)) 

# Find max temperature and day of max temperature 
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

# Load ice phenology df and select for lake, ice_on date, and ice_on_doy
# This df was curated in the script "01_ice_on_doy_code.R"
file_path <- "C:/location/ice_on_final_df.csv" 
data <- read.csv(file_path, stringsAsFactors = FALSE)

# Then rename df to ice_on_swt
ice_on_swt <- data[, c("lake", "ice_on_date", "ice_on_doy")]

# Add water year 
ice_on_swt <- ice_on_swt %>%
  mutate(water_year = ifelse(month(ice_on_date) >= 10, year(ice_on_date) + 1, year(ice_on_date)))

# Join datasets by lake and water year
swt_data_merged <- swt_data_adjusted %>%
  left_join(ice_on_swt %>% select(lake, water_year, ice_on_doy, ice_on_date),
            by = c("lake", "water_year"))

# Check that merge was sucessful 
unique(swt_data_merged$lake)

# Filter for fall period (October 1 to ice-on)
swt_fall_avg_final <- swt_data_merged %>%
  filter(as.Date(paste0(water_year - 1, "-", month, "-", day)) >= as.Date(paste0(water_year - 1, "-10-01")) &
           as.Date(paste0(water_year - 1, "-", month, "-", day)) <= ice_on_date) %>%
  group_by(lake, water_year) %>%
  summarise(swt_fall_avg = mean(temperature_co, na.rm = TRUE), .groups = "drop")

print(swt_fall_avg_final)
unique(swt_fall_avg_final$lake)

# Visualize correlation plot of ice_on and autumn SWT ----

# Identify Water year and calculate average from oct1-freeze
swt_fall_avg_ice_on_doy_cor <- swt_data_merged %>%
  filter(as.Date(paste0(water_year - 1, "-", month, "-", day)) >= as.Date(paste0(water_year - 1, "-10-01")) &
           as.Date(paste0(water_year - 1, "-", month, "-", day)) <= ice_on_date) %>%
  group_by(lake, water_year, ice_on_doy) %>%  # Include ice_on_doy in group_by
  summarise(
    swt_fall_avg = mean(temperature_co, na.rm = TRUE),
    .groups = "drop"
  )

# Verify the output
print(swt_fall_avg_ice_on_doy_cor)
unique(swt_fall_avg_ice_on_doy_cor$lake)

# Select range of years 
swt_fall_avg_ice_on_doy_cor <- swt_fall_avg_ice_on_doy_cor %>%
  filter(water_year >= 1973 & water_year <= 2022)

autumn_swt_ice_on_plot <- ggplot(swt_fall_avg_ice_on_doy_cor, aes(x = swt_fall_avg, y = ice_on_doy)) +
  geom_point(shape = 19, size = 2, alpha = 0.7) +
  stat_cor(
    method = "spearman",
    label.x.npc = "left",
    label.y.npc = "top",
    r.accuracy = 0.01,
    p.accuracy = 0.001,
    aes(label = paste(gsub("R", "r", ..r.label..), ..p.label.., sep = "~`,`~"))
  ) +
  labs(
    x = "Average autumn surface water temperature (°C)", 
    y = "Ice-on day of year"
  ) +
  theme_classic(base_size = 14) +
  theme(
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.8),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 14)
  )

print(autumn_swt_ice_on_plot)

# This is final plot within panel in paper  
ggsave("C:/location/fall_swt_and_ice_on_cor_plot.png", 
       plot = autumn_swt_ice_on_plot, width = 8, height = 6, dpi = 300)

# Sen's Slope Calculation for Average Autumn Surface water temperature ----

# For this calculation we are only selecting for 8 lakes with at least 50 years of data 
swt_fall_avg_filtered <- swt_fall_avg_final %>%
  filter(!lake %in% c("Nuasjarvi Vuokatti", "Haukivesi Oravi", "Pielavesi Savia", "Oijarvi Matilanjarvi", "Konnevesi Etela", "Pyhajarvi Kauttua" ))
unique(swt_fall_avg_filtered$lake)

# Filter data between 1973 and 2022
swt_fall_avg_filtered <- swt_fall_avg_filtered %>%
  filter(water_year >= 1973 & water_year <= 2022)

# Create a complete time series for each lake **without filling missing values**
swt_fall_missing_count <- swt_fall_avg_filtered %>%
  group_by(lake) %>%
  complete(water_year = 1973:2022) %>%  # Ensure each lake has all years
  summarise(total_missing = sum(is.na(swt_fall_avg)), .groups = "drop")

# Print the missing data summary (making sure no large gaps)
print(swt_fall_missing_count)

# Fill missing values with the mean of each lake
swt_fall_complete <- swt_fall_avg_filtered %>%
  group_by(lake) %>%
  complete(water_year = 1973:2022) %>%  
  mutate(swt_fall_avg = ifelse(is.na(swt_fall_avg), mean(swt_fall_avg, na.rm = TRUE), swt_fall_avg)) %>% 
  ungroup()

# Calculate Sen's slope & p-value for each lake
sens_slope_results <- swt_fall_complete %>%
  group_by(lake) %>%
  summarize(
    sens_slope = sens.slope(swt_fall_avg)$estimates,  # Estimate Sen’s slope
    p_value = sens.slope(swt_fall_avg)$p.value        # Get p-value
  ) %>%
  ungroup()

# Print results
print(sens_slope_results)

# Download
file_path <- "C:/location/sens_slope_results_swt.csv"  
# Write to CSV
write.csv(sens_slope_results, file = file_path, row.names = FALSE)

# Create map of trends for the 8 lake sites ----
library(tidyverse)
library("rnaturalearth")
library("rnaturalearthdata")
library("readxl")
library("ggspatial")
library(readxl)
library(gridExtra)

# Read in your data (file downloaded from above)
# This was a summary table of the coordinates, sens slope trends, and significance of trends
file_path <- "C:\location\sens_slope_results_swt.csv"

# Read the CSV file
sens_swt_results <- read.csv(file_path)

# Assuming world is defined as the spatial data for Finland
world <- ne_states(country = "Finland", returnclass = "sf")

# Plotting code for 50-year swt map
swt_trend_map <- ggplot(data = world) +
  geom_sf() +
  geom_point(data = sens_swt_results %>% na.omit(), aes(x = lng, y = lat, color = sen_dec, shape = sig), size = 3) +
  scale_color_gradient(low = "blue", high = "red", name = "°C/decade", limits = c(0.05, 0.8)) +
  labs(x = "Longitude", y = "Latitude", color = "°C/decade", shape = "Significant", subtitle = "1972-2021") +
  ggtitle("Autumn surface water temperature trends") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5)) + 
  annotation_north_arrow(location = "tr", which_north = "true", 
                         pad_x = unit(0.05, "npc"), pad_y = unit(0.01, "npc"),
                         height = unit(0.08, "npc"), width = unit(0.13, "npc"))

# Display the panel
print(swt_trend_map)

ggsave("C:/location/swt_trend_map.png", 
      plot = trend_map_50, width = 12, height = 6, dpi = 300)

# This was a final figure within the panel in the paper 
