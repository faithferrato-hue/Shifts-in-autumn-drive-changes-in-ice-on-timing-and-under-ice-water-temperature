-------------------------------------------------------------------
# Script: 01_ice_on_doy_code.R
# Author: Faith Ferrato
# Purpose: Calculate ice-on day of year (DOY) 
# Prepare clean dataset for trend and anomaly/breakpoint analysis.
-------------------------------------------------------------------
# Load packages ----
library(readxl)
library(dplyr)
library(lubridate)

# Load file from computer 
# Change location to the specific file 
file_path <- "C:/location/Syke IB 1800-2022 .xlsx"

# Get all sheet names, excluding the first one (notes)
sheet_names <- excel_sheets(file_path)[-1]

# Function to read and standardize each sheet
standardize_and_select <- function(sheet_name) {
  df <- read_excel(file_path, sheet = sheet_name)
  
  # Select the second column (numeric date) and third column (lippu which indicates ice-on or ice-off)
  df_selected <- df %>%
    select(date_numeric = 2, lippu = 3) %>%
    mutate(
      date = ymd(date_numeric),  # Convert numeric date (YYYYMMDD) to proper date format
      lake = sheet_name  # Add lake name from the sheet name
    )
  
  return(df_selected)
}

# Combine all sheets into one data frame
ice_df <- bind_rows(lapply(sheet_names, standardize_and_select))

# Remove NA from misplaced columns 
ice_df <- ice_df %>%
  filter(!is.na(date))

# Aligning ice-on records (x7 and x5) ----  
# Mutating the data set to ensure that X7 and X5 dates line up, even if in opposing years

# Filtering ice-on dates only (whole site and whole site permanently)
ice_on_df <- ice_df %>%
  filter(lippu %in% c("x7", "x5"))

# Convert to ice-on doy
calculate_ice_on_doy <- function(date) {
  year <- year(date)  # Get the year
  doy <- yday(date)   # Get the day of year
  
  # If the date is between January and April, adjust for previous year (adding 365 + julian date)
  if (month(date) %in% 1:4) {
    if (leap_year(year - 1)) {
      # Add 366 if the previous year was a leap year
      doy <- doy + 366
    } else {
      # Add 365 for non-leap years
      doy <- doy + 365
    }
  }
  
  return(doy)
}

# Apply the function to the 'date' column to create 'ice_on_doy'
ice_on_df <- ice_on_df %>%
  mutate(ice_on_doy = sapply(date, calculate_ice_on_doy))

# Separate X7 and X5 data
x7_df <- ice_on_df %>%
  filter(lippu == "x7") %>%
  select(lippu, date, lake, ice_on_doy)

x5_df <- ice_on_df %>%
  filter(lippu == "x5") %>%
  select(lippu, date, lake, ice_on_doy)

# Align X5 and x7 dates to match the freezing year
x5_df <- x5_df %>%
  mutate(freezing_year = ifelse(month(date) %in% 1:4, year(date) - 1, year(date)))

x7_df <- x7_df %>%
  mutate(freezing_year = ifelse(month(date) %in% 1:4, year(date) - 1, year(date)))

# Combine X7 and X5 data by lake and freezing year
combined_df <- full_join(x7_df, x5_df, by = c("lake", "freezing_year"), suffix = c("_x7", "_x5"))

# Prioritize X7 (use X5 only when X7 is missing)
ice_on_final_df <- combined_df %>%
  mutate(
    ice_on_date = as.Date(ifelse(!is.na(date_x7), date_x7, date_x5), origin = "1970-01-01"),     # Choose X7 date if available
    ice_on_doy = ifelse(!is.na(ice_on_doy_x7), ice_on_doy_x7, ice_on_doy_x5),  # Choose X7 DOY if available
    ice_on_lippu = ifelse(!is.na(lippu_x7), lippu_x7, lippu_x5)  # Track whether X7 or X5 is used
  ) %>%
  select(lake, freezing_year, ice_on_date, ice_on_doy, ice_on_lippu)

print(ice_on_final_df)

# This df was downloaded and used for analysis within script "07_SWT_trend_analysis_and_map" 
write.csv(
  ice_on_final_df,
  file = "C:/location/ice_on_final_df.csv",
  row.names = FALSE
)

# Ice-On General Statistics and Trend Analysis (1972–2021)----

library(dplyr)
library(tidyr)
library(zyp)
library(Kendall)
library(trend)

# Filter and prepare data ----

# Filter data for the study period (1972–2021)
ice_on_stats <- ice_on_final_df %>%
  filter(freezing_year >= 1972 & freezing_year <= 2021)

# Create full lake–year combinations (to capture missing years)
all_years <- data.frame(freezing_year = 1972:2021)
unique_lakes <- data.frame(lake = unique(ice_on_stats$lake))
all_combinations <- crossing(unique_lakes, all_years)

# Remove duplicates (keep the first entry for each lake-year)
ice_on_stats_unique <- ice_on_stats %>%
  group_by(lake, freezing_year) %>%
  summarize(
    ice_on_date = first(ice_on_date),
    ice_on_doy = first(ice_on_doy),
    ice_on_lippu = first(ice_on_lippu),
    .groups = "drop"
  )

# Join with full combinations to ensure complete lake-year coverage
ice_on_fixed <- left_join(all_combinations, ice_on_stats_unique, by = c("lake", "freezing_year"))

# Calculate Missing Data and Summary Statistics ----

# (a) Count missing ice-on DOY values for each lake
missing_counts <- ice_on_fixed %>%
  group_by(lake) %>%
  summarize(
    missing_ice_on_doy = sum(is.na(ice_on_doy)),
    .groups = "drop"
  )

# Compute mean ice-on DOY per lake (for filling missing values)
average_ice_on_doy <- ice_on_fixed %>%
  group_by(lake) %>%
  summarize(
    average_ice_on_doy = mean(ice_on_doy, na.rm = TRUE),
    .groups = "drop"
  )

# Replace missing values with the lake-specific mean
ice_on_fixed_missing <- ice_on_fixed %>%
  left_join(average_ice_on_doy, by = "lake") %>%
  mutate(ice_on_doy = ifelse(is.na(ice_on_doy), average_ice_on_doy, ice_on_doy)) %>%
  select(-average_ice_on_doy)

# Summary statistics for each lake
stats_summary <- ice_on_fixed_missing %>%
  group_by(lake) %>%
  summarize(
    mean_ice_on_doy   = mean(ice_on_doy, na.rm = TRUE),
    sd_ice_on_doy     = sd(ice_on_doy, na.rm = TRUE),
    se_ice_on_doy     = sd(ice_on_doy, na.rm = TRUE) / sqrt(n()),
    min_ice_on_doy    = min(ice_on_doy, na.rm = TRUE),
    max_ice_on_doy    = max(ice_on_doy, na.rm = TRUE),
    median_ice_on_doy = median(ice_on_doy, na.rm = TRUE),
    .groups = "drop"
  )


# Combine missing counts and summary stats (complete table)
combined_stats <- left_join(missing_counts, stats_summary, by = "lake")

# Sen’s Slope Analysis (50 years and 25 Years) ----

# Define function to compute Sen’s slope and Mann–Kendall p-value
calculate_sens_slope <- function(data) {
  if (nrow(data) < 3) {  # Check if there are at least 3 rows of data
    return(data.frame(sens_slope = NA, p_value = NA, num_years = NA, start_date = NA, end_date = NA))
  } else {
    # Calculate Sen's slope
    sens_result <- sens.slope(data$'ice_on_doy')
    sens_slope <- sens_result$estimate
    
    # Calculate p-value using Mann-Kendall test
    mk_result <- mk.test(data$'ice_on_doy')
    p_value <- mk_result$p.value
    
    # Calculate number of years, start date, and end date
    start_date <- min(data$freezing_year)
    end_date <- max(data$freezing_year)
    num_years <- n_distinct(data$freezing_year)
    
    return(data.frame(sens_slope = sens_slope, p_value = p_value, num_years = num_years, start_date = start_date, end_date = end_date))
  }
}


# Sen’s slope for the full 1972–2021 period (each lake)
sens_slope_results <- ice_on_fixed_missing %>%
  group_by(lake) %>%
  do(calculate_sens_slope(.)) %>%
  ungroup()

# Sen’s slope for the last 25 years (1997–2021)
sens_slope_results_last_25_years <- ice_on_fixed_missing %>%
  filter(freezing_year >= 1997 & freezing_year <= 2021) %>%  # Filter data for the last 25 years
  group_by(lake) %>%
  do(calculate_sens_slope(.)) %>%
  ungroup()

# Rename columns in 25 year sens slope dataframes to avoid conflicts
sens_slope_results_last_25_years_renamed <- sens_slope_results_last_25_years %>%
  rename(
    sens_slope_25_years = sens_slope,
    p_value_25_years = p_value,
    num_years_25_years = num_years,
    start_date_25_years = start_date,
    end_date_25_years = end_date
  )


# Combine all Results ----

ice_on_data_stats_final <- combined_stats %>%
  left_join(sens_slope_results_last_25_years_renamed, by = "lake") %>%
  left_join(sens_slope_results, by = "lake")

write.csv(ice_on_data_stats_final, "C:/loacation", row.names = FALSE)

# Clean up and add averages ----

# Remove lakes with < 5 years of data or rivers (indices 19 and 43)
ice_on_data_stats_final_clean <- ice_on_data_stats_final[-c(19, 43), ]

# Compute average across numeric columns
numeric_cols <- sapply(ice_on_data_stats_final_clean, is.numeric)
average_row <- colMeans(ice_on_data_stats_final_clean[, numeric_cols], na.rm = TRUE)

# Add "Average" as lake label and bind to dataset
average_row <- c("Average", average_row)
ice_on_data_stats_final_with_avg <- rbind(ice_on_data_stats_final_clean, average_row)

# View final table
print(ice_on_data_stats_final_with_avg)

# Ice-On anomaly and breakpoint analysis (1972–2021)-----

#libraries
library(ggplot2)
library(dplyr)
library(strucchange)

# Filter and prepare data ----

# Exclude lakes with <5 years of data or rivers
filtered_ice_on_data <- ice_on_fixed_missing %>%
  filter(!lake %in% c("Tornionjoki", "Lestijarvi"))

# Compute baseline mean (1972–2021) across all lakes
baseline_avg <- filtered_ice_on_data %>%
  filter(freezing_year >= 1972 & freezing_year <= 2021) %>%
  summarise(baseline_ice_on = mean(ice_on_doy, na.rm = TRUE)) %>%
  pull(baseline_ice_on)

# Calculate annual mean ice-on day across all lakes
yearly_avg <- filtered_ice_on_data %>%
  group_by(freezing_year) %>%
  summarise(yearly_avg_ice_on = mean(ice_on_doy, na.rm = TRUE))

# Calculate anomalies (difference of yearly from baseline)
yearly_avg <- yearly_avg %>%
  mutate(anomaly = yearly_avg_ice_on - baseline_avg)

# Create anomaly dataframe and categorize by timing ----

anomaly_df <- filtered_ice_on_data %>%
  group_by(freezing_year) %>%
  summarize(avg_ice_on_doy = mean(ice_on_doy, na.rm = TRUE)) %>%
  mutate(
    baseline_avg = mean(avg_ice_on_doy, na.rm = TRUE),
    anomaly = avg_ice_on_doy - baseline_avg,
    freeze_timing = if_else(anomaly > 0, 'Late freeze', 'Early freeze')
  )

# Make a factor
anomaly_df <- anomaly_df %>%
  mutate(
    freeze_timing = if_else(anomaly > 0, "Late freeze", "Early freeze") %>%
      factor()  # Ensure it's a factor
  )

# Plot anomaly time series ----

ice_anomaly_plot <- ggplot(anomaly_df, aes(x = freezing_year, y = anomaly, fill = freeze_timing)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(
    values = c("Early freeze" = "blue", "Late freeze" = "red"),
    name = "Freeze Timing"
  ) +
  labs(
    x = "Year",
    y = "Ice-on day of year anomaly (days)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",  # move legend below plot
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    
    # Add these lines
    axis.title = element_text(size = 16),  # axis titles
    axis.text = element_text(size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)# axis tick labels
  )

print(ice_anomaly_plot)

#NOTE: this is final figure within panel in paper

# Identify breakpoints in anomaly time series ----

# Create a time series object
ice_on_ts <- ts(yearly_avg$anomaly, start = min(yearly_avg$freezing_year), frequency = 1)

# Fit breakpoint model (intercept-only)
breakpoints_result <- breakpoints(ice_on_ts ~ 1)
print(breakpoints_result)

# Extract estimated breakpoint years
start_year <- 1972  
breakpoint_years <- start_year + breakpoints_result$breakpoints - 1
first_breakpoint_year <- breakpoint_years[1]

# Compute confidence intervals for breakpoint
ci_breakpoints <- confint(breakpoints_result)
ci_first_breakpoint <- ci_breakpoints$confint[1, ]  # Assuming it's the first breakpoint, adjust index if needed
print(ci_breakpoints$confint)

# Extract the lower and upper bounds (as indices)
ci_lower <- ci_first_breakpoint["2.5 %"]  
ci_upper <- ci_first_breakpoint["97.5 %"]

# Convert indices to years
ci_lower_year <- start_year + ci_lower - 1  
ci_upper_year <- start_year + ci_upper - 1  

# Print the confidence interval as years
print(paste("Confidence interval for first breakpoint: ", ci_lower_year, "-", ci_upper_year))

# Segment Means and Confidence Interval Visualization ----

# Split pre- and post-breakpoint segments
pre_breakpoint <- yearly_avg %>%
  filter(freezing_year < first_breakpoint_year)  # Data before breakpoint
post_breakpoint <- yearly_avg %>%
  filter(freezing_year >= first_breakpoint_year)  # Data after breakpoint

# Compute mean anomaly for each segment
pre_breakpoint_mean <- mean(pre_breakpoint$anomaly, na.rm = TRUE)
post_breakpoint_mean <- mean(post_breakpoint$anomaly, na.rm = TRUE)

# Create dataframe for segment means
avg_data <- data.frame(
  segment_start = c(min(pre_breakpoint$freezing_year), first_breakpoint_year),
  segment_end = c(first_breakpoint_year - 1, max(post_breakpoint$freezing_year)),
  segment_mean = c(pre_breakpoint_mean, post_breakpoint_mean)
)
print(avg_data)

# Ensure the segment_end aligns with the breakpoint year
avg_data$segment_end[avg_data$segment_end == first_breakpoint_year] <- first_breakpoint_year

# Mean anomaly (for reference line)
mean_anomaly <- mean(anomaly_df$anomaly, na.rm = TRUE)

# Dataframe for CI visualization
ci_df <- data.frame(
  x = ci_lower_year,
  xend = ci_upper_year,
  y = -22,
  yend = -22
)

# Plot breakpoint and segment means ----

ice_breakpoint_plot <- ggplot(anomaly_df, aes(x = freezing_year, y = anomaly)) +
  
  geom_line(color = "black") +
  
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
  
  geom_hline(aes(yintercept = mean_anomaly, 
                 color = "Mean of Anomalies", 
                 linetype = "Mean of Anomalies"), 
             size = 0.5) +
  
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
  
  labs(
    x = "Year", 
    y = "Ice-on day of year anomaly (days)"
  ) +
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

print(ice_breakpoint_plot)

# NOTE: the plot above was final figure within the paper. 
