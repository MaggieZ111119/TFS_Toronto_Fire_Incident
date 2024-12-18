#### Preamble ####
# Purpose: Tests the structure and validity of the analysis(cleaned) TFS dataset 
# Author: Maggie Zhang
# Date: 30 November 2024
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `tidyverse` package must be installed and loaded
# - 03-clean_data.R must have been run


#### Workspace setup ####
library(tidyverse)
library(arrow)

analysis_data <- read_parquet("data/02-analysis_data/tfs_analysis_data.parquet")
raw_data <- read_csv("data/01-raw_data/raw_fire_data.csv")


# Test if the Analysis data was successfully loaded
if (exists("analysis_data")) {
  message("Test Passed: The analysis dataset was successfully loaded.")
} else {
  stop("Test Failed: The analysis dataset could not be loaded.")
}


### Test Analysis Data - Repetitiveness ###

# Check proportion of rows retained, consider representative if at least 40% o original retianed.
retention_rate <- (nrow(analysis_data) / nrow(raw_data)) * 100
if (retention_rate > 40) {
  message("Test Passed: The proportion of rows retained is ", 
          retention_rate, "%. Consider representitive.")
} else {
  message("Test Failed: Less than 40% of rows retained, but got ", 
          retention_rate, "% instead.")
}


### Test Analysis Data - Unique Rows###

# Check if all values in the '_id' column are unique
if (n_distinct(analysis_data$`_id`) == nrow(analysis_data)) {
  message("Test Passed: All values in 'id' are unique.")
} else {
  stop("Test Failed: The 'id' column contains duplicate values.")
}


### Test Analysis Data - Missing Values###

# Check if any column in the dataset contains NA values
if (any(is.na(analysis_data))) {
  message("Test Failed: The Analysis dataset contains NA values.")
} else {
  message("Test Passed: The Analysis dataset does not contain any NA values.")
}

# Check if any cell in the Analysis dataset is NULL
null_cells <- sapply(analysis_data, function(x) any(sapply(x, is.null)))
if (any(null_cells)) {
  message("Test Failed: Some cells in the Analysis dataset are NULL.")
} else {
  message("Test Passed: No cells in the Analysis dataset are NULL.")
}


### Test Analysis Data - Categorical Variables (Valid Categories)###

# Check if area_of_origin columns contains only valid categories as in actual data
valid_area_categories <- unique(analysis_data$area_of_origin)

if (all(analysis_data$area_of_origin %in% valid_area_categories)) {
  message("Test Passed: The 'area_of_origin' column contains only valid categories as actual dataset.")
} else {
  stop("Test Failed: The 'area_of_origin' column contains invalid categories.")
}

# Check if fire_alarm_system_operation contains only valid categories as in actual data
valid_fire_alarm_categories <- unique(analysis_data$fire_alarm_system_operation)
if (all(analysis_data$fire_alarm_system_operation %in% valid_fire_alarm_categories)) {
  message("Test Passed: The 'fire_alarm_system_operation' column contains only valid categories.")
} else {
  stop("Test Failed: The 'fire_alarm_system_operation' column contains invalid categories.")
}

# Incident type
valid_incident_type_categories <- unique(clean_data$final_incident_type)
if (all(analysis_data$final_incident_type %in% valid_incident_type_categories)) {
  message("Test Passed: The 'final_incident_type' column contains only valid categories.")
} else {
  stop("Test Failed: The 'final_incident_type' column contains invalid categories.")
}

# Ignition source
valid_ignition_source_categories <- unique(analysis_data$ignition_source)
if (all(analysis_data$ignition_source %in% valid_ignition_source_categories)) {
  message("Test Passed: The 'ignition_source' column contains only valid categories.")
} else {
  stop("Test Failed: The 'ignition_source' column contains invalid categories.")
}


# Possible cause
valid_possible_cause_categories <- unique(analysis_data$possible_cause)
if (all(analysis_data$possible_cause %in% valid_possible_cause_categories)) {
  message("Test Passed: The 'possible_cause' column contains only valid categories.")
} else {
  stop("Test Failed: The 'possible_cause' column contains invalid categories.")
}


# Sprinkler system presence
valid_sprinkler_system_categories <- unique(analysis_data$sprinkler_system_presence)
if (all(analysis_data$sprinkler_system_presence %in% valid_sprinkler_system_categories)) {
  message("Test Passed: The 'sprinkler_system_presence' column contains only valid categories.")
} else {
  stop("Test Failed: The 'sprinkler_system_presence' column contains invalid categories.")
}


### Test Analysis Data - Numerical Variables (Positive Value)###

# Check for negative values in 'civilian_casualties' column
if (any(analysis_data$civilian_casualties < 0, na.rm = TRUE)) {
  stop("Test Failed: There are negative values in the 'civilian_casualties' column.")
} else {
  message("Test Passed: No negative values in the 'civilian_casualties' column.")
}

# Check for negative values in 'estimated_dollar_loss' column
if (any(analysis_data$estimated_dollar_loss < 0, na.rm = TRUE)) {
  stop("Test Failed: There are negative values in the 'estimated_dollar_loss' column.")
} else {
  message("Test Passed: No negative values in the 'estimated_dollar_loss' column.")
}

# Check for negative values in 'number_of_responding_apparatus' column
if (any(analysis_data$number_of_responding_apparatus < 0, na.rm = TRUE)) {
  stop("Test Failed: There are negative values in the 'number_of_responding_apparatus' column.")
} else {
  message("Test Passed: No negative values in the 'number_of_responding_apparatus' column.")
}

# Check for negative values in 'number_of_responding_personnel' column
if (any(analysis_data$number_of_responding_personnel < 0, na.rm = TRUE)) {
  stop("Test Failed: There are negative values in the 'number_of_responding_personnel' column.")
} else {
  message("Test Passed: No negative values in the 'number_of_responding_personnel' column.")
}

# Check for negative values in 'tfs_firefighter_casualties' column
if (any(analysis_data$tfs_firefighter_casualties < 0, na.rm = TRUE)) {
  stop("Test Failed: There are negative values in the 'tfs_firefighter_casualties' column.")
} else {
  message("Test Passed: No negative values in the 'tfs_firefighter_casualties' column.")
}

# Check for negative values in 'response_time' column
if (any(analysis_data$response_time < 0, na.rm = TRUE)) {
  stop("Test Failed: There are negative values in the 'response_time' column.")
} else {
  message("Test Passed: No negative values in the 'response_time' column.")
}


### Test Analysis Data - Date-Time Variables (Format and Consistency)###

# Check if 'tfs_alarm_time' follows the correct format
if (any(is.na(as.POSIXct(analysis_data$tfs_alarm_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))) {
  stop("Test Failed: Invalid date format in the 'tfs_alarm_time' column.")
} else {
  message("Test Passed: Date format is valid in the 'tfs_alarm_time' column.")
}

# Check if 'tfs_arrival_time' follows the correct format
if (any(is.na(as.POSIXct(analysis_data$tfs_arrival_time, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")))) {
  stop("Test Failed: Invalid date format in the 'tfs_arrival_time' column.")
} else {
  message("Test Passed: Date format is valid in the 'tfs_arrival_time' column.")
}

# Check if all arrival times occur after alarm times
invalid_time_sequence <- 
  sum(analysis_data$tfs_arrival_time < analysis_data$tfs_alarm_time, 
      na.rm = TRUE)

if (invalid_time_sequence == 0) {
  message("Test Passed: All arrival times occur after alarm times.")
} else {
  message("Test Failed: There are ", invalid_time_sequence, 
          " cases where arrival time is before alarm time.")
}


# Check alarm time is in a realistic range
min_alarm_time <- min(analysis_data$tfs_alarm_time, na.rm = TRUE)
max_alarm_time <- max(analysis_data$tfs_alarm_time, na.rm = TRUE)
expected_alarm_start <- as.POSIXct("2011-01-01 00:00:00", tz = "UTC")
expected_alarm_end <- as.POSIXct("2024-11-30 23:59:59", tz = "UTC")

if (min_alarm_time >= expected_alarm_start & max_alarm_time <= expected_alarm_end) {
  message("Test Passed: Alarm times are within the realistic range.")
} else {
  message("Test Failed: Alarm times are outside the realistic range. ",
          "Minimum: ", min_alarm_time, 
          ", Maximum: ", max_alarm_time)
}

# Check arrival time is in a realistic range
min_arrive_time <- min(analysis_data$tfs_arrival_time, na.rm = TRUE)
max_arrive_time <- max(analysis_data$tfs_arrival_time, na.rm = TRUE)
expected_arrive_start <- as.POSIXct("2011-01-01 00:00:00", tz = "UTC")
expected_arrive_end <- as.POSIXct("2024-11-30 23:59:59", tz = "UTC")

if (min_arrive_time >= expected_arrive_start & 
    max_arrive_time <= expected_arrive_end) {
  message(
    "Test Passed: Arrival times are within the realistic range.")
} else {
  message("Test Failed: Arrival times are outside the realistic range. ",
          "Minimum: ", min_arrive_time, 
          ", Maximum: ", max_arrive_time)
}

### Check for Unreasonable zeros ###
# Check for zero values in 'response_time'
if (any(analysis_data$response_time == 0, na.rm = TRUE)) {
  stop("Test Failed: Zero values found in the 'response_time' column.")
} else {
  message("Test Passed: No zero values in the 'response_time' column.")
}

# Check for zero values in 'id'
if (any(analysis_data$`_id` == 0, na.rm = TRUE)) {
  stop("Test Failed: Zero values found in the 'id' column.")
} else {
  message("Test Passed: No zero values in the 'id' column.")
}
