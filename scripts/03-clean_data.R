#### Preamble ####
# Purpose: Cleans the raw plane data by Open Data Toronto
# Author: Maggie Zhang
# Date: 30 November 2024
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have the data downloaded
# Any other information needed? No

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(lubridate)
library(arrow)

#### Load data ####
raw_data <- read_csv("data/01-raw_data/raw_fire_data.csv")

### Keep Columns in interest ###
columns_to_keep <- c("_id", "Area_of_Origin", "Building_Status", "Business_Impact", 
                     "Civilian_Casualties", "Estimated_Dollar_Loss", "Exposures", 
                     "Final_Incident_Type", "Fire_Alarm_System_Operation", 
                     "Ignition_Source",
                     "Number_of_responding_apparatus", "Number_of_responding_personnel", 
                     "Possible_Cause", "Sprinkler_System_Presence", 
                     "TFS_Alarm_Time", "TFS_Arrival_Time", "TFS_Firefighter_Casualties")

clean_data <- raw_data[, columns_to_keep]
# Change columns name to lower case and ensure no space
colnames(clean_data) <- tolower(gsub(" ", "_", colnames(clean_data)))

### Preview data ###
colnames(clean_data)
head(clean_data, 6)
summary(clean_data)   

# Since I want to study possible sources leading to different severity of fire, no info about sources should be dropped
clean_data <- clean_data %>%
  filter(ignition_source != "999 - Undetermined")

### Ensure Datetime Consistency ###

# Convert timestamps to POSIXct format (YYYY-MM-DD HH:MM:SS)
clean_data$tfs_alarm_time <- ymd_hms(clean_data$tfs_alarm_time)  
clean_data$tfs_arrival_time <- ymd_hms(clean_data$tfs_arrival_time) 
clean_data <- clean_data[!is.na(clean_data$tfs_alarm_time), ]
clean_data <- clean_data[!is.na(clean_data$tfs_arrival_time), ]



### Missing Value ###
# Calculate missing value for each columns, and return its percentage of missing
missing_values <- colSums(is.na(clean_data))
missing_percentage <- (missing_values / nrow(clean_data)) * 100
missing_info <- data.frame(Column = names(missing_percentage), 
                           Missing_Percentage = missing_percentage)
print(missing_info)

## Identify columns with more than 20% missing values and remove the columns
cols_to_remove <- missing_info$Column[missing_info$Missing_Percentage > 20]
cols_to_remove

# building_status, business_impact, and exposures are not super relevant to research topic, so just remove the columns
# "fire_alarm_system_operation" and "sprinkler_system_presence" are relavent, so keep it, replace NAs, by "unreported" 
clean_data <- clean_data[, !(colnames(clean_data) 
                             %in% c("building_status", 
                                    "business_impact", 
                                    "exposures"))]

cols_to_replace <- c("fire_alarm_system_operation", 
                     "sprinkler_system_presence")
clean_data[cols_to_replace] <- lapply(clean_data[cols_to_replace], 
                                      function(x) ifelse(is.na(x), 
                                                         "unreported", x))

## Remove rows if more than 30% of their values are missing
threshold <- 0.30
# Identify rows with more than 30% missing data
rows_to_remove <- rowSums(is.na(clean_data)) / ncol(clean_data) > threshold
# Remove rows with more than 30% missing data
clean_data <- clean_data[!rows_to_remove, ]

## Show all current NAs
new_missing_values <- colSums(is.na(clean_data))
# Display the count of missing values for each column
print(new_missing_values)
# Since NAs is not in a great proportion, it is save to get omit NA rows
clean_data <- na.omit(clean_data)


### Create New Columns For Analysis ###
## Time it takes for the Toronto Fire Services (TFS) to reach the fire location after notified of the incident
# Calculate the time difference between alarm time and arrival time
clean_data$response_time <- as.numeric(difftime(clean_data$tfs_arrival_time, 
                            clean_data$tfs_alarm_time, 
                            units = "mins"))


#### Save data ####
write_parquet(clean_data, "data/02-analysis_data/tfs_analysis_data.parquet")