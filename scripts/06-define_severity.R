#### Preamble ####
# Purpose: Models... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(rpart)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fitdistrplus)

#### Read data ####
fire_data_model <- read_csv("data/02-analysis_data/updated_tfs_analysis_data")

#### Quantify Severity ####

### Apparatus
fire_data_model <- fire_data_model %>%
  mutate(severity_bin = cut(number_of_responding_apparatus,
                            breaks = c(0, 10, 20, 30, 40, 50, Inf),
                            labels = c("[1,10]", "[11,20]", "[21,30]", "[31,40]", "[41,50]", ">50"),
                            right = TRUE))
severity_dist <- fire_data_model %>%
  group_by(severity_bin) %>%
  summarise(frequency = n()) %>%
  ungroup()
#severity_dist


### Estimated_dollar_loss
# Log transform: 
fire_data_model$log_estimated_dollar_loss <- log(fire_data_model$estimated_dollar_loss + 1)
# Recalculate quantiles on transformed data
loss_quantiles <- quantile(fire_data_model$log_estimated_dollar_loss, probs = c(0.25, 0.5, 0.75))

###  Total Casualties
table(fire_data_model$total_casualties)

### Determining Severity
fire_data_model <- fire_data_model %>%
  mutate(
    # Define Casualty Severity
    casualty_severity = case_when(
      total_casualties == 0 | total_casualties == 1 ~ "Low", 
      total_casualties >= 2 & total_casualties <= 4 ~ "Medium",
      total_casualties >= 5 ~ "High",
      TRUE ~ "Low"
    ),
    
    # Define Final Severity based on Apparatus, Dollar Loss, and Casualties
    Severity = case_when(
      # Explosion = High severity
      final_incident_type == 
        "02 - Explosion (including during Fire, excluding Codes 3 & 11-13)"
      ~ "High", 
      
      # Apparatus severity classifications based on bins
      (casualty_severity == "Low" 
       | severity_bin %in% c("[1,10]", "[11,20]") 
       | log_estimated_dollar_loss <= loss_quantiles[1]) ~ "Low",
      
      (casualty_severity == "Medium" 
       | severity_bin %in% c("[21,30]", "[31,40]") 
       | log_estimated_dollar_loss <= loss_quantiles[2]) ~ "Medium",
      
      (casualty_severity == "High" 
       | severity_bin %in% c("[41,50]", ">50") 
       | log_estimated_dollar_loss > loss_quantiles[2]) ~ "High",
      
      TRUE ~ "Low" 
    )
  )


#### Save Data ####
write_csv(fire_data_model, "data/02-analysis_data/severity_tfs_analysis_data")



