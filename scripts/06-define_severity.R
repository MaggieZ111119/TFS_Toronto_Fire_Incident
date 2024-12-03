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
      total_casualties == 0 ~ "Low", 
      total_casualties >= 4 ~ "High",
      TRUE ~ "Medium"
    ),
    
    # Define Final Severity based on Apparatus, Dollar Loss, and Casualties
    Severity = case_when(
      # Explosion = High severity
      final_incident_type == 
        "02 - Explosion (including during Fire, excluding Codes 3 & 11-13)"
      ~ "High", 
      
      # Apparatus severity classifications based on bins
      (casualty_severity == "Low" 
       | severity_bin %in% c("[1,10]") 
       | log_estimated_dollar_loss <= loss_quantiles[1]) ~ "Low",
      
      (casualty_severity == "Medium" 
       | severity_bin %in% c("[11,20]", "[21,30]") 
       | log_estimated_dollar_loss <= loss_quantiles[2]) ~ "Medium",
      
      (casualty_severity == "High" 
       | severity_bin %in% c("[31,40]", "[41,50]", ">50") 
       | log_estimated_dollar_loss > loss_quantiles[2]) ~ "High",
      
      TRUE ~ "Low" 
    )
  )


#### Save Data ####
write_csv(fire_data_model, "data/02-analysis_data/severity_tfs_analysis_data")


#### Understand what is related to Severity ####

## Factor of Ignition Source ##
# Convert Severity and ignition_source_grouped to factors
fire_data_model <- fire_data_model %>%
  mutate(
    Severity = factor(Severity, levels = c("Low", "Medium", "High")),
    ignition_source_grouped = factor(ignition_source_grouped)  # Convert ignition_source_grouped to factor
  )

# Calculate the top 5 ignition sources for each Severity group
top_ignition_sources <- fire_data_model %>%
  group_by(Severity, ignition_source_grouped) %>%
  tally() %>%
  group_by(Severity) %>%
  top_n(5, n) %>%
  ungroup()

# Filter the dataset to include only the top ignition sources for each severity
fire_data_filtered <- fire_data_model %>%
  semi_join(top_ignition_sources, by = c("Severity", "ignition_source_grouped"))

# Create a stacked bar plot with percentages
ggplot(fire_data_filtered, aes(x = Severity, fill = ignition_source_grouped)) +
  geom_bar(position = "fill", stat = "count") +  # 'fill' makes the bars proportional
  labs(
    title = "Proportional Distribution of Ignition Sources by Severity",
    x = "Severity",
    y = "Proportion",
    fill = "Ignition Source"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # X-axis label alignment
    strip.text = element_text(size = 12)  # Adjust facet label size
  ) 

## Three Separate Plot
ggplot(fire_data_filtered, aes(x = ignition_source_grouped, fill = ignition_source_grouped)) +
  geom_bar() +  # Automatically assigns colors based on 'ignition_source_grouped'
  facet_wrap(~ Severity, scales = "free_y") +  # Different y-axis for each facet
  labs(
    title = "Top 5 Ignition Sources by Severity",
    x = "Ignition Source Grouped",
    y = "Count",
    fill = "Ignition Source"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 12),  # Adjust facet label size
    legend.position = "bottom"  # Place the legend at the bottom
  )


## Factor: Area of Origin ##
# Convert Severity and area_of_origin_grouped to factors
fire_data_model <- fire_data_model %>%
  mutate(
    Severity = factor(Severity, levels = c("Low", "Medium", "High")),
    area_of_origin_grouped = factor(area_of_origin_grouped)  # Convert area_of_origin_grouped to factor
  )

# Calculate the top 5 Area of Origins for each Severity group
top_area_of_origin <- fire_data_model %>%
  group_by(Severity, area_of_origin_grouped) %>%
  tally() %>%
  group_by(Severity) %>%
  top_n(5, n) %>%
  ungroup()

# Filter the dataset to include only the top Area of Origins for each severity
area_data_filtered <- fire_data_model %>%
  semi_join(top_area_of_origin, by = c("Severity", "area_of_origin_grouped"))

# Create a stacked bar plot with percentages
ggplot(area_data_filtered, aes(x = Severity, fill = area_of_origin_grouped)) +
  geom_bar(position = "fill", stat = "count") +  # 'fill' makes the bars proportional
  labs(
    title = "Proportional Distribution of Area of Origins by Severity",
    x = "Severity",
    y = "Proportion",
    fill = "Area of Origin"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # X-axis label alignment
    strip.text = element_text(size = 12)  # Adjust facet label size
  ) 

## Three Separate Plot
ggplot(area_data_filtered, aes(x = area_of_origin_grouped, fill = area_of_origin_grouped)) +
  geom_bar() +  # Automatically assigns colors based on 'area_of_origin_grouped'
  facet_wrap(~ Severity, scales = "free_y") +  # Different y-axis for each facet
  labs(
    title = "Top 5 Area of Origins by Severity",
    x = "Area of Origin Grouped",
    y = "Count",
    fill = "Area of Origin"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 12),  # Adjust facet label size
    legend.position = "bottom"  # Place the legend at the bottom
  )

