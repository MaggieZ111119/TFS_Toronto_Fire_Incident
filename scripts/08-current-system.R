#### Preamble ####
# Purpose: Analyze and visualize the relationship between fire incident severity and factors such as fire alarm system operation and sprinkler system presence. Includes creating stacked bar plots and separate plots to explore the proportional distribution and counts by severity.
# Author: Maggie Zhang
# Date: 02 December 2023
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, rstanarm, rpart, ggplot2, dplyr, tidyr, fitdistrplus
# Any other information needed? The analysis involves creating visualizations to explore the distribution of fire alarm and sprinkler system statuses by severity.


#### Workspace setup ####
library(tidyverse)
library(rstanarm)
library(rpart)
library(ggplot2)
library(dplyr)
library(tidyr)
library(fitdistrplus)
library(arrow)

#### Read data ####
fire_data_current <- read_parquet("data/02-analysis_data/severity_tfs_analysis_data.parquet")

#### fire_alarm_system_operation ####

# Convert Severity and alarm_fire_data_filtered to factors
fire_data_current <- fire_data_current %>%
  mutate(
    Severity = factor(Severity, levels = c("Low", "Medium", "High")),
    fire_alarm_system_operation = factor(fire_alarm_system_operation) 
  )

# Calculate the top 5 Status of fire Alarm for each Severity group
top_fire_alarm_system_operation <- fire_data_current %>%
  group_by(Severity, fire_alarm_system_operation) %>%
  tally() %>%
  group_by(Severity) %>%
  top_n(5, n) %>%
  ungroup()

# Filter the dataset to include only the top status of fire alarm for each severity
alarm_fire_data_filtered <- fire_data_current %>%
  semi_join(top_fire_alarm_system_operation, by = c("Severity", "fire_alarm_system_operation"))

# Create a stacked bar plot with percentages
ggplot(alarm_fire_data_filtered, aes(x = Severity, fill = fire_alarm_system_operation)) +
  geom_bar(position = "fill", stat = "count") + 
  labs(
    title = "Proportional Distribution of fire_alarm_system_operation by Severity",
    x = "Severity",
    y = "Proportion",
    fill = "fire_alarm_system_operation"
  ) +
  scale_y_continuous(labels = scales::percent) +  # Format y-axis as percentages
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # X-axis label alignment
    strip.text = element_text(size = 12)  # Adjust facet label size
  ) 

## Three Separate Plot
ggplot(alarm_fire_data_filtered, aes(x = fire_alarm_system_operation, fill = fire_alarm_system_operation)) +
  geom_bar() +  
  facet_wrap(~ Severity, scales = "free_y") + 
  labs(
    title = "Top 5 Status of Fire Alarm by Severity",
    x = "fire_alarm_system_operation",
    y = "Count",
    fill = "fire_alarm_system_operation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 12),  # Adjust facet label size
    legend.position = "bottom"  # Place the legend at the bottom
  )




#### sprinkler_system_presence ####

# Convert Severity and sprinkler_system_data_filtered to factors
fire_data_current <- fire_data_current %>%
  mutate(
    Severity = factor(Severity, levels = c("Low", "Medium", "High")),
    sprinkler_system_presence = factor(sprinkler_system_presence)  # Convert ignition_source_grouped to factor
  )

# Calculate the top 5 Status of Sprinkler System for each Severity group
top_sprinkler_system_presence <- fire_data_current %>%
  group_by(Severity, sprinkler_system_presence) %>%
  tally() %>%
  group_by(Severity) %>%
  top_n(5, n) %>%
  ungroup()

# Filter the dataset to include only the top Status of Sprinkler System for each severity
sprinkler_system_data_filtered <- fire_data_current %>%
  semi_join(top_sprinkler_system_presence, by = c("Severity", "sprinkler_system_presence"))

# Create a stacked bar plot with percentages
ggplot(sprinkler_system_data_filtered, aes(x = Severity, fill = sprinkler_system_presence)) +
  geom_bar(position = "fill", stat = "count") + 
  labs(
    title = "Proportional Distribution of sprinkler_system_presence by Severity",
    x = "Severity",
    y = "Proportion",
    fill = "sprinkler_system_presence"
  ) +
  scale_y_continuous(labels = scales::percent) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),  # X-axis label alignment
    strip.text = element_text(size = 12)  # Adjust facet label size
  ) 

## Three Separate Plot
ggplot(sprinkler_system_data_filtered, aes(x = sprinkler_system_presence, fill = sprinkler_system_presence)) +
  geom_bar() +  
  facet_wrap(~ Severity, scales = "free_y") + 
  labs(
    title = "Top 5 Status of Sprinkler System by Severity",
    x = "sprinkler_system_presence",
    y = "Count",
    fill = "sprinkler_system_presence"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    strip.text = element_text(size = 12),  # Adjust facet label size
    legend.position = "bottom"  # Place the legend at the bottom
  )



