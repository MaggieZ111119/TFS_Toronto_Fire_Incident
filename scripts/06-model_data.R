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

#### Read data ####
fire_data_model <- read_csv("data/02-analysis_data/tfs_analysis_data")

#### Quantify Severity ####

# Load libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(fitdistrplus)

# Step 1: Choose a column to quantify fire severity, (number_of_responding_apparatus)
fire_data_model <- fire_data_model %>%
  mutate(severity_bin = cut(number_of_responding_apparatus,
                            breaks = c(0, 10, 20, 30, 40, 50, Inf),
                            labels = c("[1,10]", "[11,20]", "[21,30]", "[31,40]", "[41,50]", ">50"),
                            right = TRUE))

# Step 2: Create a frequency distribution of severity bins
severity_dist <- fire_data_model %>%
  group_by(severity_bin) %>%
  summarise(frequency = n()) %>%
  ungroup()

# Log-log plot
ggplot(severity_dist, aes(x = as.numeric(severity_bin), y = frequency)) +
  geom_point() +
  scale_x_continuous(breaks = 1:length(levels(fire_data$severity_bin)), 
                     labels = levels(fire_data$severity_bin)) +
  scale_y_log10() +
  labs(x = "Severity Bins (Apparatus Count)", 
       y = "Frequency (Log Scale)", 
       title = "Log-Log Plot of Fire Severity Distribution") +
  theme_minimal()

# Step 3: Fit a Pareto distribution to the severity data
# Fit Pareto distribution to the column 'number_of_responding_apparatus' (or 'number_of_responding_personnel')
fit_pareto <- fitdist(fire_data_model$number_of_responding_apparatus, "pareto")

# Summary of Pareto fit
summary(fit_pareto)

# Step 4: Examine excesses above a threshold (e.g., severity > 20)
threshold <- 20  # Chosen threshold for extreme events
extreme_events <- fire_data %>%
  filter(number_of_responding_apparatus > threshold) %>%
  summarise(excess = mean(number_of_responding_apparatus - threshold))

# Plot excesses above threshold
ggplot(extreme_events, aes(x = number_of_responding_apparatus)) +
  geom_histogram(binwidth = 1) +
  labs(title = "Excesses Above Threshold (Severity > 20 Apparatus)") +
  theme_minimal()

# Step 5: Estimate the critical threshold for extreme events using the ratio method
critical_threshold <- function(data, p_range = seq(1, 4, by = 0.1)) {
  ratio <- sapply(p_range, function(p) {
    return(mean(data ^ p))
  })
  
  # Plot the ratio for different p values
  ggplot(data.frame(p_range, ratio), aes(x = p_range, y = ratio)) +
    geom_line() +
    labs(x = "p", y = "Ratio") +
    ggtitle("Critical Threshold Estimation") +
    theme_minimal()
}

# Call the function with your severity data
critical_threshold(fire_data$number_of_responding_apparatus)










### Model data ####
library(rpart)
tree_model <- rpart(response_time ~ area_of_origin_grouped + ignition_source_grouped + sprinkler_system_presence,
                    data = fire_data_filtered, method = "anova")
summary(tree_model)



#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)


