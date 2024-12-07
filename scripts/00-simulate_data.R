#### Preamble ####
# Purpose: Simulates Data
# Author: Maggie Zhang 
# Date: 19 September 2024
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: First sketch the idea of the dataset
# Any other information needed? Not much

#### Workspace setup ####
library(tidyverse)
library(arrow)

set.seed(1009633096)


#### Simulate data ####

clean_data <- read_parquet("data/02-analysis_data/tfs_analysis_data.parquet")

#number of rows
n_simulated <- nrow(clean_data)

## Numerical Values ##
# civilian_casualties, use Poisson distribution for count data
simulated_civilian_casualties <- rpois(n_simulated, 
                                       mean(clean_data$civilian_casualties,
                                            na.rm = TRUE)) 

# Estimated lost, use Log-normal distribution, with adjustment to mean and sd.
clean_data_no_na <- clean_data$estimated_dollar_loss[
  !is.na(clean_data$estimated_dollar_loss) & 
    clean_data$estimated_dollar_loss > 0]
log_mean <- log(mean(clean_data_no_na))
log_sd <- sd(log(clean_data_no_na))
simulated_estimated_dollar_loss <- rlnorm(
  n_simulated, meanlog = log_mean, sdlog = log_sd)
# Adjust the distribution to get a more realistic mean and quantiles
target_mean <- mean(clean_data$estimated_dollar_loss, na.rm = TRUE)
simulated_estimated_dollar_loss <- simulated_estimated_dollar_loss * 
  (target_mean / mean(simulated_estimated_dollar_loss))
# Apply truncation based on the original data's max value
max_loss <- max(clean_data$estimated_dollar_loss, na.rm = TRUE)
simulated_estimated_dollar_loss <- 
  pmin(simulated_estimated_dollar_loss, max_loss)
# Ensure the same proportion of zero values as in the original data
zero_percentage <- mean(clean_data$estimated_dollar_loss == 0, na.rm = TRUE)
simulated_estimated_dollar_loss[
  sample(1:n_simulated, size = round(zero_percentage * n_simulated))] <- 0

# Number of responsing apparatus, use random number that range from 1 to 50.
simulated_number_of_responding_apparatus <- sample(1:50, n_simulated, 
                                                   replace = TRUE)  


## Categorical Data ##
# Fire alarm system Operation, distributed roughly by it's percentage in original dataset
fire_alarm_categories <- unique(clean_data$fire_alarm_system_operation)
category_probs <- table(clean_data$fire_alarm_system_operation) / nrow(clean_data)
simulated_fire_alarm_system_operation <- sample(fire_alarm_categories, 
                                                n_simulated, 
                                                replace = TRUE, 
                                                prob = category_probs)

# Incident type, distributed roughly by it's percentage in original dataset
incident_type_categories <- unique(clean_data$final_incident_type)
incident_type_probs <- table(clean_data$final_incident_type) / nrow(clean_data)
simulated_incident_type <- sample(incident_type_categories, n_simulated, replace = TRUE, prob = incident_type_probs)


## Date/Time Data ##
# Response time based on log-normal distribution
mean_response_time <- mean(clean_data$response_time, na.rm = TRUE)
sd_response_time <- sd(clean_data$response_time, na.rm = TRUE)
# Scale down log-normal distribution by adjusting parameters
simulated_response_time <- rlnorm(n_simulated, 
                                  meanlog = log(mean_response_time), 
                                  sdlog = log(1 + (sd_response_time /
                                                     mean_response_time)^2))

# Datetime columns: alarm_time
simulated_tfs_alarm_time <- as.POSIXct('2011-01-01') + 
  runif(n_simulated, min = 0, max = as.numeric(difftime(
    max(clean_data$tfs_alarm_time), min(clean_data$tfs_alarm_time), 
    units = "secs")))
summary(simulated_tfs_alarm_time)

# Datetime columns: arrival_time, add simulated response_time to tfs_alarm_time
simulated_tfs_arrival_time <- simulated_tfs_alarm_time + 
  as.difftime(simulated_response_time * 60, units = "secs")
summary(simulated_tfs_arrival_time)

### Combined Simulated Date into Dataset ###
## Some other categorical variables are just simulated with random probability
## id column will be a sequence from 1 to n_simulated. It only plays a role in uniquely identifying each row
simulated_data <- data.frame(
  id = seq_len(n_simulated),
  area_of_origin = sample(unique(clean_data$area_of_origin), 
                          n_simulated, replace = TRUE),
  civilian_casualties = simulated_civilian_casualties,
  estimated_dollar_loss = simulated_estimated_dollar_loss,
  final_incident_type = simulated_incident_type,
  fire_alarm_system_operation = simulated_fire_alarm_system_operation,
  ignition_source = sample(unique(clean_data$ignition_source), 
                           n_simulated, replace = TRUE),
  number_of_responding_apparatus = simulated_number_of_responding_apparatus,
  number_of_responding_personnel = sample(10:50, 
                                          n_simulated, replace = TRUE),
  possible_cause = sample(unique(clean_data$possible_cause), 
                          n_simulated, replace = TRUE),
  sprinkler_system_presence = sample(
    unique(clean_data$sprinkler_system_presence), 
    n_simulated, replace = TRUE),
  tfs_alarm_time = simulated_tfs_alarm_time,
  tfs_arrival_time = simulated_tfs_arrival_time,
  tfs_firefighter_casualties = sample(0:4, 
                                      n_simulated, replace = TRUE),
  response_time = simulated_response_time
)




#### Save data ####
write_parquet(simulated_data, "data/00-simulated_data/simulated_data.parquet")
