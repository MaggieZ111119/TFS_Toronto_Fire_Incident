#### Preamble ####
# Purpose: Replicated graphs from... [...UPDATE THIS...]
# Author: Rohan Alexander [...UPDATE THIS...]
# Date: 11 February 2023 [...UPDATE THIS...]
# Contact: rohan.alexander@utoronto.ca [...UPDATE THIS...]
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(caret)
library(xgboost)

#### Read data ####
fire_data_model <- read_csv("data/02-analysis_data/severity_tfs_analysis_data")

#### Prepare Data ####

fire_data_model <- fire_data_model[, c(
  "area_of_origin_grouped", 
  "ignition_source_grouped", 
  "Severity",
  "month"
)]

trainIndex <- createDataPartition(fire_data_model$Severity, p = 0.8, list = FALSE)
train_data <- fire_data_model[trainIndex, ]
test_data <- fire_data_model[-trainIndex, ]

train_data$Severity <- factor(train_data$Severity, levels = c("Low", "Medium", "High"))

tune_grid <- expand.grid(
  mtry = c(2, 3, 4)# Number of variables randomly selected at each split
)

train_control <- trainControl(method = "cv", number = 5)
# Calculate weight
freq <- table(train_data$Severity)
class_weights <- 1 / freq
class_weights

tuned_rf_model <- train(Severity ~ area_of_origin_grouped + ignition_source_grouped + month, 
                        data = train_data, 
                        method = "rf", 
                        trControl = train_control, 
                        tuneGrid = tune_grid, 
                        importance = TRUE, 
                        classwt = c(0.0000892618, 0.0024271845 , 0.0119047619))  

print(rf_model)

rf_predictions <- predict(rf_model, newdata = test_data)
table(Predicted = rf_predictions, Actual = test_data$Severity)



#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)

