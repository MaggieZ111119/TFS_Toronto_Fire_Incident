#### Preamble ####
# Purpose: Replicated graphs and models for analyzing severity of fire incidents based on various factors, including ignition source and area of origin.
# Author: Maggie Zhang
# Date: 02 December 2023
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: tidyverse, dplyr, caret, xgboost, car, pROC
# Any other information needed? The analysis involves logistic regression modeling to predict severity based on categorical variables, followed by model evaluation using ROC curves.


#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(caret)
library(xgboost)
library(arrow)

#### Read data ####
fire_data_model <- read_parquet("data/02-analysis_data/severity_tfs_analysis_data.parquet")

#### Prepare Data ####

fire_data_model <- fire_data_model %>%
  mutate(Severity_high = as.factor(ifelse(Severity == "High", 1, 0)))

fire_data_model <- fire_data_model[, c(
  "area_of_origin_grouped", 
  "ignition_source_grouped", 
  "Severity_high"
)]

fire_data_model$ignition_source_grouped <- as.factor(fire_data_model$ignition_source_grouped)
fire_data_model$area_of_origin_grouped <- as.factor(fire_data_model$area_of_origin_grouped)

fire_data_model$ignition_source_grouped <- gsub(" ", "_", fire_data_model$ignition_source_grouped)
fire_data_model$area_of_origin_grouped <- gsub(",", "_", fire_data_model$area_of_origin_grouped)
fire_data_model$area_of_origin_grouped <- gsub(" ", "_", fire_data_model$area_of_origin_grouped)
fire_data_model$area_of_origin_grouped <- gsub("__", "_", fire_data_model$area_of_origin_grouped)


trainIndex <- createDataPartition(fire_data_model$Severity_high, p = 0.8, list = FALSE)
train_data <- fire_data_model[trainIndex, ]
test_data <- fire_data_model[-trainIndex, ]

### Model (Logistic) Traning####
logistic_model <- glm(Severity_high ~ area_of_origin_grouped + ignition_source_grouped, data = train_data, family = binomial)
summary(logistic_model)


## Drop paramter based on p-vlaue ##
# Check for correaltion
library(car)
vif(logistic_model)

# Update the model to keep only significant variables
updated_model <- glm(Severity_high ~ area_of_origin_grouped, 
                     data = train_data, family = binomial)

# Check the summary of the updated model
summary(updated_model)

# AIC improved, model better




#### Model Evaluation ####
library(pROC)
predicted_probs <- predict(updated_model, test_data, type = "response")
roc_curve <- roc(test_data$Severity_high, predicted_probs)
x11()
plot(roc_curve)


#### Save model ####
saveRDS(
  updated_model,
  file = "models/logtsic_model.rds"
)

