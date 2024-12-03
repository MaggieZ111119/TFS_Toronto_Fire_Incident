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

fire_data_model <- fire_data_model %>%
  mutate(Severity_high = as.factor(ifelse(Severity == "High", 1, 0)))

fire_data_model <- fire_data_model[, c(
  "area_of_origin_grouped", 
  "ignition_source_grouped", 
  "Severity_high",
  "month"
)]
fire_data_model$month <- as.factor(fire_data_model$month)
fire_data_model_origin_encoded <- model.matrix(~ area_of_origin_grouped - 1, data = fire_data_model)
fire_data_model_source_encoded <- model.matrix(~ ignition_source_grouped - 1, data = fire_data_model)

fire_data_model <- cbind(fire_data_model, fire_data_model_source_encoded, fire_data_model_origin_encoded)

fire_data_model <- fire_data_model %>%
  rename_all(~ gsub(" ", "_", .)) %>%
  rename_all(~ gsub("-", "_", .)) %>%
  rename_all(~ gsub(",", "_", .)) %>%
  rename_all(~ gsub(":", "_", .))


library(randomForest)
# Fit random forest model
rf_model <- randomForest(Severity_high ~ ., data = fire_data_model, importance = TRUE)

# Print the model summary
print(rf_model)

# View feature importance
importance(rf_model)


#Split into test and trannig set
set.seed(123)
trainIndex <- createDataPartition(fire_data_model$Severity_high, p = 0.8, list = FALSE)
train_data <- fire_data_model[trainIndex, ]
test_data <- fire_data_model[-trainIndex, ]

### Model (Logistic) Traning####
table(train_data$Severity_high)
weights <- ifelse(train_data$Severity_high == 1, 10, 1)

logit_model <- glm(Severity_high ~ ., data = train_data, family = binomial, weights = weights)
summary(logit_model)

## Drop paramter based on p-vlaue ##
model_updated <- glm(
  Severity_high ~ 
    
    data = train_data,  # Replace 'your_data' with your actual data frame
  family = binomial(link = "logit")
)
summary(model_updated)



#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)

