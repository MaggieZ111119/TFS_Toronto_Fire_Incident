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
fire_data_model$area_of_origin_grouped <- as.factor(fire_data_model$area_of_origin_grouped)
fire_data_model$ignition_source_grouped <- as.factor(fire_data_model$ignition_source_grouped)
fire_data_model$month <- as.factor(fire_data_model$month)

# Encode the target variable (Severity) as a binary outcome for severity (high = 1, others = 0)
fire_data_model <- fire_data_model %>%
  mutate(Severity_high = ifelse(Severity == "High", 1, 0))
fire_data_model <- fire_data_model[, c(
  "area_of_origin_grouped", 
  "ignition_source_grouped", 
  "month",
  "Severity_high"
)]


#Split into test and trannig set
set.seed(123)
trainIndex <- createDataPartition(fire_data_model$Severity_high, p = 0.8, list = FALSE)
train_data <- fire_data_model[trainIndex, ]
test_data <- fire_data_model[-trainIndex, ]

train_matrix <- model.matrix(Severity_high ~ . - 1, data = train_data)
test_matrix <- model.matrix(Severity_high ~ . - 1, data = test_data)

### Model (XGBoost) Traning####

xgb_model <- xgboost(data = train_matrix, label = train_data$Severity_high, 
                     objective = "binary:logistic", 
                     nrounds = 100, 
                     max_depth = 6, 
                     eta = 0.3, 
                     subsample = 0.8, 
                     colsample_bytree = 0.8, 
                     eval_metric = "logloss")

# View model summary
print(xgb_model)

#### Model Evalaution ####
predictions <- predict(xgb_model, test_matrix)
predictions_class <- factor(predictions_class, levels = levels(factor(test_data$Severity_high)))
conf_matrix <- confusionMatrix(predictions_class, factor(test_data$Severity_high))
print(conf_matrix)


# ROC Curve
library(ROCR)
pred <- prediction(predictions, test_data$Severity_high)
perf <- performance(pred, "tpr", "fpr")
x11()  # Open a new graphics window
plot(perf, main = "ROC Curve", col = "blue", lwd = 2)

# Plot feature importance

importance_data <- xgb.importance(model = xgb_model)

xgb.plot.importance(importance_data, top_n = 10)




#### Save model ####
saveRDS(
  first_model,
  file = "models/first_model.rds"
)

