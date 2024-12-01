#### Preamble ####
# Purpose: Explore data of variables in interest and plot them
# Author: Maggie Zhang 
# Date: 30 November 2024
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]


#### Workspace setup ####
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(knitr)


#### Read data ####
fire_data <- read.csv("data/02-analysis_data/tfs_analysis_data")

### Data Overview ###
# Structure
head(fire_data)
str(fire_data)

# Missing Value
missing_data <- colSums(is.na(fire_data))
print(missing_data)


### Plot/Summary distribution of all Variables ###
## 1. Civilian Casualties (Numerical)
#Overall Table
civilian_casualties_df <- as.data.frame(table(fire_data$civilian_casualties))
colnames(civilian_casualties_df) <- c("Number_of_Casualties", "Frequency")
civilian_casualties_df
# Summary table
summary(fire_data$civilian_casualties)

### 2. Estimated Dollar Loss (Numerical)
#Overall Table
est_loss_df <- as.data.frame(table(fire_data$estimated_dollar_loss))
colnames(est_loss_df) <- c("Estimated of Loss", "Frequency")
est_loss_df
# Summary table
summary(fire_data$estimated_dollar_loss)

### 3. Fire Alarm System Operation (Categorical)
# Plot
ggplot(fire_data, aes(x = fire_alarm_system_operation)) +
  geom_bar(fill = "blue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Fire Alarm System Operation") +
  xlab("Fire Alarm System Operation Type") +
  ylab("Frequency") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) 
#Table
table(fire_data$fire_alarm_system_operation)

# 4. Ignition Source (Categorical)
# Create a data frame with the ignition source codes and their descriptions
# List of ignition sources
ignition_sources <- c("999 - Undetermined", "51 - Incandescent Lamp - Light Bulb, Spotlight", 
                      "23 - Distribution Equipment (includes panel boards, fuses, circuit br)", 
                      "41 - Other Heating Equipment", "11 - Stove, Range-top burner", 
                      "82 - Vehicle - Mechanical", "24 - Circuit Wiring - Copper", 
                      "98 - Other", "81 - Vehicle - Electrical", "49 - Other Appliances", 
                      "71 - Smoker's Articles (eg. cigarettes, cigars, pipes already ignited)", 
                      "13 - Microwave", "77 - Matches or Lighters (unable to distinguish)", 
                      "74 - Salamander", "12 - Oven", "47 - Refrigerator, Freezer (includes vending machine)", 
                      "73 - Blow Torch, Bunsen Burner", "72 - Cutting/Welding Equipment", 
                      "21 - Transformer", "55 - Candle", "29 - Extension Cord, Temporary Wiring", 
                      "14 - Open Fired Barbeque - Fixed or Portable", "79 - Other Open Flame Tools/Smokers' Articles", 
                      "84 - Other Mechanical", "36 - Fireplace - Masonry", "83 - Other Electrical", 
                      "20 - Service/Utility Lines (includes power/hydro transmission lines)", 
                      "92 - Open Fire (eg. camp fire, rubbish fire, etc.)", "17 - Wood burning stove", 
                      "62 - Heat Treatment Equipment (eg. furnace, oven, kiln, quench tanks, etc.)", 
                      "19 - Other Cooking Items (eg Toaster, Kettle, elec frying pan)", 
                      "93 - Hot Ashes, Embers, Spark", "52 - Florescent Lamp (includes ballast)", 
                      "64 - Chemical Processing Equipment (eg. reactors, distilling units, etc.)", 
                      "43 - Clothes Dryer", "69 - Other Processing Equipment", 
                      "88 - Multiple Ignition Source or Igniting Equipment (suspected arson)", 
                      "75 - Matches (open flame)", "32 - Water Heater", 
                      "26 - Terminations-Copper (incl receptacles, switches, lights)", "85 - Vehicle collision", 
                      "28 - Cord, Cable for Appliance, Electrical Articles", "16 - Deep Fat Fryer", 
                      "59 - Other Lighting Equipment", "30 - Other Electrical Distribution Item", 
                      "76 - Lighters (open flame)", "33 - Space Heater - Fixed", "42 - Television, Radio, Stereo, Tape Recorder, etc.", 
                      "39 - Chimney - Masonry", "34 - Space Heater - Portable", "44 - Iron, Pressing Machine", 
                      "108 - Exposure, source other", "15 - Range Hood", "63 - Painting Equipment", 
                      "107 - Exposure, source vehicle (outside structure)", 
                      "104 - Exposure, source open fire (inc campfire, rubbish fire)", 
                      "48 - Air Conditioner - Window or Room Unit", "96 - Chemical Reaction (eg. spontaneous combustion, etc.)", 
                      "106 - Exposure, source grass, shrubs, trees", "35 - Fireplace - Factory Built", 
                      "37 - Fireplace Insert", "94 - Static Electricity (spark)", "9990 - Under Investigation", 
                      "31 - Central Heating/Cooling Unit", "56 - Halogen Lamp or light", 
                      "91 - Fireworks", "46 - Electric Blanket, Heating Pad", 
                      "105 - Exposure, source forest, trees, wildland", "100 - Outdoor fireplace/heater", 
                      "103 - Exposure, source outside storage container, tank", 
                      "38 - Chimney - Factory Built", "54 - Lamp (eg. coal, oil, naphtha, etc.)", 
                      "102 - Exposure, source structure semi-detached or attached", "40 - Flue Pipe", 
                      "27 - Terminations-Aluminum (incl receptables, switches, lights)", 
                      "22 - Meter", "80 - Portable generator", "101 - Exposure, source structure detached", 
                      "95 - Lightning", "97 - Rekindle", "45 - Washing Machine", 
                      "25 - Circuit Wiring - Aluminum", "53 - Christmas Lights, Decorative Lighting", 
                      "61 - Incinerator", "90 - Explosives")

# Create a function to categorize sources
categorize_ignition_sources <- function(source) {
  if (grepl("999", source)) {
    return("Undetermined")
  } else if (grepl("Lamp|Lighting|Electrical|Cable|Wire", source)) {
    return("Lighting and Electrical Equipment")
  } else if (grepl("Stove|Oven|Microwave|Grill|Fryer|Heater", source)) {
    return("Cooking and Heating Equipment")
  } else if (grepl("Fire|Matches|Candle|Blow Torch|Flame", source)) {
    return("Fire-related and Open Flame")
  } else if (grepl("Chemical|Explosion|Flammable", source)) {
    return("Flammable Materials and Chemicals")
  } else if (grepl("Mechanical|Vehicle", source)) {
    return("Mechanical Equipment")
  } else if (grepl("Transformer|Service|Utility|Distribution", source)) {
    return("Electrical Distribution and Utility")
  } else if (grepl("Static|Spark", source)) {
    return("Static Electricity and Sparks")
  } else if (grepl("Exposure", source)) {
    return("Exposure to Fire and Environmental")
  } else if (grepl("Processing|Furnace|Kiln", source)) {
    return("Processing Equipment")
  } else if (grepl("Clothes|Washing Machine|Refrigerator", source)) {
    return("Appliances")
  } else if (grepl("Vehicle|Collision", source)) {
    return("Vehicle and Accidents")
  } else if (grepl("Multiple|Arson", source)) {
    return("Multiple Ignition Sources")
  } else if (grepl("Cooling|Air Conditioner", source)) {
    return("Cooling and Air Conditioning Equipment")
  } else {
    return("Other")
  }
}

# Apply the categorization function to each ignition source
categorized_sources <- sapply(ignition_sources, categorize_ignition_sources)

# Create a data frame with the original and categorized sources
result <- data.frame(ignition_source = ignition_sources, category = categorized_sources)

# Summarize the grouped categories
summary <- table(result$category)

# Display the result summary
view(summary)


# View the first few rows of the data frame
head(ignition_sources)


# Table
table(fire_data$ignition_source_grouped)
# Plot
ggplot(fire_data, aes(x = ignition_source_grouped)) +
  geom_bar(fill = "green") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Grouped Ignition Source Distribution") +
  xlab("Ignition Source Group") +
  ylab("Frequency") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5) 
