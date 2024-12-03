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

#### Data Overview ####
# Structure
head(fire_data)
str(fire_data)

# Missing Value
missing_data <- colSums(is.na(fire_data))
print(missing_data)


#### Plot/Summary distribution of all Variables ####
## 1. Area of Origin (Categorical) ##
all_area <- unique(fire_data$area_of_origin)
# List of Area of Origin
area_of_origin <- c("22 - Sleeping Area or Bedroom (inc. patients room, dormitory, etc)", 
                    "55 - Mechanical/Electrical Services Room", 
                    "28 - Office", 
                    "24 - Cooking Area or Kitchen", 
                    "81 - Engine Area", 
                    "25 - Washroom or Bathroom (toilet, restroom/locker room)", 
                    "73 - Parking Area, Parking Lot", 
                    "44 - Trash, Rubbish Storage (inc garbage chute room, garbage/industr", 
                    "50 - Basement/cellar (not partitioned)", 
                    "64 - Porch or Balcony", 
                    "78 - Attached Deck", 
                    "86 - Passenger Area", 
                    "52 - HVAC Equipment Room (furnace room, water heater closet, boiler)", 
                    "21 - Living Area (e.g. living, TV, recreation, etc)", 
                    "61 - Exterior Wall", 
                    "62 - Roof", 
                    "12 - Hallway, Corridor", 
                    "83 - Electrical Systems", 
                    "59 - Utility Shaft (eg. electrical wiring/phone, etc.)", 
                    "79 - Other Outside Area", 
                    "75 - Trash, rubbish area (outside)", 
                    "89 - Other Vehicle Area", 
                    "31 - Process Manufacturing (inc manf, prod assembly, repair)", 
                    "53 - Chimney/Flue Pipe", 
                    "26 - Sauna", 
                    "99 - Undetermined (formerly 98)", 
                    "87 - Trunk/Cargo Area", 
                    "11 - Lobby, Entranceway", 
                    "97 - Other - unclassified", 
                    "67 - Concealed Floor Area", 
                    "58 - Ducting - Exhaust (inc cooking, fumes, etc.)", 
                    "29 - Electronic Equipment", 
                    "51 - Elevator (includes shaft)", 
                    "46 - Product Storage (inc products or materials awaiting manuf, assembly)", 
                    "33 - Laboratory", 
                    "84 - Fuel Systems (eg. fuel tank, etc.)", 
                    "27 - Laundry Area", 
                    "69 - Attic Area", 
                    "45 - Supply Storage Room (inc maintenance/office/document storage, et", 
                    "30 - Sales, Showroom Area", 
                    "42 - Garage", 
                    "66 - Concealed Ceiling Area", 
                    "49 - Other Storage Area", 
                    "13 - Stairway, Escalator", 
                    "39 - Other Functional Area", 
                    "71 - Open Area (inc lawn, field, farmyard, park, playing field, pier)", 
                    "93 - Residential/Business: Other business area", 
                    "72 - Court, Patio, Terrace", 
                    "74 - Storage Area (outside)", 
                    "56 - Conveyor Shaft or Chute (inc dumbwaiter, laundry chute, garbage", 
                    "23 - Dining or Beverage Area (inc mess, canteen, lunchroom, cafeteria", 
                    "92 - Residential/Business: Restaurant area", 
                    "63 - Awning or Canopy", 
                    "68 - Concealed Wall Area", 
                    "41 - Closet (eg. clothes, broom, linen closet, etc.)", 
                    "82 - Running Gear (inc wheels and braking systems, transmission system", 
                    "60 - Other Building Services/Support Facilities", 
                    "85 - Operator/Control Area", 
                    "990 - Under Investigation", 
                    "43 - Locker (apartment storage)", 
                    "57 - Ducting - Heating, Air Conditioning", 
                    "70 - Other Structural Area", 
                    "35 - Performance Area (inc stage, rink, boxing ring, gym floor, altar)", 
                    "65 - Crawl Space (includes sub-structure)", 
                    "91 - Multiple Areas of Origin", 
                    "18 - Covered Court, Atrium, mall concourse", 
                    "36 - Backstage, dressing room", 
                    "32 - Assembly Area (inc school room, spectator area, church, etc)", 
                    "19 - Other Means of Egress", 
                    "47 - Shipping/Receiving/Loading Platform", 
                    "34 - Operating Room, Treatment or Examination Area", 
                    "76 - Fuel Dispensing Area (outside)", 
                    "48 - Records storage area (inc vaults)", 
                    "54 - Incinerator Room")

# Create a function to categorize area of origin
categorize_area_of_origin <- function(area) {
  # Residential and Living Spaces
  if (grepl("Bedroom|Living Area|Dormitory|Hallway|Lobby|Basement|Closet|Sauna", area)) {
    return("Residential and Living Spaces")
  } 
  # Cooking and Dining Areas
  else if (grepl("Kitchen|Dining|Canteen|Lunchroom|Mess", area)) {
    return("Cooking and Dining Areas")
  } 
  # Office and Workspace
  else if (grepl("Office|Workspace|Lobby|Showroom|Laboratory|Assembly Area", area)) {
    return("Office and Workspace")
  } 
  # Mechanical, HVAC, and Electrical Areas
  else if (grepl("Mechanical|Electrical|HVAC|Boiler|Furnace", area)) {
    return("Mechanical, HVAC, and Electrical Areas")
  } 
  # Storage and Utility Areas
  else if (grepl("Storage|Concealed|Shaft|Utility|Warehouse|Garage", area)) {
    return("Storage and Utility Areas")
  } 
  # Outdoor and Exterior Areas
  else if (grepl("Parking|Exterior|Deck|Porch|Lawn|Field|Pier|Terrace", area)) {
    return("Outdoor and Exterior Areas")
  } 
  # Vehicle and Transport Areas
  else if (grepl("Vehicle|Trunk|Cargo|Engine|Garage", area)) {
    return("Vehicle and Transport Areas")
  } 
  # Specialized Rooms
  else if (grepl("Bathroom|Laundry|Shower|Incinerator|Sauna|Elevator", area)) {
    return("Specialized Rooms")
  } 
  # Industrial and Manufacturing Areas
  else if (grepl("Manufacturing|Assembly|Fuel Systems|Conveyor|Chute", area)) {
    return("Industrial and Manufacturing Areas")
  } 
  
  # Public and Performance Spaces
  else if (grepl("Stage|Rink|Gym|Performance|Concourse|Court", area)) {
    return("Public and Performance Spaces")
  } 
  # Miscellaneous and Unclassified
  else {
    return("Miscellaneous and Unclassified")
  }
}
# Apply the categorization function to each area of origin
fire_data$area_of_origin_grouped <- sapply(fire_data$area_of_origin, 
                                           categorize_area_of_origin)
table(fire_data$area_of_origin_grouped)
# Plot
ggplot(fire_data, aes(x = area_of_origin_grouped)) +
  geom_bar(fill = "red") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Area of Origin Distribution") +
  xlab("Area of Origin") +
  ylab("Frequency") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0.5)


## 2. Civilian Casualties (Numerical) ##
#Overall Table
civilian_casualties_df <- as.data.frame(table(fire_data$civilian_casualties))
colnames(civilian_casualties_df) <- c("Number_of_Casualties", "Frequency")
civilian_casualties_df
# Summary table
summary(fire_data$civilian_casualties)
#Plot
ggplot(fire_data, aes(x = X_id, y = civilian_casualties)) +
  geom_point(color = "darkgreen", size = 2, shape = 16) +
  theme_minimal() +
  ggtitle("Scatterplot of Number of Responding Personnel vs. Incident ID") +
  xlab("Incident ID") +
  ylab("Civilian Casualties")


## 3. Estimated Dollar Loss (Numerical) ##
#Overall Table
est_loss_df <- as.data.frame(table(fire_data$estimated_dollar_loss))
colnames(est_loss_df) <- c("Estimated of Loss", "Frequency")
est_loss_df
# Summary table
summary(fire_data$estimated_dollar_loss)
#Plot


## 4. Final Incident Type (Categorical) ##
# Plot
ggplot(fire_data, aes(x = final_incident_type)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Final Incident Type Distribution") +
  xlab("Final Incident Type") +
  ylab("Frequency") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0.5)
#table
table(fire_data$final_incident_type)


## 5. Fire Alarm System Operation (Categorical) ##
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


## 6. Ignition Source (Categorical) ##
all_sources <- unique(fire_data$ignition_source)
ignition_sources <- c("51 - Incandescent Lamp - Light Bulb, Spotlight", 
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
  # Lighting and Electrical Equipment
  if (grepl("Lamp|Lighting|Electrical|Cable|Wire|Ballast|Transformer", source)) {
    return("Lighting and Electrical Equipment")
  } 
  # Cooking and Heating Equipment
  else if (grepl("Stove|Oven|Microwave|Grill|Fryer|Heater|Range", source)) {
    return("Cooking and Heating Equipment")
  } 
  # Fire-related and Open Flame
  else if (grepl("Fire|Matches|Candle|Flame|Torch|Burner|Smoker", source)) {
    return("Fire-related and Open Flame")
  } 
  # Flammable Materials and Chemicals
  else if (grepl("Chemical|Explosion|Flammable|Rekindle|Static|Spark", source)) {
    return("Flammable Materials and Chemicals")
  } 
  # Mechanical Equipment
  else if (grepl("Mechanical|Vehicle|Collision|Smokers|Blower", source)) {
    return("Mechanical Equipment")
  } 
  # Electrical Distribution and Utility
  else if (grepl("Distribution|Service|Utility|Panel|Circuit", source)) {
    return("Electrical Distribution and Utility")
  } 
  
  # Static Electricity and Sparks
  else if (grepl("Spark|Static", source)) {
    return("Static Electricity and Sparks")
  } 
  # Exposure to Fire and Environmental
  else if (grepl("Exposure|Outside|Container|Fireplace|Campfire|Wildland", source)) {
    return("Exposure to Fire and Environmental")
  } 
  # Processing Equipment (Industrial)
  else if (grepl("Furnace|Kiln|Processing|Incinerator", source)) {
    return("Processing Equipment")
  } 
  # Appliances
  else if (grepl("Appliance|Refrigerator|Washing Machine|Dryer|Iron", source)) {
    return("Appliances")
  } 
  
  # Vehicle and Accidents
  else if (grepl("Vehicle|Accident|Collision", source)) {
    return("Vehicle and Accidents")
  } 
  # Multiple Ignition Sources (or suspected arson)
  else if (grepl("Multiple|Arson", source)) {
    return("Multiple Ignition Sources")
  } 
  # Cooling and Air Conditioning Equipment
  else if (grepl("Cooling|Air Conditioner", source)) {
    return("Cooling and Air Conditioning Equipment")
  } 
  # Other (for anything that doesn't fit into the above categories)
  else {
    return("Other")
  }
}
# Apply the categorization function to each ignition source
fire_data$ignition_source_grouped <- sapply(fire_data$ignition_source, categorize_ignition_sources)
#Table
table(fire_data$ignition_source_grouped)
#Plot
# Plot of grouped ignition source distribution
ggplot(fire_data, aes(x = ignition_source_grouped)) +
  geom_bar(fill = "green") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Grouped Ignition Source Distribution") +
  xlab("Ignition Source Group") +
  ylab("Frequency") +
  geom_text(stat = "count", aes(label = ..count..), vjust = 0.5)


## 7. Number of Responding Apparatus (Numerical) ##
ggplot(fire_data, aes(x = number_of_responding_apparatus)) +
  geom_histogram(bins = 30, fill = "cyan", color = "black") +
  theme_minimal() +
  ggtitle("Number of Responding Apparatus Distribution") +
  xlab("Number of Responding Apparatus") +
  ylab("Frequency") +
  stat_bin(aes(label = ..count..), geom = "text", vjust = -0.5, size = 3)
#ScatterPlot
ggplot(fire_data, aes(x = X_id, y = number_of_responding_apparatus)) +
  geom_point(color = "darkorange", size = 2, shape = 16) +
  theme_minimal() +
  ggtitle("Scatterplot of Number of Responding apparatus vs. Incident ID") +
  xlab("Incident ID") +
  ylab("Number of Responding apparatus")
#Summary table
summary(fire_data$number_of_responding_apparatus)


# 8. Number of Responding Personnel (Numerical) ##
#Plot
ggplot(fire_data, aes(x = number_of_responding_personnel)) +
  geom_histogram(bins = 30, fill = "yellow", color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Number of Responding Personnel Distribution") +
  xlab("Number of Responding Personnel") +
  ylab("Frequency") +
  stat_bin(aes(label = ..count..), geom = "text", vjust = 0.5, size = 3)
#ScatterPlot
ggplot(fire_data, aes(x = X_id, y = number_of_responding_personnel)) +
  geom_point(color = "orange", size = 2, shape = 16) +
  theme_minimal() +
  ggtitle("Scatterplot of Number of Responding Personnel vs. Incident ID") +
  xlab("Incident ID") +
  ylab("Number of Responding Personnel")
#Summary table
summary(fire_data$number_of_responding_personnel)


## 9. Possible Cause ##
#plot all that hsa frequency greater than 550
filtered_data <- fire_data %>%
  count(possible_cause) %>%
  filter(n > 550)
ggplot(filtered_data, aes(x = possible_cause, y = n)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Possible Cause") +
  xlab("Possible Cause") +
  ylab("Frequency") +
  geom_text(aes(label = n), vjust = -0.5)
#Full Tabble
cause_table <- table(fire_data$possible_cause)
cause_table_df <- as.data.frame(cause_table) %>%
  arrange(desc(Freq))
kable(cause_table_df, col.names = c("Possible Cause", "Frequency"), 
             caption = "Frequency of Possible Causes (Descending Order)", 
             format = "markdown")

## 11. Sprinkler System Presence (Categorical) ##
#Plot
ggplot(fire_data, aes(x = sprinkler_system_presence)) +
  geom_bar(fill = "blue") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
  ggtitle("Sprinkler System Presence") +
  xlab("Sprinkler System Presence") +
  ylab("Frequency") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5)
#Table
table(fire_data$sprinkler_system_presence)


## 12. tfs_alarm_time ## 
fire_data$year <- year(fire_data$tfs_alarm_time)
fire_data$month <- month(fire_data$tfs_alarm_time)
fire_data$month <- as.numeric(fire_data$month)

# Plot Number of Incidents by Year
ggplot(fire_data, aes(x = year)) +
  geom_bar(fill = "lightblue", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), vjust = -0.5, size = 3) + 
  labs(title = "Fire Incidents by Year", x = "Year", y = "Incident Count") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(min(fire_data$year), max(fire_data$year), by = 1))
theme(axis.text.x = element_text(angle = 45, hjust = 1))

# from year plot, we can see that 2022 and 2023 does not provide much information so we can droped them.
fire_data_filtered <- fire_data %>%
  filter(!is.na(year) & !is.na(month) & month >= 1 & month <= 12)
incident_count_by_year_month <- fire_data_filtered %>%
  group_by(year, month) %>%
  summarize(count = n(), .groups = 'drop')
incident_count_by_year_month_ex <- incident_count_by_year_month %>%
  filter(!(year %in% c(2022, 2023)))
incident_count_by_year_month_ex$month <- factor(incident_count_by_year_month_ex$month, levels = 1:12)
ggplot(incident_count_by_year_month_ex, aes(x = month, y = count)) +
  geom_bar(stat = "identity", fill = "lightcoral", color = "black") +
  labs(title = "Fire Incidents by Month and Year", x = "Month", y = "Incident Count") +
  scale_x_discrete(
    breaks = 1:12,  # Show all months (1 to 12)
    labels = month.name  # Use full month names
  ) +
  facet_wrap(~year, scales = "fixed") +  # Fix the y-axis scale for comparison
  theme_minimal() +
  theme(
    strip.text.x = element_text(size = 10), 
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.5, size = 8), 
    axis.title.x = element_text(size = 12), 
    axis.title.y = element_text(size = 12), 
    strip.background = element_rect(fill = "lightblue", color = "black"), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )

# Table
incident_table <- incident_count_by_year_month %>%
  pivot_wider(names_from = month, values_from = count, values_fill = 0)

# Print the table
view(incident_table)


## 13. tfs_arrival_time ##


## 14. Firefighter Casualties (Numerical) ##
ggplot(fire_data, aes(x = tfs_firefighter_casualties)) +
  geom_histogram(bins = 30, fill = "pink", color = "black") +
  theme_minimal() +
  ggtitle("Firefighter Casualties Distribution") +
  xlab("Firefighter Casualties") +
  ylab("Frequency")
casualties_table <- table(fire_data$tfs_firefighter_casualties)
casualties_df <- as.data.frame(casualties_table)
# Plot the pie chart
ggplot(casualties_df, aes(x = "", y = Freq, fill = factor(Var1))) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  theme_void() +
  ggtitle("Distribution of Firefighter Casualties") +
  scale_fill_manual(values = c("pink", "lightblue", "lightgreen", "orange")) +
  labs(fill = "Casualties")
#table
table(fire_data$tfs_firefighter_casualties)

## 15. Response Time (Numerical) ##
#Plot
ggplot(fire_data, aes(x = response_time)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  theme_minimal() +
  ggtitle("Response Time Distribution") +
  xlab("Response Time (minutes)") +
  ylab("Frequency") +
  stat_bin(aes(label = ..count..), geom = "text", vjust = 0.5, size = 3) +
  xlim(0, 15) 
#Summary Table
summary(fire_data$response_time)


#### Understand Potential Indicators of Severity ####
## Flag High-Severity Incident##
# Define thresholds (example: $30000 loss or 5+ casualties)
fire_data$high_severity <- ifelse(fire_data$estimated_dollar_loss > 50000 | fire_data$total_casualties >= 5, "High", "Low")
# Count of High vs. Low severity
table(fire_data$high_severity)

# Bar chart of high vs. low severity
ggplot(fire_data, aes(x = high_severity, fill = high_severity)) +
  geom_bar() +
  labs(
    title = "Count of High vs. Low Severity Incidents",
    x = "Severity Level",
    y = "Count"
  ) +
  scale_fill_manual(values = c("High" = "darkblue", "Low" = "darkgreen")) +
  theme_minimal()

## Financial loss vs. Casualties ##
fire_data$total_casualties <- fire_data$civilian_casualties + fire_data$tfs_firefighter_casualties

#heatmap
fire_data <- fire_data %>%
  mutate(
    loss_group = cut(
      estimated_dollar_loss,
      breaks = c(0, 1000, 10000, 50000, 500000, Inf),
      labels = c("<1k", "1k-10k", "10k-50k", "50k-500k", ">500k"),
      right = FALSE
    ),
    casualty_group = cut(
      total_casualties,
      breaks = c(0, 1, 2, 5, Inf),
      labels = c("0", "1", "2-5", ">5"),
      right = FALSE
    )
  )

severity_table <- fire_data %>%
  group_by(loss_group, casualty_group) %>%
  summarize(count = n())

ggplot(severity_table, aes(x = loss_group, y = casualty_group, fill = count)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  labs(
    title = "Heatmap of Financial Loss and Casualty Groups",
    x = "Financial Loss Group",
    y = "Casualty Group",
    fill = "Incident Count"
  ) +
  theme_minimal()

# scatter
ggplot(fire_data, aes(x = as.factor(total_casualties), y = estimated_dollar_loss)) +
  geom_boxplot(fill = "skyblue", outlier.color = "red") +
  labs(
    title = "Financial Loss by Number of Casualties",
    x = "Casualties Count",
    y = "Estimated Dollar Loss"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar_format())


##Area of Origin vs. Financial Loss ##
# Summarize data by area_of_origin_grouped
severity_by_area <- fire_data %>%
  group_by(area_of_origin_grouped) %>%
  summarize(
    avg_loss = mean(estimated_dollar_loss, na.rm = TRUE),
  )

# Reshape for plotting
severity_long_are <- severity_by_area %>%
  tidyr::pivot_longer(cols = c(avg_loss), names_to = "metric", values_to = "value")

# Bar chart
ggplot(severity_long_are, aes(x = reorder(area_of_origin_grouped, value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Severity by Area of Origin",
    x = "Area of Origin",
    y = "Average Value",
    fill = "Severity Metric"
  ) +
  theme_minimal()


##Ignition Source vs. Financial Loss ##
severity_by_source <- fire_data %>%
  group_by(ignition_source_grouped) %>%
  summarize(
    avg_loss = mean(estimated_dollar_loss, na.rm = TRUE),
  )
# Reshape for plotting
severity_long_source <- severity_by_area %>%
  tidyr::pivot_longer(cols = c(avg_loss), names_to = "metric", values_to = "value")
# Bar chart
ggplot(severity_long_source, aes(x = reorder(area_of_origin_grouped, value), y = value, fill = metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::comma) +
  coord_flip() +
  labs(
    title = "Severity by Ignition Source",
    x = "Ignition Source",
    y = "Average Value",
    fill = "Severity Metric"
  ) +
  theme_minimal()


#### Correlations ####
# Boxplot for response time by area of origin
response_summary <- fire_data %>%
  group_by(area_of_origin_grouped) %>%
  summarize(avg_response_time = mean(response_time, na.rm = TRUE))
# Bar plot
ggplot(response_summary, aes(x = area_of_origin_grouped, y = avg_response_time)) +
  geom_col(fill = "skyblue", color = "black") +
  labs(title = "Average Response Time by Area of Origin",
       x = "Area of Origin", y = "Average Response Time (minutes)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

##Line plot for average response time over months##
fire_data %>%
  group_by(month) %>%
  summarize(avg_response_time = mean(response_time, na.rm = TRUE)) %>%
  ggplot(aes(x = month, y = avg_response_time)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Average Response Time by Month",
       x = "Month", y = "Average Response Time (minutes)") +
  theme_minimal()


##number of responding apparatus and the Financial dollar loss ##
fire_data$Log_Dollar_Loss <- log(fire_data$estimated_dollar_loss + 1)

# Scatter plot between number of responding apparatus and final dollar loss
ggplot(fire_data, aes(y = number_of_responding_apparatus, x = Log_Dollar_Loss)) +
  geom_point(color = 'blue', alpha = 0.5) +
  labs(title = 'Number of Responding Apparatus vs Log of Final Dollar Loss',
       x = 'Number of Responding Apparatus',
       y = 'Log of Final Dollar Loss') +
  theme_minimal()

cor(fire_data$Log_Dollar_Loss, fire_data$number_of_responding_apparatus, use = "complete.obs")



#### Response Time Understanding####
## Outliers ##
# Calculate IQR for response_time
Q1 <- quantile(fire_data$response_time, 0.25, na.rm = TRUE)
Q3 <- quantile(fire_data$response_time, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

# Define upper and lower bounds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter out outliers
fire_data_clean <- fire_data %>%
  filter(response_time >= lower_bound & response_time <= upper_bound)

# Histogram after removing outliers
ggplot(fire_data_clean, aes(x = response_time)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(
    title = "Distribution of Response Times (Cleaned Data)",
    x = "Response Time (minutes)",
    y = "Frequency"
  ) +
  theme_minimal()

## Response time vs financial loss  ##
# Calculate IQR for estimated_dollar_loss
Q1_loss <- quantile(fire_data$estimated_dollar_loss, 0.25, na.rm = TRUE)
Q3_loss <- quantile(fire_data$estimated_dollar_loss, 0.75, na.rm = TRUE)
IQR_loss <- Q3_loss - Q1_loss

# Define upper and lower bounds for estimated dollar loss
lower_bound_loss <- Q1_loss - 1.5 * IQR_loss
upper_bound_loss <- Q3_loss + 1.5 * IQR_loss

# Filter out outliers for estimated dollar loss
fire_data_clean_loss <- fire_data_clean %>%
  filter(estimated_dollar_loss >= lower_bound_loss & estimated_dollar_loss <= upper_bound_loss)

# Histogram after removing outliers for estimated dollar loss
ggplot(fire_data_clean_loss, aes(x = estimated_dollar_loss)) +
  geom_histogram(binwidth = 5000, fill = "lightgreen", color = "black") +
  labs(
    title = "Distribution of Estimated Dollar Loss (Cleaned Data)",
    x = "Estimated Dollar Loss",
    y = "Frequency"
  ) +
  scale_x_continuous(labels = scales::comma) +  # Format axis labels with commas
  theme_minimal()

























####


