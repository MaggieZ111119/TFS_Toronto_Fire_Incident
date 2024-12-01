#### Preamble ####
# Purpose: Downloads and saves the data from Open Data Toronto (https://open.toronto.ca/dataset/fire-incidents/)
# Author: Maggie Zhang
# Date: 29 November 2024
# Contact: maggiey.zhang@mail.utoronto.ca
# License: MIT
# Pre-requisites: Plan data ans simulated
# Any other information needed? NO


#### Workspace setup ####
library(opendatatoronto)
library(tidyverse)


#### TFS Data ####

raw_tfs_fire <- read_csv(file = "https://ckan0.cf.opendata.inter.prod-toronto.ca/dataset/64a26694-01dc-4ec3-aa87-ad8509604f50/resource/1e824947-d73b-4f48-9bac-7f7f3731a6b9/download/Fire%20Incidents%20Data.csv",
                            show_col_types = FALSE)


#### Save data ####
# Save the data to the repository
write_csv(raw_tfs_fire, "data/01-raw_data/raw_fire_data.csv") 

         
