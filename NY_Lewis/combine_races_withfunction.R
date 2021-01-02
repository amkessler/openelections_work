library(tidyverse)
library(janitor)
library(readxl)
library(arrow)

#source processing function
source("process_ny_data_functions.R")

# Function wants:
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42")
                    

## PROCESS DATA FILES ####

# Presidential ####
#run import and processing function in one step 
processed_prez <- process_ny_data(read_excel("NY_Lewis/Lewis_NY_GE20_cleaned.xlsx", sheet = "presidential"), 
                                  "President", 
                                  "")
processed_prez


## Congressional - District 21 ####
processed_cd21 <- process_ny_data(read_excel("NY_Lewis/Lewis_NY_GE20_cleaned.xlsx", sheet = "cd21"), 
                                  "U.S. House", 
                                  "21")
processed_cd21


## State Senate 45 ####
processed_statesen47 <- process_ny_data(read_excel("NY_Lewis/Lewis_NY_GE20_cleaned.xlsx", sheet = "statesen47"),
                                        "State Senate", 
                                        "47")
processed_statesen47


## State House 114 ####
processed_statehou117 <- process_ny_data(read_excel("NY_Lewis/Lewis_NY_GE20_cleaned.xlsx", sheet = "statehou117"),
                                         "State Assembly", 
                                         "117")
processed_statehou117



### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
processed_combined <- bind_rows(processed_prez,
                                processed_cd21, 
                                processed_statesen47, 
                                processed_statehou117)


#add county name for all records ####
processed_combined <- processed_combined %>% 
  mutate(
    county = "Lewis"
  ) %>% 
  select(county, everything())

processed_combined


#check parties
processed_combined %>% 
  count(party)

#check districts
processed_combined %>% 
  count(office, district)

#check candidates
processed_combined %>% 
  count(candidate)


### EXPORT RESULTS ####

#use openelex naming convention
write_csv(processed_combined, "NY_Lewis/20201103__ny__general__lewis__precinct.csv", na = "")
arrow::write_feather(processed_combined, "NY_Lewis/20201103__ny__general__lewis__precinct.feather")

