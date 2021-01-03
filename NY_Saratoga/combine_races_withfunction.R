library(tidyverse)
library(janitor)
library(readxl)
library(arrow)

#source processing function
source("process_ny_data_functions.R")

# Function wants:
# - dataset (or import from excel function)
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42")
                    

## PROCESS DATA FILES ####

# Presidential ####
#run import and processing function in one step 
processed_prez <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "presidential"), 
                                  "President", 
                                  "")
processed_prez


## Congressional - District 20 ####
processed_cd20 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "cd20"), 
                                  "U.S. House", 
                                  "20")
processed_cd20


## Congressional - District 21 ####
processed_cd21 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "cd21"), 
                                  "U.S. House", 
                                  "21")
processed_cd21



## State Senate 43 ####
processed_statesen43 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "statesen43"),
                                        "State Senate", 
                                        "43")
processed_statesen43


## State Senate 49 ####
processed_statesen49 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "statesen49"),
                                        "State Senate", 
                                        "49")
processed_statesen49



## State House 108 ####
processed_statehou108 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "statehou108"),
                                         "State Assembly", 
                                         "108")
processed_statehou108


## State House 112 ####
processed_statehou112 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "statehou112"),
                                         "State Assembly", 
                                         "112")
processed_statehou112


## State House 113 ####
processed_statehou113 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "statehou113"),
                                         "State Assembly", 
                                         "113")
processed_statehou113


## State House 114 ####
processed_statehou114 <- process_ny_data(read_excel("NY_Saratoga/Saratoga_NY_GE20_cleaned.xlsx", sheet = "statehou114"),
                                         "State Assembly", 
                                         "114")
processed_statehou114





### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
processed_combined <- bind_rows(processed_prez,
                                processed_cd20,
                                processed_cd21, 
                                processed_statesen43,
                                processed_statesen49,
                                processed_statehou108,
                                processed_statehou112,
                                processed_statehou113,
                                processed_statehou114)


#add county name for all records ####
processed_combined <- processed_combined %>% 
  mutate(
    county = "Saratoga"
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
write_csv(processed_combined, "NY_Lewis/20201103__ny__general__saratoga__precinct.csv", na = "")
arrow::write_feather(processed_combined, "NY_Lewis/20201103__ny__general__saratoga__precinct.feather")

