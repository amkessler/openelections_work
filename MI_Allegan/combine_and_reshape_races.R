library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## https://github.com/amkessler/precinctsopenelex


## PROCESS DATA FILES ####

# create state and county name variables
target_state <- "MI"
target_county <- "Allegan"

# use custom package function to create input string to Excel file
infile_string <- precinctsopenelex::create_infile_string(target_state, target_county)
infile_string

# run reshaping function from custom package along with import in one step
# Function wants:
# - dataset (or import from excel function)
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42")

# Presidential
processed_prez <- precinctsopenelex::reshape_ny_data(read_excel(infile_string, sheet = "presidential"), 
                                  "President", 
                                  "")
processed_prez


## Congressional - District 20 ####
processed_cd20 <- process_ny_data(read_excel(filestring_import, sheet = "cd20"), 
                                  "U.S. House", 
                                  "20")
processed_cd20


## Congressional - District 21 ####
processed_cd21 <- process_ny_data(read_excel(filestring_import, sheet = "cd21"), 
                                  "U.S. House", 
                                  "21")
processed_cd21


## State Senate 43 ####
processed_statesen43 <- process_ny_data(read_excel(filestring_import, sheet = "statesen43"),
                                        "State Senate", 
                                        "43")
processed_statesen43


## State Senate 49 ####
processed_statesen49 <- process_ny_data(read_excel(filestring_import, sheet = "statesen49"),
                                        "State Senate", 
                                        "49")
processed_statesen49


## State House 108 ####
processed_statehou108 <- process_ny_data(read_excel(filestring_import, sheet = "statehou108"),
                                         "State Assembly", 
                                         "108")
processed_statehou108


## State House 112 ####
processed_statehou112 <- process_ny_data(read_excel(filestring_import, sheet = "statehou112"),
                                         "State Assembly", 
                                         "112")
processed_statehou112


## State House 113 ####
processed_statehou113 <- process_ny_data(read_excel(filestring_import, sheet = "statehou113"),
                                         "State Assembly", 
                                         "113")
processed_statehou113


## State House 114 ####
processed_statehou114 <- process_ny_data(read_excel(filestring_import, sheet = "statehou114"),
                                         "State Assembly", 
                                         "114")
processed_statehou114



### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
#we'll use a pattern matching to pull all dataframes in the environment
#with "processed" in the name and build a list of them to feed to bind_rows()
target_dfs <- grep("processed", names(.GlobalEnv), value=TRUE)
target_dfs_list <- do.call("list", mget(target_dfs))

processed_combined <- bind_rows(target_dfs_list)


#add county name for all records ####
#we'll use the saved county name specified at top of script
processed_combined <- processed_combined %>% 
  mutate(
    county = target_county
  ) %>% 
  select(county, everything()) %>% 
  arrange(office, district)



## MANUAL INTEGRITY CHECKS ####
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

#build file name string using openelex naming convention
filestring_export <- paste0(
                        "NY_",
                        target_county,
                        "/20201103__ny__general__",
                        str_to_lower(target_county),
                        "__precinct.csv"
                      )

#export to csv
write_csv(processed_combined, filestring_export, na = "")

