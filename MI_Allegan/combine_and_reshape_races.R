library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## install from: https://github.com/amkessler/precinctsopenelex
                           ## remotes::("amkessler/precinctsopenelex")


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
# - district: text label for district (e.g. "42"; for statewide races use "")

# Presidential
processed_prez <- precinctsopenelex::reshape_precinct_data(read_excel(infile_string, sheet = "presidential"), 
                                  "President", 
                                  "")
processed_prez


## U.S. Senate ####
processed_ussenate <- reshape_precinct_data(read_excel(infile_string, sheet = "ussenate"), 
                                  "U.S. Senate", 
                                  "")
processed_ussenate


## Congressional - District 02 ####
processed_cd02 <- reshape_precinct_data(read_excel(infile_string, sheet = "cd02"), 
                                  "U.S. House", 
                                  "02")
processed_cd02

## Congressional - District 06 ####
processed_cd06 <- reshape_precinct_data(read_excel(infile_string, sheet = "cd06"), 
                                        "U.S. House", 
                                        "06")
processed_cd06


## State House 72  ####
processed_statehou72 <- reshape_precinct_data(read_excel(infile_string, sheet = "statehou72"),
                                        "State House", 
                                        "72")
processed_statehou72


## State House 80  ####
processed_statehou80 <- reshape_precinct_data(read_excel(infile_string, sheet = "statehou80"),
                                              "State House", 
                                              "80")
processed_statehou80


#there are also three special categories of votes: straight ticket votes, total registered voters and ballots cast
#we'll handle these below

## Straight Party Ticket  ####
processed_straightparty <- reshape_precinct_data(read_excel(infile_string, sheet = "straightparty"),
                                              "Straight Party", 
                                              "")
processed_straightparty

## Registered and Total Ballots  ####
processed_reg_and_ballots <- reshape_precinct_data(read_excel(infile_string, sheet = "total_reg_and_cast"),
                                                 "", 
                                                 "")
#this one requires a little manual step as well to finish up
processed_reg_and_ballots <- processed_reg_and_ballots %>% 
  mutate(
    office = candidate,
    candidate = "",
    party = ""
  )
processed_reg_and_ballots





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

