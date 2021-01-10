library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## install/update from: https://github.com/amkessler/precinctsopenelex
                           ## remotes::install_github("amkessler/precinctsopenelex")


## PROCESS DATA FILES ####

# create state and county name variables
current_state <- "MI"
current_county <- "Montmorency"

# use custom package function to create input string to Excel file
infile_string <- precinctsopenelex::create_infile_string(current_state, current_county)
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
processed_cd01 <- reshape_precinct_data(read_excel(infile_string, sheet = "cd01"), 
                                  "U.S. House", 
                                  "01")
processed_cd01


## State House 72  ####
processed_statehou105 <- reshape_precinct_data(read_excel(infile_string, sheet = "statehou105"),
                                        "State House", 
                                        "105")
processed_statehou105



#there are also three special categories of votes: straight ticket votes, total registered voters and ballots cast
#we'll handle these below

## Straight Party Ticket  ####
processed_straightparty <- reshape_precinct_data(read_excel(infile_string, sheet = "straightparty"),
                                              "Straight Party", 
                                              "")
processed_straightparty


## Registered and Total Ballots  ####
#this one requires some manual work to finish up
reg_and_ballots <- read_excel(infile_string, sheet = "total_reg_and_cast") %>% 
                                      janitor::clean_names()
#use new package function to handle the final steps
processed_reg <- convert_toplevel_totals(reg_and_ballots, "registered_voters", "Registered Voters")
processed_ballots <- convert_toplevel_totals(reg_and_ballots, "voters_cast", "Ballots Cast")

processed_reg
processed_ballots


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
    county = current_county
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
  count(candidate) %>% View()



### EXPORT RESULTS ####

#build file name string to openelex standardized naming convention
#we'll once again use the custom package for this, as well as our location variables assigned at the top
outfile_string <- precinctsopenelex::create_outfile_string(current_state, current_county)
outfile_string

#export to csv
write_csv(processed_combined, outfile_string, na = "")

