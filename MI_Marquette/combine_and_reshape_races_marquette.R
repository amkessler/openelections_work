library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## install from: https://github.com/amkessler/precinctsopenelex
                           ## remotes::install_github("amkessler/precinctsopenelex")


## PROCESS DATA FILES ####

# create state and county name variables
current_state <- "MI"
current_county <- "Marquette"

# use custom package function to create input string to Excel file
infile_string <- precinctsopenelex::create_infile_string(current_state, current_county)
infile_string

# run reshaping function from custom package
# Function wants:
# - dataset (or import from excel function)
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42"; for statewide races use "")

# for compatible races also run column renaming function mi_format_column_names() as well 
# (Turns "Candidate (PTY)" into "Candiate - PTY")

# Presidential
processed_prez <- read_excel(infile_string, sheet = "presidential") %>%  
  precinctsopenelex::mi_format_column_names() %>% 
  precinctsopenelex::reshape_precinct_data("President", "")

processed_prez


## U.S. Senate ####
processed_ussenate <- read_excel(infile_string, sheet = "ussenate") %>%  
  mi_format_column_names() %>% 
  reshape_precinct_data("U.S. Senate", "")

processed_ussenate


## Congressional - District ####
processed_cd01 <- read_excel(infile_string, sheet = "cd01") %>%  
  mi_format_column_names() %>% 
  reshape_precinct_data("U.S. House", "01")

processed_cd01


## State House ####
processed_statehou109 <- read_excel(infile_string, sheet = "statehou109") %>%  
  mi_format_column_names() %>% 
  reshape_precinct_data("State House", "109")

processed_statehou109


## State House ####
processed_statehou110 <- read_excel(infile_string, sheet = "statehou110") %>%  
  mi_format_column_names() %>% 
  reshape_precinct_data("State House", "110")

processed_statehou110


#there are also three special categories of votes: straight ticket votes, total registered voters and ballots cast
#we'll handle these below

## Straight Party Ticket  ####
processed_straightparty <- read_excel(infile_string, sheet = "straightparty") %>%  
                                mi_format_column_names() %>% 
                                reshape_precinct_data("Straight Party", "")

processed_straightparty



## Registered and Total Ballots  ####
#this one requires some manual work and separate function to finish up
reg_and_ballots <- read_excel(infile_string, sheet = "total_reg_and_cast") %>% 
                                      janitor::clean_names()

#use package function to handle this
processed_reg <- precinctsopenelex::convert_toplevel_totals(reg_and_ballots, "registered_voters", "Registered Voters")
processed_ballots <- precinctsopenelex::convert_toplevel_totals(reg_and_ballots, "voters_cast", "Ballots Cast")


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

