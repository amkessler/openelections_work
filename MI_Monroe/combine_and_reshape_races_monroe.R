library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## install from: https://github.com/amkessler/precinctsopenelex
                           ## remotes::install_github("amkessler/precinctsopenelex")


## PROCESS DATA FILES ####

# create state and county name variables
current_state <- "MI"
current_county <- "Monroe"

# use custom package function to create input string to Excel file
infile_string <- precinctsopenelex::create_infile_string(current_state, current_county)
infile_string


# We'll now use THREE functions from the custom package to clean/process the data for each race:

# 1)
# Since this county has staggered/embedded precinct names and types mixed together, we'll run the function
# added to the package, mi_clean_embedded_precinct_names(), as well first out of the gate

# 2)
# We'll then run the column renaming function mi_format_column_names() as well 
# (Turns "Candidate (PTY)" into "Candiate - PTY")

# 3)
# Then run the main reshaping function from the package to convert to long/tidy format and add standard openelex columns
# Function wants:
# - dataset (or import from excel function)
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42"; for statewide races use "")


######################################################################


# Presidential
processed_prez <- read_excel(infile_string, sheet = "presidential") %>%  
  precinctsopenelex:: mi_clean_embedded_precinct_names() %>% 
  precinctsopenelex::mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  precinctsopenelex::reshape_precinct_data("President", "")

processed_prez


## U.S. Senate ####
processed_ussenate <- read_excel(infile_string, sheet = "ussenate") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("U.S. Senate", "")

processed_ussenate


## Congressional - District ####
processed_cd07 <- read_excel(infile_string, sheet = "cd07") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("U.S. House", "07")

processed_cd07


## State House ####
processed_statehou17 <- read_excel(infile_string, sheet = "statehou17") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("State House", "17")

processed_statehou17


## State House - B ####
processed_statehou68 <- read_excel(infile_string, sheet = "statehou68") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("State House", "68")


## State House - C ####
processed_statehou56 <- read_excel(infile_string, sheet = "statehou56") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("State House", "56")

processed_statehou56


#there are also three special categories of votes: straight ticket votes, total registered voters and ballots cast
#we'll handle these below

## Straight Party Ticket  ####
processed_straightparty <- read_excel(infile_string, sheet = "straightparty") %>%  
                                mi_clean_embedded_precinct_names() %>% 
                                mi_format_column_names() %>% 
                                mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
                                reshape_precinct_data("Straight Party", "")

processed_straightparty 


## Registered and Total Ballots  ####
#this one requires some manual work and separate function to finish up
reg_and_ballots <- read_excel(infile_string, sheet = "total_reg_and_cast") %>% 
                                      mi_clean_embedded_precinct_names() %>% 
                                      mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
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

