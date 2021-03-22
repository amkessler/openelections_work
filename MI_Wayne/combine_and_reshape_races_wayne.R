library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## install from: https://github.com/amkessler/precinctsopenelex
                           ## remotes::install_github("amkessler/precinctsopenelex")


## PROCESS DATA FILES ####

# create state and county name variables
current_state <- "MI"
current_county <- "Wayne"

# use custom package function to create input string to Excel file
infile_string <- precinctsopenelex::create_infile_string(current_state, current_county)
infile_string


# We'll now use THREE functions from the custom package to clean/process the data for each race:

# 1)
# Since this county has staggered/embedded precinct names and types mixed together, we'll run the function
# added to the package, mi_clean_embedded_precinct_names(), as well first out of the gate

# 2)
# We'll then run the column renaming function mi_format_column_names() as well 
# (Turns "Candidate (PTY)" into "Candidate - PTY")

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


## Congressional Districts ####
processed_cd11 <- read_excel(infile_string, sheet = "cd11") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("U.S. House", "11")

processed_cd11


processed_cd12 <- read_excel(infile_string, sheet = "cd12") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("U.S. House", "12")

processed_cd12


processed_cd13 <- read_excel(infile_string, sheet = "cd13") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("U.S. House", "13")

processed_cd13


processed_cd14 <- read_excel(infile_string, sheet = "cd14") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct)) %>% #to deal with line breaks in names
  reshape_precinct_data("U.S. House", "14")

processed_cd14



## State House ####
### Due to the enormous number of House districts for Wayne, this require some
### custom stuff here below to wrangle it in a more efficient way

#load pre-processed sheet with all but 3 districts (those done separately below)
state_mostcombined <- read_excel("MI_Wayne/MI_Wayne_GE20_cleaned.xlsx", 
                                  sheet = "allstate_except_manual")


state_mostcombined 

#aim to split up combined df into multiple dfs, one for each district
state_mostcombined

split_df <- split(state_mostcombined, state_mostcombined$breakvalue, drop = TRUE)
split_df
split_df[[1]] 
split_df[[2]] 


#first let's solve for one single district
data <- split_df[[1]] 
  
data

#clean out empty rows and columns, and turn first row of data where candidates are into column names
data <- data %>%
  janitor::remove_empty(c("cols", "rows")) %>% 
  janitor::row_to_names(1) #replace column names with first row, where candidates are listed

#take out unneeded columns
data <- data %>% 
  select(
    -1,
    -votecategory,
    -`Total Votes`
  ) %>% 
  rename(
    precinct = candnamerow
  )

data

#pull value of third column name
third_col_name <- data %>%
  select(3) %>%
  names()

#then use that column name variable to do a conditional replace of the precinct name only in new column
data <- data %>%
  mutate(
    testcol := if_else(is.na(!!sym(third_col_name)), precinct, "replaceme"), #need to use tidyeval !! here with sym to use variable name
    testcol = na_if(testcol, "replaceme")
  )

data

#now, we'll use tidyr's fill() function to fill down each name through the NAs until it hits a new one
data <- data %>%
  tidyr::fill(testcol, .direction = "down")


#now that we have the precinct name with every row, we can filter for just the "Total" counts we want
data <- data %>%
  filter(precinct == "Total")

#now we'll replace the precinct value with the precinct info captured in testcol
data <- data %>% 
  mutate(
    precinct = testcol
  ) %>% 
  select(-testcol)

data

#now we're talking.

#use package function to format the candidate name column headers
data <- data %>% 
  mi_format_column_names() %>% 
  mutate(precinct = str_squish(precinct))

# isolate the district number as a value to feed to reshaping function
# e.g. reshape_precinct_data("State House", "14")
district_value <- data %>% 
  mutate(
    tempdist = gsub("[^0-9.-]", "", racename)
  ) %>% 
  distinct(tempdist) %>% 
  pull()

#set office value for reshaping function
office_value <- "State House"

#remove racename column
data <- data %>% 
  select(-racename)

data_reshaped <- data %>% 
  reshape_precinct_data(office_value, district_value)

data_reshaped



##### CREATING A FUNCTION FROM THE STATE HOUSE PROCESSING STEPS ####
# We'll turn the above worked-through code into a function to then apply to all
# house districts in the table.

split_process_statedistrict <- function(split_df_list, list_position_num) {
  #first let's solve for one single district
  data <- split_df_list[[list_position_num]] 
  #clean out empty rows and columns, and turn first row of data where candidates are into column names
  data <- data %>%
    janitor::remove_empty(c("cols", "rows")) %>% 
    janitor::row_to_names(1) #replace column names with first row, where candidates are listed
  #take out unneeded columns
  data <- data %>% 
    select(
      -1,
      -votecategory,
      -`Total Votes`
    ) %>% 
    rename(
      precinct = candnamerow
    )
  #pull value of third column name
  third_col_name <- data %>%
    select(3) %>%
    names()
  #use column name variable to do a conditional replace of the precinct name only in new column
  data <- data %>%
    mutate(
      testcol := if_else(is.na(!!sym(third_col_name)), precinct, "replaceme"), #need to use tidyeval !! here with sym to use variable name
      testcol = na_if(testcol, "replaceme")
    )
  #use tidyr's fill() function to fill down each name through the NAs until it hits a new one
  data <- data %>%
    tidyr::fill(testcol, .direction = "down")
  #filter for just the "Total" counts 
  data <- data %>%
    filter(precinct == "Total")
  #replace the precinct value with the precinct info captured in testcol
  data <- data %>% 
    mutate(
      precinct = testcol
    ) %>% 
    select(-testcol)
  #use package function to format the candidate name column headers
  data <- data %>% 
    mi_format_column_names() %>% 
    mutate(precinct = str_squish(precinct))
  # isolate the district number as a value to feed to reshaping function
  # e.g. reshape_precinct_data("State House", "14")
  district_value <- data %>% 
    mutate(
      tempdist = gsub("[^0-9.-]", "", racename)
    ) %>% 
    distinct(tempdist) %>% 
    pull()
  #set office value for reshaping function (this will always be same, since all st house dists)
  office_value <- "State House"
  #remove racename column
  data <- data %>% 
    select(-racename)
  data_reshaped <- data %>% 
    reshape_precinct_data(office_value, district_value)
  
  return(data_reshaped)

}


#run the function
split_process_statedistrict(split_df_list = split_df, list_position_num = 1)

#success!





#### CONTINUE HERE ######







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

