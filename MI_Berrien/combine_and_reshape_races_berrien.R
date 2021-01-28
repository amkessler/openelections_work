library(tidyverse)
library(janitor)
library(readxl)
library(precinctsopenelex) ## this is custom package developed for this process
                           ## install from: https://github.com/amkessler/precinctsopenelex
                           ## remotes::install_github("amkessler/precinctsopenelex")


## PROCESS DATA FILES ####

# create state and county name variables
current_state <- "MI"
current_county <- "Berrien"

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


######################################################################
# Work on new handling for precinct names meshed with actual result type names.
# We only need the "total" values, but need the precinct names themselves to be part of that row/record


# start with each step in sequence on its own...

#import some of the data to work with
data <- read_excel("MI_Baraga/MI_Baraga_GE20_cleaned.xlsx", 
                                     sheet = "straightparty")

data
#retain another copy for testing function below
data2 <- data

#we can see that the precinct column includes not just the name of the precincts but the type of vote results, too.
#this isn't good so let's find a way to remedy it.

#first we need to determine if the second column is NA, since the pattern is the precinct names are NA for vote columns
#don't want to hardcode in a column name because they'll be different for every election race type. but 2nd should always be NA.
second_col_name <- data %>% 
  select(2) %>% 
  names()

#then use that second column name variable to do a conditional replace of the precinct name only in new column
data <- data %>% 
  mutate(
    testcol := if_else(is.na(!!sym(second_col_name)), precinct, "replaceme"), #need to use tidyeval !! here with sym b/c dyanmic variable name
    testcol = na_if(testcol, "replaceme")
    )

data

#now, we'll use tidyr's fill() function to fill down each name through the NAs until it hits a new one
data <- data %>% 
  tidyr::fill(testcol, .direction = "down")

data

#now that we have the precinct name with every row, we can filter for just the "Total" counts we want
data <- data %>% 
  filter(precinct == "Total")

data

#bingo now we've got it. 
# Just need to rename our test column as "precinct" and remove the unneeded vote type total column, order remaining columns
data <- data %>% 
  select(-precinct) %>% 
  rename(precinct = testcol) %>% 
  select(precinct, everything())

data

#and we're set!


#Let's test out running this data through the other existing processing functions now
data %>%  
  precinctsopenelex::mi_format_column_names() %>% 
  precinctsopenelex::reshape_precinct_data("Straight Party", "")

#woohoo


### Let's try to now make a function out of the above sequence of steps..



#we can see that the precinct column includes not just the name of the precincts but the type of vote results, too.
#this isn't good so let's find a way to remedy it.
data


mi_clean_embedded_precinct_names <- function(data) {
  #first we need to determine if the second column is NA, since the pattern is the precinct names are NA for vote columns
  #don't want to hardcode in a column name because they'll be different for every election race type. but 2nd should always be NA.
  second_col_name <- data %>% 
    select(2) %>% 
    names()
  #then use that second column name variable to do a conditional replace of the precinct name only in new column
  data <- data %>% 
    mutate(
      testcol := if_else(is.na(!!sym(second_col_name)), precinct, "replaceme"), #need to use tidyeval !! here with sym to use variable name
      testcol = na_if(testcol, "replaceme")
    )
  #now, we'll use tidyr's fill() function to fill down each name through the NAs until it hits a new one
  data <- data %>% 
    tidyr::fill(testcol, .direction = "down")
  #now that we have the precinct name with every row, we can filter for just the "Total" counts we want
  data <- data %>% 
    filter(precinct == "Total")
  #finally rename our test column as "precinct" and remove the unneeded vote type total column, order remaining columns
  data <- data %>% 
    select(-precinct) %>% 
    rename(precinct = testcol) %>% 
    select(precinct, everything())
  
  return(data)
}


mi_clean_embedded_precinct_names(data2)

#bingo.

# Now apply all three functions together

data %>%  
  mi_clean_embedded_precinct_names() %>% 
  precinctsopenelex::mi_format_column_names() %>% 
  precinctsopenelex::reshape_precinct_data("Straight Party", "")

# it works. woohoo.

# We'll now use the function to apply below to the rest of the Baraga County races.




######################################################################


# Presidential
processed_prez <- read_excel(infile_string, sheet = "presidential") %>%  
  mi_clean_embedded_precinct_names() %>% 
  precinctsopenelex::mi_format_column_names() %>% 
  precinctsopenelex::reshape_precinct_data("President", "")

processed_prez


## U.S. Senate ####
processed_ussenate <- read_excel(infile_string, sheet = "ussenate") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  reshape_precinct_data("U.S. Senate", "")

processed_ussenate


## Congressional - District ####
processed_cd01 <- read_excel(infile_string, sheet = "cd01") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  reshape_precinct_data("U.S. House", "01")

processed_cd01


## State House ####
processed_statehou110 <- read_excel(infile_string, sheet = "statehou110") %>%  
  mi_clean_embedded_precinct_names() %>% 
  mi_format_column_names() %>% 
  reshape_precinct_data("State House", "110")

processed_statehou110



#there are also three special categories of votes: straight ticket votes, total registered voters and ballots cast
#we'll handle these below

## Straight Party Ticket  ####
processed_straightparty <- read_excel(infile_string, sheet = "straightparty") %>%  
                                mi_clean_embedded_precinct_names() %>% 
                                mi_format_column_names() %>% 
                                reshape_precinct_data("Straight Party", "")

processed_straightparty



## Registered and Total Ballots  ####
#this one requires some manual work and separate function to finish up
reg_and_ballots <- read_excel(infile_string, sheet = "total_reg_and_cast") %>% 
                                      mi_clean_embedded_precinct_names() %>% 
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

