library(tidyverse)
library(janitor)
library(readxl)


# BUILD FUNCTION ####

### We'll create a function to handle the repetitive part of the processing
# once the initial excel cleanup steps are taken. The file will have a precinct column,
# followed by columns with vote results with names formatted as "candidate - party"
# Function wants:
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42")
                                 
process_ny_data <- function(df, office, district){
  #determine how many columns, since races can have diff number of candidates
  colnum <- length(colnames(df))  
  #begin processing dataset
  df <- df %>% 
    #transform to long/tidy format
    pivot_longer(cols = 2:all_of(colnum), names_to = "name", values_to = "votes") %>% 
    #clean and add necessary columns
    mutate(
      temp = str_split(name, " - ", simplify = TRUE),
      candidate = temp[, 1],
      party = temp[, 2],
      office = office,
      district = district,
      candidate = str_replace_all(candidate, "WriteIn", "Write-Ins") #standarize with openelex name
    ) %>%
    #reorder columns to match openelex format
    select(
      precinct, office, district, candidate, party, votes
    ) 
  #return results
  return(df)
}


## PROCESS DATA FILES ####

# presidential ####

#import excel file
presidential <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                           sheet = "presidential")
presidential

#run processing function 
processed_prez <- process_ny_data(presidential, "President", "")
processed_prez


## Congressional - District 19 ####

#import
cd19 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                   sheet = "congress_NY-19")
cd19

#process
processed_cd19 <- process_ny_data(cd19, "U.S. House", "19")
processed_cd19


## State Senate 42 ####
statesen42 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                   sheet = "statesen-42")

processed_statesen42 <- process_ny_data(statesen42, "State Senate", "42")
processed_statesen42


## State House 100 ####
statehou100 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                         sheet = "statehou-100")
statehou100

processed_statehou100 <- process_ny_data(statehou100, "State Assembly", "100")
processed_statehou100


## State House 101 ####
statehou101 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                          sheet = "statehou-101")
statehou101

processed_statehou101 <- process_ny_data(statehou101, "State Assembly", "101")
processed_statehou101


### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
processed_combined <- bind_rows(processed_prez, 
                                processed_cd19, 
                                processed_statesen42, 
                                processed_statehou100, 
                                processed_statehou101)

#add county name for all records 
processed_combined <- processed_combined %>% 
  mutate(
    county = "Sullivan"
  ) %>% 
  select(county, everything())

processed_combined

#check parties
processed_combined %>% 
  count(party)

#check districts
processed_combined %>% 
  count(district)


### EXPORT RESULTS ####

#use openelex naming convention
write_csv(processed_combined, "NY_Sullivan/20201103__ny__general__sullivan__precinct.csv", na = "")
