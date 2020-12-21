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
presidential <- read_excel("NY_Essex/Essex_NY_GE20_cleaned.xlsx", 
                           sheet = "presidential")
presidential

#run processing function 
processed_prez <- process_ny_data(presidential, "President", "")
processed_prez


## Congressional - District 21 ####

#import
cd21 <- read_excel("NY_Essex/Essex_NY_GE20_cleaned.xlsx", 
                   sheet = "cd21")
cd21

#process
processed_cd21 <- process_ny_data(cd21, "U.S. House", "21")
processed_cd21


## State Senate 45 ####
statesen45 <- read_excel("NY_Essex/Essex_NY_GE20_cleaned.xlsx", 
                   sheet = "statesen45")

processed_statesen45 <- process_ny_data(statesen45, "State Senate", "45")
processed_statesen45


## State House 114 ####
statehou114 <- read_excel("NY_Essex/Essex_NY_GE20_cleaned.xlsx", 
                         sheet = "statehou114")
statehou114

processed_statehou114 <- process_ny_data(statehou114, "State Assembly", "114")
processed_statehou114



### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
processed_combined <- bind_rows(processed_prez,
                                processed_cd21, 
                                processed_statesen45, 
                                processed_statehou114)

#add county name for all records 
processed_combined <- processed_combined %>% 
  mutate(
    county = "Essex"
  ) %>% 
  select(county, everything())

processed_combined

#check parties
processed_combined %>% 
  count(party)

#check districts
processed_combined %>% 
  count(office, district)


### EXPORT RESULTS ####

#use openelex naming convention
write_csv(processed_combined, "NY_Essex/20201103__ny__general__essex__precinct.csv", na = "")
