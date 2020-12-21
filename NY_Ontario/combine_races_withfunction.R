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
presidential <- read_excel("NY_Orleans/NY_Orleans_GE20_cleaned.xlsx", 
                           sheet = "presidential")
presidential

#run processing function 
processed_prez <- process_ny_data(presidential, "President", "")
processed_prez


## Congressional - District 27 ####

#import
cd27 <- read_excel("NY_Orleans/NY_Orleans_GE20_cleaned.xlsx", 
                   sheet = "congress_NY-27")
cd27

#process
processed_cd27 <- process_ny_data(cd27, "U.S. House", "27")
processed_cd27


## State Senate 62 ####
statesen62 <- read_excel("NY_Orleans/NY_Orleans_GE20_cleaned.xlsx", 
                   sheet = "statesen-62")

processed_statesen62 <- process_ny_data(statesen62, "State Senate", "62")
processed_statesen62


## State House 139 ####
statehou139 <- read_excel("NY_Orleans/NY_Orleans_GE20_cleaned.xlsx", 
                         sheet = "statehou-139")
statehou139

processed_statehou139 <- process_ny_data(statehou139, "State Assembly", "139")
processed_statehou139


## State House 144 ####
statehou144 <- read_excel("NY_Orleans/NY_Orleans_GE20_cleaned.xlsx", 
                          sheet = "statehou-144")
statehou144

processed_statehou144 <- process_ny_data(statehou144, "State Assembly", "144")
processed_statehou144


### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
processed_combined <- bind_rows(processed_prez, 
                                processed_cd27, 
                                processed_statesen62, 
                                processed_statehou139, 
                                processed_statehou144)

#add county name for all records 
processed_combined <- processed_combined %>% 
  mutate(
    county = "Orleans"
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
write_csv(processed_combined, "NY_Orleans/20201103__ny__general__orleans__precinct.csv", na = "")
