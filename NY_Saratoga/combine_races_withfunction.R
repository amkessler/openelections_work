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

# Presidential ####
#run import and processing function in one step 
processed_prez <- process_ny_data(read_excel("NY_Wyoming/Wyoming_NY_GE20_cleaned.xlsx", sheet = "presidential"), 
                                  "President", 
                                  "")
processed_prez


## Congressional - District 27 ####
processed_cd27 <- process_ny_data(read_excel("NY_Wyoming/Wyoming_NY_GE20_cleaned.xlsx", sheet = "cd27"), 
                                  "U.S. House", 
                                  "27")
processed_cd27


## State Senate 59 ####
processed_statesen59 <- process_ny_data(read_excel("NY_Wyoming/Wyoming_NY_GE20_cleaned.xlsx", sheet = "statesen59"),
                                        "State Senate", 
                                        "59")
processed_statesen59


## State House 114 ####
processed_statehou147 <- process_ny_data(read_excel("NY_Wyoming/Wyoming_NY_GE20_cleaned.xlsx", sheet = "statehou147"),
                                         "State Assembly", 
                                         "147")
processed_statehou147



### COMBINE INTO ONE #####

#combine tidy/long datasets created above 
processed_combined <- bind_rows(processed_prez,
                                processed_cd27, 
                                processed_statesen59, 
                                processed_statehou147)


#add county name for all records ####
processed_combined <- processed_combined %>% 
  mutate(
    county = "Wyoming"
  ) %>% 
  select(county, everything())

processed_combined


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

#use openelex naming convention
write_csv(processed_combined, "NY_Wyoming/20201103__ny__general__wyoming__precinct.csv", na = "")
