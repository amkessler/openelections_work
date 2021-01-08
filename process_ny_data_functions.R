library(tidyverse)
library(janitor)
library(readxl)


### We'll create a function to handle the repetitive part of the processing
# once the initial excel cleanup steps are taken. The file will have a precinct column,
# followed by columns with vote results with names formatted as "candidate - party"
# Function wants:
# - office: text label for office (e.g. "U.S. House")
# - district: text label for district (e.g. "42")
                                 
process_ny_data <- function(df, office, district, countyname){
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
