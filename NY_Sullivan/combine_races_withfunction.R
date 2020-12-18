library(tidyverse)
library(janitor)
library(readxl)

### Bringing in and transforming to long for each race


## PRESIDENTIAL ####
presidential <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                           sheet = "presidential")

presidential

#convert to long (tidy) format
presidential_long <- presidential %>% 
  pivot_longer(cols = 2:9, names_to = "name", values_to = "votes")

presidential_long

#break out party affiliation and create race-specific values
presidential_long <- presidential_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "President",
    district = ""
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(presidential_long)



## CONGRESSIONAL DISTRICT 19 ####
cd19 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                   sheet = "congress_NY-19")

#convert to long (tidy) format
cd19_long <- cd19 %>% 
  pivot_longer(cols = 2:8, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
cd19_long <- cd19_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "U.S. House",
    district = "19"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(cd19_long)



## STATE SENATE 42 ####
statesen42 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                   sheet = "statesen-42")

#convert to long (tidy) format
statesen42_long <- statesen42 %>% 
  pivot_longer(cols = 2:8, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
statesen42_long <- statesen42_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "State Senate",
    district = "42"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(statesen42_long)



## STATE HOUSE 100 ####
statehou100 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                         sheet = "statehou-100")

#convert to long (tidy) format
statehou100_long <- statehou100 %>% 
  pivot_longer(cols = 2:5, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
statehou100_long <- statehou100_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "State Assembly",
    district = "100"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(statehou100_long)


## STATE ASSEMBLY 101 ####
statehou101 <- read_excel("NY_Sullivan/Sullivan_NY_GE20_cleaned.xlsx", 
                          sheet = "statehou-101")

#convert to long (tidy) format
statehou101_long <- statehou101 %>% 
  pivot_longer(cols = 2:8, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
statehou101_long <- statehou101_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "State Assembly",
    district = "101"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(statehou101_long)



### COMBINE INTO ONE #####

#combine tidy datasets created above 
combined_long <- bind_rows(presidential_long, cd19_long, statesen42_long, statehou100_long, statehou101_long)

#check parties
combined_long %>% 
  count(party)

#check districts
combined_long %>% 
  count(district)

#add county name column 
combined_long <- combined_long %>% 
  mutate(
    county = "Sullivan"
  ) %>% 
  select(county, everything())

#standardize write-in phrasing to match openelex
combined_long <- combined_long %>% 
  mutate(
    candidate = str_replace_all(candidate, "WriteIn", "Write-Ins")
  )

combined_long


### EXPORT RESULTS ####

#use openelex naming convention
write_csv(combined_long, "NY_Sullivan/20201103__ny__general__sullivan__precinct.csv", na = "")
