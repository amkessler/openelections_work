library(tidyverse)
library(janitor)
library(readxl)

### Bringing in and transforming to long for each race


## PRESIDENTIAL ####
presidential <- read_excel("NY_Tioga/Tioga_NY_cleaned.xlsx", 
                               sheet = "presidential")

presidential

#convert to long (tidy) format
presidential_long <- presidential %>% 
  pivot_longer(cols = 2:11, names_to = "name", values_to = "votes")

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




## CONGRESSIONAL DISTRICT 23 ####
cd23 <- read_excel("NY_Tioga/Tioga_NY_cleaned.xlsx", 
                           sheet = "congress_NY-23")

#convert to long (tidy) format
cd23_long <- cd23 %>% 
  pivot_longer(cols = 2:10, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
cd23_long <- cd23_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "U.S. House",
    district = "23"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(cd23_long)



## STATE SENATE 52 ####
statesen52 <- read_excel("NY_Tioga/Tioga_NY_cleaned.xlsx", 
                   sheet = "statesen-52")

#convert to long (tidy) format
statesen52_long <- statesen52 %>% 
  pivot_longer(cols = 2:8, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
statesen52_long <- statesen52_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "State Senate",
    district = "52"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(statesen52_long)


## STATE ASSEMBLY 124 ####
statehou124 <- read_excel("NY_Tioga/Tioga_NY_cleaned.xlsx", 
                         sheet = "statehou-124")

#convert to long (tidy) format
statehou124_long <- statehou124 %>% 
  pivot_longer(cols = 2:8, names_to = "name", values_to = "votes")

#break out party affiliation and create race-specific values
statehou124_long <- statehou124_long %>% 
  mutate(
    temp = str_split(name, " - ", simplify = TRUE),
    candidate = temp[, 1],
    party = temp[, 2],
    office = "State Assembly",
    district = "124"
  ) %>%
  select(
    precinct, office, district, candidate, party, votes
  ) 

head(statehou124_long)



