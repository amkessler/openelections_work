library(tidyverse)
library(janitor)
library(readxl)

### Bringing in and transformingn to long each race

## Presidential

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
