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
  pivot_longer(cols = 2:11, names_to = "office", values_to = "votes")

presidential_long


