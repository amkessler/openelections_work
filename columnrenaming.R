library(tidyverse)
library(janitor)
library(readxl)

cnames = c('cylinder' = 'cyl', 'mile_per_gallon' = 'mpg')

mtcars

mtcars %>% rename(!!!cnames)


MI_statewide_colnames <- read_excel('MI_Lenawee/MI_statewide_colnames.xlsx', 
                                    sheet = 'rawnames')

MI_statewide_colnames %>% 
  clean_names() %>% 
  names() 


test <- MI_statewide_colnames %>% 
  clean_names() 

newcolnames <- c('Democratic Party - DEM' = 'democratic_party_dem',
  'Republican Party - REP' = 'republican_party_rep',
  'Libertarian Party - LIB' = 'libertarian_party_lib',
  'U.S. Taxpayers Party - UST' = 'u_s_taxpayers_party_ust',
  'Working Class Party - WCP' = 'working_class_party_wc',
  'Green Party - GRN' = 'green_party_grn',
  'Natural Law Party - NLP' = 'natural_law_party_nat',
  'WriteIn' = 'unresolved_write_in',
  'Joseph R. Biden - DEM' = 'joseph_r_biden_kamala_d_harris_dem',
  'Donald J. Trump - REP' = 'donald_j_trump_michael_r_pence_rep',
  'Jo Jorgensen - LIB' = 'jo_jorgensen_jeremy_cohen_lib',
  'Don Blankenship - UST' = 'don_blankenship_willia_m_mohr_ust',
  'Howie Hawkins - GRN' = 'howie_hawkins_angela_walker_grn',
  'Rocky De La Fuente - NAT' = 'rocky_de_la_fuente_darcy_richardson_nat',
  'Gary Peters - DEM' = 'gary_peters_dem',
  'John James - REP' = 'john_james_rep',
  'Valerie L. Willis - UST' = 'valerie_l_willis_ust',
  'Marcia Squier - GRN' = 'marcia_squier_grn',
  'Doug Dern - NAT' = 'doug_dern_nat')


test %>% 
  rename(!!!newcolnames)


### now a set of functions

mi_replace_colnames_straightparty <- function(df) {
  newcolnames <- c('Democratic Party - DEM' = 'democratic_party_dem',
                   'Republican Party - REP' = 'republican_party_rep',
                   'Libertarian Party - LIB' = 'libertarian_party_lib',
                   'U.S. Taxpayers Party - UST' = 'u_s_taxpayers_party_ust',
                   'Working Class Party - WCP' = 'working_class_party_wc',
                   'Green Party - GRN' = 'green_party_grn',
                   'Natural Law Party - NLP' = 'natural_law_party_nat',
                   'WriteIn' = 'unresolved_write_in')
  
  df_renamed <- df %>% 
    clean_names() %>% 
    rename(!!!newcolnames)
  
  return(df_renamed)
  
}



mi_replace_colnames_presidential <- function(df) {
  newcolnames <- c('Joseph R. Biden - DEM' = 'joseph_r_biden_kamala_d_harris_dem',
                   'Donald J. Trump - REP' = 'donald_j_trump_michael_r_pence_rep',
                   'Jo Jorgensen - LIB' = 'jo_jorgensen_jeremy_cohen_lib',
                   'Don Blankenship - UST' = 'don_blankenship_willia_m_mohr_ust',
                   'Howie Hawkins - GRN' = 'howie_hawkins_angela_walker_grn',
                   'Rocky De La Fuente - NAT' = 'rocky_de_la_fuente_darcy_richardson_nat',
                   'WriteIn' = 'unresolved_write_in')
  
  df_renamed <- df %>% 
    clean_names() %>% 
    rename(!!!newcolnames)
  
  return(df_renamed)
  
}



mi_replace_colnames_ussenate <- function(df) {
  newcolnames <- c('Gary Peters - DEM' = 'gary_peters_dem',
                   'John James - REP' = 'john_james_rep',
                   'Valerie L. Willis - UST' = 'valerie_l_willis_ust',
                   'Marcia Squier - GRN' = 'marcia_squier_grn',
                   'Doug Dern - NAT' = 'doug_dern_nat',
                   'WriteIn' = 'unresolved_write_in')
  
  df_renamed <- df %>% 
    clean_names() %>% 
    rename(!!!newcolnames)
  
  return(df_renamed)
  
}



test2 <- MI_statewide_colnames %>% 
  select(1:8) 

mi_replace_colnames_straightparty(test2) %>% 
  names()


test2 <- MI_statewide_colnames %>% 
  select(1:10) 

mi_replace_colnames_straightparty(test2) %>% 
  names()
