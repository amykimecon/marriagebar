### CLEANING AND COMBINING RAW CENSUS DATA
### AUTHOR: AMY KIM
### LAST EDITED: MAR 17 2023

library(haven)
library(tidyverse)
library(glue)

root = "/Users/amykim/Dropbox (Princeton)/marriagebar"
rawdata = glue("{root}/ipums_raw")
outdata = glue("{root}/clean_data")

# initializing empty list of dataframes
allyearsraw <- list()

# iterating through each census file
i = 0
for (year in seq(1900,1940,10)){
  i = i + 1
  raw <- read_csv(glue("{rawdata}/census_{year}.csv")) #reading in file
  
  # filtering for only teachers and secretaries
  filteredraw <- raw %>% group_by(CITY) %>% mutate(POP = n()) %>% ungroup() %>% 
    filter(OCC1950 == 93 | OCC1950 == 350) %>% 
    mutate(OCC = case_when(OCC1950 == 93 ~ "Teacher",
                           OCC1950 == 350 ~ "Secretary",
                           TRUE ~ NA_character_)) %>%
    select(c(YEAR, CITY, COUNTYICP, STATEICP, AGE, MARST, SEX, RACE, POP, OCC))

  allyearsraw[[i]] <- filteredraw
}

# binding dataframes and saving
write_csv(bind_rows(allyearsraw, .id = "column_label"), glue("{outdata}/allyears_occ.csv"))

