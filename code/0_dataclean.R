### CLEANING AND COMBINING RAW DATA FROM DUCKDB TO PRODUCE DATAFRAMES FOR ANALYSIS
### AUTHOR: AMY KIM

# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))

############################################################
######### CROSS-SECTIONAL DATA: GROUPING BY COUNTY #########
############################################################
## GROUPING FOR GENERAL COUNTY CHARACTERISTICS: FULL SAMPLE DATA 
countysumm_gen <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.)
  group_by(YEAR, STATEICP, COUNTYICP) %>% #grouping at the county level
  summarize(POP = n(), #overall population of county
            WHITEPOP = sum(ifelse(RACE == 1, 1, 0)), #white population
            NWHITETEACH = sum(ifelse(teacher==1 & RACE==1, 1, 0)), #number of white teachers
            NWHITEWORK = sum(ifelse(worker == 1 & RACE == 1, 1, 0)), #number of white workers
            NWHITEMW = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), # number of white married women
            NWHITESW = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), # number of white unmarried women
            URBAN = sum(ifelse(URBAN == 2, 1, 0))/n(), #percent of county living in urban area
            PCT_WHITE = sum(ifelse(RACE == 1, 1, 0))/n(), # percent of county that is white
            WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, 1, 0)), #white schoolage population
            LFP = sum(worker)/sum(ifelse(AGE >= 18 & AGE <= 64, 1, 0)), #share of prime age population that is in LF
            LFP_M = sum(ifelse(worker != 0 & demgroup == "M", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "M", 1, 0)), #lfp for men
            LFP_SW = sum(ifelse(worker != 0 & demgroup == "SW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "SW", 1, 0)), #lfp for single women
            LFP_MW = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW", 1, 0)), #lfp for married women
            LFP_WMW = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW" & RACE == 1, 1, 0)), #lfp for white married women
            PCT_LF_MW = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are MW
            PCT_LF_WMW = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are white MW
            PCT_UNDER20 = sum(ifelse(AGE < 20, 1, 0))/n(), #share of pop in each age group
            PCT_20TO39 = sum(ifelse(AGE >= 20 & AGE < 40, 1, 0))/n(), #share of pop in each age group
            PCT_40TO59 = sum(ifelse(AGE >= 40 & AGE < 60, 1, 0))/n(), #share of pop in each age group
            PCT_OVER59 = sum(ifelse(AGE >= 50, 1, 0))/n(), #share of pop in each age group
            NCHILD = mean(ifelse(demgroup == "MW", NCHILD, NA), na.rm=TRUE), #avg number of children for married women
            PCT_MARR = sum(ifelse(AGE >= 18 & SEX == 2 & MARST %in% c(1,2), 1, 0))/sum(ifelse(AGE >= 18 & SEX == 2, 1, 0)), #share adult women married
            PCT_HS_GRAD = sum(ifelse(EDUC > 6 & AGE >= 25, 1, 0))/sum(ifelse(AGE >= 25, 1, 0)), #share of adults >= 25 with at least HS educ
            PCT_LIT = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)) #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
            ) %>%
  collect() %>% #pulling into R as dataframe
  addvars_county() #adding county-level variables (treatment status, FIPS codes, etc.)

## GROUPING FOR TEACHER/SECRETARY-SPECIFIC COUNTY CHARACTERISTICS: FULL SAMPLE DATA (TEACHERS + SECRETARIES)
countysumm_occ <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% filter((teacher == 1| secretary == 1) & RACE == 1) %>% #only keeping white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% # grouping by countyXyear AND teacher/secretary
  summarize(num = n(), # number of teachers (or secretaries)
            num_mw = sum(ifelse(demgroup == "MW", 1, 0)), #num of teachers MW
            num_sw = sum(ifelse(demgroup == "SW", 1, 0)), #num of teachers SW
            num_m = sum(ifelse(demgroup == "M", 1, 0)), #num of teachers M
            pct_mw = sum(ifelse(demgroup == "MW", 1, 0))/n(), #share of teachers MW
            pct_sw = sum(ifelse(demgroup == "SW", 1, 0))/n(), #share of teachers SW
            pct_m = sum(ifelse(demgroup == "M", 1, 0))/n(), #share of teachers M
            pctw_marr = sum(ifelse(demgroup == "MW", 1, 0))/sum(ifelse(demgroup != "M", 1, 0)), #share of women teachers married
            avg_age_child = mean(ifelse(demgroup != "M", age_child, NA), na.rm=TRUE),
            avg_nchild = mean(ifelse(demgroup != "M", NCHILD, NA), na.rm=TRUE),
            num_agemarr = sum(ifelse(AGEMARR > 0, 1, 0)), #number of teachers sampled for age at marriage (if not sampled, AGEMARR = 0)
            pct_marr_before3 = (sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 7 , 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MB/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married more than 7 years ago (1933 NC)
            pct_marr_after3 = (1 - sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 7, 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MA/(MA+MB) x (MW)/(MW + SW + M) approx share of teachers MW AND married less than 7 years ago
            pct_marr_before8 = (sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 2, 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MB/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married more than 2 years ago (1938 KY)
            pct_marr_after8 = (1 - sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 2, 1 ,0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MA/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married less than 2 years ago 
            pct_wc = sum(ifelse(demgroup2 == "WC", 1, 0))/n(), #share of teachers who are women AND have children
            pct_wnc = sum(ifelse(demgroup2 == "WNC", 1, 0))/n() #share of teachers who are women AND DONT have children
  ) %>% collect() %>% 
  pivot_wider(id_cols = c(YEAR, STATEICP, COUNTYICP), names_from = OCC, values_from = -c(YEAR, STATEICP, COUNTYICP, OCC)) # pivoting wide on occupation (so each variable is now of form {varname}_Teacher or {varname}_Secretary)

##################################################################################
######### CROSS-SECTIONAL DATA: COMBINING COUNTY-LEVEL DATA FOR ANALYSIS #########
##################################################################################
## Initially combining just for matching
countysumm_raw <- countysumm_gen %>% full_join(countysumm_occ, by = c("YEAR", "STATEICP", "COUNTYICP"))

# main sample (default is filter on counties with at least 10 white teachers in 1930 and 1940 AND non-missing FIPS code AND observed in all four years 1910-1940)
mainsamp_list <- mainsamp(countysumm_raw)

# ###### MATCHING & SAMPLE SELECTION [NOTE -- REVISIT AND EDIT THIS, CURRENTLY NOT OK TO USE] #######
# # matching set 1
# matchvars1 <- c("POP", "PCT_LIT", "PCT_WHITE")
# matches <- matching(countysumm_raw, matchvars1)
# 
# # matching set 2
# matchvars2 <- c("LFP", "LFP_MW", "POP", "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59", "PCT_LIT", "PCT_WHITE")
# matches2 <- matching(countysumm_raw, matchvars2, retail = TRUE)
# 
# # matching set 3
# matchvars3 <- c("LFP", "LFP_MW", "POP", "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59", "PCT_LIT", "PCT_WHITE",
#                 "pct_sw_Teacher", "pct_mw_Teacher", "num_Teacher")
# matches3 <- matching(countysumm_raw, matchvars3, retail = TRUE)
# 
# matchlist <- list(matches, matches2, matches3)

# cleaning county-level combined data, joining with matches
countysumm <- countysumm_raw %>% #matching_join(matchlist = matchlist) %>% #helper function to join county-level combined data with list of matched data
  mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0), #indicator for whether county is in main sample (see above)
         pct_pop_Teacher = num_Teacher/WHITEPOP,
         pct_pop_Secretary = num_Secretary/WHITEPOP,
         pct_workers_Teacher = num_Teacher/NWHITEWORK, #percentage of workers that are teachers 
         pct_workers_Secretary = num_Secretary/NWHITEWORK, #percentage of workers that are secretaries
         pct_Teacher_mw = num_Teacher/NWHITEMW, #percentage of white married women that are teachers
         pct_Teacher_sw = num_Teacher/NWHITESW, #percentage of white unmarried women that are teachers
         teacher_ratio = num_Teacher/WHITESCHOOLPOP #ratio of number of teachers to white school-aged pop
  ) %>% state_matching(matchtype = "neighbor") #helper function to match individual counties to specific states in order to assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before

write_csv(countysumm, glue("{cleandata}/countysumm_new.csv"))

#################################################
######### DATA CLEANING FOR LINKED DATA #########
#################################################
# Creating 'linked view' -- NOTE: NOT A TABLE/DATAFRAME, just a linking to be filtered/mutated appropriately and collected
linkview <-  tbl(con, "linkedall") %>% addvars_indiv_linked() %>%
  select(c(ends_with("_base"),ends_with("_link"))) %>% #only keeping variables that have been selected (see duckdb_init)
  filter(SEX_base == SEX_link & RACE_base == RACE_link &  #only keeping links with consistent sex and race (drops 1.2% of links)
           AGE_base <= AGE_link - 5 & AGE_base >= AGE_link - 15) #and consistent age (age in base year 5-15 years less than age in link year) -- drops an additional 2.2% of links

# group 1: unmarried women teachers in 1930
link1<- linkview %>% filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) %>% 
  summlinks(n = 5) #only requiring that a county has at least 5 unmarried women teachers that are linked from 1920 to 1930 and 1930 to 1940

# group 2: unmarried women non-teachers in 1930
link2 <- linkview %>% filter(teacher_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 10 & RACE_base == 1) %>% 
  summlinks() 

# group 3: married women non-teachers in pre-period
link3 <- linkview %>% filter(teacher_base == 0 & demgroup_base == "MW" & AGE_base <= 50& RACE_base == 1) %>% 
  summlinks() 

# # summarizing links
# link1 %>% group_by(YEAR, TREAT) %>% summarize(across(starts_with("pct"), mean),n=n())
# link2 %>% group_by(YEAR) %>% summarize(across(starts_with("pct"), mean))
# link3 %>% group_by(YEAR) %>% summarize(across(starts_with("pct"), mean))
# 
# countysumm %>% filter(match_samp3 == 1) %>% group_by(YEAR, TREAT) %>% summarize( n=n())


dbDisconnect(con, shutdown = TRUE)