### CLEANING AND COMBINING RAW DATA FROM DUCKDB TO PRODUCE DATAFRAMES FOR ANALYSIS
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_0_dataclean.txt", append=FALSE)
print("***************** RUNNING: 0_dataclean.txt *****************\n\n")

# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)
#for Carolyn, in case connection on Dropbox isn't working:
#con <- dbConnect(duckdb(), dbdir = "C:\\Users\\ctsao\\Documents\\test_duckdb/db.duckdb", read_only=TRUE) 

#________________________________________________________
# 1% SAMPLE 1910-2000: GROUPING BY YEAR ----
#________________________________________________________
print("\n\n***** writing 1% sample 1910-2000, grouping by year *****\n\n")

# grouping by year and demographic group
samp_byyear <- tbl(con, "allyears_raw_samp") %>%
  #defining person-level variables
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                              SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                              TRUE ~ "Married Women")) %>%
  lf_summ_demgroup(wide = FALSE, outvars = c("lfp", "pctlf", "teachshare", "pctteach")) %>%
  collect()

write_csv(samp_byyear, glue("{cleandata}/samp_byyear.csv"))
#!#! CHECKED 

# grouping by year and demographic x college group
samp_byyear_coll <- tbl(con, "allyears_raw_samp") %>%
  mutate(demgroup = case_when(SEX == 2 & (MARST == 1 | MARST == 2) & RACE == 1 & EDUC %in% c(7, 8, 9, 10, 11) ~ "WMW, College",
                              TRUE ~ "Other")) %>%
  lf_summ_demgroup(wide = FALSE) %>% collect()

write_csv(samp_byyear_coll, glue("{cleandata}/samp_byyear_coll.csv"))

#________________________________________________________
# CROSS-SECTIONAL DATA, GROUPING BY COUNTY ----
#________________________________________________________
print("\n\n***** grouping full count census by county/state *****\n\n")

## GROUPING FULL SAMPLE DATA  ----
# by county
countysumm_gen <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.)
  group_by(YEAR, STATEICP, COUNTYICP) %>% #grouping at the county level
  summ_gen() %>%
  collect() %>%
  addvars_county() #adding county-level variables (treatment status, FIPS codes, etc.)
#!#! CHECKED 

# by state
statesumm_gen <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  group_by(YEAR, STATEICP) %>%
  summ_gen() %>%
  collect() 

## GROUPING FOR TEACHER/SECRETARY-SPECIFIC COUNTY CHARACTERISTICS: FULL SAMPLE DATA (TEACHERS + SECRETARIES)
print("\n\n***** grouping full count census by county/state, wide for teachers/secretaries *****\n\n")
countysumm_occ <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter((teacher == 1| secretary == 1) & RACE == 1) %>% #only keeping white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% # grouping by countyXyear AND teacher/secretary
  summ_occ() %>%
  collect() %>% 
  pivot_wider(id_cols     = c(YEAR, STATEICP, COUNTYICP), 
              names_from  = OCC, 
              values_from = -c(YEAR, STATEICP, COUNTYICP, OCC)) 
  # pivoting wide on occupation (so each variable is now of 
  # form {varname}_Teacher or {varname}_Secretary)
#!#! CHECKED 

countysummblk_occ <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter((teacher == 1| secretary == 1) & RACE == 2) %>% #only keeping BLACK teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% # grouping by countyXyear AND teacher/secretary
  summ_occ() %>%
  collect() %>% 
  pivot_wider(id_cols     = c(YEAR, STATEICP, COUNTYICP), 
              names_from  = OCC, 
              values_from = -c(YEAR, STATEICP, COUNTYICP, OCC)) 

countysummall_occ <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter((teacher == 1| secretary == 1)) %>% #keeping all white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% # grouping by countyXyear AND teacher/secretary
  summ_occ() %>%
  collect() %>% 
  pivot_wider(id_cols     = c(YEAR, STATEICP, COUNTYICP), 
              names_from  = OCC, 
              values_from = -c(YEAR, STATEICP, COUNTYICP, OCC)) 

statesumm_occ <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter((teacher == 1| secretary == 1) & RACE == 1) %>% #only keeping white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, OCC) %>% # grouping by stateXyear AND teacher/secretary
  summ_occ() %>%
  collect() %>% 
  pivot_wider(id_cols     = c(YEAR, STATEICP), 
              names_from  = OCC, 
              values_from = -c(YEAR, STATEICP, OCC)) 


#________________________________________________________
#  MATCHED DATA ----
#________________________________________________________
## Initially combine county and county-occ data just for matching
print("\n\n***** merging county and county-occ data... *****\n\n")
countysumm_raw <- countysumm_gen %>% 
  full_join(countysumm_occ, by = c("YEAR", "STATEICP", "COUNTYICP")) %>% 
  ungroup()

countysummblk_raw <- countysumm_gen %>% 
  full_join(countysummblk_occ, by = c("YEAR", "STATEICP", "COUNTYICP")) %>% 
  ungroup()

countysummall_raw <- countysumm_gen %>% 
  full_join(countysummall_occ, by = c("YEAR", "STATEICP", "COUNTYICP")) %>% 
  ungroup()

# main sample of counties (FIPS)
# (default is filter on counties with at least 10 white teachers in 
# 1930 and 1940 AND non-missing FIPS code AND observed in all four years 1910-1940)
print("\n\n***** identify main sample of counties... *****\n\n")
mainsamp_list <- mainsamp(countysumm_raw)
mainsampblk_list <- mainsamp(countysummblk_raw, grp = "black")
mainsampall_list <- mainsamp(countysummall_raw, grp = "all")
#!#! CHECKED

## Matching & samp selection
# variables to match on (excl retail)
matchvars <- c("URBAN","LFP", "LFP_MW", "POP", 
               "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59", 
               "PCT_LIT", "PCT_WHITE", 
               "pct_sw_Teacher", "pct_mw_Teacher")

# METHOD ONE: NEAREST NEIGHBOR
matches1 <- matching(countysumm_raw, matchvars)

# METHOD TWO: GENETIC (note: this takes a while to run, ~5 min)
matches2 <- matching(countysumm_raw, matchvars, method = "genetic", pop.size = 200)

# METHOD THREE: FULL
matches3 <- matching(countysumm_raw, matchvars, method = "full")

matchlist <- list(matches1, matches2, matches3)

#________________________________________________________
#  FINAL DATASET ----
#________________________________________________________
# cleaning county-level combined data, joining with matches
countysumm <- countysumm_raw %>% 
  matching_join(matchlist = matchlist) %>% #helper function to join county-level combined data with list of matched data
  mutate(mainsamp              = ifelse(FIPS %in% mainsamp_list, 1, 0), #indicator for whether county is in main sample (see above)
         pct_pop_Teacher       = num_Teacher/WHITEPOP,
         pct_pop_Secretary     = num_Secretary/WHITEPOP,
         pct_workers_Teacher   = num_Teacher/NWHITEWORK, #percentage of workers that are teachers 
         pct_workers_Secretary = num_Secretary/NWHITEWORK, #percentage of workers that are secretaries
         pct_Teacher_mw        = num_mw_Teacher/NWHITEMW, #percentage of white married women that are teachers
         pct_Teacher_mw_100   = pct_Teacher_mw*100,
         pct_Teacher_sw        = num_sw_Teacher/NWHITESW, #percentage of white unmarried women that are teachers
         pct_Teacher_sw_100   = pct_Teacher_sw*100,
         pct_Teacher_sw_young  = num_sw_young_Teacher/NWHITESW_YOUNG,
         pct_Teacher_sw_young_100   = pct_Teacher_sw_young*100,
         pct_Teacher_sw_mid  = num_sw_mid_Teacher/NWHITESW_MID,
         pct_Teacher_sw_mid_100   = pct_Teacher_sw_mid*100,
         teacher_ratio         = WHITESCHOOLPOP/num_Teacher #ratio of number of teachers to white school-aged pop
         ) # %>% 
  # #helper function to match individual counties to specific states in order to 
  # #assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before
  # state_matching(matchtype = "neighbor") 
#!#! CHECKED
write_csv(countysumm, glue("{cleandata}/countysumm.csv"))

# state level
statesumm <- statesumm_gen %>% full_join(statesumm_occ, by = c("YEAR", "STATEICP")) %>% ungroup() 
write_csv(statesumm, glue("{cleandata}/statesumm.csv"))

# black teachers only
countysumm_blk <- countysummblk_raw %>%
  mutate(mainsamp = ifelse(FIPS %in% mainsampblk_list, 1, 0))
write_csv(countysumm_blk, glue("{cleandata}/countysumm_blk.csv"))

# all teachers
countysumm_all <- countysummall_raw %>% 
  mutate(mainsamp = ifelse(FIPS %in% mainsampall_list, 1, 0))
write_csv(countysumm_all, glue("{cleandata}/countysumm_all.csv"))


#________________________________________________________
# LINKED DATA ----
#________________________________________________________
# Creating 'linked view' -- NOTE: NOT A TABLE/DATAFRAME, 
# just a linking to be filtered/mutated appropriately and collected
linkview <-  tbl(con, "linkedall") %>% 
  addvars_indiv_linked() %>%
  mutate(NCHILD_base = `NCHILD`, 
         NCHILD_link = `NCHILD_1`,
         OCCSCORE_base = `OCCSCORE`,
         OCCSCORE_link = `OCCSCORE_1`) %>% #temporary while NCHILD isn't explicitly relabelled in duckdb
  dplyr::select(c(ends_with("_base"),ends_with("_link"))) %>% #only keeping variables that have been selected (see duckdb_init)
  filter(SEX_base == SEX_link & RACE_base == RACE_link &  #only keeping links with consistent sex and race (drops 1.2% of links)
           AGE_base <= AGE_link - 5 & AGE_base >= AGE_link - 15) #and consistent age (age in base year 5-15 years less than age in link year) -- drops an additional 2.2% of links
##! CHECKED: JUST HAVE TO CONFIRM THE COMMENTS ABOVE WHEN RE-RUN

# group 1: unmarried women teachers in t-10
link1 <- linkview %>% 
  filter(teacher_base == 1 & worker_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) %>% 
  summlinks(n = 5) %>% #only requiring that a county has at least 5 unmarried women teachers that are linked from 1920 to 1930 and 1930 to 1940
  matching_join(matchlist)
write_csv(link1, glue("{cleandata}/link1_swt.csv"))
#!#! CHECKED

link1full <- linkview %>% filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) %>%
  collect()

# group 1.5: women teachers without children in t-10
link1point5 <- linkview %>%
  filter(teacher_base == 1 & worker_base == 1 & NCHILD_base == 0 & RACE_base == 1 & AGE_base <= 40) %>%
  summlinks(n = 5) %>% #only requiring that a county has at least 5 unmarried women teachers that are linked from 1920 to 1930 and 1930 to 1940
  matching_join(matchlist)
write_csv(link1point5, glue("{cleandata}/link1point5_wtnc.csv"))
#!#! CHECKED

# group 2: unmarried women not in labor force in pre-period
link2 <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) %>% 
  summlinks() %>%
  matching_join(matchlist)
write_csv(link2, glue("{cleandata}/link2_swnilf.csv"))
#!#! CHECKED 

# group 2.5: unmarried women in lf but not teaching in pre-period
link2point5 <- linkview %>% 
  filter(worker_base == 1 & teacher_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) %>% 
  summlinks() %>%
  matching_join(matchlist)
write_csv(link2point5, glue("{cleandata}/link2point5_swnt.csv"))

# group 3: married women not in labor force in pre-period
link3 <- linkview %>% 
  filter(LABFORCE_base == 1 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) %>% 
  summlinks() %>%
  matching_join(matchlist)
write_csv(link3, glue("{cleandata}/link3_mwnilf.csv"))
#!#! CHECKED 

# group 3.5: married women in lf but not teachers in pre-period 
link3point5 <- linkview %>% 
  filter(worker_base == 1 & teacher_base == 0 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) %>% 
  summlinks() %>%
  matching_join(matchlist)
write_csv(link3point5, glue("{cleandata}/link3point5_mwnt.csv"))

#________________________________________________________
# LINKED DATA FOR SECRETARIES----
#________________________________________________________
# group 1: unmarried women secretaries in t-10
link1sec <- linkview %>% 
  filter(secretary_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) %>% 
  summlinks_sec(n = 5) %>% #only requiring that a county has at least 5 unmarried women teachers that are linked from 1920 to 1930 and 1930 to 1940
  matching_join(matchlist)
write_csv(link1sec, glue("{cleandata}/link1_sws.csv"))

# group 2: unmarried women non-secretaries in pre-period
link2sec <- linkview %>% 
  filter(secretary_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) %>% 
  summlinks_sec() %>%
  matching_join(matchlist)
write_csv(link2sec, glue("{cleandata}/link2_swns.csv"))
#!#! CHECKED 

# group 3: married women non-secretaries in pre-period
link3sec <- linkview %>% 
  filter(LABFORCE_base == 1 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) %>% 
  summlinks_sec() %>%
  matching_join(matchlist)
write_csv(link3sec, glue("{cleandata}/link3_mwns.csv"))


dbDisconnect(con, shutdown = TRUE)
# close log ----
sink(NULL)


