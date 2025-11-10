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

## TEMP: GETTING LOCAL UNEMPLOYMENT AND MANUFACTURING RATES BY COUNTY
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))

tempstats <- allyears_raw_samp %>% filter(YEAR <= 1950) %>% group_by(STATEICP, COUNTYICP, YEAR) %>%
  summarize(share_manuf = sum(ifelse(LABFORCE == 2 & AGE >= 16 & IND1950 >= 300 & IND1950 < 400, PERWT, 0))/sum(ifelse(LABFORCE == 2 & AGE >= 16, PERWT, 0)),
            share_ag = sum(ifelse(LABFORCE == 2 & AGE >= 16 & IND1950 >= 100 & IND1950 < 200, PERWT, 0))/sum(ifelse(LABFORCE == 2 & AGE >= 16, PERWT, 0)),
            unemp_rate = sum(ifelse(EMPSTAT == 2 & AGE >= 16 & LABFORCE == 2, PERWT, 0),na.rm=TRUE)/sum(ifelse(AGE >= 16 & LABFORCE == 2, PERWT, 0), na.rm=TRUE)
            )

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
  addvars_county() %>%#adding county-level variables (treatment status, FIPS codes, etc.)
  left_join(tempstats, by = c('YEAR', 'STATEICP', 'COUNTYICP')) # temp

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
  filter((teacher == 1| secretary == 1)) %>% #all teachers and secretaries
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

countysummwht_occ <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  filter((teacher == 1| secretary == 1) & RACE == 1) %>% #keeping all white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% # grouping by countyXyear AND teacher/secretary
  summ_occ() %>%
  collect() %>%
  pivot_wider(id_cols     = c(YEAR, STATEICP, COUNTYICP),
              names_from  = OCC,
              values_from = -c(YEAR, STATEICP, COUNTYICP, OCC))

statesumm_occ <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  filter((teacher == 1| secretary == 1)) %>%
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, OCC) %>% # grouping by stateXyear AND teacher/secretary
  summ_occ() %>%
  collect() %>%
  pivot_wider(id_cols     = c(YEAR, STATEICP),
              names_from  = OCC,
              values_from = -c(YEAR, STATEICP, OCC))

statesummwht_occ <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  filter((teacher == 1| secretary == 1) & RACE == 1) %>%
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

countysummwht_raw <- countysumm_gen %>% 
  full_join(countysummwht_occ, by = c("YEAR", "STATEICP", "COUNTYICP")) %>% 
  ungroup()

# main sample of counties (FIPS)
# (default is filter on counties with at least 10 white teachers in 
# 1930 and 1940 AND non-missing FIPS code AND observed in all four years 1910-1940)
print("\n\n***** identify main sample of counties... *****\n\n")
mainsamp_list <- mainsamp(countysumm_raw, grp = "all")
mainsampblk_list <- mainsamp(countysummblk_raw, grp = "black")
mainsampwht_list <- mainsamp(countysummwht_raw, grp = "white")

#!#! CHECKED

## Matching & samp selection
# variables to match on
matchvars <- c("URBAN","LFP", "LFP_MW", "POP", 
               "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59", 
               "PCT_WHITE", 
               "pct_sw_Teacher", "pct_mw_Teacher", "share_manuf", "share_ag")

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
# adding border county indicators
borders <- read.table(glue("{root}/county_adjacency2010.txt"), sep = "\t", col.names = c("county_name","FIPS","border_name","border_FIPS")) %>%
  mutate(county_name = ifelse(county_name == "", NA, county_name),
         FIPS = str_pad(as.character(FIPS), 5, "left", pad = "0"),
         border_FIPS = str_pad(as.character(border_FIPS), 5, "left", pad = "0")) %>%
  fill(c(county_name, FIPS), .direction = "down") %>%
  mutate(state = substr(FIPS, 1, 2),
         border_state = substr(border_FIPS, 1, 2),
         border = ifelse(state != border_state, 1, 0),
         border_ctrl = ifelse(border & border_state %in% c("37", "21"), 1, 0),
         border_treat = ifelse(border & state %in% c("37", "21"), 1, 0)) 

border_treat <- filter(borders, border_treat == 1)$FIPS
border_ctrl <- unique(filter(borders, border_ctrl == 1)$FIPS)

# cleaning county-level combined data, joining with matches
countysumm <- countysumm_raw %>% 
  matching_join(matchlist = matchlist) %>% #helper function to join county-level combined data with list of matched data
  retailsales() %>% 
  mutate(mainsampblk = ifelse(FIPS %in% mainsampblk_list, 1, 0),
         mainsampwht = ifelse(FIPS %in% mainsampwht_list, 1, 0),
         mainsampall = ifelse(FIPS %in% mainsamp_list, 1, 0),
         bordertreat = ifelse(FIPS %in% border_treat, 1, 0),
         borderctrl = ifelse(FIPS %in% border_ctrl, 1, 0),
         pct_pop_Teacher       = num_Teacher/POP,
         pct_pop_Secretary     = num_Secretary/POP,
         pct_Teacher_mw        = num_mw_Teacher/NMW, #percentage of married women that are teachers
         pct_Teacher_mw_100   = pct_Teacher_mw*100,
         teacher_ratio         = SCHOOLPOP/num_Teacher #ratio of number of teachers to school-aged pop
         ) # %>% 
  # #helper function to match individual counties to specific states in order to 
  # #assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before
  # state_matching(matchtype = "neighbor") 
#!#! CHECKED
write_csv(countysumm, glue("{cleandata}/countysumm.csv"))

# state level
statesumm <- statesumm_gen %>% full_join(statesumm_occ, by = c("YEAR", "STATEICP")) %>% ungroup() 
write_csv(statesumm, glue("{cleandata}/statesumm.csv"))

statesumm_wht <- statesumm_gen %>% full_join(statesummwht_occ, by = c("YEAR", "STATEICP")) %>% ungroup() 
write_csv(statesumm_wht, glue("{cleandata}/statesumm_wht.csv"))

# black teachers only
countysumm_blk <- countysummblk_raw %>%
  matching_join(matchlist = matchlist) %>%
  mutate(mainsampblk = ifelse(FIPS %in% mainsampblk_list, 1, 0),
         mainsampwht = ifelse(FIPS %in% mainsampwht_list, 1, 0),
         mainsampall = ifelse(FIPS %in% mainsamp_list, 1, 0),
         bordertreat = ifelse(FIPS %in% border_treat, 1, 0),
         borderctrl = ifelse(FIPS %in% border_ctrl, 1, 0),
         pct_Teacher_mw        = num_mw_Teacher/NBLACKMW, #percentage of white married women that are teachers
         pct_Teacher_mw_100   = pct_Teacher_mw*100,
         teacher_ratio = BLACKSCHOOLPOP/num_Teacher)
write_csv(countysumm_blk, glue("{cleandata}/countysumm_blk.csv"))

# white teachers
countysumm_wht <- countysummwht_raw %>%
  matching_join(matchlist = matchlist) %>% 
  mutate(mainsampblk = ifelse(FIPS %in% mainsampblk_list, 1, 0),
         mainsampwht = ifelse(FIPS %in% mainsampwht_list, 1, 0),
         mainsampall = ifelse(FIPS %in% mainsamp_list, 1, 0),
         bordertreat = ifelse(FIPS %in% border_treat, 1, 0),
         borderctrl = ifelse(FIPS %in% border_ctrl, 1, 0),
         pct_Teacher_mw        = num_mw_Teacher/NWHITEMW, #percentage of white married women that are teachers
         pct_Teacher_mw_100   = pct_Teacher_mw*100,
         teacher_ratio = WHITESCHOOLPOP/num_Teacher)
write_csv(countysumm_wht, glue("{cleandata}/countysumm_wht.csv"))


#________________________________________________________
# LINKED DATA ----
#________________________________________________________
# constructing matchlist from countysumm 

# Creating 'linked view' -- NOTE: NOT A TABLE/DATAFRAME, 
# just a linking to be filtered/mutated appropriately and collected
linkview <-  tbl(con, "linkedall") %>% 
  addvars_indiv_linked() %>%
  mutate(NCHILD_base = `NCHILD`, 
         NCHILD_link = `NCHILD_1`,
         URBAN_base = `URBAN`,
         URBAN_link = `URBAN_1`,
         OCCSCORE_base = `OCCSCORE`,
         OCCSCORE_link = `OCCSCORE_1`) %>% #temporary while NCHILD isn't explicitly relabelled in duckdb
  dplyr::select(c(ends_with("_base"),ends_with("_link"))) %>% #only keeping variables that have been selected (see duckdb_init)
  filter(SEX_base == SEX_link & RACE_base == RACE_link &  #only keeping links with consistent sex and race (drops 1.2% of links)
           AGE_base <= AGE_link - 5 & AGE_base >= AGE_link - 15) %>% #and consistent age (age in base year 5-15 years less than age in link year) -- drops an additional 2.2% of links
  mutate(move = ifelse(STATEICP_base != STATEICP_link | COUNTYICP_base != COUNTYICP_link, 1, 0),
         move_state = ifelse(STATEICP_base != STATEICP_link, 1, 0),
         urbanind_link = ifelse(URBAN_link == 2, 1, 0),
         urbanind_base = ifelse(URBAN_base == 2, 1, 0),
         big_ct = case_when(STATEICP_base == 51 & COUNTYICP_base %in% c(1110) ~ 1,
                            STATEICP_base == 54 & COUNTYICP_base %in% c(1570, 370, 930, 650) ~ 1,
                            STATEICP_base == 40 & COUNTYICP_base %in% c(7600) ~ 1,
                            STATEICP_base == 56 & COUNTYICP_base %in% c(390) ~ 1,
                            STATEICP_base == 47 & COUNTYICP_base %in% c(810, 1190) ~ 1,
                            STATEICP_base == 48 & COUNTYICP_base %in% c(830) ~ 1,
                            TRUE ~ 0))

matchdata <- countysumm 

# INDIVIDUAL LINKAGE DATASETS
# group 1: unmarried women teachers in t-10
link1 <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & AGE_base <= 40) %>%
  indiv_link_clean(matchdata)

write_csv(link1, glue("{cleandata}/link1_swt_indiv.csv"))

# group 1.5: women teachers without children in t-10
link1point5 <- linkview %>%
  filter(teacher_base == 1 & NCHILD_base == 0 & AGE_base <= 40) %>%
  indiv_link_clean(matchdata)

write_csv(link1point5, glue("{cleandata}/link1point5_wtnc_indiv.csv"))

# group 2: married women not in labor force in pre-period
link2 <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18)  %>%
  indiv_link_clean(matchdata)

write_csv(link2, glue("{cleandata}/link2_mwnilf_indiv.csv"))

# group 2.5: married women in lf but not teachers in pre-period 
link2point5 <- linkview %>% 
  filter(worker_base == 1 & teacher_base == 0 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18) %>% 
  indiv_link_clean(matchdata)

write_csv(link2point5, glue("{cleandata}/link2point5_mwnt_indiv.csv"))

# group 3: unmarried women not in labor force (note: either 16+ and not in LF or 8-16 everyone) in pre-period
link3 <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8) %>%
  indiv_link_clean(matchdata)

write_csv(link3, glue("{cleandata}/link3_swnilf_indiv.csv"))

# group 3.5: unmarried women in lf but not teaching in pre-period
link3point5 <- linkview %>% 
  filter(worker_base == 1 & teacher_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8) %>% 
  indiv_link_clean(matchdata)

write_csv(link3point5, glue("{cleandata}/link3point5_swnt_indiv.csv"))

link1_state <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & AGE_base <= 40 & RACE_base == 1) %>%
  summlinks_state()
write_csv(link1_state, glue("{cleandata}/link1_swt_state.csv"))

link2_state <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1)  %>%
  summlinks_state()
write_csv(link2_state, glue("{cleandata}/link2_mwnilf_state.csv"))

link3_state <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) %>%
  summlinks_state()
write_csv(link3_state, glue("{cleandata}/link3_swnilf_state.csv"))


#________________________________________________________
# LINKED DATA FOR SECRETARIES----
#________________________________________________________
# group 1: unmarried women secretaries in t-10
link1sec <- linkview %>% 
  filter(secretary_base == 1 & demgroup_base == "SW" & AGE_base <= 40) %>% 
  indiv_link_clean_sec(matchdata, n = 5)
write_csv(link1sec, glue("{cleandata}/link1_sws_indiv.csv"))

# # group 2: married women not in lf in pre-period
# link2sec <- linkview %>% 
#   filter(LABFORCE_base == 1 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) %>% 
#   indiv_link_clean_sec(matchlist)
# write_csv(link2sec, glue("{cleandata}/link2_mwns_indiv.csv"))
# 
# # group 3: unmarried women non-secretaries in pre-period
# link3sec <- linkview %>% 
#   filter(LABFORCE_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) %>% 
#   indiv_link_clean_sec(matchlist)
# write_csv(link3sec, glue("{cleandata}/link3_swns_indiv.csv"))

#!#! CHECKED 
dbDisconnect(con, shutdown = TRUE)
# close log ----
sink(NULL)


