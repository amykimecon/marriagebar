### CLEANING AND COMBINING RAW DATA FROM DUCKDB TO PRODUCE DATAFRAMES FOR ANALYSIS
### AUTHOR: AMY KIM

# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))

## GROUPING BY COUNTY FOR GENERAL COUNTY CHARACTERISTICS: FULL SAMPLE DATA
countysumm_gen <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% group_by(YEAR, STATEICP, COUNTYICP) %>% 
  summarize(POP = n(),
            NWHITETEACH = sum(ifelse(teacher==1 & RACE==1, 1, 0)), #number of white teachers
            NWHITEWORK = sum(ifelse(worker == 1 & RACE == 1, 1, 0)), #number of white workers
            NWHITEMW = sum(ifelse(demgroup == "MW" & RACE == 1, 1, 0)), # number of white married women
            NWHITESW = sum(ifelse(demgroup == "SW" & RACE == 1, 1, 0)), # number of white unmarried women
            URBAN = sum(ifelse(URBAN == 2, 1, 0))/n(), #percent of county living in urban area
            PCT_WHITE = sum(ifelse(RACE == 1, 1, 0))/n(), # percent of county that is black
            WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, 1, 0)), #white schoolage population
            LFP = sum(worker)/sum(ifelse(AGE >= 18 & AGE <= 64, 1, 0)), #share of prime age population that is in LF
            LFP_M = sum(ifelse(worker != 0 & demgroup == "M", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "M", 1, 0)), #lfp for men
            LFP_SW = sum(ifelse(worker != 0 & demgroup == "SW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "SW", 1, 0)), #lfp for single women
            LFP_MW = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW", 1, 0)), #lfp for married women
            LFP_WMW = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW" & RACE == 1, 1, 0)), #lfp for white married women
            PCT_LF_MW = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are MW
            PCT_LF_WMW = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are white MW
            PCT_UNDER20 = sum(ifelse(AGE < 20, 1, 0))/n(), #share in each age group
            PCT_20TO39 = sum(ifelse(AGE >= 20 & AGE < 40, 1, 0))/n(), #share in each age group
            PCT_40TO59 = sum(ifelse(AGE >= 40 & AGE < 60, 1, 0))/n(), #share in each age group
            PCT_OVER59 = sum(ifelse(AGE >= 50, 1, 0))/n(), #share in each age group
            NCHILD = mean(ifelse(demgroup == "MW", NCHILD, NA), na.rm=TRUE), #avg number of children for married women
            PCT_MARR = sum(ifelse(AGE >= 18 & SEX == 2 & MARST %in% c(1,2), 1, 0))/sum(ifelse(AGE >= 18 & SEX == 2, 1, 0)), #share adult women married
            PCT_HS_GRAD = sum(ifelse(EDUC > 6 & AGE >= 25, 1, 0))/sum(ifelse(AGE >= 25, 1, 0)), #share of adults >= 25 with at least HS educ
            PCT_LIT = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)) #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
            ) %>%
  collect() %>% addvars_county()

## GROUPING BY COUNTY FOR OCCUPATION-SPECIFIC COUNTY CHARACTERISTICS: FULL SAMPLE DATA (TEACHERS + SECRETARIES)
countysumm_occ <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% filter((teacher == 1| secretary == 1) & RACE == 1) %>% # & MARST %in% c(1,6)) %>% #only keeping white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>%
  summarize(num = n(), # number of teachers (or secretaries)
            num_mw = sum(ifelse(demgroup == "MW", 1, 0)), #num of teachers MW
            num_sw = sum(ifelse(demgroup == "SW", 1, 0)), #num of teachers SW
            num_m = sum(ifelse(demgroup == "M", 1, 0)), #num of teachers M
            pct_mw = sum(ifelse(demgroup == "MW", 1, 0))/n(), #share of teachers MW
            pct_sw = sum(ifelse(demgroup == "SW", 1, 0))/n(), #share of teachers SW
            pct_m = sum(ifelse(demgroup == "M", 1, 0))/n(), #share of teachers M
            pctw_marr = sum(ifelse(demgroup == "MW", 1, 0))/sum(ifelse(demgroup != "M", 1, 0)), #share of women teachers married
            num_agemarr = sum(ifelse(AGEMARR > 0, 1, 0)), #number of teachers sampled for age at marriage (if not sampled, AGEMARR = 0)
            pct_marr_before3 = (sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 7 , 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MB/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married more than 7 years ago (1933 NC)
            pct_marr_after3 = (1 - sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 7, 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MA/(MA+MB) x (MW)/(MW + SW + M) approx share of teachers MW AND married less than 7 years ago
            pct_marr_before8 = (sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 2, 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MB/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married more than 2 years ago (1938 KY)
            pct_marr_after8 = (1 - sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 2, 1 ,0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MA/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married less than 2 years ago 
            pct_wkids = sum(ifelse(NCHILD > 0 & demgroup != "M", 1, 0))/n() #share of teachers who are women AND have children
  ) %>% collect() %>% pivot_wider(id_cols = c(YEAR, STATEICP, COUNTYICP), names_from = OCC, values_from = -c(YEAR, STATEICP, COUNTYICP, OCC))


## COMBINING COUNTY-LEVEL DATA FOR ANALYSIS
countysumm_raw <- countysumm_gen %>% full_join(countysumm_occ, by = c("YEAR", "STATEICP", "COUNTYICP"))# %>% 
  #mutate(TREAT = ifelse(STATEICP == 47, 1, 0))

## MATCHING & SAMPLE SELECTION
# main sample
mainsamp_list <- mainsamp(countysumm_raw)

# matching set 1
matchvars1 <- c("POP", "PCT_LIT", "PCT_WHITE")
matches <- matching(countysumm_raw, matchvars1)

# matching set 2
matchvars2 <- c("LFP", "LFP_MW", "POP", "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59", "PCT_LIT", "PCT_WHITE")
matches2 <- matching(countysumm_raw, matchvars2, retail = TRUE)

# matching set 3
matchvars3 <- c("LFP", "LFP_MW", "POP", "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59", "PCT_LIT", "PCT_WHITE",
                "pct_sw_Teacher", "pct_mw_Teacher", "num_Teacher")
matches3 <- matching(countysumm_raw, matchvars3, retail = TRUE)

matchlist <- list(matches, matches2, matches3)

# cleaning county-level combined data
countysumm <- countysumm_raw %>% matching_join(matchlist = matchlist) %>%
  mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0),
         pct_workers_Teacher = num_Teacher/NWHITEWORK,
         pct_workers_Secretary = num_Secretary/NWHITEWORK,
         pct_mw_Teacher = num_Teacher/NWHITEMW,
         pct_sw_Teacher = num_Teacher/NWHITESW,
         teacher_ratio = num_Teacher/WHITESCHOOLPOP,
         STATE_MATCH = case_when(match == 1 ~ STATE_MATCH,
                                 TREAT == 1 ~ STATEICP,
                                 TRUE ~ NA_integer_),
         pct_marr_before_Teacher = case_when(STATE_MATCH == 47 ~ pct_marr_before3_Teacher, #if matched with NC (or in NC), 'law passes' in 1933
                                             STATE_MATCH == 51 ~ pct_marr_before8_Teacher, #if matched with KY (or in KY), 'law passes' in 1938
                                             TRUE ~ NA_real_),
         pct_marr_after_Teacher = case_when(STATE_MATCH == 47 ~ pct_marr_after3_Teacher, #if matched with NC (or in NC), 'law passes' in 1933
                                            STATE_MATCH == 51 ~ pct_marr_after8_Teacher, #if matched with KY (or in KY), 'law passes' in 1938
                                            TRUE ~ NA_real_),
         pct_marr_before_Secretary = case_when(STATE_MATCH == 47 ~ pct_marr_before3_Secretary, #if matched with NC (or in NC), 'law passes' in 1933
                                               STATE_MATCH == 51 ~ pct_marr_before8_Secretary, #if matched with KY (or in KY), 'law passes' in 1938
                                               TRUE ~ NA_real_),
         pct_marr_after_Secretary = case_when(STATE_MATCH == 47 ~ pct_marr_after3_Secretary, #if matched with NC (or in NC), 'law passes' in 1933
                                              STATE_MATCH == 51 ~ pct_marr_after8_Secretary, #if matched with KY (or in KY), 'law passes' in 1938
                                              TRUE ~ NA_real_)) %>% select(-c(starts_with("pct_marr_before3"),starts_with("pct_marr_after3"),starts_with("pct_marr_before8"),starts_with("pct_marr_after8")))

write_csv(countysumm, glue("{cleandata}/countysumm_new.csv"))

# ## INDIVIDUAL-LEVEL DATA
# indiv_teach <- tbl(con, "censusrawall") %>% addvars_indiv() %>% filter(teacher == 1) %>% collect() %>% addvars_county()

## LINKED DATA
# Creating 'linked view' -- NOTE: NOT A TABLE/DATAFRAME, just a linking to be filtered/mutated appropriately and collected
linkview <-  tbl(con, "linkedall") %>% addvars_indiv_linked() %>%
  select(c(ends_with("_base"),ends_with("_link"))) %>%
  filter(SEX_base == SEX_link, RACE_base == RACE_link)

# group 1: unmarried women teachers in 1930
link1 <- linkview %>% filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) %>% 
  group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
  summarize(n = n(),
            pct_t = sum(ifelse(teacher_link == 1, 1, 0))/n(),
            pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
  ) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  collect() %>%
  addvars_county() %>%
  matching_join(matchlist = matchlist) %>%
  mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0))

# group 2: unmarried women non-teachers in 1930
link2 <- linkview %>% filter(teacher_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 10& RACE_base == 1) %>% 
  group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
  summarize(pct_t = sum(ifelse(teacher_link == 1, 1, 0))/n(),
            pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
  ) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  collect() %>%
  addvars_county() %>%
  matching_join(matchlist = matchlist) %>%
  mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0))

# group 3: married women non-teachers in pre-period
link3 <- linkview %>% filter(teacher_base == 0 & demgroup_base == "MW" & AGE_base <= 50& RACE_base == 1) %>% 
  group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
  summarize(pct_t = sum(ifelse(teacher_link == 1, 1, 0))/n(),
            pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
  ) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  collect() %>%
  addvars_county() %>%
  matching_join(matchlist = matchlist) %>%
  mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0))

# summarizing links

link1 %>% group_by(YEAR, TREAT) %>% summarize(across(starts_with("pct"), mean),n=n())
link2 %>% group_by(YEAR) %>% summarize(across(starts_with("pct"), mean))
link3 %>% group_by(YEAR) %>% summarize(across(starts_with("pct"), mean))

countysumm %>% filter(match_samp3 == 1) %>% group_by(YEAR, TREAT) %>% summarize( n=n())


dbDisconnect(con, shutdown = TRUE)