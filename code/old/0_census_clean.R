### CLEANING AND COMBINING RAW CENSUS DATA TO PRODUCE DATA FOR ANALYSIS -- OLD VERSION
### AUTHOR: AMY KIM
### LAST EDITED: MAY 2023

# HELPER FUNCTION: CREATE UNIQUE FIPS from STATEICP + COUNTYICP
## icpsr to fips crosswalk
fipscrosswalk <- read_xls(glue("{root}/StateFIPSicsprAB.xls"))
mergefips <- function(dataset, crosswalk = fipscrosswalk){
  outdata <- left_join(dataset, fipscrosswalk, by = c("STATEICP" = "STICPSR"))  %>% 
    mutate(FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),str_pad(COUNTYICP, 4, "left", pad = "0")),
           fips = ifelse(substr(FIPSRAW,6,6) == "0", substr(FIPSRAW,1,5), NA)) %>% select(-c(FIPS, NAME, STNAME, FIPSRAW))
  return(outdata)
}

###############################################
##### DATASET 1: 1% SAMPLE DATA 1900-2000 #####
###############################################
## reading in raw data
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))

# grouping by year
samp_byyear <- allyears_raw_samp %>% 
  group_by(YEAR) %>%
  mutate(demgroup = case_when(SEX == 1 ~ "Men",
                              SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                              TRUE ~ "Married Women"),
         teacher = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  group_by(YEAR, demgroup) %>%
  mutate(pop = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0))) %>%
  filter(LABFORCE == 2 & AGE >= 18 & AGE <= 64) %>% 
  summarise(pct_dem_teaching = sum(ifelse(teacher == 1, PERWT, 0))/sum(PERWT),
            numlf = sum(ifelse(LABFORCE == 2, PERWT, 0)),
            lfp = numlf/mean(pop),
            numteachers = sum(ifelse(teacher == 1,PERWT,0)),
            pct_coll_teachers = sum(ifelse(teacher == 1 & coll_above == 1, PERWT, 0))/sum(ifelse(coll_above == 1, PERWT, 0))) %>%
  group_by(YEAR) %>%
  mutate(pctteachers = numteachers/sum(numteachers),
         pctlf = numlf/sum(numlf))

write_csv(samp_byyear, glue("{outdata}/samp_byyear.csv"))

### FOR REFERENCE: SUMMARY STATS OF MARRIED WOMEN, MARRIED WORKING WOMEN, MARRIED WORKING WOMEN TEACHERS OVER TIME
mw_samp <- allyears_raw_samp %>% filter(SEX == 2 & (MARST ==1 | MARST == 2) & AGE >= 18 & AGE <= 64) %>% 
  mutate(URBAN = ifelse(URBAN == 2, 1, 0),
         VALUEH = ifelse(VALUEH >= 9999998 | VALUEH == 0, NA, VALUEH),
         SOMECOLL = ifelse(EDUC >= 7, 1, 0),
         WHITE = ifelse(RACE == 1, 1, 0),
         BLACK = ifelse(RACE == 2, 1, 0),
         INCWAGE = ifelse(INCWAGE >= 9999998, NA, INCWAGE))

summ_mw <- mw_samp %>% group_by(YEAR) %>% 
  summarize(across(c(URBAN, VALUEH, SOMECOLL, WHITE, BLACK, NCHILD, AGE, INCWAGE),
                   ~weighted.mean(.x, PERWT, na.rm=TRUE))) %>% mutate(cat = "All MW")

summ_mw_lf <- mw_samp %>% filter(LABFORCE == 2) %>% group_by(YEAR) %>%
  summarize(across(c(URBAN, VALUEH, SOMECOLL, WHITE, BLACK, NCHILD, AGE, INCWAGE),
                   ~weighted.mean(.x, PERWT, na.rm=TRUE))) %>% mutate(cat = "MW in LF")

summ_mw_teach <- mw_samp %>% filter(LABFORCE == 2 & OCC1950 == 93) %>% group_by(YEAR) %>%
  summarize(across(c(URBAN, VALUEH, SOMECOLL, WHITE, BLACK, NCHILD, AGE, INCWAGE),
                   ~weighted.mean(.x, PERWT, na.rm=TRUE))) %>% mutate(cat = "MW Teachers")

summ_mw_all <- bind_rows(list(summ_mw, summ_mw_lf, summ_mw_teach))
# ggplot(summ_mw_all, aes(x = YEAR, y = URBAN, color = cat)) + geom_line()
# ggplot(summ_mw_all, aes(x = YEAR, y = SOMECOLL, color = cat)) + geom_line()
# ggplot(summ_mw_all, aes(x = YEAR, y = BLACK, color = cat)) + geom_line()
# ggplot(summ_mw_all, aes(x = YEAR, y = AGE, color = cat)) + geom_line()
# ggplot(summ_mw_all, aes(x = YEAR, y = NCHILD, color = cat)) + geom_line()

##################################################################
##### DATASET 2: FULL COUNT TEACHER/SECRETARY DATA 1910-1940 #####
##################################################################
# initializing empty list of dataframes
filtereddata <- list()
cleandata <- list()
n_max = Inf #change only for testing

# iterating through each census file
i = 1
for (year in seq(1910,1940,10)){
  print(year)
  raw <- read_csv(glue("{rawdata}/census_{year}.csv"), n_max = n_max) #reading in file
  clean <- raw %>% 
    mutate(YEAR = year,
           demgroup = case_when(SEX == 1 ~ "M",
                                SEX == 2 & MARST == 6 ~ "SW",
                                TRUE ~ "MW"),
           teacher = ifelse(OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & AGE <= 64,1,0),# & RACE == 1, 1, 0),
           secretary = ifelse(OCC1950 == 350 & CLASSWKR == 2 & AGE >= 18 & AGE <= 64,1,0),# & RACE == 1, 1, 0),
           worker = ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, 1, 0)) %>%
    group_by(SERIAL) 
  cleandata[[i]] <- clean
  
  # filtering for only white teachers and secretaries
  filteredraw <- clean %>% ungroup() %>% 
    filter((teacher == 1 | secretary == 1) & RACE == 1 & MARST != 2 & MARST != 3 & MARST != 4 & MARST != 5 & AGE >= 18 & AGE <= 64) %>%
    #filter((teacher == 1 | secretary == 1) & MARST != 2 & MARST != 3 & MARST != 4 & MARST != 5 & AGE >= 18 & AGE <= 64) %>% 
    mutate(OCC = case_when(OCC1950 == 93 ~ "Teacher",
                           OCC1950 == 350 ~ "Secretary",
                           TRUE ~ NA_character_)) 
  
  filteredraw_spouse <- filteredraw %>% filter(SPLOC != 0) %>%
    left_join(clean %>% select(SERIAL, PERNUM, SPLOC, SEX, AGE, RACE, LABFORCE, CLASSWKR, OCC1950, OCCSCORE) %>%
                rename_with(~paste0(.x, "_SP")), by = c("SERIAL" = "SERIAL_SP", "SPLOC" = "PERNUM_SP")) %>%
    bind_rows(filter(filteredraw, SPLOC == 0))
  
  sum(which(filteredraw_spouse$SPLOC_SP != filteredraw_spouse$PERNUM))
  
  filtereddata[[i]] <- filteredraw_spouse
  
  i = i + 1
}

# binding dataframes and saving
filtered_bind <- bind_rows(filtereddata, .id = "column_label")
#write_csv(filtered_bind, glue("{outdata}/filtereddata.csv"))

linkeddata <- list()
j = 1
# linking
for (year in seq(1910,1930,10)){
  links <- read_csv(glue("{rawdata}/{year}_{year+10}.csv"), n_max = n_max)
  
  #getting appropriate census years
  rawbase <- cleandata[[j]]
  rawlink <- cleandata[[j+1]]
  
  #joining base year with links: only keeping indiv. who are linked to link year (i.e. in links)
  linkedbase <- rawbase %>% inner_join(links, by = c("HISTID"=glue("histid{year}"))) 
  
  #joining link year with links + base year: only keeping indiv. who are linked to base year (i.e. in links)
  linked <- rawlink %>% inner_join(linkedbase, by = c("HISTID"=glue("histid{year+10}")), suffix = c("_link","_base")) #linking link year by HISTID, adding suffixes for duplicate variables
  
  linkeddata[[j]] <- linked
  j = j + 1
}

linked_all <- bind_rows(linkeddata)
write_csv(linked_all, glue("{outdata}/linkedall.csv"))
linked_teach <- linked_all %>% filter(teacher_base == 1 | teacher_link == 1)
write_csv(linked_teach, glue("{outdata}/linkedteach.csv"))

# sample of college-educated women in 1940


#####################################################################
##### DATASET 3: TEACHER/SECRETARY COUNTY-LEVEL STATS 1910-1940 #####
#####################################################################
# grouping by county, shaping long on occupation
countydist_byocc <- filtered_bind %>% group_by(YEAR, STATEICP, COUNTYICP, OCC) %>%
  summarise(num = n(),
            numfem = sum(ifelse(SEX == 2, 1, 0)),
            nummale = num - numfem,
            numsf = sum(ifelse(SEX == 2 & MARST == 6, 1, 0)),
            nummf = sum(ifelse(SEX == 2 & MARST != 6, 1, 0)),
            nummf_agemarr = sum(ifelse(SEX == 2 & !is.na(AGEMARR) & AGEMARR > 0, 1, 0)),
            nummf_marr_before = sum(case_when(is.na(AGEMARR) | AGEMARR == 0 | SEX == 1 ~ 0,
                                              (STATEICP %in% c(47, 48, 40)) & (YEAR - (AGE - AGEMARR) < 1933) ~ 1,
                                              (STATEICP %in% c(51, 54, 56)) & (YEAR - (AGE - AGEMARR) < 1938) ~ 1,
                                              TRUE ~ 0)),
            pct_female_mar = nummf/numfem,
            share_male = nummale/num,
            share_female = numfem/num,
            share_single_fem = numsf/num,
            share_mar_fem = nummf/num,
            share_mar_before_mf = (nummf_marr_before/nummf_agemarr), # MB/(MA + MB)
            share_mar_after_mf = (1 - nummf_marr_before/nummf_agemarr),# MA/(MA + MB)
            share_mar_before_fem = (nummf_marr_before/nummf_agemarr)*pct_female_mar, # MB/(MA+MB) x (MW)/(MW + SW) approx share of women teachers who were married before law
            share_mar_after_fem = (1 - nummf_marr_before/nummf_agemarr)*pct_female_mar, # MA/(MA+MB) x (MW)/(MW + SW) approx share of women teachers who were married after law
            share_mar_before = (nummf_marr_before/nummf_agemarr)*share_mar_fem, # MB/(MA+MB) x (MW)/(MW + SW + M) approx share of teachers who were women & married before law
            share_mar_after = (1 - nummf_marr_before/nummf_agemarr)*share_mar_fem, # MA/(MA+MB) x (MW)/(MW + SW + M) approx share of teachers who were women & married after law
            share_ma_mb = (nummf_agemarr - nummf_marr_before)/nummf_marr_before,
            share_single = 1 - pct_female_mar,
            age = mean(AGE),
            NCHILD_MW = mean(ifelse(SEX == 2 & MARST != 6, NCHILD, NA), na.rm=TRUE),
            NCHLT5_MW = mean(ifelse(SEX == 2 & MARST != 6, NCHLT5, NA), na.rm=TRUE),
            OCCSCORE_MW_SP = mean(ifelse(SEX == 2 & MARST != 6, OCCSCORE_SP, NA), na.rm=TRUE),
            SOMECOLL_MW = mean(ifelse(SEX == 2 & MARST != 6, ifelse(EDUC >= 7, 1, 0), NA), na.rm=TRUE),
            AGE_MW = mean(ifelse(SEX ==2 & MARST != 6, AGE, NA), na.rm=TRUE)) %>%
  mutate(TREAT = ifelse(STATEICP == 47 | STATEICP == 51, 1, 0),
         SOUTH = ifelse(STATEICP %in% c(54, 41, 46, 51, 40, 56, 47, 11, 52, 48, 43, 44), 1, 0), #us census regions east south central and south atlantic
         TREAT_NEIGHBOR = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 40), 1, 0), #neighboring states of treated states
         STATEGROUP = case_when(TREAT == 1 ~ "Treated",
                                SOUTH == 1 & TREAT_NEIGHBOR == 1 ~ "Untreated (Neighbor)",
                                TRUE ~ "Untreated (Non-Neighbor)"),
         STATEGROUP_ALT1 = case_when(TREAT == 1 ~ "Treated",
                                     SOUTH == 1 ~ "Southern Untreated",
                                     TRUE ~ "Non-Southern Untreated"),
         STATEGROUP_ALT2 = case_when(TREAT == 1 ~ "Treated",
                                     TREAT_NEIGHBOR == 1 ~ "Neighboring Untreated",
                                     TRUE ~ "Non-Neighboring Untreated")) %>%
  ungroup()

# defining main sample of counties
main_county_samp <- inner_join(countydist_byocc %>% filter(YEAR == 1930 & OCC == "Teacher" & num >= 10) %>% select(c(STATEICP, COUNTYICP)),
                               countydist_byocc %>% filter(YEAR == 1940 & OCC == "Teacher" & num >= 10) %>% select(c(STATEICP, COUNTYICP))) %>% 
  mutate(main_samp = 1)

countydist_byocc <- countydist_byocc %>% left_join(main_county_samp)

countydist_wide <- countydist_byocc %>% pivot_wider(id_cols = c(YEAR, STATEICP, COUNTYICP, TREAT, SOUTH, TREAT_NEIGHBOR, STATEGROUP, STATEGROUP_ALT1, STATEGROUP_ALT2, main_samp),
                                                    names_from = OCC,
                                                    values_from = c(num, numfem, nummale, numsf, nummf, pct_female_mar, share_male, share_female, share_single_fem, share_mar_fem, 
                                                                    share_mar_before_fem, share_mar_before_mf, share_mar_after_fem, share_mar_before, share_mar_after,
                                                                    share_mar_after_mf, share_single, age, NCHILD_MW, NCHLT5_MW, share_ma_mb, nummf_marr_before, nummf_agemarr,
                                                                    OCCSCORE_MW_SP, SOMECOLL_MW, AGE_MW))

write_csv(countydist_byocc, glue("{outdata}/countydist_byocc.csv"))

##################################################################
##### DATASET 4: COUNTY-LEVEL SUMM STATS 1930-1940 ###############
##################################################################
## collapsing to summary stats (NOTE: THESE ARE FROM SAMPLED DATA -- LESS RELIABLE THAN DATA FROM FULL SAMPLE)
countysumm <- allyears_raw_samp %>% filter(YEAR <= 1940) %>%
  group_by(YEAR, STATEICP, COUNTYICP) %>% 
  mutate(demgroup = case_when(SEX == 1 ~ "M",
                              SEX == 2 & MARST == 6 ~ "SW",
                              TRUE ~ "MW"),
         worker = ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, PERWT, 0)) %>%
  summarize(POP = sum(PERWT),
            URBAN = sum(ifelse(URBAN == 2, PERWT, 0))/POP,
            WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
            LFP = sum(worker)/sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
            LFP_M = sum(ifelse(worker != 0 & demgroup == "M", PERWT, 0))/sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
            LFP_SW = sum(ifelse(worker != 0 & demgroup == "SW", PERWT, 0))/sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
            LFP_MW = sum(ifelse(worker != 0 & demgroup == "MW", PERWT, 0))/sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
            pct_female_mar_Worker = sum(ifelse(worker != 0 & demgroup == "MW", PERWT, 0))/sum(ifelse(worker != 0 & (demgroup == "MW" | demgroup == "SW"), PERWT, 0)),
            AGE = weighted.mean(AGE, PERWT),
            NCHILD = weighted.mean(ifelse(demgroup == "MW", NCHILD, NA), PERWT, na.rm=TRUE),
            NCHLT5 = weighted.mean(ifelse(demgroup == "MW", NCHLT5, NA), PERWT, na.rm=TRUE),
            PCT_HS_GRAD = sum(ifelse(EDUC > 6 & AGE >= 25, PERWT, 0))/sum(ifelse(AGE >= 25, PERWT, 0)),
            AGEMARR = weighted.mean(ifelse(AGEMARR == 0, NA, AGEMARR), PERWT, na.rm=TRUE),
            INCWAGE = weighted.mean(ifelse(INCWAGE <= 5002 & INCWAGE > 0, INCWAGE, NA), PERWT, na.rm=TRUE)) %>%
  full_join(countydist_wide) %>% #joining with county-level teacher summ stats
  left_join(fipscrosswalk, by= c("STATEICP" = "STICPSR")) %>% 
  mutate(FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),str_pad(COUNTYICP, 4, "left", pad = "0")),
         fips = ifelse(substr(FIPSRAW,6,6) == "0", substr(FIPSRAW,1,5), NA)) %>% select(-c(FIPS, NAME, STNAME, FIPSRAW))



write_csv(countysumm, glue("{outdata}/countysumm.csv"))

countysumm_allyears <- allyears_raw_samp %>% left_join(fipscrosswalk, by= c("STATEICP" = "STICPSR")) %>% 
  group_by(FIPS, YEAR) %>%
  mutate(demgroup = case_when(SEX == 1 ~ "M",
                              SEX == 2 & MARST == 6 ~ "SW",
                              TRUE ~ "MW"),
         worker = ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, PERWT, 0),
         teacher = ifelse(worker != 0 & OCC1950 == 93 & CLASSWKR == 2, PERWT, 0)) %>%
  summarize(pct_workers_mw = sum(ifelse(worker != 0 & demgroup == "MW", PERWT, 0))/sum(ifelse(worker != 0, PERWT, 0)),
            pct_teachers_mw = sum(ifelse(teacher != 0 & demgroup == "MW", PERWT, 0))/sum(ifelse(teacher != 0, PERWT, 0)))



# View(matched_data %>% group_by(TREAT) %>% 
#        summarize(across(c(starts_with("POP"), starts_with("URBAN"), starts_with("LFP"), starts_with("PCT_HS_GRAD"),
#                           starts_with("INCWAGE"), starts_with("share")), function(.x) mean(.x, na.rm=TRUE))))


# ## COMPARING INTENSIVE VS EXTENSIVE MARGIN HYPOTHESES
# marr_before <- filtered_bind %>% filter((YEAR == 1940 | YEAR == 1930) & OCC == "Teacher" & 
#                                           AGEMARR > 0 & AGEMARR <= AGE & STATEICP %in% c(47, 48, 51, 54, 56)) %>%
#   mutate(group = ifelse(STATEICP %in% c(47, 48), "NC", "KY"),
#          TREAT = ifelse(STATEICP %in% c(47, 51), 1, 0),
#          years_marr = AGE - AGEMARR,
#          marr_before = case_when(group == "NC" & years_marr > 7 ~ 1,
#                                  group == "KY" & years_marr > 2 ~ 1,
#                                  TRUE ~ 0))
# 
# sw_sample_1940 <- filtered_bind %>% filter(YEAR == 1940 & OCC == "Teacher" & 
#                                              STATEICP %in% c(47, 48, 51, 54, 56) & demgroup == "SW") %>%
#   slice_sample(prop = 0.05)
#   
# marr_before_all <- filtered_bind %>% filter(((YEAR == 1940 & demgroup == "MW" & AGEMARR > 0) | YEAR == 1930) & OCC == "Teacher" & 
#                                           STATEICP %in% c(47, 48, 51, 54, 56) & demgroup != "M") %>%
#   bind_rows(sw_sample_1940) %>%
#   mutate(group = ifelse(STATEICP %in% c(47, 48), "NC", "KY"),
#          TREAT = ifelse(STATEICP %in% c(47, 51), 1, 0),
#          years_marr = AGE - AGEMARR,
#          marr_before = case_when(group == "NC" & years_marr > 7 ~ 1,
#                                  group == "KY" & years_marr > 2 ~ 1,
#                                  TRUE ~ 0))
# 
# marr_before_summ <- marr_before %>% group_by(YEAR, TREAT) %>%
#   summarize(pct_marr_recent = 1 - mean(marr_before),
#             se = sd(marr_before)/sqrt(n())) %>%
#   mutate(pct_lb = pct_marr_recent - se,
#          pct_ub = pct_marr_recent + se)
# 
# ggplot(marr_before_summ, aes(x = YEAR, y = pct_marr_recent, color = factor(TREAT), group = factor(TREAT))) + 
#   geom_point() + geom_line() + geom_errorbar(aes(min = pct_lb, max = pct_ub, width = 0.5))
# 



