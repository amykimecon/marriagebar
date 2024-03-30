## MATCHING COUNTIES ON PRE-PERIOD CHARACTERISTICS
match_data <- countysumm %>% filter(main_samp == 1) %>%
  pivot_wider(id_cols = c(STATEICP, COUNTYICP, TREAT, STATEGROUP, main_samp),
              names_from = YEAR, values_from = c(POP, URBAN, WHITESCHOOLPOP, LFP, LFP_M, LFP_SW, LFP_MW, pct_female_mar_Worker,
                                                 PCT_HS_GRAD, INCWAGE, share_male_Teacher, share_single_fem_Teacher,
                                                 share_mar_fem_Teacher)) %>%
  filter(!is.na(TREAT) & !is.na(URBAN_1930) & !is.na(LFP_1930) & !is.na(LFP_MW_1930) & !is.na(URBAN_1920) & !is.na(LFP_1920) & !is.na(LFP_MW_1920))


match_obj <- matchit(TREAT ~ URBAN_1930 + LFP_1930 + LFP_MW_1930 + URBAN_1920 + LFP_1920 + LFP_MW_1920,
                     data = match_data, REPLACE = TRUE)

summary(match_obj)

match_obj_alt <- matchit(TREAT ~ URBAN_1930 + LFP_1930 + LFP_MW_1930 + LFP_SW_1930 + pct_female_mar_Worker_1930 +
                           URBAN_1920 + LFP_1920 + LFP_MW_1920 + LFP_SW_1920 + pct_female_mar_Worker_1920,
                         data = match_data %>% filter(!is.na(WHITESCHOOLPOP_1930) & !is.na(WHITESCHOOLPOP_1920) &
                                                        !is.na(LFP_SW_1930) & !is.na(LFP_SW_1920) & !is.na(pct_female_mar_Worker_1930) & 
                                                        !is.na(pct_female_mar_Worker_1920)), REPLACE = TRUE)

summary(match_obj_alt)



matched_data <- match.data(match_obj) 
control_matches <- match_data[match_obj$match.matrix,] 
control_matches[["TREAT_STATE_MATCH"]] <- match_data[rownames(match_obj$match.matrix),][["STATEICP"]]
matched_counties <- bind_rows(matched_data %>% filter(TREAT == 1) %>% mutate(TREAT_STATE_MATCH = STATEICP) %>% select(STATEICP, COUNTYICP, TREAT_STATE_MATCH),
                              control_matches %>% select(STATEICP, COUNTYICP, TREAT_STATE_MATCH)) %>%
  mutate(matched = 1)

### RECONSTRUCTING DATASETS
countydist_byocc_matched <- filtered_bind %>% inner_join(matched_counties) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>%
  summarise(num = n(),
            numfem = sum(ifelse(SEX == 2, 1, 0)),
            nummale = num - numfem,
            numsf = sum(ifelse(SEX == 2 & MARST == 6, 1, 0)),
            nummf = sum(ifelse(SEX == 2 & MARST != 6, 1, 0)),
            nummf_agemarr = sum(ifelse(SEX == 2 & !is.na(AGEMARR) & AGEMARR > 0, 1, 0)),
            nummf_marr_before = sum(case_when(is.na(AGEMARR) | AGEMARR == 0 | SEX == 1 ~ 0,
                                              (TREAT_STATE_MATCH == 47) & (YEAR - (AGE - AGEMARR) < 1933) ~ 1,
                                              (TREAT_STATE_MATCH == 51) & (YEAR - (AGE - AGEMARR) < 1938) ~ 1,
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
main_county_samp_matched <- inner_join(countydist_byocc_matched %>% filter(YEAR == 1930 & OCC == "Teacher" & num >= 10) %>% select(c(STATEICP, COUNTYICP)),
                               countydist_byocc_matched %>% filter(YEAR == 1940 & OCC == "Teacher" & num >= 10) %>% select(c(STATEICP, COUNTYICP))) %>% 
  mutate(main_samp = 1)

countydist_byocc_matched <- countydist_byocc_matched %>% left_join(main_county_samp_matched)

countydist_wide_matched <- countydist_byocc_matched %>% pivot_wider(id_cols = c(YEAR, STATEICP, COUNTYICP, TREAT, SOUTH, TREAT_NEIGHBOR, STATEGROUP, STATEGROUP_ALT1, STATEGROUP_ALT2, main_samp),
                                                    names_from = OCC,
                                                    values_from = c(num, numfem, nummale, numsf, nummf, pct_female_mar, share_male, share_female, share_single_fem, share_mar_fem, 
                                                                    share_mar_before_fem, share_mar_before_mf, share_mar_after_fem, share_mar_before, share_mar_after,
                                                                    share_mar_after_mf, share_single, age, NCHILD_MW, NCHLT5_MW, share_ma_mb, nummf_marr_before, nummf_agemarr,
                                                                    OCCSCORE_MW_SP, SOMECOLL_MW, AGE_MW))

write_csv(countydist_byocc_matched, glue("{outdata}/countydist_byocc_matched.csv"))

##################################################################
##### DATASET 4: COUNTY-LEVEL SUMM STATS 1930-1940 ###############
##################################################################
## collapsing to summary stats (NOTE: THESE ARE FROM SAMPLED DATA -- LESS RELIABLE THAN DATA FROM FULL SAMPLE)
countysumm_matched <- allyears_raw_samp %>% filter(YEAR <= 1940) %>%
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
  full_join(countydist_wide_matched) #joining with county-level teacher summ stats

write_csv(countysumm_matched, glue("{outdata}/countysumm_matched.csv"))

