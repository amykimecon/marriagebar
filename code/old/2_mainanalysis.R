### MAIN REGRESSIONS
### AUTHOR: AMY KIM
### LAST EDITED: APR 26 2023
library(haven)
library(tidyverse)
library(lmtest)
library(sandwich)
library(glue)
library(ivreg)

root = "/Users/amykim/Dropbox (Princeton)/marriagebar"
git = "Users/amykim/GitHub/marriagebar"
rawdata = glue("{root}/ipums_raw")
outdata = glue("{root}/clean_data")
outfigs = glue("{git}/figures")


#### CLEANING DATA (move to 0_census_clean.R later)
raw_teach <- read_csv(glue("{rawdata}/census_teach.csv"))
raw_1930 <- read_csv(glue("{rawdata}/census_1930.csv"))
raw_1940 <- read_csv(glue("{rawdata}/census_1940.csv"))
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))

# using 1 percent sample data to get county-level aggregates (population and school-age population)
samp_aggs <- allyears_raw_samp %>% filter(YEAR == 1930 | YEAR == 1940) %>%
  group_by(STATEICP, COUNTYICP, YEAR) %>%
  summarize(POP = sum(PERWT),
            WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
            MW_WORK = sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2 & LABFORCE == 2, PERWT, 0))/sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2, PERWT, 0)))

# # fullcount aggregates
# agg_1930 <- raw_1930 %>% group_by(STATEICP, COUNTYICP) %>%
#   summarize(POP = sum(PERWT),
#             WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
#             MW_WORK = sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2 & LABFORCE == 2, PERWT, 0))/sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2, PERWT, 0)))
# 
# agg_1940 <- raw_1940 %>% group_by(STATEICP, COUNTYICP) %>%
#   summarize(POP = sum(PERWT),
#             WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
#             MW_WORK = sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2 & LABFORCE == 2, PERWT, 0))/sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2, PERWT, 0)),
#             AGEMARR = mean(ifelse(AGEMARR == 0, NA, AGEMARR), na.rm=TRUE))
# 

## TESTING GROUPINGS FOR EDUC X EXPERIENCE CELLS
clean_teach <- raw_teach %>% filter(YEAR == 1940 & RACE == 1 & OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & EDUCD != 999 & INCWAGE > 0) %>%
    group_by(STATEICP, COUNTYICP) %>% 
  left_join(samp_aggs %>% filter(YEAR == 1930) %>% select(c(STATEICP, COUNTYICP, MW_WORK)), by = c("STATEICP", "COUNTYICP")) %>%
    mutate(GROUP = case_when(SEX == 1 ~ "Male",
                           SEX == 2 & MARST == 6 ~ "Single Female",
                           TRUE ~ "Married Female"),
           EMP_MAR = sum(ifelse(GROUP == "Married Female", 1, 0)),
           EMP_SINGLE = sum(ifelse(GROUP == "Single Female", 1, 0)),
         EDUCGRP = case_when(EDUC <= 9 ~ "Some or No College",
                             EDUC <= 11 ~ "College",
                             TRUE ~ NA_character_),
         EDUCYEARS = case_when(EDUCD < 13 ~ 0,
                               EDUCD < 20 ~ EDUCD - 13,
                               EDUCD < 24 ~ EDUCD - 17,
                               EDUCD < 30 ~ EDUCD - 18,
                               EDUCD == 30 ~ 9,
                               EDUCD == 40 ~ 10,
                               EDUCD == 50 ~ 11,
                               EDUCD == 60 ~ 12,
                               EDUCD == 70 ~ 13,
                               EDUCD == 80 ~ 14,
                               EDUCD == 90 ~ 15,
                               EDUCD == 100 ~ 16,
                               EDUCD < 120 ~ EDUCD - 93,
                               TRUE ~ NA_real_),
         AGEMARR = mean(ifelse(AGEMARR == 0, NA, AGEMARR), na.rm=TRUE), #county level mean age of marriage for married women teaching in 1940
         MAR_PENALTY = ifelse(GROUP == "Married Female", 1, 0)*rbinom(1,1,1-MW_WORK)*(AGE - AGEMARR),
         EXP = AGE - EDUCYEARS - 6 - MAR_PENALTY, 
         EXPALT = ifelse(MARST != 6 & SEX == 2, AGEMARR - EDUCYEARS - 6, EXP),
         EXPGRP = case_when(EXP <= 5 ~ "0-5 Years",
                            EXP <= 15 ~ "5-15 Years",
                            !is.na(EXP) ~ ">15 Years",
                            TRUE ~ NA_character_),
         EXPGRP_ALT1 = case_when(EXPALT <= 5 ~ "0-5 Years", #for married women, only count years before they were married
                            EXPALT <= 15 ~ "5-15 Years",
                            !is.na(EXPALT) ~ ">15 Years",
                            TRUE ~ NA_character_),
         EXPGRP_ALT2 = case_when((MARST != 6 & SEX == 2) | EXP <= 5 ~ "0-5 Years", #for married women, put them in lowest experience group
                                EXP <= 15 ~ "5-15 Years",
                                !is.na(EXP) ~ ">15 Years",
                                TRUE ~ NA_character_),
         HRS = ifelse(HRSWORK1 > 0 & WKSWORK1 > 0, HRSWORK1 * WKSWORK1, NA),
         HR_WAGE = ifelse(INCWAGE <= 5002 & !is.na(HRS), INCWAGE/HRS, NA),
         N_TEACHERS = n()) 

# histogram of educ / exp
ggplot(clean_teach, aes(x = EDUCYEARS)) + geom_histogram(binwidth = 1) + geom_vline(xintercept = 12) + geom_vline(xintercept = 16) + facet_wrap(~GROUP)
ggplot(clean_teach, aes(x = EXP)) + geom_histogram(binwidth = 1) + facet_wrap(~GROUP)

# graphing exp-earnings profile by group
#expearn1 <- clean_teach %>% filter(YEAR == 1940) %>% group_by(GROUP, EXPALT) %>%
  #summarize(wage = mean(HR_WAGE, na.rm=TRUE))
expearn2 <- clean_teach %>% filter(YEAR == 1940 & EDUCYEARS == 16 & EXP < 40) %>% group_by(GROUP, EXP) %>%
  summarize(wage = mean(HR_WAGE, na.rm=TRUE))

#ggplot(expearn1, aes(x = EXPALT, y = log(wage), color = GROUP)) + geom_point()
ggplot(expearn2, aes(x = EXP, y = log(wage), color = GROUP)) + geom_point()

# graphing age-earnings profile by group
ageearn <- clean_teach %>% filter(YEAR == 1940) %>% group_by(GROUP, AGE) %>%
  summarize(wage = mean(HR_WAGE, na.rm=TRUE))

ggplot(ageearn, aes(x = AGE, y = log(wage), color = GROUP)) + geom_point()

educ_sum <- clean_teach %>% filter(YEAR == 1940) %>%
  ungroup() %>% 
  group_by(GROUP) %>% 
  mutate(NGROUP = n()) %>% 
  ungroup() %>%
  group_by(GROUP, EDUCGRP) %>%
  summarise(PCT = n()/mean(NGROUP),
            wages = mean(HR_WAGE, na.rm=TRUE))

exp_sum <- clean_teach %>% filter(YEAR == 1940) %>%
  ungroup() %>% 
  group_by(GROUP) %>% 
  mutate(NGROUP = n()) %>% 
  ungroup() %>%
  group_by(GROUP, EXPGRP) %>%
  summarise(PCT = n()/mean(NGROUP),
            wages = mean(HR_WAGE, na.rm=TRUE))

all_cells <- clean_teach %>% filter(YEAR == 1940) %>%
  ungroup() %>% 
  group_by(GROUP) %>% 
  mutate(NGROUP = n()) %>% 
  ungroup() %>%
  group_by(GROUP, EDUCGRP, EXPGRP_ALT) %>%
  summarise(PCT = n()/mean(NGROUP),
            wages = mean(HR_WAGE, na.rm=TRUE))


clean_teach_group <- clean_teach %>%
  group_by(STATEICP, COUNTYICP, GROUP, EDUCGRP, EXPGRP, YEAR) %>%
  summarize(N_TEACHERS = mean(N_TEACHERS),
            L = n(),
            L_hours = sum(HRS, na.rm=TRUE),
            w = mean(HR_WAGE, na.rm=TRUE))

emp1930 <- raw_teach %>% filter(RACE == 1 & OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & YEAR == 1930) %>%
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(GROUP = case_when(SEX == 1 ~ "Male",
                           SEX == 2 & MARST == 6 ~ "Single Female",
                           TRUE ~ "Married Female")) %>%
  summarize(EMPRATIOMS = sum(ifelse(GROUP == "Married Female", 1, 0))/sum(ifelse(GROUP == "Single Female", 1, 0)),
            EMPRATIOWM = sum(ifelse(GROUP != "Male", 1, 0))/sum(ifelse(GROUP == "Male", 1, 0)))

clean_all <- inner_join(clean_teach_group, samp_aggs, by = c("STATEICP", "COUNTYICP", "YEAR")) %>% 
  filter(N_TEACHERS >= 100) %>%
  pivot_wider(names_from = c(YEAR, GROUP), values_from = c(SCHOOLPOP, L, L_hours, w)) %>%
  mutate(SCHOOLPOP1940 = max(SCHOOLPOP_1940_Male,`SCHOOLPOP_1940_Married Female`),
         wage_ratioms = log(`w_1940_Married Female`/`w_1940_Single Female`),
         emp_ratioms = log(`L_1940_Married Female`/`L_1940_Single Female`),
         hours_ratioms = log(`L_hours_1940_Married Female`/`L_hours_1940_Single Female`),
         totalhours_women = `L_hours_1940_Married Female` + `L_hours_1940_Single Female`,
         sharehours_marriedwomen = `L_hours_1940_Married Female`/totalhours_women,
         sharehours_singlewomen = `L_hours_1940_Single Female`/totalhours_women,
         wage_ratiowm = log((`w_1940_Single Female`*sharehours_singlewomen +`w_1940_Married Female`*sharehours_marriedwomen)/`w_1940_Male`),
         emp_ratiowm = log((`L_1940_Married Female`+`L_1940_Single Female`)/`L_1940_Male`),
         hours_ratiowm = log((`L_hours_1940_Married Female`+`L_hours_1940_Single Female`)/`L_hours_1940_Male`),
         STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated"),
         SOUTH = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98), 1, 0)) %>% # adding grouping for whether in group 1 (ky, nc) or group 2 (WV, TN, SC, OH, IN, IL, MO) southern state
  left_join(emp1930, by = c("STATEICP", "COUNTYICP")) %>%
  filter(SOUTH == 1)

ols_ms <- lm(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP + SCHOOLPOP1940)
iv_ms <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP + SCHOOLPOP1940 | STATEGROUP*EMPRATIOMS + EDUCGRP*EXPGRP + SCHOOLPOP1940)


ols_wm <- lm(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP + SCHOOLPOP1940)
iv_wm <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP + SCHOOLPOP1940 | STATEGROUP*EMPRATIOWM + EDUCGRP*EXPGRP + SCHOOLPOP1940)


summary(ols_ms)
summary(iv_ms)

summary(ols_wm)
summary(iv_wm)



### ALT DEF OF EXP
clean_teach_group <- clean_teach %>%
  group_by(STATEICP, COUNTYICP, GROUP, EDUCGRP, EXPGRP_ALT1, YEAR) %>%
  summarize(N_TEACHERS = mean(N_TEACHERS),
            L = n(),
            L_hours = sum(HRS, na.rm=TRUE),
            w = mean(HR_WAGE, na.rm=TRUE))

emp1930 <- raw_teach %>% filter(RACE == 1 & OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & YEAR == 1930) %>%
  group_by(STATEICP, COUNTYICP) %>% 
  mutate(GROUP = case_when(SEX == 1 ~ "Male",
                           SEX == 2 & MARST == 6 ~ "Single Female",
                           TRUE ~ "Married Female")) %>%
  summarize(EMPRATIOMS = sum(ifelse(GROUP == "Married Female", 1, 0))/sum(ifelse(GROUP == "Single Female", 1, 0)),
            EMPRATIOWM = sum(ifelse(GROUP != "Male", 1, 0))/sum(ifelse(GROUP == "Male", 1, 0)))


clean_all <- inner_join(clean_teach_group, samp_aggs, by = c("STATEICP", "COUNTYICP", "YEAR")) %>% 
  filter(N_TEACHERS >= 100) %>%
  pivot_wider(names_from = c(YEAR, GROUP), values_from = c(SCHOOLPOP, L, L_hours, w)) %>%
  mutate(SCHOOLPOP1940 = max(SCHOOLPOP_1940_Male,`SCHOOLPOP_1940_Married Female`),
         wage_ratioms = log(`w_1940_Married Female`/`w_1940_Single Female`),
         emp_ratioms = log(`L_1940_Married Female`/`L_1940_Single Female`),
         hours_ratioms = log(`L_hours_1940_Married Female`/`L_hours_1940_Single Female`),
         totalhours_women = `L_hours_1940_Married Female` + `L_hours_1940_Single Female`,
         sharehours_marriedwomen = `L_hours_1940_Married Female`/totalhours_women,
         sharehours_singlewomen = `L_hours_1940_Single Female`/totalhours_women,
         wage_ratiowm = log((`w_1940_Single Female`*sharehours_singlewomen +`w_1940_Married Female`*sharehours_marriedwomen)/`w_1940_Male`),
         emp_ratiowm = log((`L_1940_Married Female`+`L_1940_Single Female`)/`L_1940_Male`),
         hours_ratiowm = log((`L_hours_1940_Married Female`+`L_hours_1940_Single Female`)/`L_hours_1940_Male`),
         STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated"),
         SOUTH = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98), 1, 0)) %>% # adding grouping for whether in group 1 (ky, nc) or group 2 (WV, TN, SC, OH, IN, IL, MO) southern state
  left_join(emp1930, by = c("STATEICP", "COUNTYICP")) %>%
  filter(SOUTH == 1)

ols_ms <- lm(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)
iv_ms <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940 | STATEGROUP*EMPRATIOMS + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)


ols_wm <- lm(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)
iv_wm <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940 | STATEGROUP*EMPRATIOWM + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)


summary(ols_ms)
summary(iv_ms)

summary(ols_wm)
summary(iv_wm)





