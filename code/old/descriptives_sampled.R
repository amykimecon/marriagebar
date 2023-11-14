### INITIAL DESCRIPTIVES WITH 1% SAMPLE CENSUS DATA
### AUTHOR: AMY KIM
### LAST EDITED: MAR 24 2023
library(Hmisc)
library(haven)
library(tidyverse)
library(glue)
library(xlsx)

root = "/Users/amykim/Dropbox (Princeton)/marriagebar"
git = "Users/amykim/GitHub/marriagebar"
rawdata = glue("{root}/ipums_raw")
outdata = glue("{root}/clean_data")
outfigs = glue("{git}/figures")

# importing data
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))

# cpi deflator
cpi <- read.xlsx(glue("{root}/icpsr_southern_states_salaries_1910-1940/additional_data/cpi_historical.xlsx"), sheetIndex = 1) %>%
  filter(!is.na(cpi)) %>% mutate(cpi_target = ifelse(year == 2000, cpi, NA)) %>% 
  fill(cpi_target, .direction = "downup") %>% 
  mutate(cpi_adj = cpi_target/cpi,
         YEAR = as.numeric(year))

# grouping by year
year_samp <- allyears_raw_samp %>% 
  group_by(YEAR) %>%
  mutate(pop = sum(PERWT),
         schoolagepop = sum(ifelse(AGE >= 6 & AGE <= 18, PERWT, 0)),
         INCWAGE = ifelse(INCWAGE > 999998, NA, INCWAGE),
         demgroup = case_when(SEX == 1 ~ "Male",
                              SEX == 2 & MARST == 6 ~ "Single Female",
                              TRUE ~ "Married Female"),
         pop_mw = sum(ifelse(demgroup == "Married Female", PERWT, 0))) %>%
  filter(LABFORCE == 2) %>% 
  group_by(YEAR, demgroup) %>%
  summarise(pop = mean(pop),
            lfp_mw = sum(ifelse(demgroup == "Married Female", PERWT, 0))/mean(pop_mw),
            schoolagepop = mean(schoolagepop),
            numworkers = sum(PERWT),
            numteachers = sum(ifelse(OCC1950 == 93,PERWT,0)),
            wage = weighted.mean(INCWAGE, PERWT, na.rm=TRUE),
            teachwage = weighted.mean(INCWAGE, ifelse(OCC1950 == 93, PERWT, 0), na.rm=TRUE)) %>%
  group_by(YEAR) %>%
  mutate(pctteachers = numteachers/sum(numteachers),
         pctworkers = numworkers/sum(numworkers)) %>%
  left_join(cpi) %>%
  mutate(wage = wage*cpi_adj,
         teachwage = teachwage*cpi_adj)

### OVER ENTIRE TIME PERIOD -- AGGREGATE TRENDS IN GENDER COMPOSITION AND WAGES 
# graphing trends over time
ggplot(data = year_samp, aes(x = YEAR, y = pctteachers, fill = demgroup)) + geom_area() #share teachers male/single fem/married fem over time
ggplot(data = year_samp, aes(x = YEAR, y = numteachers, fill = demgroup)) + geom_area() #share teachers male/single fem/married fem over time

ggplot(data = year_samp, aes(x = YEAR, y = pctworkers, fill = demgroup)) + geom_area() #share workers male/single fem/married fem over time
  ggplot(data = year_samp, aes(x = YEAR, y = numworkers, fill = demgroup)) + geom_area() #share workers male/single fem/married fem over time

# graphing wages over time
ggplot(data = year_samp %>% filter(!is.na(teachwage)), aes(x = YEAR, y = teachwage, color = demgroup)) + geom_line()
ggplot(data = year_samp %>% filter(!is.na(wage)), aes(x = YEAR, y = wage, color = demgroup)) + geom_line()

### IN 1940 -- gender gap in teacher wages after controlling for age and education?
teachers_1940 <- allyears_raw_samp %>% filter(OCC1950 == 93 & YEAR == 1940 & AGE >= 18 & AGE <= 65) %>%
  mutate(EDUCGRP = case_when(EDUC <= 6 ~ "No College",
                             EDUC <= 8 ~ "Some College",
                             TRUE ~ "College"),
         married = ifelse(MARST!= 6, 1, 0))
summary(lm(INCWAGE ~ factor(married)*AGE*factor(EDUCGRP), weights = PERWT, data = teachers_1940 %>% filter(SEX == 2)))

# grouping by county (long on occupation)
grp_county_long_samp <- allyears_raw_samp %>% 
  filter(OCC1950 == 93 | OCC1950 == 350) %>%
  mutate(OCC = ifelse(OCC1950 == 93, "Teacher", "Secretary")) %>%
  filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% 
  summarise(num = n(),
            num_male = sum(ifelse(SEX == 1, PERWT, 0)),
            num_female_single = sum(ifelse(SEX == 2 & MARST == 6, PERWT, 0)),
            num_female_married = sum(ifelse(SEX == 2 & MARST != 6, PERWT, 0)),
            pct_female = sum(ifelse(SEX == 2, PERWT, 0))/sum(PERWT),
            pct_female_single = num_female_single/sum(ifelse(SEX == 2, PERWT, 0))) %>%
  mutate(STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated"))

# grouping by county (wide on occupation)
grp_county_wide_samp <- grp_county_long_samp %>% pivot_wider(id_cols = c(YEAR, STATEICP, COUNTYICP, STATEGROUP), names_from = OCC, values_from = c(num, num_male, num_female_single, num_female_married, pct_female, pct_female_single))

#### COUNTY-LEVEL DENSITY PLOTS #### 
## What were the overall trends of teacher demographics?

# density plot of: percentage of teachers/secretaries in a county who were female
female_plot_samp <- ggplot(grp_county_long_samp, aes(x = pct_female, group = factor(OCC), color = factor(OCC))) + geom_density() + facet_wrap(~YEAR)

# density plot of: percentage of female teachers/secretaries in a county who were single
singlefemale_plot_samp <- ggplot(grp_county_long_samp, aes(x = pct_female_single, group = factor(OCC), color = factor(OCC))) + geom_density() + facet_wrap(~YEAR)

# density plot of: percentage of female teachers in a county who were single (by State Group)
marbar_plot_group_samp <- ggplot(grp_county_wide_samp, aes(x = pct_female_single_Teacher, group = factor(STATEGROUP), color = factor(STATEGROUP))) + geom_density() + facet_wrap(~YEAR)


### RESIDUAL DENSITY PLOTS ###
## What were the trends of teacher demographics, differencing out secretary demographics?

# Generating residuals of regression of pct female single teachers on pct female single secretaries -- separately by year
allresids_samp <- list()
i = 0
for (year in seq(1900,1960,10)){
  i = i + 1
  datayear <- grp_county_wide_samp %>% filter(YEAR == year & !is.na(pct_female_single_Teacher) & !is.na(pct_female_single_Secretary))
  
  # NOTE: not including fixed effects, since already differencing out county-level trends with secretary trends
  sec_teach <- lm(data = datayear, pct_female_single_Teacher ~ pct_female_single_Secretary)
  datayear$resids <- sec_teach$residuals
  allresids_samp[[i]] <- datayear
}

## plotting residuals
allresids_df_samp <- bind_rows(allresids_samp)

# density plot of: residuals (interpretation -- positive residual means that a county had more female single teachers relative to female single secretaries, indicating more likely to have had a marriage bar)
teach_resid_plot_samp <- ggplot(data = allresids_df_samp, aes(x = resids)) + geom_density() + facet_wrap(~YEAR)

# density plot of: residuals by state group (interpretation -- if policy had bite, would expect to see group 1 [non marriage bar] counties had more negative residuals in 1940 than group 2 [marriage bar] counties, as compared to 1930)
teach_resid_plot_group_samp <- ggplot(data = allresids_df_samp %>% filter(STATEGROUP != 0), 
                                 aes(x = resids, group = factor(STATEGROUP), color = factor(STATEGROUP))) + 
  geom_density() + facet_wrap(~YEAR)

### DIFF IN DIFF ###
## Quantifying: how did teacher demographics change for group 1 relative to group 2 counties between 1930 and 1940, taking out any pre trends?

did_data_samp_teach <- allyears_raw_samp %>% filter(YEAR <= 1970) %>%
  filter(OCC1950 == 93 & SEX == 2 & MARST != 3 & MARST != 4 & MARST != 5) %>% # filtering out female teachers who are not widowed/divorced, only keeping southern/neighboring states for now
  mutate(married = ifelse(MARST == 1 | MARST == 2, 1, 0),
         treat = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, 1, 0),
         treatx1900 = ifelse(treat & YEAR == 1900, 1, 0),
         treatx1910 = ifelse(treat & YEAR == 1910, 1, 0),
         treatx1920 = ifelse(treat & YEAR == 1920, 1, 0),
         treatx1940 = ifelse(treat & YEAR == 1940, 1, 0),
         treatx1950 = ifelse(treat & YEAR == 1950, 1, 0),
         treatx1960 = ifelse(treat & YEAR == 1960, 1, 0),
         treatx1970 = ifelse(treat & YEAR == 1970, 1, 0))

# & STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98)
did_data_samp_sec <- allyears_raw_samp %>%
  filter(OCC1950 == 350 & SEX == 2 & MARST != 3 & MARST != 4 & MARST != 5) %>% # filtering out female teachers who are not widowed/divorced
  mutate(married = ifelse(MARST == 1 | MARST == 2, 1, 0),
         treat = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, 1, 0),
         treatx1900 = ifelse(treat & YEAR == 1900, 1, 0),
         treatx1910 = ifelse(treat & YEAR == 1910, 1, 0),
         treatx1920 = ifelse(treat & YEAR == 1920, 1, 0),
         treatx1940 = ifelse(treat & YEAR == 1940, 1, 0),
         treatx1950 = ifelse(treat & YEAR == 1950, 1, 0),
         treatx1960 = ifelse(treat & YEAR == 1960, 1, 0))

did_data_samp_grp <- grp_county_wide_samp %>%
  filter(YEAR <= 1960) %>%
  mutate(treat = ifelse(STATEGROUP == "Treated", 1, 0),
         treatx1900 = ifelse(treat & YEAR == 1900, 1, 0),
         treatx1910 = ifelse(treat & YEAR == 1910, 1, 0),
         treatx1920 = ifelse(treat & YEAR == 1920, 1, 0),
         treatx1940 = ifelse(treat & YEAR == 1940, 1, 0),
         treatx1950 = ifelse(treat & YEAR == 1950, 1, 0),
         treatx1960 = ifelse(treat & YEAR == 1960, 1, 0))

did_reg_samp <- lm(married ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950+ treatx1960 + treatx1970, data = did_data_samp_teach, weights = PERWT)
did_reg_teach_samp_grp_nums1 <- lm(num_male_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950 + treatx1960, data = did_data_samp_grp)
did_reg_teach_samp_grp_nums2 <- lm(num_female_single_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950 + treatx1960, data = did_data_samp_grp)
did_reg_teach_samp_grp_nums3 <- lm(num_female_married_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950 + treatx1960, data = did_data_samp_grp)



# did_reg_sec_samp <- lm(married ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950 + treatx1960, data = did_data_samp_sec, weights = PERWT)
# 
# did_reg_samp_grp <- lm(pct_female_single_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950+ treatx1960, data = did_data_samp_grp)
# did_reg_sec_samp_grp <- lm(pct_female_single_Secretary ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950 + treatx1960, data = did_data_samp_grp)

effects_samp <- data.frame(y_teach = c(did_reg_samp$coefficients[["treatx1900"]],
                                  did_reg_samp$coefficients[["treatx1910"]],
                                  did_reg_samp$coefficients[["treatx1920"]],
                                  0,
                                  did_reg_samp$coefficients[["treatx1940"]],
                                  did_reg_samp$coefficients[["treatx1950"]],
                                  did_reg_samp$coefficients[["treatx1960"]]),
                      year = seq(1900,1960,10),
                      var_teach = c(diag(vcov(did_reg_samp))[["treatx1900"]],
                                    diag(vcov(did_reg_samp))[["treatx1910"]],
                                    diag(vcov(did_reg_samp))[["treatx1920"]],
                                    0,
                                    diag(vcov(did_reg_samp))[["treatx1940"]],
                                    diag(vcov(did_reg_samp))[["treatx1950"]],
                                    diag(vcov(did_reg_samp))[["treatx1960"]]),
                      y_sec = c(did_reg_sec_samp$coefficients[["treatx1900"]],
                                did_reg_sec_samp$coefficients[["treatx1910"]],
                                did_reg_sec_samp$coefficients[["treatx1920"]],
                                0,
                                did_reg_sec_samp$coefficients[["treatx1940"]],
                                did_reg_sec_samp$coefficients[["treatx1950"]],
                                did_reg_sec_samp$coefficients[["treatx1960"]]),
                      var_sec = c(diag(vcov(did_reg_sec_samp))[["treatx1900"]],
                                  diag(vcov(did_reg_sec_samp))[["treatx1910"]],
                                  diag(vcov(did_reg_sec_samp))[["treatx1920"]],
                                  0,
                                  diag(vcov(did_reg_sec_samp))[["treatx1940"]],
                                  diag(vcov(did_reg_sec_samp))[["treatx1950"]],
                                  diag(vcov(did_reg_sec_samp))[["treatx1960"]]))

effects_samp_grp <- data.frame(y_teach = c(did_reg_samp_grp$coefficients[["treatx1900"]],
                                       did_reg_samp_grp$coefficients[["treatx1910"]],
                                       did_reg_samp_grp$coefficients[["treatx1920"]],
                                       0,
                                       did_reg_samp_grp$coefficients[["treatx1940"]],
                                       did_reg_samp_grp$coefficients[["treatx1950"]],
                                       did_reg_samp_grp$coefficients[["treatx1960"]]),
                           year = seq(1900,1960,10),
                           var_teach = c(diag(vcov(did_reg_samp_grp))[["treatx1900"]],
                                         diag(vcov(did_reg_samp_grp))[["treatx1910"]],
                                         diag(vcov(did_reg_samp_grp))[["treatx1920"]],
                                         0,
                                         diag(vcov(did_reg_samp_grp))[["treatx1940"]],
                                         diag(vcov(did_reg_samp_grp))[["treatx1950"]],
                                         diag(vcov(did_reg_samp_grp))[["treatx1960"]]),
                           y_sec = c(did_reg_sec_samp_grp$coefficients[["treatx1900"]],
                                     did_reg_sec_samp_grp$coefficients[["treatx1910"]],
                                     did_reg_sec_samp_grp$coefficients[["treatx1920"]],
                                     0,
                                     did_reg_sec_samp_grp$coefficients[["treatx1940"]],
                                     did_reg_sec_samp_grp$coefficients[["treatx1950"]],
                                     did_reg_sec_samp_grp$coefficients[["treatx1960"]]),
                           var_sec = c(diag(vcov(did_reg_sec_samp_grp))[["treatx1900"]],
                                       diag(vcov(did_reg_sec_samp_grp))[["treatx1910"]],
                                       diag(vcov(did_reg_sec_samp_grp))[["treatx1920"]],
                                       0,
                                       diag(vcov(did_reg_sec_samp_grp))[["treatx1940"]],
                                       diag(vcov(did_reg_sec_samp_grp))[["treatx1950"]],
                                       diag(vcov(did_reg_sec_samp_grp))[["treatx1960"]]))

sec_color = "red"
teach_color = "blue"
did_graph_samp <- ggplot(effects_samp, aes(x = year, y = y_teach)) + 
  geom_errorbar(inherit.aes = FALSE, aes(x =  year, y = y_sec, min = y_sec - 1.96*sqrt(var_sec), max = y_sec + 1.96*sqrt(var_sec)), width = 1, color = sec_color) +
  geom_point(aes(x =  year, y = y_sec), color = sec_color) + geom_line(aes(x =  year, y = y_sec), color = sec_color) + 
  ylab("Treated x Year on Share of Female in Job Single") + xlab("Year") +
  geom_errorbar(aes(min = y_teach - 1.96*sqrt(var_teach), max = y_teach + 1.96*sqrt(var_teach)), width = 1, color = teach_color) +
  geom_point(color = teach_color) + geom_line(color = teach_color) 
  

did_graph_samp_grp <- ggplot(effects_samp_grp, aes(x = year, y = y_teach)) + 
  geom_errorbar(inherit.aes = FALSE, aes(x =  year, y = y_sec, min = y_sec - 1.96*sqrt(var_sec), max = y_sec + 1.96*sqrt(var_sec)), width = 1, color = sec_color) +
  geom_point(aes(x =  year, y = y_sec), color = sec_color) + geom_line(aes(x =  year, y = y_sec), color = sec_color) + 
  ylab("Treated x Year on Share of Female in Job Single") + xlab("Year") +
  geom_errorbar(aes(min = y_teach - 1.96*sqrt(var_teach), max = y_teach + 1.96*sqrt(var_teach)), width = 1, color = teach_color) +
  geom_point(color = teach_color) + geom_line(color = teach_color) 

ggsave(glue("{outfigs}/did_graph_samp.png"), did_graph_samp)

# ### DIFF IN DIFF -- POOLED ###
# did_data_pooled <- grp_county_wide_samp %>%
#   mutate(treat = ifelse(STATEGROUP == "Treated", 1, 0),
#          treatxpre = ifelse(treat & YEAR < 1940, 1, 0))
# 
# did_reg_pooled <- lm(pct_female_single_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950+ treatx1960, data = did_data_samp)
# did_reg_sec_samp <- lm(pct_female_single_Secretary ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940 + treatx1950 + treatx1960, data = did_data_samp)
# 
# effects_samp <- data.frame(y_teach = c(did_reg_samp$coefficients[["treatx1900"]],
#                                        did_reg_samp$coefficients[["treatx1910"]],
#                                        did_reg_samp$coefficients[["treatx1920"]],
#                                        0,
#                                        did_reg_samp$coefficients[["treatx1940"]],
#                                        did_reg_samp$coefficients[["treatx1950"]],
#                                        did_reg_samp$coefficients[["treatx1960"]]),
#                            year = seq(1900,1960,10),
#                            var_teach = c(diag(vcov(did_reg_samp))[["treatx1900"]],
#                                          diag(vcov(did_reg_samp))[["treatx1910"]],
#                                          diag(vcov(did_reg_samp))[["treatx1920"]],
#                                          0,
#                                          diag(vcov(did_reg_samp))[["treatx1940"]],
#                                          diag(vcov(did_reg_samp))[["treatx1950"]],
#                                          diag(vcov(did_reg_samp))[["treatx1960"]]),
#                            y_sec = c(did_reg_sec_samp$coefficients[["treatx1900"]],
#                                      did_reg_sec_samp$coefficients[["treatx1910"]],
#                                      did_reg_sec_samp$coefficients[["treatx1920"]],
#                                      0,
#                                      did_reg_sec_samp$coefficients[["treatx1940"]],
#                                      did_reg_sec_samp$coefficients[["treatx1950"]],
#                                      did_reg_sec_samp$coefficients[["treatx1960"]]),
#                            var_sec = c(diag(vcov(did_reg_sec_samp))[["treatx1900"]],
#                                        diag(vcov(did_reg_sec_samp))[["treatx1910"]],
#                                        diag(vcov(did_reg_sec_samp))[["treatx1920"]],
#                                        0,
#                                        diag(vcov(did_reg_sec_samp))[["treatx1940"]],
#                                        diag(vcov(did_reg_sec_samp))[["treatx1950"]],
#                                        diag(vcov(did_reg_sec_samp))[["treatx1960"]]))
# 
# did_graph_samp <- ggplot(effects_samp, aes(x = year, y = y_teach)) + 
#   geom_errorbar(aes(min = y_teach - 1.96*sqrt(var_teach), max = y_teach + 1.96*sqrt(var_teach)), width = 0.1, color = teach_color) +
#   geom_point(color = teach_color) + geom_line(color = teach_color) + 
#   geom_errorbar(inherit.aes = FALSE, aes(x =  year, y = y_sec, min = y_sec - 1.96*sqrt(var_sec), max = y_sec + 1.96*sqrt(var_sec)), width = 0.1, color = sec_color) +
#   geom_point(aes(x =  year, y = y_sec), color = sec_color) + geom_line(aes(x =  year, y = y_sec), color = sec_color) + 
#   ylab("Treated x Year on Share of Female in Job Single") + xlab("Year")


### checking for changes in employment of married women in general
emp_mar_stategroup <- allyears_raw_samp %>% mutate(EMP = ifelse(OCC1950 < 980, 1, 0),
                                        STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated")) %>%
  group_by(YEAR, STATEGROUP) %>% summarize(pct_workers_women = sum(ifelse(EMP == 1 & SEX == 2, PERWT, 0))/sum(ifelse(EMP == 1, PERWT, 0)),
                               pct_marwomen_workers = sum(ifelse(EMP == 1 & SEX == 2 & (MARST == 1 | MARST == 2), PERWT, 0))/sum(ifelse(SEX == 2 & (MARST == 1 | MARST == 2), PERWT, 0)),
                               pct_marwomen_teachers = sum(ifelse(OCC1950 == 93 & SEX == 2 & (MARST == 1 | MARST == 2), PERWT, 0))/sum(ifelse(SEX == 2 & (MARST == 1 | MARST == 1), PERWT, 0)),
                               pct_empmarwomen_teachers = sum(ifelse(OCC1950 == 93 & SEX == 2 & (MARST == 1 | MARST == 2), PERWT, 0))/sum(ifelse(SEX == 2 & EMP == 1 & (MARST == 1 | MARST == 1), PERWT, 0))) 

emp_mar_diffs <- emp_mar_stategroup %>% pivot_wider(id_cols = YEAR, names_from = STATEGROUP, values_from = pct_marwomen_workers) %>%
  mutate(diff = Treated - `Not Treated`)

ggplot(emp_mar_stategroup, aes(x = YEAR, y = pct_marwomen_workers, color = factor(STATEGROUP), group = factor(STATEGROUP))) + geom_point() + geom_line()
ggplot(emp_mar_diffs, aes(x = YEAR, y = diff)) + geom_point() + geom_line()

### looking at teachers as a share of married women, and as a share of married working women
ggplot(emp_mar_stategroup, aes(x = YEAR, y = pct_marwomen_teachers, color = factor(STATEGROUP), group = factor(STATEGROUP))) + geom_point() + geom_line()
ggplot(emp_mar_stategroup, aes(x = YEAR, y = pct_empmarwomen_teachers, color = factor(STATEGROUP), group = factor(STATEGROUP))) + geom_point() + geom_line()




##### QUESTION: ARE MARRIED WOMEN MAKING LESS AS TEACHERS? 
# regression
yearfilt = 1960
regwage_data <- allyears_raw_samp %>% filter(YEAR == yearfilt) %>% group_by(STATEICP, COUNTYICP) %>% 
  mutate(MARRIED = ifelse(MARST == 1 | MARST == 2, 1, 0),
         EMP = ifelse(OCC1950 < 980, 1, 0),
         pct_female_teachers = sum(ifelse(SEX == 2, PERWT, 0))/sum(PERWT),
         pct_marwomen_working = sum(ifelse(EMP == 1 & SEX == 2 & MARRIED == 1, PERWT, 0))/sum(ifelse(SEX == 2 & MARRIED == 1, PERWT, 0))) 

reg_wage_teach <- lm(log(INCWAGE) ~ factor(MARRIED)*factor(SEX) + AGE + pct_marwomen_working + factor(STATEICP) + pct_female_teachers + factor(RACE) + factor(EDUC) + factor(WKSWORK2) + factor(HRSWORK2), 
                    weights = PERWT, data = regwage_data %>% filter(OCC1950 == 93 & INCWAGE > 0) )

reg_wage_all <- lm(log(INCWAGE) ~ factor(MARRIED)*factor(SEX) + AGE + pct_marwomen_working + factor(STATEICP) + pct_female_teachers + factor(RACE) + factor(EDUC) + factor(WKSWORK2) + factor(HRSWORK2), 
                        weights = PERWT, data = regwage_data %>% filter(INCWAGE > 0) )


# means
samp_1940 <- filter(allyears_raw_samp, YEAR == 1940)

incwage_1940 <- samp_1940 %>% mutate(group = case_when((MARST == 1 | MARST == 2) & SEX == 2 ~ "Married Woman",
                                                       SEX == 2 ~ "Non-Married Woman",
                                                       SEX == 1 ~ "Man"),
                                     STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated")) %>%
  filter(OCC1950 == 93, INCWAGE > 0) 

incwagegrp_1940 <- incwage_1940 %>% group_by(group, STATEGROUP) %>% summarize(INCWAGE_ERR = sqrt(wtd.var(INCWAGE, na.rm=TRUE)/n()),
                                                          INCWAGE = weighted.mean(INCWAGE))
  

samp_1960 <- filter(allyears_raw_samp, YEAR == 1960)

incwage_1960 <- samp_1960 %>% mutate(group = case_when((MARST == 1 | MARST == 2) & SEX == 2 ~ "Married Woman",
                                                       SEX == 2 ~ "Non-Married Woman",
                                                       SEX == 1 ~ "Man"),
                                     STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated")) %>%
  filter(OCC1950 == 93, INCWAGE > 0) 

incwagegrp_1960 <- incwage_1960 %>% group_by(group, STATEGROUP) %>% summarize(INCWAGE_ERR = sqrt(wtd.var(INCWAGE, na.rm=TRUE)/n()),
                                                                              INCWAGE = weighted.mean(INCWAGE))



##### QUESTION: DOES AVERAGE AGE AT MARRIAGE CHANGE FOR TEACHERS RELATIVE TO POP AFTER MBs?
did_ageatmarriage <- allyears_raw_samp %>%
  filter(SEX == 2 & (MARST == 1 | MARST == 2) & (YEAR == 1930 | YEAR == 1940) & AGEMARR > 0) %>% #only keeping married females
  mutate(teach = ifelse(OCC1950 == 93, 1, 0),
         treat = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, 1, 0),
         after = ifelse(YEAR == 1940, 1, 0),
         treatxbefore = ifelse(treat & YEAR == 1930, 1, 0),
         treatxafter = ifelse(treat & YEAR == 1940, 1, 0))

summary(lm(AGEMARR ~ teach*treat*after + AGE, weights = PERWT, data = did_ageatmarriage))


##### QUESTION: HOW DOES THE TOTAL NUMBER OF TEACHERS (rel to school age pop) CHANGE OVER TIME?
teacher_ratio <- allyears_raw_samp %>% group_by(COUNTYICP, STATEICP, YEAR) %>%
  summarize(pop = sum(PERWT),
            numchildren = sum(ifelse(AGE <= 18 & AGE >= 6, PERWT, 0)),
         ratioteachers = sum(ifelse(OCC1950 == 93, PERWT, 0))/numchildren,
         ratiomaleteachers = sum(ifelse(OCC1950 == 93 & SEX == 1, PERWT, 0))/numchildren,
         ratiosinglefemaleteachers = sum(ifelse(OCC1950 == 93 & SEX == 2 & MARST == 6, PERWT, 0))/numchildren,
         ratiomarriedfemaleteachers = sum(ifelse(OCC1950 == 93 & SEX == 2 & (MARST == 1 | MARST == 2), PERWT, 0))/numchildren) 

teacher_ratio_means <- allyears_raw_samp %>%group_by(YEAR) %>% 
  summarize(pop = sum(PERWT),
            numchildren = sum(ifelse(AGE <= 18 & AGE >= 6, PERWT, 0)),
            ratioteachers = sum(ifelse(OCC1950 == 93, PERWT, 0))/numchildren,
            ratiomaleteachers = sum(ifelse(OCC1950 == 93 & SEX == 1, PERWT, 0))/numchildren,
            ratiosinglefemaleteachers = sum(ifelse(OCC1950 == 93 & SEX == 2 & MARST == 6, PERWT, 0))/numchildren,
            ratiomarriedfemaleteachers = sum(ifelse(OCC1950 == 93 & SEX == 2 & (MARST == 1 | MARST == 2), PERWT, 0))/numchildren)  %>%
  pivot_longer(starts_with("ratio"), names_to = "cat", names_prefix = "ratio", values_to = "ratio")
  
ggplot(data =teacher_ratio_means, aes(x = YEAR, y = ratio, group= factor(cat), color = factor(cat))) + 
  geom_line()




