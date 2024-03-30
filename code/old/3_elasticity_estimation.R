### MAIN ESTIMATION OF SUBSTITUTION ELASTICITIES
### AUTHOR: AMY KIM
### LAST EDITED: MAY 2023

######################################
##### CLEANING DATA FOR THIS PART ####
######################################
clean_teach <- filtered_bind %>% filter(YEAR == 1940 & OCC == "Teacher" & EDUCD != 999 & INCWAGE > 0) %>%
  group_by(STATEICP) %>%
  mutate(AGEMARR_STATE = mean(ifelse(AGEMARR == 0, NA, AGEMARR), na.rm=TRUE)) %>% #state level mean age of marriage for married women teaching in 1940
  group_by(STATEICP, COUNTYICP) %>% 
  inner_join(select(countysumm %>% ungroup() %>% filter(YEAR == 1930) %>% rename(LFP_MW_1930 = LFP_MW, num_Teacher_1930 = num_Teacher, 
                                                                                 EMPRATIOMS_1930 = nummf_Teacher/numsf_Teacher, 
                                                                                 EMPRATIOWM_1930 = numfem_Teacher/nummale_Teacher), 
                    c(STATEICP, COUNTYICP, LFP_MW_1930, main_samp, STATEGROUP, TREAT, num_Teacher_1930, EMPRATIOMS_1930, EMPRATIOWM_1930))) %>% #joining with county-level 1930 LFP of married women
  filter(main_samp == 1 & !is.na(LFP_MW_1930)) %>%
  mutate(COUNTYSTATE = paste0(STATEICP, "_", COUNTYICP),
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
         AGEMARR_COUNTY = mean(ifelse(AGEMARR == 0, NA, AGEMARR), na.rm=TRUE), #county level mean age of marriage for married women teaching in 1940
         AGEMARR = ifelse(!is.na(AGEMARR_COUNTY), AGEMARR_COUNTY, AGEMARR_STATE),
         MAR_PENALTY = ifelse(demgroup == "MW", 1, 0)*rbinom(1,1,1-LFP_MW_1930)*(AGE - AGEMARR),
         EXP_NONADJ_RAW = AGE - EDUCYEARS - 6,
         EXP_NONADJ = ifelse(EXP_NONADJ_RAW < 0, 0, EXP_NONADJ_RAW),
         EXP_RAW = round(AGE - EDUCYEARS - 6 - MAR_PENALTY),
         EXP = ifelse(EXP_RAW < 0, 0, EXP_RAW),
         EXPGRP_ADJ = case_when(EXP <= 5 ~ "0-5 Years", 
                            EXP <= 10 ~ "5-10 Years", 
                            !is.na(EXP) ~ ">15 Years",
                            TRUE ~ NA_character_),
         EXPGRP_NONADJ = case_when(EXP_NONADJ <= 5 ~ "0-5 Years", 
                                   EXP <= 10 ~ "5-10 Years", 
                                   !is.na(EXP) ~ ">10 Years",
                                   TRUE ~ NA_character_),
         HRS = ifelse(HRSWORK1 > 0 & WKSWORK1 > 0, HRSWORK1 * WKSWORK1, NA),
         INCWAGE = ifelse(INCWAGE <= 5002, INCWAGE, NA),
         HR_WAGE = ifelse(INCWAGE <= 5002 & !is.na(HRS), INCWAGE/HRS, NA),
         N = n())


####################################
##### EVAL SKILL CELL GROUPINGS ####
####################################
# # histogram of education
ggplot(clean_teach %>%
         mutate(demgroup = case_when(demgroup == "M" ~ "Men", 
                                     demgroup == "SW" ~ "Single Women", 
                                     demgroup == "MW" ~ "Married Women")), 
       aes(x = EDUCYEARS, after_stat(density))) + geom_histogram(binwidth = 1) + geom_vline(xintercept = 16, linetype = "dashed") + facet_wrap(~factor(demgroup, levels = c("Men", "Single Women", "Married Women"))) +
  labs(x = "Years of Education", y = "Density") + theme_minimal()
ggsave(filename = glue("{outfigs}/figa7_educhist.png"), width = 8, height = 5)

# graphing educ-earnings profile
educearn <- clean_teach %>% group_by(EDUCYEARS) %>% summarize(wage = mean(HR_WAGE, na.rm=TRUE))
ggplot(educearn, aes(x = EDUCYEARS, y = log(wage))) + geom_line() + geom_vline(xintercept = 16, linetype = "dashed") +
  scale_color_manual(values=c(treat_col, control_col)) +
  labs(x = "Years of Education", y = "Log(Hourly Wage)") + theme_minimal() 
ggsave(filename = glue("{outfigs}/figa5_educearn.png"), width = 8, height = 5)

# graphing exp-earnings profile by group
expearn <- clean_teach %>% group_by(EDUCGRP, EXP_NONADJ) %>% summarize(wage = mean(HR_WAGE, na.rm=TRUE))
ggplot(expearn %>% filter(EXP_NONADJ <= 30), aes(x = EXP_NONADJ, y = log(wage), color = EDUCGRP)) + geom_line()  +
  scale_color_manual(values=c(treat_col, control_col)) +
  labs(x = "Unadjusted Years of Experience", y = "Log(Wage/Hours)", color = "") + theme_minimal() + theme(legend.position = "bottom", text = element_text(size=20))
ggsave(filename = glue("{outfigs}/figa6_expearn_unadj.png"), width = 8, height = 5)

expearn2 <- clean_teach %>% group_by(EDUCGRP, EXP) %>% summarize(wage = mean(HR_WAGE, na.rm=TRUE))
ggplot(expearn2 %>% filter(EXP <= 30), aes(x = EXP, y = log(wage), color = EDUCGRP)) + geom_line() +
  scale_color_manual(values=c(treat_col, control_col)) +
  labs(x = "Adjusted Years of Experience", y = "Log(Wage/Hours)", color = "") + theme_minimal() + theme(legend.position = "bottom", text = element_text(size=20))
ggsave(filename = glue("{outfigs}/figa6_expearn.png"), width = 8, height = 5)

# 
# 
# # ggplot(clean_teach, aes(x = EXP, after_stat(density), color = demgroup)) + geom_freqpoly(binwidth = 1) + xlim(0,40)
# ggplot(clean_teach, aes(x = EXP, after_stat(density), color = EDUCGRP)) + geom_freqpoly(binwidth = 1) + xlim(0,40) + facet_wrap(~demgroup)
# ggplot(clean_teach, aes(x = EXP_NONADJ, after_stat(density), color = EDUCGRP)) + geom_freqpoly(binwidth = 1) + xlim(0,40) + facet_wrap(~demgroup)

#################################
##### GROUPING BY SKILL CELL ####
#################################clean_teach_group_adj <- clean_teach %>% 
  group_by(STATEICP, COUNTYICP, COUNTYSTATE, TREAT, STATEGROUP, main_samp, demgroup, EDUCGRP, EXPGRP_ADJ) %>%
  summarize(N = mean(N),
            EMPRATIOMS_1930 = mean(EMPRATIOMS_1930),
            EMPRATIOWM_1930 = mean(EMPRATIOWM_1930),
            L = n(),
            w = mean(INCWAGE, na.rm=TRUE),
            L_hours = sum(HRS, na.rm=TRUE),
            w_hours = mean(HR_WAGE, na.rm=TRUE)) %>%
  filter(!is.na(EDUCGRP) & !is.na(EXPGRP_ADJ) & !is.na(w) & !is.na(L))

clean_teach_gender <- clean_teach %>% 
  mutate(gender = ifelse(demgroup == "M", "M", "W")) %>%
  group_by(STATEICP, COUNTYICP, COUNTYSTATE, TREAT, STATEGROUP, main_samp, gender) %>%
  summarize(N = mean(N),
            EMPRATIOMS_1930 = mean(EMPRATIOMS_1930),
            EMPRATIOWM_1930 = mean(EMPRATIOWM_1930),
            L = n(),
            w = mean(INCWAGE, na.rm=TRUE),
            L_hours = sum(HRS, na.rm=TRUE),
            w_hours = mean(HR_WAGE, na.rm=TRUE)) %>%
  filter(!is.na(w) & !is.na(L))


clean_all_adj <- inner_join(clean_teach_group_adj, countysumm %>% filter(YEAR == 1940) %>% select(c(STATEICP, COUNTYICP, WHITESCHOOLPOP, POP, URBAN))) %>%
  pivot_wider(names_from = c(demgroup), values_from = c(L, L_hours, w, w_hours)) %>%
  mutate(wage_ratioms = log(w_MW/w_SW),
         emp_ratioms = log(L_MW/L_SW),
         wagehr_ratioms = log(w_hours_MW/w_hours_SW),
         hours_ratioms = log(L_hours_MW/L_hours_SW),
         totalhours_women = ifelse(is.na(L_hours_MW), 0, L_hours_MW) + L_hours_SW,
         sharehours_marriedwomen = L_hours_MW/totalhours_women,
         sharehours_singlewomen = L_hours_SW/totalhours_women,
         totalemp_women = ifelse(is.na(L_hours_MW), 0, L_hours_MW) + L_SW,
         shareemp_marriedwomen = L_MW/totalemp_women,
         shareemp_singlewomen = L_SW/totalemp_women,
         wagehr_ratiowm = log((w_hours_SW*sharehours_singlewomen +w_hours_MW*sharehours_marriedwomen)/w_hours_M),
         wage_ratiowm = log((w_SW*shareemp_singlewomen +w_MW*shareemp_marriedwomen)/w_M),
         emp_ratiowm = log((L_MW+L_SW)/L_M),
         hours_ratiowm = log(totalhours_women/L_hours_M),
         TREAT_wagehr_ratioms = TREAT*wagehr_ratioms) %>%
  filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)")


clean_all_gender <- inner_join(clean_teach_gender, countysumm %>% filter(YEAR == 1940) %>% select(c(STATEICP, COUNTYICP))) %>%
  pivot_wider(names_from = c(gender), values_from = c(L, L_hours, w, w_hours)) %>%
  mutate(wage_ratiowm = log(w_W/w_M),
         emp_ratiowm = log(L_W/L_M),
         wagehr_ratiowm = log(w_hours_W/w_hours_M),
         hours_ratiowm = log(L_hours_W/L_hours_M),
         TREAT_wagehr_ratiowm = TREAT*wagehr_ratiowm) %>%
  filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)")

####################################
##### ELASTICITY TABLES -- MAIN ####
####################################
ms_emp <- lm(data = clean_all_adj, wage_ratioms ~ emp_ratioms + EDUCGRP*EXPGRP_ADJ - 1 )
# ms_emp_bytreat <- lm(data = clean_all_adj, wage_ratioms ~ emp_ratioms*factor(TREAT) + EDUCGRP*EXPGRP_ADJ - 1 )
ms_hours <- lm(data = clean_all_adj, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1 )
# ms_hours_bytreat <- lm(data = clean_all_adj, wagehr_ratioms ~ hours_ratioms*factor(TREAT) + EDUCGRP*EXPGRP_ADJ - 1 )
ms_hours_notreat <- lm(data = clean_all_adj %>% filter(TREAT == 0), wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1 )
ms_hours_treat <- lm(data = clean_all_adj %>% filter(TREAT == 1), wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1 )
summary(ms_hours_notreat)
summary(ms_hours_treat)

wm_emp <- lm(data = clean_all_gender, wage_ratiowm ~ emp_ratiowm )
# wm_emp_bytreat <- lm(data = clean_all_gender, wage_ratiowm ~ emp_ratiowm*factor(TREAT) - 1 )
wm_hours <- lm(data = clean_all_gender, wagehr_ratiowm ~ hours_ratiowm )
#wm_hours_bytreat <- lm(data = clean_all_gender, wagehr_ratiowm ~ hours_ratiowm*factor(TREAT) - 1 )
wm_hours_notreat <- lm(data = clean_all_gender %>% filter(TREAT == 0), wagehr_ratiowm ~ hours_ratiowm  )
wm_hours_treat <- lm(data = clean_all_gender %>% filter(TREAT == 1), wagehr_ratiowm ~ hours_ratiowm )
summary(wm_hours_notreat)
summary(wm_hours_treat)

stargazer(ms_emp, ms_hours, ms_hours_notreat, ms_hours_treat)
stargazer(wm_emp, wm_hours, wm_hours_notreat, wm_hours_treat)


# dep var means
print(nrow(filter(clean_all_adj, !is.na(wage_ratioms))))
print(mean(filter(clean_all_adj, !is.na(wage_ratioms))$wage_ratioms))

print(nrow(filter(clean_all_adj, !is.na(hours_ratioms) & !is.na(wagehr_ratioms))))
print(mean(filter(clean_all_adj, !is.na(hours_ratioms) & !is.na(wagehr_ratioms))$wagehr_ratioms))

print(nrow(filter(clean_all_adj, TREAT == 0 & !is.na(hours_ratioms) & !is.na(wagehr_ratioms))))
print(mean(filter(clean_all_adj, TREAT == 0 & !is.na(hours_ratioms) & !is.na(wagehr_ratioms))$wagehr_ratioms))

print(nrow(filter(clean_all_adj, TREAT == 1 & !is.na(hours_ratioms) & !is.na(wagehr_ratioms))))
print(mean(filter(clean_all_adj, TREAT == 1 & !is.na(hours_ratioms) & !is.na(wagehr_ratioms))$wagehr_ratioms))

print(nrow(filter(clean_all_gender, !is.na(wage_ratiowm))))
print(mean(filter(clean_all_gender, !is.na(wage_ratiowm))$wage_ratiowm))
print(nrow(filter(clean_all_gender, !is.na(hours_ratiowm) & !is.na(wagehr_ratiowm))))
print(mean(filter(clean_all_gender, !is.na(hours_ratiowm) & !is.na(wagehr_ratiowm))$wagehr_ratiowm))

####################################
##### ADDITIONAL STUFF -- NOT IN PAPER ####
####################################
clean_teach_group_nonadj <- clean_teach %>% 
  group_by(STATEICP, COUNTYICP, COUNTYSTATE, TREAT, STATEGROUP, main_samp, demgroup, EDUCGRP, EXPGRP_NONADJ) %>%
  summarize(N = mean(N),
            EMPRATIOMS_1930 = mean(EMPRATIOMS_1930),
            EMPRATIOWM_1930 = mean(EMPRATIOWM_1930),
            L = n(),
            w = mean(INCWAGE, na.rm=TRUE),
            L_hours = sum(HRS, na.rm=TRUE),
            w_hours = mean(HR_WAGE, na.rm=TRUE)) %>%
  filter(!is.na(EDUCGRP) & !is.na(EXPGRP_NONADJ) & !is.na(w) & !is.na(L))

clean_teach_gender_educ <- clean_teach %>% 
  mutate(gender = ifelse(demgroup == "M", "M", "W")) %>%
  group_by(STATEICP, COUNTYICP, COUNTYSTATE, TREAT, STATEGROUP, main_samp, gender, EDUCGRP) %>%
  summarize(N = mean(N),
            EMPRATIOMS_1930 = mean(EMPRATIOMS_1930),
            EMPRATIOWM_1930 = mean(EMPRATIOWM_1930),
            L = n(),
            w = mean(INCWAGE, na.rm=TRUE),
            L_hours = sum(HRS, na.rm=TRUE),
            w_hours = mean(HR_WAGE, na.rm=TRUE)) %>%
  filter(!is.na(w) & !is.na(L) & !is.na(EDUCGRP))

clean_all_nonadj <- inner_join(clean_teach_group_nonadj, countysumm %>% filter(YEAR == 1940) %>% select(c(STATEICP, COUNTYICP, WHITESCHOOLPOP, POP, URBAN))) %>%
  pivot_wider(names_from = c(demgroup), values_from = c(L, L_hours, w, w_hours)) %>%
  mutate(wage_ratioms = log(w_MW/w_SW),
         emp_ratioms = log(L_MW/L_SW),
         wagehr_ratioms = log(w_hours_MW/w_hours_SW),
         hours_ratioms = log(L_hours_MW/L_hours_SW),
         totalhours_women = ifelse(is.na(L_hours_MW), 0, L_hours_MW) + L_hours_SW,
         sharehours_marriedwomen = L_hours_MW/totalhours_women,
         sharehours_singlewomen = L_hours_SW/totalhours_women,
         totalemp_women = ifelse(is.na(L_hours_MW), 0, L_hours_MW) + L_SW,
         shareemp_marriedwomen = L_MW/totalemp_women,
         shareemp_singlewomen = L_SW/totalemp_women,
         wagehr_ratiowm = log((w_hours_SW*sharehours_singlewomen +w_hours_MW*sharehours_marriedwomen)/w_hours_M),
         wage_ratiowm = log((w_SW*shareemp_singlewomen +w_MW*shareemp_marriedwomen)/w_M),
         emp_ratiowm = log((L_MW+L_SW)/L_M),
         hours_ratiowm = log(totalhours_women/L_hours_M),
         TREAT_EMPRAT = TREAT*EMPRATIOMS_1930) %>%
  filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)")

clean_all_gender_educ <- inner_join(clean_teach_gender_educ, countysumm %>% filter(YEAR == 1940) %>% select(c(STATEICP, COUNTYICP))) %>%
  pivot_wider(names_from = c(gender), values_from = c(L, L_hours, w, w_hours)) %>%
  mutate(wage_ratiowm = log(w_W/w_M),
         emp_ratiowm = log(L_W/L_M),
         wagehr_ratiowm = log(w_hours_W/w_hours_M),
         hours_ratiowm = log(L_hours_W/L_hours_M)) %>%
  filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)")

# gender under education nest
wm_emp_educ <- lm(data = clean_all_gender_educ, wage_ratiowm ~ emp_ratiowm + EDUCGRP- 1)
wm_emp_bytreat_educ <- lm(data = clean_all_gender_educ, wage_ratiowm ~ emp_ratiowm*factor(TREAT) + EDUCGRP- 1 )
wm_hours_educ <- lm(data = clean_all_gender_educ, wagehr_ratiowm ~ hours_ratiowm + EDUCGRP- 1)
wm_hours_bytreat_educ <- lm(data = clean_all_gender_educ, wagehr_ratiowm ~ hours_ratiowm*factor(TREAT)+ EDUCGRP - 1 )

# unadjusted experience
ms_emp_nonadj <- lm(data = clean_all_nonadj, wage_ratioms ~ emp_ratioms + EDUCGRP*EXPGRP_NONADJ - 1 )
ms_emp_bytreat_nonadj <- lm(data = clean_all_nonadj, wage_ratioms ~ emp_ratioms*factor(TREAT) + EDUCGRP*EXPGRP_NONADJ - 1 )
ms_hours_nonadj <- lm(data = clean_all_nonadj, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_NONADJ - 1 )
ms_hours_bytreat_nonadj <- lm(data = clean_all_nonadj, wagehr_ratioms ~ hours_ratioms*factor(TREAT) + EDUCGRP*EXPGRP_NONADJ - 1 )

summary(ms_emp_nonadj)
summary(ms_emp_bytreat_nonadj)
summary(ms_hours_nonadj)
summary(ms_hours_bytreat_nonadj)


## REVERSED NESTS
wm_hours_old <- lm(data = clean_all_adj, wagehr_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ADJ - 1 )
wm_hours_notreat_old <- lm(data = clean_all_adj %>% filter(TREAT == 0), wagehr_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ADJ - 1 )
wm_hours_treat_old <- lm(data = clean_all_adj %>% filter(TREAT == 1), wagehr_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ADJ - 1 )
summary(wm_hours_old)
summary(wm_hours_notreat_old)
summary(wm_hours_treat_old)




# ols_ms_emp <- lm(data = clean_all_adj, wage_ratioms ~ emp_ratioms + EDUCGRP*EXPGRP_ADJ - 1 )
# ols_ms_hours_nonadj <- lm(data = clean_all_nonadj, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_NONADJ - 1)
# 
# summary(lm(data = clean_all_adj, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1))
# summary(lm(data = clean_all_adj, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ + EDUCGRP*factor(COUNTYSTATE) + EXPGRP_ADJ*factor(COUNTYSTATE)- 1))
# 
# 
# ols_ms_hours <- lm(data = clean_all_adj, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1)
# 
# summary(ols_ms_emp)
# summary(ols_ms_hours_nonadj)
# summary(ols_ms_hours)
# 
# ggplot(data = clean_all_adj %>% mutate(skillcell = paste0(EDUCGRP, EXPGRP_ADJ)), aes(x = hours_ratioms, y = wagehr_ratioms, color = factor(TREAT))) + 
#   geom_point() + geom_smooth(method = "lm") + facet_wrap(~skillcell)
# 
# summary( lm(data = clean_all_adj %>% filter(TREAT == 1), wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1 ))
# print(mean(filter(clean_all_adj, TREAT == 1)$wagehr_ratioms, na.rm = TRUE))
# summary( lm(data = clean_all_adj %>% filter(TREAT == 0), wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ADJ - 1 ))
# print(mean(filter(clean_all_adj, TREAT == 0)$wagehr_ratioms, na.rm = TRUE))
# 
# 
# 
# summary(ivreg(data = clean_all, wage_ratioms ~ emp_ratioms + EDUCGRP*EXPGRP - 1 | TREAT+ EDUCGRP*EXPGRP ))
# summary(ivreg(data = clean_all, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP - 1| TREAT+ EDUCGRP*EXPGRP ))
# 
# 
# iv_ms_emp <- ivreg(data = clean_all, wage_ratioms ~ emp_ratioms + EDUCGRP*EXPGRP | STATEGROUP*EMPRATIOMS_1930 + EDUCGRP*EXPGRP)
# iv_ms_hours <- ivreg(data = clean_all, wagehr_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP | STATEGROUP*EMPRATIOMS_1930 + EDUCGRP*EXPGRP)
# 
# 
# ols_wm_emp <- lm(data = clean_all, wage_ratiowm ~ emp_ratiowm + EDUCGRP*EXPGRP - 1)# + WHITESCHOOLPOP)
# ols_wm_hours <- lm(data = clean_all, wagehr_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP - 1)# + WHITESCHOOLPOP)
# 
# summary(ols_wm_emp)
# summary(ols_wm_hours)
# 
# iv_wm_emp <- ivreg(data = clean_all, wage_ratiowm ~ emp_ratiowm + EDUCGRP*EXPGRP + WHITESCHOOLPOP | STATEGROUP*EMPRATIOWM_1930 + EDUCGRP*EXPGRP + WHITESCHOOLPOP)
# iv_wm_hours <- ivreg(data = clean_all, wagehr_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP + WHITESCHOOLPOP | STATEGROUP*EMPRATIOWM_1930 + EDUCGRP*EXPGRP + WHITESCHOOLPOP)
# 
# 
# 
# 
# summary(ols_ms)
# summary(iv_ms)
# 
# summary(ols_wm)
# summary(iv_wm)

### EXTRA STUFF ####
summary(lm(data = clean_all_gender, wagehr_ratiowm ~ hours_ratiowm + factor(COUNTYSTATE) - 1))

summary(ivreg(data = clean_all_gender, wagehr_ratiowm ~ hours_ratiowm  + factor(STATEICP) - 1 | TREAT + factor(STATEICP)))



# #### CLEANING DATA (move to 0_census_clean.R later)
# raw_teach <- read_csv(glue("{rawdata}/census_teach.csv"))
# raw_1930 <- read_csv(glue("{rawdata}/census_1930.csv"))
# raw_1940 <- read_csv(glue("{rawdata}/census_1940.csv"))
# allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))
# 
# # using 1 percent sample data to get county-level aggregates (population and school-age population)
# samp_aggs <- allyears_raw_samp %>% filter(YEAR == 1930 | YEAR == 1940) %>%
#   group_by(STATEICP, COUNTYICP, YEAR) %>%
#   summarize(POP = sum(PERWT),
#             WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
#             MW_WORK = sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2 & LABFORCE == 2, PERWT, 0))/sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2, PERWT, 0)))
# 
# # # fullcount aggregates
# # agg_1930 <- raw_1930 %>% group_by(STATEICP, COUNTYICP) %>%
# #   summarize(POP = sum(PERWT),
# #             WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
# #             MW_WORK = sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2 & LABFORCE == 2, PERWT, 0))/sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2, PERWT, 0)))
# # 
# # agg_1940 <- raw_1940 %>% group_by(STATEICP, COUNTYICP) %>%
# #   summarize(POP = sum(PERWT),
# #             WHITESCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, PERWT, 0)),
# #             MW_WORK = sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2 & LABFORCE == 2, PERWT, 0))/sum(ifelse(RACE == 1 & MARST != 6 & SEX == 2, PERWT, 0)),
# #             AGEMARR = mean(ifelse(AGEMARR == 0, NA, AGEMARR), na.rm=TRUE))
# # 
# 
# ## TESTING GROUPINGS FOR EDUC X EXPERIENCE CELLS
# clean_teach <- raw_teach %>% filter(YEAR == 1940 & RACE == 1 & OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & EDUCD != 999 & INCWAGE > 0) %>%
#     group_by(STATEICP, COUNTYICP) %>% 
#   left_join(samp_aggs %>% filter(YEAR == 1930) %>% select(c(STATEICP, COUNTYICP, MW_WORK)), by = c("STATEICP", "COUNTYICP")) %>%
#     mutate(GROUP = case_when(SEX == 1 ~ "Male",
#                            SEX == 2 & MARST == 6 ~ "Single Female",
#                            TRUE ~ "Married Female"),
#            EMP_MAR = sum(ifelse(GROUP == "Married Female", 1, 0)),
#            EMP_SINGLE = sum(ifelse(GROUP == "Single Female", 1, 0)),
#          EDUCGRP = case_when(EDUC <= 9 ~ "Some or No College",
#                              EDUC <= 11 ~ "College",
#                              TRUE ~ NA_character_),
#          EDUCYEARS = case_when(EDUCD < 13 ~ 0,
#                                EDUCD < 20 ~ EDUCD - 13,
#                                EDUCD < 24 ~ EDUCD - 17,
#                                EDUCD < 30 ~ EDUCD - 18,
#                                EDUCD == 30 ~ 9,
#                                EDUCD == 40 ~ 10,
#                                EDUCD == 50 ~ 11,
#                                EDUCD == 60 ~ 12,
#                                EDUCD == 70 ~ 13,
#                                EDUCD == 80 ~ 14,
#                                EDUCD == 90 ~ 15,
#                                EDUCD == 100 ~ 16,
#                                EDUCD < 120 ~ EDUCD - 93,
#                                TRUE ~ NA_real_),
#          AGEMARR = mean(ifelse(AGEMARR == 0, NA, AGEMARR), na.rm=TRUE), #county level mean age of marriage among married women teaching in 1940
#          MAR_PENALTY = ifelse(demgroup == "MW", 1, 0)*rbinom(1,1,1-LFP_MW_1930)*(AGE - AGEMARR),
#          EXP = AGE - EDUCYEARS - 6 - MAR_PENALTY, 
#          EXPALT = ifelse(MARST != 6 & SEX == 2, AGEMARR - EDUCYEARS - 6, EXP),
#          EXPGRP = case_when(EXP <= 5 ~ "0-5 Years",
#                             EXP <= 15 ~ "5-15 Years",
#                             !is.na(EXP) ~ ">15 Years",
#                             TRUE ~ NA_character_),
#          EXPGRP_ALT1 = case_when(EXPALT <= 5 ~ "0-5 Years", #for married women, only count years before they were married
#                             EXPALT <= 15 ~ "5-15 Years",
#                             !is.na(EXPALT) ~ ">15 Years",
#                             TRUE ~ NA_character_),
#          EXPGRP_ALT2 = case_when((MARST != 6 & SEX == 2) | EXP <= 5 ~ "0-5 Years", #for married women, put them in lowest experience group
#                                 EXP <= 15 ~ "5-15 Years",
#                                 !is.na(EXP) ~ ">15 Years",
#                                 TRUE ~ NA_character_),
#          HRS = ifelse(HRSWORK1 > 0 & WKSWORK1 > 0, HRSWORK1 * WKSWORK1, NA),
#          HR_WAGE = ifelse(INCWAGE <= 5002 & !is.na(HRS), INCWAGE/HRS, NA),
#          N_TEACHERS = n()) 
# 
# # histogram of educ / exp
# ggplot(clean_teach, aes(x = EDUCYEARS)) + geom_histogram(binwidth = 1) + geom_vline(xintercept = 12) + geom_vline(xintercept = 16) + facet_wrap(~GROUP)
# ggplot(clean_teach, aes(x = EXP)) + geom_histogram(binwidth = 1) + facet_wrap(~GROUP)
# 
# # graphing exp-earnings profile by group
# #expearn1 <- clean_teach %>% filter(YEAR == 1940) %>% group_by(GROUP, EXPALT) %>%
#   #summarize(wage = mean(HR_WAGE, na.rm=TRUE))
# expearn2 <- clean_teach %>% filter(YEAR == 1940 & EDUCYEARS == 16 & EXP < 40) %>% group_by(GROUP, EXP) %>%
#   summarize(wage = mean(HR_WAGE, na.rm=TRUE))
# 
# #ggplot(expearn1, aes(x = EXPALT, y = log(wage), color = GROUP)) + geom_point()
# ggplot(expearn2, aes(x = EXP, y = log(wage), color = GROUP)) + geom_point()
# 
# # graphing age-earnings profile by group
# ageearn <- clean_teach %>% filter(YEAR == 1940) %>% group_by(GROUP, AGE) %>%
#   summarize(wage = mean(HR_WAGE, na.rm=TRUE))
# 
# ggplot(ageearn, aes(x = AGE, y = log(wage), color = GROUP)) + geom_point()
# 
# educ_sum <- clean_teach %>% filter(YEAR == 1940) %>%
#   ungroup() %>% 
#   group_by(GROUP) %>% 
#   mutate(NGROUP = n()) %>% 
#   ungroup() %>%
#   group_by(GROUP, EDUCGRP) %>%
#   summarise(PCT = n()/mean(NGROUP),
#             wages = mean(HR_WAGE, na.rm=TRUE))
# 
# exp_sum <- clean_teach %>% filter(YEAR == 1940) %>%
#   ungroup() %>% 
#   group_by(GROUP) %>% 
#   mutate(NGROUP = n()) %>% 
#   ungroup() %>%
#   group_by(GROUP, EXPGRP) %>%
#   summarise(PCT = n()/mean(NGROUP),
#             wages = mean(HR_WAGE, na.rm=TRUE))
# 
# all_cells <- clean_teach %>% filter(YEAR == 1940) %>%
#   ungroup() %>% 
#   group_by(GROUP) %>% 
#   mutate(NGROUP = n()) %>% 
#   ungroup() %>%
#   group_by(GROUP, EDUCGRP, EXPGRP_ALT) %>%
#   summarise(PCT = n()/mean(NGROUP),
#             wages = mean(HR_WAGE, na.rm=TRUE))
# 
# 
# clean_teach_group <- clean_teach %>%
#   group_by(STATEICP, COUNTYICP, GROUP, EDUCGRP, EXPGRP, YEAR) %>%
#   summarize(N_TEACHERS = mean(N_TEACHERS),
#             L = n(),
#             L_hours = sum(HRS, na.rm=TRUE),
#             w = mean(HR_WAGE, na.rm=TRUE))
# 
# emp1930 <- raw_teach %>% filter(RACE == 1 & OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & YEAR == 1930) %>%
#   group_by(STATEICP, COUNTYICP) %>% 
#   mutate(GROUP = case_when(SEX == 1 ~ "Male",
#                            SEX == 2 & MARST == 6 ~ "Single Female",
#                            TRUE ~ "Married Female")) %>%
#   summarize(EMPRATIOMS = sum(ifelse(GROUP == "Married Female", 1, 0))/sum(ifelse(GROUP == "Single Female", 1, 0)),
#             EMPRATIOWM = sum(ifelse(GROUP != "Male", 1, 0))/sum(ifelse(GROUP == "Male", 1, 0)))
# 
# clean_all <- inner_join(clean_teach_group, samp_aggs, by = c("STATEICP", "COUNTYICP", "YEAR")) %>% 
#   filter(N_TEACHERS >= 100) %>%
#   pivot_wider(names_from = c(YEAR, GROUP), values_from = c(SCHOOLPOP, L, L_hours, w)) %>%
#   mutate(SCHOOLPOP1940 = max(SCHOOLPOP_1940_Male,`SCHOOLPOP_1940_Married Female`),
#          wage_ratioms = log(`w_1940_Married Female`/`w_1940_Single Female`),
#          emp_ratioms = log(`L_1940_Married Female`/`L_1940_Single Female`),
#          hours_ratioms = log(`L_hours_1940_Married Female`/`L_hours_1940_Single Female`),
#          totalhours_women = `L_hours_1940_Married Female` + `L_hours_1940_Single Female`,
#          sharehours_marriedwomen = `L_hours_1940_Married Female`/totalhours_women,
#          sharehours_singlewomen = `L_hours_1940_Single Female`/totalhours_women,
#          wage_ratiowm = log((`w_1940_Single Female`*sharehours_singlewomen +`w_1940_Married Female`*sharehours_marriedwomen)/`w_1940_Male`),
#          emp_ratiowm = log((`L_1940_Married Female`+`L_1940_Single Female`)/`L_1940_Male`),
#          hours_ratiowm = log((`L_hours_1940_Married Female`+`L_hours_1940_Single Female`)/`L_hours_1940_Male`),
#          STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated"),
#          SOUTH = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98), 1, 0)) %>% # adding grouping for whether in group 1 (ky, nc) or group 2 (WV, TN, SC, OH, IN, IL, MO) southern state
#   left_join(emp1930, by = c("STATEICP", "COUNTYICP")) %>%
#   filter(SOUTH == 1)
# 
# ols_ms <- lm(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP + SCHOOLPOP1940)
# iv_ms <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP + SCHOOLPOP1940 | STATEGROUP*EMPRATIOMS + EDUCGRP*EXPGRP + SCHOOLPOP1940)
# 
# 
# ols_wm <- lm(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP + SCHOOLPOP1940)
# iv_wm <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP + SCHOOLPOP1940 | STATEGROUP*EMPRATIOWM + EDUCGRP*EXPGRP + SCHOOLPOP1940)
# 
# 
# summary(ols_ms)
# summary(iv_ms)
# 
# summary(ols_wm)
# summary(iv_wm)
# 
# 
# 
# ### ALT DEF OF EXP
# clean_teach_group <- clean_teach %>%
#   group_by(STATEICP, COUNTYICP, GROUP, EDUCGRP, EXPGRP_ALT1, YEAR) %>%
#   summarize(N_TEACHERS = mean(N_TEACHERS),
#             L = n(),
#             L_hours = sum(HRS, na.rm=TRUE),
#             w = mean(HR_WAGE, na.rm=TRUE))
# 
# emp1930 <- raw_teach %>% filter(RACE == 1 & OCC1950 == 93 & CLASSWKR == 2 & AGE >= 18 & YEAR == 1930) %>%
#   group_by(STATEICP, COUNTYICP) %>% 
#   mutate(GROUP = case_when(SEX == 1 ~ "Male",
#                            SEX == 2 & MARST == 6 ~ "Single Female",
#                            TRUE ~ "Married Female")) %>%
#   summarize(EMPRATIOMS = sum(ifelse(GROUP == "Married Female", 1, 0))/sum(ifelse(GROUP == "Single Female", 1, 0)),
#             EMPRATIOWM = sum(ifelse(GROUP != "Male", 1, 0))/sum(ifelse(GROUP == "Male", 1, 0)))
# 
# 
# clean_all <- inner_join(clean_teach_group, samp_aggs, by = c("STATEICP", "COUNTYICP", "YEAR")) %>% 
#   filter(N_TEACHERS >= 100) %>%
#   pivot_wider(names_from = c(YEAR, GROUP), values_from = c(SCHOOLPOP, L, L_hours, w)) %>%
#   mutate(SCHOOLPOP1940 = max(SCHOOLPOP_1940_Male,`SCHOOLPOP_1940_Married Female`),
#          wage_ratioms = log(`w_1940_Married Female`/`w_1940_Single Female`),
#          emp_ratioms = log(`L_1940_Married Female`/`L_1940_Single Female`),
#          hours_ratioms = log(`L_hours_1940_Married Female`/`L_hours_1940_Single Female`),
#          totalhours_women = `L_hours_1940_Married Female` + `L_hours_1940_Single Female`,
#          sharehours_marriedwomen = `L_hours_1940_Married Female`/totalhours_women,
#          sharehours_singlewomen = `L_hours_1940_Single Female`/totalhours_women,
#          wage_ratiowm = log((`w_1940_Single Female`*sharehours_singlewomen +`w_1940_Married Female`*sharehours_marriedwomen)/`w_1940_Male`),
#          emp_ratiowm = log((`L_1940_Married Female`+`L_1940_Single Female`)/`L_1940_Male`),
#          hours_ratiowm = log((`L_hours_1940_Married Female`+`L_hours_1940_Single Female`)/`L_hours_1940_Male`),
#          STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated"),
#          SOUTH = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98), 1, 0)) %>% # adding grouping for whether in group 1 (ky, nc) or group 2 (WV, TN, SC, OH, IN, IL, MO) southern state
#   left_join(emp1930, by = c("STATEICP", "COUNTYICP")) %>%
#   filter(SOUTH == 1)
# 
# ols_ms <- lm(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)
# iv_ms <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOMS) & EMPRATIOMS != Inf), wage_ratioms ~ hours_ratioms + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940 | STATEGROUP*EMPRATIOMS + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)
# 
# 
# ols_wm <- lm(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)
# iv_wm <- ivreg(data = clean_all %>% filter(!is.na(EMPRATIOWM) & EMPRATIOWM != Inf), wage_ratiowm ~ hours_ratiowm + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940 | STATEGROUP*EMPRATIOWM + EDUCGRP*EXPGRP_ALT1 + SCHOOLPOP1940)
# 
# 
# summary(ols_ms)
# summary(iv_ms)
# 
# summary(ols_wm)
# summary(iv_wm)





