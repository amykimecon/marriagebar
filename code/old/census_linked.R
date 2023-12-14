### INITIAL DESCRIPTIVES WITH 1930 and 1940 linked census data
### AUTHOR: AMY KIM
### LAST EDITED: APR 20 2023
library(tidyverse)
library(glue)

root = "/Users/amykim/Dropbox (Princeton)/marriagebar"
git = "Users/amykim/GitHub/marriagebar"
rawdata = glue("{root}/ipums_raw")
outdata = glue("{root}/clean_data")
outfigs = glue("{git}/figures")

# importing raw data and only selecting teachers -- 1930 and 1940 data
# raw_linked <- read_csv(glue("{rawdata}/census_linking.csv"))
# 
# teach_ids <- filter(raw_linked, OCC1950 == 93 & CLASSWKR == 2)$HIK
# teach_linked <- raw_linked %>% filter(HIK %in% teach_ids)
# write_csv(teach_linked, glue("{outdata}/linked_teachers.csv"))
teach_linked <- read_csv(glue("{outdata}/linked_teachers.csv"))

# importing raw data and only selecting teachers -- 1900-1940 data
# raw_linked_all <- read_csv(glue("{rawdata}/census_linking_all.csv"))
# 
# teach_ids_all <- filter(raw_linked_all, OCC1950 == 93)$HIK
# teach_linked_all <- raw_linked_all %>% filter(HIK %in% teach_ids_all)
# write_csv(teach_linked_all, glue("{outdata}/linked_teachers_all.csv"))
# teach_linked_all <- read_csv(glue("{outdata}/linked_teachers_all.csv"))



#### WHO MOVES IN AND WHO MOVES OUT OF TEACHING?
teach_wide <- teach_linked %>% 
  mutate(demgroup = case_when(SEX == 1 ~ "M",
                              SEX == 2 & MARST == 6 ~ "SW",
                              TRUE ~ "MW"),
         teach = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0)) %>%
  pivot_wider(id_cols = c(HIK), names_from = YEAR, values_from = c(STATEICP, COUNTYICP, AGE, AGEMARR, SEX, RACE, MARST, CHBORN, EDUC, EMPSTAT, CLASSWKR, LABFORCE, OCC1950, OCCSCORE, WKSWORK1, HRSWORK1, INCWAGE, demgroup, teach))

teach_wide_clean <- teach_wide %>% filter((teach_1940 == 1)|(teach_1930 == 1)) %>%
          mutate(new_teach = ifelse(OCC1950_1930 != 93 & OCC1950_1940 == 93, 1, 0),
                                           leave_teach = ifelse(OCC1950_1930 == 93 & OCC1950_1940 != 93, "Left Teaching", "Did Not Leave Teaching"),
                                           got_married = ifelse(MARST_1930 == 6 & MARST_1940 != 6, 1, 0),
                                           married_1940 = ifelse(MARST_1940 != 6, 1, 0),
                                           married_1930 = ifelse(MARST_1930 != 6, 1, 0),
                                           educ_level = case_when(EDUC_1940 <= 6 ~ "High School",
                                                                  EDUC_1940 <= 10 ~ "Some College",
                                                                  EDUC_1940 <= 11 ~ "College",
                                                                  TRUE ~ NA_character_),
                                           treat = ifelse(STATEICP_1940 == 47 | STATEICP_1940 == 51 | STATEICP_1940 == 98, 1, 0))
  

## QUESTION 1: CAN WE MODEL MARRIED WOMEN WHO ENTER TEACHING AS HAVING NO EXPERIENCE?
# what percent of married women who entered teaching in 1940 and were over 18 (if didn't go to college) or over 23 (if did go to college) were previously in the labor force?
new_teachers <- teach_wide_clean %>% filter(new_teach == "New Teacher" & AGE_1930 >= 23) %>%#((AGE_1930 >= 18 & educ_level == "High School") | (AGE_1930 >= 25 & (educ_level == "Some College" | educ_level == "College")))) %>%
  mutate(nilf = ifelse(EMPSTAT_1930 == 3, 1, 0),
         unemp = ifelse(EMPSTAT_1930 == 2, 1, 0))

## average age of new married female teachers (vs. avg age of new single female teachers)
new_summs <- new_teachers %>% group_by(SEX_1940, married_1940, married_1930) %>% 
  summarize(pct_nilf = sum(nilf)/n(),
            pct_unemp = sum(unemp)/n(),
            age = mean(AGE_1940),
            age_firstmarr = mean(ifelse(AGEMARR_1940==0,NA,AGEMARR_1940),na.rm=TRUE),
            n = n(),
            CHBORN = mean(ifelse(CHBORN_1940 == 0 | CHBORN_1940 == 99, NA, CHBORN_1940 - 1), na.rm=TRUE),
            HRSWORK1 = mean(ifelse(HRSWORK1_1940 == 0, NA, HRSWORK1_1940), na.rm=TRUE),
            WKSWORK1 = mean(ifelse(WKSWORK1_1940 == 0, NA, WKSWORK1_1940), na.rm=TRUE))

## if women employed in 1930 but not teaching, what job?
old_occs <- new_teachers %>% filter(SEX_1940 == 2 & EMPSTAT_1930 == 1 & CLASSWKR_1930 == 2) %>% group_by(OCC1950_1930) %>% 
  summarize(married = sum(ifelse(married_1930 == "Married in 1930", 1, 0)), 
            unmarried = sum(ifelse(married_1930 == "Married in 1930", 0, 1)),
            occscore = mean(OCCSCORE_1930))

## QUESTION 2: WHAT WERE MARRIED WOMEN TEACHERS IN 1940 DOING IN 1930?
mw_teach_1940 <- teach_wide_clean %>% filter(OCC1950_1940 == 93 & CLASSWKR_1940 == 2 & married_1940 == "Married in 1940") %>% #selecting mw teachers in 1940
  filter(STATEICP_1940 %in% c(47, 48, 51, 54, 56)) %>% #treated or control state
  mutate(group = ifelse(STATEICP_1940 %in% c(47, 48), "NC", "KY"),
         TREAT = ifelse(STATEICP_1940 %in% c(47, 51), 1, 0),
         cat = case_when(married_1930 == "Married in 1930" & OCC1950_1930 == 93 & CLASSWKR_1930 == 2 ~ "MW Teach 1930",
                         married_1930 == "Not Married in 1930" & OCC1950_1930 == 93 & CLASSWKR_1930 == 2 ~ "SW Teach 1930",
                         OCC1950_1930 != 93 ~ "Non Teach 1930",
                         TRUE ~ NA_character_)) %>%
  filter(cat %in% c("SW Teach 1930", "Non Teach 1930")) %>%
  group_by(TREAT, cat) %>%
  summarize(n = n()) %>%
  group_by(TREAT) %>% mutate(pct_group = n/sum(n))

ma_1940 <- teach_wide_clean %>% filter(OCC1950_1940 == 93 & CLASSWKR_1940 == 2 & SEX_1940 == 2) %>%
  filter(STATEICP_1940 %in% c(47, 48, 51, 54, 56)) %>%
  mutate(group = ifelse(STATEICP_1940 %in% c(47, 48), "NC", "KY"),
         TREAT = ifelse(STATEICP_1940 %in% c(47, 51), 1, 0),
         AGEMARR_1940 = ifelse(AGEMARR_1940 > 0, AGEMARR_1940, NA),
         years_marr = AGE_1940 - AGEMARR_1940,
         marr_before = case_when(group == "NC" & years_marr > 7 ~ 1,
                                 group == "KY" & years_marr > 2 ~ 1,
                                 is.na(years_marr) ~ NA_real_,
                                 TRUE ~ 0),
         cat = case_when(marr_before == 1 ~ "Married Before Law",
                         marr_before == 0 ~ "Married After Law",
                         is.na(marr_before) & MARST_1940 != 6 ~ "Married, No AGEMARR",
                         is.na(marr_before) & MARST_1940 == 6 ~ "Single"))

ma_1940 %>% group_by(TREAT, cat) %>% summarize(n = n(), 
                                               pct_new_teach = mean(new_teach),
                                               avg_age1940 = mean(AGE_1940))
# 
# 
# 
# teach_new <- teach_wide_clean %>% group_by(STATEICP_1940, COUNTYICP_1940) %>% 
#   summarize(n_teach40 = n(),
#             num_female = sum(ifelse(SEX_1940 == 2, 1, 0)),
#             pct_new = sum(new_teach)/n_teach40,
#             pct_new_female = sum(ifelse(SEX_1940 == 2 & new_teach, 1, 0))/num_female,
#             pct_female_married = sum(ifelse(MARST_1940 != 6 & SEX_1940 == 2, 1, 0))/num_female,
#             pct_female_got_married = sum(ifelse(!new_teach & got_married & SEX_1940 == 2, 1, 0))/num_female) %>%
#   full_join(teach_wide_clean %>% group_by(STATEICP_1930, COUNTYICP_1930) %>% summarize(n_teach30 = n(), pct_left = sum(leave_teach)/n_teach30), by = c("STATEICP_1940" = "STATEICP_1930","COUNTYICP_1940" = "COUNTYICP_1930")) %>%
#   mutate(treat = ifelse(STATEICP_1940 == 47 | STATEICP_1940 == 51 | STATEICP_1940 == 98, 1, 0))
# 
# summary(lm(pct_female_got_married ~ treat, data = teach_new))
# 
# 
# 










