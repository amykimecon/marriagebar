
# checking some numbers for the motivation
# 202403

#___________________________________________________
# set up base dataset ----
#___________________________________________________
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))

test_base <- allyears_raw_samp %>% 
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         white      = ifelse(RACE==1, 1, 0), 
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0),
         worker     = ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, 1, 0),
         demgroup_coll = case_when( demgroup=="Unmarried Women" & coll_above==1 ~ "SW, College",
                                    demgroup=="Unmarried Women" & coll_above==0 ~ "SW, Less than college",
                                    demgroup=="Married Women"   & coll_above==1 ~ "MW, College",
                                    demgroup=="Married Women"   & coll_above==0 ~ "MW, Less than college",
                                    demgroup=="Men"             & coll_above==1 ~ "Men, College",
                                    TRUE ~ "Men, Less than college"))


#___________________________________________________
# CHECK 0: % LF who are college-educated MW? ----
#___________________________________________________
check0 <- test_base %>% 
  filter(YEAR >= 1940) %>% # labforce data is different before 1940
  group_by(YEAR) %>% 
  mutate(workingpop = sum(ifelse(worker==1, PERWT, 0)),
         share_men  = sum(ifelse(demgroup=="Men" & worker==1, PERWT, 0))/workingpop,
         share_mwc  = sum(ifelse(demgroup_coll=="MW, College" & worker==1, PERWT, 0))/workingpop,
         share_swc  = sum(ifelse(demgroup_coll=="SW, College" & worker==1, PERWT, 0))/workingpop,
         share_mwlc = sum(ifelse(demgroup_coll=="MW, Less than college" & worker==1, PERWT, 0))/workingpop,
         share_swlc = sum(ifelse(demgroup_coll=="SW, Less than college" & worker==1, PERWT, 0))/workingpop) %>% 
  summarize(share_men  = mean(share_men),
            share_mwc  = mean(share_mwc),
            share_swc  = mean(share_swc),
            share_mwlc = mean(share_mwlc),
            share_swlc = mean(share_swlc))
check0 # SUMMARY STAT IN INTRO: 16% LFP for MW with College in 2020

#___________________________________________________
# CHECK 1: LFP of college-educated MW? ----
#___________________________________________________
check1 <- test_base %>% 
  filter(YEAR >= 1940) %>% # labforce data is different before 1940
  group_by(YEAR) %>% 
  summarize(lfp_mwc  = sum(ifelse(demgroup_coll=="MW, College" & worker==1, PERWT, 0))/sum(ifelse(demgroup_coll == "MW, College", PERWT, 0)))
check1


#______________________________________________________
# CHECK 2, BY RACE: % of rise in MW LFP came from WMW ----
#______________________________________________________
# plot: Time series of LFP for married/unmarried women/men, WHITE ONLY
check2 <- test_base %>% 
  filter(white==1) %>% 
  group_by(YEAR, demgroup) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 

ggplot(check2, aes(x=YEAR, y=lfp, group=demgroup, col=demgroup)) + 
  geom_point()


# plot: time series of LFP for married/unmarried women BY RACE 
check2p5 <- test_base %>% 
  filter(demgroup!="Men") %>% 
  mutate(women_group = case_when(demgroup=="Married Women"   & white==1 ~ "Married, White", 
                                 demgroup=="Married Women"   & white==0 ~ "Married, Non-White",
                                 demgroup=="Unmarried Women" & white==1 ~ "Unmarried, White",
                                 TRUE ~ "Unmarried, Non-White")) %>% 
  group_by(YEAR, women_group) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 

ggplot(check2p5, aes(x=YEAR, y=lfp, group=women_group, col=women_group)) + 
  geom_point(size = 3) + geom_line() + 
  theme_minimal() + 
  scale_x_continuous(limits=c(1900, 2000), breaks=seq(1900, 2000, 10))


#______________________________________________________
# CHECK 3, BY RACE+EDUC: % of rise in MW LFP from educated WMW ----
#______________________________________________________
# plot: Time series of LFP by married/unmarried women/men BY EDUC
check3 <- test_base %>% 
  group_by(YEAR, demgroup_coll) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 

ggplot(check3, aes(x=YEAR, y=lfp, group=demgroup_coll, col=demgroup_coll)) + 
  geom_point()

# plot: time series of LFP for married/unmarried women BY RACE AND EDUC
check3p5 <- test_base %>% 
  filter(demgroup!="Men") %>% 
  mutate(women_group = case_when(
    # white women
    demgroup_coll=="MW, College"           & white==1 ~ "MW, College, White", 
    demgroup_coll=="MW, Less than college" & white==1 ~ "MW, Less than college, White",
    # non-white women
    demgroup_coll=="MW, College"           & white==0 ~ "MW, College, Non-white", 
    TRUE ~ "MW, Less than college, Non-white")) %>% 
  # mutate(women_group = case_when(# white women
  #                                demgroup_coll=="MW, College"           & white==1 ~ "MW, College, White", 
  #                                demgroup_coll=="MW, Less than college" & white==1 ~ "MW, Less than college, White",
  #                                demgroup_coll=="SW, College"           & white==1 ~ "SW, College, White", 
  #                                demgroup_coll=="SW, Less than college" & white==1 ~ "SW, Less than college, White",
  #                                # non-white women
  #                                demgroup_coll=="MW, College"           & white==0 ~ "MW, College, Non-white", 
  #                                demgroup_coll=="MW, Less than college" & white==0 ~ "MW, Less than college, Non-white",
  #                                demgroup_coll=="SW, College"           & white==0 ~ "SW, College, Non-white", 
  #                                TRUE ~ "SW, Less than college, Non-white"))  %>% 
  group_by(YEAR, women_group) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 
         
print(check3p5 %>% filter(women_group=="MW, College, White")) # SUMMARY STAT IN INTRO


