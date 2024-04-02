# checking some numbers for the motivation
# 202403



# CHECK 1 : what share of the rise in MW LFP came from educated WMW? ----
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))

test<- allyears_raw_samp %>% 
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
                                    TRUE ~ "Men"),
         demgroup_coll_all = case_when(demgroup == "Married Women" & coll_above == 1 ~ "MW college",
                                       demgroup == "Unmarried Women" & coll_above == 1 ~ "SW college",
                                       demgroup == "Men" & coll_above == 1 ~ "Men college")) %>%
  group_by(YEAR, white, demgroup_coll) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 

# plot: Time series of LFP by white married/unmarried women (and men)
ggplot(test %>% filter(white==1), aes(x=YEAR, y=lfp, group=demgroup_coll_all, col=demgroup_coll_all)) + 
  geom_point()

# plot: time series of LFP for married/unmarried white/non-white women
data_to_plot <- test %>% 
  filter(demgroup_coll!="Men") %>% 
  mutate(women_group = case_when(# white women
                                 demgroup_coll=="MW, College"           & white==1 ~ "MW, College, White", 
                                 demgroup_coll=="MW, Less than college" & white==1 ~ "MW, Less than college, White",
                                 demgroup_coll=="SW, College"           & white==1 ~ "SW, College, White", 
                                 demgroup_coll=="SW, Less than college" & white==1 ~ "SW, Less than college, White",
                                 # non-white women
                                 demgroup_coll=="MW, College"           & white==0 ~ "MW, College, Non-white", 
                                 demgroup_coll=="MW, Less than college" & white==0 ~ "MW, Less than college, Non-white",
                                 demgroup_coll=="SW, College"           & white==0 ~ "SW, College, Non-white", 
                                 TRUE ~ "SW, Less than college, Non-white")) 
         
ggplot(data_to_plot, aes(x=YEAR, y=lfp, group=women_group, col=women_group)) + 
  geom_point(size = 3) + geom_line() + theme_minimal() + scale_x_continuous(limits=c(1900, 2000), breaks=seq(1900, 2000, 10))




# CHECK 2: what share of the rise in MW LFP came from WMW? ----
test<- allyears_raw_samp %>% 
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         white      = ifelse(RACE==1, 1, 0), 
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0),
         worker     = ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, 1, 0)) %>%
  group_by(YEAR, demgroup, white) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 

ggplot(test %>% filter(white==1), aes(x=YEAR, y=lfp, group=demgroup, col=demgroup)) + 
  geom_point()

# plot: time series of LFP for married/unmarried white/non-white women 
data_to_plot <- test %>% 
  filter(demgroup!="Men") %>% 
  mutate(women_group = ifelse(demgroup=="Married Women" & white==1, "Married, White", 
                              ifelse(demgroup=="Married Women" & white==0, "Married, Non-White", 
                                     ifelse(demgroup=="Unmarried Women" & white==1, "Unmarried, White", "Unmarried, Non-White"))))

ggplot(data_to_plot, aes(x=YEAR, y=lfp, group=women_group, col=women_group)) + 
  geom_point(size = 3) + geom_line() + theme_minimal() + scale_x_continuous(limits=c(1900, 2000), breaks=seq(1900, 2000, 10))
