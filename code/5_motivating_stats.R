# checking some numbers for the motivation
# 202403

# CHECK: what share of the rise in MW LFP came from educated WMW? 
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

data_to_plot <- test %>% 
  filter(demgroup!="Men") %>% 
  mutate(women_group = ifelse(demgroup=="Married Women" & white==1, "Married, White", 
                              ifelse(demgroup=="Married Women" & white==0, "Married, Non-White", 
                                     ifelse(demgroup=="Unmarried Women" & white==1, "Unmarried, White", "Unmarried, Non-White"))))

ggplot(data_to_plot, aes(x=YEAR, y=lfp, group=women_group, col=women_group)) + 
  geom_point(size = 3) + geom_line() + theme_minimal() + scale_x_continuous(limits=c(1900, 2000), breaks=seq(1900, 2000, 10))
