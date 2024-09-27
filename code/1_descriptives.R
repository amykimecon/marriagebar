### DESCRIPTIVES (edited sep 2024)
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_1_descriptives.txt", append=FALSE)

#_________________________________________________
# MOTIVATING STATS FROM INTRO ----
#_________________________________________________

## set up base dataset ----
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
## CHECK 0: % LF who are college-educated MW? ----
#___________________________________________________
check0 <- test_base %>% 
  filter(YEAR >= 1940) %>% # labforce data is different before 1940
  group_by(YEAR) %>% 
  mutate(workingpop = sum(ifelse(worker==1, PERWT, 0)),
         share_men  = sum(ifelse(demgroup=="Men" & worker==1, PERWT, 0))/workingpop,
         share_mwc  = sum(ifelse(demgroup_coll=="MW, College" & worker==1, PERWT, 0))/workingpop,
         share_mwcw  = sum(ifelse(demgroup_coll=="MW, College" & worker==1 & white==1, PERWT, 0))/workingpop,
         share_swc  = sum(ifelse(demgroup_coll=="SW, College" & worker==1, PERWT, 0))/workingpop,
         share_mwlc = sum(ifelse(demgroup_coll=="MW, Less than college" & worker==1, PERWT, 0))/workingpop,
         share_swlc = sum(ifelse(demgroup_coll=="SW, Less than college" & worker==1, PERWT, 0))/workingpop) %>% 
  summarize(share_men  = mean(share_men),
            share_mwc  = mean(share_mwc),
            share_mwcw  = mean(share_mwcw),
            share_swc  = mean(share_swc),
            share_mwlc = mean(share_mwlc),
            share_swlc = mean(share_swlc))
check0 # SUMMARY STAT IN INTRO: 16% LFP for MW with College in 2020

#___________________________________________________
## CHECK 1: LFP of college-educated MW? ----
#___________________________________________________
check1 <- test_base %>% 
  filter(YEAR >= 1940) %>% # labforce data is different before 1940
  group_by(YEAR) %>% 
  summarize(lfp_mwc  = sum(ifelse(demgroup_coll=="MW, College" & worker==1, PERWT, 0))/sum(ifelse(demgroup_coll == "MW, College", PERWT, 0)))
check1


#______________________________________________________
## CHECK 2, BY RACE: % of rise in MW LFP came from WMW ----
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
## CHECK 3, BY RACE+EDUC: % of rise in MW LFP from educated WMW ----
#______________________________________________________
# plot: Time series of LFP by married/unmarried women/men BY EDUC
check3 <- test_base %>% 
  group_by(YEAR, demgroup_coll) %>%
  mutate(pop     = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)),
         working = sum(ifelse(worker==1, PERWT, 0))) %>% 
  summarize(working = mean(working),
            pop     = mean(pop),
            lfp     = working/pop) 
print(check2 %>% filter(demgroup_coll=="Men, College"))

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

## for footnote: LFP & college-going of black married women
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)
black_white_stats <- tbl(con, "allyears_raw_samp") %>% 
  filter(SEX == 2 & MARST %in% c(1,2) & AGE >= 18 & AGE <= 64 & RACE %in% c(1,2)) %>%
  mutate(coll = ifelse(EDUC %in% c(7, 8, 9, 10, 11), 1, 0)) %>%
  group_by(YEAR, RACE) %>%
  mutate(share_coll = sum(ifelse(coll == 1, PERWT, 0))/sum(PERWT),
         lfp = sum(ifelse(LABFORCE == 2, PERWT, 0))/sum(PERWT)) %>%
  group_by(RACE, coll, YEAR) %>%
  summarize(n_group = n(), 
            share_coll = mean(share_coll),
            lfp = mean(lfp),
            lfp_coll = sum(ifelse(LABFORCE == 2, PERWT, 0))/sum(PERWT)) %>% collect()
dbDisconnect(con, shutdown = TRUE)

## 1940 LFP of WMW w/ college (from full count) ----
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)
wmw_coll_lfp_1940 <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(YEAR == 1940 & RACE == 1 & demgroup == "MW" & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11) %>%
  mutate(group = 1) %>% group_by(group) %>%
  summarize(lfp = sum(worker)/n()) %>% collect()
print(glue("1940 LFP of White Married Women with at least some college: {round(wmw_coll_lfp_1940$lfp[1]*100,2)}%"))

bmw_coll_lfp_1940 <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(YEAR == 1940 & RACE == 2 & demgroup == "MW" & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11) %>%
  mutate(group = 1) %>% group_by(group) %>%
  summarize(lfp = sum(worker)/n()) %>% collect()
print(glue("1940 LFP of Black Married Women with at least some college: {round(bmw_coll_lfp_1940$lfp[1]*100,2)}%"))

wmw_lfp_1940 <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(YEAR == 1940 & RACE == 1 & demgroup == "MW" & AGE >= 18 & AGE <= 64) %>%
  mutate(group = 1) %>% group_by(group) %>%
  summarize(lfp = sum(worker)/n()) %>% collect()
print(glue("1940 LFP of White Married Women: {round(wmw_lfp_1940$lfp[1]*100,2)}%"))

bmw_lfp_1940 <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(YEAR == 1940 & RACE == 2 & demgroup == "MW" & AGE >= 18 & AGE <= 64) %>%
  mutate(group = 1) %>% group_by(group) %>%
  summarize(lfp = sum(worker)/n()) %>% collect()
print(glue("1940 LFP of Black Married Women: {round(bmw_lfp_1940$lfp[1]*100,2)}%"))

dbDisconnect(con, shutdown = TRUE)

## 2020 LFP of WMW w/ college (from ACS) ----
wmw_coll_lfp_2020 <- filter(samp_byyear_coll, demgroup == "WMW, College" & YEAR == 2020)
print(glue("2020 LFP of White Married Women with at least some college: {round(wmw_coll_lfp_2020$lfp*100,2)}%"))

## 2020 Share LF WMW w/ college (from ACS) ----
print(glue("2020 LF Share of White Married Women with at least some college: {round(wmw_coll_lfp_2020$pctlf*100,2)}%"))

## figure: share workers male/single fem/married fem over time ----
fig1a_demogtrends_workers <- ggplot(data = samp_byyear, 
                                    aes(x = YEAR, y = pctlf, 
                                        fill = factor(demgroup, levels = c("Men", "Married Women", "Unmarried Women")))) + 
  geom_area() +
  xlab("Year") + ylab("Fraction of US Labor Force") + labs(fill = "") + 
  scale_fill_manual(values=c(men_col, mw_col, sw_col)) + 
  theme_minimal() + theme(legend.position = "bottom") 

## figure: share teachers male/single fem/married fem over time ----
fig1b_demogtrends_teachers <- ggplot(data = samp_byyear, 
                                     aes(x = YEAR, y = pctteach, 
                                         fill = factor(demgroup, levels = c("Men", "Married Women", "Unmarried Women")))) + 
  geom_area() +
  xlab("Year") + ylab("Fraction of US Teachers") + labs(fill = "") + 
  scale_fill_manual(values=c(men_col, mw_col, sw_col)) + 
  theme_minimal() + theme(legend.position = "bottom")
# 
# #_________________________________________________
# # MAP OF TREATMENT & CONTROL COUNTIES ----
# #_________________________________________________
# ## neighbor ----
# graph_treatment(countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1), 
#                 filename = "treatmap_neighbor") + 
#                 ggtitle("Neighbor Sample")
# 
# ## match 1 ----
# graph_treatment(countysumm %>% filter(match_weight1 != 0 & mainsamp == 1), 
#                 filename = "treatmap_matched1") + 
#                 ggtitle("Matched Sample 1")
# 
# ## match 2 ----
# graph_treatment(countysumm %>% filter(match_weight2 != 0 & mainsamp == 1), 
#                 filename = "treatmap_matched2") + 
#                 ggtitle("Matched Sample 2")
# 
# ## match 3 ----
# graph_treatment(countysumm %>% 
#                   filter(match_weight3 != 0 & mainsamp == 1) %>%
#                   mutate(weights = match_weight3),
#                 filename = "treatmap_matched3", full = TRUE) +
#                 ggtitle("Matched Sample 3")

#_________________________________________________
# MATCHING TEST FOR TREATMENT AND CONTROL: BOXPLOTS
#_________________________________________________
#varnames <- c("URBAN","LFP", "PCT_LIT")
varnames <-  c("URBAN","LFP", "LFP_MW", "POP", "AGE", "PCT_LIT", "PCT_WHITE", "pct_sw_Teacher", "pct_mw_Teacher", "NCHILD","PCT_MARR")

countysummwide <- countysumm %>% filter(mainsamp==1)%>%
  pivot_wider(id_cols = c(match_weight1, match_weight2, match_weight3, STATEICP, COUNTYICP, FIPS, mainsamp, TREAT, neighbor_samp),
                                             names_from  = YEAR, values_from = all_of(c(varnames))) %>%
  retailsales() %>% #merging with retail sales
  mutate(across(all_of(c(glue("{varnames}_1930"),"RRTSAP39")), scale)) #standardizing variables
  
boxplotdata <- countysummwide %>% filter(TREAT != 1) %>% 
  pivot_longer(starts_with("match_weight"), names_to = "sample", names_prefix = "match_weight", values_to = "weight") %>%
  filter(weight != 0) %>% #pivoting long so we have each row corresp. to a control unit from a different matched sample
  bind_rows(countysummwide %>% filter(TREAT != 1) %>% mutate(sample = "All Untreated", weight = 1)) %>% #adding all units
  bind_rows(countysummwide %>% filter(TREAT == 1) %>% mutate(sample = "Treated", weight = 1)) %>% #adding treated units
  bind_rows(countysummwide %>% filter(neighbor_samp == 1 & TREAT != 1) %>% mutate(sample = "Neighbor", weight = 1)) %>% #adding neighbor sample
  mutate(sample = factor(sample, levels = c("All Untreated", 3, 2, 1, "Neighbor", "Treated"), labels = c("All Untreated", "Control 3", "Control 2", "Control 1",  "Neighbor","Treated"))) %>%
  select(c(all_of(glue("{varnames}_1930")), RRTSAP39, FIPS, sample, weight)) %>%
  pivot_longer(all_of(c(glue("{varnames}_1930"),"RRTSAP39")), names_to = "var", values_to = "value") %>%
  mutate(var = factor(var, levels = c("NCHILD_1930","PCT_MARR_1930","pct_mw_Teacher_1930","pct_sw_Teacher_1930","RRTSAP39","LFP_MW_1930", "LFP_1930",
    "PCT_LIT_1930", "PCT_WHITE_1930", "AGE_1930", "URBAN_1930","POP_1930"), 
    labels = c("Number of Children", "Share Married", "Share Teachers Marr. Women", "Share Teachers Unmarr. Women",
               "Retail Sales per Capita (1939)", "LFP of Married Women", "Labor Force Participation", "Share Literate", "Share White", "Mean Age", "Share Urban", "Population")))

# boxplot
ggplot(data = boxplotdata, aes(x = var, y = value, weight = weight)) + 
  geom_boxplot(aes(middle = mean(value), fill = sample), outlier.shape = NA) + 
  ylim(-4,4) + scale_fill_manual(values = c(treat_col, control_col, "#76c996", "#9fd9b3", "#c6e8d1", "grey"),
                                 breaks = c("Treated", "Neighbor", "Control 1", "Control 2", "Control 3", "All Untreated")) +
  coord_flip() +
  labs(y = "Standardized Distribution",x = "",fill = "") + theme_minimal() +
  theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(glue("{outfigs}/paper/matching_boxplot.png"), width = 12, height = 10)

#_________________________________________________
# MATCHING TEST FOR TREATMENT AND CONTROL: ABS STD MEAN DIFFS
#_________________________________________________
meandiffsdata <- countysummwide %>% filter(TREAT != 1) %>% 
  pivot_longer(starts_with("match_weight"), names_to = "sample", names_prefix = "match_weight", values_to = "weight") %>%
  filter(weight != 0) %>% #pivoting long so we have each row corresp. to a control unit from a different matched sample
  bind_rows(countysummwide %>% filter(TREAT != 1) %>% mutate(sample = "All", weight = 1)) %>% #adding all units
  bind_rows(countysummwide %>% filter(neighbor_samp == 1 & TREAT != 1) %>% mutate(sample = "Neighbor", weight = 1)) %>% #adding neighbor sample
  mutate(sample = factor(sample, levels = c("All", 3, 2, 1, "Neighbor"), labels = c("All", "Control 3", "Control 2", "Control 1",  "Neighbor"))) %>%
  group_by(sample) %>%
  summarize(across(c(all_of(glue("{varnames}_1930")), RRTSAP39), function(.x) weighted.mean(.x, weight, na.rm=TRUE))) %>%
  pivot_longer(all_of(c(glue("{varnames}_1930"),"RRTSAP39")), names_to = "var", values_to = "value") %>%
  left_join(countysummwide %>% filter(TREAT == 1) %>% 
              summarize(across(c(all_of(glue("{varnames}_1930")), RRTSAP39), function(.x) mean(.x, na.rm=TRUE))) %>%
              pivot_longer(all_of(c(glue("{varnames}_1930"),"RRTSAP39")), names_to = "var", values_to = "treatvalue")) %>%
  mutate(absmeandiff = abs(value - treatvalue))

ggplot(data = meandiffsdata, aes(x = var, y = absmeandiff)) + 
  geom_point(aes(color = sample, shape = sample), size = 4) + 
  coord_flip()
  

#______________________________________________________________________
# FIG 1: DISTN OF FRAC ALL TEACHERS/SEC MARRIED WOMEN ----
#______________________________________________________________________
county_means_all <- countysumm %>% 
  group_by(YEAR, TREAT) %>%
  filter(mainsamp == 1) %>%
  summarise(across(c(pct_mw_Teacher, pct_mw_Secretary), function(.x) mean(.x, na.rm=TRUE))) %>%
  mutate(fig_label = ifelse(TREAT == 1, glue("Treated Mean: {round(pct_mw_Teacher,3)}"),
                            glue("Untreated Mean: {round(pct_mw_Teacher,3)}")),
         xpos = ifelse(YEAR == 1950, 0.17, 0.5),
         ypos = ifelse(TREAT == 1, 8, 11),
         TREAT = ifelse(TREAT == 1, "Marriage Bar Removed (Treated)", "Marriage Bar Not Removed (Untreated)"),
  )

## paper ----
ggplot(filter(countysumm, mainsamp == 1) %>% 
         mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed (Treated)", "Marriage Bar Not Removed (Untreated)")),
       aes(x = pct_mw_Teacher, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_histogram(aes(y=after_stat(density)), position = "identity", alpha = 0.3, binwidth = 0.01, linewidth = 0.2) + 
  #geom_density(alpha = 0.2) +
  geom_vline(data = county_means_all, 
             aes(xintercept = pct_mw_Teacher, color = factor(TREAT)), 
             linewidth = 0.6,linetype = "dashed") + 
  geom_text(data = county_means_all, aes(x = xpos, y = ypos, label = fig_label), size = 3) +
  scale_color_manual(values=c(control_col, treat_col)) +
  scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
  facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Teachers as Fraction of All Teachers in County", color = "") + 
  theme_minimal() + 
  theme(legend.position = "bottom", axis.text = element_text(size = 12), text = element_text(size = 14))

ggsave(filename = glue("{outfigs}/paper/fig2_marteach_dist.png"), width = 8, height = 5)

## slides (teachers by year) ----
for (yr in seq(1910,1950,10)){
  ggplot(filter(countysumm, mainsamp == 1 & YEAR == yr) %>% 
           mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")),
         aes(x = pct_mw_Teacher, color = factor(TREAT), fill = factor(TREAT))) + 
    geom_histogram(aes(y=after_stat(density)), position = "identity", alpha = 0.3, binwidth = 0.01, linewidth = 0.2) + 
    #geom_density(alpha = 0.2) +
    geom_vline(data = county_means_all %>% 
                 filter(YEAR == yr), aes(xintercept = pct_mw_Teacher, color = factor(TREAT)), 
               linewidth = 0.6, linetype = "dashed") + 
    scale_color_manual(values=c(control_col, treat_col)) +
    scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
    facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Teachers as Fraction of All Teachers in County", color = "") + 
    theme_minimal() + 
    theme(legend.position = "bottom")
}


#______________________________________________________
# TAB 1: SUM STATS BY COUNTY GROUP ----
#______________________________________________________
countysumm_stats <- countysumm %>%
  #filter(mainsamp == 1) %>% #main sample (all counties)
  mutate(POP_THOUS            = POP/1000, 
         WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
         STUDENT_PER_TEACH    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA),
         summgroup            = "All") %>%
  rbind(countysumm %>% filter(SOUTH == 1) %>% #mainsamp == 1 & SOUTH == 1) %>%  # southern counties only
          mutate(POP_THOUS            = POP/1000, 
                 WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
                 STUDENT_PER_TEACH    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA), 
                 summgroup            = "South")) %>%
  rbind(countysumm %>% filter(neighbor_samp == 1) %>% #mainsamp == 1 & neighbor_samp == 1) %>% #neighboring & treated counties (separately)
          mutate(POP_THOUS            = POP/1000, 
                 WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
                 STUDENT_PER_TEACH    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA), 
                 summgroup            = ifelse(TREAT == 1, "Treated", "Neighb. Sth."))) %>%
  mutate(summgroup = factor(summgroup, levels = c("All", "South", "Treated", "Neighb. Sth.")))

varnames_1930 = c("POP_THOUS","WHITESCHOOLPOP_THOUS", "URBAN", 
                  "LFP_MW", "LFP_WMW", "NCHILD", 
                  "STUDENT_PER_TEACH","pct_m_Teacher", 
                  "pct_sw_Teacher", "pct_mw_Teacher")
varlabs_1930 = c("Population (Thous.)", "White School-Age Pop. (Thous.)", "Share Urban", 
                 "LFP of Married Women", "LFP of White Married Women", "Num. Children of Marr. Wom.", 
                 "Students/Teachers", "Share Men", 
                 "Share Single Women", "Share Married Women")

# summary statistics by state group
summ_stats <- countysumm_stats %>%
  filter(YEAR == 1930) %>%
  group_by(summgroup) %>% 
  summarize(OBS = n(),
            across(all_of(varnames_1930), .fns = c(~mean(.x, na.rm=TRUE), #mean
                                                   ~sd(.x, na.rm=TRUE)/sqrt(OBS))))

summ_stats_out <- as.data.frame(t(summ_stats))

summtex <- file(glue("./tables/summstats.tex"), open = "w")
names <- summ_stats_out[1,]

writeLines(c("\\begin{tabular}{lC{2cm}C{2cm}C{2.3cm}C{2cm}}", 
             "\\hhline{=====}",
             "&", glue("{names[1]} & {names[2]} & {names[3]} & {names[4]}\\\\"),
             "& (1) & (2) & (3) & (4) \\\\", "\\hhline{-----}"), summtex)
for (i in 1:length(c(varlabs_1930))){
  means <- round(as.numeric(summ_stats_out[2*i+1,]), 3)
  sds <- paste0("(", round(as.numeric(summ_stats_out[2*i + 2,]), 3), ")")
  # panel A header
  if (varlabs_1930[i] == "Population (Thous.)"){
    writeLines(c("\\multicolumn{5}{l}{\\underline{\\textbf{Panel A: General County Statistics}}}\\\\ [1em]"), summtex)
  }
  # panel B header
  if (varlabs_1930[i] == "Students/Teachers"){
    writeLines(c("[1em] \\multicolumn{5}{l}{\\underline{\\textbf{Panel B: County Statistics on White Teachers}}}\\\\ [1em]"), summtex)
  }
  writeLines(c(paste(c(varlabs_1930)[i], "&", glue("{means[1]} & {means[2]} & {means[3]} & {means[4]}\\\\")),
               "&", glue("{sds[1]} & {sds[2]} & {sds[3]} & {sds[4]}\\\\")), summtex)
}

obs <- round(as.numeric(summ_stats_out[2,]), 0)
writeLines(c("\\hhline{-----}","$N$ (Counties)", "&", glue("{obs[1]} & {obs[2]} & {obs[3]} & {obs[4]}\\\\")), summtex)
writeLines(c("\\hhline{=====}","\\end{tabular}"), summtex)
close(summtex)

# close log ----
sink()

