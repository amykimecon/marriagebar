### DESCRIPTIVES
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_1_descriptives.txt", append=FALSE)

#_________________________________________________
# MAP OF TREATMENT & CONTROL COUNTIES ----
#_________________________________________________
## neighbor ----
graph_treatment(countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                filename = "treatmap_neighbor") + 
                ggtitle("Neighbor Sample")

## match 1 ----
graph_treatment(countysumm %>% filter(match_weight1 != 0 & mainsamp == 1), 
                filename = "treatmap_matched1") + 
                ggtitle("Matched Sample 1")

## match 2 ----
graph_treatment(countysumm %>% filter(match_weight2 != 0 & mainsamp == 1), 
                filename = "treatmap_matched2") + 
                ggtitle("Matched Sample 2")

## match 3 ----
graph_treatment(countysumm %>% 
                  filter(match_weight3 != 0 & mainsamp == 1) %>%
                  mutate(weights = match_weight3),
                filename = "treatmap_matched3", full = TRUE) +
                ggtitle("Matched Sample 3")

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
  
#________________________________________________________________
# FIG 1: DEMOG TRENDS FOR WORKERS/TEACHERS OVER TIME ----
#________________________________________________________________
## share workers male/single fem/married fem over time ----
fig1a_demogtrends_workers <- ggplot(data = samp_byyear, 
                                    aes(x = YEAR, y = pctlf, 
                                        fill = factor(demgroup, levels = c("Men", "Married Women", "Unmarried Women")))) + 
  geom_area() +
  xlab("Year") + ylab("Fraction of US Labor Force") + labs(fill = "") + 
  scale_fill_manual(values=c(men_col, mw_col, sw_col)) + 
  theme_minimal() + theme(legend.position = "bottom") 
#slides
fig1a_demogtrends_workers + theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig1a_demogtrends_workers.png"), width = 8, height = 5) 
#paper
fig1a_demogtrends_workers + theme(text = element_text(size=12))
ggsave(filename = glue("{outfigs}/paper/fig1a_demogtrends_workers.png"), width = 6, height = 4) 

## share teachers male/single fem/married fem over time ----
fig1b_demogtrends_teachers <- ggplot(data = samp_byyear, 
                                     aes(x = YEAR, y = pctteachers, 
                                         fill = factor(demgroup, levels = c("Men", "Married Women", "Unmarried Women")))) + 
  geom_area() +
  xlab("Year") + ylab("Fraction of US Teachers") + labs(fill = "") + 
  scale_fill_manual(values=c(men_col, mw_col, sw_col)) + 
  theme_minimal() + theme(legend.position = "bottom")
#slides
fig1b_demogtrends_teachers + theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig1b_demogtrends_teachers.png"), width = 8, height = 5)
#paper
fig1b_demogtrends_teachers + theme(text = element_text(size=12))
ggsave(filename = glue("{outfigs}/paper/fig1b_demogtrends_teachers.png"), width = 6, height = 4)

## extra stats ---- 
# percentage of married women in LF that were teachers, over time
print(samp_byyear %>% filter(demgroup == "Married Women") %>% select(c(YEAR, pct_dem_teaching)))
# percentage of married women with at least some college that were teachers
print(samp_byyear %>% filter(demgroup == "Married Women") %>% select(c(YEAR, pct_coll_teachers)))

#______________________________________________________________________
# FIG 2: DISTN OF FRAC ALL TEACHERS/SEC MARRIED WOMEN ----
#______________________________________________________________________
county_means_all <- countysumm %>% 
  group_by(YEAR, TREAT) %>%
  filter(mainsamp == 1) %>%
  summarise(across(c(pct_mw_Teacher, pct_mw_Secretary), function(.x) mean(.x, na.rm=TRUE))) %>%
  mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed"))
  
## paper ----
ggplot(filter(countysumm, mainsamp == 1) %>% 
         mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")),
       aes(x = pct_mw_Teacher, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_histogram(aes(y=after_stat(density)), position = "identity", alpha = 0.3, binwidth = 0.01, linewidth = 0.2) + 
  #geom_density(alpha = 0.2) +
  geom_vline(data = county_means_all, 
             aes(xintercept = pct_mw_Teacher, color = factor(TREAT)), 
             linewidth = 0.6,linetype = "dashed") + 
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
  filter(mainsamp == 1) %>% #main sample (all counties)
  mutate(POP_THOUS            = POP/1000, 
         WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
         TEACH_PER_STUDENT    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA),
         summgroup            = "All") %>%
  rbind(countysumm %>% filter(mainsamp == 1 & SOUTH == 1) %>%  # southern counties only
          mutate(POP_THOUS            = POP/1000, 
                 WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
                 TEACH_PER_STUDENT    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA), 
                 summgroup            = "South")) %>%
  rbind(countysumm %>% filter(mainsamp == 1 & neighbor_samp == 1) %>% #neighboring & treated counties (separately)
          mutate(POP_THOUS            = POP/1000, 
                 WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
                 TEACH_PER_STUDENT    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA), 
                 summgroup            = glue("Treat{TREAT}"))) %>%
  mutate(summgroup = factor(summgroup, levels = c("All", "South", "Treat0", "Treat1")))

varnames_1930 = c("POP_THOUS","WHITESCHOOLPOP_THOUS", "URBAN", 
                  "LFP_MW", "LFP_WMW", "NCHILD", 
                  "TEACH_PER_STUDENT","pct_m_Teacher", 
                  "pct_sw_Teacher", "pct_mw_Teacher")
varlabs_1930 = c("Population (Thousands)", "White School-Age Pop. (Thous.)", "Share Urban", 
                 "LFP of Married Women", "LFP of White Married Women", "Num. Children*", 
                 "start panel here -- Teachers/Students", "Share Men", 
                 "Share Single Women", "Share Married Women")
# varnames_1940 = c("PCT_HS_GRAD","INCWAGE","AGEMARR")
# varlabs_1940 = c("Share HS Grads", "Wage Income", "Age at First Marriage")

# summary statistics by state group
summ_stats <- countysumm_stats %>%
  filter(YEAR == 1930) %>%
  group_by(summgroup) %>% 
  summarize(OBS = n(),
            across(all_of(varnames_1930), .fns = c(~mean(.x, na.rm=TRUE), #mean
                                                   ~sd(.x, na.rm=TRUE)/sqrt(OBS))))
# summ_stats_1940 <- countysumm_stats %>%
#   filter(YEAR == 1940) %>%
#   group_by(summgroup) %>% 
#   summarize(OBS = n(),
#             across(all_of(varnames_1940), .fns = c(~mean(.x, na.rm=TRUE), #mean
#                                                    ~sd(.x, na.rm=TRUE)/sqrt(OBS))))
# 
# summ_stats_all <- inner_join(summ_stats_1930,summ_stats_1940)
summ_stats_out <- as.data.frame(t(summ_stats))

summtex <- file(glue("./tables/summstats.tex"), open = "w")
names <- summ_stats_out[1,]

writeLines(c("\\begin{tabular}{lcccc}", 
             "\\hhline{=====}",
             "&", glue("{names[1]} & {names[2]} & {names[3]} & {names[4]}\\\\")), summtex)
for (i in 1:length(c(varlabs_1930))){
  means <- summ_stats_out[2*i+1,]
  sds <- paste0("(", round(as.numeric(summ_stats_out[2*i + 2,]), 3), ")")
  writeLines(c(paste(c(varlabs_1930)[i], "&", glue("{means[1]} & {means[2]} & {means[3]} & {means[4]}\\\\")),
               "&", glue("{sds[1]} & {sds[2]} & {sds[3]} & {sds[4]}\\\\")), summtex)
}

obs <- round(as.numeric(summ_stats_out[2,]), 0)
writeLines(c("Obs.", "&", glue("{obs[1]} & {obs[2]} & {obs[3]} & {obs[4]}\\\\")), summtex)
writeLines(c("\\hhline{-----}","\\end{tabular}"), summtex)
close(summtex)

# close log ----
sink()
