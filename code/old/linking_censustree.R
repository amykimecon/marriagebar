## LINKED ANALYSIS W/ CENSUS TREE
# sample: teachers who didnt move counties/linked to diff sex/race
linked_sample <- linked_teach %>% 
  filter(STATEICP_base == STATEICP_link & COUNTYICP_base == COUNTYICP_link & SEX_base == SEX_link & RACE_base == RACE_link) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, SEX = SEX_base, RACE = RACE_base) %>% 
  select(-c(STATEICP_link,COUNTYICP_link, SEX_link, RACE_link)) %>% mergefips()

# sample: those with at least some college in 1940 who didnt move counties/linked to diff sex/race and are linked to 1930
educ_sample <- educ_linked %>% 
  filter(STATEICP_base == STATEICP_link & COUNTYICP_base == COUNTYICP_link & SEX_base == SEX_link & RACE_base == RACE_link) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, SEX = SEX_base, RACE = RACE_base) %>% 
  select(-c(STATEICP_link,COUNTYICP_link, SEX_link, RACE_link)) %>% mergefips()

# sample: single women non-teachers age 15-33 in base year who didnt move counties/linked to diff sex/race
sw_sample <- linked_sw %>%
  filter(STATEICP_base == STATEICP_link & COUNTYICP_base == COUNTYICP_link & SEX_base == SEX_link & RACE_base == RACE_link) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, SEX = SEX_base, RACE = RACE_base) %>% 
  select(-c(STATEICP_link,COUNTYICP_link, SEX_link, RACE_link)) %>% mergefips()

# grouping by county for ES
linked_sample_county <- linked_sample %>%
  filter(teacher_base == 1 & demgroup_base == "SW") %>% #only keeping single women teachers (swt) in base year
  group_by(STATEICP, COUNTYICP, fips, YEAR_base, YEAR_link) %>%
  summarize(pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
  )

# calculating averages by year 
linked_sample_years <- linked_sample %>%
  left_join(countysumm_matched %>% select(all_of(select_vars)), by = c("STATEICP", "COUNTYICP", "YEAR_link" = "YEAR")) %>%
  filter(teacher_base == 1 & demgroup_base == "SW" & main_samp == 1) %>% #only keeping single women teachers (swt) in base year
  group_by(YEAR_base, YEAR_link, TREAT) %>%
  summarize(n = n(),
            pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
  )# %>%
  #pivot_longer(starts_with("pct"), names_to = "var", values_to = "pct")

ggplot(data = linked_sample_years, aes(x = YEAR_link, y = pct_mwt, color = factor(TREAT))) + geom_point(size = 4)
ggplot(data = linked_sample_years, aes(x = YEAR_link, y = pct_swnt, color = factor(TREAT))) + geom_point(size = 4)

# grouping by county for ES -- secretaries
linked_sample_county_sec <- linked_sample %>%
  filter(sec_base == 1 & demgroup_base == "SW") %>% #only keeping single women teachers (swt) in base year
  group_by(STATEICP, COUNTYICP, fips, YEAR_base, YEAR_link) %>%
  summarize(pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & sec_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & sec_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & sec_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & sec_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & sec_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & sec_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
  )

# grouping by county for ES -- educ sample
educ_sample_county <- educ_sample %>%
  filter(demgroup_base != "M") %>% #only keeping women
  group_by(STATEICP, COUNTYICP, fips, YEAR_base, YEAR_link) %>%
  summarize(pct_work = ) #share of college educated women who are working

# grouping by county for ES -- SW sample
sw_sample_county <- sw_sample %>% group_by(STATEICP, COUNTYICP, fips, YEAR_base, YEAR_link) %>%
  summarize(pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
            pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
            pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
            pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
            pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
            pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later sw and not in lf
            pct_lf = sum(ifelse(worker_link == 1, 1, 0))/n(),
            pct_t = sum(ifelse(teacher_link == 1, 1, 0))/n()
            )

## EVENT STUDY STUFF (copied from 2_firststage.R)
# helper function for event study graph
es_graph_data <- function(depvar, controls = "", data = es_county_linked_main, xvars = interact_vars, years = c(1920, 1940), yearomit = 1930){
  es_reg <- lm(glue("{depvar} ~ factor(YEAR) + cluster + {glue_collapse(xvars, sep = '+')} {controls}"), data = data %>% mutate(cluster = as.character(fips)))
  #data = data %>% mutate(cluster = paste0(str_pad(STATEICP,2,"left","0"), str_pad(COUNTYICP,3,"left","0"))))
  print(summary(es_reg))
  
  vcov = vcovCL(es_reg, type = "HC1")
  effects <- data.frame(y = c(sapply(xvars, function (.x) es_reg$coefficients[[.x]]), 0),
                        depvar = depvar,
                        year = c(years, yearomit),
                        var = c(sapply(xvars, function(.x) as.numeric(diag(vcov)[[.x]])), 0)) %>%
    mutate(y_ub = y + 1.96*sqrt(var),
           y_lb = y - 1.96*sqrt(var))
  return(effects)
}

#################################################
##### CLEANING DATA FOR FIRST STAGE ANALYSIS ####
#################################################
# COUNTY-LEVEL (FS) EVENT STUDY DATA
select_vars = c("STATEICP", "COUNTYICP", "YEAR", "main_samp", "STATEGROUP", "TREAT")
es_county_linked_main <- countysumm %>% select(all_of(select_vars)) %>% inner_join(linked_sample_county, by = c("STATEICP" = "STATEICP", "COUNTYICP" = "COUNTYICP", "YEAR" = "YEAR_link")) %>%
  filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)") 
es_county_linked_sec_main <- countysumm %>% select(all_of(select_vars)) %>% inner_join(linked_sample_county_sec, by = c("STATEICP" = "STATEICP", "COUNTYICP" = "COUNTYICP", "YEAR" = "YEAR_link")) %>%
  filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)") 
es_county_linked_matched <- countysumm_matched %>% select(all_of(select_vars)) %>% inner_join(linked_sample_county, by = c("STATEICP" = "STATEICP", "COUNTYICP" = "COUNTYICP", "YEAR" = "YEAR_link")) %>%
  filter(main_samp == 1) 
es_county_linked_matched_sw <- countysumm_matched %>% select(all_of(select_vars)) %>% inner_join(sw_sample_county, by = c("STATEICP" = "STATEICP", "COUNTYICP" = "COUNTYICP", "YEAR" = "YEAR_link")) %>%
  filter(main_samp == 1) 
# es_county_linked_sw <- countysumm %>% select(all_of(select_vars)) %>% inner_join(sw_sample_county, by = c("STATEICP" = "STATEICP", "COUNTYICP" = "COUNTYICP", "YEAR" = "YEAR_link")) %>%
#   filter(main_samp == 1) 

# es_county_linked_matched_agg <- es_county_linked_matched %>% group_by(YEAR, TREAT) %>% 
#   summarize(across(c(pct_mwt, pct_swt, pct_swnt, pct_swnilf), mean), n = n())
# ggplot(data = es_county_linked_matched_agg, aes(x = YEAR, y = pct_mwt, color = factor(TREAT))) + geom_point(size = 4)
# ggplot(data = es_county_linked_matched_agg, aes(x = YEAR, y = pct_swnt, color = factor(TREAT))) + geom_point(size = 4)
es_county_linked_matched_sw_agg <- es_county_linked_matched_sw %>% group_by(YEAR, TREAT) %>%
  summarize(across(c(pct_mwt, pct_swt, pct_mwnt, pct_mwnilf, pct_swnt, pct_swnilf, pct_mw, pct_lf, pct_t), mean), n = n())
ggplot(data = es_county_linked_matched_sw_agg, aes(x = YEAR, y = pct_mwt, color = factor(TREAT))) + geom_point(size = 4)
ggplot(data = es_county_linked_matched_sw_agg, aes(x = YEAR, y = pct_mwnt, color = factor(TREAT))) + geom_point(size = 4)
ggplot(data = es_county_linked_matched_sw_agg, aes(x = YEAR, y = pct_mw, color = factor(TREAT))) + geom_point(size = 4)
ggplot(data = es_county_linked_matched_sw_agg, aes(x = YEAR, y = pct_lf, color = factor(TREAT))) + geom_point(size = 4)
ggplot(data = es_county_linked_matched_sw_agg, aes(x = YEAR, y = pct_t, color = factor(TREAT))) + geom_point(size = 4)

# ADDING TREAT x YEAR INTERACTIONS
interact_vars <- c()
for (YEAR in seq(1920,1940,10)){
  if (YEAR != 1930){
    es_county_linked_main[[glue("TREATx{YEAR}")]] <- ifelse(es_county_linked_main$YEAR == YEAR, 1, 0)*ifelse(es_county_linked_main$TREAT == 1, 1, 0)
    es_county_linked_sec_main[[glue("TREATx{YEAR}")]] <- ifelse(es_county_linked_sec_main$YEAR == YEAR, 1, 0)*ifelse(es_county_linked_sec_main$TREAT == 1, 1, 0)
    es_county_linked_matched[[glue("TREATx{YEAR}")]] <- ifelse(es_county_linked_matched$YEAR == YEAR, 1, 0)*ifelse(es_county_linked_matched$TREAT == 1, 1, 0)
    es_county_linked_matched_sw[[glue("TREATx{YEAR}")]] <- ifelse(es_county_linked_matched_sw$YEAR == YEAR, 1, 0)*ifelse(es_county_linked_matched_sw$TREAT == 1, 1, 0)
    interact_vars <- c(interact_vars, glue("TREATx{YEAR}"))
  }
}

share_swt_byoutcome <- bind_rows(es_graph_data("pct_mwt"), 
                                 es_graph_data("pct_mwnt"), 
                                 es_graph_data("pct_mwnilf"),
                                 es_graph_data("pct_swt"),
                                 es_graph_data("pct_swnt"),
                                 es_graph_data("pct_swnilf")) %>% 
  mutate(Group = case_when(depvar == "pct_mwt" ~ "MW Teacher",
                           depvar == "pct_mwnt" ~ "MW Non-Teacher in LF",
                           depvar == "pct_mwnilf" ~ "MW Not in LF",
                           depvar == "pct_swt" ~ "SW Teacher",
                           depvar == "pct_swnt" ~ "SW Non-Teacher in LF",
                           TRUE ~ "SW Not in LF"),
         year = case_when(Group == "MW Teacher" ~ year - 1,
                          Group == "MW Non-Teacher in LF" ~ year - 0.6,
                          Group == "MW Not in LF" ~ year-0.2,
                          Group == "SW Teacher" ~ year + 0.2,
                          Group == "SW Non-Teacher in LF" ~ year + 0.6,
                          Group == "SW Not in LF" ~ year + 1))

ggplot(share_swt_byoutcome, aes(x = year, y = y, color = factor(Group), shape = factor(Group))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

ggsave(glue("{outfigs}/linked_swt.png"))

## MATCHED COUNTIES
share_swt_byoutcome_matched <- bind_rows(es_graph_data("pct_mwt", data = es_county_linked_matched), 
                                 es_graph_data("pct_mwnt", data = es_county_linked_matched), 
                                 es_graph_data("pct_mwnilf", data = es_county_linked_matched),
                                 es_graph_data("pct_swt", data = es_county_linked_matched),
                                 es_graph_data("pct_swnt", data = es_county_linked_matched),
                                 es_graph_data("pct_swnilf", data = es_county_linked_matched)) %>% 
  mutate(Group = case_when(depvar == "pct_mwt" ~ "MW Teacher",
                           depvar == "pct_mwnt" ~ "MW Non-Teacher in LF",
                           depvar == "pct_mwnilf" ~ "MW Not in LF",
                           depvar == "pct_swt" ~ "SW Teacher",
                           depvar == "pct_swnt" ~ "SW Non-Teacher in LF",
                           TRUE ~ "SW Not in LF"),
         year = case_when(Group == "MW Teacher" ~ year - 1,
                          Group == "MW Non-Teacher in LF" ~ year - 0.6,
                          Group == "MW Not in LF" ~ year-0.2,
                          Group == "SW Teacher" ~ year + 0.2,
                          Group == "SW Non-Teacher in LF" ~ year + 0.6,
                          Group == "SW Not in LF" ~ year + 1))

ggplot(share_swt_byoutcome_matched, aes(x = year, y = y, color = factor(Group), shape = factor(Group))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

ggsave(glue("{outfigs}/linked_swt_matched.png"))

## SECRETARY GRAPH
share_swsec_byoutcome <- bind_rows(es_graph_data("pct_mwt", data = es_county_linked_sec_main), 
                                 es_graph_data("pct_mwnt", data = es_county_linked_sec_main), 
                                 es_graph_data("pct_mwnilf", data = es_county_linked_sec_main),
                                 es_graph_data("pct_swt", data = es_county_linked_sec_main),
                                 es_graph_data("pct_swnt", data = es_county_linked_sec_main),
                                 es_graph_data("pct_swnilf", data = es_county_linked_sec_main)) %>% 
  mutate(Group = case_when(depvar == "pct_mwt" ~ "MW Secretary",
                           depvar == "pct_mwnt" ~ "MW Non-Secretary in LF",
                           depvar == "pct_mwnilf" ~ "MW Not in LF",
                           depvar == "pct_swt" ~ "SW Secretary",
                           depvar == "pct_swnt" ~ "SW Non-Secretary in LF",
                           TRUE ~ "SW Not in LF"),
         year = case_when(Group == "MW Secretary" ~ year - 1,
                          Group == "MW Non-Secretary in LF" ~ year - 0.6,
                          Group == "MW Not in LF" ~ year-0.2,
                          Group == "SW Secretary" ~ year + 0.2,
                          Group == "SW Non-Secretary in LF" ~ year + 0.6,
                          Group == "SW Not in LF" ~ year + 1))

ggplot(share_swsec_byoutcome, aes(x = year, y = y, color = factor(Group), shape = factor(Group))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

ggsave(glue("{outfigs}/linked_swsec.png"))

### SINGLE WOMEN GRAPH
share_sw_byoutcome_matched <- bind_rows(es_graph_data("pct_mwt", data = es_county_linked_matched_sw), 
                                         es_graph_data("pct_mwnt", data = es_county_linked_matched_sw), 
                                         es_graph_data("pct_mwnilf", data = es_county_linked_matched_sw),
                                         es_graph_data("pct_swt", data = es_county_linked_matched_sw),
                                         es_graph_data("pct_swnt", data = es_county_linked_matched_sw),
                                         es_graph_data("pct_swnilf", data = es_county_linked_matched_sw)) %>% 
  mutate(Group = case_when(depvar == "pct_mwt" ~ "MW Teacher",
                           depvar == "pct_mwnt" ~ "MW Non-Teacher in LF",
                           depvar == "pct_mwnilf" ~ "MW Not in LF",
                           depvar == "pct_swt" ~ "SW Teacher",
                           depvar == "pct_swnt" ~ "SW Non-Teacher in LF",
                           TRUE ~ "SW Not in LF"),
         year = case_when(Group == "MW Teacher" ~ year - 1,
                          Group == "MW Non-Teacher in LF" ~ year - 0.6,
                          Group == "MW Not in LF" ~ year-0.2,
                          Group == "SW Teacher" ~ year + 0.2,
                          Group == "SW Non-Teacher in LF" ~ year + 0.6,
                          Group == "SW Not in LF" ~ year + 1))

ggplot(share_sw_byoutcome_matched, aes(x = year, y = y, color = factor(Group), shape = factor(Group))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

ggsave(glue("{outfigs}/sw_matched.png"))

### EXPLORATORY STUFF
# todo -- decompose base teachers in linked year, what pct leave LF vs what pct stay in teaching, etc. and by demgroup
linking_summary <- linked_sample %>% filter(teacher_base == 1) %>% group_by(YEAR_link, YEAR_base, demgroup_base) %>%
  summarize(pct_stay = sum(ifelse(teacher_link == 1, 1, 0))/n(),
            pct_nilf = sum(ifelse(worker_link == 0, 1, 0))/n(),
            avg_age = mean(AGE_link))

# looking at those who leave teaching
leavers_sample <- linked_sample %>% filter(teacher_base == 1 & teacher_link == 0)


# todo -- what are most leavers doing? by demgroup? if in lf, what occ are most common? by demgroup?



# ## OLD 
# # helper function: base year < link year (default is linking to immediate following census), full is indicator for keeping all links (instead of filtering to teachers only) n_max for testing purposes
# link_census <- function(baseyear, linkyear = baseyear + 10, full = FALSE, n_max = Inf){
#   #reading in censustree links
#   links <- read_csv(glue("{rawdata}/{baseyear}_{linkyear}.csv"), n_max = n_max)
#   #reading in raw census files from IPUMS
#   rawbase <- read_csv(glue("{rawdata}/census_{baseyear}.csv"), n_max = n_max)
#   rawlink <- read_csv(glue("{rawdata}/census_{linkyear}.csv"), n_max = n_max)
#   
#   #joining
#   linkedbase <- rawbase %>% inner_join(links, by = c("HISTID"=glue("histid{baseyear}"))) #linking base year by HISTID (only keeping links)
#   linked <- rawlink %>% inner_join(linkedbase, by = c("HISTID"=glue("histid{linkyear}")), suffix = c("_link","_base")) %>% #linking link year by HISTID, adding suffixes for duplicate variables
#     mutate(BASEYEAR = baseyear, LINKYEAR = linkyear)
#   
#   if (full){
#     #saving data
#     write_csv(linked, glue("{outdata}/linked{baseyear}_{linkyear}.csv"))
#     return(linked)
#   }
#   
#   #filtering to only keep teachers
#   linked_teach <- linked %>% filter(OCC1950_base == 93 | OCC1950_link == 93)
#   write_csv(linked_teach, glue("{outdata}/linked{baseyear}_{linkyear}_teach.csv"))
#   return(linked_teach)
# }
# 
# all_linked = list()
# i = 1
# for (yr in seq(1900,1930,10)){
#   all_linked[[i]] <- link_census(yr)
#   i = i + 1
# }
# linked_full <- bind_rows(all_linked)

# 
# # 1930-1940
# links1930 <- read_csv(glue("{rawdata}/1930_1940.csv"))
# raw1930 <- read_csv(glue("{rawdata}/census_1930.csv"))
# raw1940 <- read_csv(glue("{rawdata}/census_1940.csv"))
# 
# linked1930 <- raw1930 %>% left_join(links1930, by = c("HISTID" = "histid1930"))
# linked1930_1940 <- inner_join(linked1930, raw1940, 
#                               by = c("histid1940" = "HISTID"),
#                               suffix = c("_1930","_1940"))
# write_csv(linked1930_1940, glue("{outdata}/linked1930_1940.csv"))
# 
# #linked1930_1940 <- read_csv(glue("{outdata}/linked1930_1940.csv"))
# linked1930_1940_teach <- linked1930_1940 %>% filter(OCC1950_1930 == 93 | OCC1950_1940 == 93)
# write_csv(linked1930_1940_teach, glue("{outdata}/linked1930_1940_teach.csv"))
# 
# 
# # 1920-1930
# links1920 <- read_csv(glue("{rawdata}/1920_1930.csv"))
# raw1920 <- read_csv(glue("{rawdata}/census_1920.csv"))
# raw1930 <- read_csv(glue("{rawdata}/census_1930.csv"))
# 
# linked1920 <- raw1920 %>% left_join(links1920, by = c("HISTID" = "histid1920"))
# linked1920_1930 <- inner_join(linked1920, raw1930, 
#                               by = c("histid1930" = "HISTID"),
#                               suffix = c("_1920","_1930"))
# write_csv(linked1920_1930, glue("{outdata}/linked1920_1930.csv"))
# 
# #linked1920_1930 <- read_csv(glue("{outdata}/linked1920_1930.csv"))
# linked1920_1930_teach <- linked1920_1930 %>% filter(OCC1950_1920 == 93 | OCC1950_1930 == 93)
# write_csv(linked1920_1930_teach, glue("{outdata}/linked1920_1930_teach.csv"))
# 
# #1910-1920
# links1910 <- read_csv(glue("{rawdata}/1910_1920.csv"))
# raw1910 <- read_csv(glue("{rawdata}/census_1910.csv"))
# raw1920 <- read_csv(glue("{rawdata}/census_1920.csv"))
# 
# linked1910 <- raw1910 %>% left_join(links1910, by = c("HISTID" = "histid1910"))
# linked1910_1920 <- inner_join(linked1910, raw1920, 
#                               by = c("histid1920" = "HISTID"),
#                               suffix = c("_1910","_1920"))
# write_csv(linked1910_1920, glue("{outdata}/linked1910_1920.csv"))
# 
# linked1910_1920_teach <- linked1910_1920 %>% filter(OCC1950_1910 == 93 | OCC1950_1920 == 93)
# write_csv(linked1910_1920_teach, glue("{outdata}/linked1910_1920_teach.csv"))
# 
# 
# sample1930 <- linked1930_1940_teach %>% 
#   mutate(demgroup_1930 = ) %>%
#   filter(STATEICP_1930 == STATEICP_1940 & COUNTYICP_1930 == COUNTYICP_1940 &
#            OCC1950_1930 == 93 & demgroup_1930 == "SW" & SEX_1930 == SEX_1940) %>%
#   mutate(outcome_1940 = case_when((MARST_1940 == 1 | MARST_1940 == 2) & OCC1950_1940 == 93 ~ "Married Teacher",
#                                   (MARST_1940 == 1 | MARST_1940 == 2) & OCC1950_1940 != 93 ~ "Married Non-Teacher",
#                                   OCC1950_1940 == 93 ~ "Unmarried Teacher",
#                                   TRUE ~ "Unmarried Non-Teacher"))
# 
# sample1930 %>% group_by(outcome_1940) %>% summarize(pct = n()/nrow(sample1930))
# 
# 
# sample1920 <- linked1920_1930_teach %>% 
#   mutate(demgroup_1920 = case_when(SEX_1920 == 1 ~ "M",
#                                    SEX_1920 == 2 & !(MARST_1920 == 1 | MARST_1920 == 2) ~ "SW",
#                                    TRUE ~ "MW")) %>%
#   filter(STATEICP_1920 == STATEICP_1930 & COUNTYICP_1920 == COUNTYICP_1930 &
#            OCC1950_1920 == 93 & demgroup_1920 == "SW" & SEX_1920 == SEX_1930) %>%
#   mutate(outcome_1930 = case_when((MARST_1930 == 1 | MARST_1930 == 2) & OCC1950_1930 == 93 ~ "Married Teacher",
#                                   (MARST_1930 == 1 | MARST_1930 == 2) & OCC1950_1930 != 93 ~ "Married Non-Teacher",
#                                   OCC1950_1930 == 93 ~ "Unmarried Teacher",
#                                   TRUE ~ "Unmarried Non-Teacher"))
# 
# sample1920 %>% group_by(outcome_1930) %>% summarize(pct = n()/nrow(sample1920))




