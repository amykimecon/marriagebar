# COHORT ANALYSIS
# AUTHOR: CAROLYN

## NOTE: Temporary.
## Maybe split across 0_dataclean and 2_didanalysis?

# open connection to database ----
#con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)
#for Carolyn, in case connection on Dropbox isn't working:
con <- dbConnect(duckdb(), dbdir = "C:\\Users\\ctsao\\Documents\\test_duckdb/db.duckdb", read_only=TRUE) 

# desc stats ----
# get info on age at marriage 
countysumm_marrycohort <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.) 
  filter(SEX==2 & AGEMARR>0) %>% 
  group_by(YEAR, STATEICP, COUNTYICP) %>% #grouping at the county level
  summarize(AGEMARR = mean(AGEMARR)) %>% 
  collect() %>%
  addvars_county() 
 
countysumm_marrycohort %>% group_by(YEAR, TREAT) %>% summarize(AGEMARR = mean(AGEMARR)) %>% View()

# identify main sample of counties ----
countysumm  <- read_csv(glue("{cleandata}/countysumm_new.csv")) # county X year-level stats (most analysis uses this)
# select main sample (i.e. min size of county)
mainsamp_list <- mainsamp(countysumm)

# generate eCDF and "DiD" by birth cohorts ----
cohort_cdf <- function(minyear, maxyear) {
  # construct table, limiting sample to certain birth cohorts of women
  countysumm_marrycohort <- tbl(con, "censusrawall") %>% #taking table from DuckDB
    addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.) 
    filter(SEX==2 & RACE==1) %>% 
    mutate(birthyear = YEAR - AGE) %>%
    filter(birthyear>=minyear & birthyear<=maxyear | 
             (AGE==0 & YEAR==1910)) %>% # just to add datapoints for 1910; needed for DiD helper functions
    group_by(YEAR, STATEICP, COUNTYICP) %>% #grouping at the county level
    summarize(POP             = n(), #overall population of county
              PCT_TEACH       = sum(ifelse(teacher==1,1,0))/n(), #number of white teachers
              PCT_MARR        = sum(ifelse(MARST %in% c(1,2), 1, 0))/n(), #share of cohort married
              PCT_MARRTEACH   = sum(ifelse(teacher==1,1,0)*ifelse(MARST %in% c(1,2), 1, 0))/n(), # interaction
              PCT_LIT         = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)) #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
    ) %>%
    collect() %>%
    addvars_county() %>% #adding county-level variables (treatment status, FIPS codes, etc.)
    mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0)) # keep only main counties
  
  # prep data for plots 
  ## for means
  plot_cohort <- countysumm_marrycohort %>% 
    filter(neighbor_sampNC==1 & mainsamp==1) %>% 
    group_by(YEAR, TREAT) %>% 
    summarize(PCT_MARR      = weighted.mean(PCT_MARR, POP),
              PCT_TEACH     = weighted.mean(PCT_TEACH, POP),
              PCT_MARRTEACH = weighted.mean(PCT_MARRTEACH, POP)) 
  ## for did 
  neighbor_marry   <- countysumm_marrycohort %>% 
    filter(neighbor_sampNC == 1 & mainsamp==1) 
  
  # plot: pr(married)
  ## means
  graph_out <- ggplot(data = plot_cohort, aes(x = YEAR, y = PCT_MARR, color = factor(TREAT))) +
    geom_point() +
    labs(x = "Year", y = "Cumulative Pr(Married)") +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +
    ggtitle(glue("Empirical CDF: Marriage rate among women born between {minyear}-{maxyear} (NC)")) +
    theme_minimal()
  ggsave(glue("{outfigs}/paper/ecdf_married_cohort_{minyear}_{maxyear}.png"),
         graph_out, width = 8, height = 5)
  # did 
  did_graph(dataset     = neighbor_marry, 
            depvarlist  = c("PCT_MARR"),
            depvarnames = c("Share married"),
            colors      = c(mw_col),
            yvar        = "DiD Estimate: Share cohort married",
            filename    = glue("did_ecdf_married_cohort_{minyear}_{maxyear}"))
  
  # plot: pr(teacher)
  ## means
  graph_out <- ggplot(data = plot_cohort, aes(x = YEAR, y = PCT_TEACH, color = factor(TREAT))) +
    geom_point() +
    labs(x = "Year", y = "Cumulative Pr(Teacher)") +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +
    ggtitle(glue("Empirical CDF: Pr(teacher) among women born between {minyear}-{maxyear} (NC)")) +
    theme_minimal()
  ggsave(glue("{outfigs}/paper/ecdf_teacher_cohort_{minyear}_{maxyear}.png"),
         graph_out, width = 8, height = 5)
  # did 
  did_graph(dataset     = neighbor_marry, 
            depvarlist  = c("PCT_TEACH"),
            depvarnames = c("Share teaching"),
            colors      = c(mw_col),
            yvar        = "DiD Estimate: Share cohort teaching",
            filename    = glue("did_ecdf_teacher_cohort_{minyear}_{maxyear}"))
  
  # plot: pr(teacher and married)
  ## means
  graph_out <- ggplot(data = plot_cohort, aes(x = YEAR, y = PCT_MARRTEACH, color = factor(TREAT))) +
    geom_point() +
    labs(x = "Year", y = "Cumulative Pr(Married Teacher)") +
    scale_color_manual(values = c("0" = "blue", "1" = "red")) +
    ggtitle(glue("Empirical CDF: Pr(married teacher) among women born between {minyear}-{maxyear} (NC)")) +
    theme_minimal()
  ggsave(glue("{outfigs}/paper/ecdf_marriedteacher_cohort_{minyear}_{maxyear}.png"),
         graph_out, width = 8, height = 5)
  # did 
  did_graph(dataset     = neighbor_marry, 
            depvarlist  = c("PCT_MARRTEACH"),
            depvarnames = c("Share married teachers"),
            colors      = c(mw_col),
            yvar        = "DiD Estimate: Share cohort married and teaching",
            filename    = glue("did_ecdf_marriedteacher_cohort_{minyear}_{maxyear}"))
}
## run by cohort ----
cohort_cdf(minyear = 1920, maxyear = 1922) # 11-13yo in 1933
cohort_cdf(minyear = 1915, maxyear = 1917) # 16-18yo in 1933
cohort_cdf(minyear = 1912, maxyear = 1914) # 19-21yo in 1933
cohort_cdf(minyear = 1908, maxyear = 1910) # 22-24yo in 1933


## COHORT EVENT STUDY (?) [added by amy 2/8/23] ----
cohort_range = seq(1905,1925)

# construct table by birth cohort
countysumm_allcohorts <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.) 
  filter(SEX==2 & RACE==1) %>% 
  mutate(birthyear = YEAR - AGE) %>%
  filter(birthyear>=min(cohort_range) & birthyear<=max(cohort_range) | 
           (AGE==0 & YEAR==1910)) %>% # just to add datapoints for 1910; needed for DiD helper functions
  group_by(YEAR, TREAT, birthyear) %>% #grouping at the year x birth cohort x treatment status level (NOTE: quicker way to aggregate than to first group by county then group by treatment & weight by population)
  summarize(POP             = n(), #overall population of county
            PCT_TEACH       = sum(ifelse(teacher==1,1,0))/n(), #number of white teachers
            PCT_MARR        = sum(ifelse(MARST %in% c(1,2), 1, 0))/n(), #share of cohort married
            PCT_MARRTEACH   = sum(ifelse(teacher==1,1,0)*ifelse(MARST %in% c(1,2), 1, 0))/n(), # interaction
            PCT_LIT         = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)) #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
  ) %>%
  collect() %>%
  addvars_county() %>% #adding county-level variables (treatment status, FIPS codes, etc.)
  pivot_wider(id_cols     = c(TREAT, birthyear), #pivoting wide on census year -- so each row is county x birth cohort obs
              names_from  = YEAR, 
              values_from = all_of(c("POP", "PCT_TEACH", "PCT_MARR", "PCT_MARRTEACH"))) %>%
  mutate(mainsamp = ifelse(FIPS %in% mainsamp_list, 1, 0), # keep only main counties
         PCT_TEACH_CHANGE_30_40 = PCT_TEACH_1940 - PCT_TEACH_1930,
         PCT_MARR_CHANGE_30_40 = PCT_MARR_1940 - PCT_MARR_1930,
         PCT_MARRTEACH_CHANGE_30_40 = PCT_MARRTEACH_1940 - PCT_MARRTEACH_1930
         )

# graphing
ggplot(data = countysumm_allcohorts,
       aes(x = birthyear, y = PCT_TEACH_CHANGE_30_40, color = factor(TREAT))) +
  geom_point()


