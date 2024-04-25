### COUNTY-LEVEL VARIATION: ROBUSTNESS CHECK
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_6_countyvaranalysis.log", append=FALSE)
print("***************** RUNNING: 6_countyvaranalysis *****************\n\n")


#____________________________________________________________
# DESCRIPTIVES ----
#_____________________________________________________________
countyvar_desc <- countysumm %>% filter(YEAR == 1910) %>% 
  mutate(treat_grp = ntile(pct_mw_Teacher, 10)) %>%
  select(c(FIPS, treat_grp)) %>%
  right_join(countysumm) %>%
  group_by(YEAR, treat_grp) %>%
  summarize(across(c(pct_mw_Teacher, pct_sw_Teacher, pct_m_Teacher), ~ mean(.x, na.rm=TRUE)))

ggplot(countyvar_desc, aes(x = YEAR, y = pct_mw_Teacher, color = factor(treat_grp))) + geom_point() 

#____________________________________________________________
# IDENTIFYING TREAT+CTRL COUNTIES IN TREATED STATES ----
#_____________________________________________________________
#only keeping main sample counties in treated states
countysumm_treated <- countysumm %>% filter(TREAT == 1 & mainsamp == 1) 

#cutoff percentiles for treatment (1930 share teachers MW)
treat_pctl_low = 0.3
treat_cutoff_low = quantile(filter(countysumm_treated, YEAR == 1910)$pct_mw_Teacher, probs = treat_pctl_low)

print(glue("Treatment cutoff: Counties in NC/KY with fewer than {round(treat_cutoff_low,4)*100}% married women teachers in 1930 are considered 'treated'"))

treat_pctl_high = 1 - treat_pctl_low
treat_cutoff_high = quantile(filter(countysumm_treated, YEAR == 1910)$pct_mw_Teacher, probs = treat_pctl_high)

print(glue("Control cutoff: Counties in NC/KY with more than {round(treat_cutoff_high,4)*100}% married women teachers in 1930 are considered 'control'"))

treat_pctl_mean_low = 0.2
treat_cutoff_mean_low = quantile(summarize(group_by(filter(countysumm_treated, YEAR <= 1930), FIPS), mean_pct_mw_Teacher = mean(pct_mw_Teacher))$mean_pct_mw_Teacher, probs = treat_pctl_mean_low)

print(glue("Treatment cutoff: Counties in NC/KY with more than {round(treat_cutoff_mean_low,4)*100}% married women teachers from 1910-1930 on average are considered 'treated'"))

treat_pctl_mean_high = 1 - treat_pctl_mean_low
treat_cutoff_mean_high = quantile(summarize(group_by(filter(countysumm_treated, YEAR <= 1930), FIPS), mean_pct_mw_Teacher = mean(pct_mw_Teacher))$mean_pct_mw_Teacher, probs = treat_pctl_mean_high)

print(glue("Treatment cutoff: Counties in NC/KY with more than {round(treat_cutoff_mean_high,4)*100}% married women teachers from 1910-1930 on average are considered 'treated'"))

#labelling relevant counties
treated_counties <- filter(countysumm_treated, YEAR == 1910 & pct_mw_Teacher < treat_cutoff_low)$FIPS
control_counties <- filter(countysumm_treated, YEAR == 1910 & pct_mw_Teacher >= treat_cutoff_high)$FIPS
countyvar <- countysumm_treated %>% mutate(TREAT = case_when(FIPS %in% treated_counties ~ 1,
                                                             FIPS %in% control_counties ~ 0,
                                                             TRUE ~ NA_integer_))

# version using mean from 1910-1930
treated_counties_mean <- filter(summarize(group_by(filter(countysumm_treated, YEAR <= 1930), FIPS), mean_pct_mw_Teacher = mean(pct_mw_Teacher)), mean_pct_mw_Teacher < treat_cutoff_mean_low)$FIPS
control_counties_mean <- filter(summarize(group_by(filter(countysumm_treated, YEAR <= 1930), FIPS), mean_pct_mw_Teacher = mean(pct_mw_Teacher)), mean_pct_mw_Teacher >= treat_cutoff_mean_high)$FIPS
countyvar_mean <- countysumm_treated %>% mutate(TREAT = case_when(FIPS %in% treated_counties_mean ~ 1,
                                                             FIPS %in% control_counties_mean ~ 0,
                                                             TRUE ~ NA_integer_))

# version where separate by urban/rural (urban have more marriage bars, so treated)
treated_urban <- filter(countysumm_treated, YEAR == 1930 & URBAN > 0)$FIPS
countyvar_urb <- countysumm_treated %>% mutate(TREAT = ifelse(FIPS %in% treated_urban, 1, 0))

#____________________________________________________________
# DID ANALYSIS ----
#_____________________________________________________________

did_graph(dataset     = countyvar,
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          verbose     = FALSE #set to true to see regression coefficients at the very end of output stream
          ) 

did_graph(dataset     = countyvar_mean,
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          verbose     = FALSE #set to true to see regression coefficients at the very end of output stream
) 

did_graph(dataset     = countyvar_urb,
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          verbose     = FALSE #set to true to see regression coefficients at the very end of output stream
)




