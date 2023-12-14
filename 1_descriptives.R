### INITIAL DESCRIPTIVES
### AUTHOR: AMY KIM
### LAST EDITED: MAR 17 2023

library(haven)
library(tidyverse)
library(glue)
library(sandwich)
library(lmtest)

#root = "/Users/amykim/Dropbox (Princeton)/marriagebar"
root = "/Users/carolyn/Library/CloudStorage/Dropbox/marriagebar_data"
#git = "Users/amykim/GitHub/marriagebar"
git = "/Users/carolyn/Library/CloudStorage/Dropbox/marriagebar"
rawdata = glue("{root}/ipums_raw")
outdata = glue("{root}/clean_data")
outfigs = glue("{git}/figures")

# importing data
allyears_raw <- read_csv(glue("{outdata}/allyears_occ.csv"))

# grouping by county (long on occupation)
grp_county_long <- allyears_raw %>% 
  filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% 
  summarize(num = n(),
            pct_female = sum(ifelse(SEX == 2, 1, 0))/num,
            pct_female_single = sum(ifelse(SEX == 2 & MARST == 6, 1, 0))/sum(ifelse(SEX == 2, 1, 0))) %>%
  mutate(STATEGROUP       = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21), 2, ifelse(STATEICP == 47 | STATEICP == 51, 1, 0)), # adding grouping for whether in group 1 (ky, nc) or group 2 (WV, TN, SC, OH, IN, IL, MO) neighbouring state
         STATEGROUP_SOUTH = ifelse(STATEICP %in% c(41, 42, 44, 46, 48, 54), 2, ifelse(STATEICP == 47 | STATEICP == 51, 1, 0)), # AL, Arkansas, Georgia, Mississippi, S Carolina, Tennessee (omitting Louisiana, TX)
         STATEGROUP_NBR   = ifelse(STATEICP %in% c(48, 54), 2, ifelse(STATEICP == 47 | STATEICP == 51, 1, 0)), # S Carolina, Tennessee (Southern neighbours only)
         STATEGROUP_ALL   = ifelse(STATEICP %in% c(47, 51, 11), 1, 2)) # KY, NC, DC 

# grouping by county (wide on occupation)
grp_county_wide <- grp_county_long %>% pivot_wider(id_cols = c(YEAR, 
                                                               STATEICP, 
                                                               COUNTYICP, 
                                                               STATEGROUP, 
                                                               STATEGROUP_SOUTH,
                                                               STATEGROUP_NBR,
                                                               STATEGROUP_ALL), 
                                                   names_from = OCC, 
                                                   values_from = c(num, 
                                                                   pct_female, 
                                                                   pct_female_single))

#### COUNTY-LEVEL DENSITY PLOTS #### 
## What were the overall trends of teacher demographics?

# density plot of: percentage of teachers/secretaries in a county who were female
female_plot <- ggplot(grp_county_long, aes(x = pct_female, group = factor(OCC), color = factor(OCC))) + geom_density() + facet_wrap(~YEAR)

# density plot of: percentage of female teachers/secretaries in a county who were single
singlefemale_plot <- ggplot(grp_county_long, aes(x = pct_female_single, group = factor(OCC), color = factor(OCC))) + geom_density() + facet_wrap(~YEAR)

# density plot of: percentage of female teachers in a county who were single (by State Group)
marbar_plot_group <- ggplot(grp_county_wide, aes(x = pct_female_single, group = factor(STATEGROUP), color = factor(STATEGROUP))) + geom_density() + facet_wrap(~YEAR)


### RESIDUAL DENSITY PLOTS ###
## What were the trends of teacher demographics, differencing out secretary demographics?

# Generating residuals of regression of pct female single teachers on pct female single secretaries -- separately by year
allresids <- list()
i = 0
for (year in seq(1900,1940,10)){
  i = i + 1
  datayear <- grp_county_wide %>% filter(YEAR == year & !is.na(pct_female_single_Teacher) & !is.na(pct_female_single_Secretary))

  # NOTE: not including fixed effects, since already differencing out county-level trends with secretary trends
  sec_teach <- lm(data = datayear, pct_female_single_Teacher ~ pct_female_single_Secretary)
  datayear$resids <- sec_teach$residuals
  allresids[[i]] <- datayear
}

## plotting residuals
allresids_df <- bind_rows(allresids)

# density plot of: residuals (interpretation -- positive residual means that a county had more female single teachers relative to female single secretaries, indicating more likely to have had a marriage bar)
teach_resid_plot <- ggplot(data = allresids_df, aes(x = resids)) + geom_density() + facet_wrap(~YEAR)

# density plot of: residuals by state group (interpretation -- if policy had bite, would expect to see group 1 [non marriage bar] counties had more negative residuals in 1940 than group 2 [marriage bar] counties, as compared to 1930)
teach_resid_plot_group <- ggplot(data = allresids_df %>% filter(STATEGROUP != 0), 
                                 aes(x = resids, group = factor(STATEGROUP), color = factor(STATEGROUP))) + 
  geom_density() + facet_wrap(~YEAR)

### DIFF IN DIFF ###
## Quantifying: how did teacher demographics change for group 1 relative to group 2 counties between 1930 and 1940, taking out any pre trends?

### TODO: make sure the reg omits 1930 instead of 1900
did_data <- grp_county_wide %>% 
  filter(YEAR!=1900) %>% 
  filter(STATEGROUP != 0) %>% 
  mutate(treat = ifelse(STATEGROUP == 1, 1, 0))

did_reg <- lm(pct_female_single_Teacher ~ treat*relevel(factor(YEAR), ref="1930") + pct_female_single_Secretary, 
              data = did_data)
summary(did_reg)
print(lmtest::coeftest(did_reg, type='HC0', vcov. = sandwich::vcovHC))

did_reg_plot <- did_data %>% 
  group_by(treat, YEAR) %>% 
  summarize(pct_female_single_Teacher=mean(pct_female_single_Teacher))
ggplot(did_reg_plot, aes(x = YEAR, y=pct_female_single_Teacher, group=treat, color=treat)) + 
  geom_line() + 
  geom_point()

# trying to use packages...
# did_est <- DIDparams(yname = "pct_female_single_Teacher",
#           tname = "YEAR",
#           gname = "treat", 
#           data = did_data,
#           control_group = "notyettreated",
#           true_repeated_cross_sections = TRUE)
# ggdid(did_est)
# example_attgt <- att_gt(yname = "pct_female_single_Teacher",
#                         tname = "YEAR",
#                         gname = "treat",
#                         xformla = ~1,
#                         data = did_data)
# # summarize the results
# summary(example_attgt)

### try using only the states in the crazy data collected on all Southern states
# (excluding TX and Louisiana just due to distance)
did_data <- grp_county_wide %>% 
  filter(YEAR!=1900) %>% 
  filter(STATEGROUP_SOUTH != 0) %>% 
  mutate(treat = ifelse(STATEGROUP_SOUTH == 1, 1, 0))

did_reg <- lm(pct_female_single_Teacher ~ treat*relevel(factor(YEAR), ref="1930") + pct_female_single_Secretary, 
                    data = did_data)
summary(did_reg)
print(lmtest::coeftest(did_reg, type='HC1', vcov = vcovCL, cluster=~STATEICP))
did_plot <- did_data %>% 
  group_by(treat, YEAR) %>% 
  summarize(pct_female_single_Teacher=mean(pct_female_single_Teacher))
ggplot(did_plot, aes(x = YEAR, y=pct_female_single_Teacher, group=treat, color=treat)) + 
  geom_line() + 
  geom_point()
# TO DO: Cluster on ... state? 

### try using only neighbouring states to KY and NC
did_data_nbr <- grp_county_wide %>% 
  filter(YEAR!=1900) %>% 
  filter(STATEGROUP_NBR != 0) %>% 
  mutate(treat = ifelse(STATEGROUP_NBR == 1, 1, 0))
# regression
did_reg <- lm(pct_female_single_Teacher ~ treat*relevel(factor(YEAR), ref="1930") + pct_female_single_Secretary, 
                    data = did_data_nbr)
summary(did_reg)
print(lmtest::coeftest(did_reg, type='HC1', vcov = vcovCL, cluster=~STATEICP))
# plot 
did_plot <- did_data_nbr %>% 
  group_by(treat, YEAR) %>% 
  summarize(pct_female_single_Teacher=mean(pct_female_single_Teacher))
ggplot(did_plot, aes(x = YEAR, y=pct_female_single_Teacher, group=treat, color=treat)) + 
  geom_line() + 
  geom_point()

### try using all states compared to KY, NC, DC
did_data_all <- grp_county_wide %>% 
  filter(YEAR!=1900) %>% 
  filter(STATEGROUP_ALL != 0) %>% 
  mutate(treat = ifelse(STATEGROUP_ALL == 1, 1, 0))
# regression
did_reg <- lm(pct_female_single_Teacher ~ treat*relevel(factor(YEAR), ref="1930") + pct_female_single_Secretary, 
              data = did_data_all)
summary(did_reg)
print(lmtest::coeftest(did_reg, type='HC1', vcov = vcovCL, cluster=~STATEICP))
# plot 
did_plot <- did_data_all %>% 
  group_by(treat, YEAR) %>% 
  summarize(pct_female_single_Teacher=mean(pct_female_single_Teacher, na.rm=TRUE))
ggplot(did_plot, aes(x = YEAR, y=pct_female_single_Teacher, group=treat, color=treat)) + 
  geom_line() + 
  geom_point()
