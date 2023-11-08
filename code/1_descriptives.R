### INITIAL DESCRIPTIVES
### AUTHOR: AMY KIM
### LAST EDITED: MAY 2023

#################################################################
##### SLIDES FIGURE: MAP OF TREATMENT & CONTROL COUNTIES ########
#################################################################
surr_states <- c("NC", "KY","VA", "SC", "TN", "WV", "OH", "IN", "IL", "MO", "GA")
plot_usmap(regions = "states", include = surr_states, 
           data = data.frame(state = surr_states, 
                             treat = c(rep("Treated", 2), rep("Control", 4), rep("Excluded", length(surr_states) - 6))),
           values = "treat") +
  scale_fill_manual(values=c("#89DCB3", "grey", treat_col)) + labs(fill = "") +
  theme(text = element_text(size = 16))
  
ggsave(filename = glue("{outfigs}/slides/treatmentmap.png"), width = 8, height = 5) 

## US MAPS
# county wide
countywide <- countysumm %>% select(c(fips, share_mar_fem_Teacher, YEAR)) %>%
  pivot_wider(id_cols = fips, names_from = YEAR, values_from = share_mar_fem_Teacher, values_fill = NA) 

#full sample
plot_usmap(data = filter(countysumm) %>% select(c(fips, share_mar_fem_Teacher, YEAR)),
           values = "share_mar_fem_Teacher", color = NA) + 
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 0.5)) + facet_wrap(~YEAR) + theme(legend.position = "right")

plot_usmap(data = filter(countysumm, YEAR == 1930 | YEAR == 1940) %>% select(c(fips, share_mar_fem_Teacher, YEAR)),
           values = "share_mar_fem_Teacher", color = NA, include = c("NC","SC")) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 0.5)) + theme(legend.position = "right") + facet_wrap(~YEAR)

plot_usmap(data = countysumm %>% group_by(fips) %>% select(c(fips, share_mar_fem_Teacher, YEAR)),
           values = "share_mar_fem_Teacher", color = NA, include = c("VA","SC","TN")) +
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 0.5)) + theme(legend.position = "right") + facet_wrap(~YEAR)

#sample line
plot_usmap(data = filter(countysumm_allyears) %>% select(c(FIPS, pct_teachers_mw, YEAR)) %>% rename(fips = FIPS),
           values = "pct_teachers_mw", color = NA) + 
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 1), oob = scales::squish) + facet_wrap(~YEAR)

#sample line
plot_usmap(data = filter(countysumm_allyears) %>% select(c(FIPS, pct_workers_mw, YEAR)) %>% rename(fips = FIPS),
           values = "pct_workers_mw", color = NA) + 
  scale_fill_gradient(low = "blue", high = "red", limits = c(0, 0.26), oob = scales::squish) + facet_wrap(~YEAR)



#################################################################
##### FIGURE 1: DEMOG TRENDS FOR WORKERS/TEACHERS OVER TIME #####
#################################################################
# share workers male/single fem/married fem over time
fig1a_demogtrends_workers <- ggplot(data = samp_byyear, aes(x = YEAR, y = pctlf, fill = factor(demgroup, levels = c("Men", "Married Women", "Unmarried Women")))) + geom_area() +
  xlab("Year") + ylab("Fraction of US Labor Force") + labs(fill = "") + scale_fill_manual(values=c(men_col, mw_col, sw_col)) + 
  theme_minimal() + theme(legend.position = "bottom") 

#slides
fig1a_demogtrends_workers + theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig1a_demogtrends_workers.png"), width = 8, height = 5) 

#paper
fig1a_demogtrends_workers + theme(text = element_text(size=12))
ggsave(filename = glue("{outfigs}/fig1a_demogtrends_workers.png"), width = 6, height = 4) 

# share teachers male/single fem/married fem over time
fig1b_demogtrends_teachers <- ggplot(data = samp_byyear, aes(x = YEAR, y = pctteachers, fill = factor(demgroup, levels = c("Men", "Married Women", "Unmarried Women")))) + geom_area() +
  xlab("Year") + ylab("Fraction of US Teachers") + labs(fill = "") + scale_fill_manual(values=c(men_col, mw_col, sw_col)) + 
  theme_minimal() + theme(legend.position = "bottom")

#slides
fig1b_demogtrends_teachers + theme(text = element_text(size=18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig1b_demogtrends_teachers.png"), width = 8, height = 5)

#paper
fig1b_demogtrends_teachers + theme(text = element_text(size=12))
ggsave(filename = glue("{outfigs}/fig1b_demogtrends_teachers.png"), width = 6, height = 4)


# ggarrange(pctworkers, pctteachers, common.legend = TRUE, legend = "bottom")
# ggsave(filename = glue("{outfigs}/fig1_demogtrends.png"), width = 15, height = 5)

# extra descriptive -- percentage of married women in LF that were teachers, over time
print(samp_byyear %>% filter(demgroup == "Married Women") %>% select(c(YEAR, pct_dem_teaching)))
# percentage of married women with at least some college that were teachers
print(samp_byyear %>% filter(demgroup == "Married Women") %>% select(c(YEAR, pct_coll_teachers)))

#################################################################
##### FIGURE 2: DISTRIBUTION OF FRAC WOMEN TEACHERS MARRIED #####
#################################################################
county_means <- countydist_byocc %>% group_by(YEAR, OCC, TREAT) %>%
  filter(main_samp == 1) %>%
  summarise(pct_female_mar_mean = mean(pct_female_mar, na.rm=TRUE),
            pct_female_mar_median = median(pct_female_mar, na.rm=TRUE),
            se_mar = sd(pct_female_mar, na.rm=TRUE)/sqrt(n())) %>%
  mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed"))

# paper (teachers)
ggplot(countydist_byocc %>% filter(OCC == "Teacher" & main_samp == 1) %>% mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")), 
       aes(x = pct_female_mar, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_density(alpha = 0.2, linewidth = 0.8) + ylim(0,11) + geom_vline(data = county_means %>% filter(OCC == "Teacher"), 
                                           aes(xintercept = pct_female_mar_mean, color = factor(TREAT)), linewidth = 0.6,
                                           linetype = "dashed") + scale_color_manual(values=c(control_col, treat_col)) +
  scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
  facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Teachers as Fraction of Women Teachers", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/fig2_marteach_dist.png"), width = 8, height = 6)

# paper (secretaries)
ggplot(countydist_byocc %>% filter(OCC == "Secretary" & main_samp == 1 & (YEAR != 1910 | num >= 10)) %>% mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")), 
       aes(x = pct_female_mar, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_density(alpha = 0.2, linewidth = 0.8) + ylim(0,8) + geom_vline(data = county_means %>% filter(OCC == "Secretary"), 
                                           aes(xintercept = pct_female_mar_mean, color = factor(TREAT)), linewidth = 0.6,
                                           linetype = "dashed") + scale_color_manual(values=c(control_col, treat_col)) +
  scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
  facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Secretaries as Fraction of Women Secretaries", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/figa1_marsec_dist.png"), width = 8, height = 6)

########################################################################
##### FIGURE 2 V2: DISTRIBUTION OF FRAC ALL TEACHERS MARRIED WOMEN #####
########################################################################
county_means_all <- countydist_byocc %>% group_by(YEAR, OCC, TREAT) %>%
  filter(main_samp == 1) %>%
  summarise(share_mar_fem_mean = mean(share_mar_fem, na.rm=TRUE),
            share_mar_fem_median = median(share_mar_fem, na.rm=TRUE),
            se_mar = sd(share_mar_fem, na.rm=TRUE)/sqrt(n())) %>%
  mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed"))

# slides (teachers)
ggplot(countydist_byocc %>% filter(OCC == "Teacher" & main_samp == 1) %>% mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")), 
       aes(x = share_mar_fem, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_density(alpha = 0.2, linewidth = 0.8) + ylim(0,14) + geom_vline(data = county_means_all %>% filter(OCC == "Teacher"), 
                                                                       aes(xintercept = share_mar_fem_mean, color = factor(TREAT)), linewidth = 0.6,
                                                                       linetype = "dashed") + scale_color_manual(values=c(control_col, treat_col)) +
  scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
  facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Teachers as Fraction of Women Teachers", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/slides/fig2_marteach_dist.png"), width = 8, height = 6)

# slides (secretaries)
ggplot(countydist_byocc %>% filter(OCC == "Secretary" & main_samp == 1 & (YEAR != 1910 | num >= 10)) %>% mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")), 
       aes(x = share_mar_fem, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_density(alpha = 0.2, linewidth = 0.8) + ylim(0,15) + geom_vline(data = county_means_all %>% filter(OCC == "Secretary"), 
                                                                      aes(xintercept = share_mar_fem_mean, color = factor(TREAT)), linewidth = 0.6,
                                                                      linetype = "dashed") + scale_color_manual(values=c(control_col, treat_col)) +
  scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
  facet_wrap(~YEAR) + labs(y = "Density", x = "Married Women Secretaries as Fraction of Women Secretaries", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/slides/figa1_marsec_dist.png"), width = 8, height = 6)

# slides (teachers by year)
for (yr in seq(1910,1940,10)){
  ggplot(countydist_byocc %>% filter(OCC == "Teacher" & main_samp == 1 & YEAR == yr) %>% mutate(TREAT = ifelse(TREAT == 1, "Marriage Bar Removed", "Marriage Bar Not Removed")), 
         aes(x = share_mar_fem, color = factor(TREAT), fill = factor(TREAT))) + 
    geom_density(alpha = 0.2, linewidth = 0.8) + ylim(0,14) + geom_vline(data = county_means_all %>% filter(OCC == "Teacher" & YEAR == yr), 
                                                                         aes(xintercept = share_mar_fem_mean, color = factor(TREAT)), linewidth = 0.6,
                                                                         linetype = "dashed") + scale_color_manual(values=c(control_col, treat_col)) +
    scale_fill_manual(values=c(control_col, treat_col), guide = "none") +
    labs(y = "Density", x = "Share of Married Women Teachers", color = "", title = yr) + 
    theme_minimal() + theme(legend.position = "bottom", text = element_text(size = 18), 
                            plot.title = element_text(hjust = 0.5, vjust = -5), axis.text = element_text(size = 14))
  ggsave(filename = glue("{outfigs}/slides/fig2_marteach_dist_{yr}.png"), width = 10, height = 6)
}

##################################################
##### TABLE 1: SUMMARY STATS BY COUNTY GROUP #####
##################################################
countysumm_stats <- countysumm %>%
  filter(main_samp == 1) %>% #main sample
  mutate(POP_THOUS = POP/1000, WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, TEACH_PER_STUDENT = ifelse(WHITESCHOOLPOP != 0, num_Teacher/WHITESCHOOLPOP, NA),
         summgroup = "All") %>%
  rbind(countysumm %>% filter(main_samp == 1 & SOUTH == 1) %>%  
          mutate(POP_THOUS = POP/1000, WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, TEACH_PER_STUDENT = ifelse(WHITESCHOOLPOP != 0, num_Teacher/WHITESCHOOLPOP, NA), summgroup = "South")) %>%
  rbind(countysumm %>% filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)") %>%
          mutate(POP_THOUS = POP/1000, WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, TEACH_PER_STUDENT = ifelse(WHITESCHOOLPOP != 0, num_Teacher/WHITESCHOOLPOP, NA), summgroup = STATEGROUP)) %>%
  mutate(summgroup = factor(summgroup, levels = c("All", "South", "Untreated (Neighbor)", "Treated")))
  
varnames_1930 = c("POP_THOUS","WHITESCHOOLPOP_THOUS", "URBAN","LFP_MW", "NCHILD", "NCHLT5", "TEACH_PER_STUDENT" ,"age_Teacher","share_male_Teacher", "share_single_fem_Teacher", "share_mar_fem_Teacher")
varlabs_1930 = c("Population (Thousands)", "White School-Age Pop. (Thous.)", "Share Urban", "LFP of Married Women", "Num. Children*", "Num. Children Under 5*", "start panel here -- Teachers/Students", "Age", "Share Men", "Share Single Women", "Share Married Women")

varnames_1940 = c("PCT_HS_GRAD","INCWAGE","AGEMARR")
varlabs_1940 = c("Share HS Grads", "Wage Income", "Age at First Marriage")
             
# summary statistics by state group
summ_stats_1930 <- countysumm_stats %>%
  filter(YEAR == 1930) %>%
  group_by(summgroup) %>% 
  summarize(OBS = n(),
            across(all_of(varnames_1930), .fns = c(~mean(.x, na.rm=TRUE), #mean
                                           ~sd(.x, na.rm=TRUE)/sqrt(OBS))))

summ_stats_1940 <- countysumm_stats %>%
  filter(YEAR == 1940) %>%
  group_by(summgroup) %>% 
  summarize(OBS = n(),
            across(all_of(varnames_1940), .fns = c(~mean(.x, na.rm=TRUE), #mean
                                                   ~sd(.x, na.rm=TRUE)/sqrt(OBS))))

summ_stats_all <- inner_join(summ_stats_1930,summ_stats_1940)
summ_stats_out <- as.data.frame(t(summ_stats_all))

summtex <- file(glue("{git}/tables/summstats.tex"), open = "w")
names <- summ_stats_out[1,]

writeLines(c("\\begin{tabular}{lcccc}", 
             "\\hhline{=====}",
             "&", glue("{names[1]} & {names[2]} & {names[3]} & {names[4]}\\\\")), summtex)
for (i in 1:length(c(varlabs_1930, varlabs_1940))){
  means <- summ_stats_out[2*i+1,]
  sds <- paste0("(", round(as.numeric(summ_stats_out[2*i + 2,]), 3), ")")
  writeLines(c(paste(c(varlabs_1930, varlabs_1940)[i], "&", glue("{means[1]} & {means[2]} & {means[3]} & {means[4]}\\\\")),
               "&", glue("{sds[1]} & {sds[2]} & {sds[3]} & {sds[4]}\\\\")), summtex)
}

obs <- round(as.numeric(summ_stats_out[2,]), 0)
writeLines(c("Obs. (Thousands)", "&", glue("{obs[1]} & {obs[2]} & {obs[3]} & {obs[4]}\\\\")), summtex)
writeLines(c("\\hhline{-----}","\\end{tabular}"), summtex)
close(summtex)




# 
# 
# # importing data
# allyears_raw <- read_csv(glue("{outdata}/allyears_occ.csv"))
# 
# # grouping by county (long on occupation)
# grp_county_long <- allyears_raw %>% 
#   filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
#   group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% 
#   summarise(num = n(),
#             pct_female = sum(ifelse(SEX == 2, 1, 0))/num,
#             pct_female_single = sum(ifelse(SEX == 2 & MARST == 6, 1, 0))/sum(ifelse(SEX == 2, 1, 0)),
#             pct_female_mar = sum(ifelse(SEX == 2 & MARST != 6, 1, 0))/sum(ifelse(SEX == 2, 1, 0)),
#             share_male = 1-pct_female,
#             share_single_female = pct_female_single*pct_female,
#             share_married_female = (1-pct_female_single)*pct_female) %>%
#   mutate(STATEGROUP = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, "Treated", "Not Treated"),
#          SOUTH = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98), 1, 0)) # adding grouping for whether in group 1 (ky, nc) or group 2 (WV, TN, SC, OH, IN, IL, MO) southern state
# 
# # grouping by county (wide on occupation)
# grp_county_wide <- grp_county_long %>% pivot_wider(id_cols = c(YEAR, STATEICP, COUNTYICP, STATEGROUP), names_from = OCC, values_from = c(num, pct_female, pct_female_single, share_male, share_single_female, share_married_female))
# 
# #### COUNTY-LEVEL DENSITY PLOTS #### 
# ## What were the overall trends of teacher demographics?
# 
# # density plot of: percentage of teachers/secretaries in a county who were female
# female_plot <- ggplot(grp_county_long, aes(x = pct_female, group = factor(OCC), color = factor(OCC))) + geom_density() + facet_wrap(~YEAR)
# 
# # density plot of: percentage of female teachers/secretaries in a county who were married
# marfemale_plot <- ggplot(grp_county_long %>% filter(OCC == "Teacher" & YEAR == 1940), aes(x = pct_female_single, group = factor(OCC), color = factor(OCC))) + geom_density()
# 
# # density plot of: percentage of female teachers in a county who were single (by State Group)
# marbar_plot_group <- ggplot(grp_county_wide, aes(x = pct_female_single_Teacher, group = factor(STATEGROUP), color = factor(STATEGROUP))) + geom_density() + facet_wrap(~YEAR)
# 
# 
# ### RESIDUAL DENSITY PLOTS ###
# ## What were the trends of teacher demographics, differencing out secretary demographics?
# 
# # Generating residuals of regression of pct female single teachers on pct female single secretaries -- separately by year
# allresids <- list()
# i = 0
# for (year in seq(1900,1940,10)){
#   i = i + 1
#   datayear <- grp_county_wide %>% filter(YEAR == year & !is.na(pct_female_single_Teacher) & !is.na(pct_female_single_Secretary))
# 
#   # NOTE: not including fixed effects, since already differencing out county-level trends with secretary trends
#   sec_teach <- lm(data = datayear, pct_female_single_Teacher ~ pct_female_single_Secretary)
#   datayear$resids <- sec_teach$residuals
#   allresids[[i]] <- datayear
# }
# 
# ## plotting residuals
# allresids_df <- bind_rows(allresids)
# 
# # density plot of: residuals (interpretation -- positive residual means that a county had more female single teachers relative to female single secretaries, indicating more likely to have had a marriage bar)
# teach_resid_plot <- ggplot(data = allresids_df, aes(x = resids)) + geom_density() + facet_wrap(~YEAR)
# 
# # density plot of: residuals by state group (interpretation -- if policy had bite, would expect to see group 1 [non marriage bar] counties had more negative residuals in 1940 than group 2 [marriage bar] counties, as compared to 1930)
# teach_resid_plot_group <- ggplot(data = allresids_df %>% filter(STATEGROUP != 0), 
#                                  aes(x = resids, group = factor(STATEGROUP), color = factor(STATEGROUP))) + 
#   geom_density() + facet_wrap(~YEAR)
# 
# ### DIFF IN DIFF ###
# ## Quantifying: how did teacher demographics change for group 1 relative to group 2 counties between 1930 and 1940, taking out any pre trends?
# 
# # TODO: make sure the reg omits 1930 instead of 1900
# did_data_teach <- allyears_raw %>%
#   filter(OCC == "Teacher" & SEX == 2 & MARST != 3 & MARST != 4 & MARST != 5) %>% # filtering out female teachers who are not widowed/divorced, only keeping southern/neighboring states for now
#   mutate(married = ifelse(MARST == 1 | MARST == 2, 1, 0),
#          treat = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, 1, 0),
#          treatx1900 = ifelse(treat & YEAR == 1900, 1, 0),
#          treatx1910 = ifelse(treat & YEAR == 1910, 1, 0),
#          treatx1920 = ifelse(treat & YEAR == 1920, 1, 0),
#          treatx1940 = ifelse(treat & YEAR == 1940, 1, 0))
# 
# # & STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98)
# did_data_sec <- allyears_raw %>%
#   filter(OCC == "Secretary" & SEX == 2 & MARST != 3 & MARST != 4 & MARST != 5) %>% # filtering out female teachers who are not widowed/divorced
#   mutate(married = ifelse(MARST == 1 | MARST == 2, 1, 0),
#          treat = ifelse(STATEICP == 47 | STATEICP == 51 | STATEICP == 98, 1, 0),
#          treatx1900 = ifelse(treat & YEAR == 1900, 1, 0),
#          treatx1910 = ifelse(treat & YEAR == 1910, 1, 0),
#          treatx1920 = ifelse(treat & YEAR == 1920, 1, 0),
#          treatx1940 = ifelse(treat & YEAR == 1940, 1, 0))
# 
# did_data_grp <- grp_county_wide %>%
#   mutate(treat = ifelse(STATEGROUP == "Treated", 1, 0),
#          treatx1900 = ifelse(treat & YEAR == 1900, 1, 0),
#          treatx1910 = ifelse(treat & YEAR == 1910, 1, 0),
#          treatx1920 = ifelse(treat & YEAR == 1920, 1, 0),
#          treatx1940 = ifelse(treat & YEAR == 1940, 1, 0))
# 
# did_reg <- lm(married ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_teach )
# did_reg_sec <- lm(married ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_sec)
# did_reg_filt <- lm(married ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_teach %>% filter(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98)))
# did_reg_sec_filt <- lm(married ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_sec %>% filter(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21, 47, 51, 98)))
# # 
# # did_reg_grp <- lm(pct_female_single_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_grp)
# # did_reg_sec_grp <- lm(pct_female_single_Secretary ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_grp)
# 
# did_reg_grp_male <- lm(share_male_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_grp)
# did_reg_grp_sf <- lm(share_single_female_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_grp)
# did_reg_grp_mf <- lm(share_married_female_Teacher ~ factor(STATEICP) + factor(YEAR) + treatx1900 + treatx1910 + treatx1920 + treatx1940, data = did_data_grp)
# 
# effects <- data.frame(y_teach = c(did_reg$coefficients[["treatx1900"]],
#              did_reg$coefficients[["treatx1910"]],
#              did_reg$coefficients[["treatx1920"]],
#              0,
#              did_reg$coefficients[["treatx1940"]]),
#              year = seq(1900,1940,10),
#              var_teach = c(diag(vcov(did_reg))[["treatx1900"]],
#                         diag(vcov(did_reg))[["treatx1910"]],
#                         diag(vcov(did_reg))[["treatx1920"]],
#                         0,
#                         diag(vcov(did_reg))[["treatx1940"]]),
#              y_sec = c(did_reg_sec$coefficients[["treatx1900"]],
#                                            did_reg_sec$coefficients[["treatx1910"]],
#                                            did_reg_sec$coefficients[["treatx1920"]],
#                                            0,
#                                            did_reg_sec$coefficients[["treatx1940"]]),
#                                      var_sec = c(diag(vcov(did_reg_sec))[["treatx1900"]],
#                                              diag(vcov(did_reg_sec))[["treatx1910"]],
#                                              diag(vcov(did_reg_sec))[["treatx1920"]],
#                                              0,
#                                              diag(vcov(did_reg_sec))[["treatx1940"]])) %>%
#   pivot_longer(cols = -year, names_to = c(".value","occup"), names_pattern = "(.*)_(.*)")
# 
# effects_filt <- data.frame(y_teach = c(did_reg_filt$coefficients[["treatx1900"]],
#                                   did_reg_filt$coefficients[["treatx1910"]],
#                                   did_reg_filt$coefficients[["treatx1920"]],
#                                   0,
#                                   did_reg_filt$coefficients[["treatx1940"]]),
#                       year = seq(1900,1940,10),
#                       var_teach = c(diag(vcov(did_reg_filt))[["treatx1900"]],
#                                     diag(vcov(did_reg_filt))[["treatx1910"]],
#                                     diag(vcov(did_reg_filt))[["treatx1920"]],
#                                     0,
#                                     diag(vcov(did_reg_filt))[["treatx1940"]]),
#                       y_sec = c(did_reg_sec_filt$coefficients[["treatx1900"]],
#                                 did_reg_sec_filt$coefficients[["treatx1910"]],
#                                 did_reg_sec_filt$coefficients[["treatx1920"]],
#                                 0,
#                                 did_reg_sec_filt$coefficients[["treatx1940"]]),
#                       var_sec = c(diag(vcov(did_reg_sec_filt))[["treatx1900"]],
#                                   diag(vcov(did_reg_sec_filt))[["treatx1910"]],
#                                   diag(vcov(did_reg_sec_filt))[["treatx1920"]],
#                                   0,
#                                   diag(vcov(did_reg_sec_filt))[["treatx1940"]]))%>%
#   pivot_longer(cols = -year, names_to = c(".value","occup"), names_pattern = "(.*)_(.*)")
# 
# 
# effects_grp <- data.frame(y_male = c(did_reg_grp_male$coefficients[["treatx1900"]],
#                                   did_reg_grp_male$coefficients[["treatx1910"]],
#                                   did_reg_grp_male$coefficients[["treatx1920"]],
#                                   0,
#                                   did_reg_grp_male$coefficients[["treatx1940"]]),
#                       year = seq(1900,1940,10),
#                       var_male = c(diag(vcov(did_reg_grp_male))[["treatx1900"]],
#                                     diag(vcov(did_reg_grp_male))[["treatx1910"]],
#                                     diag(vcov(did_reg_grp_male))[["treatx1920"]],
#                                     0,
#                                     diag(vcov(did_reg_grp_male))[["treatx1940"]]),
#                       y_sf = c(did_reg_grp_sf$coefficients[["treatx1900"]],
#                                  did_reg_grp_sf$coefficients[["treatx1910"]],
#                                  did_reg_grp_sf$coefficients[["treatx1920"]],
#                                  0,
#                                  did_reg_grp_sf$coefficients[["treatx1940"]]),
#                       var_sf = c(diag(vcov(did_reg_grp_sf))[["treatx1900"]],
#                                    diag(vcov(did_reg_grp_sf))[["treatx1910"]],
#                                    diag(vcov(did_reg_grp_sf))[["treatx1920"]],
#                                    0,
#                                    diag(vcov(did_reg_grp_sf))[["treatx1940"]]),
#                       y_mf = c(did_reg_grp_mf$coefficients[["treatx1900"]],
#                                  did_reg_grp_mf$coefficients[["treatx1910"]],
#                                  did_reg_grp_mf$coefficients[["treatx1920"]],
#                                  0,
#                                  did_reg_grp_mf$coefficients[["treatx1940"]]),
#                       var_mf = c(diag(vcov(did_reg_grp_mf))[["treatx1900"]],
#                                    diag(vcov(did_reg_grp_mf))[["treatx1910"]],
#                                    diag(vcov(did_reg_grp_mf))[["treatx1920"]],
#                                    0,
#                                    diag(vcov(did_reg_grp_mf))[["treatx1940"]]))%>%
#   pivot_longer(cols = -year, names_to = c(".value","dem_grp"), names_pattern = "(.*)_(.*)")
# 
# 
# # graphs
# did_graph <- ggplot(effects, aes(x = year, y = y, group = occup, color = occup)) + 
#   geom_errorbar(aes(min = y - 1.96*sqrt(var), max = y + 1.96*sqrt(var), width = 1)) +
#   geom_point() + geom_line() + 
#   ylab("Treated x Year on Probability Female Teacher Married") + xlab("Year")
# 
# did_graph_filt <- ggplot(effects_filt, aes(x = year, y = y, group = occup, color = occup)) + 
#   geom_errorbar(aes(min = y - 1.96*sqrt(var), max = y + 1.96*sqrt(var), width = 1)) +
#   geom_point() + geom_line() + 
#   ylab("Treated x Year on Probability Female Teacher Married") + xlab("Year")
# 
# did_graph_grp <- ggplot(effects_grp, aes(x = year, y = y, group = dem_grp, color = dem_grp)) +
#   geom_errorbar(aes(min = y - 1.96*sqrt(var), max = y + 1.96*sqrt(var), width = 1)) +
#   geom_point() + geom_line() +
#   ylab("Treated x Year on Share of Teachers in Group") + xlab("Year")
# 
# 
# ggsave(glue("{outfigs}/did_graph_filt.png"), did_graph_filt)
# 
# 
# 



