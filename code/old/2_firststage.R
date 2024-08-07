### FIRST-STAGE ANALYSIS
### AUTHOR: AMY KIM
### LAST EDITED: MAY 2023

# goal of these figures/analyses -- show more rigorously that marriage bar lifting had an effect on married women teachers
# secondary goal -- provide suggestive evidence that the influx of married women largely affected single women rather than men

# helper function for event study graph
es_graph_data <- function(depvar, controls = "", data = es_county_main, xvars = interact_vars, years = c(1910, 1920, 1940), yearomit = 1930){
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
es_county_matched <- countysumm_matched %>% filter(main_samp == 1)
es_county_main <- countysumm %>% filter(main_samp == 1 & STATEGROUP != "Untreated (Non-Neighbor)")
es_county_all <- countysumm %>% filter(main_samp == 1)

# INDIVIDUAL-LEVEL (FS) EVENT STUDY DATA -- maybe do later for appendix?
#es_indiv <- filtered_bind %>% filter()

# ADDING TREAT x YEAR INTERACTIONS
interact_vars <- c()
for (YEAR in seq(1910,1940,10)){
  if (YEAR != 1930){
    es_county_matched[[glue("TREATx{YEAR}")]] <- ifelse(es_county_matched$YEAR == YEAR, 1, 0)*ifelse(es_county_matched$TREAT == 1, 1, 0)
    es_county_main[[glue("TREATx{YEAR}")]] <- ifelse(es_county_main$YEAR == YEAR, 1, 0)*ifelse(es_county_main$TREAT == 1, 1, 0)
    es_county_all[[glue("TREATx{YEAR}")]] <- ifelse(es_county_all$YEAR == YEAR, 1, 0)*ifelse(es_county_all$TREAT == 1, 1, 0)
    interact_vars <- c(interact_vars, glue("TREATx{YEAR}"))
  }
}

##############################################################
##### FIG 3 -- SHARE FEM MARRIED: TEACHERS VS SECRETARIES ####
##############################################################
teach_sec <- bind_rows(es_graph_data("pct_female_mar_Teacher"), es_graph_data("pct_female_mar_Secretary")) %>% 
  mutate(Occupation = case_when(depvar == "pct_female_mar_Teacher" ~ "Teacher", TRUE ~ "Secretary"),
         year = ifelse(Occupation == "Teacher", as.numeric(year) + 0.05, as.numeric(year) - 0.05))

ggplot(teach_sec, aes(x = year, y = y, color = factor(Occupation, levels = c("Teacher", "Secretary")))) + 
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
  geom_vline(aes(xintercept = 1935), linetype = 3) +
  scale_color_manual(values=c(treat_col, control_col)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Treat X Year", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/fig3_es_teachsec.png"), width = 8, height = 5)

teach_sec_ctrls <- bind_rows(es_graph_data("pct_female_mar_Teacher", controls = "+ LFP_MW*factor(YEAR) + URBAN*factor(YEAR)"), 
                             es_graph_data("pct_female_mar_Secretary", controls = "+ LFP_MW*factor(YEAR) + URBAN*factor(YEAR)")) %>% 
  mutate(Occupation = case_when(depvar == "pct_female_mar_Teacher" ~ "Teacher", TRUE ~ "Secretary"),
         year = ifelse(Occupation == "Teacher", as.numeric(year) + 0.05, as.numeric(year) - 0.05))

ggplot(teach_sec_ctrls, aes(x = year, y = y, color = factor(Occupation, levels = c("Teacher", "Secretary")))) + 
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
  scale_color_manual(values=c(treat_col, control_col)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Treat X Year", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/figa2_es_teachsec_ctrls.png"), width = 8, height = 5)

teach_sec_allstates <- bind_rows(es_graph_data("pct_female_mar_Teacher", data = es_county_all), 
                             es_graph_data("pct_female_mar_Secretary", data = es_county_all)) %>% 
  mutate(Occupation = case_when(depvar == "pct_female_mar_Teacher" ~ "Teacher", TRUE ~ "Secretary"),
         year = ifelse(Occupation == "Teacher", as.numeric(year) +  0.05, as.numeric(year) - 0.05))

ggplot(teach_sec_allstates, aes(x = year, y = y, color = factor(Occupation, levels = c("Teacher", "Secretary")))) + 
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
  scale_color_manual(values=c(treat_col, control_col)) +
  geom_point() + geom_line() + labs(x = "Year", y = "Treat X Year", color = "") + theme_minimal() + theme(legend.position = "bottom")
ggsave(filename = glue("{outfigs}/figa3_es_teachsec_allstates.png"), width = 8, height = 5)
# 
# # teach numbers -- NOTE: THIS DOESNT MAKE SENSE, DO INDIVIDUAL LEVEL ES INSTEAD
# teach_nums <- bind_rows(es_graph_data("numsf_Teacher"), es_graph_data("nummf_Teacher"), es_graph_data("nummale_Teacher")) %>%
#   mutate(group = case_when(depvar == "numsf_Teacher" ~ "Single Women (#)", depvar == "nummf_Teacher" ~ "Married Women (#)", TRUE ~ "Men (#)"),
#          year = case_when(group == "Single Women" ~ as.numeric(year) - 0.05, group == "Married Women" ~ as.numeric(year) + 0.05, TRUE ~ as.numeric(year)))
# 
# ggplot(teach_nums, aes(x = year, y = y, color = factor(group))) +
#   geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
#   geom_point() + geom_line() + labs(x = "Year", y = "Treat X Year", color = "") + theme_minimal() + theme(legend.position = "bottom")
# #ggsave(filename = glue("{outfigs}/fig3_es_teachsec.png"), width = 8, height = 5)
# 
# # number of children
# teach_sec_children <- bind_rows(es_graph_data("NCHLT5_MW_Teacher"), es_graph_data("NCHLT5_MW_Secretary")) %>% 
#   mutate(Occupation = case_when(depvar == "NCHLT5_MW_Teacher" ~ "Teacher", TRUE ~ "Secretary"),
#          year = ifelse(Occupation == "Teacher", as.numeric(year) + 0.05, as.numeric(year) - 0.05))
# 
# ggplot(teach_sec_children, aes(x = year, y = y, color = factor(Occupation, levels = c("Teacher", "Secretary")))) + 
#   geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
#   scale_color_manual(values=c(treat_col, control_col)) +
#   geom_point() + geom_line() + labs(x = "Year", y = "Treat X Year", color = "") + theme_minimal() + theme(legend.position = "bottom")

##############################################################
##### SHARE SINGLE, MARRIED BEFORE, MARRIED AFTER ####
##############################################################
share_teach_bymar <- bind_rows(es_graph_data("share_mar_fem_Teacher", 
                                             data = es_county_main %>% filter(nummf_agemarr_Teacher != 0),
                                             years = c(1940), xvars = c("TREATx1940")),
                               es_graph_data("share_mar_before_Teacher", 
                                             data = es_county_main %>% filter(nummf_agemarr_Teacher != 0),
                                             years = c(1940), xvars = c("TREATx1940")), 
                                  es_graph_data("share_mar_after_Teacher", 
                                                data = es_county_main %>% filter(nummf_agemarr_Teacher != 0),
                                                years = c(1940), xvars = c("TREATx1940"))) %>% 
  mutate(xcat = case_when(depvar == "share_mar_fem_Teacher" ~ 1,
                           depvar == "share_mar_before_Teacher" ~ 2,
                           TRUE ~ 3)) %>% filter(year == 1940)
  
fig5_share_teach_bymar <- ggplot(share_teach_bymar, aes(x = xcat, y = y)) +
  ylim(-0.015,0.06) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1,2,3), labels = c("1"= "Share Teachers \n Married Women", 
                                                       "2" = "Share Married Before", 
                                                       "3" = "Share Married After"), limits = c(0.5,3.5)) +
  guides(linewidth = "none", alpha = "none")

fig5_share_teach_bymar + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig5_share_teach_bymar.png"), width = 8, height = 5)

# first point only
fig5_share_teach_bymar1 <- ggplot(share_teach_bymar %>% filter(depvar == "share_mar_fem_Teacher"), aes(x = xcat, y = y)) +
  ylim(-0.015,0.06) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1), labels = c("1"= "Share Teachers \n Married Women"), limits = c(0.5,3.5)) +
  guides(linewidth = "none", alpha = "none")

fig5_share_teach_bymar1 + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig5_share_teach_bymar1.png"), width = 8, height = 5)

# first two points only
fig5_share_teach_bymar2 <- ggplot(share_teach_bymar %>% filter(depvar != "share_mar_after_Teacher"), aes(x = xcat, y = y)) +
  ylim(-0.015,0.06) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1,2), labels = c("1"= "Share Teachers \n Married Women",
                                                     "2" = "Share Married Before"), limits = c(0.5,3.5)) +
  guides(linewidth = "none", alpha = "none")

fig5_share_teach_bymar2 + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig5_share_teach_bymar2.png"), width = 8, height = 5)

## MATCHED COUNTIES
share_teach_bymar_matched <- bind_rows(es_graph_data("share_mar_fem_Teacher", 
                                                     data = es_county_matched %>% filter(nummf_agemarr_Teacher != 0),
                                                     years = c(1940), xvars = c("TREATx1940")),
                                   es_graph_data("share_mar_before_Teacher", 
                                                 data = es_county_matched %>% filter(nummf_agemarr_Teacher != 0),
                                                 years = c(1940), xvars = c("TREATx1940")),
                                   es_graph_data("share_mar_after_Teacher", 
                                                 data = es_county_matched %>% filter(nummf_agemarr_Teacher != 0),
                                                 years = c(1940), xvars = c("TREATx1940"))) %>% 
  mutate(xcat = case_when(depvar == "share_mar_fem_Teacher" ~ 1,
                          depvar == "share_mar_before_Teacher" ~ 2,
                          TRUE ~ 3)) %>% filter(year == 1940)

fig5_share_teach_bymar_matched <- ggplot(share_teach_bymar_matched, aes(x = xcat, y = y)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) + ylim(-0.01,0.065) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1,2,3), labels = c("1"= "Share Teachers \n Married Women", 
                                                       "2" = "Share Married Before", 
                                                       "3" = "Share Married After"), limits = c(0.5,3.5)) +
  guides(linewidth = "none", alpha = "none")

fig5_share_teach_bymar_matched + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig5_share_teach_bymar_matched.png"), width = 8, height = 5)

# first point only
fig5_share_teach_bymar_matched1 <- ggplot(share_teach_bymar_matched %>% filter(depvar == "share_mar_fem_Teacher"), aes(x = xcat, y = y)) +
  ylim(-0.01,0.065) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1), labels = c("1"= "Share Teachers \n Married Women"), limits = c(0.5,3.5)) +
  guides(linewidth = "none", alpha = "none")

fig5_share_teach_bymar_matched1 + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig5_share_teach_bymar_matched1.png"), width = 8, height = 5)

# first two points only
fig5_share_teach_bymar_matched2 <- ggplot(share_teach_bymar_matched %>% filter(depvar != "share_mar_after_Teacher"), aes(x = xcat, y = y)) +
  ylim(-0.01,0.065) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1,2), labels = c("1"= "Share Teachers \n Married Women",
                                                     "2" = "Share Married Before"), limits = c(0.5,3.5)) +
  guides(linewidth = "none", alpha = "none")

fig5_share_teach_bymar_matched2 + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig5_share_teach_bymar_matched2.png"), width = 8, height = 5)

##################################
##### SHARE TEACH BY DEMGROUP ####
##################################
share_teach_bygroup <- bind_rows(es_graph_data("share_male_Teacher"), 
                               es_graph_data("share_single_fem_Teacher"), 
                               es_graph_data("share_mar_fem_Teacher")) %>% 
  mutate(Group = case_when(depvar == "share_male_Teacher" ~ "Men",
                           depvar == "share_single_fem_Teacher" ~ "Single Women",
                           TRUE ~ "Married Women"),
         year = case_when(Group == "Men" ~ year - 0.6,
                          Group == "Single Women" ~ year + 0.6,
                          Group == "Married Women" ~ year))

fig4_es_shareteach <- ggplot(share_teach_bygroup, aes(x = year, y = y, color = factor(Group, levels = c("Men", "Married Women", "Single Women")), 
                                                      shape = factor(Group, levels = c("Men", "Married Women", "Single Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  scale_color_manual(values=c(men_col, mw_col, sw_col)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

# paper
fig4_es_shareteach
ggsave(filename = glue("{outfigs}/fig4_es_shareteach.png"), width = 8, height = 5)

# slides
fig4_es_shareteach + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach.png"), width = 8, height = 5)

ggplot(share_teach_bygroup %>% filter(Group == "Married Women"), aes(x = year, y = y, color = Group, shape = Group)) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05, color = Group)) +
  geom_point(size = 4) + scale_color_manual(values = mw_col) + scale_shape_manual(values = 17) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 18), axis.text = element_text(size = 14)) +
  guides(linewidth = "none", alpha = "none")
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach_mw.png"), width = 8, height = 5)

ggplot(share_teach_bygroup %>% filter(Group != "Single Women"), aes(x = year, y = y, color = factor(Group, levels = c("Men", "Married Women")), 
                                                                    shape = factor(Group, levels = c("Men", "Married Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05, color = factor(Group, levels = c("Men", "Married Women")))) +
  geom_point(size = 4) + scale_color_manual(values = c(men_col,mw_col)) + scale_shape_manual(values = c(16,17)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 18), axis.text = element_text(size = 14)) +
  guides(linewidth = "none", alpha = "none")
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach_mw+men.png"), width = 8, height = 5)


# secretaries
share_sec_bygroup <- bind_rows(es_graph_data("share_male_Secretary"), 
                             es_graph_data("share_single_fem_Secretary"), 
                             es_graph_data("share_mar_fem_Secretary")) %>% 
  mutate(Group = case_when(depvar == "share_male_Secretary" ~ "Men",
                           depvar == "share_single_fem_Secretary" ~ "Single Women",
                           TRUE ~ "Married Women"),
         year = case_when(Group == "Men" ~ year - 0.6,
                          Group == "Single Women" ~ year + 0.6,
                          Group == "Married Women" ~ year ))

figa4_es_sharesec <- ggplot(share_sec_bygroup, aes(x = year, y = y, color = factor(Group, levels = c("Men", "Married Women", "Single Women")), 
                                shape = factor(Group, levels = c("Men", "Married Women", "Single Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  scale_color_manual(values=c(men_col, mw_col, sw_col)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

# paper 
figa4_es_sharesec
ggsave(filename = glue("{outfigs}/figa4_es_sharesec.png"), width = 8, height = 5)

# slides
figa4_es_sharesec + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/figa4_es_sharesec.png"), width = 8, height = 5)


## Teachers: all states
share_teach_bygroup_allstates <- bind_rows(es_graph_data("share_male_Teacher", data = es_county_all), 
                                 es_graph_data("share_single_fem_Teacher", data = es_county_all), 
                                 es_graph_data("share_mar_fem_Teacher", data = es_county_all)) %>% 
  mutate(Group = case_when(depvar == "share_male_Teacher" ~ "Men",
                           depvar == "share_single_fem_Teacher" ~ "Single Women",
                           TRUE ~ "Married Women"),
         year = case_when(Group == "Men" ~ year - 0.6,
                          Group == "Single Women" ~ year + 0.6,
                          Group == "Married Women" ~ year))

fig4_es_shareteach_allstates <- ggplot(share_teach_bygroup_allstates, aes(x = year, y = y, color = factor(Group, levels = c("Men", "Married Women", "Single Women")), 
                                                      shape = factor(Group, levels = c("Men", "Married Women", "Single Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  scale_color_manual(values=c(men_col, mw_col, sw_col)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

# paper
fig4_es_shareteach_allstates
ggsave(filename = glue("{outfigs}/fig4_es_shareteach_allstates.png"), width = 8, height = 5)

# slides
fig4_es_shareteach_allstates + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach_allstates.png"), width = 8, height = 5)

## Teachers: matched sample
share_teach_bygroup_matched <- bind_rows(es_graph_data("share_male_Teacher", data = es_county_matched), 
                                           es_graph_data("share_single_fem_Teacher", data = es_county_matched), 
                                           es_graph_data("share_mar_fem_Teacher", data = es_county_matched)) %>% 
  mutate(Group = case_when(depvar == "share_male_Teacher" ~ "Men",
                           depvar == "share_single_fem_Teacher" ~ "Single Women",
                           TRUE ~ "Married Women"),
         year = case_when(Group == "Men" ~ year - 0.6,
                          Group == "Single Women" ~ year + 0.6,
                          Group == "Married Women" ~ year))

fig4_es_shareteach_matched <- ggplot(share_teach_bygroup_matched, aes(x = year, y = y, color = factor(Group, levels = c("Men", "Married Women", "Single Women")), 
                                                                          shape = factor(Group, levels = c("Men", "Married Women", "Single Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  scale_color_manual(values=c(men_col, mw_col, sw_col)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

# paper
fig4_es_shareteach_matched
ggsave(filename = glue("{outfigs}/fig4_es_shareteach_matched.png"), width = 8, height = 5)

# slides
fig4_es_shareteach_matched + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach_matched.png"), width = 8, height = 5)

ggplot(share_teach_bygroup_matched %>% filter(Group == "Married Women"), aes(x = year, y = y, color = Group, shape = Group)) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05, color = Group)) +
  geom_point(size = 4) + scale_color_manual(values = mw_col) + scale_shape_manual(values = 17) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 18), axis.text = element_text(size = 14)) +
  guides(linewidth = "none", alpha = "none")
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach_matched_mw.png"), width = 8, height = 5)

ggplot(share_teach_bygroup_matched %>% filter(Group != "Single Women"), aes(x = year, y = y, color = factor(Group, levels = c("Men", "Married Women")), 
                                                                    shape = factor(Group, levels = c("Men", "Married Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05, color = factor(Group, levels = c("Men", "Married Women")))) +
  geom_point(size = 4) + scale_color_manual(values = c(men_col,mw_col)) + scale_shape_manual(values = c(16,17)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
  ylim(-0.08,0.08) +
  labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom", text = element_text(size = 18), axis.text = element_text(size = 14)) +
  guides(linewidth = "none", alpha = "none")
ggsave(filename = glue("{outfigs}/slides/fig4_es_shareteach_matched_mw+men.png"), width = 8, height = 5)



##########################
##### OTHER DID PLOTS ####
##########################
occscore_sp <- es_graph_data("OCCSCORE_MW_SP_Teacher") #, es_graph_data("NCHILD",years=c(1940),xvars=c("TREATx1940")))
slidefig_occscore_sp <- ggplot(occscore_sp, aes(x = year, y = y)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  guides(linewidth = "none", alpha = "none")
slidefig_occscore_sp + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/slidefig_occscore_sp.png"), width = 8, height = 5)

nchild <- bind_rows(es_graph_data("NCHILD_MW_Teacher",years=c(1940),xvars=c("TREATx1940")), 
                    es_graph_data("NCHLT5_MW_Teacher",years=c(1940),xvars=c("TREATx1940"))) %>%
  mutate(xvar = ifelse(depvar == "NCHILD_MW_Teacher", 1, 2)) %>% filter(year == 1940)

slidefig_nchild <- ggplot(nchild, aes(x = xvar, y = y)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X 1940", color = "", shape = "") + theme_minimal() + 
  scale_x_continuous("", breaks = c(1,2), labels = c("1"= "# Children of MW Teachers", 
                                                       "2" = "# Children Under 5 of MW Teachers"), limits = c(0.5,2.5)) +
  guides(linewidth = "none", alpha = "none")

slidefig_nchild + theme(text = element_text(size = 18), axis.text = element_text(size = 14))
ggsave(filename = glue("{outfigs}/slides/slidefig_nchild.png"), width = 8, height = 5)

### NUMBER OF TEACHERS
num_teach <- es_graph_data("num_Teacher")
teacher_ratio <- es_graph_data("teacher_ratio", data = es_county_main %>% mutate(teacher_ratio = num_Teacher/WHITESCHOOLPOP) %>% filter(WHITESCHOOLPOP > 0))

ggplot(num_teach, aes(x = year, y = y)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  guides(linewidth = "none", alpha = "none")

ggplot(teacher_ratio, aes(x = year, y = y)) +
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.8, alpha = 0.05), color = mw_col) +
  geom_point(size = 5, shape = 17, color = mw_col) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
  guides(linewidth = "none", alpha = "none")

# ###############################################################################
# ##### BREAKDOWN OF MARRIED WOMEN IN 1940 INTO MARRIED BEFORE/MARRIED AFTER ####
# ###############################################################################
# marr_before <- es_county_main %>% filter(YEAR == 1940) %>%
#   group_by(TREAT) %>%
#   summarize(share_mar_before_Teacher = mean(share_mar_before_Teacher, na.rm=TRUE))
# 
# ggplot(es_county_main %>% filter(YEAR == 1940), aes(x = share_mar_before_Teacher)) + geom_histogram() + facet_wrap(~TREAT)
# 

# ################################
# ##### NUM TEACH BY DEMGROUP ####
# ################################
# num_teach_bygroup <- bind_rows(es_graph_data("nummale_Teacher", controls = "+ WHITESCHOOLPOP*factor(YEAR)"), 
#                                es_graph_data("numsf_Teacher", controls = "+ WHITESCHOOLPOP*factor(YEAR)"), 
#                                es_graph_data("nummf_Teacher", controls = "+ WHITESCHOOLPOP*factor(YEAR)")) %>% 
#   mutate(Group = case_when(depvar == "nummale_Teacher" ~ "Men",
#                            depvar == "numsf_Teacher" ~ "Single Women",
#                            TRUE ~ "Married Women"))
# ggplot(num_teach_bygroup, aes(x = year, y = y, color = Group)) + 
#   geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
#   geom_point() + geom_line()
# 
# num_sec_bygroup <- bind_rows(es_graph_data("nummale_Secretary"), 
#                              es_graph_data("numsf_Secretary"), 
#                              es_graph_data("nummf_Secretary")) %>% 
#   mutate(Group = case_when(depvar == "nummale_Secretary" ~ "Men",
#                            depvar == "numsf_Secretary" ~ "Single Women",
#                            TRUE ~ "Married Women"))
# ggplot(num_sec_bygroup, aes(x = year, y = y, color = Group)) + 
#   geom_errorbar(aes(min = y_lb, max = y_ub, width = 0.5)) +
#   geom_point() + geom_line()


