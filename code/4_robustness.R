### ROBUSTNESS CHECKS (edited sep 2024)
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_4_robustness.txt", append=FALSE)
print("***************** RUNNING: 4_robustness *****************\n\n")

# initializing main datasets ----
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)
matched1   <- countysumm %>% filter(match_weight1 != 0 & mainsamp == 1)
matched2   <- countysumm %>% filter(match_weight2 != 0 & mainsamp == 1)
matched3   <- countysumm %>% filter(match_weight3 != 0 & mainsamp == 1) %>% mutate(weight = match_weight3)

match_datasets   <- list(matched1, matched2, matched3)
link1_match_datasets   <- list(link1 %>% filter(match_weight1 != 0 & mainsamp == 1), 
                               link1 %>% filter(match_weight2 != 0 & mainsamp == 1), 
                               link1 %>% filter(match_weight3 != 0 & mainsamp == 1))

match_datanames  <- list("matched1", "matched2", "matched3")

#______________________________________________________
# CHECK ONE: SECRETARIES PLACEBO ----
#______________________________________________________

# OUTCOME: SHARE SECRETARIES MW/SW/M (only for neighbor sample)
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_m_Secretary", "pct_mw_Secretary", "pct_sw_Secretary"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Secretaries",
          ymin        = -0.065, 
          ymax        = 0.06,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("sharesec_neighbor"))  %>% print()


#______________________________________________________
# CHECK TWO: MATCHED COUNTIES ----
#______________________________________________________

# OUTCOME: SHARE TEACHERS MW/SW/M 
for (i in 1:3){
  did_graph(dataset     = match_datasets[[i]], 
            depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
            depvarnames = c("Men", "Married Women", "Single Women"),
            colors      = c(men_col, mw_col, sw_col),
            yvar        = "DiD Estimate: Share of Teachers",
            fig_width    = 6,
            fig_height   = 4,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("shareteach_{match_datanames[[i]]}"))  %>% print()
  Sys.sleep(2) #pause so i can see the graph output
  
}


# OUTCOME: NET LFP RESULTS FOR LINK1
for (i in 1:3){
  did_graph(dataset     = link1_match_datasets[[i]],
            depvarlist  = c("pct_lf", "pct_mw_cond_inlf", "pct_sw_cond_inlf"),
            depvarnames = c("LFP in t", "LFP in t | Married in t", "LFP in t | Unmarried in t"),
            colors      = c("darkgrey", mw_col, sw_col),
            years       = c(1920, 1940),
            fig_width    = 6,
            fig_height   = 4,
            yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("overall_effects_lfp_{match_datanames[[i]]}"))
  
}


## MAP OF TREATMENT & CONTROL COUNTIES ----

### neighbor ----
graph_treatment(countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                filename = "treatmap_neighbor") + 
  ggtitle("Neighbor Sample")

### match 1 ----
graph_treatment(countysumm %>% filter(match_weight1 != 0 & mainsamp == 1), 
                filename = "treatmap_matched1") + 
  ggtitle("Matched Sample 1")

### match 2 ----
graph_treatment(countysumm %>% filter(match_weight2 != 0 & mainsamp == 1), 
                filename = "treatmap_matched2") + 
  ggtitle("Matched Sample 2")

### match 3 ----
graph_treatment(countysumm %>%
                  filter(match_weight3 != 0 & mainsamp == 1) %>%
                  mutate(weights = match_weight3),
                filename = "treatmap_matched3", full = TRUE) +
                ggtitle("Matched Sample 3")

#______________________________________________________
# CHECK THREE: BORDER COUNTY ANALYSIS ----
#______________________________________________________
borders <- read.table(glue("{root}/county_adjacency2010.txt"), sep = "\t", col.names = c("county_name","FIPS","border_name","border_FIPS")) %>%
  mutate(county_name = ifelse(county_name == "", NA, county_name),
         FIPS = str_pad(as.character(FIPS), 5, "left", pad = "0"),
         border_FIPS = str_pad(as.character(border_FIPS), 5, "left", pad = "0")) %>%
  fill(c(county_name, FIPS), .direction = "down") %>%
  mutate(state = substr(FIPS, 1, 2),
         border_state = substr(border_FIPS, 1, 2),
         border = ifelse(state != border_state, 1, 0),
         border_ctrl = ifelse(border & border_state %in% c("37", "21"), 1, 0),
         border_treat = ifelse(border & state %in% c("37", "21"), 1, 0)) 

border_treat <- filter(borders, border_treat == 1)$FIPS
border_ctrl <- unique(filter(borders, border_ctrl == 1)$FIPS)

countysumm_border <- countysumm %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
  mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))

# countysumm_border <- countysumm %>% filter(TREAT == 1 | FIPS %in% border_ctrl) %>%
#   mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))

graph_treatment(countysumm_border, filename = "treatmap_border")

did_graph(dataset     = countysumm_border %>% filter(mainsamp == 1),
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          fig_width    = 6,
          fig_height   = 4,
          filename    = "shareteach_border") %>% print()

link1_border <- link1 %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
  mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))

did_graph(dataset     = link1_border %>% filter(mainsamp == 1), 
          depvarlist  = c("pct_lf", "pct_mw_cond_inlf", "pct_sw_cond_inlf"),
          depvarnames = c("LFP in t", "LFP in t | Married in t", "LFP in t | Unmarried in t"),
          colors      = c("darkgrey", mw_col, sw_col),
          years       = c(1920, 1940),
          fig_width = 6,
          fig_height = 4,
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          filename    = "overall_effects_lfp_border")

#______________________________________________________
# CHECK FOUR: SYNTHETIC DID ----
#______________________________________________________
# COUNTY
fips1920 <- filter(countysumm, YEAR == 1920 & NWHITETEACH >= n)$FIPS
fips1910 <- filter(countysumm, YEAR == 1910 & NWHITETEACH >= n)$FIPS

county_panel = panel.matrices.new(countysumm %>% filter(mainsamp == 1 & FIPS %in% fips1920 & FIPS %in% fips1910) %>% 
                             mutate(treatment = ifelse(YEAR <= 1930, 0, TREAT)), #%>% 
                             #filter(YEAR < 1950), 
                           unit = "FIPS", time = "YEAR", outcome = "pct_mw_Teacher", treatment = "treatment")

synthdid_est = synthdid_estimate(county_panel$Y, county_panel$N0, county_panel$T0)
plot(synthdid_est)
ctrls = synthdid_controls(synthdid_est)
ctrl_fips = rownames(ctrls)

graph_treatment(countysumm %>% left_join(data.frame(FIPS = ctrl_fips, ctrl_weights = ctrls)) %>%
                  mutate(weights = estimate.1) %>% filter(!is.na(estimate.1)), full = TRUE)

# STATE
balanceddata_state <- statesumm %>% 
  filter(YEAR != 1900) %>% 
  group_by(STATEICP) %>% 
  summarize(n = n()) %>% 
  filter(n == 5)

treat_states <- c(47,51)
treat_exclude <- c()
state_panel = panel.matrices.new(statesumm %>% filter(STATEICP %in% balanceddata_state$STATEICP & !STATEICP %in% treat_exclude) %>% 
                             mutate(treatment = ifelse(YEAR <= 1930, 0, ifelse(STATEICP %in% treat_states, 1, 0))), #%>% 
                           #filter(YEAR < 1950), 
                           unit = "STATEICP", time = "YEAR", outcome = "pct_mw_Teacher", treatment = "treatment")

synthdid_est_state = synthdid_estimate(state_panel$Y, state_panel$N0, state_panel$T0)
#plot(synthdid_est_state)

# crosswalk from state icpsr to fips codes
fipscrosswalk <- read_xls(glue("{root}/StateFIPSicsprAB.xls"))

ctrls_st = synthdid_controls(synthdid_est_state)
ctrl_states = data.frame(STATEICP = as.numeric(rownames(ctrls_st)), weights = ctrls_st) %>% 
  rename(weights = estimate.1) %>% left_join(fipscrosswalk, by = c("STATEICP" = "STICPSR"))

n = 5
plot_usmap(data = ctrl_states %>% filter(!is.na(weights)) %>%
             mutate(state = AB, weights = cut(weights, quantile(weights, probs = seq(0,1,1/n)))) %>%
             select(c(state, weights)),
           values = "weights", color = NA) +
  scale_fill_manual(values = colorRampPalette(c("white",control_col))(n))

# did with states
statedid_data <- statesumm %>% mutate(FIPS = STATEICP, 
                                 TREAT = ifelse(STATEICP %in% treat_states, 1, 0)) %>%
  left_join(ctrl_states %>% select(c(STATEICP, weights))) %>%
  mutate(weight = ifelse(TREAT == 1, 1, weights)) %>%
  filter(STATEICP %in% balanceddata_state$STATEICP)

did_graph(statedid_data,
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col))

#______________________________________________________
# CHECK FIVE: STATE-LEVEL CLUSTERED SEs ----
#______________________________________________________
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          clus = "STATEICP",
          yvar        = "DiD Estimate: Share of Teachers",
          ymin        = -0.065, 
          ymax        = 0.06,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("shareteach_neighbor_stateclus"))  %>% print()



#______________________________________________________
# CHECK SIX: BLACK TEACHERS ----
#______________________________________________________
# redoing analysis with black teachers
did_graph(dataset     = countysumm_blk %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Black Teachers",
          filename    = glue("shareteach_neighbor_blackteach"))  %>% print()

# redoing analysis with all teacher (all races)
did_graph(dataset     = countysumm_all %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of All Teachers",
          filename    = glue("shareteach_neighbor_allteach"))  %>% print()

# close log ----
sink()
