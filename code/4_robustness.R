### ROBUSTNESS CHECKS (edited sep 2024)
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_4_robustness.txt", append=FALSE)
print("***************** RUNNING: 4_robustness *****************\n\n")

# initializing main datasets ----
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsampall == 1)
matched1   <- countysumm %>% filter(match_weight1 != 0 & mainsampall == 1)
matched2   <- countysumm %>% filter(match_weight2 != 0 & mainsampall == 1)
matched3   <- countysumm %>% filter(match_weight3 != 0 & mainsampall == 1) %>% mutate(weight = match_weight3)

matched1_wht   <- countysumm_wht %>% filter(match_weight1 != 0 & mainsampwht == 1)
matched2_wht   <- countysumm_wht %>% filter(match_weight2 != 0 & mainsampwht == 1)
matched3_wht   <- countysumm_wht %>% filter(match_weight3 != 0 & mainsampwht == 1) %>% mutate(weight = match_weight3)

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
countysumm_border_wht <- countysumm_wht %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
  mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))

match_datasets   <- list(matched1, matched2, matched3, countysumm_border)
match_datasets_wht   <- list(matched1_wht, matched2_wht, matched3_wht, countysumm_border_wht)

match_datanames  <- list("matched1", "matched2", "matched3", 'border')

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
for (i in 1:4){
  did_graph(dataset     = match_datasets_wht[[i]], 
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

# OUTCOME: for each sample, P[mwt]
datalist = list(link1, link2, link3)
datanames = c("Sample 1: SWT in t-10", "Sample 2: MWNILF in t-10", "Sample 3: SWNILF in t-10")
coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")
coefnames = c("0)P[married]","1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]")

for (dataind in 1:3){
  models = list(
    feols(mwt_link ~  Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
          data = datalist[[dataind]] %>% filter(neighbor_samp == 1 & RACE == 1) %>% add_did_dummies()),
    feols(mwt_link ~  Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
          data = datalist[[dataind]] %>% filter((FIPS %in% border_ctrl | FIPS %in% border_treat) & RACE == 1) %>% add_did_dummies()),
    feols(mwt_link ~  Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
          data = datalist[[dataind]] %>% filter(match_weight1 != 0 & RACE == 1) %>% add_did_dummies()),
    feols(mwt_link ~  Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
          data = datalist[[dataind]] %>% filter(match_weight2 != 0 & RACE == 1) %>% add_did_dummies()),
    feols(mwt_link ~  Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
          data = datalist[[dataind]] %>% filter(match_weight3 != 0 & RACE == 1) %>% add_did_dummies(),
          weights = ~match_weight3)
  )
  means = list(mean(filter(datalist[[dataind]], neighbor_samp == 1 & RACE == 1 & TREAT == 1 & YEAR == 1930)$mwt_link),
               mean(filter(datalist[[dataind]], (FIPS %in% border_ctrl | FIPS %in% border_treat) & RACE == 1 & TREAT == 1 & YEAR == 1930)$mwt_link),
               mean(filter(datalist[[dataind]], match_weight1 != 0 & RACE == 1 & TREAT == 1 & YEAR == 1930)$mwt_link),
               mean(filter(datalist[[dataind]], match_weight2 != 0 & RACE == 1 & TREAT == 1 & YEAR == 1930)$mwt_link),
               mean(filter(datalist[[dataind]], match_weight3 != 0 & RACE == 1 & TREAT == 1 & YEAR == 1930)$mwt_link))
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = unlist(means))) %>% print()
}

# OUTCOME: TABLE 3 (LINKED RESULTS)
datalist = list(link1, link2, link3)
datanames = c("Sample 1: SWT in t-10", "Sample 2: MWNILF in t-10", "Sample 3: SWNILF in t-10")
coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link", "swt_link", "swnt_link", "swnilf_link")
coefnames = c("0)P[married]","1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]",
              "4)P[unmarried teacher]", "5)P[unmarried nonteacher in LF]", "6)P[unmarried not in lf]")

for (dataind in 1:3){
  models = list()
  means = list()
  i = 1
  for (coefind in 1:7){
    df <- datalist[[dataind]] %>% 
      filter(match_weight2 != 0) %>%
      #filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
      filter(RACE == 1) %>%
      add_did_dummies()
    model <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
                   data = df)
    models[[i]] <- model
    means[[i]] <- mean(filter(df, YEAR == 1930 & TREAT == 1)[[coefs[coefind]]], na.rm=TRUE)
    i = i + 1 
  }
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = unlist(means))) %>% print()
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

did_graph(dataset     = countysumm_border %>% filter(mainsampall == 1),
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          fig_width    = 6,
          fig_height   = 4,
          filename    = "shareteach_border") %>% print()

link1_border <- link1 %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
  mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))


datalist = list(link1, link2, link3)
datanames = c("Sample 1: SWT in t-10", "Sample 2: MWNILF in t-10", "Sample 3: SWNILF in t-10")
coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")
coefnames = c("0)P[married]","1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]")


for (dataind in 1:3){
  models = list()
  means = list()
  i = 1
  for (coefind in 1:4){
    df <- datalist[[dataind]] %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
      mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1)) %>%
      filter(RACE == 1) %>%
      add_did_dummies()
    model <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
                   data = df)
    models[[i]] <- model
    means[[i]] <- mean(filter(df, YEAR == 1930 & TREAT == 1)[[coefs[coefind]]], na.rm=TRUE)
    i = i + 1 
  }
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = unlist(means))) %>% print()
}


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
# STATE-LEVEL SYNTH DID SETUP
balanceddata_state <- statesumm_wht %>% 
  filter(YEAR != 1900 & YEAR < 1950) %>% 
  group_by(STATEICP) %>% 
  summarize(n = n()) %>% 
  filter(n == 4)

# x <- statesumm %>% filter(STATEICP %in% balanceddata_state$STATEICP & !STATEICP %in% treat_exclude_list[[i]]) %>% 
#   mutate(treatment = ifelse(YEAR <= 1930, 0, ifelse(STATEICP %in% treat_states, 1, 0))) %>% 
#   filter(YEAR < 1950)
# write.csv(x, glue("{cleandata}/tempstatepanel.csv"))

# crosswalk from state icpsr to fips codes
fipscrosswalk <- read_xls(glue("{root}/StateFIPSicsprAB.xls"))

## Cross-Sectional Results ----
treat_states <- c(47,51)
treat_exclude_list <- list(c(), c(47), c(51))
treat_labels <- list(c("KY","NC"),"KY","NC")

# empty list for outputs (to graph)
coefs_sdid <- numeric(9)
ses_sdid_bs <- numeric(9)
ses_sdid_plac <- numeric(9)
ses_sdid_jk <- numeric(9)
coefs_wdid <- numeric(9)
ses_wdid <- numeric(9)
var_labs <- character(9)
group_labs <- character(9)
j = 1

for (i in 1:3){
  for (var in c("pct_mw_Teacher", "pct_sw_Teacher", "pct_m_Teacher")){
    var_labs[j] <- var
    group_labs[j] <- glue_collapse(treat_labels[[i]])
    # creating panel in correct format
    state_panel = panel.matrices.new(statesumm_wht %>% filter(STATEICP %in% balanceddata_state$STATEICP & !STATEICP %in% treat_exclude_list[[i]]) %>% 
                                       mutate(treatment = ifelse(YEAR <= 1930, 0, ifelse(STATEICP %in% treat_states, 1, 0))) %>% 
                                       filter(YEAR < 1950), 
                                     unit = "STATEICP", time = "YEAR", outcome = var, treatment = "treatment")
    
    # synthetic did estimates
    synthdid_est_state = synthdid_estimate(state_panel$Y, state_panel$N0, state_panel$T0)
    
    # SEs using bootstrap method
    se_bs = sqrt(vcov(synthdid_est_state, method = "bootstrap"))
    
    # SEs using placebo method
    se_plac = sqrt(vcov(synthdid_est_state, method = "placebo"))
    
    # SEs using jackknife method
    se_jk = sqrt(vcov(synthdid_est_state, method = "jackknife"))
  
    
    ## mapping unit weights
    ctrls_st = synthdid_controls(synthdid_est_state)
    ctrl_states = data.frame(STATEICP = as.numeric(rownames(ctrls_st)), weights = ctrls_st) %>% 
      rename(weights = estimate.1) %>% left_join(fipscrosswalk, by = c("STATEICP" = "STICPSR"))
    
    n = 5
    sdid_plot <- plot_usmap(data = ctrl_states %>% filter(!is.na(weights)) %>%
                              mutate(state = AB, weights = cut(weights, quantile(weights, probs = seq(0,1,1/n)))) %>%
                              select(c(state, weights)),
                            values = "weights", color = NA) +
      scale_fill_manual(values = colorRampPalette(c("white",control_col))(n)) +
      geom_sf(data = us_map("counties", include = treat_labels[[i]]), fill = treat_col, color = NA)
    
    ggsave(glue("{outfigs}/paper/sdid_graph_{glue_collapse(treat_labels[[i]])}_{var}.png"), sdid_plot, width = 8, height = 5)
    
    ## printing results
    print(synthdid_est_state)
    coefs_sdid[j] <- synthdid_est_state
    ses_sdid_plac[j] <- se_plac
    ses_sdid_jk[j] <- se_jk
    
    statedid_data <- statesumm_wht %>% mutate(FIPS = STATEICP, 
                                          TREAT = ifelse(STATEICP %in% treat_states, 1, 0)) %>%
      left_join(ctrl_states %>% select(c(STATEICP, weights))) %>%
      mutate(weight = ifelse(TREAT == 1, 0.5, weights/sum(ctrl_states$weights))) %>%
      filter(STATEICP %in% balanceddata_state$STATEICP & !STATEICP %in% treat_exclude_list[[i]] & YEAR %in% c(1930, 1940) & !is.na(weight))
    
    state_wdid <- lm(glue("{var} ~ factor(STATEICP) + factor(YEAR)*factor(TREAT)"), data = statedid_data, weights = weight)
    #x <- lm(pct_mw_Teacher ~ factor(YEAR)*factor(TREAT), data = statedid_data, weights = weight)
    print(state_wdid$coefficients[["factor(YEAR)1940:factor(TREAT)1"]])
    coefs_wdid[j] <- state_wdid$coefficients[["factor(YEAR)1940:factor(TREAT)1"]]
    # clustered SE at state level
    ses_wdid[j] <- sqrt(diag(vcovCL(state_wdid, cluster = statedid_data[["STATEICP"]], type = "HC1"))[["factor(YEAR)1940:factor(TREAT)1"]])
    
    # did_graph_data(statedid_data, "pct_mw_Teacher", years = c(1940), yearomit = 1930) %>% print()
    # did_graph(statedid_data, years = c(1940), yearomit = 1930,
    #           depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
    #           depvarnames = c("Men", "Married Women", "Single Women"),
    #           colors      = c(men_col, mw_col, sw_col)) %>% print() 
    
    j = j + 1
  }
}

var_labs_ct = character(3)
group_labs_ct = character(3)
coefs_wdid_ct = numeric(3)
ses_wdid_ct = numeric(3)

j = 1
# adding baseline county results
for (var in c("pct_mw_Teacher", "pct_sw_Teacher", "pct_m_Teacher")){
  group_labs_ct[j] <- "countydid"
  var_labs_ct[j] <- var
  coefs_wdid_ct[j] <- did_graph_data(neighbor, var, years = c(1940))["TREATx1940","y"]
  ses_wdid_ct[j] <- sqrt(did_graph_data(neighbor, var, years = c(1940))["TREATx1940","var"])
  j = j + 1
}

## GRAPHING RESULTS
sdid_graph_data <- data.frame(var = var_labs, group = group_labs, coefs_sdid = coefs_sdid,
                              ses_sdid_bs = ses_sdid_bs,
                              ses_sdid_plac = ses_sdid_plac,
                              ses_sdid_jk = ses_sdid_jk,
                              coefs_wdid = coefs_wdid,
                              ses_wdid = ses_wdid) %>%
  bind_rows(data.frame(var = var_labs_ct, group = group_labs_ct, coefs_sdid = coefs_wdid_ct, ses_sdid_plac = ses_wdid_ct)) %>%
  mutate(coefs_wdid_ub = coefs_wdid + 1.96*ses_wdid,
         coefs_wdid_lb = coefs_wdid - 1.96*ses_wdid,
         coefs_sdid_ub = coefs_sdid + 1.96*ses_sdid_plac,
         coefs_sdid_lb = coefs_sdid - 1.96*ses_sdid_plac,
         i = case_when(var == "pct_mw_Teacher" ~ 2, var == "pct_sw_Teacher" ~ 3, var == "pct_m_Teacher" ~ 1),
         group_num = case_when(group == "countydid" ~ 2 - 0.1 + (i-1)*0.1,
                               group == "KYNC" ~ 4 - 0.1 + (i-1)*0.1,
                               group == "NC" ~ 6 - 0.1 + (i-1)*0.1,
                               group == "KY" ~ 8 - 0.1 + (i-1)*0.1),
         var = case_when(var == "pct_mw_Teacher" ~ "Married Women",
                         var == "pct_m_Teacher" ~ "Men",
                         var == "pct_sw_Teacher" ~ "Single Women"))

sdid_cs_graph <- ggplot(data = sdid_graph_data, aes(x = group_num, y = coefs_sdid, color = factor(var, levels = c("Men", "Married Women", "Single Women")), 
                                                    shape = factor(var, levels = c("Men", "Married Women", "Single Women")))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = coefs_sdid_lb, max = coefs_sdid_ub), width = 0, linewidth = 4, alpha = 0.5) +
  scale_color_manual(values = c(men_col, mw_col, sw_col)) +
  scale_x_continuous(breaks=c(2,4,6,8),
                   labels=c("Original County-Level DiD", "State-Level Synthetic DiD", "State-Level SDiD (NC Only)", "State-Level SDiD (KY Only)")) +
  geom_point(size = 5) + labs(x = "", y = "Treat x 1940: Share of Teachers", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")

ggsave(glue("{outfigs}/paper/sdid_shareteach_wht.png"), sdid_cs_graph, width = 8, height = 5)


## Linked Results ----
## stargazer tables ----
coefs1        <- numeric(4)
ses1           <- numeric(4)
linkreg_means1 <- c()
coefs2        <- numeric(4)
ses2           <- numeric(4)
linkreg_means2 <- c()
coefs3        <- numeric(4)
ses3           <- numeric(4)
linkreg_means3 <- c()

i = 1
for (coefname in c("pct_mw", "pct_mwt", "pct_mwnt", "pct_mwnilf")){
  #link 1
  state_panel1 = panel.matrices.new(link1_state %>% filter(STATEICP %in% balanceddata_state$STATEICP) %>% 
                                     mutate(treatment = ifelse(YEAR <= 1930, 0, ifelse(STATEICP %in% treat_states, 1, 0))) %>% 
                                   filter(YEAR < 1950), 
                                   unit = "STATEICP", time = "YEAR", outcome = coefname, treatment = "treatment")
  synthdid_est_state1 = synthdid_estimate(state_panel1$Y, state_panel1$N0, state_panel1$T0)
  coefs1[[i]] <- synthdid_est_state1
  ses1[[i]] <- sqrt(vcov(synthdid_est_state1, method = "placebo"))
  linkreg_means1 <- c(linkreg_means1, mean(filter(link1_state, YEAR == 1930 & STATEICP %in% treat_states)[[coefname]]))

  #link 2
  state_panel2 = panel.matrices.new(link2_state %>% filter(STATEICP %in% balanceddata_state$STATEICP) %>%
                                      mutate(treatment = ifelse(YEAR <= 1930, 0, ifelse(STATEICP %in% treat_states, 1, 0))) %>%
                                      filter(YEAR < 1950),
                                    unit = "STATEICP", time = "YEAR", outcome = coefname, treatment = "treatment")
  synthdid_est_state2 = synthdid_estimate(state_panel2$Y, state_panel2$N0, state_panel2$T0)
  se2 = sqrt(vcov(synthdid_est_state2, method = "placebo"))
  coefs2[[i]] <- synthdid_est_state2
  ses2[[i]] <- se2
  linkreg_means2 <- c(linkreg_means2, mean(filter(link2_state, YEAR == 1930 & STATEICP %in% treat_states)[[coefname]]))

  #link 1
  state_panel3 = panel.matrices.new(link3_state %>% filter(STATEICP %in% balanceddata_state$STATEICP) %>% 
                                      mutate(treatment = ifelse(YEAR <= 1930, 0, ifelse(STATEICP %in% treat_states, 1, 0))) %>% 
                                      filter(YEAR < 1950), 
                                    unit = "STATEICP", time = "YEAR", outcome = coefname, treatment = "treatment")
  synthdid_est_state3 = synthdid_estimate(state_panel3$Y, state_panel3$N0, state_panel3$T0)
  coefs3[[i]] <- synthdid_est_state3
  ses3[[i]] <- sqrt(vcov(synthdid_est_state3, method = "placebo"))
  linkreg_means3 <- c(linkreg_means3, mean(filter(link3_state, YEAR == 1930 & STATEICP %in% treat_states)[[coefname]]))
  
  i = i + 1
}


#______________________________________________________
# CHECK FIVE: STATE-LEVEL CLUSTERED SEs ----
#______________________________________________________
regdata <- neighbor %>% 
  add_did_dummies() %>% 
  filter(YEAR %in% seq(1910,1950,10))

years <- c(1910, 1920, 1940, 1950)
interact_vars <- glue("TREATx{years}")
yearvars <- glue("Year{years}")

n = 15
var_labs = character(n)
year_labs = numeric(n)
coefs = numeric(n)
cf_lbs = numeric(n)
cf_ubs = numeric(n)

set.seed(100)
i = 1
for (var in c("pct_mw_Teacher","pct_sw_Teacher","pct_m_Teacher")){
  did_reg_state <- lm(glue("{var} ~ {glue_collapse(yearvars, sep = '+')} + factor(STATEICP) + 
                       {glue_collapse(interact_vars, sep = '+')}"), 
                      data = regdata)
  for (yr in years){
    btse <- boottest(did_reg_state, B = 10000, param = glue("TREATx{yr}"),
                     clustid = "STATEICP", r = 1.5)
    var_labs[i] = var
    year_labs[i] = yr
    coefs[i] = btse$point_estimate
    cf_lbs[i] = btse$conf_int[1]
    cf_ubs[i] = btse$conf_int[2]
    i = i + 1
  }
  var_labs[i] = var
  year_labs[i] = 1930
  coefs[i] = 0
  cf_lbs[i] = 0
  cf_ubs[i] = 0
  i = i + 1
}

pointspan = 2
btse_data <- data.frame(year = year_labs, var = var_labs, coefs = coefs, y_lb = cf_lbs, y_ub = cf_ubs) %>%
  mutate(i = case_when(var == "pct_mw_Teacher" ~ 2,
                       var == "pct_m_Teacher" ~ 1,
                       var == "pct_sw_Teacher" ~ 3),
         year_graph = year - pointspan/2 + (i-1),
         var = case_when(var == "pct_mw_Teacher" ~ "Married Women",
                         var == "pct_m_Teacher" ~ "Men",
                         var == "pct_sw_Teacher" ~ "Single Women"))

depvarnames = c("Men", "Married Women", "Single Women")
btse_graph <- ggplot(btse_data, aes(x = year_graph, 
                     y = coefs, 
                     color = factor(var, levels = depvarnames), 
                     shape = factor(var, levels = depvarnames))) + 
  geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
  geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
  scale_color_manual(values=c(men_col, mw_col, sw_col)) +
  annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) + 
  geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed"), color = "#656565") +
  geom_point(size = 4) + labs(x = "Year", y = "fill", color = "", shape = "") + theme_minimal() + 
  theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none") + labs(y = "DiD Estimate: Share of Teachers")
ggsave(glue("{outfigs}/paper/shareteach_neighbor_stateclus.png"), btse_graph, width = 8, height = 5)

# close log ----
sink()
