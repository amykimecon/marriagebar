### CREATING DiD PLOTS (edited sep 2024)
### (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_2_didanalysis.txt", append=FALSE)
print("***************** RUNNING: 2_didanalysis *****************\n\n")

# initializing main dataset ----
# countysumm: cross-sectional data
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsampall == 1)
neighbor_blk   <- countysumm_blk %>% filter(neighbor_samp == 1 & mainsampblk == 1)
neighbor_wht   <- countysumm_wht %>% filter(neighbor_samp == 1 & mainsampwht == 1)

#______________________________________________________
# RESULT 1: DIRECT EFFECT ON MARRIED WOMEN ----
#______________________________________________________
# # OUTCOME: P(teacher|MW) (only for neighbor sample)
# did_graph(dataset     = neighbor, 
#           depvarlist  = c("pct_mw_Teacher"),
#           depvarnames = c("Married Women"),
#           colors      = c(mw_col),
#           yvar        = "DiD Estimate: Share of Teachers",
#           pointtypes = c(17),
#           ymin = -0.015, ymax = 0.06,
#           filename = "shareteachmw_neighbor")

## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (df in list(neighbor, neighbor_wht, neighbor_blk)){
  for (coefname in c("pct_mw_Teacher", "pct_Teacher_mw_100")){
    out_did        <- did_graph_data(df, coefname, table = TRUE) #returns list of [model, cov matrix]
    models[[i]]    <- out_did[[1]]
    ses[[i]]       <- sqrt(diag(out_did[[2]]))
    sharereg_means <- c(sharereg_means, mean(filter(df, YEAR == 1930 & TREAT == 1)[[coefname]]))
    i = i + 1
  }
}

# gen table
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950"), #omit = c("Constant","cluster*", "factor*"), 
          out = glue("./tables/mwregs.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          column.labels = c("Share Teach Mar. Wom.", "MW Teach/100 MW"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 2: MECHANISMS FOR MARRIED WOMEN ----
#______________________________________________________

## writing regression output key info to df
datalist = list(link1, link2, link3)
datanames = c("Sample 1: SWT in t-10", "Sample 2: MWNILF in t-10", "Sample 3: SWNILF in t-10")
coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")
coefnames = c("0)P[married]","1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]")


for (dataind in 1:3){
  models = list()
  means = list()
  i = 1
  for (coefind in 1:4){
    model <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
                   data = datalist[[dataind]] %>%
                     filter(neighbor_samp == 1 & RACE == 1) %>%
                     add_did_dummies())
    models[[i]] <- model
    means[[i]] <- mean(filter(datalist[[dataind]], YEAR == 1930 & TREAT == 1 & RACE == 1)[[coefs[coefind]]], na.rm=TRUE)
    i = i + 1 
  }
  n = nrow(filter(datalist[[dataind]], neighbor_samp == 1 & RACE == 1 & YEAR == 1930 & TREAT == 1))
  print(glue("n in treat x 1930 x white: {n}"))
  est = models[[2]]$coeftable['TREATx1940','Estimate']
  print(glue("estimate: {est}"))
  print(glue("effect size: {est * n}"))
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = unlist(means))) %>% print()
}

## APPENDIX TABLE: GROUPS 2.5 and 3.5
datalist = list(link2point5, link3point5)
datanames = c("Sample 4: MWNT in t-10", "Sample 5: SWNNT in t-10")
coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")
coefnames = c("0) P[Married]", "1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]")

# prepare data 
for (dataind in 1:2){
  models = list()
  means = list()
  i = 1
  for (coefind in 1:4){
    models[[i]] <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
                   data = datalist[[dataind]] %>%
                     filter(neighbor_samp == 1 & RACE == 1) %>%
                     add_did_dummies())
    means[[i]] <- mean(filter(datalist[[dataind]], YEAR == 1930 & TREAT == 1)[[coefs[coefind]]], na.rm=TRUE)
    i = i + 1 
  }
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = unlist(means))) %>% print()

}

## APPENDIX TABLE 2: EXPLORING RESULTS FOR SAMPLES 2 + 3 -> P[MWNT]
datalist = list(link2,
                link3)

for (dataind in 1:2){
  df = datalist[[dataind]]%>% filter(neighbor_samp == 1 & RACE == 1) %>%
    add_did_dummies() %>%
    left_join(countysumm_wht %>% retailsales() %>% select(YEAR, STATEICP, COUNTYICP, share_manuf, share_ag, UNEMP_RATE, unemp_rate, POP, RRTSAP29, RRTSAP39),
              by = c("YEAR","STATEICP","COUNTYICP")) %>%
    mutate(rrtsap = case_when(YEAR == 1930 ~ RRTSAP29,
                              YEAR == 1940 ~ RRTSAP39,
                              TRUE ~ NA_real_)) %>%
    add_tva_ind()
  
  models = list(feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
                      data = df),
                feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
                      weights = ~1/nlink,
                      data = df),
                feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 + share_manuf + share_ag + POP | STATEICP^COUNTYICP,
                            data = df),
                feols(mwnt_link ~ Year1940+TREATx1940 + unemp_rate + rrtsap | STATEICP^COUNTYICP,
                      data = df %>% filter(YEAR >= 1930)),
                feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
                      data = df %>% filter(tva == 0)),
                feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
                      data = df %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl)),
                feols(mwnt_link ~ factor(URBAN) + Year1940*factor(URBAN)+TREATx1940*factor(URBAN) | STATEICP^COUNTYICP,
                      data = df ),
                feols(mwnt_link ~ AGE + Year1940*AGE+TREATx1940*AGE | STATEICP^COUNTYICP,
                      data = df)
  )
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2")) %>% print()
  
}

# 1. unweighted (same as col 3 of table 3), 2. inv weighted, 3. industry + pop controls, 4. unemp controls (note that sample smaller bc no EMPSTAT in 1920 census),
# 5. rem tva, 6. interact w urban, 7. interact w age

#______________________________________________________
# RESULT 3: DIRECT EFFECT ON MEN/UNMARRIED WOMEN ----
#______________________________________________________
# OUTCOME: NUMBER OF TEACHERS
did_graph(dataset     = neighbor_wht, 
          depvarlist  = c("num_Teacher"), 
          depvarnames = c("Number of Teachers"),
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Number of Teachers",
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("numteach_neighbor"))  %>% print()

# OUTCOME: SHARE TEACHERS MW/SW/M 
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("shareteach_neighbor"))  %>% print()

did_graph(dataset     = neighbor_wht, 
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of White Teachers",
          ymin        = -0.1, 
          ymax        = 0.1,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("shareteach_neighbor_wht"))  %>% print()

did_graph(dataset     = neighbor_blk, 
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Black Teachers",
          ymin        = -0.1, 
          ymax        = 0.1,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("shareteach_neighbor_blk"))  %>% print()


## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "num_Teacher")){
  out_did        <- did_graph_data(neighbor_wht, coefname, table = TRUE) #returns list of [model, cov matrix]
  models[[i]]    <- out_did[[1]]
  ses[[i]]       <- sqrt(diag(out_did[[2]]))
  sharereg_means <- c(sharereg_means, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}
# gen table
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950"),#omit = c("Constant","cluster*", "factor*"), 
          out = glue("./tables/shareregs.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          dep.var.labels.include = FALSE,
          column.labels = c("\\% Teach Mar. Wom.", "\\% Teach Men", 
                            "\\% Teach Unmar. Wom.","\\# Teachers"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
          table.layout = "=lc#-t-as=")



#______________________________________________________
# RESULT 4: MECHANISMS FOR UNMARRIED WOMEN DECREASE ----
#______________________________________________________

## writing regression output key info to df
datalist = list(link1, link3, link3point5)
datanames = c("Sample 1: SWT in t-10", "Sample 3: SWNILF in t-10", "Sample 5: SWNT in t-10")
coefs = c("unmarried_link", "swt_link", "swnt_link", "swnilf_link")
coefnames = c("0) P[Unmarried]", "1) P[Unmarried Teacher]", "2) P[Unmarried Non-Teacher in LF]", "3) P[Unmarried Not in LF]")


# prepare data 
for (dataind in 1:3){
  models = list()
  means = list()
  i = 1
  for (coefind in 1:4){
    models[[i]] <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 +unemp_rate| STATEICP^COUNTYICP")),
                         data = datalist[[dataind]] %>%
                           left_join(countysumm_wht %>% select(YEAR, STATEICP, COUNTYICP, share_manuf, share_ag, unemp_rate, POP),
                                     by = c("YEAR","STATEICP","COUNTYICP")) %>%
                           filter(neighbor_samp == 1 & RACE == 1) %>%
                           #add_tva_ind() %>% filter(tva == 0) %>%
                           add_did_dummies())
    means[[i]] <- mean(filter(datalist[[dataind]], YEAR == 1930 & TREAT == 1)[[coefs[coefind]]], na.rm=TRUE)
    i = i + 1 
  }
  esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = unlist(means))) %>% print()
  
}

#______________________________________________________
# RESULT 5: NET EFFECTS ----
#______________________________________________________
netdf <- link2 %>% filter(neighbor_samp == 1 & RACE == 1) %>% add_did_dummies()
models = list(
  feols(ch ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
        data = netdf %>% mutate(ch = ifelse(NCHILD_link > 0, 1, 0))),
  feols(OCCSCORE_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
        data = netdf),
  feols(OCCSCORE_link ~ married_link + married_link*Year1920+married_link*Year1940+married_link*TREATx1920+married_link*TREATx1940 | STATEICP^COUNTYICP,
        data = netdf),
  feols(worker_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
        data = netdf),
  feols(worker_link ~ married_link + married_link*Year1920+married_link*Year1940+married_link*TREATx1920+married_link*TREATx1940 | STATEICP^COUNTYICP,
        data = netdf),
  feols(move_state ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
        data = netdf),
  feols(move_state ~ married_link + married_link*Year1920+married_link*Year1940+married_link*TREATx1920+married_link*TREATx1940 | STATEICP^COUNTYICP,
        data = netdf)
  )

means = c(mean(mutate(filter(netdf, YEAR == 1930 & TREAT == 1), ch = ifelse(NCHILD_link > 0, 1, 0))$ch),
          mean(filter(netdf, YEAR == 1930 & TREAT == 1)$OCCSCORE_link),
          mean(filter(netdf, YEAR == 1930 & TREAT == 1)$OCCSCORE_link),
          mean(filter(netdf, YEAR == 1930 & TREAT == 1)$worker_link),
          mean(filter(netdf, YEAR == 1930 & TREAT == 1)$worker_link),
          mean(filter(netdf, YEAR == 1930 & TREAT == 1)$move_state),
          mean(filter(netdf, YEAR == 1930 & TREAT == 1)$move_state))

esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"), extralines = list("Dep. Var. 1930 Treated Mean" = means)) %>% print()

#______________________________________________________
# RESULT 6: FERTILITY ----
#______________________________________________________
# children
did_graph(dataset     = neighbor_wht,
          depvarlist  = c("pctw_wc_Teacher"),
          depvarnames = c("At least one child"),
          colors      = c(mw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Women Teachers with Children"),
          filename = "sharewteach_children")

# close log ----
sink()

