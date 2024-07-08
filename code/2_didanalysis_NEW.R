### CREATING DiD PLOTS -- New Version 5/3/24 (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

# initializing main datasets ----
# countysumm: cross-sectional data
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)

matched1   <- countysumm %>% filter(match_weight1 != 0 & mainsamp == 1)
matched2   <- countysumm %>% filter(match_weight2 != 0 & mainsamp == 1)
matched3   <- countysumm %>% filter(match_weight3 != 0 & mainsamp == 1) %>% mutate(weight = match_weight3)

datasets   <- list(neighbor, matched1, matched2, matched3)
datanames  <- list("neighbor", "matched1", "matched2", "matched3")

#______________________________________________________
# RESULT 1: DIRECT EFFECT ON MARRIED WOMEN ----
#______________________________________________________
# OUTCOME: P(teacher|MW) (only for neighbor sample)
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_mw_Teacher"),
          depvarnames = c("Married Women"),
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          pointtypes = c(17),
          ymin = -0.015, ymax = 0.06,
          filename = "shareteachmw_neighbor")

## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("pct_mw_Teacher", "pct_Teacher_mw_100")){
  out_did        <- did_graph_data(neighbor, coefname, table = TRUE) #returns list of [model, cov matrix]
  if (coefname == "pct_Teacher_mw_100"){
    out_did <- did_graph_data(neighbor %>% mutate(weight = NWHITEMW), coefname, table = TRUE) #returns list of [model, cov matrix]
  }
  models[[i]]    <- out_did[[1]]
  ses[[i]]       <- sqrt(diag(out_did[[2]]))
  
  if (coefname == "pct_Teacher_mw_100"){
    sharereg_means <- c(sharereg_means, weighted.mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]],filter(neighbor, YEAR == 1930 & TREAT == 1)$NWHITEMW))
  }
  else{
    sharereg_means <- c(sharereg_means, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]]))
  }
  i = i + 1
}
# gen table
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950"),#omit = c("Constant","cluster*", "factor*"), 
          out = glue("./tables/mwregs.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          dep.var.labels.include = FALSE,
          column.labels = c("Share Teach Mar. Wom.", "MW Teach/100 MW"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 2: MECHANISMS FOR MARRIED WOMEN ----
#______________________________________________________

## stargazer tables ----
models1        <- list()
ses1           <- list()
linkreg_means1 <- c()
models2        <- list()
ses2           <- list()
linkreg_means2 <- c()
models3        <- list()
ses3           <- list()
linkreg_means3 <- c()
# prepare data 
i = 1
for (coefname in c("pct_mw", "pct_mwt", "pct_mwnt", "pct_mwnilf")){
  # for link 1
  out_did <- did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                            coefname, years = c(1920,1940), table = TRUE)
  models1[[i]] <- out_did[[1]]
  ses1[[i]] <- sqrt(diag(out_did[[2]]))
  linkreg_means1 <- c(linkreg_means1, mean(filter(link1, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2
  out_did2 <- did_graph_data(link2 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models2[[i]] <- out_did2[[1]]
  ses2[[i]] <- sqrt(diag(out_did2[[2]]))
  linkreg_means2 <- c(linkreg_means2, mean(filter(link2, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 3
  out_did3 <- did_graph_data(link3 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models3[[i]] <- out_did3[[1]]
  ses3[[i]] <- sqrt(diag(out_did3[[2]]))
  linkreg_means3 <- c(linkreg_means3, mean(filter(link3, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}
# gen tables
stargazer(models1, se=ses1, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_swt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1))),
          table.layout = "=lc#-t-as=")

stargazer(models2, se=ses2, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_swnilf.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2))),
          table.layout = "=lc#-t-as=")

stargazer(models3, se=ses3, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_mwnilf.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means3))),
          table.layout = "=lc#-t-as=")

## APPENDIX TABLE: GROUPS 2.5 and 3.5

## stargazer tables ----
models1app        <- list()
ses1app           <- list()
linkreg_means1app <- c()
models2app        <- list()
ses2app           <- list()
linkreg_means2app <- c()
# prepare data 
i = 1
for (coefname in c("pct_mw", "pct_mwt", "pct_mwnt", "pct_mwnilf")){
  # for link 1
  out_did <- did_graph_data(link2point5 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                            coefname, years = c(1920,1940), table = TRUE)
  models1app[[i]] <- out_did[[1]]
  ses1app[[i]] <- sqrt(diag(out_did[[2]]))
  linkreg_means1app <- c(linkreg_means1app, mean(filter(link2point5, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2
  out_did2 <- did_graph_data(link3point5 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models2app[[i]] <- out_did2[[1]]
  ses2app[[i]] <- sqrt(diag(out_did2[[2]]))
  linkreg_means2app <- c(linkreg_means2app, mean(filter(link3point5, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}
# gen tables
stargazer(models1app, se=ses1app, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_swnt_app.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1app))),
          table.layout = "=lc#-t-as=")

stargazer(models2app, se=ses2app, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_mwnt_app.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2app))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 3: DIRECT EFFECT ON MEN/UNMARRIED WOMEN ----
#______________________________________________________
for (i in 1:4){
  # OUTCOME: SHARE TEACHERS MW/SW/M 
  did_graph(dataset     = datasets[[i]], 
            depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
            depvarnames = c("Men", "Married Women", "Single Women"),
            colors      = c(men_col, mw_col, sw_col),
            yvar        = "DiD Estimate: Share of Teachers",
            ymin        = -0.065, 
            ymax        = 0.06,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("shareteach_{datanames[[i]]}"))  %>% print()
  Sys.sleep(2) #pause so i can see the graph output
  
}

## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "num_Teacher")){
  out_did        <- did_graph_data(neighbor, coefname, table = TRUE) #returns list of [model, cov matrix]
  if (coefname == "pct_Teacher_mw_1000"){
    out_did <- did_graph_data(neighbor %>% mutate(weight = NWHITEMW), coefname, table = TRUE) #returns list of [model, cov matrix]
  }
  models[[i]]    <- out_did[[1]]
  ses[[i]]       <- sqrt(diag(out_did[[2]]))
  
  if (coefname == "pct_Teacher_mw_1000"){
    sharereg_means <- c(sharereg_means, weighted.mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]],filter(neighbor, YEAR == 1930 & TREAT == 1)$NWHITEMW))
  }
  else{
    sharereg_means <- c(sharereg_means, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]]))
  }
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
# SAMPLE: SWT IN BASE YEAR; OUTCOME: SW x workingstatus
did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_swt", "pct_swnt", "pct_swnilf"),
          depvarnames = c("Unmarried Teacher in t", "Unmarr. Non-Teacher in LF in t", "Unmarr. Not in LF in t"),
          colors      = c(men_col, mw_col, sw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swt"))

did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_swwhitecollar", "pct_swbluecollar"),
          depvarnames = c("Unmarried Non-Teacher White Collar Occ. in t", "Blue Collar Occ. in t"),
          colors      = c(mw_col, "grey"),
          pointspan = 1,
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swt_inlf"))

link2combined <- link2 %>% filter(neighbor_samp == 1 & mainsamp == 1) %>% 
  select(c(YEAR, FIPS, STATEICP, COUNTYICP, pct_swt, TREAT)) %>% rename(pct_swt_link2 = pct_swt) %>%
  left_join(link2point5 %>% filter(neighbor_samp == 1 & mainsamp == 1) %>% 
              select(c(YEAR, FIPS, STATEICP, COUNTYICP, pct_swt, TREAT)) %>% rename(pct_swt_link2point5 = pct_swt))

did_graph(dataset     = link2combined,
          depvarlist  = c("pct_swt_link2", "pct_swt_link2point5"),
          depvarnames = c("Sample 2: Un", "unmarried 2 point 5"),
          colors      = c(mw_col, sw_col),
          pointspan = 1,
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swnilf_swt"))


## stargazer tables ----
models1        <- list()
ses1           <- list()
linkreg_means1 <- c()
models2        <- list()
ses2           <- list()
linkreg_means2 <- c()
models3        <- list()
ses3           <- list()
linkreg_means3 <- c()
# prepare data 
i = 1
for (coefname in c("pct_sw","pct_swt", "pct_swnt", "pct_swnilf")){
  # for link 1
  out_did <- did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                            coefname, years = c(1920,1940), table = TRUE)
  models1[[i]] <- out_did[[1]]
  ses1[[i]] <- sqrt(diag(out_did[[2]]))
  linkreg_means1 <- c(linkreg_means1, mean(filter(link1, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2.5 (unmarried women non teachers in the labor force)
  out_did2 <- did_graph_data(link2point5 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models2[[i]] <- out_did2[[1]]
  ses2[[i]] <- sqrt(diag(out_did2[[2]]))
  linkreg_means2 <- c(linkreg_means2, mean(filter(link2point5, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2 (unmarried women not in the labor force)
  out_did3 <- did_graph_data(link2 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models3[[i]] <- out_did3[[1]]
  ses3[[i]] <- sqrt(diag(out_did3[[2]]))
  linkreg_means3 <- c(linkreg_means3, mean(filter(link2, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}
# gen tables
stargazer(models1, se=ses1, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregssw_swt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Unmarried in t", "Unmarried Teacher in t", 
                            "Unmarried Non-Teacher in LF in t", 
                            "Unmarried Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1))),
          table.layout = "=lc#-t-as=")

stargazer(models2, se=ses2, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregssw_swnilf.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Unmarried in t", "Unmarried Teacher in t", 
                            "Unmarried Non-Teacher in LF in t", 
                            "Unmarried Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2))),
          table.layout = "=lc#-t-as=")

stargazer(models3, se=ses3, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregssw_swnt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Unmarried in t", "Unmarried Teacher in t", 
                            "Unmarried Non-Teacher in LF in t", 
                            "Unmarried Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means3))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 5: NET EFFECTS ----
#______________________________________________________
link1datasets   <- list(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                        link1 %>% filter(match_weight1 != 0 & mainsamp == 1), 
                        link1 %>% filter(match_weight2 != 0 & mainsamp == 1), 
                        link1 %>% filter(match_weight3 != 0 & mainsamp == 1))
datanames  <- list("neighbor", "matched1", "matched2", "matched3")

for (i in 1:4){
  did_graph(dataset     = link1datasets[[i]],
            depvarlist  = c("pct_lf", "pct_mw_cond_inlf", "pct_sw_cond_inlf"),
            depvarnames = c("LFP in t", "LFP in t | Married in t", "LFP in t | Unmarried in t"),
            colors      = c("darkgrey", mw_col, sw_col),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("overall_effects_lfp_{datanames[[i]]}"))
  
}

did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("avg_occscore", "avg_occscore_mw", "avg_occscore_sw"),
          depvarnames = c("Mean Occupational Score in t", "Mean Occ. Score in t | Married in t", "Mean Occ. Score in t | Unmarried in t"),
          colors      = c("darkgrey", mw_col, sw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("overall_effects_occscore"))



