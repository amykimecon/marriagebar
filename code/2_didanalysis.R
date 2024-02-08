### CREATING DiD PLOTS (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

# open log ----
sink("./logs/log_2_didanalysis.txt", append=FALSE)

# initializing main datasets ----
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)
neighborNC <- countysumm %>% filter(neighbor_sampNC == 1 & mainsamp == 1)
neighborKY <- countysumm %>% filter(neighbor_sampKY == 1 & mainsamp == 1)

matched1   <- countysumm %>% filter(match_weight1 != 0 & mainsamp == 1)
matched2   <- countysumm %>% filter(match_weight2 != 0 & mainsamp == 1)
matched3   <- countysumm %>% filter(match_weight3 != 0 & mainsamp == 1) %>% mutate(weight = match_weight3)

datasets   <- list(neighbor, matched1, matched2, matched3)
datanames  <- list("neighbor", "matched1", "matched2", "matched3")

#______________________________________________________
# RESULT 1: COMPOSITION OF TEACHER WORKFORCE ----
#______________________________________________________
## figures ----
# iterating through each sample in datasets
for (i in 1:4){
  # OUTCOME: OVERALL SUPPLY OF TEACHERS
  did_graph(dataset     = datasets[[i]], 
            depvarlist  = c("num_Teacher"),
            depvarnames = c("Number of Teachers"),
            colors      = c(mw_col),
            yvar        = "DiD Estimate: Number of Teachers",
            filename    = glue("numteach_{datanames[[i]]}")) %>% print()
  Sys.sleep(2) #pause so i can see the graph output
  ##! CHECKED
  
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
  
  # OUTCOME: SHARE SECRETARIES MW/SW/M 
  did_graph(dataset     = datasets[[i]], 
            depvarlist  = c("pct_m_Secretary", "pct_mw_Secretary", "pct_sw_Secretary"), 
            depvarnames = c("Men", "Married Women", "Single Women"),
            colors      = c(men_col, mw_col, sw_col),
            yvar        = "DiD Estimate: Share of Secretaries",
            ymin        = -0.065, 
            ymax        = 0.06,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("sharesec_{datanames[[i]]}"))  %>% print()
  Sys.sleep(2) #pause so i can see the graph output
}

## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("num_Teacher","teacher_ratio","pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "pctw_wc_Teacher")){
  out_did        <- did_graph_data(neighbor, coefname, years = c(1940, 1950), table = TRUE) #returns list of [model, cov matrix]
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
          column.labels = c("\\# Teachers","Students/Teacher",
                            "\\% Teach Mar. Wom.", "\\% Teach Men", 
                            "\\% Teach Unmar. Wom.","\\% Teach Wom. w/ Chil."),
          column.separate = c(1,1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ Post-Ban ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
          table.layout = "=lc#-t-as=")


#________________________________________________________
# RESULT 2: TRANSITION PROBABILITIES W/ LINKED DATA ----
#________________________________________________________
linkdatasets <- list(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
                     link2 %>% filter(neighbor_samp == 1 & mainsamp == 1),
                     link3 %>% filter(neighbor_samp == 1 & mainsamp == 1),
                     
                     link1 %>% filter(match_weight1 == 1 & mainsamp == 1),
                     link2 %>% filter(match_weight1 == 1 & mainsamp == 1),
                     link3 %>% filter(match_weight1 == 1 & mainsamp == 1),
                     
                     link1 %>% filter(match_weight2 == 1 & mainsamp == 1),
                     link2 %>% filter(match_weight2 == 1 & mainsamp == 1),
                     link3 %>% filter(match_weight2 == 1 & mainsamp == 1)
)
yvarlist    <- rep(c("Unmarried Women Teachers", "Unmarried Women Non-Teachers", "Married Women Non-Teachers"),3)
yvarlablist <- rep(c("swt","swnt","mwnt"),3)
linklablist <- c(rep("neighbor",3), rep("matched1", 3), rep("matched2", 3))

## figures ----
for (i in 1:9){
  # OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & 
  # TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
  did_graph(dataset     = linkdatasets[[i]],
            depvarlist  = c("pct_mwt", "pct_mwnt", "pct_mwnilf", "pct_sw"), 
            depvarnames = c("Married Teacher", "Married Non-Teacher in LF", "Married Not in LF", "Not Married"),
            colors      = c(men_col, mw_col, "grey", sw_col),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
            ymin        = -0.066, 
            ymax        = 0.05,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("linked_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
  Sys.sleep(2) #pause so i can see the graph output
  
}

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
for (coefname in c("pct_mwt", "pct_mwnt", "pct_mwnilf", "pct_sw")){
  # for link 1
  out_did <- did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                            coefname, years = c(1940), table = TRUE)
  models1[[i]] <- out_did[[1]]
  ses1[[i]] <- sqrt(diag(out_did[[2]]))
  linkreg_means1 <- c(linkreg_means1, mean(filter(link1, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2
  out_did2 <- did_graph_data(link2 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1940), table = TRUE)
  models2[[i]] <- out_did2[[1]]
  ses2[[i]] <- sqrt(diag(out_did2[[2]]))
  linkreg_means2 <- c(linkreg_means2, mean(filter(link2, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 3
  out_did3 <- did_graph_data(link3 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1940), table = TRUE)
  models3[[i]] <- out_did3[[1]]
  ses3[[i]] <- sqrt(diag(out_did3[[2]]))
  linkreg_means3 <- c(linkreg_means3, mean(filter(link3, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}
# gen tables
stargazer(models1, se=ses1, omit = c("Constant","cluster*", "factor*", "Year*"),
          out = glue("./tables/linkregs_swt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married Women Teachers", "Married Women Non-Teachers in LF", 
                            "Married Women Not in LF", "Unmarried Women"),
          column.separate = c(1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1))),
          table.layout = "=lc#-t-as=")

stargazer(models2, se=ses2, omit = c("Constant","cluster*", "factor*", "Year*"),
          out = glue("./tables/linkregs_swnt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married Women Teachers", "Married Women Non-Teachers in LF", 
                            "Married Women Not in LF", "Unmarried Women"),
          column.separate = c(1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2))),
          table.layout = "=lc#-t-as=")

stargazer(models3, se=ses3, omit = c("Constant","cluster*", "factor*", "Year*"),
          out = glue("./tables/linkregs_mwnt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married Women Teachers", "Married Women Non-Teachers in LF", 
                            "Married Women Not in LF", "Unmarried Women"),
          column.separate = c(1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means3))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
#  RESULT 3: HH OUTCOMES ----
#______________________________________________________
## figures ----
# # OUTCOME: SHARE TEACHERS WC/WNC/M (neighbor sample) ##! Keep?
# did_graph(dataset = neighbor, 
#           depvarlist = c("pct_m_Teacher", "pct_wc_Teacher", "pct_wnc_Teacher"), 
#           depvarnames = c("Men", "Women with Children", "Women without Children"),
#           colors = c(men_col, mw_col, sw_col),
#           yvar = "DiD Estimate: Share of Teachers",
#           ymin = -0.065, ymax = 0.06,
#           verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
#           filename = "shareteach_children_neighbor")
# 
# OUTCOME: SHARE W TEACHERS WC/WNC/M (neighbor sample)
did_graph(dataset     = neighborNC,
          depvarlist  = c("pctw_wc_Teacher", "pctw_wc_Secretary"),
          depvarnames = c("Teachers", "Secretaries"),
          colors      = c(mw_col, "grey"),
          yvar        = "DiD Estimate: Share Women Teachers w/ Children",
          ymin        = -0.065, 
          ymax        = 0.06,
          pointspan   = 1.2,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = "sharewteach_children_neighborNC")

# OUTCOME: Avg # children (neighbor sample)
did_graph(dataset     = neighborNC,
          depvarlist  = c("avg_nchild_Teacher", "avg_nchild_Secretary"),
          depvarnames = c("Women Teachers", "Women Secretaries"),
          colors      = c(mw_col, sw_col),
          yvar        = "DiD Estimate: Mean # of Children",
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = "nchild_neighborNC")

# OUTCOME: Avg age at first child (neighbor sample)
did_graph(dataset     = neighborNC,
          depvarlist  = c("avg_age_child_Teacher", "avg_age_child_Secretary"),
          depvarnames = c("Women Teachers", "Women Secretaries"),
          colors      = c(mw_col, sw_col),
          yvar        = "DiD Estimate: Mean Age at First Child",
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = "agechild_neighborNC")

# OUTCOME: Average occscore of spouse for teachers (neighbor sample)
did_graph(dataset     = neighbor, 
          depvarlist  = c("avg_occscore_w_Teacher", "avg_occscore_m_Teacher"), 
          depvarnames = c("Women Married Teachers", "Men Married Teachers"),
          colors      = c(mw_col, men_col),
          yvar        = "DiD Estimate: Average OCCSCORE of Spouse",
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = "spouse_occscore_neighbor")

# OUTCOME: Average share of teachers' spouses that are also teachers (neighbor sample)
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_sp_teach_m_Teacher"), 
          depvarnames = c("Teachers"),
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Share Married Men Teachers w/ Teacher Spouses",
          filename    = "spouse_sameocc_neighbor")

# OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
did_graph(dataset     = link1point5 %>% filter(neighbor_sampNC == 1 & mainsamp == 1),
          depvarlist  = c("pct_wct", "pct_wcnt", "pct_wcnilf", "pct_wnc"),
          depvarnames = c("Woman Teacher w/ Children", "Woman Non-Teacher in LF w/ Children", "Woman Not in LF w/ Children", "Woman w/o Children"),
          colors      = c(men_col, mw_col, "grey", sw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share of Women Teachers Without Children in t-10"),
          filename    = "linktemp_childrenNC")

## THIS IS THE MAIN GRAPH
did_graph(dataset     = link1 %>% filter(neighbor_sampNC == 1 & mainsamp == 1),
          depvarlist  = c("pct_wct", "pct_wcnt", 
                          "pct_wcnilf", "pct_wnct", 
                          "pct_wncnt", "pct_wncnilf"), 
          depvarnames = c("Woman Teacher w/ Children", "Woman Non-Teacher in LF w/ Children", 
                          "Woman Not in LF w/ Children", "Woman Teacher w/o Children", 
                          "Woman Non-Teacher in LF w/o Children", "Woman Not in LF w/o Children"),
          colors      = gg_color_hue(6),
          years       = c(1920, 1940),
          ymin        = -0.066, 
          ymax        = 0.06,
          yvar        = glue("DiD Estimate: Share of Unmarried Women Teachers in t-10"),
          filename    = "linked_swt_childrenNC")
# 
# did_graph(dataset = link1point52 %>% filter(neighbor_sampNC == 1 & mainsamp == 1),
#           depvarlist = c("pct_wct", "pct_wcnt", "pct_wcnilf", "pct_wnc"), 
#           depvarnames = c("Woman Teacher w/ Children", "Woman Non-Teacher in LF w/ Children", "Woman Not in LF w/ Children", "Woman w/o Children"),
#           colors = c(men_col, mw_col, "grey", sw_col),
#           years = c(1920, 1940),
#           yvar = glue("DiD Estimate: Share of Women Teachers Without Children in t-10"))
# 

## stargazer tables ----
models4 <- list()
ses4 <- list()
reg_means4 <- c()
i = 1
for (coefname in c("avg_occscore_w_Teacher", "avg_occscore_m_Teacher", "pct_sp_teach_m_Teacher")){
  out_did <- did_graph_data(neighbor, coefname, years = c(1940, 1950), table = TRUE)
  models4[[i]] <- out_did[[1]]
  ses4[[i]] <- sqrt(diag(out_did[[2]]))
  reg_means4 <- c(reg_means4, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]], na.rm=TRUE))
  i = i + 1
}

stargazer(models4, se=ses4, keep = c("TREATx1940","TREATx1950"), 
          out = glue("./tables/outregs_spouse.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          dep.var.labels.include = FALSE,
          column.labels = c("Avg. Spouse OCCSCORE of Mar. Wom. Teachers", "Avg. Spouse OCCSCORE of Mar. Men Teachers", "Share Mar. Men Teachers w/ Teacher Spouse"),
          column.separate = c(1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(reg_means4))),
          table.layout = "=lc#-t-as=")

# close log ----
sink() 

# code graveyard ---- 
##! keep? 
# ## Initializing key datasets
# matched <- countysumm %>% filter(match_samp == 1 & mainsamp == 1)
# neighbor <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)
# neighborNC <- countysumm %>% filter(neighbor_sampNC == 1 & mainsamp == 1)
# neighborKY <- countysumm %>% filter(neighbor_sampKY == 1 & mainsamp == 1)
# all <- countysumm %>% filter(mainsamp == 1)
# 
# # testing other versions of matching
# matched2 <- countysumm %>% filter(match_samp2 == 1 & mainsamp == 1)
# matched3 <- countysumm %>% filter(match_samp3 == 1 & mainsamp == 1)
# #matched4 <- countysumm %>% filter(match_samp4 == 1 & mainsamp == 1)
# 
# ## Graphing distribution of treatment/control counties
# graph_treatment(neighbor, filename = "neighbormap") + ggtitle("Neighbor Sample")
# graph_treatment(neighborNC, filename = "neighbormapNC") + ggtitle("Neighbor Sample NC")
# graph_treatment(neighborKY, filename = "neighbormapKY") + ggtitle("Neighbor Sample KY")
# 
# graph_treatment(matched, filename = "matchedmap") + ggtitle("Matched Sample: Set 1")
# graph_treatment(matched2, filename = "matchedmap2") + ggtitle("Matched Sample: Set 2")
# graph_treatment(matched3, filename = "matchedmap3") + ggtitle("Matched Sample: Set 3")
# 
# ## TESTING MATCHING
# test_all <- bind_rows(list(all %>% mutate(sample = "All Counties"),
#                            neighborNC %>% mutate(sample = "Neighbor SampleNC"),
#                            neighborKY %>% mutate(sample = "Neighbor SampleKY"),
#                            matched %>% mutate(sample = "Matched Sample: Set 1"),
#                            matched2 %>% mutate(sample = "Matched Sample: Set 2"),
#                            matched3 %>% mutate(sample = "Matched Sample: Set 3")))
# 
# match_test(test_all %>% filter(sample != "All Counties"), "POP") 
# match_test(test_all, "LFP_SW") 
# match_test(test_all, "PCT_LF_MW") 
# match_test(test_all, "LFP_MW") 
# match_test(test_all, "LFP_WMW") 
# match_test(test_all, "PCT_WHITE") 
# match_test(test_all, "pct_mw_Teacher") 
# 
# ## Result 1: Share Teachers by Demgroup
# depvars1 <- c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher")
# depvarlabs1 <- c("Men", "Married Women", "Single Women")
# colors1 <- c(men_col, mw_col, sw_col)
# did_graph(neighbor, depvars1, depvarlabs1, colors1, filename = "shareteach_neighborsamp") + ggtitle("neighbor sample")
# did_graph(neighborNC, depvars1, depvarlabs1, colors1, filename = "shareteach_neighborsampNC") + ggtitle("neighbor sample NC")
# did_graph(neighborKY, depvars1, depvarlabs1, colors1, filename = "shareteach_neighborsampKY") + ggtitle("neighbor sample KY")
# did_graph(matched, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp1") + ggtitle("matched sample: set 1")
# did_graph(matched2, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp2") + ggtitle("matched sample: set 2")
# did_graph(matched3, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp3") + ggtitle("matched sample: set 3")
# 
# ## Result 1.5: Share MW/SW Teachers
# did_graph(neighbor, c("pct_Teacher_mw", "pct_Teacher_sw"), c("% MW that are teachers", "% SW that are teachers"), c(mw_col, sw_col))
# 
# ## Result 2: Num Teachers Total and Share of Workers Teachers
# did_graph(neighbor, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("neighbor sample")
# did_graph(matched, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 1")
# did_graph(matched2, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 2")
# did_graph(matched3, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 3")
# did_graph(matched3, c("num_Teacher","POP"), c("Teacher", "POP"), c("red","blue")) + ggtitle("matched sample: set 4")
# did_graph(matched3, c("num_Teacher","POP"), c("Teacher", "POP"), c("red","blue")) + ggtitle("matched sample: set 4")
# 
# depvars2 <- c("pct_workers_Teacher", "pct_workers_Secretary")
# depvarlabs2 <- c("Teacher","Secretary")
# did_graph(neighbor, depvars2, depvarlabs2, c("red","blue")) + ggtitle("neighbor sample")
# did_graph(neighborNC, depvars2, depvarlabs2, c("red","blue")) + ggtitle("neighbor sample NC")
# did_graph(neighborKY, depvars2, depvarlabs2, c("red","blue")) + ggtitle("neighbor sample KY")
# 
# did_graph(matched, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 1")
# did_graph(matched2, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 2")
# did_graph(matched3, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 3")
# 
# ## Result 4: Share Teachers MA/MB
# depvars4 <- c("pct_marr_before_Teacher", "pct_marr_after_Teacher")
# depvarlabs4 <- c("Married Before", "Married After")
# did_graph(neighbor, depvars4, depvarlabs4, c("red","blue"), years = c(1940), filename = "share_ma_mb_neighbor") + ggtitle("neighbor sample")
# did_graph(neighborNC, depvars4, depvarlabs4, c("red","blue"), years = c(1940), filename = "share_ma_mb_neighbor") + ggtitle("neighbor sample")
# did_graph(neighborKY, depvars4, depvarlabs4, c("red","blue"), years = c(1940), filename = "share_ma_mb_neighbor") + ggtitle("neighbor sample")
# 
# did_graph(matched, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 1")
# did_graph(matched2, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 2")
# did_graph(matched3, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 3")
# 
# 
# ### LINKED RESULTS ###
# linkvars1 <- c("pct_mwt", "pct_mwnt","pct_mwnilf", "pct_swt", "pct_swnt", "pct_swnilf")
# linklabs1 <- c("MW Teacher", "MW Non-Teacher in LF", "MW Not in LF", "SW Teacher", "SW Non-Teacher in LF", "SW Not in LF")
# # sample 1: outcomes for unmarried women teachers in base year
# did_graph(link1 %>% filter(neighbor_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), 
#           years = c(1920, 1940), filename = "linked1") + ggtitle("neighbor sample, denom SW Teachers in 1930")
# did_graph(link1 %>% filter(neighbor_sampNC & mainsamp), linkvars1, linklabs1, gg_color_hue(6), 
#           years = c(1920, 1940), filename = "linked1NC") + ggtitle("neighbor sample NC, denom SW Teachers in 1930")
# did_graph(link1 %>% filter(neighbor_sampKY & mainsamp), linkvars1, linklabs1, gg_color_hue(6), 
#           years = c(1920, 1940), filename = "linked1KY") + ggtitle("neighbor sample KY, denom SW Teachers in 1930")
# did_graph(link1 %>% filter(match_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1910, 1920, 1940)) + ggtitle("match sample 1, denom SW Teachers in 1930")
# did_graph(link1 %>% filter(match_samp2 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1910, 1920, 1940)) + ggtitle("match sample 2, denom SW Teachers in 1930")
# did_graph(link1 %>% filter(match_samp3 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1910, 1920, 1940)) + ggtitle("match sample 3, denom SW Teachers in 1930")
# 
# # sample 2: outcomes for unmarried women non-teachers in base year
# did_graph(link2 %>% filter(neighbor_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), 
#           years = c(1920, 1940), filename = "linked2") + ggtitle("neighbor sample, denom SW Non-Teachers in 1930")
# did_graph(link2 %>% filter(neighbor_sampNC & mainsamp), linkvars1, linklabs1, gg_color_hue(6), 
#           years = c(1920, 1940), filename = "linked2NC") + ggtitle("neighbor sample NC, denom SW Non-Teachers in 1930")
# did_graph(link2 %>% filter(neighbor_sampKY & mainsamp), linkvars1, linklabs1, gg_color_hue(6), 
#           years = c(1920, 1940), filename = "linked2KY") + ggtitle("neighbor sample KY, denom SW Non-Teachers in 1930")
# did_graph(link2 %>% filter(match_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940)) + ggtitle("match sample 1")
# did_graph(link2 %>% filter(match_samp2 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940)) + ggtitle("match sample 4")
# did_graph(link2 %>% filter(match_samp3 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940)) + ggtitle("match sample 4")
# 
# # sample 3: outcomes for married women non-teachers in base year
# did_graph(link3 %>% filter(neighbor_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940),  filename = "linked3") + ggtitle("neighbor sample, denom MW Non-Teachers in 1930")
# did_graph(link3 %>% filter(neighbor_sampNC & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940),  filename = "linked3NC") + ggtitle("neighbor sample, denom MW Non-Teachers in 1930")
# did_graph(link3 %>% filter(neighbor_sampKY & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940),  filename = "linked3KY") + ggtitle("neighbor sample, denom MW Non-Teachers in 1930")
# 
# did_graph(link3 %>% filter(match_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1910, 1920, 1940)) + ggtitle("match sample 1")
# did_graph(link3 %>% filter(match_samp2 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940)) + ggtitle("match sample 4")
# did_graph(link3 %>% filter(match_samp3 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1910, 1920, 1940)) + ggtitle("match sample 4")
# 
# # raw trends for linked results
# match_test(link1 %>% filter(neighbor_samp & mainsamp) %>% mutate(sample = "Sample: Unmarried Women Teachers in t-10 in Treated & Neighboring States"), linkvars1)
# match_test(link2 %>% filter(neighbor_samp & mainsamp) %>% mutate(sample = "Sample: Unmarried Women Non-Teachers in t-10 in Treated & Neighboring States"), linkvars1)
# match_test(link3 %>% filter(neighbor_samp & mainsamp) %>% mutate(sample = "Sample: Married Women Non-Teachers in t-10 in Treated & Neighboring States"), linkvars1)
# match_test(link3 %>% filter(neighbor_samp & mainsamp) %>% 
#              mutate(sample = "Sample: Married Women Non-Teachers in t-10 in Treated & Neighboring States"), 
#            c("pct_mwnt", "pct_mwt"))
# 
# 
# 
# 
# 
# 
# did_graph(link1 %>% filter(neighbor_samp & mainsamp), "pct_t", "pct teacher", "red", filename = "linked1_pctteacher")
# did_graph(link2 %>% filter(neighbor_samp & mainsamp), "pct_t", "pct teacher", "red", filename = "linked2_pctteacher")
# did_graph(link3 %>% filter(neighbor_samp & mainsamp), "pct_t", "pct teacher", "red", filename = "linked3_pctteacher")
# 
# 



