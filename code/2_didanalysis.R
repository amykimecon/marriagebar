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
# OG figures: All figures
# iterating through each sample in datasets
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

# OUTCOME: OVERALL SUPPLY OF TEACHERS (only for neighbor sample)
did_graph(dataset     = neighbor, 
          depvarlist  = c("num_Teacher"),
          depvarnames = c("Number of Teachers"),
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Number of Teachers",
          filename    = glue("numteach_neighbor")) %>% print()
Sys.sleep(2) #pause so i can see the graph output
##! CHECKED

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
Sys.sleep(2) #pause so i can see the graph output

## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "num_Teacher", "pct_Teacher_mw_1000")){
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
                              "\\% Teach Unmar. Wom.","\\# Teachers", "MW Teach/1000 MW"),
            column.separate = c(1,1,1,1,1),
            covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                                 "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
            add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
            table.layout = "=lc#-t-as=")
  

#________________________________________________________
# RESULT 2: TRANSITION PROBABILITIES W/ LINKED DATA ----
#________________________________________________________
linkdatasets <- list(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
                     link1 %>% filter(match_weight1 != 0 & mainsamp == 1),
                     link1 %>% filter(match_weight2 != 0 & mainsamp == 1),
                     link1 %>% filter(match_weight3 != 0 & mainsamp == 1)
)

## figures ----
# OG code for generating all combos of Pr(MWT/MWNT/MWNILF/SW) on all linked samples
for (i in 1:4){
  # SAMPLE: SWT IN BASE YEAR; OUTCOME: MARRIED BY LINK YEAR
  did_graph(dataset     = linkdatasets[[i]],
            depvarlist  = c("pct_mw"),
            depvarnames = c("Married in t"),
            colors      = c(mw_col),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
            ymin        = -0.066,
            ymax        = 0.05,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("linked_swt_{datanames[[i]]}_mw"))
  
  # SAMPLE: SWT IN BASE YEAR; OUTCOME: (MARRIED BY LINK YEAR ) X (OCC IN LINK YEAR)
  did_graph(dataset     = linkdatasets[[i]],
            depvarlist  = c("pct_mwt", "pct_mwnilf"),
            depvarnames = c("Married Teacher in t", "Married Not in LF in t"),
            colors      = c(mw_col, "grey"),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
            ymin        = -0.066,
            ymax        = 0.05,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("linked_swt_{datanames[[i]]}")) %>% print()
  Sys.sleep(2) #pause so i can see the graph output
}

# SAMPLE: SWT IN BASE YEAR; OUTCOME: TEACHER IN LINK YEAR (only for neighbor)
did_graph(dataset     = linkdatasets[[1]],
          depvarlist  = c("pct_t"),
          depvarnames = c("Teacher in t"),
          colors      = c(men_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          ymin        = -0.066,
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swt_neighbor_t"))

# SAMPLE: SW SECRETARIES IN BASE YEAR; OUTCOME: MARRIED BY LINK YEAR (only for neighbor)
did_graph(dataset     = link1sec %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_mw"),
          depvarnames = c("Married in t"),
          colors      = c(mw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Secretaries in t-10"),
          ymin        = -0.066,
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_sws_neighbor_mw"))

# SAMPLE: SW SECRETARIES IN BASE YEAR; OUTCOME: (MARRIED BY LINK YEAR ) X (OCC IN LINK YEAR) (only for neighbor)
did_graph(dataset     = link1sec %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_mws", "pct_mwnilf"),
          depvarnames = c("Married Secretary in t", "Married Not in LF in t"),
          colors      = c(mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Secretaries in t-10"),
          ymin        = -0.066,
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_sws_neighbor")) %>% print()
Sys.sleep(2) #pause so i can see the graph output

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
for (coefname in c("pct_mw", "pct_t", "pct_mwt", "pct_mwnt", "pct_mwnilf")){
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
          column.labels = c("Married in t", "Teacher in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1))),
          table.layout = "=lc#-t-as=")

stargazer(models2, se=ses2, omit = c("Constant","cluster*", "factor*", "Year*"),
          out = glue("./tables/linkregs_swnt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Teacher in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2))),
          table.layout = "=lc#-t-as=")

stargazer(models3, se=ses3, omit = c("Constant","cluster*", "factor*", "Year*"),
          out = glue("./tables/linkregs_mwnilf.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Teacher in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means3))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
#  RESULT 3: HH OUTCOMES ----
#______________________________________________________
## figures ----
# # # OUTCOME: SHARE TEACHERS WC/WNC/M (neighbor sample) ##! Keep?
# # did_graph(dataset = neighbor, 
# #           depvarlist = c("pct_m_Teacher", "pct_wc_Teacher", "pct_wnc_Teacher"), 
# #           depvarnames = c("Men", "Women with Children", "Women without Children"),
# #           colors = c(men_col, mw_col, sw_col),
# #           yvar = "DiD Estimate: Share of Teachers",
# #           ymin = -0.065, ymax = 0.06,
# #           verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
# #           filename = "shareteach_children_neighbor")
# # 
# # OUTCOME: SHARE W TEACHERS WC/WNC/M (neighbor sample)
# did_graph(dataset     = neighborNC,
#           depvarlist  = c("pctw_wc_Teacher", "pctw_wc_Secretary"),
#           depvarnames = c("Teachers", "Secretaries"),
#           colors      = c(mw_col, "grey"),
#           yvar        = "DiD Estimate: Share Women Teachers w/ Children",
#           ymin        = -0.065, 
#           ymax        = 0.06,
#           pointspan   = 1.2,
#           verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
#           filename    = "sharewteach_children_neighborNC")
# 
# # OUTCOME: Avg # children (neighbor sample)
# did_graph(dataset     = neighborNC,
#           depvarlist  = c("avg_nchild_Teacher", "avg_nchild_Secretary"),
#           depvarnames = c("Women Teachers", "Women Secretaries"),
#           colors      = c(mw_col, sw_col),
#           yvar        = "DiD Estimate: Mean # of Children",
#           verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
#           filename    = "nchild_neighborNC")
# 
# # OUTCOME: Avg age at first child (neighbor sample)
# did_graph(dataset     = neighborNC,
#           depvarlist  = c("avg_age_child_Teacher", "avg_age_child_Secretary"),
#           depvarnames = c("Women Teachers", "Women Secretaries"),
#           colors      = c(mw_col, sw_col),
#           yvar        = "DiD Estimate: Mean Age at First Child",
#           verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
#           filename    = "agechild_neighborNC")
# 
# # OUTCOME: Average occscore of spouse for teachers (neighbor sample)
# did_graph(dataset     = neighbor, 
#           depvarlist  = c("avg_occscore_w_Teacher", "avg_occscore_m_Teacher"), 
#           depvarnames = c("Women Married Teachers", "Men Married Teachers"),
#           colors      = c(mw_col, men_col),
#           yvar        = "DiD Estimate: Average OCCSCORE of Spouse",
#           verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
#           filename    = "spouse_occscore_neighbor")
# 
# # OUTCOME: Average share of teachers' spouses that are also teachers (neighbor sample)
# did_graph(dataset     = neighbor, 
#           depvarlist  = c("pct_sp_teach_m_Teacher"), 
#           depvarnames = c("Teachers"),
#           colors      = c(mw_col),
#           yvar        = "DiD Estimate: Share Married Men Teachers w/ Teacher Spouses",
#           filename    = "spouse_sameocc_neighbor")
# 
# # OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
# did_graph(dataset     = link1point5 %>% filter(neighbor_sampNC == 1 & mainsamp == 1),
#           depvarlist  = c("pct_wct", "pct_wcnt", "pct_wcnilf", "pct_wnc"),
#           depvarnames = c("Woman Teacher w/ Children", "Woman Non-Teacher in LF w/ Children", "Woman Not in LF w/ Children", "Woman w/o Children"),
#           colors      = c(men_col, mw_col, "grey", sw_col),
#           years       = c(1920, 1940),
#           yvar        = glue("DiD Estimate: Share of Women Teachers Without Children in t-10"),
#           filename    = "linktemp_childrenNC")
# 
# ## THIS IS THE MAIN GRAPH
# did_graph(dataset     = link1 %>% filter(neighbor_sampNC == 1 & mainsamp == 1),
#           depvarlist  = c("pct_wct", "pct_wcnt", 
#                           "pct_wcnilf", "pct_wnct", 
#                           "pct_wncnt", "pct_wncnilf"), 
#           depvarnames = c("Woman Teacher w/ Children", "Woman Non-Teacher in LF w/ Children", 
#                           "Woman Not in LF w/ Children", "Woman Teacher w/o Children", 
#                           "Woman Non-Teacher in LF w/o Children", "Woman Not in LF w/o Children"),
#           colors      = gg_color_hue(6),
#           years       = c(1920, 1940),
#           ymin        = -0.066, 
#           ymax        = 0.06,
#           yvar        = glue("DiD Estimate: Share of Unmarried Women Teachers in t-10"),
#           filename    = "linked_swt_childrenNC")
# # 
# # did_graph(dataset = link1point52 %>% filter(neighbor_sampNC == 1 & mainsamp == 1),
# #           depvarlist = c("pct_wct", "pct_wcnt", "pct_wcnilf", "pct_wnc"), 
# #           depvarnames = c("Woman Teacher w/ Children", "Woman Non-Teacher in LF w/ Children", "Woman Not in LF w/ Children", "Woman w/o Children"),
# #           colors = c(men_col, mw_col, "grey", sw_col),
# #           years = c(1920, 1940),
# #           yvar = glue("DiD Estimate: Share of Women Teachers Without Children in t-10"))
# # 
# 
# ## stargazer tables ----
# models4 <- list()
# ses4 <- list()
# reg_means4 <- c()
# i = 1
# for (coefname in c("avg_occscore_w_Teacher", "avg_occscore_m_Teacher", "pct_sp_teach_m_Teacher")){
#   out_did <- did_graph_data(neighbor, coefname, years = c(1940, 1950), table = TRUE)
#   models4[[i]] <- out_did[[1]]
#   ses4[[i]] <- sqrt(diag(out_did[[2]]))
#   reg_means4 <- c(reg_means4, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]], na.rm=TRUE))
#   i = i + 1
# }
# 
# stargazer(models4, se=ses4, keep = c("TREATx1940","TREATx1950"), 
#           out = glue("./tables/outregs_spouse.tex"),
#           float = FALSE,
#           keep.stat = c('n','adj.rsq'),
#           dep.var.caption = "Dependent Variable:",
#           dep.var.labels.include = FALSE,
#           column.labels = c("Avg. Spouse OCCSCORE of Mar. Wom. Teachers", "Avg. Spouse OCCSCORE of Mar. Men Teachers", "Share Mar. Men Teachers w/ Teacher Spouse"),
#           column.separate = c(1,1,1),
#           covariate.labels = c("Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)",
#                                "Treated $\\times$ Post-Ban ($\\gamma_{1940}^{DD}$)"),
#           add.lines = list(c("Dep. Var. 1930 Mean", formatC(reg_means4))),
#           table.layout = "=lc#-t-as=")

# close log ----
sink() 



