### CREATING DiD PLOTS (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

# initializing main dataset
neighbor <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)

##############################################################
######### RESULT 1: COMPOSITION OF TEACHER WORKFORCE #########
##############################################################
# OUTCOME: OVERALL SUPPLY OF TEACHERS
did_graph(dataset = neighbor, 
          depvarlist = c("num_Teacher"),
          depvarnames = c("Number of Teachers"),
          colors = c(mw_col),
          yvar = "DiD Estimate: Number of Teachers",
          filename = "numteach_neighbor")

# OUTCOME: SHARE TEACHERS MW/SW/M (neighbor sample)
did_graph(dataset = neighbor, 
          depvarlist = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors = c(men_col, mw_col, sw_col),
          yvar = "DiD Estimate: Share of Teachers",
          ymin = -0.065, ymax = 0.06,
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "shareteach_neighbor")

#####################################################################
######### RESULT 2: TRANSITION PROBABILITIES W/ LINKED DATA #########
#####################################################################
# OUTCOME: SHARE OF UNMARRIED WOMEN TEACHERS WHO ARE MARRIED & TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
did_graph(dataset = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
          depvarlist = c("pct_mwt", "pct_mwnt", "pct_mwnilf", "pct_sw"), 
          depvarnames = c("Married Teacher", "Married Non-Teacher in LF", "Married Not in LF", "Not Married"),
          colors = c(men_col, mw_col, "grey", sw_col),
          years = c(1920, 1940),
          yvar = "DiD Estimate: Share of Unmarried Women Teachers in t-10",
          ymin = -0.065, ymax = 0.05,
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "linked_swt_neighbor")

# OUTCOME: SHARE OF UNMARRIED WOMEN NON-TEACHERS WHO ARE MARRIED & TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
did_graph(dataset = link2 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
          depvarlist = c("pct_mwt", "pct_mwnt", "pct_mwnilf", "pct_sw"), 
          depvarnames = c("Married Teacher", "Married Non-Teacher in LF", "Married Not in LF", "Not Married"),
          colors = c(men_col, mw_col, "grey", sw_col),
          years = c(1920, 1940),
          yvar = "DiD Estimate: Share of Unmarried Women Non-Teachers in t-10",
          ymin = -0.065, ymax = 0.05,
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "linked_swnt_neighbor")

# OUTCOME: SHARE OF MARRIED WOMEN NON-TEACHERS WHO ARE MARRIED & TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
did_graph(dataset = link3 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
          depvarlist = c("pct_mwt", "pct_mwnt", "pct_mwnilf", "pct_sw"), 
          depvarnames = c("Married Teacher", "Married Non-Teacher in LF", "Married Not in LF", "Not Married"),
          colors = c(men_col, mw_col, "grey", sw_col),
          years = c(1920, 1940),
          yvar = "DiD Estimate: Share of Married Women Non-Teachers in t-10",
          ymin = -0.065, ymax = 0.05,
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "linked_mwnt_neighbor")

#########################################
######### RESULT 3: HH OUTCOMES #########
#########################################
# OUTCOME: SHARE TEACHERS WC/WNC/M (neighbor sample)
did_graph(dataset = neighbor, 
          depvarlist = c("pct_m_Teacher", "pct_wc_Teacher", "pct_wnc_Teacher"), 
          depvarnames = c("Men", "Women with Children", "Women without Children"),
          colors = c(men_col, mw_col, sw_col),
          yvar = "DiD Estimate: Share of Teachers",
          ymin = -0.065, ymax = 0.06,
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "shareteach_children_neighbor")

# OUTCOME: Avg # children (neighbor sample)
did_graph(dataset = neighbor, 
          depvarlist = c("avg_nchild_Teacher", "avg_nchild_Secretary"), 
          depvarnames = c("Women Teachers", "Women Secretaries"),
          colors = c(mw_col, sw_col),
          yvar = "DiD Estimate: Mean # of Children",
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "nchild_neighbor")

# OUTCOME: Avg age at first child (neighbor sample)
did_graph(dataset = neighbor, 
          depvarlist = c("avg_age_child_Teacher", "avg_age_child_Secretary"), 
          depvarnames = c("Women Teachers", "Women Secretaries"),
          colors = c(mw_col, sw_col),
          yvar = "DiD Estimate: Mean Age at First Child",
          verbose = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename = "agechild_neighbor")

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



