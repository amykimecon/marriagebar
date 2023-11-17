### CREATING DiD PLOTS (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

## Initializing key datasets
matched <- countysumm %>% filter(match_samp == 1 & mainsamp == 1)
neighbor <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)
all <- countysumm %>% filter(mainsamp == 1)

# testing other versions of matching
matched2 <- countysumm %>% filter(match_samp2 == 1 & mainsamp == 1)
matched3 <- countysumm %>% filter(match_samp3 == 1 & mainsamp == 1)
matched4 <- countysumm %>% filter(match_samp4 == 1 & mainsamp == 1)

## Graphing distribution of treatment/control counties
graph_treatment(neighbor, filename = "neighbormap") + ggtitle("Neighbor Sample")
graph_treatment(matched, filename = "matchedmap") + ggtitle("Matched Sample: Set 1")
graph_treatment(matched2, filename = "matchedmap2") + ggtitle("Matched Sample: Set 2")
graph_treatment(matched3, filename = "matchedmap3") + ggtitle("Matched Sample: Set 3")
graph_treatment(matched4, filename = "matchedmap4") + ggtitle("Matched Sample: Set 4")

## TESTING MATCHING
test_all <- bind_rows(list(all %>% mutate(sample = "All Counties"),
                           neighbor %>% mutate(sample = "Neighbor Sample"),
                           matched %>% mutate(sample = "Matched Sample: Set 1"),
                           matched2 %>% mutate(sample = "Matched Sample: Set 2"),
                           matched3 %>% mutate(sample = "Matched Sample: Set 3"),
                           matched4 %>% mutate(sample = "Matched Sample: Set 4")))

match_test(test_all %>% filter(sample != "All Counties"), "POP") 
match_test(test_all, "LFP_SW") 
match_test(test_all, "PCT_LF_MW") 
match_test(test_all, "LFP_MW") 
match_test(test_all, "LFP_WMW") 
match_test(test_all, "PCT_WHITE") 
match_test(test_all, "pct_mw_Teacher") 

## Result 1: Share Teachers by Demgroup
depvars1 <- c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher")
depvarlabs1 <- c("Men", "Married Women", "Single Women")
colors1 <- c(men_col, mw_col, sw_col)
did_graph(neighbor, depvars1, depvarlabs1, colors1, filename = "shareteach_neighborsamp") + ggtitle("neighbor sample")
did_graph(matched, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp1") + ggtitle("matched sample: set 1")
did_graph(matched2, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp2") + ggtitle("matched sample: set 2")
did_graph(matched3, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp3") + ggtitle("matched sample: set 3")
did_graph(matched4, depvars1, depvarlabs1, colors1, filename = "shareteach_matchsamp4") + ggtitle("matched sample: set 4")

## Result 2: Num Teachers Total and Share of Workers Teachers
did_graph(neighbor, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("neighbor sample")
did_graph(matched, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 1")
did_graph(matched2, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 2")
did_graph(matched3, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 3")
did_graph(matched4, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 3")
did_graph(matched4, c("num_Teacher","POP"), c("Teacher", "POP"), c("red","blue")) + ggtitle("matched sample: set 4")

depvars2 <- c("pct_workers_Teacher", "pct_workers_Secretary")
depvarlabs2 <- c("Teacher","Secretary")
did_graph(neighbor, depvars2, depvarlabs2, c("red","blue")) + ggtitle("neighbor sample")
did_graph(matched, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 1")
did_graph(matched2, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 2")
did_graph(matched3, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 3")
did_graph(matched4, depvars2, depvarlabs2, c("red","blue")) + ggtitle("matched sample: set 4")

## Result 3: Teacher Student Ratio
depvars3 <- c("teacher_ratio")
depvarlabs3 <- c("Teacher Ratio")
did_graph(neighbor, depvars3, depvarlabs3, c("red")) + ggtitle("neighbor sample")
did_graph(matched, depvars3, depvarlabs3, c("red")) + ggtitle("matched sample: set 1")
did_graph(matched2 %>% filter(!is.na(teacher_ratio) & teacher_ratio != Inf), depvars3, depvarlabs3, c("red"), verbose = TRUE) + ggtitle("matched sample: set 2")
did_graph(matched3 %>% filter(!is.na(teacher_ratio) & teacher_ratio != Inf), depvars3, depvarlabs3, c("red")) + ggtitle("matched sample: set 3")

## Result 4: Share Teachers MA/MB
depvars4 <- c("pct_marr_before_Teacher", "pct_marr_after_Teacher")
depvarlabs4 <- c("Married Before", "Married After")
did_graph(neighbor, depvars4, depvarlabs4, c("red","blue"), years = c(1940), filename = "share_ma_mb_neighbor") + ggtitle("neighbor sample")
did_graph(matched, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 1")
did_graph(matched2, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 2")
did_graph(matched3, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 3")
did_graph(matched4, depvars4, depvarlabs4, c("red","blue"), years = c(1940)) + ggtitle("matched sample: set 3")


### LINKED RESULTS ###
linkvars1 <- c("pct_mwt", "pct_mwnt","pct_mwnilf", "pct_swt", "pct_swnt", "pct_swnilf")
linklabs1 <- c("MW Teacher", "MW Non-Teacher in LF", "MW Not in LF", "SW Teacher", "SW Non-Teacher in LF", "SW Not in LF")
# sample 1: outcomes for unmarried single women in base year
did_graph(link1 %>% filter(neighbor_samp & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940)) + ggtitle("neighbor sample")
did_graph(link1 %>% filter(match_samp4 & mainsamp), linkvars1, linklabs1, gg_color_hue(6), years = c(1920, 1940)) + ggtitle("neighbor sample")
