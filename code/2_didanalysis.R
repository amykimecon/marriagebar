### CREATING DiD PLOTS (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

## Initializing key datasets
matched <- countysumm %>% filter(match_samp == 1 & mainsamp == 1)
neighbor <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)
all <- countysumm %>% filter(mainsamp == 1)

# testing other versions of matching
matched2 <- countysumm %>% filter(match_samp2 == 1 & mainsamp == 1)
matched3 <- countysumm %>% filter(match_samp3 == 1 & mainsamp == 1)

## Result 1: Share Teachers by Demgroup
depvars1 <- c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher")
depvarlabs1 <- c("Men", "Married Women", "Single Women")
colors1 <- c(men_col, mw_col, sw_col)
did_graph(neighbor, depvars1, depvarlabs1, colors1) + ggtitle("neighbor sample")
did_graph(matched, depvars1, depvarlabs1, colors1) + ggtitle("matched sample: set 1")
did_graph(matched2, depvars1, depvarlabs1, colors1) + ggtitle("matched sample: set 2")
did_graph(matched3, depvars1, depvarlabs1, colors1) + ggtitle("matched sample: set 3")

## Result 2: Num Teachers Total and by Demgroup
depvars2 <- c("num_m_Teacher", "num_mw_Teacher", "num_sw_Teacher")
did_graph(neighbor, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("neighbor sample")
did_graph(matched, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 1")
did_graph(matched2, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 2")
did_graph(matched3, c("num_Teacher","num_Secretary"), c("Teacher", "Secretary"), c("red","blue")) + ggtitle("matched sample: set 3")

did_graph(neighbor, depvars2, depvarlabs1, colors1) + ggtitle("neighbor sample")
did_graph(matched, depvars2, depvarlabs1, colors1) + ggtitle("matched sample: set 1")
did_graph(matched2, depvars2, depvarlabs1, colors1) + ggtitle("matched sample: set 2")
did_graph(matched3, depvars2, depvarlabs1, colors1) + ggtitle("matched sample: set 3")
