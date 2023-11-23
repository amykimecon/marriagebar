### DESCRIPTIVES
### AUTHOR: AMY KIM

# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))

##################################################
##### MAP OF TREATMENT & CONTROL COUNTIES ########
##################################################
graph_treatment(countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)) + ggtitle("Neighbor Sample")