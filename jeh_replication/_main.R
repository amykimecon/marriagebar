### _main.R: REPLICATION OF FIGURES AND TABLES FROM ``The Effects of Prohibiting Marriage Bars: The Case of U.S. Teachers'' 
### UPDATED: DECEMBER 2025
### AUTHORS: AMY KIM (kimamy@princeton.edu) AND CAROLYN TSAO (carolyntsao@microsoft.com)

# TODO: CHANGE <YOUR PATH HERE> TO YOUR RELEVANT PATHS
#root = "<YOUR PATH HERE>/jeh_replication"
root = "/Users/amykim/GitHub/marriagebar/jeh_replication" #TEMP
setwd(root)

#data = paste0(root,"/data") 
# TEMP: POINT TO DROPBOX
data = "~/Dropbox (Princeton)/marriagebar/clean_data" #TEMP

# load packages and functions ----
require(glue)
require(duckdb)
require(tidyverse)

# toggles ----
verbose = TRUE # toggle true to see figures and tables outputted as code runs
save = TRUE # toggle true to save figures and tables to output folders

# set colors ----
mw_col  = "#8751A4"
sw_col  = "#2D1B37"
men_col = "#C8ADD7"

control_col = "#46b97a"
treat_col   = "#B94685"

# importing data ----
## Cross-Sectional County x Year-level Data (used for Tables 1, 2, 4 and Figures 1, 2, 3)
# TODO: notes, fix paths
countysumm      <- read_csv(glue("{data}/countysumm.csv"))
countysumm_blk  <- read_csv(glue("{data}/countysumm_blk.csv"))
countysumm_wht  <- read_csv(glue("{data}/countysumm_wht.csv"))

# filtering to preferred sample of balanced panel of treatment and control counties
neighbor       <- countysumm     %>% filter(neighbor_samp == 1 & mainsampall == 1)
neighbor_blk   <- countysumm_blk %>% filter(neighbor_samp == 1 & mainsampblk == 1)
neighbor_wht   <- countysumm_wht %>% filter(neighbor_samp == 1 & mainsampwht == 1)

## Linked Individual-level Data (used for Tables 3 and 5)
# TODO: notes, fix paths
link1_swt          <- read_csv(glue("{data}/link1_swt_indiv.csv"))
link2_mwnilf       <- read_csv(glue("{data}/link2_mwnilf_indiv.csv"))
link3_swnilf       <- read_csv(glue("{data}/link3_swnilf_indiv.csv"))

# RUNNING HELPER FILE
source("helper.R")

# RUNNING TABLES FILE
source("tables.R")

# RUNNING FIGURES FILE
source("figures.R")
