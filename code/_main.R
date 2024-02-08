### MAIN FILE FOR MARRIAGEBAR PROJECT -- RUN TO GENERATE ALL CLEANED DATA, FIGURES AND TABLES
### AUTHOR: AMY KIM
### LAST EDITED: FEB 2024

# importing all packages ----
library(glue)
library(ggpubr)
library(xtable)
library(stargazer)
library(usmap)
library(MatchIt)
library(sandwich)
library(lmtest)
library(readxl)
library(duckdb)
library(tictoc)
library(tidyverse)

# set filepaths ----
if (Sys.info()[["user"]]=="yk0581"){ #Amy's section laptop
  root = "C:\\Users\\yk0581\\Dropbox (Princeton)\\marriagebar"
}
if (Sys.info()[["user"]]=="amykim"){ #Amy's macbook
  root = "~/Dropbox (Princeton)/marriagebar"
}
if (Sys.info()[["user"]]=="ctsao"){ #Carolyn's section laptop
  root = "C:/Users/ctsao/Dropbox/marriagebar_data"
}
if (Sys.info()[["user"]]=="carolyn"){ #Carolyn's macbook
  root = "~/Dropbox/marriagebar_data"
}

rawdata   = glue("{root}/ipums_raw")
cleandata = glue("{root}/clean_data")
outfigs   = "./figures"


# set colors ----
mw_col  = "#8751A4"
sw_col  = "#2D1B37"
men_col = "#C8ADD7"

control_col = "#46b97a"
treat_col   = "#B94685"

# RUN ALL ----
## helper functions ----
source("./code/helper.R")

## initial duckdb creation: ONLY NEED TO RUN ONCE EVER ----
# source(glue("./code/duckdb_init.R"))

## cleaning data (or loading all cleaned datasets) ---- run 1) OR 2)
### 1) code to clean data ----
#source("./code/0_dataclean.R")
### 2) loading all cleaned datasets ----
samp_byyear <- read_csv(glue("{cleandata}/samp_byyear.csv")) # year-level summary stats (figure 1)
countysumm  <- read_csv(glue("{cleandata}/countysumm_newmatch.csv")) # county X year-level stats (most analysis uses this)
link1       <- read_csv(glue("{cleandata}/link1_swt.csv"))
link1point5 <- read_csv(glue("{cleandata}/link1point5_wtnc.csv"))
link2       <- read_csv(glue("{cleandata}/link2_swnt.csv"))
link3       <- read_csv(glue("{cleandata}/link3_mwnt.csv"))

## run analysis scripts ----
source("./code/1_descriptives.R")
source("./code/2_didanalysis.R")
source("./code/2_cohortanalysis.R") # requires duckdb connection




