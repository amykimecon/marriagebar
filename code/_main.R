### MAIN FILE FOR MARRIAGEBAR PROJECT -- RUN TO GENERATE ALL CLEANED DATA, FIGURES AND TABLES
### AUTHOR: AMY KIM
### LAST EDITED: NOV 2023

# importing all packages
library(tidyverse)
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

### setting all filepaths 
if (Sys.info()[["user"]]=="yk0581"){ #section laptop
  root = "C:\\Users\\yk0581\\Dropbox (Princeton)\\marriagebar"
  git = "C:\\Users\\yk0581\\Documents\\GitHub\\marriagebar"
}
if (Sys.info()[["user"]]=="amykim"){ #amys macbook
  root = "~/Dropbox (Princeton)/marriagebar"
  git = "~/GitHub/marriagebar"
}

rawdata = glue("{root}/ipums_raw")
cleandata = glue("{root}/clean_data")
outfigs = glue("{git}/figures")


### colors
mw_col = "#8751A4"
sw_col = "#2D1B37"
men_col = "#C8ADD7"

control_col = "#479F73"
treat_col = "#8751A4"

### helper function
source(glue("{git}/code/helper.R"))

# ### cleaning data (or loading all cleaned datasets) -- comment out one of these
# ## code to clean data
# # source(glue("{git}/code/0_census_clean.R"))
# 
# # loading all cleaned datasets
# samp_byyear <- read_csv(glue("{cleandata}/samp_byyear.csv"))
# filtered_bind <- read_csv(glue("{cleandata}/filtereddata.csv"))
# countydist_byocc <- read_csv(glue("{cleandata}/countydist_byocc.csv"))
# countysumm <- read_csv(glue("{cleandata}/countysumm.csv"))
# countysumm_matched <- read_csv(glue("{cleandata}/countysumm_matched.csv"))
# 
# ### running scripts
# source(glue("{git}/code/1_descriptives.R"))
# source(glue("{git}/code/2_firststage.R"))
# source(glue("{git}/code/3_elasticity_estimation.R"))
# 


