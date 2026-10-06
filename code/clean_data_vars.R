### clean_data_vars.R: REMOVE UNNECESSARY VARIABLES FROM REPLICATION DATA FILES
### Keeps variables used in: tables.R, figures.R, helper.R (main replication)
###                      and: code/2_didanalysis.R, code/4_robustness.R (appendix)
### Overwrites files in Dropbox clean_data folder

library(tidyverse)

data_dir <- "~/Dropbox (Princeton)/marriagebar/clean_data"
out_dir  <- "~/Dropbox (Princeton)/marriagebar/clean_data/replication"
dir.create(out_dir, showWarnings = FALSE)

#________________________________________________________________________________________________________
# countysumm.csv / countysumm_wht.csv / countysumm_blk.csv ----
# All three files share the same variable set; only the mainsampl* indicator differs.
#
# Main replication:
#   countysumm     — Table 1 (summary stats), Table 2, Table 4, Figures 2-3
#   countysumm_wht — Table 2 (white sample), Table 4, Figure 1 (density), Figure 3a
#   countysumm_blk — Table 2 (black sample), Figure 3b
# Appendix (code/2_didanalysis.R, code/4_robustness.R):
#   match_weight*           — matched-sample robustness
#   pct_*_Secretary         — secretary placebo check
#   share_manuf/ag, POP,
#   UNEMP_RATE              — industry/pop controls (Table B6)
#   bordertreat/borderctrl  — border county robustness
#________________________________________________________________________________________________________
countysumm_shared_vars <- c(
  # identifiers
  "YEAR", "FIPS", "STATEICP", "COUNTYICP",
  # treatment and geographic
  "SOUTH", "TREAT",
  # sample indicator (mainsampl* added per-file below)
  "neighbor_samp",
  # Table 1: summary stats
  "POP", "SCHOOLPOP", "URBAN",
  "LFP_MW", "LFP_WMW", "NCHILD", "UNEMP_RATE",
  "num_Teacher",
  "pct_m_Teacher", "pct_sw_Teacher", "pct_mw_Teacher",
  # Table 2 / Table 4
  "pct_Teacher_mw_100",
  # Appendix: matched-sample robustness (4_robustness.R)
  "match_weight1", "match_weight2", "match_weight3",
  # Appendix: secretary placebo check (4_robustness.R) + Figure 1 comparison series
  "pct_m_Secretary", "pct_mw_Secretary", "pct_sw_Secretary",
  # Appendix: border county robustness (4_robustness.R)
  "bordertreat", "borderctrl"
)

for (info in list(
  list(fname = "countysumm.csv",     mainsampl = "mainsampall"),
  list(fname = "countysumm_wht.csv", mainsampl = "mainsampwht"),
  list(fname = "countysumm_blk.csv", mainsampl = "mainsampblk")
)) {
  df <- read_csv(file.path(data_dir, info$fname))
  df_clean <- df %>% select(any_of(c(countysumm_shared_vars, info$mainsampl)))
  write_csv(df_clean, file.path(out_dir, info$fname))
  cat(info$fname, ": kept", ncol(df_clean), "of", ncol(df), "variables\n")
}

#________________________________________________________________________________________________________
# link1_swt_indiv.csv, link2_mwnilf_indiv.csv, link3_swnilf_indiv.csv ----
# Used in: Table 3 (marriage/work propensity regs), Table 5 (link1 only: unmarried women outcomes)
# did_data_indiv_linked uses: STATEICP, COUNTYICP, YEAR, TREAT, AGE (fixed effects + age control)
# Appendix: inverse weighting (nlink), urban heterogeneity (URBAN),
#           net effects analysis (OCCSCORE_link, NCHILD_link, worker_link, move_state),
#           matched robustness (match_weight*), border filtering (FIPS)
#________________________________________________________________________________________________________
linked_vars <- c(
  # identifiers
  "STATEICP", "COUNTYICP", "YEAR",
  # treatment and geographic
  "SOUTH", "TREAT", "FIPS",
  # individual characteristics used in regression
  "RACE", "AGE",
  # sample indicator
  "neighbor_samp",
  # Table 3: outcomes (married women samples)
  "married_link", "mwt_link", "mwnt_link", "mwnilf_link",
  # Table 5: outcomes (single women sample, link1 only — kept in all for consistency)
  "unmarried_link", "swt_link", "swnt_link", "swnilf_link",
  # Appendix: inverse weighting by link count (2_didanalysis.R)
  "nlink",
  # Appendix: urban heterogeneity analysis (2_didanalysis.R)
  "URBAN",
  # Appendix: net effects outcomes — occ score, children, work status, migration (2_didanalysis.R)
  "OCCSCORE_link", "NCHILD_link", "worker_link", "move_state",
  # Appendix: matched-sample robustness (4_robustness.R)
  "match_weight1", "match_weight2", "match_weight3"
)

for (fname in c("link1_swt_indiv.csv", "link2_mwnilf_indiv.csv", "link3_swnilf_indiv.csv")) {
  df <- read_csv(file.path(data_dir, fname))
  df_clean <- df %>% select(any_of(linked_vars))
  write_csv(df_clean, file.path(out_dir, fname))
  cat(fname, ": kept", ncol(df_clean), "of", ncol(df), "variables\n")
}
