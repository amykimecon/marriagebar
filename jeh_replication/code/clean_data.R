### clean_data.R: RECONSTRUCT REPLICATION DATA FILES FROM RAW IPUMS AND CENSUS TREE DATA
### Authors: Amy Kim (kimamy@princeton.edu) and Carolyn Tsao (tsaocarolyn.econ@gmail.com)
###
### This script recreates the 6 data files used in the replication package:
###   - countysumm.csv, countysumm_wht.csv, countysumm_blk.csv  [OPTIONAL: already provided]
###   - link1_swt_indiv.csv, link2_mwnilf_indiv.csv, link3_swnilf_indiv.csv  [REQUIRED]
###
### The countysumm*.csv files are provided directly in the replication package and do NOT
### need to be regenerated unless you wish to verify them from scratch. They are needed
### as inputs to the linking step regardless (to supply matching weights), so if you have
### not run the optional Section 2, the script will read them from data/.
###
### The link1-3 files contain individual-level Census microdata and cannot be distributed
### under IPUMS terms of use. They must be constructed by running this script.
###
### See README.md "Obtaining and Preparing Raw Data" for instructions on downloading
### the required raw input files before running this script.
###
### Required packages: duckdb, DBI, tidyverse, glue, readxl, MatchIt

# ============================================================================
# SECTION 0: SETUP
# ============================================================================
# All user-configurable settings (paths, ipums_format, run_county_section)
# are set in _main.R. Run _main.R rather than this script directly.

# Derived paths (do not edit)
data_out   <- file.path(root, "data")            # output folder for generated data files
fips_xwalk <- file.path(data_out, "StateFIPSicsprAB.xls")         # IPUMS state FIPS crosswalk
adj_file   <- file.path(data_out, "county_adjacency2010.txt")      # Census Bureau county adjacency file

# --- Open DuckDB connection ---
# DuckDB is used for memory-efficient processing of large IPUMS full-count files.
# A temporary in-memory database is used; no .duckdb file is saved to disk.
con <- dbConnect(duckdb(), dbdir = ":memory:")

# ============================================================================
# SECTION 1: LOAD RAW IPUMS DATA INTO DUCKDB
# ============================================================================
# Supports two extract formats (set ipums_format in _main.R):
#   "multi"  — one CSV per census year: census_1910.csv, ..., census_1950.csv in rawdata/
#   "single" — one combined CSV for all years: ipums_all.csv in rawdata/
#              (must contain a YEAR column identifying the census year)
#
# In both cases, this section produces:
#   censusraw{yr}  — per-year DuckDB TABLEs (materialised) used in Section 3 joins
#   censusrawall   — UNION ALL view over those tables, used in Section 2 aggregates
#
# For "multi":  censusraw{yr} is a lazy VIEW over each per-year CSV file.
# For "single": censusraw{yr} is a materialised TABLE, populated by scanning
#   ipums_all.csv once per year. The alternative — filtering a lazy view of the
#   full combined file inside a join — causes DuckDB to build a hash table of
#   all census years before applying the year predicate, exhausting memory.

cat("\n=== SECTION 1: Loading raw IPUMS data into DuckDB ===\n")

census_years <- c(1910, 1920, 1930, 1940, 1950)

if (ipums_format == "single") {

  # --- Single combined extract (ipums_all.csv) ---
  ipums_all_path <- file.path(rawdata, "ipums_all.csv")
  if (!file.exists(ipums_all_path)) stop(glue("ipums_all.csv not found in: {rawdata}"))

  # Verify YEAR column is present (required to distinguish census years)
  csv_cols <- dbGetQuery(con,
    glue("SELECT column_name FROM (DESCRIBE FROM read_csv_auto('{ipums_all_path}') LIMIT 0)")
  )$column_name
  if (!"YEAR" %in% toupper(csv_cols)) {
    stop("ipums_all.csv must contain a YEAR column to identify individual census years.")
  }

  # Materialise each year as a separate DuckDB table (one sequential scan per year).
  # Peak memory during each scan is one year's rows; after all scans, the tables are
  # compact columnar storage. Subsequent joins in Section 3 use these small tables
  # rather than re-scanning and re-filtering the full combined file each time.
  cat("Materialising per-year tables from ipums_all.csv (one scan per year)...\n")
  for (yr in census_years) {
    cat(glue("  {yr}...\n"))
    dbExecute(con, glue(
      "CREATE OR REPLACE TABLE censusraw{yr} AS
       SELECT * FROM read_csv_auto('{ipums_all_path}') WHERE YEAR = {yr}"
    ))
  }

} else {

  # --- Multiple per-year extracts (census_1910.csv ... census_1950.csv) ---
  # Register each census CSV as a DuckDB VIEW — no data is read at this step.
  # DuckDB reads directly from disk when a query is executed, so all scanning
  # happens at the collect() call, which is the expected slow step.
  cat("Registering census CSV files as DuckDB views...\n")
  for (yr in census_years) {
    # Read only the CSV header (LIMIT 0) to check whether YEAR is already a column
    csv_cols <- dbGetQuery(con,
      glue("SELECT column_name FROM (DESCRIBE FROM read_csv_auto('{rawdata}/census_{yr}.csv') LIMIT 0)")
    )$column_name
    if ("YEAR" %in% toupper(csv_cols)) {
      dbExecute(con, glue(
        "CREATE OR REPLACE VIEW censusraw{yr} AS
         FROM read_csv_auto('{rawdata}/census_{yr}.csv')"
      ))
    } else {
      dbExecute(con, glue(
        "CREATE OR REPLACE VIEW censusraw{yr} AS
         SELECT *, {yr}::INTEGER AS YEAR
         FROM read_csv_auto('{rawdata}/census_{yr}.csv')"
      ))
    }
  }

}

# censusrawall is a UNION ALL over the per-year tables/views, used in Section 2.
# DuckDB can push column projections and year filters into each member.
cat("Registering censusrawall view (union of all years)...\n")
union_parts <- paste(glue("SELECT * FROM censusraw{census_years}"),
                     collapse = "\nUNION ALL BY NAME\n")
dbExecute(con, paste("CREATE OR REPLACE VIEW censusrawall AS", union_parts))

# ============================================================================
# SECTION 2 (OPTIONAL): BUILD COUNTY-LEVEL DATA 
# ============================================================================
# Skip this section if you trust the provided countysumm*.csv files in data/.
# The provided files are identical to what this section produces.
# If you run this section, it will overwrite those files.
#
# NOTE: This section requires the county_adjacency2010.txt and StateFIPSicsprAB.xls
# auxiliary files to be present in data/. See README.md for download instructions.

cat("\n=== SECTION 2: Building county-level data (optional) ===\n")

if (run_county_section) {
  # --- 2a: Load FIPS crosswalk ---
  fipscrosswalk <- read_xls(fips_xwalk)   # crosswalk: STICPSR → state FIPS + abbreviation

  # --- 2b: Helper: add FIPS, treatment, and geographic indicators ---
  # Converts STATEICP + COUNTYICP to 5-digit FIPS; adds SOUTH, TREAT, neighbor_samp
  add_geo_vars <- function(df) {
    df %>%
      left_join(fipscrosswalk, by = c("STATEICP" = "STICPSR")) %>%
      mutate(
        FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),
                         str_pad(COUNTYICP, 4, "left", pad = "0")),
        FIPS = ifelse(substr(FIPSRAW, 6, 6) == "0", substr(FIPSRAW, 1, 5), NA)
      ) %>%
      select(-any_of(c("NAME", "STNAME", "FIPSRAW", "AB"))) %>%
      mutate(
        SOUTH        = ifelse(STATEICP %in% c(54, 41, 46, 51, 40, 56, 47, 11, 52, 48, 43, 44), 1, 0),
        TREAT        = ifelse(STATEICP %in% c(47, 51), 1, 0),             # TN=47, VA=51
        neighbor_samp = ifelse(STATEICP %in% c(47, 51, 40, 48, 54, 56), 1, 0)  # TN, VA, NC, SC, WV, KY
      )
  }

  # --- 2c: Build general county-level aggregates (all individuals) ---
  cat("Computing general county aggregates...\n")
  countysumm_gen <- tbl(con, "censusrawall") %>%
    mutate(
      demgroup  = case_when(SEX == 1 ~ "M",
                            SEX == 2 & MARST != 1 & MARST != 2 ~ "SW",
                            TRUE ~ "MW"),
      worker    = ifelse(LABFORCE == 2 & AGE >= 16 & AGE <= 64, 1, 0)
    ) %>%
    group_by(YEAR, STATEICP, COUNTYICP) %>%
    summarize(
      POP       = n(),
      SCHOOLPOP = sum(ifelse(AGE >= 6  & AGE <= 18, 1, 0), na.rm = TRUE),
      URBAN     = sum(ifelse(URBAN == 2, 1, 0), na.rm = TRUE) / n(),
      NMW       = sum(ifelse(demgroup == "MW" & AGE >= 16, 1, 0), na.rm = TRUE),
      NWHITEMW  = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 16 & AGE <= 64, 1, 0), na.rm = TRUE),
      NBLACKMW  = sum(ifelse(demgroup == "MW" & RACE == 2 & AGE >= 16 & AGE <= 64, 1, 0), na.rm = TRUE),
      LFP       = sum(ifelse(worker == 1, 1, 0), na.rm = TRUE) /
                  sum(ifelse(AGE >= 16 & AGE <= 64, 1, 0), na.rm = TRUE),
      LFP_MW    = sum(ifelse(worker == 1 & demgroup == "MW", 1, 0), na.rm = TRUE) /
                  sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "MW", 1, 0), na.rm = TRUE),
      LFP_WMW   = sum(ifelse(worker == 1 & demgroup == "MW" & RACE == 1, 1, 0), na.rm = TRUE) /
                  sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "MW" & RACE == 1, 1, 0), na.rm = TRUE),
      NCHILD    = mean(ifelse(demgroup == "MW", NCHILD, NA), na.rm = TRUE),
      UNEMP_RATE = sum(ifelse(EMPSTAT == 2 & AGE >= 16 & LABFORCE == 2, 1, 0), na.rm = TRUE) /
                   sum(ifelse(AGE >= 16 & LABFORCE == 2, 1, 0), na.rm = TRUE),
      SHARE_MANUF = sum(ifelse(LABFORCE == 2 & AGE >= 16 & IND1950 >= 300 & IND1950 < 400, 1, 0), na.rm = TRUE) /
                    sum(ifelse(LABFORCE == 2 & AGE >= 16, 1, 0), na.rm = TRUE),
      SHARE_AG = sum(ifelse(LABFORCE == 2 & AGE >= 16 & IND1950 >= 100 & IND1950 < 200, 1, 0), na.rm = TRUE) /
                    sum(ifelse(LABFORCE == 2 & AGE >= 16, 1, 0), na.rm = TRUE),
      PCT_UNDER20 = sum(ifelse(AGE < 20, 1, 0), na.rm = TRUE) / n(),
      PCT_20TO39  = sum(ifelse(AGE >= 20 & AGE < 40, 1, 0), na.rm = TRUE) / n(),
      PCT_40TO59  = sum(ifelse(AGE >= 40 & AGE < 60, 1, 0), na.rm = TRUE) / n(),
      PCT_WHITE   = sum(ifelse(RACE == 1, 1, 0), na.rm = TRUE) / n(),
      .groups = "drop"
    ) %>%
    collect() %>%
    add_geo_vars()
  
  # --- 2d: Build occupation-level aggregates (teachers and secretaries) ---
  # Runs separately for all/white/Black teachers
  build_occ_summ <- function(race_filter_expr) {
    tbl(con, "censusrawall") %>%
      mutate(
        demgroup  = case_when(SEX == 1 ~ "M",
                              SEX == 2 & MARST != 1 & MARST != 2 ~ "SW",
                              TRUE ~ "MW"),
        worker    = ifelse(LABFORCE == 2 & AGE >= 16 & AGE <= 64, 1, 0),
        teacher   = ifelse(OCC1950 == 93  & CLASSWKR == 2 & worker == 1, 1, 0),
        secretary = ifelse(OCC1950 == 350 & CLASSWKR == 2 & worker == 1, 1, 0)
      ) %>%
      filter(!!rlang::parse_expr(race_filter_expr)) %>%
      filter(teacher == 1 | secretary == 1) %>%
      mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
      group_by(YEAR, STATEICP, COUNTYICP, OCC) %>%
      summarize(
        num    = n(),
        num_mw = sum(ifelse(demgroup == "MW", 1, 0), na.rm = TRUE),
        pct_mw = sum(ifelse(demgroup == "MW", 1, 0), na.rm = TRUE) / n(),
        pct_sw = sum(ifelse(demgroup == "SW", 1, 0), na.rm = TRUE) / n(),
        pct_m  = sum(ifelse(demgroup == "M",  1, 0), na.rm = TRUE) / n(),
        .groups = "drop"
      ) %>%
      collect() %>%
      pivot_wider(
        id_cols     = c(YEAR, STATEICP, COUNTYICP),
        names_from  = OCC,
        values_from = -c(YEAR, STATEICP, COUNTYICP, OCC)
      )
  }

  cat("Computing teacher/secretary occupational aggregates...\n")
  occ_all <- build_occ_summ("TRUE")             # all races
  occ_wht <- build_occ_summ("RACE == 1")        # white only
  occ_blk <- build_occ_summ("RACE == 2")        # Black only

  # --- 2e: Merge general + occupation data for each sample ---
  merge_county <- function(gen, occ, mw_denom_col) {
    gen %>%
      full_join(occ, by = c("YEAR", "STATEICP", "COUNTYICP")) %>%
      mutate(
        pct_Teacher_mw_100 = (num_mw_Teacher / !!sym(mw_denom_col)) * 100
      ) %>%
      ungroup()
  }

  countysumm_raw_all <- merge_county(countysumm_gen, occ_all, "NMW")
  countysumm_raw_wht <- merge_county(countysumm_gen, occ_wht, "NWHITEMW")
  countysumm_raw_blk <- merge_county(countysumm_gen, occ_blk, "NBLACKMW")

  # --- 2f: Sample indicators ---
  cat("Computing main sample indicators...\n")
  mainsamp_fn <- function(df, teach_col = "num_Teacher", n = 10) {
    balanced <- df %>%
      filter(!!sym(teach_col) > 0) %>%
      group_by(FIPS) %>%
      summarize(n_yrs = n_distinct(YEAR)) %>%
      filter(n_yrs == 5) %>%
      pull(FIPS)
    fips30 <- filter(df, YEAR == 1930 & !!sym(teach_col) >= n)$FIPS
    fips40 <- filter(df, YEAR == 1940 & !!sym(teach_col) >= n)$FIPS
    intersect(intersect(balanced, fips30), fips40) %>% na.omit() %>% unique()
  }

  mainsamp_list    <- mainsamp_fn(countysumm_raw_all)
  mainsampwht_list <- mainsamp_fn(countysumm_raw_wht)
  mainsampblk_list <- mainsamp_fn(countysumm_raw_blk)

  # --- 2g: Border county indicators ---
  cat("Computing border county indicators...\n")
  borders <- read.table(adj_file, sep = "\t",
                        col.names = c("county_name", "FIPS", "border_name", "border_FIPS")) %>%
    mutate(
      county_name = ifelse(county_name == "", NA, county_name),
      FIPS        = str_pad(as.character(FIPS),        5, "left", pad = "0"),
      border_FIPS = str_pad(as.character(border_FIPS), 5, "left", pad = "0")
    ) %>%
    fill(c(county_name, FIPS), .direction = "down") %>%
    mutate(
      state       = substr(FIPS, 1, 2),
      border_state = substr(border_FIPS, 1, 2),
      border      = ifelse(state != border_state, 1, 0),
      border_ctrl  = ifelse(border == 1 & border_state %in% c("37", "21"), 1, 0),
      border_treat = ifelse(border == 1 & state        %in% c("37", "21"), 1, 0)
    )
  border_treat_fips <- unique(filter(borders, border_treat == 1)$FIPS)
  border_ctrl_fips  <- unique(filter(borders, border_ctrl  == 1)$FIPS)

  # --- 2h: Matching weights using MatchIt ---
  # Three matching methods are applied to 1930 county characteristics + 1920-1930 growth.
  # See Online Appendix A.2 for details on the matching procedure.

  matchvars <- c("URBAN", "LFP", "LFP_MW", "POP",
                 "PCT_UNDER20", "PCT_20TO39", "PCT_40TO59",
                 "PCT_WHITE", "SHARE_MANUF", "SHARE_AG",
                 "pct_sw_Teacher", "pct_mw_Teacher")

  # Helper: merge retail sales data (with county code corrections for historical mapping)
  add_retailsales <- function(df) {
    rs <- read_xls(retailsales_file)
    df %>%
      mutate(COUNTYTEMP = case_when(
        STATEICP == 13 & COUNTYICP == 50   ~ 1500, STATEICP == 13 & COUNTYICP == 470  ~ 1500,
        STATEICP == 13 & COUNTYICP == 610  ~ 1500, STATEICP == 13 & COUNTYICP == 810  ~ 1500,
        STATEICP == 13 & COUNTYICP == 850  ~ 1500, STATEICP == 34 & COUNTYICP == 1890 ~ 3000,
        STATEICP == 34 & COUNTYICP == 5100 ~ 3000, STATEICP == 40 & COUNTYICP == 30   ~ 2000,
        STATEICP == 40 & COUNTYICP == 5400 ~ 2000, STATEICP == 40 & COUNTYICP == 50   ~ 2100,
        STATEICP == 40 & COUNTYICP == 5600 ~ 2100, STATEICP == 40 & COUNTYICP == 130  ~ 2150,
        STATEICP == 40 & COUNTYICP == 5100 ~ 2150, STATEICP == 40 & COUNTYICP == 150  ~ 2200,
        STATEICP == 40 & COUNTYICP == 7900 ~ 2200, STATEICP == 40 & COUNTYICP == 310  ~ 2300,
        STATEICP == 40 & COUNTYICP == 6800 ~ 2300, STATEICP == 40 & COUNTYICP == 530  ~ 2400,
        STATEICP == 40 & COUNTYICP == 7300 ~ 2400, STATEICP == 40 & COUNTYICP == 550  ~ 2500,
        STATEICP == 40 & COUNTYICP == 6500 ~ 2500, STATEICP == 40 & COUNTYICP == 690  ~ 2600,
        STATEICP == 40 & COUNTYICP == 8400 ~ 2600, STATEICP == 40 & COUNTYICP == 870  ~ 2700,
        STATEICP == 40 & COUNTYICP == 7600 ~ 2700, STATEICP == 40 & COUNTYICP == 890  ~ 2800,
        STATEICP == 40 & COUNTYICP == 6900 ~ 2800, STATEICP == 40 & COUNTYICP == 950  ~ 2900,
        STATEICP == 40 & COUNTYICP == 8300 ~ 2900, STATEICP == 40 & COUNTYICP == 1210 ~ 3000,
        STATEICP == 40 & COUNTYICP == 7500 ~ 3000, STATEICP == 40 & COUNTYICP == 1230 ~ 3100,
        STATEICP == 40 & COUNTYICP == 8000 ~ 3100, STATEICP == 40 & COUNTYICP == 1290 ~ 3200,
        STATEICP == 40 & COUNTYICP == 7100 ~ 3200, STATEICP == 40 & COUNTYICP == 7850 ~ 3200,
        STATEICP == 40 & COUNTYICP == 7400 ~ 3200, STATEICP == 40 & COUNTYICP == 1430 ~ 3300,
        STATEICP == 40 & COUNTYICP == 5900 ~ 3300, STATEICP == 40 & COUNTYICP == 1490 ~ 3400,
        STATEICP == 40 & COUNTYICP == 6700 ~ 3400, STATEICP == 40 & COUNTYICP == 1610 ~ 3500,
        STATEICP == 40 & COUNTYICP == 7700 ~ 3500, STATEICP == 40 & COUNTYICP == 1630 ~ 3600,
        STATEICP == 40 & COUNTYICP == 5300 ~ 3600, STATEICP == 40 & COUNTYICP == 1650 ~ 3700,
        STATEICP == 40 & COUNTYICP == 6600 ~ 3700, STATEICP == 40 & COUNTYICP == 1770 ~ 3800,
        STATEICP == 40 & COUNTYICP == 6300 ~ 3800, STATEICP == 40 & COUNTYICP == 1875 ~ 3900,
        STATEICP == 40 & COUNTYICP == 7000 ~ 3900, STATEICP == 40 & COUNTYICP == 1910 ~ 4000,
        STATEICP == 40 & COUNTYICP == 5200 ~ 4000, STATEICP == 44 & COUNTYICP == 410  ~ 1210,
        STATEICP == 44 & COUNTYICP == 2030 ~ 1210, STATEICP == 44 & COUNTYICP == 1210 ~ 1210,
        TRUE ~ COUNTYICP
      )) %>%
      left_join(rs %>% select(STATE, NDMTCODE, RRTSAP29, RRTSAP33, RRTSAP39,
                               RLDF3929, RLDF3329, RLDF3933),
                by = c("STATEICP" = "STATE", "COUNTYTEMP" = "NDMTCODE")) %>%
      select(-COUNTYTEMP)
  }

  # Helper: run one MatchIt call and return FIPS + weights
  run_match <- function(longdata, matchvars, method, pop.size = 200) {
    retail_vars    <- c("RRTSAP29", "RLDF3329", "RLDF3933")
    filter_varnames <- c(glue("{matchvars}_1930"), glue("{matchvars}_growth1930"), retail_vars)

    matchdata <- longdata %>%
      filter(FIPS %in% mainsamp_list) %>%
      pivot_wider(id_cols    = c(FIPS, STATEICP, COUNTYICP, TREAT),
                  names_from = YEAR,
                  values_from = all_of(matchvars)) %>%
      add_retailsales()

    for (var in matchvars) {
      v30 <- glue("{var}_1930"); v20 <- glue("{var}_1920")
      if (any(longdata[[var]] == 0, na.rm = TRUE)) {
        matchdata[[v30]] <- matchdata[[v30]] + 0.01
        matchdata[[v20]] <- matchdata[[v20]] + 0.01
      }
      matchdata[[glue("{var}_growth1930")]] <-
        (matchdata[[v30]] - matchdata[[v20]]) / matchdata[[v20]]
    }

    matchdata <- matchdata %>%
      filter(if_all(all_of(filter_varnames), ~ !is.na(.) & . != Inf))

    match_obj <- matchit(
      reformulate(filter_varnames, response = "TREAT"),
      data     = matchdata,
      method   = method,
      distance = "robust_mahalanobis",
      pop.size = pop.size
    )
    match.data(match_obj) %>% select(FIPS, weights)
  }

  cat("Running matching (method 1: nearest neighbor)...\n")
  m1 <- run_match(countysumm_raw_all, matchvars, method = "nearest")
  cat("Running matching (method 2: genetic, ~5 min)...\n")
  m2 <- run_match(countysumm_raw_all, matchvars, method = "genetic", pop.size = 200)
  cat("Running matching (method 3: full)...\n")
  m3 <- run_match(countysumm_raw_all, matchvars, method = "full")

  # Combine into one county-level dataframe (one row per FIPS)
  match_weights <- m1 %>% rename(match_weight1 = weights) %>%
    full_join(m2 %>% rename(match_weight2 = weights), by = "FIPS") %>%
    full_join(m3 %>% rename(match_weight3 = weights), by = "FIPS") %>%
    mutate(across(starts_with("match_weight"), ~ ifelse(is.na(.), 0, .)))

  # --- 2i: Finalize and save county files ---
  finalize_county <- function(df, mainsamp_col, mainsamp_fips) {
    df %>%
      left_join(match_weights, by = "FIPS") %>%
      mutate(
        mainsampall = ifelse(FIPS %in% mainsamp_list,    1, 0),
        mainsampwht = ifelse(FIPS %in% mainsampwht_list, 1, 0),
        mainsampblk = ifelse(FIPS %in% mainsampblk_list, 1, 0),
        bordertreat = ifelse(FIPS %in% border_treat_fips, 1, 0),
        borderctrl  = ifelse(FIPS %in% border_ctrl_fips,  1, 0)
      )
  }

  # Shared variable list
  county_vars_shared <- c(
    "YEAR", "FIPS", "STATEICP", "COUNTYICP",
    "SOUTH", "TREAT", "neighbor_samp",
    "POP", "SCHOOLPOP", "URBAN",
    "LFP_MW", "LFP_WMW", "NCHILD", "UNEMP_RATE",
    "num_Teacher", "pct_m_Teacher", "pct_sw_Teacher", "pct_mw_Teacher",
    "pct_Teacher_mw_100",
    "match_weight1", "match_weight2", "match_weight3",
    "pct_m_Secretary", "pct_mw_Secretary", "pct_sw_Secretary",
    "bordertreat", "borderctrl"
  )

  cat("Saving county-level files...\n")
  finalize_county(countysumm_raw_all, "mainsampall", mainsamp_list) %>%
    mutate(across(everything(), ~.)) %>%   # ensure all cols materialized
    select(any_of(c(county_vars_shared, "mainsampall"))) %>%
    write_csv(file.path(data_out, "countysumm.csv"))

  finalize_county(countysumm_raw_wht, "mainsampwht", mainsampwht_list) %>%
    select(any_of(c(county_vars_shared, "mainsampwht"))) %>%
    write_csv(file.path(data_out, "countysumm_wht.csv"))

  finalize_county(countysumm_raw_blk, "mainsampblk", mainsampblk_list) %>%
    select(any_of(c(county_vars_shared, "mainsampblk"))) %>%
    write_csv(file.path(data_out, "countysumm_blk.csv"))

  cat("County-level files saved to data/.\n")

} else{
  print("Skipping Section")
}# end if (run_county_section)

# ============================================================================
# SECTION 3: LOAD CENSUS TREE LINKED DATA INTO DUCKDB
# ============================================================================
# Census Tree linking files connect individuals across adjacent decennial censuses.
# Expected input: CSV files named 1910_1920.csv, 1920_1930.csv, 1930_1940.csv
# in the censustreepath folder, following the Census Tree data format.
# Each file contains columns histid{base_year} and histid{link_year} (IPUMS HISTID values).
#
# We use a clean explicit-rename approach: we select and rename all needed columns
# before joining, avoiding DuckDB's implicit ":1" suffix behavior.

cat("\n=== SECTION 3: Building Census Tree linked panels ===\n")

# Columns to carry from each census year (base and link)
link_vars <- c("YEAR", "STATEICP", "COUNTYICP", "SEX", "AGE", "MARST", "RACE",
               "URBAN", "LABFORCE", "CLASSWKR", "OCC1950", "NCHILD", "OCCSCORE", "EMPSTAT")

for (linkyear in c(1920, 1930, 1940)) {
  baseyear <- linkyear - 10
  cat(glue("Linking {baseyear} → {linkyear}...\n"))

  # Register Census Tree linking file as a view (read lazily during the JOIN below)
  dbExecute(con, glue(
    "CREATE OR REPLACE VIEW link{linkyear} AS
     FROM read_csv_auto('{censustreepath}/{baseyear}_{linkyear}.csv')"
  ))

  # Detect Census Tree column names — files may use histid1910/histid1920 or histid_1910/histid_1920
  link_cols   <- dbGetQuery(con, glue("DESCRIBE link{linkyear}"))$column_name
  histid_base <- grep(as.character(baseyear), link_cols, value = TRUE, ignore.case = TRUE)[1]
  histid_link <- grep(as.character(linkyear), link_cols, value = TRUE, ignore.case = TRUE)[1]
  if (is.na(histid_base) || is.na(histid_link)) {
    stop(glue(
      "Cannot find HISTID columns in {baseyear}_{linkyear}.csv.\n",
      "Columns found: {paste(link_cols, collapse = ', ')}\n",
      "Expected columns containing '{baseyear}' and '{linkyear}'."
    ))
  }

  # Check that HISTID exists in the census raw files (required for linking; must be in IPUMS download)
  # Preserve the actual column name (may be lowercase "histid" depending on IPUMS download settings)
  census_cols_base <- dbGetQuery(con, glue("DESCRIBE censusraw{baseyear}"))$column_name
  census_cols_link <- dbGetQuery(con, glue("DESCRIBE censusraw{linkyear}"))$column_name
  histid_census_base <- census_cols_base[toupper(census_cols_base) == "HISTID"][1]
  histid_census_link <- census_cols_link[toupper(census_cols_link) == "HISTID"][1]
  if (is.na(histid_census_base) || is.na(histid_census_link)) {
    stop(glue(
      "HISTID column not found in census_{baseyear}.csv or census_{linkyear}.csv.\n",
      "HISTID must be included in your IPUMS download to enable Census Tree linking.\n",
      "See README.md Step 1 for the required variable list."
    ))
  }

  # Verify all link_vars columns are present in both census files
  # (case-insensitive: IPUMS full-count exports may use uppercase or lowercase headers)
  missing_base <- setdiff(link_vars, toupper(census_cols_base))
  missing_link_yr <- setdiff(link_vars, toupper(census_cols_link))
  if (length(missing_base) > 0 || length(missing_link_yr) > 0) {
    stop(glue(
      "Missing columns needed for {baseyear}→{linkyear} link:\n",
      if (length(missing_base) > 0)
        glue("  census_{baseyear}.csv missing: {paste(missing_base, collapse=', ')}\n") else "",
      if (length(missing_link_yr) > 0)
        glue("  census_{linkyear}.csv missing: {paste(missing_link_yr, collapse=', ')}\n") else "",
      "These must be included in your IPUMS download. See README.md Step 1."
    ))
  }

  # Map link_vars to their actual column names (preserving case as found in the CSV)
  actual_base <- census_cols_base[match(link_vars, toupper(census_cols_base))]
  actual_link <- census_cols_link[match(link_vars, toupper(census_cols_link))]

  # Build SELECT clause for base-year columns (all renamed to _base suffix)
  base_select <- paste(
    glue("b.\"{actual_base}\" AS {link_vars}_base"),
    collapse = ",\n    "
  )
  # Build SELECT clause for link-year columns (all renamed to _link suffix)
  link_select <- paste(
    glue("l.\"{actual_link}\" AS {link_vars}_link"),
    collapse = ",\n    "
  )

  # Look up the actual CSV column names for the WHERE clause predicates,
  # consistent with the case-preserving lookup used for base_select / link_select above.
  sex_col_base  <- actual_base[match("SEX",  link_vars)]
  sex_col_link  <- actual_link[match("SEX",  link_vars)]
  race_col_base <- actual_base[match("RACE", link_vars)]
  race_col_link <- actual_link[match("RACE", link_vars)]
  age_col_base  <- actual_base[match("AGE",  link_vars)]
  age_col_link  <- actual_link[match("AGE",  link_vars)]

  # Push the consistency filters and a women-only restriction into the view so
  # DuckDB can apply them during the join scan — before any rows are materialised.
  # All three output samples are women, so SEX = 2 discards ~half the data early.
  # Sex/race consistency and the age-gap window further prune the join result.
  dbExecute(con, glue(
    "CREATE OR REPLACE VIEW linked{linkyear} AS
     SELECT
       {base_select},
       {link_select}
     FROM censusraw{baseyear} b
     JOIN link{linkyear} lk ON (b.\"{histid_census_base}\" = lk.\"{histid_base}\")
     JOIN censusraw{linkyear} l ON (lk.\"{histid_link}\" = l.\"{histid_census_link}\")
     WHERE b.\"{sex_col_base}\" = 2
       AND l.\"{sex_col_link}\" = 2
       AND b.\"{race_col_base}\" = l.\"{race_col_link}\"
       AND l.\"{age_col_link}\" - b.\"{age_col_base}\" BETWEEN 5 AND 15"
  ))
}

# ============================================================================
# SECTION 4: BUILD LINKED INDIVIDUAL PANELS
# ============================================================================
# We define outcome variables in DuckDB, then collect and filter to three
# baseline population samples. Consistency filters (sex/race/age-gap) and the
# women-only restriction are already enforced in the Section 3 view definitions.

cat("\n=== SECTION 4: Building linked individual panels ===\n")

# Load countysumm.csv (provided in data/) to obtain match weights and neighbor_samp
# for merging into individual-level linked files.
countysumm_ref <- read_csv(file.path(data_out, "countysumm.csv")) %>%
  select(FIPS, YEAR, SOUTH, TREAT, neighbor_samp,
         match_weight1, match_weight2, match_weight3) %>%
  distinct()

# FIPS crosswalk (needed to add FIPS to linked data)
fipscrosswalk <- read_xls(fips_xwalk)

# --- 4a: Collect linked data one decade at a time ---
# Collecting all three decades through a UNION ALL in one shot keeps three large
# joins in flight simultaneously and exhausts DuckDB's temp space. Collecting
# per-decade lets only one join run at a time; results are bound in R.
cat("Collecting linked panels (one decade at a time)...\n")
link_pieces <- lapply(c(1920, 1930, 1940), function(linkyear) {
  cat(glue("  {linkyear - 10}\u2192{linkyear}...\n"))
  tbl(con, glue("linked{linkyear}")) %>%
    # Define demographic groups and labor market status in DuckDB before collecting
    mutate(
      demgroup_base = case_when(SEX_base == 2 & MARST_base != 1 & MARST_base != 2 ~ "SW",
                                TRUE ~ "MW"),
      demgroup_link = case_when(SEX_link == 2 & MARST_link != 1 & MARST_link != 2 ~ "SW",
                                TRUE ~ "MW"),
      worker_base  = ifelse(LABFORCE_base == 2 & AGE_base >= 16 & AGE_base <= 64, 1, 0),
      teacher_base = ifelse(OCC1950_base == 93  & CLASSWKR_base == 2 & worker_base == 1, 1, 0),
      worker_link  = ifelse(LABFORCE_link == 2 & AGE_link >= 16 & AGE_link <= 64, 1, 0),
      teacher_link = ifelse(OCC1950_link == 93  & CLASSWKR_link == 2 & worker_link == 1, 1, 0),
      move_state   = ifelse(STATEICP_base != STATEICP_link, 1, 0)
    ) %>%
    # Rename for output: use base-year geography/demographics as individual identifiers
    mutate(
      STATEICP  = STATEICP_base,
      COUNTYICP = COUNTYICP_base,
      YEAR      = YEAR_link,
      RACE      = RACE_base,
      AGE       = AGE_base,
      URBAN     = URBAN_base
    ) %>%
    select(STATEICP, COUNTYICP, YEAR,
           demgroup_base, demgroup_link,
           teacher_base, worker_base,
           teacher_link, worker_link,
           RACE, AGE, URBAN,
           OCCSCORE_link, NCHILD_link,
           move_state) %>%
    collect()
})
linkview_raw <- bind_rows(link_pieces)

# --- 4b: Add FIPS, treatment variables, and county-level merge variables ---
add_fips_and_geo <- function(df) {
  df %>%
    left_join(fipscrosswalk, by = c("STATEICP" = "STICPSR")) %>%
    mutate(
      FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),
                       str_pad(COUNTYICP, 4, "left", pad = "0")),
      FIPS = ifelse(substr(FIPSRAW, 6, 6) == "0", substr(FIPSRAW, 1, 5), NA)
    ) %>%
    select(-any_of(c("NAME", "STNAME", "FIPSRAW", "AB")))
}

linkview <- linkview_raw %>%
  add_fips_and_geo() %>%
  # Compute county-level nlink (number of linked individuals per county × year)
  group_by(STATEICP, COUNTYICP, YEAR) %>%
  mutate(nlink = n()) %>%
  ungroup() %>%
  # Merge in treatment status, neighbor_samp, and match weights from provided countysumm.csv
  left_join(countysumm_ref, by = c("FIPS", "YEAR")) %>%
  # Create outcome variables (labor market states in the link year)
  mutate(
    married_link   = ifelse(demgroup_link == "MW", 1, 0),
    mwt_link       = ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0),
    mwnt_link      = ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0),
    mwnilf_link    = ifelse(demgroup_link == "MW" & worker_link == 0, 1, 0),
    unmarried_link = ifelse(demgroup_link == "SW", 1, 0),
    swt_link       = ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0),
    swnt_link      = ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0),
    swnilf_link    = ifelse(demgroup_link == "SW" & worker_link == 0, 1, 0)
  )

# Variable list for linked files (mirrors clean_data_vars.R)
linked_vars_out <- c(
  "STATEICP", "COUNTYICP", "YEAR",
  "SOUTH", "TREAT", "FIPS",
  "RACE", "AGE", "neighbor_samp",
  "married_link", "mwt_link", "mwnt_link", "mwnilf_link",
  "unmarried_link", "swt_link", "swnt_link", "swnilf_link",
  "nlink", "URBAN",
  "OCCSCORE_link", "NCHILD_link", "worker_link", "move_state",
  "match_weight1", "match_weight2", "match_weight3"
)

# --- 4c: Filter to three baseline populations and save ---
# link1: Women who were unmarried teachers in t-10 (baseline for Table 3 Panel 1, Table 5)
cat("Saving link1_swt_indiv.csv...\n")
linkview %>%
  filter(teacher_base == 1 & demgroup_base == "SW" & AGE <= 40) %>%
  select(any_of(linked_vars_out)) %>%
  write_csv(file.path(data_out, "link1_swt_indiv.csv"))

# link2: Women who were married and not in the labor force in t-10 (Table 3 Panel 2)
cat("Saving link2_mwnilf_indiv.csv...\n")
linkview %>%
  filter(worker_base == 0 & demgroup_base == "MW" & AGE >= 18 & AGE <= 50) %>%
  select(any_of(linked_vars_out)) %>%
  write_csv(file.path(data_out, "link2_mwnilf_indiv.csv"))

# link3: Women who were unmarried and not in the labor force in t-10 (Table 3 Panel 3)
cat("Saving link3_swnilf_indiv.csv...\n")
linkview %>%
  filter(worker_base == 0 & demgroup_base == "SW" & AGE >= 8 & AGE <= 40) %>%
  select(any_of(linked_vars_out)) %>%
  write_csv(file.path(data_out, "link3_swnilf_indiv.csv"))

cat("\nLink files saved to data/. Done.\n")

# ============================================================================
# SECTION 5: CLOSE DATABASE CONNECTION
# ============================================================================
dbDisconnect(con, shutdown = TRUE)
cat("DuckDB connection closed.\n")
