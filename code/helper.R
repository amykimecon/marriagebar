### HELPER FUNCTIONS FOR MARRIAGEBAR PROJECT ###

##################
## DATA HELPERS ##
##################

# Adds useful/common person-level variables, requires indiv-level dataset with variables OCC1950, SEX, AGE, MARST (with IPUMS codings)
addvars_indiv <- function(dataset){
  outdata <- dataset %>% 
    mutate(demgroup = case_when(SEX == 1 ~ "M",
                                SEX == 2 & MARST != 1 & MARST != 2 ~ "SW",
                                TRUE ~ "MW"),
           worker = ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, 1, 0),
           teacher = ifelse(OCC1950 == 93 & CLASSWKR == 2 & worker == 1, 1, 0),
           secretary = ifelse(OCC1950 == 350 & CLASSWKR == 2 & worker == 1, 1, 0)) 
  return(outdata)
}

# Adds useful/common county-level variables, requires county-level dataset with variables STATEICP, COUNTYICP (with IPUMS codings)
addvars_county <- function(dataset){
  mainsampfips <- mainsamp(dataset)
  outdata <- dataset %>% 
    mergefips() %>% 
    mutate(TREAT = ifelse(STATEICP == 47 | STATEICP == 51, 1, 0), # indicator for treated states
           neighbor_samp = ifelse(STATEICP %in% c(47, 51, 40, 48, 54, 56), 1, 0),
           mainsamp = ifelse(FIPS %in% mainsampfips, 1, 0)) %>% # indicator for treated or neighbor control
    
  return(outdata)
}

# Takes year x county-level dataset and returns vector of FIPS of counties in main sample 
#   (only keeping counties with at least n teachers in 1930 and 1940 and non-missing FIPS)
mainsamp <- function(dataset, n = 10, verbose = FALSE){
  if (!("FIPS" %in% names(dataset))){
    dataset <- dataset %>% mergefips()
  }
  outdata <- dataset %>% filter(YEAR == 1930 & NWHITETEACH >= n & !is.na(FIPS) &
                                  FIPS %in% filter(dataset, YEAR == 1940 & NWHITETEACH >= n)$FIPS)
  if (verbose){
    print(glue("Original Dataset: {length(unique(dataset$FIPS))} counties"))
    print(glue("Main Sample (Counties with >= {n} teachers in 1930 and 1940): {length(unique(outdata$FIPS))} counties"))
    print(glue("Dropped {100*length(unique(outdata$FIPS))/length(unique(dataset$FIPS))}% of Observations"))
  }
  return(unique(outdata$FIPS))
}

# Takes a dataset with variables STATEICP and COUNTYICP and outputs new dataset with FIPS (county fips code) and AB (state abbreviation) variables
# Useful if using any mapping functions that only take FIPS or merging with another dataset using FIPS codes, or if a unique county id is required
#   (note that COUNTYICP is not unique -- needs to be combined with STATEICP for unique county identifiers)
mergefips <- function(dataset){
  if ("FIPS" %in% names(dataset)){ # if fips already defined, don't change anything
    return(dataset)
  }
  
  # crosswalk from state icpsr to fips codes
  fipscrosswalk <- read_xls(glue("{root}/StateFIPSicsprAB.xls"))
  
  # joining data with crosswalk and creating unique fips from state fips + county icpsr 
  #   (county fips = first 3 digits of county icpsr except if county disappears over time, in which case FIPS = NA)
  outdata <- left_join(dataset, fipscrosswalk, by = c("STATEICP" = "STICPSR"))  %>% 
    mutate(FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),str_pad(COUNTYICP, 4, "left", pad = "0")),
           FIPS = ifelse(substr(FIPSRAW,6,6) == "0", substr(FIPSRAW,1,5), NA)) %>% select(-c(NAME, STNAME, FIPSRAW))

  return(outdata)
}

# merging a dataset with STATEICP and COUNTYICP variables with retail sales data
retailsales <- function(dataset){
  # reading in retail sales data for merging 
  retailsales <- read_xls(glue("{root}/retailsales/retailsales_data.xls")) 
  
  # altering county codes according to Retail-Sales-sources.doc to match retailsales county codes
  outdata <- dataset %>%
    mutate(COUNTYTEMP = case_when(STATEICP == 13 & COUNTYICP == 50 ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 470 ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 610 ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 810 ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 850 ~ 1500,
                                  STATEICP == 34 & COUNTYICP == 1890 ~ 3000,
                                  STATEICP == 34 & COUNTYICP == 5100 ~ 3000,
                                  STATEICP == 40 & COUNTYICP == 30 ~ 2000,
                                  STATEICP == 40 & COUNTYICP == 5400 ~ 2000,
                                  STATEICP == 40 & COUNTYICP == 50 ~ 2100,
                                  STATEICP == 40 & COUNTYICP == 5600 ~ 2100,
                                  STATEICP == 40 & COUNTYICP == 130 ~ 2150,
                                  STATEICP == 40 & COUNTYICP == 5100 ~ 2150,
                                  STATEICP == 40 & COUNTYICP == 150 ~ 2200,
                                  STATEICP == 40 & COUNTYICP == 7900 ~ 2200,
                                  STATEICP == 40 & COUNTYICP == 310 ~ 2300,
                                  STATEICP == 40 & COUNTYICP == 6800 ~ 2300,
                                  STATEICP == 40 & COUNTYICP == 530 ~ 2400,
                                  STATEICP == 40 & COUNTYICP == 7300 ~ 2400,
                                  STATEICP == 40 & COUNTYICP == 550 ~ 2500,
                                  STATEICP == 40 & COUNTYICP == 6500 ~ 2500,
                                  STATEICP == 40 & COUNTYICP == 690 ~ 2600,
                                  STATEICP == 40 & COUNTYICP == 8400 ~ 2600,
                                  STATEICP == 40 & COUNTYICP == 870 ~ 2700,
                                  STATEICP == 40 & COUNTYICP == 7600 ~ 2700,
                                  STATEICP == 40 & COUNTYICP == 890 ~ 2800,
                                  STATEICP == 40 & COUNTYICP == 6900 ~ 2800,
                                  STATEICP == 40 & COUNTYICP == 950 ~ 2900,
                                  STATEICP == 40 & COUNTYICP == 8300 ~ 2900,
                                  STATEICP == 40 & COUNTYICP == 1210 ~ 3000,
                                  STATEICP == 40 & COUNTYICP == 7500 ~ 3000,
                                  STATEICP == 40 & COUNTYICP == 1230 ~ 3100,
                                  STATEICP == 40 & COUNTYICP == 8000 ~ 3100,
                                  STATEICP == 40 & COUNTYICP == 1290 ~ 3200,
                                  STATEICP == 40 & COUNTYICP == 7100 ~ 3200,
                                  STATEICP == 40 & COUNTYICP == 7850 ~ 3200,
                                  STATEICP == 40 & COUNTYICP == 7400 ~ 3200,
                                  STATEICP == 40 & COUNTYICP == 1430 ~ 3300,
                                  STATEICP == 40 & COUNTYICP == 5900 ~ 3300,
                                  STATEICP == 40 & COUNTYICP == 1490 ~ 3400,
                                  STATEICP == 40 & COUNTYICP == 6700 ~ 3400,
                                  STATEICP == 40 & COUNTYICP == 1610 ~ 3500,
                                  STATEICP == 40 & COUNTYICP == 7700 ~ 3500,
                                  STATEICP == 40 & COUNTYICP == 1630 ~ 3600,
                                  STATEICP == 40 & COUNTYICP == 5300 ~ 3600,
                                  STATEICP == 40 & COUNTYICP == 1650 ~ 3700,
                                  STATEICP == 40 & COUNTYICP == 6600 ~ 3700,
                                  STATEICP == 40 & COUNTYICP == 1770 ~ 3800,
                                  STATEICP == 40 & COUNTYICP == 6300 ~ 3800,
                                  STATEICP == 40 & COUNTYICP == 1875 ~ 3900,
                                  STATEICP == 40 & COUNTYICP == 7000 ~ 3900,
                                  STATEICP == 40 & COUNTYICP == 1910 ~ 4000,
                                  STATEICP == 40 & COUNTYICP == 5200 ~ 4000,
                                  STATEICP == 44 & COUNTYICP == 410 ~ 1210,
                                  STATEICP == 44 & COUNTYICP == 2030 ~ 1210,
                                  STATEICP == 44 & COUNTYICP == 1210 ~ 1210,
                                  TRUE ~ COUNTYICP
                                  )) %>%
    left_join(retailsales %>% select(STATE, NDMTCODE, RLDF3929), by = c("STATEICP"="STATE","COUNTYTEMP"="NDMTCODE")) %>% #rldf3929 = growth in retail sales from 1929 to 1939
    select(-COUNTYTEMP)
}

##############
## MATCHING ##
##############
# takes a dataset (long on year x county) and list of variable names (excluding retail sales) on which to match, plus indicator for using retail sales
# returns dataframe of control counties including the control county FIPS code, and the state of the treatment county to which it was matched
matching <- function(longdata, varnames, retail = FALSE, verbose = FALSE){
  if (!("FIPS" %in% names(longdata))){
    longdata <- longdata %>% mergefips()
  }
  
  # fips codes of main sample
  mainsampfips = mainsamp(longdata)
  
  # names of variables for matching (varnames in 1920, 1930, and retail sales if retail=TRUE)
  filter_varnames = c(glue("{varnames}_1920"),glue("{varnames}_1930"))
  if(retail){
    filter_varnames <- c(filter_varnames, "RLDF3929")
  }
  
  # prepping data for matching
  matchdata <- longdata %>% filter(FIPS %in% mainsampfips) %>% #keeping only counties in main sample
    pivot_wider(id_cols = c(FIPS, STATEICP, COUNTYICP, TREAT), #pivoting wide (so only one row per county)
                names_from = YEAR, 
                values_from = all_of(varnames)) %>%
    retailsales() %>% #merging with retail sales
    filter(if_all(all_of(filter_varnames), ~ !is.na(.x)))
  
  # matching treated counties with nontreated counties, not allowing replacement
  match_obj <- matchit(reformulate(filter_varnames, response = "TREAT"), data = matchdata, REPLACE = FALSE)
  
  if (verbose){
    print(summary(match_obj))
  }
  
  # getting matched data
  control_matches <- matchdata[match_obj$match.matrix,] # match_obj$match.matrix links indices of treated units (rownames) to control units (values)
  control_matches[["STATE_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["STATEICP"]] # creating new var equal to state of treated match
  control_matches[["FIPS_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["FIPS"]] # creating new var equal to fips of treated match
  
  return(select(control_matches, c(FIPS, STATE_MATCH, FIPS_MATCH)))
}

##########################
## PRODUCING DID GRAPHS ##
##########################
# Adding interaction dummies to dataset for dynamic DiD
#   Takes in dataset with YEAR and TREAT, and creates indicators for YEAR x TREAT for all years
add_did_dummies <- function(dataset){
  for (yr in unique(dataset$YEAR)){
    dataset[[glue("TREATx{yr}")]] <- ifelse(dataset$YEAR == yr, 1, 0)*ifelse(dataset$TREAT == 1, 1, 0)
  }
  return(dataset)
}
  
# Creating data for graphing dynamic DiD 
# takes in dataset, depvar (dependant variable), any controls (string of form '+ X1 + X2 +...'), years to include, year to omit
did_graph_data <- function(dataset, depvar, controls = "", years = c(1910, 1920, 1940), yearomit = 1930, verbose = FALSE){
  # vector of interaction terms
  interact_vars <- glue("TREATx{years}")
  
  # modifying dataset (adding interaction terms, setting cluster to FIPS, filtering to only include relevant years)
  regdata <- dataset %>% add_did_dummies() %>% filter(YEAR %in% c(years, yearomit)) %>% mutate(cluster = as.character(FIPS))
  
  # running regression: include year and county fixed effects + interaction terms + any controls
  did_reg <- lm(glue("{depvar} ~ factor(YEAR) + cluster + {glue_collapse(interact_vars, sep = '+')} {controls}"), data = regdata)
  
  if (verbose){
    print(summary(did_reg))
  }
  
  # clustered standard errors (hc1 equiv to ,robust in stata i think? double check this)
  vcov = vcovCL(did_reg, type = "HC1")
  
  # constructing dataframe for output -- y is coef var is estimated variance using clustered standard errors
  effects <- data.frame(y = c(sapply(interact_vars, function (.x) did_reg$coefficients[[.x]]), 0),
                        depvar = depvar,
                        year = c(years, yearomit),
                        var = c(sapply(interact_vars, function(.x) as.numeric(diag(vcov)[[.x]])), 0)) %>%
    mutate(y_ub = y + 1.96*sqrt(var),
           y_lb = y - 1.96*sqrt(var))
  return(effects)
}

# Creating dynamic DiD graph
# takes in all parameters of did_graph_data (with list of dep vars), as well as vector of labels for dep vars and labels for graph
#   and toggles for slides (default is for paper) and steps (i.e. saving versions of the graph with points gradually revealed -- default is no)
did_graph <- function(dataset, depvarlist, depvarnames, colors, controls = "", years = c(1910, 1920, 1940), yearomit = 1930, verbose = FALSE, 
                      slides = FALSE, steps = FALSE){
  nvars = length(depvarlist)
  if (nvars != length(depvarnames)){
    print("Error: depvarlist length diff from depvarnames length")
    return(NA)
  }

  # compiling all regression outputs (in format for graphing) from different dependent variables
  did_datasets <- list()
  for (i in seq(1,nvars)){
    did_data_temp <- did_graph_data(dataset, depvarlist[[i]], controls, years, yearomit, verbose) %>%
      mutate(group = depvarnames[[i]],
             year_graph = year - 0.6 + (i-1)*(1.2/(nvars - 1))) # shifting over so dots don't overlap
    did_datasets[[i]] <- did_data_temp
  }
  did_data <- bind_rows(did_datasets)
  # creating graph
  graph_out <- ggplot(did_data, aes(x = year_graph, 
                                    y = y, 
                                    color = factor(group, levels = depvarnames), 
                                    shape = factor(group, levels = depvarnames))) + 
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
    scale_color_manual(values=colors) +
    annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_text(aes(x = 1935.5, y = -0.07, label = "Marriage Bars \n Removed") ,color = "#656565") +
    geom_point(size = 4) + labs(x = "Year", y = "Treat X Year", color = "", shape = "") + theme_minimal() + 
    theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")
  
  return(graph_out)
}




