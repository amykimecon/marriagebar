### HELPER FUNCTIONS FOR MARRIAGEBAR PROJECT ###

##################
## DATA HELPERS ##
##################
# Adds useful/common person-level variables, requires indiv-level dataset with variables OCC1950, SEX, AGE, MARST (with IPUMS codings)
addvars_indiv <- function(dataset){
  outdata <- dataset %>% 
    mutate(demgroup = case_when(SEX == 1 ~ "M", #man
                                SEX == 2 & MARST != 1 & MARST != 2 ~ "SW", #single/divorced/widowed/separated woman
                                TRUE ~ "MW"), #married woman
           demgroup2 = case_when(SEX == 1 ~ "M", #man
                                 SEX == 2 & NCHILD == 0 ~ "WNC", #woman without children
                                 TRUE ~ "WC"), #woman with children
           worker = ifelse(YEAR == 1900, 
                           ifelse(OCC1950 != 999 & AGE >= 18 & AGE <= 64, 1, 0), #in 1900, no LABFORCE so use those with occupation
                           ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, 1, 0)), #otherwise, those in LABFORCE 
           teacher = ifelse(YEAR == 1900,
                            ifelse(OCC1950 == 93 & worker == 1, 1, 0), #in 1900, no CLASSWKR
                            ifelse(OCC1950 == 93 & CLASSWKR == 2 & worker == 1, 1, 0)),
           secretary = ifelse(YEAR == 1900,
                              ifelse(OCC1950 == 350 & worker == 1, 1, 0), #in 1900, no CLASSWKR
                              ifelse(OCC1950 == 350 & CLASSWKR == 2 & worker == 1, 1, 0)),
           occcat = case_when(OCC1950 %in% c(93, 57) | (OCC1950 >= 12 & OCC1950 <= 29) ~ "Teaching",
                              OCC1950 < 100 ~ "Professional and Technical",
                              OCC1950 >= 200 & OCC1950 < 300 ~ "Managers, Officials, Proprietors",
                              OCC1950 >= 300 & OCC1950 < 400 ~ "Clerical Workers",
                              OCC1950 >= 400 & OCC1950 < 500 ~ "Sales Workers",
                              OCC1950 >= 500 & OCC1950 < 600 ~ "Craftsmen",
                              OCC1950 >= 600 & OCC1950 < 700 ~ "Operative Workers",
                              OCC1950 >= 700 & OCC1950 <= 720 ~ "Private Household Workers",
                              OCC1950 >= 730 & OCC1950 < 800 ~ "Non-HH Service Workers",
                              (OCC1950 >= 800 & OCC1950 < 900) | (OCC1950 >= 100 & OCC1950 <= 123) ~ "Farm Workers/Owners",
                              OCC1950 >= 900 & OCC1950 <= 970 ~ "Non-Farm Laborers",
                              TRUE ~ "Not Classified/Non-Occupational Response"
                              ),
           age_child = ifelse(NCHILD == 0, NA, AGE - ELDCH))
  return(outdata)
}

# Adds useful/common county-level variables, requires dataset with variables STATEICP, COUNTYICP (with IPUMS codings)
addvars_county <- function(dataset){
  outdata <- dataset %>% 
    mergefips() %>% #helper function to convert separate STATE and COUNTY ICP codes into single FIPS code
    mutate(TREAT = ifelse(STATEICP == 47 | STATEICP == 51, 1, 0), # indicator for treated states
           neighbor_samp = ifelse(STATEICP %in% c(47, 51, 40, 48, 54, 56), 1, 0), # indicator for treated or neighbor control
           neighbor_sampNC = ifelse(STATEICP %in% c(47, 48, 40), 1, 0), #neighbors for NC: SC, VA, TN
           neighbor_sampKY = ifelse(STATEICP %in% c(51, 54, 56), 1, 0) #neighbors for KY: TN, WV, IL, IN, OH
           ) 
    
  return(outdata)
}

# Takes year x county-level dataset and returns vector of FIPS of counties in main sample 
#   (only keeping balanced panel of counties with at least n white teachers in 1930 and 1940 and non-missing FIPS)
mainsamp <- function(dataset, balanced = TRUE, n = 10, verbose = FALSE){
  if (!("FIPS" %in% names(dataset))){
    dataset <- dataset %>% mergefips()
  }
  
  # identifying counties that are observed in all four years
  balanceddata <- dataset %>% filter(YEAR != 1900) %>% group_by(FIPS) %>% summarize(n = n()) %>% filter(n == 4)
  
  fips1940 <- filter(dataset, YEAR == 1940 & NWHITETEACH >= n)$FIPS
  outdata <- dataset %>% filter(YEAR == 1930 & NWHITETEACH >= n & !is.na(FIPS) & #only keeping FIPS with at least n white teachers in 1930
                                  FIPS %in% fips1940) #only keeping FIPS with at least n white teachers in 1940
  
  if (balanced){
    outdata <- outdata %>% filter(FIPS %in% balanceddata$FIPS)
  }
  
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
    left_join(retailsales %>% select(STATE, NDMTCODE, RLDF3929, RRTSAP29, RRTSAP39), by = c("STATEICP"="STATE","COUNTYTEMP"="NDMTCODE")) %>% #rldf3929 = growth in retail sales from 1929 to 1939
    select(-COUNTYTEMP)
}

#####################
## LINKING HELPERS ##
#####################
# Adds useful/common person-level variables FOR LINKED DATA, requires indiv-level LINKED dataset 
#   with variables OCC1950, SEX, AGE, MARST (with IPUMS codings) and suffixes _base and _link
addvars_indiv_linked <- function(dataset){
  outdata <- dataset %>% 
    mutate(demgroup_base = case_when(SEX_base == 1 ~ "M",
                                     SEX_base == 2 & MARST_base != 1 & MARST_base != 2 ~ "SW",
                                     TRUE ~ "MW"),
           demgroup_link = case_when(SEX_link == 1 ~ "M",
                                     SEX_link == 2 & MARST_link != 1 & MARST_link != 2 ~ "SW",
                                     TRUE ~ "MW"),
           worker_base = ifelse(YEAR_base == 1900, 
                                ifelse(OCC1950_base != 999 & AGE_base >= 18 & AGE_base <= 64, 1, 0), #in 1900, no LABFORCE so use those with occupation
                                ifelse(LABFORCE_base == 2 & AGE_base >= 18 & AGE_base <= 64, 1, 0)), #otherwise, those in LABFORCE 
           worker_link = ifelse(LABFORCE_link == 2 & AGE_link >= 18 & AGE_link <= 64, 1, 0),
           teacher_base = ifelse(YEAR_base == 1900,
                                 ifelse(OCC1950_base == 93 & worker_base == 1, 1, 0), #in 1900, no CLASSWKR
                                 ifelse(OCC1950_base == 93 & CLASSWKR_base == 2 & worker_base == 1, 1, 0)),
           teacher_link = ifelse(OCC1950_link == 93 & CLASSWKR_link == 2 & worker_link == 1, 1, 0),
           secretary_base = ifelse(YEAR_base == 1900,
                                   ifelse(OCC1950_base == 350 & worker_base == 1, 1, 0), #in 1900, no CLASSWKR
                                   ifelse(OCC1950_base == 350 & CLASSWKR_base == 2 & worker_base == 1, 1, 0)),
           secretary_link = ifelse(OCC1950_link == 350 & CLASSWKR_link == 2 & worker_link == 1, 1, 0))
  return(outdata)
}

# summarizes linked dataset at the county level with key variables
summlinks <- function(dataset, n = 10){
  outdata <- dataset %>% group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
    summarize(nlink = n(),
              pct_t = sum(ifelse(teacher_link == 1, 1, 0))/n(),
              pct_mw = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
              pct_mwt = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw teach
              pct_mwnt = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later mw non teach but in lf
              pct_mwnilf = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_link) that are later mw and not in lf
              pct_sw = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
              pct_swt = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw teach
              pct_swnt = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_link) that are later sw non teach but in lf
              pct_swnilf = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n() #share of sample (swt_link) that are later sw and not in lf
    ) %>%
    rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
    collect() %>%
    addvars_county()
  
  # using helper function to test if FIPS in main linked sample
  mainlinksamplist = mainlinksamp(outdata, n = n)
  outdata <- outdata %>% mutate(mainsamp = ifelse(FIPS %in% mainlinksamplist, 1, 0))
  
  return(outdata)
}

# Takes year x county-level LINKED dataset and returns vector of FIPS of counties in main LINKED sample 
#   (only keeping balanced panel of counties with at least n filtered & linked people in 1930 and 1940 and non-missing FIPS)
mainlinksamp <- function(dataset, balanced = TRUE, n = 10, verbose = FALSE){
  if (!("FIPS" %in% names(dataset))){
    dataset <- dataset %>% mergefips()
  }
  
  # identifying counties that are observed in all three links
  balanceddata <- dataset %>% filter(YEAR != 1910) %>% group_by(FIPS) %>% summarize(n = n()) %>% filter(n == 3)
  
  fips1940 <- filter(dataset, YEAR == 1940 & nlink >= n)$FIPS
  outdata <- dataset %>% filter(YEAR == 1930 & nlink >= n & !is.na(FIPS) & #only keeping FIPS with at least n filtered & linked people in each county in 1930
                                  FIPS %in% fips1940) #only keeping FIPS with at least n filtered & linked people in each county in 1940
  
  if (balanced){
    outdata <- outdata %>% filter(FIPS %in% balanceddata$FIPS)
  }
  
  if (verbose){
    print(glue("Original Dataset: {length(unique(dataset$FIPS))} counties"))
    print(glue("Main Sample (Counties with >= {n} teachers in 1930 and 1940): {length(unique(outdata$FIPS))} counties"))
    print(glue("Dropped {100*length(unique(outdata$FIPS))/length(unique(dataset$FIPS))}% of Observations"))
  }
  return(unique(outdata$FIPS))
}

##############
## MATCHING ##
##############
# takes a dataset (long on year x county) and list of variable names (excluding % urban and retail sales) on which to match, plus indicator for using retail sales
# returns dataframe of control counties including the control county FIPS code, and the state of the treatment county to which it was matched
matching <- function(longdata, varnames, distance = "robust_mahalanobis", retail = FALSE, verbose = FALSE){
  if (!("FIPS" %in% names(longdata))){
    longdata <- longdata %>% mergefips()
  }
  
  # fips codes of main sample
  mainsampfips = mainsamp(longdata)
  
  # names of variables for matching (varnames in 1920, 1930, and retail sales if retail=TRUE)
  # filter_varnames = c(glue("{varnames}_1930"), glue("{varnames}_growth1930"), 
  #                     glue("{varnames}_growth1920")) #c(glue("{varnames}_1910"), glue("{varnames}_1920"),glue("{varnames}_1930"))
  # filter_varnames = c(glue("{varnames}_1930"),glue("{varnames}_1920"),glue("{varnames}_1910"))
  filter_varnames = c(glue("{varnames}_growth1930"),glue("{varnames}_growth1920"))
  match_varnames = c(filter_varnames,"URBAN_1910", "URBAN_1920", "URBAN_1930")
  
  if(retail){
    filter_varnames <- c(filter_varnames, "RRTSAP29", "RRTSAP39", "RLDF3929")
    match_varnames <- c(match_varnames, "RRTSAP29", "RLDF3929")
  }
  
  # prepping data for matching
  matchdata <- longdata %>% filter(FIPS %in% mainsampfips) %>% #keeping only counties in main sample
    pivot_wider(id_cols = c(FIPS, STATEICP, COUNTYICP, TREAT), #pivoting wide (so only one row per county)
                names_from = YEAR, 
                values_from = all_of(c(varnames, "URBAN"))) %>%
    retailsales() #merging with retail sales
  
  for (var in varnames){
      matchdata[[glue("{var}_growth1930")]] = (matchdata[[glue("{var}_1930")]]-matchdata[[glue("{var}_1920")]])/matchdata[[glue("{var}_1920")]]
      matchdata[[glue("{var}_growth1920")]] = (matchdata[[glue("{var}_1920")]]-matchdata[[glue("{var}_1910")]])/matchdata[[glue("{var}_1910")]]
  }
  
  matchdata <- matchdata %>%
    filter(if_all(all_of(filter_varnames), function(.x) !is.na(.x) & .x != Inf)) %>% 
    filter(!is.na(URBAN_1910) & !is.na(URBAN_1920) & !is.na(URBAN_1930))
  
  print(glue("Retention: {length(unique(matchdata$FIPS))} out of {length(unique(longdata$FIPS))} counties"))
  
  # matching treated counties with nontreated counties, not allowing replacement
  match_obj <- matchit(reformulate(match_varnames, response = "TREAT"), data = matchdata, REPLACE = FALSE, 
                       distance = distance)
  
  if (verbose){
    print(summary(match_obj))
  }
  
  # getting matched data
  control_matches <- matchdata[match_obj$match.matrix,] # match_obj$match.matrix links indices of treated units (rownames) to control units (values)
  control_matches[["STATE_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["STATEICP"]] # creating new var equal to state of treated match
  control_matches[["FIPS_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["FIPS"]] # creating new var equal to fips of treated match
  
  return(select(bind_rows(control_matches, 
                          get_matches(match_obj) %>% filter(TREAT == 1) %>% mutate(STATE_MATCH = STATEICP, FIPS_MATCH = FIPS)), 
                c(FIPS, STATE_MATCH, FIPS_MATCH)))
}

# testing matching between control and treatment for various samples on variable 'varname'
match_test <- function(dataset, varnames){
  # if multiple variables and multiple samples, return error
  if (length(varnames) > 1 & length(unique(dataset$sample))>1){
    print('ERROR: need single variable OR single sample')
    return(NA)
  }
  # if only one variable, facet wrap by sample
  if (length(varnames) == 1){
    graphdata <- dataset %>% group_by(TREAT, YEAR, sample) %>% summarize(across(all_of(varnames), ~mean(.x, na.rm=TRUE)))
    graph_out <- ggplot(data = graphdata, aes(x = YEAR, y = .data[[varnames]], color = factor(TREAT), shape = factor(TREAT))) + 
      geom_point() + geom_line() + facet_wrap(~sample)
  }
  # if multiple variables and only one sample, pivot long and facet wrap by 
  if (length(varnames) > 1 & length(unique(dataset$sample)) == 1){
    graphdata <- dataset %>% group_by(TREAT, YEAR) %>% summarize(across(all_of(varnames), ~mean(.x, na.rm=TRUE))) %>%
      pivot_longer(all_of(varnames), names_to = "cat", values_to = "val")
    graph_out <- ggplot(data = graphdata, aes(x = YEAR, y = val, color = factor(TREAT), shape = factor(TREAT))) + 
      geom_point() + geom_line() + facet_wrap(~cat) + ggtitle(dataset$sample[1])
  }
  
  return(graph_out)
}

# joining outputs of matching (where matches is a list of matching sets) with dataset 
matching_join <- function(dataset, matchlist){
  for (i in 1:length(matchlist)){
    if (i == 1){ # for first matching dataset, keep state of match
      dataset <- dataset %>% left_join(matchlist[[i]] %>% mutate(match = 1) %>% select(-FIPS_MATCH), by = c("FIPS", "STATEICP")) %>% 
        mutate(match_samp = ifelse(match == 1, 1, 0))
    }
    else{
      dataset[[glue("match_samp{i}")]] <- ifelse(dataset$FIPS %in% matchlist[[i]]$FIPS, 1, 0)
    }
  }
  return(dataset)
}

# matching individual control counties to specific treated states in order to assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before
# matchtype is a string equal to either `neighbor` (using neighboring states analysis) or `match` (using matched counties analysis)
state_matching <- function(dataset, matchtype){
  if (matchtype == "neighbor"){
    outdata <- dataset %>% mutate(STATE_MATCH = case_when(neighbor_sampNC == 1 ~ 47,
                                                          neighbor_sampKY == 1 ~ 51,
                                                          TRUE ~ NA_integer_))
  }
  else{
    outdata <- dataset %>% mutate(STATE_MATCH = case_when(match == 1 ~ STATE_MATCH,
                                                          TREAT == 1 ~ STATEICP,
                                                          TRUE ~ NA_integer_))
  }
  outdata <- outdata %>% mutate(pct_marr_before_Teacher = case_when(STATE_MATCH == 47 ~ pct_marr_before3_Teacher, #if matched with NC (or in NC), 'law passes' in 1933
                                                                    STATE_MATCH == 51 ~ pct_marr_before8_Teacher, #if matched with KY (or in KY), 'law passes' in 1938
                                                                    TRUE ~ NA_real_),
                                pct_marr_after_Teacher = case_when(STATE_MATCH == 47 ~ pct_marr_after3_Teacher, #if matched with NC (or in NC), 'law passes' in 1933
                                                                   STATE_MATCH == 51 ~ pct_marr_after8_Teacher, #if matched with KY (or in KY), 'law passes' in 1938
                                                                   TRUE ~ NA_real_),
                                pct_marr_before_Secretary = case_when(STATE_MATCH == 47 ~ pct_marr_before3_Secretary, #if matched with NC (or in NC), 'law passes' in 1933
                                                                      STATE_MATCH == 51 ~ pct_marr_before8_Secretary, #if matched with KY (or in KY), 'law passes' in 1938
                                                                      TRUE ~ NA_real_),
                                pct_marr_after_Secretary = case_when(STATE_MATCH == 47 ~ pct_marr_after3_Secretary, #if matched with NC (or in NC), 'law passes' in 1933
                                                                     STATE_MATCH == 51 ~ pct_marr_after8_Secretary, #if matched with KY (or in KY), 'law passes' in 1938
                                                                     TRUE ~ NA_real_)) %>% 
  select(-c(starts_with("pct_marr_before3"),starts_with("pct_marr_after3"),starts_with("pct_marr_before8"),starts_with("pct_marr_after8")))
  return(outdata)
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
#   and pointspan, i.e. total width of all dots for a given year, default is 2
did_graph <- function(dataset, depvarlist, depvarnames, colors, controls = "", years = c(1910, 1920, 1940), yearomit = 1930, verbose = FALSE, yvar = "Coef on Treat X Year",
                      ymax = NA, ymin = NA, slides = FALSE, steps = FALSE, pointspan = 2, filename = NA){
  nvars = length(depvarlist)
  if (nvars != length(depvarnames)){
    print("Error: depvarlist length diff from depvarnames length")
    return(NA)
  }

  # compiling all regression outputs (in format for graphing) from different dependent variables
  if (nvars == 1){
    did_data_temp <- did_graph_data(dataset, depvarlist[[1]], controls, years, yearomit, verbose) 
    did_data <- did_data_temp %>%
      mutate(group = depvarnames[[1]], year_graph = did_data_temp$year)
  }
  else{
    did_datasets <- list()
    for (i in seq(1,nvars)){
      did_data_temp <- did_graph_data(dataset, depvarlist[[i]], controls, years, yearomit, verbose) %>%
        mutate(group = depvarnames[[i]],
               year_graph = year - pointspan/2 + (i-1)*(pointspan/(nvars - 1))) # shifting over so dots don't overlap
      did_datasets[[i]] <- did_data_temp
    }
    did_data <- bind_rows(did_datasets)
  }
  
  if(verbose){
    print(did_data)
  }

  # creating graph
  graph_out <- ggplot(did_data, aes(x = year_graph, 
                                    y = y, 
                                    color = factor(group, levels = depvarnames), 
                                    shape = factor(group, levels = depvarnames))) + 
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
    scale_color_manual(values=colors) +
    annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_point(size = 4) + labs(x = "Year", y = yvar, color = "", shape = "") + theme_minimal() + 
    theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")
  
  if (!is.na(ymax) | !is.na(ymin)){
    graph_out <- graph_out + ylim(ymin,ymax) + geom_text(aes(x = 1935.5, y = ymin + (ymax-ymin)/10, label = "Marriage Bars \n Removed") ,color = "#656565") 
  }
  else{
    graph_out <- graph_out + geom_text(aes(x = 1935.5, y = min(did_data$y_lb) + (max(did_data$y_ub) - min(did_data$y_lb))/10, label = "Marriage Bars \n Removed") ,color = "#656565") 
  }
  
  if (!is.na(filename) & !slides){ #saving graph in folder for paper figs
    ggsave(glue("{outfigs}/paper/{filename}.png"), graph_out, width = 8, height = 5)
  }
  if (!is.na(filename) & slides){ #changing text size for slides and saving in folder for slide figs
    ggsave(glue("{outfigs}/slides/{filename}.png"), graph_out + theme(text = element_text(size = 18), axis.text = element_text(size = 14)), width = 8, height = 5)
  }
  return(graph_out)
}


graph_treatment <- function(dataset, filename = NA){
  graph_out <- plot_usmap(data = dataset %>% filter(YEAR == 1940) %>% mutate(fips = FIPS, TREAT = factor(TREAT)) %>% 
                            select(c(fips, TREAT)), values = "TREAT", color = NA) 
  if (!is.na(filename)){
    ggsave(glue("{outfigs}/{filename}.png"), graph_out)
  }
  return(graph_out)
}

#ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}