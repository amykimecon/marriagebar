### HELPER FUNCTIONS FOR MARRIAGEBAR PROJECT ###

#__________________________
# GENERAL DATA PREP ----
#__________________________
# Adds useful/common person-level variables, requires indiv-level dataset with variables OCC1950, SEX, AGE, MARST (with IPUMS codings), and OCC1950_SP
addvars_indiv <- function(dataset){
  outdata <- dataset %>% 
    mutate(demgroup = case_when(SEX == 1 ~ "M", #man
                                SEX == 2 & MARST != 1 & MARST != 2 ~ "SW", #single/divorced/widowed/separated woman
                                TRUE ~ "MW"), #married woman
           demgroup2 = case_when(SEX == 1 ~ "M", #man
                                 SEX == 2 & NCHILD == 0 ~ "WNC", #woman without children
                                 TRUE ~ "WC"), #woman with children
           worker = ifelse(YEAR == 1900, 
                           ifelse(OCC1950 != 999 & AGE >= 16 & AGE <= 64, 1, 0), #in 1900, no LABFORCE so use those with occupation
                           ifelse(LABFORCE == 2 & AGE >= 16 & AGE <= 64, 1, 0)), #otherwise, those in LABFORCE 
           teacher = ifelse(YEAR == 1900,
                            ifelse(OCC1950 == 93 & worker == 1, 1, 0), #in 1900, no CLASSWKR
                            ifelse(OCC1950 == 93 & CLASSWKR == 2 & worker == 1, 1, 0)),
           teacher_SP = ifelse(OCC1950_SP == 93, 1, 0), #SP = spouse; FOR NOW, no LABFORCE_SP so just use OCC1950_SP
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
} #!#! CHECKED

# Adds useful/common county-level variables, requires dataset with variables STATEICP, COUNTYICP (with IPUMS codings)
addvars_county <- function(dataset){
  outdata <- dataset %>% 
    mergefips() %>% #helper function to convert separate STATE and COUNTY ICP codes into single FIPS code
    mutate(SOUTH           = ifelse(STATEICP %in% c(54, 41, 46, 51, 40, 56, 47, 11, 52, 48, 43, 44), 1, 0), #us census regions east south central and south atlantic
           TREAT           = ifelse(STATEICP == 47 | STATEICP == 51, 1, 0), # indicator for treated states
           neighbor_samp   = ifelse(STATEICP %in% c(47, 51, 40, 48, 54, 56), 1, 0), # indicator for treated or neighbor control
           neighbor_sampNC = ifelse(STATEICP %in% c(47, 48, 40, 54), 1, 0), #neighbors for NC: SC, VA, TN
           neighbor_sampKY = ifelse(STATEICP %in% c(51, 54, 56, 21, 24, 22), 1, 0) #neighbors for KY: TN, WV, IL, IN, OH
           ) 
    
  return(outdata)
} #!#! CHECKED

# takes GROUPED duckdb individual-level panel data, summarizes full sample with variables of interest
summ_gen <- function(dataset){
  outdata <- dataset %>%
    summarize(POP             = n(), #overall population of county
              WHITEPOP        = sum(ifelse(RACE == 1, 1, 0)), #white population
              BLACKPOP        = sum(ifelse(RACE == 2, 1, 0)), #black pop
              NTEACH          = sum(ifelse(teacher == 1, 1, 0)),
              NMW = sum(ifelse(demgroup == "MW" & AGE >= 16, 1, 0)),
              NWHITETEACH     = sum(ifelse(teacher==1 & RACE==1, 1, 0)), #number of white teachers
              NWHITEWORK      = sum(ifelse(worker == 1 & RACE == 1, 1, 0)), #number of white workers
              NWHITEMW        = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 16 & AGE <= 64, 1, 0)), # number of white married women
              NWHITESW        = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 16 & AGE <= 64, 1, 0)), # number of white unmarried women
              NBLACKTEACH     = sum(ifelse(teacher==1 & RACE== 2, 1, 0)), #number of white teachers
              NBLACKWORK      = sum(ifelse(worker == 1 & RACE == 2, 1, 0)), #number of white workers
              NBLACKMW        = sum(ifelse(demgroup == "MW" & RACE == 2 & AGE >= 16 & AGE <= 64, 1, 0)), # number of white married women
              NBLACKSW        = sum(ifelse(demgroup == "SW" & RACE == 2 & AGE >= 16 & AGE <= 64, 1, 0)), # number of white unmarried women
              NWHITESW_YOUNG  = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 18 & AGE <= 25, 1, 0)), # number of white unmarried women 18-25
              NWHITESW_MID    = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE > 25 & AGE <= 35, 1, 0)), # number of white unmarried women 26-35
              URBAN           = sum(ifelse(URBAN == 2, 1, 0))/n(), #percent of county living in urban area
              PCT_WHITE       = sum(ifelse(RACE == 1, 1, 0))/n(), # percent of county that is white
              SCHOOLPOP = sum(ifelse(AGE <= 18 & AGE >= 6, 1, 0)),
              BLACKSCHOOLPOP = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, 1, 0)),
              WHITESCHOOLPOP  = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, 1, 0)), #white schoolage population
              LFP             = sum(ifelse(worker == 1 & AGE >= 16 & AGE <= 64, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64, 1, 0)), #share of prime age population that is in LF
              LFP_M           = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "M",  1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "M",  1, 0)), #lfp for men
              LFP_W = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup != "M",  1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup != "M",  1, 0)),
              LFP_SW          = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "SW", 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "SW", 1, 0)), #lfp for single women
              LFP_WSW         = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "SW" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "SW" & RACE == 1, 1, 0)), #lfp for white single women
              LFP_BSW         = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "SW" & RACE == 2, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "SW" & RACE == 2, 1, 0)), #lfp for black single women
              LFP_MW          = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "MW", 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "MW", 1, 0)), #lfp for married women
              LFP_WMW         = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "MW" & RACE == 1, 1, 0)), #lfp for white married women
              LFP_BMW         = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup == "MW" & RACE == 2, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup == "MW" & RACE == 2, 1, 0)), #lfp for black married women
              LFP_WW = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup != "M" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup != "M" & RACE == 1, 1, 0)), #lfp for white women
              LFP_BW = sum(ifelse(worker != 0 & AGE >= 16 & AGE <= 64& demgroup != "M" & RACE == 2, 1, 0))/sum(ifelse(AGE >= 16 & AGE <= 64 & demgroup != "M" & RACE == 2, 1, 0)), #lfp for black women
              PCT_LF_MW       = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are MW
              PCT_LF_WMW      = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are white MW
              PCT_LF_BMW      = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 2, 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are black MW
              UNEMP_RATE      = sum(ifelse(EMPSTAT == 2 & AGE >= 16 & LABFORCE == 2, 1, 0))/sum(ifelse(AGE >= 16 & LABFORCE == 2, 1, 0)),
              PCT_UNDER20     = sum(ifelse(AGE < 20, 1, 0))/n(), #share of pop in each age group
              PCT_20TO39      = sum(ifelse(AGE >= 20 & AGE < 40, 1, 0))/n(), #share of pop in each age group
              PCT_40TO59      = sum(ifelse(AGE >= 40 & AGE < 60, 1, 0))/n(), #share of pop in each age group
              PCT_OVER59      = sum(ifelse(AGE >= 60, 1, 0))/n(), #share of pop in each age group
              AGE             = mean(AGE),
              NCHILD          = mean(ifelse(demgroup == "MW", NCHILD, NA), na.rm=TRUE), #avg number of children for married women
              PCT_MARR        = sum(ifelse(AGE >= 16 & SEX == 2 & MARST %in% c(1,2), 1, 0))/sum(ifelse(AGE >= 16 & SEX == 2, 1, 0)), #share adult women married
              PCT_MARR_COHORT = sum(ifelse(AGE >= 16 & AGE <= 40 & SEX == 2 & MARST %in% c(1,2), 1, 0)) / 
                sum(ifelse(AGE >= 16 & AGE <= 40 & SEX == 2, 1, 0)), #share adult women aged 16-40 married
              PCT_LIT         = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)), #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
              N_SWT           = sum(ifelse(RACE == 1 & teacher == 1 & demgroup == "SW" & AGE <= 40, 1, 0)),
              N_MWNT          = sum(ifelse(RACE == 1 & teacher == 0 & demgroup == "MW" & AGE <= 50, 1, 0)),
              N_MWNILF        = sum(ifelse(RACE == 1 & worker == 0 & demgroup == "MW" & AGE <= 50, 1, 0)),
              PCT_CHILD_SCHOOL= sum(ifelse(AGE <= 18 & AGE >= 6 & SCHOOL == 2, 1, 0))/sum(ifelse(AGE <= 18 & AGE >= 6 & SCHOOL > 0, 1, 0)),
              PCT_GIRLS_SCHOOL= sum(ifelse(AGE <= 18 & AGE >= 6 & SEX == 2 & SCHOOL == 2, 1, 0))/sum(ifelse(AGE <= 18 & AGE >= 6 & SEX == 2 & SCHOOL > 0, 1, 0)),
              PCT_GIRLS_HS    = sum(ifelse(AGE <= 18 & AGE >= 12 & SEX == 2 & SCHOOL == 2, 1, 0))/sum(ifelse(AGE <= 18 & AGE >= 12 & SEX == 2 & SCHOOL > 0, 1, 0))
    )
  return(outdata)
}

# takes GROUPED AND FILTERED duckdb individual-level panel data, summarizes with occupational variables of interest
summ_occ <- function(dataset){
  outdata <- dataset %>%
    summarize(num              = n(), # number of teachers (or secretaries)
              num_mw           = sum(ifelse(demgroup == "MW", 1, 0)), #num of teachers MW
              num_sw           = sum(ifelse(demgroup == "SW", 1, 0)), #num of teachers SW
              num_sw_young     = sum(ifelse(demgroup == "SW" & AGE <= 25, 1, 0)), #num of teachers SW
              num_sw_mid       = sum(ifelse(demgroup == "SW" & AGE > 25 & AGE <= 35, 1, 0)), #num of teachers SW
              num_m            = sum(ifelse(demgroup == "M", 1, 0)), #num of teachers M
              pct_mw           = sum(ifelse(demgroup == "MW", 1, 0))/n(), #share of teachers MW
              pct_sw           = sum(ifelse(demgroup == "SW", 1, 0))/n(), #share of teachers SW
              pct_m            = sum(ifelse(demgroup == "M", 1, 0))/n(), #share of teachers M
              pctw_marr        = sum(ifelse(demgroup == "MW", 1, 0))/sum(ifelse(demgroup != "M", 1, 0)), #share of women teachers married
              avg_age_child    = mean(ifelse(demgroup != "M", age_child, NA), na.rm=TRUE),
              avg_nchild       = mean(ifelse(demgroup != "M", NCHILD, NA), na.rm=TRUE),
              num_agemarr      = sum(ifelse(AGEMARR > 0, 1, 0)), #number of teachers sampled for age at marriage (if not sampled, AGEMARR = 0)
              pct_marr_before3 = (sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 7 , 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MB/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married more than 7 years ago (1933 NC)
              pct_marr_after3  = (1 - sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 7, 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MA/(MA+MB) x (MW)/(MW + SW + M) approx share of teachers MW AND married less than 7 years ago
              pct_marr_before8 = (sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 2, 1, 0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MB/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married more than 2 years ago (1938 KY)
              pct_marr_after8  = (1 - sum(ifelse(AGEMARR > 0 & AGE - AGEMARR > 2, 1 ,0))/sum(ifelse(AGEMARR > 0, 1, 0)))*sum(ifelse(demgroup == "MW", 1, 0))/n(), # MA/(MA+MB) x (MW)/(MW + SW + M) approx share teachers MW AND married less than 2 years ago 
              pct_wc           = sum(ifelse(demgroup2 == "WC", 1, 0))/n(), #share of teachers who are women AND have children
              pct_wnc          = sum(ifelse(demgroup2 == "WNC", 1, 0))/n(), #share of teachers who are women AND DONT have children
              pctw_wc          = sum(ifelse(demgroup2 == "WC"  & demgroup != "M", 1, 0))/sum(ifelse(demgroup != "M", 1, 0)), #share of W teachers who have children
              pctw_wnc         = sum(ifelse(demgroup2 == "WNC" & demgroup != "M", 1, 0))/sum(ifelse(demgroup != "M", 1, 0)), #share of W teachers who don't have children
              pct_sp_teach     = mean(teacher_SP), #share with teacher spouses
              pct_sp_teach_w   = sum(ifelse(teacher_SP == 1 & demgroup!="M",1,0))/sum(ifelse(demgroup!="M",1,0)), #share women with teacher spouses
              pct_sp_teach_m   = sum(ifelse(teacher_SP == 1 & demgroup=="M",1,0))/sum(ifelse(demgroup=="M",1,0)), #share men with teacher spouses
              avg_occscore     = mean(OCCSCORE_SP, na.rm=TRUE), #avg occscore of spouse 
              avg_occscore_w   = mean(ifelse(demgroup == "MW", OCCSCORE_SP, NA), na.rm=TRUE), #avg occscore of spouse for women
              avg_occscore_m   = mean(ifelse(demgroup == "M", OCCSCORE_SP, NA), na.rm=TRUE), #avg occscore of spouse for men
              med_occscore     = median(OCCSCORE_SP, na.rm=TRUE), #median occscore of spouse
              med_occscore_w   = median(ifelse(demgroup == "MW", OCCSCORE_SP, NA), na.rm=TRUE), #median occscore of spouse for women
              med_occscore_m   = median(ifelse(demgroup == "M",  OCCSCORE_SP, NA), na.rm=TRUE) #median occscore of spouse for men
    )
  
  return(outdata)
}


# Takes year x county-level dataset and returns vector of FIPS of counties in main sample 
#   (only keeping balanced panel of counties with at least n white teachers in 
#   1930 and 1940 and non-missing FIPS)
mainsamp <- function(dataset, balanced = TRUE, n = 10, grp = "white", verbose = FALSE){
  # do nothing if FIPS already exists in dataset
  if (!("FIPS" %in% names(dataset))){
    dataset <- dataset %>% mergefips()
  }
  
  # filter to FIPS with at least n white teachers in 1930 and 40
  if(grp == "white"){
    # identifying counties that have at least one teacher in all years (1910, 20, 30, 40, 50)
    balanceddata <- dataset %>% 
      filter(YEAR != 1900 & NWHITETEACH > 0) %>% 
      group_by(FIPS) %>% 
      summarize(n = n()) %>% 
      filter(n == 5)
    fips1940 <- filter(dataset, YEAR == 1940 & NWHITETEACH >= n)$FIPS
    fips1930 <- filter(dataset, YEAR == 1930 & NWHITETEACH >= n)$FIPS
  }else if(grp == "black"){
    balanceddata <- dataset %>% 
      filter(YEAR != 1900 & NBLACKTEACH > 0) %>% 
      group_by(FIPS) %>% 
      summarize(n = n()) %>% 
      filter(n == 5)
    fips1940 <- filter(dataset, YEAR == 1940 & NBLACKTEACH >= n)$FIPS
    fips1930 <- filter(dataset, YEAR == 1930 & NBLACKTEACH >= n)$FIPS
  }else{
    balanceddata <- dataset %>% 
      filter(YEAR != 1900 & NTEACH > 0) %>% 
      group_by(FIPS) %>% 
      summarize(n = n()) %>% 
      filter(n == 5)
    fips1940 <- filter(dataset, YEAR == 1940 & NTEACH >= n)$FIPS
    fips1930 <- filter(dataset, YEAR == 1930 & NTEACH >= n)$FIPS
  }
  
  outdata <- dataset %>% filter(!is.na(FIPS) &  FIPS %in% fips1930 &     #only keeping FIPS with at least n white teachers in 1930
                                  FIPS %in% fips1940)  #AND at least n white teachers in 1940
  
  # keep only counties observed in all four years if balanced==TRUE
  if (balanced){
    outdata <- outdata %>% filter(FIPS %in% balanceddata$FIPS)
  }
  
  if (verbose){
    print(glue("Original Dataset: {length(unique(dataset$FIPS))} counties"))
    print(glue("Main Sample (Counties with >= {n} teachers in 1930, 40): {length(unique(outdata$FIPS))} counties"))
    print(glue("Dropped {100*(1-length(unique(outdata$FIPS))/length(unique(dataset$FIPS)))}% of Observations")) 
  }
  return(unique(outdata$FIPS))
} #!#! CHECKED

# Takes a dataset with variables STATEICP and COUNTYICP and outputs new dataset with FIPS 
# (county fips code) and AB (state abbreviation) variables
# Useful if using any mapping functions that only take FIPS or merging with another 
# dataset using FIPS codes, or if a unique county id is required
#   (note that COUNTYICP is not unique -- needs to be combined with STATEICP for 
#    unique county identifiers)
mergefips <- function(dataset){
  if ("FIPS" %in% names(dataset)){ # if fips already defined, don't change anything
    return(dataset)
  }
  
  # crosswalk from state icpsr to fips codes
  fipscrosswalk <- read_xls(glue("{root}/StateFIPSicsprAB.xls"))
  
  # joining data with crosswalk and creating unique fips from state fips + county icpsr 
  #   (county fips = first 3 digits of county icpsr except if county disappears over time, 
  #   in which case FIPS = NA)
  outdata <- left_join(dataset, fipscrosswalk, by = c("STATEICP" = "STICPSR"))  %>% 
    mutate(FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),
                            str_pad(COUNTYICP, 4, "left", pad = "0")),
           FIPS    = ifelse(substr(FIPSRAW,6,6) == "0", substr(FIPSRAW,1,5), NA)) %>% 
    dplyr::select(-c(NAME, STNAME, FIPSRAW))

  return(outdata)
} #!#! CHECKED

add_tva_ind <- function(dataset){
  tva <- read_csv(glue("{root}/tva_code/icp_codes_with_tva.csv"))
  x <- dataset %>% left_join(tva %>% select(stateicp, countycode, tva), by = c("STATEICP" = "stateicp", "COUNTYICP" = "countycode")) %>%
    mutate(tva = ifelse(is.na(tva), 0, tva))
  return(x)
}

# merging a dataset with STATEICP and COUNTYICP variables with retail sales data
retailsales <- function(dataset){
  # reading in retail sales data for merging 
  retailsales <- read_xls(glue("{root}/retailsales/retailsales_data.xls")) 
  
  # altering county codes according to Retail-Sales-sources.doc to match retailsales county codes
  outdata <- dataset %>%
    mutate(COUNTYTEMP = case_when(STATEICP == 13 & COUNTYICP == 50   ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 470  ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 610  ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 810  ~ 1500,
                                  STATEICP == 13 & COUNTYICP == 850  ~ 1500,
                                  STATEICP == 34 & COUNTYICP == 1890 ~ 3000,
                                  STATEICP == 34 & COUNTYICP == 5100 ~ 3000,
                                  STATEICP == 40 & COUNTYICP == 30   ~ 2000,
                                  STATEICP == 40 & COUNTYICP == 5400 ~ 2000,
                                  STATEICP == 40 & COUNTYICP == 50   ~ 2100,
                                  STATEICP == 40 & COUNTYICP == 5600 ~ 2100,
                                  STATEICP == 40 & COUNTYICP == 130  ~ 2150,
                                  STATEICP == 40 & COUNTYICP == 5100 ~ 2150,
                                  STATEICP == 40 & COUNTYICP == 150  ~ 2200,
                                  STATEICP == 40 & COUNTYICP == 7900 ~ 2200,
                                  STATEICP == 40 & COUNTYICP == 310  ~ 2300,
                                  STATEICP == 40 & COUNTYICP == 6800 ~ 2300,
                                  STATEICP == 40 & COUNTYICP == 530  ~ 2400,
                                  STATEICP == 40 & COUNTYICP == 7300 ~ 2400,
                                  STATEICP == 40 & COUNTYICP == 550  ~ 2500,
                                  STATEICP == 40 & COUNTYICP == 6500 ~ 2500,
                                  STATEICP == 40 & COUNTYICP == 690  ~ 2600,
                                  STATEICP == 40 & COUNTYICP == 8400 ~ 2600,
                                  STATEICP == 40 & COUNTYICP == 870  ~ 2700,
                                  STATEICP == 40 & COUNTYICP == 7600 ~ 2700,
                                  STATEICP == 40 & COUNTYICP == 890  ~ 2800,
                                  STATEICP == 40 & COUNTYICP == 6900 ~ 2800,
                                  STATEICP == 40 & COUNTYICP == 950  ~ 2900,
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
                                  STATEICP == 44 & COUNTYICP == 410  ~ 1210,
                                  STATEICP == 44 & COUNTYICP == 2030 ~ 1210,
                                  STATEICP == 44 & COUNTYICP == 1210 ~ 1210,
                                  TRUE ~ COUNTYICP
                                  )) %>%
    #rldf3929 = growth in retail sales from 1929 to 1939
    left_join(retailsales %>% dplyr::select(STATE, NDMTCODE, RLDF3929, RRTSAP29, RRTSAP33, RRTSAP39, RLDF3329, RLDF3933), 
              by = c("STATEICP"="STATE","COUNTYTEMP"="NDMTCODE")) %>% 
    dplyr::select(-COUNTYTEMP)
} #!#! CHECKED

# summarizes dataset by year and demgroup: 
# for each year x demgroup, computes share of LF that is in demgroup and share of demgroup that is in LF,
#   and also computes share of teachers that are in demgroup and share of demgroup that are teachers
#   if wide = TRUE, returns year-level dataset, with demgroup x variable in columns
lf_summ_demgroup <- function(dataset, outvars = c("lfp", "pctlf"), wide = TRUE){
  outdata <- dataset %>% 
    group_by(YEAR, demgroup) %>%
    summarize(pop = sum(ifelse(AGE >= 16 & AGE <= 64, PERWT, 0)), #population of demgroup x year
              numlf= sum(ifelse(LABFORCE == 2 & AGE >= 16 & AGE <= 64, PERWT, 0)), #lf size of demgroup x year
              numteach = sum(ifelse(OCC1950 == 93 & CLASSWKR == 2 & LABFORCE == 2 & AGE >= 16 & AGE <= 64, PERWT, 0)), #num teachers
              lfp = sum(ifelse(LABFORCE == 2 & AGE >= 16 & AGE <= 64, PERWT, 0))/sum(ifelse(AGE >= 16 & AGE <= 64, PERWT, 0)), #share of group in lf
              teachshare = sum(ifelse(OCC1950 == 93 & CLASSWKR == 2 & LABFORCE == 2 & AGE >= 16 & AGE <= 64, PERWT, 0))/sum(ifelse(LABFORCE == 2 & AGE >= 16 & AGE <= 64, PERWT, 0)) #share of workers that are teachers in group
                ) %>%
    group_by(YEAR) %>%
    mutate(pctlf = numlf/sum(numlf),# share of lf in demgroup
           pctteach = numteach/sum(numteach) #share of teachers that are in demgroup
           ) %>%
    ungroup()
  
  if (wide == TRUE){
    outdata_wide <- outdata %>%
      pivot_wider(id_cols = YEAR, names_from = demgroup, values_from = all_of(outvars))
    return(outdata_wide)
  }
  
  return(outdata)
}

#________________________
# LINKING ----
#________________________
# Adds useful/common person-level variables FOR LINKED DATA, requires indiv-level LINKED dataset 
#   with variables OCC1950, SEX, AGE, MARST (with IPUMS codings) and suffixes _base and _link
addvars_indiv_linked <- function(dataset){
  outdata <- dataset %>% 
    mutate(demgroup_base  = case_when(SEX_base == 1 ~ "M",
                                      SEX_base == 2 & MARST_base != 1 & MARST_base != 2 ~ "SW",
                                      TRUE ~ "MW"),
           demgroup_link  = case_when(SEX_link == 1 ~ "M",
                                      SEX_link == 2 & MARST_link != 1 & MARST_link != 2 ~ "SW",
                                      TRUE ~ "MW"),
           marst_link     = ifelse(MARST_link == 1 | MARST_link == 2, 1, 0), 
           worker_base    = ifelse(YEAR_base == 1900, 
                                ifelse(OCC1950_base != 999 & AGE_base >= 16 & AGE_base <= 64, 1, 0), #in 1900, no LABFORCE so use those with occupation
                                ifelse(LABFORCE_base == 2  & AGE_base >= 16 & AGE_base <= 64, 1, 0)), #otherwise, those in LABFORCE 
           worker_link    = ifelse(LABFORCE_link == 2 & AGE_link >= 16 & AGE_link <= 64, 1, 0),
           teacher_base   = ifelse(YEAR_base == 1900,
                                 ifelse(OCC1950_base == 93 & worker_base == 1, 1, 0), #in 1900, no CLASSWKR
                                 ifelse(OCC1950_base == 93 & CLASSWKR_base == 2 & worker_base == 1, 1, 0)),
           teacher_link   = ifelse(OCC1950_link == 93 & CLASSWKR_link == 2 & worker_link == 1, 1, 0),
           secretary_base = ifelse(YEAR_base == 1900,
                                   ifelse(OCC1950_base == 350 & worker_base == 1, 1, 0), #in 1900, no CLASSWKR
                                   ifelse(OCC1950_base == 350 & CLASSWKR_base == 2 & worker_base == 1, 1, 0)),
           secretary_link = ifelse(OCC1950_link == 350 & CLASSWKR_link == 2 & worker_link == 1, 1, 0),
           occcat_link = case_when(OCC1950_link < 100 ~ "Professional and Technical",
                              OCC1950_link >= 200 & OCC1950_link < 300 ~ "Managers, Officials, Proprietors",
                              OCC1950_link >= 300 & OCC1950_link < 400 ~ "Clerical Workers",
                              OCC1950_link >= 400 & OCC1950_link < 500 ~ "Sales Workers",
                              OCC1950_link >= 500 & OCC1950_link < 600 ~ "Craftsmen",
                              OCC1950_link >= 600 & OCC1950_link < 700 ~ "Operative Workers",
                              OCC1950_link >= 700 & OCC1950_link <= 720 ~ "Private Household Workers",
                              OCC1950_link >= 730 & OCC1950_link < 800 ~ "Non-HH Service Workers",
                              (OCC1950_link >= 800 & OCC1950_link < 900) | (OCC1950_link >= 100 & OCC1950_link <= 123) ~ "Farm Workers/Owners",
                              OCC1950_link >= 900 & OCC1950_link <= 970 ~ "Non-Farm Laborers",
                              TRUE ~ "Not Classified/Non-Occupational Response"
           ))
  return(outdata)
} #!#! CHECKED

# summarizes linked dataset at the county level with key variables -- FOR TEACHERS
summlinks <- function(dataset, n = 10){
  outdata <- dataset %>% 
    group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
    summlinks_help(n = n) %>%
    rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
    collect() %>%
    addvars_county()
  
  # using helper function to test if FIPS in main linked sample
  mainlinksamplist = mainlinksamp(outdata, n = n)
  outdata <- outdata %>% 
    mutate(mainsamp = ifelse(FIPS %in% mainlinksamplist, 1, 0))
  
  return(outdata)
} #!#! CHECKED

summlinks_state <- function(dataset, n = 10){
  outdata <- dataset %>% 
    group_by(STATEICP_base, YEAR_base, YEAR_link) %>%
    summlinks_help(n=n) %>%
    rename(STATEICP = STATEICP_base, YEAR = YEAR_link) %>%
    collect()
}

# helper with all the guts of the summlinks (other than grouping)
summlinks_help <- function(dataset, n = 10){
  outdata <- dataset %>% 
    summarize(nlink       = n(),
              pct_t       = sum(ifelse(teacher_link == 1, 1, 0))/n(),
              pct_marr    = sum(ifelse(marst_link == 1, 1, 0))/n(),
              pct_lf = sum(ifelse(worker_link == 1, 1, 0))/n(),
              pct_nilf    = sum(ifelse(worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later mw and not in lf
              pct_mw      = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
              pct_mwt     = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw teach
              pct_mwnt    = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw non teach but in lf
              pct_mwnilf  = sum(ifelse(demgroup_link == "MW" & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later mw and not in lf
              pct_mwt_cond = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)),
              pct_mwnt_cond = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)),
              pct_mwnilf_cond = sum(ifelse(demgroup_link == "MW" & worker_link == 0, 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)),
              pct_mw_cond_nilf = sum(ifelse(demgroup_link == "MW" & worker_link == 0, 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)), #share of MW 
              pct_mw_cond_inlf = sum(ifelse(demgroup_link == "MW" & worker_link == 1, 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)), #sha, #share of sample (swt_base) that are later mw and not in lf
              pct_mwprof = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & occcat_link == "Professional and Technical", 1, 0))/n(),
              pct_mwclerical = sum(ifelse(demgroup_link == "MW" & occcat_link == "Clerical Workers", 1, 0))/n(),
              pct_mwsales = sum(ifelse(demgroup_link == "MW" & occcat_link == "Sales Workers", 1, 0))/n(),
              pct_mwoperative = sum(ifelse(demgroup_link == "MW" & occcat_link == "Operative Workers", 1, 0))/n(),
              pct_mwwhitecollar = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & occcat_link %in% c("Professional and Technical", "Managers, Officials, Proprietors",
                                                                                                            "Clerical Workers", "Sales Workers"), 1, 0))/n(),
              pct_mwwhitecollar_cond = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & occcat_link %in% c("Professional and Technical", "Managers, Officials, Proprietors",
                                                                                                                 "Clerical Workers", "Sales Workers"), 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)),
              pct_mwbluecollar = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & occcat_link %in% c("Craftsmen","Operative Workers", "Private Household Workers",
                                                                                                           "Non-HH Service Workers", "Farm Workers/Owners", "Non-Farm Laborers"), 1, 0))/n(),
              pct_mwbluecollar_cond = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & occcat_link %in% c("Craftsmen","Operative Workers", "Private Household Workers",
                                                                                                                "Non-HH Service Workers", "Farm Workers/Owners", "Non-Farm Laborers"), 1, 0))/sum(ifelse(demgroup_link == "MW", 1, 0)),
              pct_sw      = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
              pct_swt     = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw teach
              pct_swnt    = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw non teach but in lf
              pct_swnilf  = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later sw and not in lf
              pct_swt_cond = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)),
              pct_swnt_cond = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)),
              pct_swnilf_cond = sum(ifelse(demgroup_link == "SW" & worker_link == 0, 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)),
              pct_sw_cond_nilf = sum(ifelse(demgroup_link == "SW" & worker_link == 0, 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)), #share of MW 
              pct_sw_cond_inlf = sum(ifelse(demgroup_link == "SW" & worker_link == 1, 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)), #sha, #share of sample (swt_base) that are later mw and not in lf
              pct_swprof = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & occcat_link == "Professional and Technical", 1, 0))/n(),
              pct_swclerical = sum(ifelse(demgroup_link == "SW" & occcat_link == "Clerical Workers", 1, 0))/n(),
              pct_swsales = sum(ifelse(demgroup_link == "SW" & occcat_link == "Sales Workers", 1, 0))/n(),
              pct_swoperative = sum(ifelse(demgroup_link == "SW" & occcat_link == "Operative Workers", 1, 0))/n(),
              pct_swwhitecollar = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & occcat_link %in% c("Professional and Technical", "Managers, Officials, Proprietors",
                                                                                                            "Clerical Workers", "Sales Workers"), 1, 0))/n(),
              pct_swwhitecollar_cond = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & occcat_link %in% c("Professional and Technical", "Managers, Officials, Proprietors",
                                                                                                                 "Clerical Workers", "Sales Workers"), 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)),
              pct_swbluecollar = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & occcat_link %in% c("Craftsmen","Operative Workers", "Private Household Workers",
                                                                                                           "Non-HH Service Workers", "Farm Workers/Owners", "Non-Farm Laborers"), 1, 0))/n(),
              pct_swbluecollar_cond = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & occcat_link %in% c("Craftsmen","Operative Workers", "Private Household Workers",
                                                                                                                "Non-HH Service Workers", "Farm Workers/Owners", "Non-Farm Laborers"), 1, 0))/sum(ifelse(demgroup_link == "SW", 1, 0)),
              avg_occscore = mean(ifelse(worker_link == 1, OCCSCORE_link, 0)), #non-workers should have occscore of 0
              avg_occscore_sw = mean(case_when(demgroup_link != "SW" ~ NA_integer_, 
                                               worker_link == 1 ~ OCCSCORE_link, 
                                               TRUE ~ 0),na.rm=TRUE),
              avg_occscore_mw = mean(case_when(demgroup_link != "MW" ~ NA_integer_, 
                                               worker_link == 1 ~ OCCSCORE_link, 
                                               TRUE ~ 0),na.rm=TRUE),
              pct_wc      = sum(ifelse(NCHILD_link > 0, 1, 0))/n(), #share of sample (swt_base) that later have children (teach + nonteach)
              pct_wct     = sum(ifelse(NCHILD_link > 0 & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and teach
              pct_wcnt    = sum(ifelse(NCHILD_link > 0 & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and work, but not as teachers
              pct_wcnilf  = sum(ifelse(NCHILD_link > 0 & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that later have children and exit lf
              pct_wnc     = sum(ifelse(NCHILD_link == 0, 1, 0))/n(), #share of sample (swt_base) that later do not have children
              pct_wnct    = sum(ifelse(NCHILD_link == 0 & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that later don't have children and teach
              pct_wncnt   = sum(ifelse(NCHILD_link == 0 & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that don't have children and work, but not as teachers
              pct_wncnilf = sum(ifelse(NCHILD_link == 0 & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that don't have children and exit lf
    )
  
  return(outdata)
}

# summarizes linked dataset at the county level with key variables -- FOR SECRETARIES
summlinks_sec <- function(dataset, n = 10){
  outdata <- dataset %>% 
    group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
    summarize(nlink       = n(),
              pct_s       = sum(ifelse(secretary_link == 1, 1, 0))/n(),
              pct_marr    = sum(ifelse(marst_link == 1, 1, 0))/n(),
              pct_nilf    = sum(ifelse(worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later mw and not in lf
              pct_mw      = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
              pct_mws     = sum(ifelse(demgroup_link == "MW" & secretary_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw teach
              pct_mwns    = sum(ifelse(demgroup_link == "MW" & secretary_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw non teach but in lf
              pct_mwnilf  = sum(ifelse(demgroup_link == "MW" & secretary_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later mw and not in lf
              pct_sw      = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
              pct_sws     = sum(ifelse(demgroup_link == "SW" & secretary_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw teach
              pct_swns    = sum(ifelse(demgroup_link == "SW" & secretary_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw non teach but in lf
              pct_swnilf  = sum(ifelse(demgroup_link == "SW" & secretary_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later sw and not in lf
              pct_swprof = sum(ifelse(demgroup_link == "SW" & occcat_link == "Professional and Technical", 1, 0))/n(),
              pct_swclerical = sum(ifelse(demgroup_link == "SW" & secretary_link == 0 & occcat_link == "Clerical Workers", 1, 0))/n(),
              pct_swsales = sum(ifelse(demgroup_link == "SW" & occcat_link == "Sales Workers", 1, 0))/n(),
              pct_swoperative = sum(ifelse(demgroup_link == "SW" & occcat_link == "Operative Workers", 1, 0))/n(),
              pct_swwhitecollar = sum(ifelse(demgroup_link == "SW" & secretary_link == 0 & occcat_link %in% c("Professional and Technical", "Managers, Officials, Proprietors",
                                                                                                            "Clerical Workers", "Sales Workers"), 1, 0))/n(),
              pct_swbluecollar = sum(ifelse(demgroup_link == "SW" & secretary_link == 0 & occcat_link %in% c("Craftsmen","Operative Workers", "Private Household Workers",
                                                                                                           "Non-HH Service Workers", "Farm Workers/Owners", "Non-Farm Laborers"), 1, 0))/n(),
              pct_wc      = sum(ifelse(NCHILD_link > 0, 1, 0))/n(), #share of sample (swt_base) that later have children (teach + nonteach)
              pct_wct     = sum(ifelse(NCHILD_link > 0 & secretary_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and teach
              pct_wcns    = sum(ifelse(NCHILD_link > 0 & secretary_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and work, but not as secretarys
              pct_wcnilf  = sum(ifelse(NCHILD_link > 0 & secretary_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that later have children and exit lf
              pct_wnc     = sum(ifelse(NCHILD_link == 0, 1, 0))/n(), #share of sample (swt_base) that later do not have children
              pct_wncs    = sum(ifelse(NCHILD_link == 0 & secretary_link == 1, 1, 0))/n(), #share of sample (swt_base) that later don't have children and teach
              pct_wncns   = sum(ifelse(NCHILD_link == 0 & secretary_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that don't have children and work, but not as secretarys
              pct_wncnilf = sum(ifelse(NCHILD_link == 0 & secretary_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that don't have children and exit lf
    ) %>%
    rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
    collect() %>%
    addvars_county()
  
  # using helper function to test if FIPS in main linked sample
  mainlinksamplist = mainlinksamp(outdata, n = n)
  outdata <- outdata %>% 
    mutate(mainsamp = ifelse(FIPS %in% mainlinksamplist, 1, 0))
  
  return(outdata)
} #!#! CHECKED

# takes filtered duckdb linked dataset, summarizes county level stuff, merges to cleaned individual level stuff
indiv_link_clean <- function(dataset, matches = NA, n = 10){
  link_ct <- dataset %>% summlinks(n) %>% left_join(matches %>% group_by(FIPS, match_weight1, match_weight2, match_weight3) %>% summarize(), 
                                                      by = "FIPS")
  
  link_clean <- dataset %>% 
    mutate(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link, RACE = RACE_base, URBAN = URBAN_base, AGE = AGE_base) %>%
    select(STATEICP, COUNTYICP, YEAR, demgroup_base, demgroup_link, teacher_link, worker_link, RACE, URBAN,AGE,
           OCCSCORE_link, NCHILD_link, move, move_state) %>%
    collect() %>% 
    left_join(link_ct %>% select(STATEICP, COUNTYICP, YEAR_base, YEAR, mainsamp, neighbor_samp, neighbor_sampNC, neighbor_sampKY,
                                 FIPS, SOUTH, TREAT, nlink, match_weight1, match_weight2, match_weight3),
              by = c("STATEICP", "COUNTYICP", "YEAR")
              ) %>%
    mutate(
      married_link = ifelse(demgroup_link == "MW", 1, 0),
      mwt_link = ifelse(teacher_link & demgroup_link == "MW", 1, 0),
      mwnt_link = ifelse(teacher_link == 0 & worker_link == 1 & demgroup_link == "MW", 1, 0),
      mwnilf_link = ifelse(worker_link == 0 & demgroup_link == "MW", 1, 0),
      unmarried_link = ifelse(demgroup_link == "SW", 1, 0),
      swt_link = ifelse(teacher_link & demgroup_link == "SW", 1, 0),
      swnt_link = ifelse(teacher_link == 0 & worker_link == 1 & demgroup_link == "SW", 1, 0),
      swnilf_link = ifelse(worker_link == 0 & demgroup_link == "SW", 1, 0)) 
  
  return(link_clean)
}

# same as above for secretaries
indiv_link_clean_sec <- function(dataset, matches, n = 10){
  link_ct <- dataset %>% summlinks_sec(n) %>% left_join(matches %>% group_by(FIPS, match_weight1, match_weight2, match_weight3) %>% summarize(), 
                                                        by = "FIPS")
  link_clean <- dataset %>% 
    mutate(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link, RACE = RACE_base, AGE = AGE_base) %>%
    select(STATEICP, COUNTYICP, YEAR, demgroup_base, demgroup_link, secretary_link, worker_link, RACE, AGE) %>%
    collect() %>% 
    left_join(link_ct %>% select(STATEICP, COUNTYICP, YEAR_base, YEAR, mainsamp, neighbor_samp, neighbor_sampNC, neighbor_sampKY,
                                 FIPS, SOUTH, TREAT, nlink, match_weight1, match_weight2, match_weight3),
              by = c("STATEICP", "COUNTYICP", "YEAR")
    ) %>%
    mutate(
      married_link = ifelse(demgroup_link == "MW", 1, 0),
      mws_link = ifelse(secretary_link & demgroup_link == "MW", 1, 0),
      mwns_link = ifelse(secretary_link == 0 & worker_link == 1 & demgroup_link == "MW", 1, 0),
      mwnilf_link = ifelse(worker_link == 0 & demgroup_link == "MW", 1, 0)) 
  
  return(link_clean)
}

# Takes year x county-level LINKED dataset and returns vector of FIPS of counties in main LINKED sample 
#   (only keeping balanced panel of counties with at least n filtered & linked people in 1930 and 1940 and non-missing FIPS)
mainlinksamp <- function(dataset, balanced = TRUE, n = 10, verbose = FALSE){
  if (!("FIPS" %in% names(dataset))){
    dataset <- dataset %>% mergefips()
  }
  
  # identifying counties that are observed in all three links
  balanceddata <- dataset %>% 
    filter(YEAR != 1910) %>% 
    group_by(FIPS) %>% 
    summarize(n = n()) %>% 
    filter(n == 3)
  
  fips1940 <- filter(dataset, YEAR == 1940 & nlink >= n)$FIPS
  outdata <- dataset %>% 
    filter(YEAR == 1930 & nlink >= n & !is.na(FIPS) & #only keeping FIPS with at least n filtered & linked people in each county in 1930
                                  FIPS %in% fips1940) #AND only keeping FIPS with at least n filtered & linked people in each county in 1940
  
  if (balanced){
    outdata <- outdata %>% filter(FIPS %in% balanceddata$FIPS)
  }
  
  if (verbose){
    print(glue("Original Dataset: {length(unique(dataset$FIPS))} counties"))
    print(glue("Main Sample (Counties with >= {n} teachers in 1930 and 1940): {length(unique(outdata$FIPS))} counties"))
    print(glue("Dropped {100*(1-length(unique(outdata$FIPS))/length(unique(dataset$FIPS)))}% of Observations"))
  }
  return(unique(outdata$FIPS))
} #!#! CHECKED

#_________________
# MATCHING ----
#_________________
##!! RE-CHECK ALL MATCHING FUNCS (getfipsmatch = TRUE probably breaks somewhere, didn't check it -- not priority right now but should double check later)

# takes a dataset (long on year x county) and list of variable names 
# (excluding % urban and retail sales) on which to match, plus indicator 
# for using retail sales
# distance, method all arguments for matchit function, pop.size for genetic matching
# IF getfipsmatch is TRUE:
# returns dataframe of control counties including the control county FIPS code, 
# and the state of the treatment county to which it was matched
# ELSE: returns dataframe of all matched counties with weights (all equal to 1 unless full matching)
matching <- function(longdata, varnames, distance = "robust_mahalanobis", method = "nearest", pop.size = 200,
                     getfipsmatch = FALSE, retail = TRUE, verbose = FALSE){
  if (!("FIPS" %in% names(longdata))){
    longdata <- longdata %>% mergefips()
  }
  
  # fips codes of main sample
  mainsampfips = mainsamp(longdata, grp = 'all')
  
  # create lists of names of variables for matching (varnames in 1930 & growth from 1920 to 1930, and retail sales if retail=TRUE)
  filter_varnames = c(glue("{varnames}_1930"),glue("{varnames}_growth1930"))
  
  # add on retail variable names if matching on retail
  if(retail){
    filter_varnames <- c(filter_varnames, "RRTSAP29", "RLDF3329", "RLDF3933") #39 level and growth 29-39
  }
  
  # prepping data for matching: restrict to main sample, 
  # pivot to county-level, merge in retail sales
  matchdata <- longdata %>% 
    filter(FIPS %in% mainsampfips) %>% #keeping only counties in main sample
    pivot_wider(id_cols     = c(FIPS, STATEICP, COUNTYICP, TREAT), #pivoting wide (so only one row per county)
                names_from  = YEAR, 
                values_from = all_of(c(varnames))) %>%
    retailsales() #merging with retail sales
  
  # gen growth variables (for matching) for each var passed to function
  for (var in varnames){
    # if there are zeros, add 0.01 before calculating growth
    if (sum(which(longdata[[var]]==0)) > 0){
      matchdata[[glue("{var}_1930")]] = matchdata[[glue("{var}_1930")]] + 0.01
      matchdata[[glue("{var}_1920")]] = matchdata[[glue("{var}_1920")]] + 0.01
    }
    matchdata[[glue("{var}_growth1930")]] = (matchdata[[glue("{var}_1930")]]-matchdata[[glue("{var}_1920")]])/matchdata[[glue("{var}_1920")]]
  }
  
  # keep only counties with complete non-missing data on matching vars
  matchdata <- matchdata %>%
    filter(if_all(all_of(filter_varnames), function(.x) !is.na(.x) & .x != Inf))
  print(glue("Retention: {length(unique(matchdata$FIPS))} out of {length(unique(longdata$FIPS))} counties"))
  
  # matching treated counties with nontreated counties, drawing matched counties
  match_obj <- matchit(reformulate(filter_varnames, response = "TREAT"), 
                       data     = matchdata, 
                       method = method,
                       distance = distance,
                       pop.size = pop.size) #pop size for genetic matching
  if (verbose){
    print(summary(match_obj))
  }
  
  if (getfipsmatch){
    if (method == "full"){
      print("Error: can't match to treated FIPS when using full matching")
      return(NA)
    }
    # getting matched data WITH corresponding 
    control_matches                  <- matchdata[match_obj$match.matrix,] # match_obj$match.matrix links indices of treated units (rownames) to control units (values)
    control_matches[["STATE_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["STATEICP"]] # creating new var equal to state of treated match
    control_matches[["FIPS_MATCH"]]  <- matchdata[rownames(match_obj$match.matrix),][["FIPS"]] # creating new var equal to fips of treated match
    
    return(dplyr::select(bind_rows(control_matches, 
                            get_matches(match_obj) %>% 
                              filter(TREAT == 1) %>% 
                              mutate(STATE_MATCH = STATEICP, FIPS_MATCH = FIPS)), 
                  c(FIPS, STATE_MATCH, FIPS_MATCH)))
  }
  else{
    return(dplyr::select(match.data(match_obj), c(FIPS, weights)))
  }
} 

# joining outputs of matching (where matches is a list of matching sets) with dataset;
# specifically, adds indicators "match_samp1", "match_samp2" to dataset that flag whether
# the county in a given row appears in a matched dataset or not, either as a
# treated or control county
matching_join <- function(dataset, matchlist, getfipsmatch = FALSE){
  for (i in 1:length(matchlist)){
    if (i == 1 & getfipsmatch){ # for first matching dataset, keep state of match IF getfipsmatch = TRUE
      dataset <- dataset %>% 
        left_join(matchlist[[i]] %>% mutate(match = 1) %>% dplyr::select(-FIPS_MATCH), # note: keep STATE_MATCH here for state_matching fcn
                  by = c("FIPS")) %>%  
        mutate(match_samp1 = ifelse(match == 1, 1, 0))
    }
    else{
      dataset <- dataset %>% left_join(matchlist[[i]]) %>%
        mutate("match_weight{i}" := ifelse(is.na(weights), 0, weights)) %>% dplyr::select(-weights)
    }
  }
  return(dataset)
}

#________________________________________________________
# PRODUCING DID GRAPHS ----
#________________________________________________________
# Adding interaction dummies to dataset for dynamic DiD
#   Takes in dataset with YEAR and TREAT, and creates indicators for YEAR x TREAT for all years
add_did_dummies <- function(dataset){
  for (yr in unique(dataset$YEAR)){
    dataset[[glue("TREATx{yr}")]] <- ifelse(dataset$YEAR == yr, 1, 0)*ifelse(dataset$TREAT == 1, 1, 0)
    dataset[[glue("Year{yr}")]]   <- ifelse(dataset$YEAR == yr, 1, 0)
  }
  return(dataset)
} #!#! CHECKED

# Creating data for graphing dynamic DiD (if using weights, must be in dataset with name weight)
# takes in dataset, depvar (dependant variable), any controls (string of form '+ X1 + X2 +...'), years to include, year to omit
#   if table = TRUE, returns list of regression model output and vcov for stargazer table formatting
#   if septreat = TRUE, runs regression for treatment and control groups separately
did_graph_data <- function(dataset, depvar, controls = "", 
                           years = c(1910, 1920, 1940, 1950), yearomit = 1930, clus = "FIPS",
                           verbose = FALSE, table = FALSE, septreat = FALSE){
  # modifying dataset (adding interaction terms, FIPS vars for clustering, 
  #   filter to only include relevant years, setting weights to 1 if they don't exist)

  regdata <- dataset %>% 
    add_did_dummies() %>% 
    filter(YEAR %in% c(years, yearomit))
  
  # if weight not already a variable, just weight by vector of 1s
  if(!("weight" %in% names(dataset))){
    regdata$weight <- 1 
  }
  
  if (septreat){ # if septreat==TRUE, run regs separately by treatment group
    yearvars     <- glue("Year{years}")
    # for control group
    did_reg_ctrl <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) +  {controls}"),  
                       data = regdata %>% filter(TREAT == 0), weights = weight)
    vcov_ctrl    = vcovCL(did_reg_ctrl, cluster = filter(regdata, TREAT == 0)[[clus]], type = "HC1")
    # for treated group
    did_reg_treat <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) {controls}"), 
                        data = regdata %>% filter(TREAT == 1), weights = weight)
    vcov_treat    = vcovCL(did_reg_treat, cluster = filter(regdata, TREAT == 1)[[clus]], type = "HC1")
    # make dataframe for output to return 
    if (table){ # for stargazer 
      return(list(did_reg_ctrl, vcov_ctrl, did_reg_treat, vcov_treat))
    }
    else { # standalone table of point estimates/CIs
      # include omitted year (1930) for plotting, with point estimate/CIs = 0
      effects <- data.frame(y      = c(sapply(yearvars, function (.x) did_reg_ctrl$coefficients[[.x]]), 0), 
                            depvar = depvar,
                            year   = c(years, yearomit),
                            var    = c(sapply(yearvars, function(.x) as.numeric(diag(vcov_ctrl)[[.x]])), 0),
                            treat  = "Control") %>%
        rbind(data.frame(y      = c(sapply(yearvars, function (.x) did_reg_treat$coefficients[[.x]]), 0),
                         depvar = depvar,
                         year   = c(years, yearomit),
                         var    = c(sapply(yearvars, function(.x) as.numeric(diag(vcov_treat)[[.x]])), 0),
                         treat  = "Treated")) %>%
        mutate(y_ub = y + 1.96*sqrt(var),
               y_lb = y - 1.96*sqrt(var))
      return(effects)
    }
  } 
  else { # otherwise, if septreat==FALSE, run standard DiD regression
    # vector of interaction terms
    interact_vars <- glue("TREATx{years}")
    yearvars <- glue("Year{years}")
    # run reg: include year and county FE + interaction terms + any controls
    did_reg <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) + 
                       {glue_collapse(interact_vars, sep = '+')} {controls}"), 
                  data = regdata, weights = weight)
    if (verbose){
      print(summary(did_reg))
    }
    # clustered standard errors (hc1 equiv to ,robust in stata)
    vcov = vcovCL(did_reg, cluster = regdata[[clus]], type = "HC1")
    #vcov = vcovCL(did_reg, type = "HC1")
    # return table of estimates as output
    if (table){
      return(list(did_reg, vcov))
    }
    else {
      # make dataframe for output -- y is coef, var is estimated variance using clustered standard errors
      effects <- data.frame(y      = c(sapply(interact_vars, function (.x) did_reg$coefficients[[.x]]), 0),
                            depvar = depvar,
                            year   = c(years, yearomit),
                            var    = c(sapply(interact_vars, function(.x) as.numeric(diag(vcov)[[.x]])), 0)) %>%
        mutate(y_ub = y + 1.96*sqrt(var),
               y_lb = y - 1.96*sqrt(var))
      return(effects)
    }
  } # end ifelse
} #!#! CHECKED

# Creating dynamic DiD graph
# takes in all parameters of did_graph_data (with list of dep vars), 
# as well as vector of labels for dep vars and labels for graph
#   and toggles for slides (default is for paper) 
#   and steps (i.e. saving versions of the graph with points gradually revealed -- default is no)
#   and pointspan, i.e. total width of all dots for a given year, default is 2
did_graph <- function(dataset, depvarlist, depvarnames, colors, controls = "", pointtypes = NA,
                      years = c(1910, 1920, 1940, 1950), yearomit = 1930, 
                      verbose = FALSE, yvar = "Coef on Treat X Year", clus = "STATEICP",
                      ymax = NA, ymin = NA, fig_width = 8, fig_height = 5,
                      slides = FALSE, steps = FALSE, pointspan = 2, 
                      septreat = FALSE, filename = NA){
  # check that varlist and namelist passed to function are same length
  nvars = length(depvarlist)
  if (nvars != length(depvarnames)){
    print("Error: depvarlist length diff from depvarnames length")
    return(NA)
  }

  # make tables ----
  # make table of DiD regression outputs (in format for graphing) from all dependent variables
  if (nvars == 1){ # if only one depvar is specified
    did_data_temp <- did_graph_data(dataset, depvarlist[[1]], controls, 
                                    years, yearomit, clus,
                                    verbose, septreat, table = FALSE) 
    did_data      <- did_data_temp %>%
      mutate(group      = depvarnames[[1]], 
             year_graph = did_data_temp$year)
  }
  else{ # if more than one depvar is specified
    if (septreat){
      print("Error: can only use septreat for single variable")
      return(NA)
    }
    # create separate reg tables for each depvar, then bind together
    did_datasets <- list()
    for (i in seq(1,nvars)){
      did_data_temp <- did_graph_data(dataset, depvarlist[[i]], controls, 
                                      years, yearomit, clus, verbose) %>%
        mutate(group      = depvarnames[[i]], 
               year_graph = year - pointspan/2 + (i-1)*(pointspan/(nvars - 1))) # shifting over so dots don't overlap
      did_datasets[[i]] <- did_data_temp
    }
    did_data <- bind_rows(did_datasets)
  }
  if(verbose){
    print(did_data)
  } # end make tables

  # make graphs ----
  if (septreat){ # if only one dep var
    graph_out <- ggplot(did_data, aes(x = year_graph, 
                                      y = y, 
                                      color = treat, ##! Where is treat generated?
                                      shape = treat)) + 
      geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
      geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
      annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
      geom_point(size = 4) + labs(x = "Year", y = yvar, color = "", shape = "") + theme_minimal() + 
      theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")
    
    return(graph_out)
  } else { # if more than one dep var
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
    
    
    if(!is.na(pointtypes)){
      graph_out <- graph_out + scale_shape_manual(values = pointtypes)
    }
    
    # adjust ymin/ymax 
    if (!is.na(ymax) | !is.na(ymin)){ # if ymin/ymax bounds are specified, and ...
      if (ymax > max(did_data$y_ub) & ymin < min(did_data$y_lb)){ # the observed ymin/ymax are within said bounds
        graph_out <- graph_out + 
          ylim(ymin,ymax) + 
          geom_text(aes(x = 1935.5, y = ymin + (ymax-ymin)/10, 
                        label = "Marriage Bars \n Removed"), color = "#656565")   
      }
      else{ # the observed ymin/ymax are outside of said bounds
        print("Warning: ymin/ymax out of bounds") ##! changed from "Error" just so that it doesn't seem like a calc was wrong!
        graph_out <- graph_out + geom_text(aes(x = 1935.5, 
                                               y = min(y_lb) + (max(y_ub) - min(y_lb))/10, 
                                               label = "Marriage Bars \n Removed"), color = "#656565") 
      }
    }
    else{ # no ymin/ymax bound was specified
      graph_out <- graph_out + geom_text(aes(x = 1935.5, 
                                             y = min(y_lb) + (max(y_ub) - min(y_lb))/10, 
                                             label = "Marriage Bars \n Removed"), color = "#656565") 
    }
    
    # save graphs
    if (!is.na(filename) & !slides){ #saving graph in folder for paper figs
      ggsave(glue("{outfigs}/paper/{filename}.png"), graph_out, width = fig_width, height = fig_height)
    }
    if (!is.na(filename) & slides){ #changing text size for slides and saving in folder for slide figs
      ggsave(glue("{outfigs}/slides/{filename}.png"), graph_out + 
               theme(text = element_text(size = 18), 
                     axis.text = element_text(size = 14)), width = 8, height = 5)
    }
    return(graph_out)
  } # end make graphs

} #!#! CHECKED
#__________________________
# SYNTHETIC DID ----
#__________________________
panel.matrices.new <- function(panel, unit = 1, time = 2, outcome = 3, treatment = 4, treated.last = TRUE) {
  keep = c(unit, time, outcome, treatment)
  if (!all(keep %in% 1:ncol(panel) | keep %in% colnames(panel))) {
    stop("Column identifiers should be either integer or column names in `panel`.")
  }
  index.to.name = function(x) { if(x %in% 1:ncol(panel)) { colnames(panel)[x] } else { x } }
  unit = index.to.name(unit)
  time = index.to.name(time)
  outcome = index.to.name(outcome)
  treatment = index.to.name(treatment)
  keep = c(unit, time, outcome, treatment)
  
  panel = panel[keep]
  if (!is.data.frame(panel)){
    stop("Unsupported input type `panel.`")
  }
  if (anyNA(panel)) {
    stop("Missing values in `panel`.")
  }
  # if (length(unique(panel[, treatment])) == 1) { ERROR: NOT A VECTOR, WILL ALWAYS RETURN TRUE
  #   stop("There is no variation in treatment status.")
  # }
  # if (!all(panel[, treatment] %in% c(0, 1))) {
  #   stop("The treatment status should be in 0 or 1.")
  # }
  # Convert potential factor/date columns to character
  panel = data.frame(
    lapply(panel, function(col) {if (is.factor(col) || inherits(col, "Date")) as.character(col) else col}), stringsAsFactors = FALSE
  )
  val <- as.vector(table(panel[, unit], panel[, time]))
  if (!all(val == 1)) {
    stop("Input `panel` must be a balanced panel: it must have an observation for every unit at every time.")
  }
  
  panel = panel[order(panel[, unit], panel[, time]), ]
  num.years = length(unique(panel[, time]))
  num.units = length(unique(panel[, unit]))
  Y = matrix(panel[,outcome], num.units, num.years, byrow = TRUE,
             dimnames = list(unique(panel[,unit]), unique(panel[,time])))
  W = matrix(panel[,treatment], num.units, num.years, byrow = TRUE,
             dimnames = list(unique(panel[,unit]), unique(panel[,time])))
  w = apply(W, 1, any)                         # indicator for units that are treated at any time
  T0 = unname(which(apply(W, 2, any))[1]-1)    # last period nobody is treated
  N0 = sum(!w)
  
  if(! (all(W[!w,] == 0) && all(W[,1:T0] == 0) && all(W[w, (T0+1):ncol(Y)]==1))) {
    stop("The package cannot use this data. Treatment adoption is not simultaneous.")
  }
  
  unit.order = if(treated.last) { order(W[,T0+1], rownames(Y)) } else { 1:nrow(Y) }
  list(Y = Y[unit.order, ], N0 = N0, T0 = T0, W = W[unit.order, ])
}



#__________________________
# MISC ----
#__________________________
# graph treated vs control counties -- default is to plot whole country;
# eastern is TRUE if want to show only eastern states
graph_treatment <- function(dataset, eastern = FALSE, filename = NA, full = FALSE){
  exclude_st = c("HI", "AK") # states to exclude
  if (eastern){ # states to exclude if only want to plot eastern US
    exclude_st = c("CA", "CO", "OR", "TX", "WA", 
                   "ND", "WY", "UT", "NM", "NV", 
                   "SD", "MT", "ID", "AZ", "AK", 
                   "HI", "KS", "NE", "OK",
                   "MN", "AR", "MO", "IA", "LA")
  }
  if (full){ #if full matching, graph weights gradient
    graph_out <- plot_usmap(data = dataset %>% 
                              filter(YEAR == 1940) %>% 
                              mutate(fips = FIPS, weights = cut(weights, quantile(weights, probs = seq(0,1,0.2)))) %>% 
                              dplyr::select(c(fips, weights)), 
                            values = "weights", exclude = exclude_st, color = NA) + 
      theme(legend.position = "right", text = element_text(size = 20)) + 
      scale_fill_manual(values = colorRampPalette(c("white",control_col))(5)) +
      #scale_fill_gradient(low = control_col_min, high = control_col_max) + 
      geom_sf(data = us_map("counties", include = c("KY", "NC")), fill = treat_col, color = NA) +
      labs(fill = "Matching Weights")
  }
  else{
    # plot 
    graph_out <- plot_usmap(data = dataset %>% 
                              filter(YEAR == 1940) %>% 
                              mutate(fips = FIPS, TREAT = ifelse(TREAT == 1, "Treated", "Control")) %>% 
                              dplyr::select(c(fips, TREAT)), 
                            values = "TREAT", color = NA, exclude = exclude_st) +
      theme(legend.position = "right", text = element_text(size = 20)) + 
      scale_fill_manual(breaks = c("Treated", "Control"), values = c(treat_col, control_col)) + 
      labs(fill = "")  
  }
  # save 
  if (!is.na(filename)){
    ggsave(glue("{outfigs}/paper/{filename}.png"), graph_out, width = 8, height = 5)
  }
  return(graph_out)
} #!#! CHECKED

#ggplot colors
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

#________________________________________________________
# TESTER FUNCTIONS/DEPRECATED ----
#________________________________________________________
# testing matching between control and treatment for various samples on variable 'varname'
match_test <- function(dataset, varnames){
  # if multiple variables and multiple samples, return error
  if (length(varnames) > 1 & length(unique(dataset$sample))>1){
    print('ERROR: need single variable OR single sample')
    return(NA)
  }
  # if only one variable, facet wrap by sample
  if (length(varnames) == 1){
    graphdata <- dataset %>% 
      group_by(TREAT, YEAR, sample) %>% 
      summarize(across(all_of(varnames), ~mean(.x, na.rm=TRUE)))
    graph_out <- ggplot(data = graphdata, 
                        aes(x = YEAR, y = .data[[varnames]], 
                            color = factor(TREAT), 
                            shape = factor(TREAT))) + 
      geom_point() + geom_line() + facet_wrap(~sample)
  }
  # if multiple variables and only one sample, pivot long and facet wrap by 
  if (length(varnames) > 1 & length(unique(dataset$sample)) == 1){
    graphdata <- dataset %>% 
      group_by(TREAT, YEAR) %>% 
      summarize(across(all_of(varnames), ~mean(.x, na.rm=TRUE))) %>%
      pivot_longer(all_of(varnames), names_to = "cat", values_to = "val")
    graph_out <- ggplot(data = graphdata, 
                        aes(x = YEAR, y = val, color = factor(TREAT), shape = factor(TREAT))) + 
      geom_point() + geom_line() + facet_wrap(~cat) + ggtitle(dataset$sample[1])
  }
  
  return(graph_out)
}

# matching individual control counties to specific treated states in order to 
# assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before
# matchtype is a string equal to either `neighbor` (using neighboring states analysis) 
# or `match` (using matched counties analysis)
# NOTE: ONLY COVERS MATCHSAMP1 (first matched dataset)
state_matching <- function(dataset, matchtype){
  # gen variable indicating in which state the county in question has been matched to
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
  
  # "update" data on the % of women (teachers or secretaries) married 
  # before and after the laws passing in 1933 and 38
  outdata <- outdata %>% 
    mutate(pct_marr_before_Teacher   = case_when(STATE_MATCH == 47 ~ pct_marr_before3_Teacher, #if matched with NC (or in NC), 'law passes' in 1933
                                                 STATE_MATCH == 51 ~ pct_marr_before8_Teacher, #if matched with KY (or in KY), 'law passes' in 1938
                                                 TRUE ~ NA_real_),
           pct_marr_after_Teacher    = case_when(STATE_MATCH == 47 ~ pct_marr_after3_Teacher, #if matched with NC (or in NC), 'law passes' in 1933
                                                 STATE_MATCH == 51 ~ pct_marr_after8_Teacher, #if matched with KY (or in KY), 'law passes' in 1938
                                                 TRUE ~ NA_real_),
           pct_marr_before_Secretary = case_when(STATE_MATCH == 47 ~ pct_marr_before3_Secretary, #if matched with NC (or in NC), 'law passes' in 1933
                                                 STATE_MATCH == 51 ~ pct_marr_before8_Secretary, #if matched with KY (or in KY), 'law passes' in 1938
                                                 TRUE ~ NA_real_),
           pct_marr_after_Secretary  = case_when(STATE_MATCH == 47 ~ pct_marr_after3_Secretary, #if matched with NC (or in NC), 'law passes' in 1933
                                                 STATE_MATCH == 51 ~ pct_marr_after8_Secretary, #if matched with KY (or in KY), 'law passes' in 1938
                                                 TRUE ~ NA_real_)) %>% 
    dplyr::select(-c(starts_with("pct_marr_before3"),
              starts_with("pct_marr_after3"),
              starts_with("pct_marr_before8"),
              starts_with("pct_marr_after8")))
  return(outdata)
} #!#! CHECKED