# Code check 202402
# Purpose: Independent code to check the main code files
# Author: Carolyn Tsao

#___________________________________
# gen test data ----
#___________________________________
# Generate fake data
set.seed(123) # For reproducibility
fake_data <- data.frame(
  sex     = sample(0:1, 100, replace = TRUE), # 0 or 1
  teacher = sample(0:1, 100, replace = TRUE), # 0 or 1
  county  = sample(1:4, 100, replace = TRUE), # 1 to 4
  year    = sample(c(1910, 1920, 1930, 1940), 100, replace = TRUE), # 1910, 1920, 1930, or 1940
  age     = sample(18:60, 100, replace = TRUE) # Random between 18 and 60
)
# Display the first few rows of the dataset
head(fake_data)

# take random sample of the allyears_raw_samp dataset
samp_counties <- allyears_raw_samp %>% 
  select(c("STATEICP","COUNTYICP")) %>% 
  unique() %>% 
  sample_n(250) 

rs_allyears_raw_samp <- allyears_raw_samp %>% 
  inner_join(samp_counties, by = c("STATEICP","COUNTYICP")) %>% 
  mutate(ELDCH       = sample(c(2, 4, 6), dim(rs_allyears_raw_samp)[1], replace = TRUE),
         ELDCH       = ifelse(NCHILD==0, NA, ELDCH),
         OCC1950_SP  = sample(unique(OCC1950), dim(rs_allyears_raw_samp)[1], replace=TRUE),
         OCCSCORE_SP = sample(unique(OCC1950), dim(rs_allyears_raw_samp)[1], replace=TRUE),
         LIT         = sample(c(0,1), dim(rs_allyears_raw_samp)[1], replace=TRUE))

#___________________________________
# 0_dataclean ----
#___________________________________
## test ungroup vs. adding on groups ----
# with ungrouping
w_ungroup <- rs_allyears_raw_samp %>% 
  group_by(YEAR) %>%
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  ungroup() %>% 
  group_by(YEAR, demgroup) %>%
  mutate(pop = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0))) %>%
  filter(LABFORCE == 2 & AGE >= 18 & AGE <= 64) %>% 
  summarise(pct_dem_teaching  = sum(ifelse(teacher == 1, PERWT, 0))/sum(PERWT),
            numlf             = sum(ifelse(LABFORCE == 2, PERWT, 0)),
            lfp               = numlf/mean(pop),
            numteachers       = sum(ifelse(teacher == 1,PERWT,0)),
            pct_coll_teachers = sum(ifelse(teacher == 1 & coll_above == 1, PERWT, 0))/sum(ifelse(coll_above == 1, PERWT, 0))) %>%
  ungroup() %>% 
  group_by(YEAR) %>%
  mutate(pctteachers = numteachers/sum(numteachers),
         pctlf       = numlf/sum(numlf))
# no ungrouping
wo_ungroup <- rs_allyears_raw_samp %>% 
  group_by(YEAR) %>%
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  group_by(YEAR, demgroup) %>%
  mutate(pop = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0))) %>%
  filter(LABFORCE == 2 & AGE >= 18 & AGE <= 64) %>% 
  summarise(pct_dem_teaching  = sum(ifelse(teacher == 1, PERWT, 0))/sum(PERWT),
            numlf             = sum(ifelse(LABFORCE == 2, PERWT, 0)),
            lfp               = numlf/mean(pop),
            numteachers       = sum(ifelse(teacher == 1,PERWT,0)),
            pct_coll_teachers = sum(ifelse(teacher == 1 & coll_above == 1, PERWT, 0))/sum(ifelse(coll_above == 1, PERWT, 0))) %>%
  group_by(YEAR) %>%
  mutate(pctteachers = numteachers/sum(numteachers),
         pctlf       = numlf/sum(numlf))
# compare
check <- setdiff(w_ungroup, wo_ungroup) # same output
# seems fine. Maybe it's not necessary to ungroup then, even though it's good practice.

# let's understand better what this is doing though.
test1 <- rs_allyears_raw_samp %>% 
  group_by(YEAR) %>%
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  ungroup() 
test2 <- rs_allyears_raw_samp %>% 
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) 
setdiff(test1, test2) # same output

# my edit 
test1 <- rs_allyears_raw_samp %>% 
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  group_by(YEAR, demgroup) %>%
  mutate(pop = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0))) %>%
  filter(LABFORCE == 2 & AGE >= 18 & AGE <= 64) %>% 
  summarise(pct_dem_teaching  = sum(ifelse(teacher == 1, PERWT, 0))/sum(PERWT),
            numlf             = sum(ifelse(LABFORCE == 2, PERWT, 0)),
            lfp               = numlf/mean(pop),
            numteachers       = sum(ifelse(teacher == 1,PERWT,0)),
            pct_coll_teachers = sum(ifelse(teacher == 1 & coll_above == 1, PERWT, 0))/sum(ifelse(coll_above == 1, PERWT, 0))) %>%
  group_by(YEAR) %>%
  mutate(pctteachers = numteachers/sum(numteachers),
         pctlf       = numlf/sum(numlf)) %>% 
  ungroup() 
# the original
test2 <- rs_allyears_raw_samp %>% 
  group_by(YEAR) %>%
  mutate(demgroup   = case_when(SEX == 1 ~ "Men",
                                SEX == 2 & (MARST == 6 | MARST == 3 | MARST == 4 | MARST == 5) ~ "Unmarried Women",
                                TRUE ~ "Married Women"),
         teacher    = ifelse(OCC1950 == 93 & CLASSWKR == 2, 1, 0),
         hs_above   = ifelse(EDUC >= 6, 1, 0),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  group_by(YEAR, demgroup) %>%
  mutate(pop = sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0))) %>%
  filter(LABFORCE == 2 & AGE >= 18 & AGE <= 64) %>% 
  summarise(pct_dem_teaching  = sum(ifelse(teacher == 1, PERWT, 0))/sum(PERWT),
            numlf             = sum(ifelse(LABFORCE == 2, PERWT, 0)),
            lfp               = numlf/mean(pop),
            numteachers       = sum(ifelse(teacher == 1,PERWT,0)),
            pct_coll_teachers = sum(ifelse(teacher == 1 & coll_above == 1, PERWT, 0))/sum(ifelse(coll_above == 1, PERWT, 0))) %>%
  group_by(YEAR) %>%
  mutate(pctteachers = numteachers/sum(numteachers),
         pctlf       = numlf/sum(numlf))
# compare
setdiff(test1, test2) # again, the same 

# conclusion: ungroup() at the end to be safe, and before doing another group_by() 

## test addvar functions ----
test1 <- rs_allyears_raw_samp %>% 
  addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.)
  group_by(YEAR, STATEICP, COUNTYICP) %>% #grouping at the county level
  summarize(POP             = n(), #overall population of county
            WHITEPOP        = sum(ifelse(RACE == 1, 1, 0)), #white population
            NWHITETEACH     = sum(ifelse(teacher==1 & RACE==1, 1, 0)), #number of white teachers
            NWHITEWORK      = sum(ifelse(worker == 1 & RACE == 1, 1, 0)), #number of white workers
            NWHITEMW        = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), # number of white married women
            NWHITESW        = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), # number of white unmarried women
            URBAN           = sum(ifelse(URBAN == 2, 1, 0))/n(), #percent of county living in urban area
            PCT_WHITE       = sum(ifelse(RACE == 1, 1, 0))/n(), # percent of county that is white
            WHITESCHOOLPOP  = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, 1, 0)), #white schoolage population
            LFP             = sum(worker)/sum(ifelse(AGE >= 18 & AGE <= 64, 1, 0)), #share of prime age population that is in LF
            LFP_M           = sum(ifelse(worker != 0 & demgroup == "M",  1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "M",  1, 0)), #lfp for men
            LFP_SW          = sum(ifelse(worker != 0 & demgroup == "SW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "SW", 1, 0)), #lfp for single women
            LFP_MW          = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW", 1, 0)), #lfp for married women
            LFP_WMW         = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW" & RACE == 1, 1, 0)), #lfp for white married women
            PCT_LF_MW       = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are MW
            PCT_LF_WMW      = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are white MW
            PCT_UNDER20     = sum(ifelse(AGE < 20, 1, 0))/n(), #share of pop in each age group
            PCT_20TO39      = sum(ifelse(AGE >= 20 & AGE < 40, 1, 0))/n(), #share of pop in each age group
            PCT_40TO59      = sum(ifelse(AGE >= 40 & AGE < 60, 1, 0))/n(), #share of pop in each age group
            PCT_OVER59      = sum(ifelse(AGE >= 60, 1, 0))/n(), #share of pop in each age group
            NCHILD          = mean(ifelse(demgroup == "MW", NCHILD, NA), na.rm=TRUE), #avg number of children for married women
            PCT_MARR        = sum(ifelse(AGE >= 18 & SEX == 2 & MARST %in% c(1,2), 1, 0))/sum(ifelse(AGE >= 18 & SEX == 2, 1, 0)), #share adult women married
            PCT_LIT         = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)) #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
  ) %>% 
  arrange(STATEICP, COUNTYICP) %>% 
  collect() %>% #pulling into R as dataframe ##! Why do we need to do this? 
  addvars_county() #adding county-level variables (treatment status, FIPS codes, etc.)
# testing without collect
test2 <- rs_allyears_raw_samp %>% 
  addvars_indiv() %>% #helper function to add individual-level variables (demgroup, teacher indicator, etc.)
  group_by(YEAR, STATEICP, COUNTYICP) %>% #grouping at the county level
  summarize(POP             = n(), #overall population of county
            WHITEPOP        = sum(ifelse(RACE == 1, 1, 0)), #white population
            NWHITETEACH     = sum(ifelse(teacher==1 & RACE==1, 1, 0)), #number of white teachers
            NWHITEWORK      = sum(ifelse(worker == 1 & RACE == 1, 1, 0)), #number of white workers
            NWHITEMW        = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), # number of white married women
            NWHITESW        = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), # number of white unmarried women
            URBAN           = sum(ifelse(URBAN == 2, 1, 0))/n(), #percent of county living in urban area
            PCT_WHITE       = sum(ifelse(RACE == 1, 1, 0))/n(), # percent of county that is white
            WHITESCHOOLPOP  = sum(ifelse(RACE == 1 & AGE <= 18 & AGE >= 6, 1, 0)), #white schoolage population
            LFP             = sum(worker)/sum(ifelse(AGE >= 18 & AGE <= 64, 1, 0)), #share of prime age population that is in LF
            LFP_M           = sum(ifelse(worker != 0 & demgroup == "M",  1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "M",  1, 0)), #lfp for men
            LFP_SW          = sum(ifelse(worker != 0 & demgroup == "SW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "SW", 1, 0)), #lfp for single women
            LFP_MW          = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW", 1, 0)), #lfp for married women
            LFP_WMW         = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(AGE >= 18 & AGE <= 64 & demgroup == "MW" & RACE == 1, 1, 0)), #lfp for white married women
            PCT_LF_MW       = sum(ifelse(worker != 0 & demgroup == "MW", 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are MW
            PCT_LF_WMW      = sum(ifelse(worker != 0 & demgroup == "MW" & RACE == 1, 1, 0))/sum(ifelse(worker != 0, 1, 0)), #share of workers that are white MW
            PCT_UNDER20     = sum(ifelse(AGE < 20, 1, 0))/n(), #share of pop in each age group
            PCT_20TO39      = sum(ifelse(AGE >= 20 & AGE < 40, 1, 0))/n(), #share of pop in each age group
            PCT_40TO59      = sum(ifelse(AGE >= 40 & AGE < 60, 1, 0))/n(), #share of pop in each age group
            PCT_OVER59      = sum(ifelse(AGE >= 60, 1, 0))/n(), #share of pop in each age group
            NCHILD          = mean(ifelse(demgroup == "MW", NCHILD, NA), na.rm=TRUE), #avg number of children for married women
            PCT_MARR        = sum(ifelse(AGE >= 18 & SEX == 2 & MARST %in% c(1,2), 1, 0))/sum(ifelse(AGE >= 18 & SEX == 2, 1, 0)), #share adult women married
            PCT_LIT         = sum(ifelse(LIT == 4, 1, 0))/sum(ifelse(LIT != 0 & !is.na(LIT), 1, 0)) #share literate (out of applicable respondents -- 1870-1930 census this is everyone age 10+)
  ) %>% 
  arrange(STATEICP, COUNTYICP) %>% 
  addvars_county() 

setdiff(test1, test2) # they're the same- so maybe I'm missing something about how the dataset is held in memory from tbl
test_county <- test2 

## testing pivot ----
test_county_occ <- rs_allyears_raw_samp %>% 
  addvars_indiv() %>% 
  filter((teacher == 1| secretary == 1) & RACE == 1) %>% #only keeping white teachers and secretaries
  mutate(OCC = ifelse(teacher == 1, "Teacher", "Secretary")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, OCC) %>% # grouping by countyXyear AND teacher/secretary
  summarize(num              = n(), # number of teachers (or secretaries)
            num_mw           = sum(ifelse(demgroup == "MW", 1, 0)), #num of teachers MW
            num_sw           = sum(ifelse(demgroup == "SW", 1, 0)), #num of teachers SW
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
  ) %>% 
  collect() %>% 
  pivot_wider(id_cols     = c(YEAR, STATEICP, COUNTYICP), 
              names_from  = OCC, 
              values_from = -c(YEAR, STATEICP, COUNTYICP, OCC)) # i.e. values come from everywhere except these vars

## test county selection ----
## Initially combining just for matching
test_raw <- test_county %>% 
  full_join(test_county_occ, by = c("YEAR", "STATEICP", "COUNTYICP")) %>%
  filter(YEAR==1900 | YEAR == 1910 | YEAR == 1920 | YEAR == 1930 | YEAR == 1940)# just for my fake data

### ACTUAL FUNCTION CALL
# main sample (default is filter on counties with at least 10 white teachers in 
# 1930 and 1940 AND non-missing FIPS code AND observed in all four years 1910-1940)
#mainsamp_list <- mainsamp(test_raw)

### DECONSTRUCTED FUNCTION: MAINSAMP
##identifying counties that are observed in all four years (1910, 20, 30, 40)
balanceddata <- test_raw %>% 
  filter(YEAR != 1900) %>% 
  group_by(FIPS) %>% 
  summarize(n = n()) %>% 
  filter(n == 4)
n <- 5
fips1940 <- filter(test_raw, YEAR == 1940 & NWHITETEACH >= n)$FIPS
outdata <- test_raw %>% filter(YEAR == 1930 & NWHITETEACH >= n & 
                                !is.na(FIPS) & #only keeping FIPS with at least n white teachers in 1930
                                FIPS %in% fips1940) #only keeping FIPS with at least n white teachers in 1940
balanced = TRUE
if (balanced){
  outdata <- outdata %>% filter(FIPS %in% balanceddata$FIPS)
}
mainsamp_list <- outdata

if (verbose){
  print(glue("Original Dataset: {length(unique(test_raw$FIPS))} counties"))
  print(glue("Main Sample (Counties with >= {n} teachers in 1930 and 1940): {length(unique(outdata$FIPS))} counties"))
  print(glue("Dropped {100*(1-length(unique(outdata$FIPS))/length(unique(test_raw$FIPS)))}% of Observations"))
}
mainsamp_list <- unique(outdata$FIPS)

## test matching, retailsales ----

### ACTUAL FUNCTION CALL 
# matching set 1
# matchvars1 <- c("POP", "PCT_LIT", "PCT_WHITE")
# matches <- matching(test_raw, matchvars1)
# 
# # takes a dataset (long on year x county) and list of variable names 
# # (excluding % urban and retail sales) on which to match, plus indicator 
# # for using retail sales
# # returns dataframe of control counties including the control county FIPS code, 
# # and the state of the treatment county to which it was matched
# 
# #matching <- function(longdata, varnames, distance = "robust_mahalanobis", 
# #                     retail = FALSE, verbose = FALSE){

### DECONSTRUCTED FUNCTION: MATCHING
longdata <- test_raw 
varnames = c("POP", "PCT_WHITE")
distance = "robust_mahalanobis"
retail = FALSE
verbose = TRUE

  if (!("FIPS" %in% names(longdata))){
    longdata <- longdata %>% mergefips()
  }
  
  # fips codes of main sample
  #mainsampfips = mainsamp(longdata)
  mainsampfips = mainsamp_list 
  
  # create lists of names of variables for matching (varnames in 1920, 1930, and retail sales if retail=TRUE)
  # filter_varnames = c(glue("{varnames}_1930"), glue("{varnames}_growth1930"), 
  #                     glue("{varnames}_growth1920")) #c(glue("{varnames}_1910"), glue("{varnames}_1920"),glue("{varnames}_1930"))
  # filter_varnames = c(glue("{varnames}_1930"),glue("{varnames}_1920"),glue("{varnames}_1910"))
  filter_varnames = c(glue("{varnames}_growth1930"),glue("{varnames}_growth1920"))
  match_varnames  = c(filter_varnames,"URBAN_1910", "URBAN_1920", "URBAN_1930")
  
  # add on retail variable names if matching on retail
  if(retail){
    filter_varnames <- c(filter_varnames, "RRTSAP29", "RRTSAP39", "RLDF3929")
    match_varnames  <- c(match_varnames, "RRTSAP29", "RLDF3929")
  }
  
  # prepping data for matching
  matchdata <- longdata %>% 
    filter(FIPS %in% mainsampfips) %>% #keeping only counties in main sample
    pivot_wider(id_cols     = c(FIPS, STATEICP, COUNTYICP, TREAT), #pivoting wide (so only one row per county)
                names_from  = YEAR, 
                values_from = all_of(c(varnames, "URBAN")))
  
  ### ACTUAL FUNCTION CALL 
  # matchdata <- matchdata %>% retailsales() #merging with retail sales
  ### DECONSTRUCTING FUNCTION: RETAILSALES
  # merging a dataset with STATEICP and COUNTYICP variables with retail sales data
    # reading in retail sales data for merging 
    retailsales <- read_xls(glue("{root}/retailsales/retailsales_data.xls")) 
    
    # altering county codes according to Retail-Sales-sources.doc to match retailsales county codes
    matchdata <- matchdata %>%
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
      left_join(retailsales %>% select(STATE, NDMTCODE, RLDF3929, RRTSAP29, RRTSAP39), 
                by = c("STATEICP"="STATE","COUNTYTEMP"="NDMTCODE")) %>% 
      select(-COUNTYTEMP)
    ## END DECONSTRUCTING: RETAILSALES()

  for (var in varnames){
    matchdata[[glue("{var}_growth1930")]] = (matchdata[[glue("{var}_1930")]]-matchdata[[glue("{var}_1920")]])/matchdata[[glue("{var}_1920")]]
    matchdata[[glue("{var}_growth1920")]] = (matchdata[[glue("{var}_1920")]]-matchdata[[glue("{var}_1910")]])/matchdata[[glue("{var}_1910")]]
  }
  
  matchdata <- matchdata %>%
    filter(if_all(all_of(filter_varnames), function(.x) !is.na(.x) & .x != Inf)) %>% # nice!
    filter(!is.na(URBAN_1910) & !is.na(URBAN_1920) & !is.na(URBAN_1930))
  
  print(glue("Retention: {length(unique(matchdata$FIPS))} out of {length(unique(longdata$FIPS))} counties"))
  
  # matching treated counties with nontreated counties, not allowing replacement
  # FOR CODE CHECK ONLY: 
  # make some of the counties treated
  matchdata <- matchdata %>% 
    mutate(TREAT = ifelse(FIPS %in% c("36007", "36071", "36075", "42029", "42069"), 1, TREAT))
  match_obj <- matchit(reformulate(match_varnames, response = "TREAT"),  # cool! 
                       data = matchdata, 
                       REPLACE = FALSE, 
                       distance = distance)
  
  if (verbose){
    print(summary(match_obj))
  }
  
  # getting matched data
  control_matches <- matchdata[match_obj$match.matrix,] # match_obj$match.matrix links indices of treated units (rownames) to control units (values)
  control_matches[["STATE_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["STATEICP"]] # creating new var equal to state of treated match
  control_matches[["FIPS_MATCH"]] <- matchdata[rownames(match_obj$match.matrix),][["FIPS"]] # creating new var equal to fips of treated match
  
  # I think the reasons she does this instead of just taking get_matches is to
  # actually see which counties match to which
matches <- select(bind_rows(control_matches, 
                   get_matches(match_obj) %>% # this gets a matrix with Treat/Control, but doesn't say which county matches to which
                     filter(TREAT == 1) %>% 
                     mutate(STATE_MATCH = STATEICP, FIPS_MATCH = FIPS)), 
                c(FIPS, STATE_MATCH, FIPS_MATCH))
  
# second set of matching
matches2 <- matches

matchlist <- list(matches, matches2)

## test matching_join, state_matching ----
# cleaning county-level combined data, joining with matches

## ORIGINAL CODE 
# countysumm <- countysumm_raw %>% 
#   matching_join(matchlist = matchlist) 

## DECONSTRUCTING: MATCHING_JOIN
      i=1 
      dataset <- test_raw %>% 
        left_join(matchlist[[i]] %>% mutate(match = 1) %>% select(-FIPS_MATCH), 
                  by = c("FIPS", "STATEICP")) %>% 
        mutate(match_samp = ifelse(match == 1, 1, 0))

      i = 2
      dataset[[glue("match_samp{i}")]] <- ifelse(dataset$FIPS %in% matchlist[[i]]$FIPS, 1, 0)
    # seems to generate flag that a certain county in the original county list is 
      # in the matched dataset, either as a control or treated county
   

#helper function to join county-level combined data with list of matched data
test <- dataset %>% 
  mutate(mainsamp              = ifelse(FIPS %in% mainsamp_list, 1, 0), #indicator for whether county is in main sample (see above)
         pct_pop_Teacher       = num_Teacher/WHITEPOP,
         pct_pop_Secretary     = num_Secretary/WHITEPOP,
         pct_workers_Teacher   = num_Teacher/NWHITEWORK, #percentage of workers that are teachers 
         pct_workers_Secretary = num_Secretary/NWHITEWORK, #percentage of workers that are secretaries
         pct_Teacher_mw        = num_mw_Teacher/NWHITEMW, #percentage of white married women that are teachers
         pct_Teacher_sw        = num_sw_Teacher/NWHITESW, #percentage of white unmarried women that are teachers
         teacher_ratio         = WHITESCHOOLPOP/num_Teacher) #ratio of number of teachers to white school-aged pop
         
## ORIGINAL CALL: STATE_MATCHING
# state_matching(matchtype = "neighbor") #helper function to match individual counties to specific states in order to assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before

## DECONSTRUCTING: STATE_MATCHING
# matching individual control counties to specific treated states in order to 
# assign counties to 'law passing' in 1933 or 1938 and use outcome Married After/Married Before
# matchtype is a string equal to either `neighbor` (using neighboring states analysis) 
# or `match` (using matched counties analysis)
matchtype = "neighbor"
dataset <- test

  if (matchtype == "neighbor"){
    outdata <- dataset %>% mutate(STATE_MATCH = case_when(neighbor_sampNC == 1 ~ 47,
                                                          neighbor_sampKY == 1 ~ 51,
                                                          TRUE ~ NA_integer_))
  } else{ 
    outdata <- dataset %>% mutate(STATE_MATCH = case_when(match == 1 ~ STATE_MATCH, ## this only does the state_matching for the first match set?
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
  select(-c(starts_with("pct_marr_before3"),
            starts_with("pct_marr_after3"),
            starts_with("pct_marr_before8"),
            starts_with("pct_marr_after8")))
test <- outdata

## test summlinks, mainlinksamp ----
# Creating 'linked view' -- NOTE: NOT A TABLE/DATAFRAME, 
# just a linking to be filtered/mutated appropriately and collected
#linkview <-  

# group 1: unmarried women teachers in t-10
link1 <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) 
## ACTUAL CODE
# link1 <- link1 %>% summlinks(n=5)
## DECONSTRUCTING: summlinks
# summarizes linked dataset at the county level with key variables
dataset <- link1 
n = 5

  outdata <- dataset %>% 
    group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
    summarize(nlink       = n(),
              pct_t       = sum(ifelse(teacher_link == 1, 1, 0))/n(),
              pct_mw      = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
              pct_mwt     = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw teach
              pct_mwnt    = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw non teach but in lf
              pct_mwnilf  = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later mw and not in lf
              pct_sw      = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
              pct_swt     = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw teach
              pct_swnt    = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw non teach but in lf
              pct_swnilf  = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later sw and not in lf
              pct_wc      = sum(ifelse(NCHILD_link > 0, 1, 0))/n(), #share of sample (swt_base) that later have children (teach + nonteach)
              pct_wct     = sum(ifelse(NCHILD_link > 0 & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and teach
              pct_wcnt    = sum(ifelse(NCHILD_link > 0 & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and work, but not as teachers
              pct_wcnilf  = sum(ifelse(NCHILD_link > 0 & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that later have children and exit lf
              pct_wnc     = sum(ifelse(NCHILD_link == 0, 1, 0))/n(), #share of sample (swt_base) that later do not have children
              pct_wnct    = sum(ifelse(NCHILD_link == 0 & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that later don't have children and teach
              pct_wncnt   = sum(ifelse(NCHILD_link == 0 & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that don't have children and work, but not as teachers
              pct_wncnilf = sum(ifelse(NCHILD_link == 0 & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that don't have children and exit lf
    ) %>%
    rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
    collect() %>%
    addvars_county()
  
  ## ACTUAL ORIGINAL CODE: 
  # using helper function to test if FIPS in main linked sample
  # mainlinksamplist = mainlinksamp(outdata, n = n)
  ## DECONSTRUCTING: mainlinksamp() 
  # Takes year x county-level LINKED dataset and returns vector of FIPS of counties in main LINKED sample 
  #   (only keeping balanced panel of counties with at least n filtered & linked people in 1930 and 1940 and non-missing FIPS)
  #mainlinksamp <- function(dataset, balanced = TRUE, n = 10, verbose = FALSE){
  dataset <- outdata 
  balanced = TRUE 
  n = 5
  
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
    outdata_mainlinksamp <- dataset %>% 
      filter(YEAR == 1930 & nlink >= n & !is.na(FIPS) & #only keeping FIPS with at least n filtered & linked people in each county in 1930
               FIPS %in% fips1940) #AND only keeping FIPS with at least n filtered & linked people in each county in 1940
    
    if (balanced){
      outdata_mainlinksamp <- outdata_mainlinksamp %>% filter(FIPS %in% balanceddata$FIPS)
    }
    
    if (verbose){
      print(glue("Original Dataset: {length(unique(dataset$FIPS))} counties"))
      print(glue("Main Sample (Counties with >= {n} teachers in 1930 and 1940): {length(unique(outdata_mainlinksamp$FIPS))} counties"))
      print(glue("Dropped {100*length(unique(outdata_mainlinksamp$FIPS))/length(unique(dataset$FIPS))}% of Observations"))
    }
  mainlinksamplist <- unique(outdata_mainlinksamp$FIPS)
  ## end mainlinksamp
  
  ## back to testing summlinks()
  outdata <- outdata %>% 
    mutate(mainsamp = ifelse(FIPS %in% mainlinksamplist, 1, 0))
  
link1 <- outdata

# then: 
link1 <- link1 %>% matching_join(matchlist)
# which means that this link is all women who were single teachers in 19XX,
# under the age of 40, in a county with at least 5 such women in EACH? base year,
# and then we collapse the information on these women to the county level,
# and only keep counties that are balanced as in they exist in the data in 1920, 30 and 40
# Is this too restrictive? 

#_______________________________________
# 1_descriptives ----
#_______________________________________



#_______________________________________
# 2_didanalysis ----
#_______________________________________

## test did_graph, did_graph_data ----
# use just a subset of the data
countysumm  <- read_csv(glue("{cleandata}/countysumm_new.csv"))
neighbor    <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)
datasets    <- list(neighbor)
datanames   <- list("neighbor") #, "matched1", "matched2")
# ACTUAL FUNCTION CALL
# did_graph(dataset     = datasets[[i]], 
#           depvarlist  = c("num_Teacher"),
#           depvarnames = c("Number of Teachers"),
#           colors      = c(mw_col),
#           yvar        = "DiD Estimate: Number of Teachers",
#           filename    = glue("numteach_{datanames[[i]]}")) %>% print()
# DECONSTRUCTING: did_graph
# Creating dynamic DiD graph
# takes in all parameters of did_graph_data (with list of dep vars), 
# as well as vector of labels for dep vars and labels for graph
#   and toggles for slides (default is for paper) 
#   and steps (i.e. saving versions of the graph with points gradually revealed -- default is no)
#   and pointspan, i.e. total width of all dots for a given year, default is 2
# did_graph <- function(dataset, depvarlist, depvarnames, 
#                       colors, yvar = "Coef on Treat X Year", controls = "", 
#                       years = c(1910, 1920, 1940), yearomit = 1930, 
#                       verbose = FALSE, ,
#                       ymax = NA, ymin = NA, 
#                       slides = FALSE, steps = FALSE, pointspan = 2, septreat = FALSE, 
#                       filename = NA){
dataset       <- neighbor 
depvarlist    <- c("num_Teacher")
depvarnames   <- c("Nr of teachers")
colors   = c(mw_col)
yvar     = "Coef: Nr of teachers"
filename = glue("numteach_neighbor")
years = c(1910, 1920, 1940) # add in 1950
yearomit = 1930
controls = ""
verbose = FALSE 


  # checks that the names and varlist lengths line up
  nvars = length(depvarlist)
  if (nvars != length(depvarnames)){
    print("Error: depvarlist length diff from depvarnames length")
    #return(NA)
  }
  
  # compiling all regression outputs (in format for graphing) from different dependent variables
  #if (nvars == 1){
    #______________________________________________________
    ## ACTUAL FUNCTION CALL: did_graph_data
    # did_data_temp <- did_graph_data(dataset, depvarlist[[1]], controls, years, yearomit, verbose, septreat) 
    
    ## DECONSTRUCTING: did_graph_data
    # Creating data for graphing dynamic DiD 
    # takes in dataset, depvar (dependant variable), any controls (string of form '+ X1 + X2 +...'), years to include, year to omit
    # if table = TRUE, returns list of regression model output and vcov for stargazer table formatting
    # if septreat = TRUE, runs regression for treatment and control groups separately
    # did_graph_data <- function(dataset, depvar, controls = "", 
    #                            years = c(1910, 1920, 1940), yearomit = 1930, 
    #                            verbose = FALSE, table = FALSE, septreat = FALSE){
    depvar = depvarlist[[1]]
      # modifying dataset (adding interaction terms, setting cluster to FIPS, filtering to only include relevant years)
      # ACTUAL FUNCTION CALL: regdata <- dataset %>% add_did_dummies() ....
      # DECONSTRUCTING: add_did_dummies
      for (yr in unique(dataset$YEAR)){
        dataset[[glue("TREATx{yr}")]] <- ifelse(dataset$YEAR == yr, 1, 0)*ifelse(dataset$TREAT == 1, 1, 0)
        dataset[[glue("Year{yr}")]]   <- ifelse(dataset$YEAR == yr, 1, 0)
      }
      #View(dataset %>% arrange(STATEICP, COUNTYICP, YEAR))
      # end deconstructing add_did_dummies
      
     regdata <- dataset %>% 
        #add_did_dummies() %>%  #done above
        filter(YEAR %in% c(years, yearomit)) %>% 
        mutate(cluster = as.character(FIPS))
     #View(regdata %>% arrange(STATEICP, COUNTYICP, YEAR))
     
     #if (septreat){ # if septreat==TRUE, run regs separately by treatment group
     # pretend septreat==TRUE first
     septreat = TRUE
       yearvars     <- glue("Year{years}")
       if(length(controls)>1) {
         controls = glue("+{glue_collapse(controls, sep='+')}") 
       } else {
         if(controls!="") controls = glue("+{glue_collapse(controls, sep='+')}") 
       }
       # for control group
       did_reg_ctrl <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + cluster {controls}"), 
                          data = regdata %>% filter(TREAT == 0))
       vcov_ctrl    = vcovCL(did_reg_ctrl, type = "HC1")
       # for treated group
       did_reg_treat <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + cluster {controls}"), 
                           data = regdata %>% filter(TREAT == 1))
       vcov_treat    = vcovCL(did_reg_treat, type = "HC1")
       # make dataframe for output to return 
       if (table) { # for stargazer
         list(did_reg_ctrl, vcov_ctrl, did_reg_treat, vcov_treat)
       } else {
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
         View(effects)
       }
     #} # end SEPTREAT = TRUE 
      septreat = FALSE
     else { # otherwise, if septreat==FALSE, run standard DiD regression
       # vector of interaction terms
       interact_vars <- glue("TREATx{years}")
       yearvars <- glue("Year{years}")
       # run reg: include year and county FE + interaction terms + any controls
       did_reg <- lm(glue("{depvar} ~ {glue_collapse(yearvars, sep = '+')} + 
                       cluster + {glue_collapse(interact_vars, sep = '+')} {controls}"), 
                     data = regdata)
       verbose = TRUE
       if (verbose){
         print(summary(did_reg))
       }
       # clustered standard errors (hc1 equiv to ,robust in stata i think? double check this) 
       ##! Yes, I believe vcov hetero = same as HC1 = same as robust SE in Stata.
       vcov = vcovCL(did_reg, type = "HC1")
       # return table of estimates as output
       if (table){
         list(did_reg, vcov)
       }
       else {
         # make dataframe for output -- y is coef, var is estimated variance using clustered standard errors
         effects <- data.frame(y      = c(sapply(interact_vars, function (.x) did_reg$coefficients[[.x]]), 0),
                               depvar = depvar,
                               year   = c(years, yearomit),
                               var    = c(sapply(interact_vars, function(.x) as.numeric(diag(vcov)[[.x]])), 0)) %>%
           mutate(y_ub = y + 1.96*sqrt(var),
                  y_lb = y - 1.96*sqrt(var))
       }
     } # end ifelse
    ## end deconstructing did_graph_data -- back to did_graph
    did_data_temp <- effects
    #______________________________________________________
      
    ## FIX FOR SEPTREAT == TRUE!!! ##! ? 
    did_data <- did_data_temp %>%
      mutate(group      = depvarnames[[1]], 
             year_graph = did_data_temp$year)

    # if more than 1 variable
    if (septreat){
      print("Error: can only use septreat for single variable")
      return(NA)
    }
    did_datasets <- list()
    for (i in seq(1,nvars)){
      did_data_temp <- did_graph_data(dataset, depvarlist[[i]], controls, years, yearomit, verbose) %>%
        mutate(group = depvarnames[[i]],
               year_graph = year - pointspan/2 + (i-1)*(pointspan/(nvars - 1))) # shifting over so dots don't overlap
      did_datasets[[i]] <- did_data_temp
    }
    did_data <- bind_rows(did_datasets)
  
  if(verbose){
    print(did_data)
  }
  
# make graphs
septreat=TRUE
#if (septreat){ # if only one dep var
  graph_out <- ggplot(did_data, aes(x = year_graph, 
                                    y = y, 
                                    color = treat, 
                                    shape = treat)) + 
    geom_hline(yintercept = 0, color = "black", alpha = 0.5) +
    geom_errorbar(aes(min = y_lb, max = y_ub, width = 0, linewidth = 0.5, alpha = 0.05)) +
    annotate("rect", xmin = 1933, xmax = 1938, ymin = -Inf, ymax = Inf, alpha = 0.2) +
    geom_point(size = 4) + labs(x = "Year", y = yvar, color = "", shape = "") + theme_minimal() + 
    theme(legend.position = "bottom") + guides(linewidth = "none", alpha = "none")
  
  #return(graph_out)

  septreat=FALSE
  # if more than one dep var
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
                                             y = min(did_data$y_lb) + (max(did_data$y_ub) - min(did_data$y_lb))/10, 
                                             label = "Marriage Bars \n Removed"), color = "#656565") 
    }
  }
  else{ # no ymin/ymax bound was specified
    graph_out <- graph_out + geom_text(aes(x = 1935.5, 
                                           y = min(did_data$y_lb) + (max(did_data$y_ub) - min(did_data$y_lb))/10, 
                                           label = "Marriage Bars \n Removed"), color = "#656565") 
  }
  # save graphs
  if (!is.na(filename) & !slides){ #saving graph in folder for paper figs
    ggsave(glue("{outfigs}/paper/{filename}.png"), graph_out, width = 8, height = 5)
  }
  if (!is.na(filename) & slides){ #changing text size for slides and saving in folder for slide figs
    ggsave(glue("{outfigs}/slides/{filename}.png"), graph_out + 
             theme(text = element_text(size = 18), 
                   axis.text = element_text(size = 14)), width = 8, height = 5)
  }
  return(graph_out)
  }

# simplify linked analysis graphs ----
con <- dbConnect(duckdb(), dbdir = "C:\\Users\\ctsao\\Documents\\test_duckdb/db.duckdb", read_only=TRUE) 

# Creating 'linked view' -- NOTE: NOT A TABLE/DATAFRAME, 
# just a linking to be filtered/mutated appropriately and collected
linkview <-  tbl(con, "linkedall") %>% 
  addvars_indiv_linked() %>%
  mutate(NCHILD_base = `NCHILD`, 
         NCHILD_link = `NCHILD:1`) %>% #temporary while NCHILD isn't explicitly relabelled in duckdb
  dplyr::select(c(ends_with("_base"),ends_with("_link"))) %>% #only keeping variables that have been selected (see duckdb_init)
  filter(SEX_base == SEX_link & RACE_base == RACE_link &  #only keeping links with consistent sex and race (drops 1.2% of links)
           AGE_base <= AGE_link - 5 & AGE_base >= AGE_link - 15) #and consistent age (age in base year 5-15 years less than age in link year) -- drops an additional 2.2% of links

## for women ----
# group: all married women in t-10
link_mw <- linkview %>% 
  filter(demgroup_base == "MW" & RACE_base == 1 & AGE_base <= 40) %>% 
  summlinks(n = 5) 
# group: all unmarried women teachers in t-10
link_sw <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base >= 8 & AGE_base<=14) %>% 
  summlinks(n = 5)
# group: all unmarried women teachers in t-10
link_swt <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base > 25 & AGE_base <= 30) %>% 
  mutate(TREAT_base           = ifelse(STATEICP_base == 47 | STATEICP_base == 51, 1, 0), # indicator for treated states
         neighbor_samp   = ifelse(STATEICP_base %in% c(47, 51, 40, 48, 54, 56), 1, 0), # indicator for treated or neighbor control
         neighbor_sampNC = ifelse(STATEICP_base %in% c(47, 48, 40, 54), 1, 0), #neighbors for NC: SC, VA, TN
         neighbor_sampKY = ifelse(STATEICP_base %in% c(51, 54, 56, 21, 24, 22), 1, 0) #neighbors for KY: TN, WV, IL, IN, OH)
  ) %>%
  group_by(TREAT_base, YEAR_base, YEAR_link) %>%
  summarize(nlink       = n(),
            pct_t       = sum(ifelse(teacher_link == 1, 1, 0))/n(),
            pct_marr    = sum(ifelse(marst_link == 1, 1, 0))/n(),
            pct_mw      = sum(ifelse(demgroup_link == "MW", 1, 0))/n(), #share of sample (swt_base) that are later mw (teach + nonteach)
            pct_mwt     = sum(ifelse(demgroup_link == "MW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw teach
            pct_mwnt    = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later mw non teach but in lf
            pct_mwnilf  = sum(ifelse(demgroup_link == "MW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later mw and not in lf
            pct_sw      = sum(ifelse(demgroup_link == "SW", 1, 0))/n(), #share of sample (swt_base) that are later sw (teach + nonteach)
            pct_swt     = sum(ifelse(demgroup_link == "SW" & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw teach
            pct_swnt    = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that are later sw non teach but in lf
            pct_swnilf  = sum(ifelse(demgroup_link == "SW" & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that are later sw and not in lf
            pct_wc      = sum(ifelse(NCHILD_link > 0, 1, 0))/n(), #share of sample (swt_base) that later have children (teach + nonteach)
            pct_wct     = sum(ifelse(NCHILD_link > 0 & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and teach
            pct_wcnt    = sum(ifelse(NCHILD_link > 0 & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that later have children and work, but not as teachers
            pct_wcnilf  = sum(ifelse(NCHILD_link > 0 & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that later have children and exit lf
            pct_wnc     = sum(ifelse(NCHILD_link == 0, 1, 0))/n(), #share of sample (swt_base) that later do not have children
            pct_wnct    = sum(ifelse(NCHILD_link == 0 & teacher_link == 1, 1, 0))/n(), #share of sample (swt_base) that later don't have children and teach
            pct_wncnt   = sum(ifelse(NCHILD_link == 0 & teacher_link == 0 & worker_link == 1, 1, 0))/n(), #share of sample (swt_base) that don't have children and work, but not as teachers
            pct_wncnilf = sum(ifelse(NCHILD_link == 0 & teacher_link == 0 & worker_link == 0, 1, 0))/n(), #share of sample (swt_base) that don't have children and exit lf
  ) %>%
  rename(TREAT = TREAT_base, YEAR = YEAR_link) %>%
  collect() 
ggplot(data = link_swt, aes(x = YEAR, y = pct_mw, color = factor(TREAT))) + geom_point()
  
# full version
minage <- 18
maxage <- 40
link_swt <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & 
           AGE_base > minage & AGE_base <= maxage) %>%
  summlinks(n = 5)
linkdatasets <- list(link_sw  %>% filter(neighbor_samp == 1 & mainsamp == 1), # SW 
                     link_mw  %>% filter(neighbor_samp == 1 & mainsamp == 1), # MW 
                     link_swt %>% filter(neighbor_samp == 1 & mainsamp == 1), # SWT
                     link2    %>% filter(neighbor_samp == 1 & mainsamp == 1)  # SWNT
)
yvarlist    <- c("Unmarried W", 
                 "Married W",
                 "Unmarried W Teachers",
                 "Unmarried W Non-Teachers")
yvarlablist <- c("sw", "mw", "swt", "swnt")
linklablist <- c(rep("neighbor",4))
## figures
#for (i in 1:4){
i=3
did_graph(dataset     = linkdatasets[[i]],
          depvarlist  = c("pct_mw"), #, "pct_t", "pct_mwt"),
          depvarnames = c("Married"), #, "Teacher", "Married Teacher"),
          colors      = c(men_col), #, mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
          ymin        = -0.066, 
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          #septreat    = TRUE,
          filename    = glue("test_linked_baseage{minage}to{maxage}_mw_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
did_graph(dataset     = linkdatasets[[i]],
          depvarlist  = c("pct_t"), #, "pct_t", "pct_mwt"),
          depvarnames = c("Teacher"), #, "Teacher", "Married Teacher"),
          colors      = c(men_col), #, mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
          ymin        = -0.066, 
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          #septreat    = TRUE,
          filename    = glue("test_linked_baseage{minage}to{maxage}_t_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
did_graph(dataset     = linkdatasets[[i]],
          depvarlist  = c("pct_mwt"), #, "pct_t", "pct_mwt"),
          depvarnames = c("Married Teacher"), #, "Teacher", "Married Teacher"),
          colors      = c(men_col), #, mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
          ymin        = -0.066, 
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          #septreat    = TRUE,
          filename    = glue("test_linked_baseage{minage}to{maxage}_mwt_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
did_graph(dataset     = linkdatasets[[i]],
          depvarlist  = c("pct_nilf"), #, "pct_t", "pct_mwt"),
          depvarnames = c("Not in labor force"), #, "Teacher", "Married Teacher"),
          colors      = c(men_col), #, mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
          ymin        = -0.066, 
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          #septreat    = TRUE,
          filename    = glue("test_linked_baseage{minage}to{maxage}_nilf_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()

## ARCHIVE
# OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & 
# TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
did_graph(dataset     = linkdatasets[[i]],
          depvarlist  = c("pct_mwt"), #, "pct_t", "pct_mwt"),
          depvarnames = c("Married Teacher"), #, "Teacher", "Married Teacher"),
          colors      = c(men_col), #, mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
          ymin        = -0.066, 
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          #septreat    = TRUE,
          filename    = glue("test_linked_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
#}

### NC ----
# group: all unmarried women in t-10
link_sw <- linkview %>% 
  filter(demgroup_base == "SW" & RACE_base == 1 & AGE_base >= 8 & AGE_base <= 14) %>% 
  summlinks(n = 5) 
# group: all single women teachers in 5-10
link_swt <- linkview %>% 
  filter(teacher_base==1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base >= 30 & AGE_base <=35) %>% 
  summlinks(n = 5) 
linkdatasets <- list(link_sw  %>% filter(neighbor_sampNC == 1 & mainsamp == 1), # SW 
                     link_mw  %>% filter(neighbor_sampNC == 1 & mainsamp == 1), # MW 
                     link_swt %>% filter(neighbor_sampNC == 1 & mainsamp == 1), # SWT
                     link2    %>% filter(neighbor_sampNC == 1 & mainsamp == 1)  # SWNT
)
yvarlist    <- c("Unmarried W", 
                 "Married W",
                 "Unmarried W Teachers",
                 "Unmarried W Non-Teachers")
yvarlablist <- c("sw", "mw", "swt", "swnt")
linklablist <- c(rep("neighbor",4))
## figures
i=3
#for (i in 1:4){
  # OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & 
  # TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
  did_graph(dataset     = linkdatasets[[i]],
            depvarlist  = c("pct_mw", "pct_t", "pct_mwt"),
            depvarnames = c("Married", "Teacher", "Married Teacher"),
            colors      = c(men_col, mw_col, "grey"),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
            ymin        = -0.066, 
            ymax        = 0.05,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            #septreat    = TRUE,
            filename    = glue("test_nc_linked_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
#}

### KY ----
# group: all unmarried women in t-10
link_sw <- linkview %>% 
  filter(demgroup_base == "SW" & RACE_base == 1 & AGE_base >= 7 & AGE_base <= 10) %>% 
  summlinks(n = 5) 
# group: all single women teachers in 5-10
link_swt <- linkview %>% 
    filter(teacher_base==1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 25) %>% 
    summlinks(n = 5) 
linkdatasets <- list(link_sw  %>% filter(neighbor_sampKY == 1 & mainsamp == 1), # SW 
                     link_mw  %>% filter(neighbor_sampKY == 1 & mainsamp == 1), # MW 
                     link_swt %>% filter(neighbor_sampKY == 1 & mainsamp == 1), # SWT
                     link2    %>% filter(neighbor_sampKY == 1 & mainsamp == 1)  # SWNT
)
yvarlist    <- c("Unmarried W", 
                 "Married W",
                 "Unmarried W Teachers",
                 "Unmarried W Non-Teachers")
yvarlablist <- c("sw", "mw", "swt", "swnt")
linklablist <- c(rep("neighbor",4))
## figures
#for (i in 1:4){
i=3
  # OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & 
  # TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
  did_graph(dataset     = linkdatasets[[i]],
            depvarlist  = c("pct_mw", "pct_t", "pct_mwt"),
            depvarnames = c("Married", "Teacher", "Married Teacher"),
            colors      = c(men_col, mw_col, "grey"),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
            ymin        = -0.066, 
            ymax        = 0.05,
            #septreat    = TRUE,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("test_ky_linked_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
#}

## for men ----
# group: all men in t-10
link_m <- linkview %>% 
  filter(demgroup_base == "M" & RACE_base == 1 & AGE_base >= 10 & AGE_base <= 40) %>% 
  summlinks(n = 10) %>% 
  filter(neighbor_samp == 1 & mainsamp == 1) 
yvarlist    <- c("Men")
yvarlablist <- c("m")
linklablist <- c(rep("neighbor",1))

## figures
for (i in 1:1){
  # OUTCOME: SHARE OF UNMARRIED/MARRIED WOMEN (NON-)TEACHERS WHO ARE MARRIED & 
  # TEACHING/WORKING NOT IN TEACHING/NOT IN LF 10 YEARS LATER
  did_graph(dataset     = link_m,
            depvarlist  = c("pct_marr", "pct_t"),
            depvarnames = c("Married", "Teacher"),
            colors      = c(men_col, mw_col),
            years       = c(1920, 1940),
            yvar        = glue("DiD Estimate: Share of {yvarlist[[i]]} in t-10"),
            ymin        = -0.066, 
            ymax        = 0.05,
            verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
            filename    = glue("test_linked_{yvarlablist[[i]]}_{linklablist[[i]]}")) %>% print()
  Sys.sleep(2) #pause so i can see the graph output
  
}

# simplified linking results
# merge on controls/weights
link1 <- linkview %>% 
  filter(teacher_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) %>% 
  summlinks(n = 5)
link_controls <- left_join(x=link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                           y=neighbor %>% 
                             select(c("YEAR","STATEICP","COUNTYICP","URBAN","LFP","NWHITETEACH","POP")), 
                           by=c("YEAR","STATEICP","COUNTYICP"))
# SAMPLE: SWT IN BASE YEAR; OUTCOME: MARRIED BY LINK YEAR
did_graph(dataset     = link_controls,
          depvarlist  = c("pct_mw"),
          depvarnames = c("Pr(Married by t)"),
          colors      = c(men_col),
          years       = c(1920, 1940),
          yvar        = glue("Estimated coefficient"),
          ymin        = -0.066,
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_{yvarlablist[[1]]}_{linklablist[[1]]}_mw"))
# SAMPLE: SWT IN BASE YEAR; OUTCOME: (MARRIED BY LINK YEAR ) X (OCC IN LINK YEAR)
did_graph(dataset     = link_controls,
          depvarlist  = c("pct_mwt", "pct_mwnt", "pct_mwnilf"),
          depvarnames = c("Pr(Married and teaching in t)", "Pr(Married and working (not teaching) in t)", "Pr(Married and not in labor force in t)"),
          colors      = c(men_col, mw_col, "grey"),
          years       = c(1920, 1940),
          yvar        = glue("Estimated coefficient"),
          ymin        = -0.066,
          ymax        = 0.05,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_{yvarlablist[[1]]}_{linklablist[[1]]}_mw_heterog"))
  
 