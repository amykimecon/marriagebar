# Back-of-the-envelope calculations
# (How much did the removal of marriage-related institutional barriers
# contribute to the increase in MW's LFP?)
# 2024/03
  
# open log ----
sink("./logs/log_4_back_of_the_envelope.log", append=FALSE)
print("***************** RUNNING: 3_back_of_the_envelope *****************\n\n")


# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)

# neighbor data
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)

#____________________________________________________________
# MAIN EFFECT ----
#_____________________________________________________________
## est DiD coef on Pr(teacher | MW) ----

# getting effect on probability of being a teacher | MW
mainreg     <- did_graph_data(neighbor %>% mutate(weight = NWHITEMW), depvar = "pct_Teacher_mw_1000")
mainreg_unw <- did_graph_data(neighbor,                               depvar = "pct_Teacher_mw")

# est. coef weighted: 0.0009767 !! Where does this come from? 
# est. coef unweighted: 0.0012195

# baseline share of wmw teachers
baseline_mean <- weighted.mean(filter(neighbor, TREAT == 1 & YEAR == 1930)$pct_Teacher_mw_1000, 
                               filter(neighbor, TREAT == 1 & YEAR == 1930)$NWHITEMW)

# est. effect (%)
did_est_pp <- mainreg$y[mainreg$year == 1940]
did_est <- did_est_pp/baseline_mean
print(did_est)

# initializing base and end years
base_yr = 1940
end_yr  = 1950


## identify MB occs ----

# get shares of each occ that were MW, coll, in LF
# NOTE: OCCs includes 999, which is not employed
occs <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter(YEAR == base_yr | YEAR == end_yr) %>%
  group_by(OCC1950, YEAR) %>%
  summarize(n_occ = sum(ifelse(LABFORCE == 2, 1, 0)),
            n_occ_wmw      = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), #num white MW in each occ
            n_occ_wmw_educ = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & EDUC != 0 & EDUC != 99, 1, 0)), # num white MW with nonmissing educ
            n_emp_wmw      = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & LABFORCE == 2, 1, 0)), #num of white MW in labor force in each occ
            n_occ_wmw_coll = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11, 1, 0)), # num white MW w some college in each occ
            n_emp_wmw_coll = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11 & LABFORCE == 2, 1, 0)), #num of white MW w some college in LF in each occ
            ) %>%
  ungroup() %>%
  group_by(YEAR) %>% 
  mutate(NWHITEMW         = sum(n_occ_wmw), # total num white MW
         NWHITEMW_coll    = sum(n_occ_wmw_coll), # total num white MW w some college
         NWHITEMW_LF      = sum(n_emp_wmw),
         NWHITEMW_LF_coll = sum(n_emp_wmw_coll)
         ) %>%
  ungroup() %>% 
  collect() %>% #grouping into specific occupation groups
  mutate(share_occ_wmw_coll = n_occ_wmw_coll/n_occ_wmw_educ,
         share_wmw_occ      = n_occ_wmw/NWHITEMW,
         share_wmw_coll_occ = n_occ_wmw_coll/NWHITEMW_coll,
         mb_occ_gen         = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400) | OCC1950 %in% c(730, 731, 56, 58), 1, 0),
         mb_occ             = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400), 1, 0),
         mb_occ_specific    = ifelse(OCC1950 %in% c(301, 302, 305, 350, 93), 1, 0),
         whitecollar_occ    = ifelse(OCC1950 < 500 & !(OCC1950 %in% c(100,123)), 1, 0))

## what share of the LF was employed in white-collar jobs?
tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(YEAR >= 1940 & worker == 1) %>%
  mutate(whitecollar_occ    = ifelse(OCC1950 < 500 & !(OCC1950 %in% c(100,123)), 1, 0)) %>%
  group_by(whitecollar_occ, YEAR) %>% 
  summarize(lf = n()) %>% 
  group_by(YEAR) %>% 
  mutate(share_lf = lf/sum(lf))

## what share of the female LF was employed in teaching?
tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(YEAR >= 1940 & worker == 1 & demgroup != "M") %>%
  group_by(teacher, YEAR) %>% 
  summarize(lf = n()) %>% 
  group_by(YEAR) %>% 
  mutate(share_lf = lf/sum(lf))

## what share of teachers had college?
occs %>% filter(OCC1950 == 93 & YEAR == 1940) %>% dplyr::select(share_occ_wmw_coll)

## what share of all wmw had college?
occs %>% group_by(YEAR) %>% summarize(share_coll = sum(n_occ_wmw_coll)/sum(n_occ_wmw_educ))

tbl(con, "censusrawall") %>% 
  addvars_indiv() %>%
  filter(RACE == 1 & AGE >= 18 & AGE <= 64 & YEAR >= 1940) %>%
  group_by(YEAR, demgroup) %>%
  summarize(share_coll = sum(ifelse(EDUC %in% c(7, 8, 9, 10, 11), 1, 0))/sum(ifelse(EDUC != 0 & EDUC != 99, 1, 0))) %>% collect() %>% View()

## BoTE ----
# back of the envelope: how much did introducing employment protections/removing
# MBs contribute to the rise in (white) married women's LFP in white collar jobs? 

### Numerators ----
## defn 1: includes only WMW growth in likely MB-specific occupations
mb_spec <- occs %>% 
  group_by(mb_occ_specific, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) #total share of women in mb specific occupations in each census year

num_mb_occ_specific   <- mb_spec$s[mb_spec$mb_occ_specific==1 & mb_spec$YEAR==base_yr]*did_est
print(glue("BoTE Numerator 1: MB-induced rise in WMW's LFP in MB-specific occupations: {round(num_mb_occ_specific*100,4)}%"))

## defn 2: includes WMW growth in all clerical work + adjacent occupations
mb_gen <- occs %>% 
  group_by(mb_occ_gen, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) 

num_mb_occ_gen <- mb_gen$s[mb_gen$mb_occ_gen==1 & mb_gen$YEAR==base_yr]*did_est
print(glue("BoTE Numerator 2: MB-induced rise in WMW's LFP in clerical occupations: {round(num_mb_occ_gen*100,4)}%"))

## defn 3: includes only WMW growth in likely MB occupations - MAIN NUMERATOR
mb <- occs %>% 
  group_by(mb_occ, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) #total share of women in mb specific occupations in each census year

num_mb_occ   <- mb$s[mb$mb_occ==1 & mb$YEAR==base_yr]*did_est
print(glue("BoTE Numerator 3: MB-induced rise in WMW's LFP in MB occupations: {round(num_mb_occ*100,4)}%"))

## defn 4: includes WMW with college growth in all likely MB occupations
mb_educ <- occs %>% 
  group_by(mb_occ, YEAR) %>%
  summarize(s = sum(share_wmw_coll_occ))

num_mb_occ_educ <-  mb_educ$s[mb_educ$mb_occ==1 & mb_educ$YEAR==base_yr]*did_est
print(glue("BoTE Numerator 4: MB-induced rise in college educated WMW's LFP in clerical occupations: {round(num_mb_occ_educ*100,4)}%"))


### Denominators ----
## defn 1: WMW growth in MB specific occupations
denom_mb_occ_specific <- mb_spec$s[mb_spec$mb_occ_specific==1 & mb_spec$YEAR==end_yr] - 
                         mb_spec$s[mb_spec$mb_occ_specific==1 & mb_spec$YEAR==base_yr] 
print(glue("BoTE Denominator 1: Total Actual Rise in WMW's LFP in MB-specific occupations: {round(denom_mb_occ_specific*100,4)}%"))

## defn 2: WMW growth in all clerical occupations
denom_mb_occ <- mb$s[mb$mb_occ==1 & mb$YEAR==end_yr] - 
                    mb$s[mb$mb_occ==1 & mb$YEAR==base_yr] 
print(glue("BoTE Denominator 2: Total Actual Rise in WMW's LFP in clerical occupations: {round(denom_mb_occ_gen*100,4)}%"))

## defn 3: WMW growth in all white-collar occupations
whitecollar <- occs %>% 
  group_by(whitecollar_occ, YEAR) %>% 
  summarize(s = sum(share_wmw_occ))

denom_whitecollar <- whitecollar$s[whitecollar$whitecollar_occ==1 & whitecollar$YEAR==end_yr] - 
                     whitecollar$s[whitecollar$whitecollar_occ==1 & whitecollar$YEAR==base_yr] 
print(glue("BoTE Denominator 3: Total Actual Rise in WMW's LFP in all white-collar occupations: {round(denom_whitecollar*100,4)}%"))

## defn 4: college educ WMW growth in LFP
coll_lfp <- occs %>% 
  group_by(YEAR) %>% 
  summarize(lfp = sum(n_emp_wmw_coll)/sum(n_occ_wmw_coll))
denom_coll <- coll_lfp$lfp[coll_lfp$YEAR == end_yr] - coll_lfp$lfp[coll_lfp$YEAR == base_yr]
print(glue("BoTE Denominator 4: Total Actual Rise in all college-educated WMW's LFP: {round(denom_coll*100,4)}%"))

### BoTE Calcs ----
print(glue("BoTE 1: MB share of rise in white collar jobs: {round(100*num_mb_occ_gen/denom_whitecollar,4)}%"))
print(glue("BoTE 2: MB share of rise in college-educated LFP: {round(100*num_mb_occ_educ/denom_coll,4)}%"))

#____________________________________________________________
# MECHANISM MAGNITUDES (SHARE SWT -> MWT vs SHARE MWNILF -> MWT) ----
#____________________________________________________________

## share SWT -> MWT ----
## SWT (unmarried women teachers under age 40, white)

# get list of main sample counties (i.e. run 0_dataclean up to matching)
# TEMP: USE countysumm normally
y <- countysumm_gen %>% filter(FIPS %in% mainsamp_list)

# get counts of SWT (county-yr means and yr-treat totals) for...
# ...linked sample
y_swt_link <- link1 %>% 
  filter(mainsamp == 1) %>% 
  group_by(YEAR, TREAT) %>% 
  summarize(tot_nlink = sum(nlink),
            across(c(nlink, pct_mw, pct_mwt, pct_mwnilf, pct_swt, pct_swnilf), mean))
y_swt_link 
count_swt_link_cty <- y_swt_link$nlink[y_swt_link$YEAR==1930 & y_swt_link$TREAT==1]
count_swt_link_tot <- y_swt_link$tot_nlink[y_swt_link$YEAR==1930 & y_swt_link$TREAT==1]

# ...full sample 
y_swt_full <- y %>% 
  group_by(YEAR, TREAT) %>% 
  summarize(nswt     = mean(N_SWT), 
            tot_nswt = sum(N_SWT))
y_swt_full 
count_swt_full_cty <- y_swt_full$nswt[y_swt_full$YEAR==1930 & y_swt_full$TREAT==1]
count_swt_full_tot <- y_swt_full$tot_nswt[y_swt_full$YEAR==1930 & y_swt_full$TREAT==1]

# get DiD coefficient: effect of lifting MB on Pr(MWT in 10 yrs) for SWT
coef <- did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), depvar = "pct_mwt", years = c(1920, 1940))
eff_swt = coef$y[2] # 0.019636 

# Apply DiD estimate to base counts of SWT in 1930
print(" Est. nr. of SWT who -> MWT in treated counties, using linked sample counts: ")
print(paste0(".. per county: ", eff_swt*count_swt_link_cty))    
print(paste0(".. in total: ",   eff_swt*count_swt_link_tot))   

print("Est. nr. of SWT who -> MWT in treated counties, using full sample counts: ")
print(paste0(".. per county: ", eff_swt*count_swt_full_cty))    
print(paste0(".. in total: ",   eff_swt*count_swt_full_tot))   



## share MWNILF -> MWT ----
# MWNILF (married women non teachers under age 50, white)

# get counts of MWNILF (county-yr means and yr-treat totals) for...
# ...linked sample
link3_mwnilf <- read_csv(glue("{cleandata}/link3_mwnilf.csv"))
y_mwnilf_link <- link3_mwnilf %>% 
  filter(mainsamp == 1) %>% 
  group_by(YEAR, TREAT) %>% 
  summarize(tot_nlink = sum(nlink),
            across(c(nlink, pct_mw, pct_mwt, pct_mwnt, pct_mwnilf), mean))
y_mwnilf_link
count_mwnilf_link_cty <- y_mwnilf_link$nlink[y_mwnilf_link$YEAR==1930 & y_mwnilf_link$TREAT==1]
count_mwnilf_link_tot <- y_mwnilf_link$tot_nlink[y_mwnilf_link$YEAR==1930 & y_mwnilf_link$TREAT==1]

# ...full sample
y_mwnilf_full <- y %>% 
  group_by(YEAR, TREAT) %>% 
  summarize(nmwnilf     = mean(N_MWNILF), 
            tot_nmwnilf = sum(N_MWNILF))
y_mwnilf_full 
count_mwnilf_full_cty <- y_mwnilf_full$nmwnilf[y_mwnilf_full$YEAR==1930 & y_mwnilf_full$TREAT==1]
count_mwnilf_full_tot <- y_mwnilf_full$tot_nmwnilf[y_mwnilf_full$YEAR==1930 & y_mwnilf_full$TREAT==1]

# get DiD coef
coef <- did_graph_data(link3_mwnilf %>% filter(neighbor_samp == 1 & mainsamp == 1), depvar = "pct_mwt", years = c(1920, 1940))
eff_mwnilf <- coef$y[2]

# apply DiD estimate to base counts of MWNT in 1930
print(" Est. nr. of MWNILF who -> MWT in treated counties, using linked sample counts: ")
print(paste0(".. per county: ", eff_mwnilf*count_mwnilf_link_cty))     # 2018
print(paste0(".. in total: ",   eff_mwnilf*count_mwnilf_link_tot))     # 437888

print("Est. nr. of MWNILF who -> MWT in treated counties, using full sample counts: ")
print(paste0(".. per county: ", eff_mwnilf*count_mwnilf_full_cty))    # 3188
print(paste0(".. in total: ",   eff_mwnilf*count_mwnilf_full_tot))    # 685361



#____________________________________________________________
# MISC REFERENCE NUMBERS ----
#____________________________________________________________
# linking rates
7612/19368    #swt treated
208369/361220 #swt non treated

468142/751501     #mwnt treated
10719571/19525320 #mwnt non treated

# total counts of teachers
countysumm %>% filter(mainsamp == 1) %>%
  group_by(YEAR, TREAT) %>% 
  summarize(num = sum(num_Teacher))

## TESTING IF REG + POP WEIGHTS IS EQUIV TO INDIV REGRESSION
# random sample of treat/ctrl counties
samp <- neighbor %>% 
  mutate(ICP = glue("{STATEICP}_{COUNTYICP}"))
mw_samp <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  mutate(ICP = paste0(STATEICP, "_", COUNTYICP)) %>%
  filter(demgroup == "MW" & ICP %in% 
           c("40_1630", "40_350", "54_1370", "54_1830", "54_650", "54_1490", "40_1590",
             "54_550", "48_670", "40_1150", "51_2150", "47_1770", "47_1990", "47_1710", 
             "47_30", "51_1270", "51_1010", "51_730", "51_50", "51_1690")) %>%
  collect() 

icp_samp <- c("40_1630", "40_350", "54_1370", "54_1830", "54_650", "54_1490", "40_1590",
              "54_550", "48_670", "40_1150", "51_2150", "47_1770", "47_1990", "47_1710", 
              "47_30", "51_1270", "51_1010", "51_730", "51_50", "51_1690")
# indiv
did_graph_data(mw_samp %>% addvars_county() %>%
                 filter(RACE == 1 & AGE >= 18 & AGE <= 64),
               depvar = "teacher")

reg1 <- lm(data = mw_samp %>% addvars_county() %>%
             filter(RACE == 1 & AGE >= 18 & AGE <= 64), teacher ~ TREAT*factor(YEAR))
vcov1 = vcovCL(reg1, type = "HC1")

# weighted county
did_graph_data(samp %>% filter(ICP %in% icp_samp) %>% mutate(weight = NWHITEMW),
               depvar = "pct_Teacher_mw")
reg2 <- lm(data = samp %>% filter(ICP %in% icp_samp), pct_Teacher_mw ~ TREAT*factor(YEAR), weights = NWHITEMW)
vcov2 = vcovCL(reg2, type = "HC1")

sink()
