# Back-of-the-envelope calculations
# (How much did the removal of marriage-related institutional barriers
# contribute to the increase in MW's LFP?)
# 2024/03
  
# open log ----
sink("./logs/log_4_back_of_the_envelope.log", append=FALSE)
print("***************** RUNNING: 4_back_of_the_envelope *****************\n\n")

#____________________________________________________________
# MAIN EFFECT ----
#_____________________________________________________________
## est DiD coef on Pr(teacher | MW) ----

# getting effect on probability of being a teacher | MW
mainreg <- did_graph_data(neighbor %>% mutate(weight = NWHITEMW), depvar = "pct_Teacher_mw_1000")
mainreg_unw <- did_graph_data(neighbor, depvar = "pct_Teacher_mw")

# est. coef weighted: 0.0009767
# est. coef unweighted: 0.0012195

# baseline share of wmw teachers
baseline_mean <- weighted.mean(filter(neighbor, TREAT == 1 & YEAR == 1930)$pct_Teacher_mw_1000, filter(neighbor, YEAR == 1930 & TREAT == 1)$NWHITEMW)

# est. effect (%)
did_est_pp <- mainreg$y[mainreg$year == 1940]
did_est <- did_est_pp/baseline_mean
print(did_est)

# initializing base and end years
base_yr = 1940
end_yr = 1950



## identify MB occs ----

# get shares of each occ that were MW
occs <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter(YEAR == base_yr | YEAR == end_yr) %>%
  group_by(YEAR) %>% 
  mutate(NWHITEMW = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
  ungroup() %>% 
  group_by(OCC1950, YEAR) %>%
  summarize(n_occ     = sum(ifelse(AGE >= 18 & AGE <= 64, 1, 0)),
            NWHITEMW  = mean(NWHITEMW),
            n_occ_mw  = sum(ifelse(demgroup == "MW" & AGE >= 18 & AGE <= 64, 1, 0)),
            n_occ_sw  = sum(ifelse(demgroup == "SW" & AGE >= 18 & AGE <= 64, 1, 0)),
            n_occ_wmw = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
  collect()

# clean crosswalk of occupation names
occ_crosswalk <- read_csv(glue("{root}/occ1950_codes_raw.csv")) %>%
  # name1 is occupation title, name2 is specific type of name1 
  fill(name1, .direction = "down") %>%
  filter(!is.na(code)) %>%
  # concatenate name1 and name2
  mutate(name = ifelse(is.na(name2), name1, glue("{name1}; {name2}")),
         code = as.numeric(code)) %>%
  select(c(code, name))

# focus on white collar employment:
# Teachers are 93
# “Clerical and kindred” category is between 300-390. We select ones that are
# likely more women dominated: 
  # 301 Attendants and assistants, library
  # 302 Attendants, physician’s and dentist’s office
  # 305 Bank tellers
  # 350 Stenographers, typists and secretaries
  # 390 Clerical and kindred workers
# plus some service workers:
  # 730 attendants, hospital or other institution
  # 731 attendants, professional and personal service (nec)
# other maybes:
  # 56 librarians
  # 58 nurses (professional)

# get shares of white MW in occupations likely affected by MBs or not
x <- occs %>% mutate(share_occ_mw    = n_occ_mw/n_occ,
                     share_occ_sw    = n_occ_sw/n_occ,
                     share_wmw_occ   = n_occ_wmw/NWHITEMW,
                     mb_occ          = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400) | OCC1950 %in% c(730, 731, 56, 58), 1, 0),
                     mb_occ_specific = ifelse(OCC1950 %in% c(301, 302, 305, 350, 390, 93), 1, 0)) %>% 
  left_join(occ_crosswalk, by = c("OCC1950"="code"))

# investigate: shares of white MW in occupations that had high shares of single women in base yr
occs_high_share_sw_base <- x %>% 
  filter(YEAR==base_yr) %>% 
  filter(share_occ_sw >= 0.66 & share_occ_mw < 0.20) %>%
  View()


## BoTE ----
# back of the envelope: how much did introducing employment protections/removing
# MBs contribute to the rise in (white) married women's LFP in white collar jobs? 
# 
# # get share of teachers in 1930 who are MW
# x_teach <- occs %>% 
#   filter(OCC1950==93) %>% 
#   mutate(share_occ_mw    = n_occ_mw/n_occ,
#          share_occ_sw    = n_occ_sw/n_occ,
#          share_wmw_occ   = n_occ_wmw/NWHITEMW)
# x_teach

## defn 1: includes only WMW growth in likely MB-specific occupations
x1 <- x %>% 
  group_by(mb_occ_specific, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) 
x1
num                   <- x1$s[x1$mb_occ_specific==1 & x1$YEAR==base_yr]*did_est
denom_mb_occ_specific <- x1$s[x1$mb_occ_specific==1 & x1$YEAR==end_yr] - x1$s[x1$mb_occ_specific==1 & x1$YEAR==base_yr] 
print(paste0("BoTE 1: Contr. of MB to rise in WMW's LFP in white collar work: ", num/denom_mb_occ_specific))

## defn 2: includes WMW growth in all clerical work occupations
x2 <- x %>% 
  group_by(mb_occ, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) 
x2
num                   <- x2$s[x2$mb_occ==1 & x2$YEAR==base_yr]*did_est
denom_mb_occ          <- x2$s[x2$mb_occ==1 & x2$YEAR==end_yr] - x2$s[x2$mb_occ==1 & x2$YEAR==base_yr] 
print(paste0("BoTE 2: Contr. of MB to rise in WMW's LFP in white collar work: ", num/denom_mb_occ))

## defn 3: uses numerator from defn 2 (all clerical work) but denominator is growth in college-educ WMW LFP
allyears_raw_samp <- read_csv(glue("{rawdata}/census_sample_allyears.csv"))
wmw_samp <- allyears_raw_samp %>%
  mutate(demgroup = case_when(SEX == 1 ~ "M", #man
                              SEX == 2 & MARST != 1 & MARST != 2 ~ "SW", #single/divorced/widowed/separated woman
                              TRUE ~ "MW"), #married woman
         worker = ifelse(YEAR == 1900, 
                         ifelse(OCC1950 != 999 & AGE >= 18 & AGE <= 64, 1, 0), #in 1900, no LABFORCE so use those with occupation
                         ifelse(LABFORCE == 2 & AGE >= 18 & AGE <= 64, 1, 0)), #otherwise, those in LABFORCE 
         teacher = ifelse(YEAR == 1900,
                          ifelse(OCC1950 == 93 & worker == 1, 1, 0), #in 1900, no CLASSWKR
                          ifelse(OCC1950 == 93 & CLASSWKR == 2 & worker == 1, 1, 0)),
         coll_above = ifelse(EDUC >= 7, 1, 0)) %>%
  filter(demgroup == "MW" & RACE == 1 & coll_above == 1) %>%
  group_by(YEAR) %>%
  summarize(lfp = sum(ifelse(worker == 1, PERWT, 0))/sum(ifelse(AGE >= 18 & AGE <= 64, PERWT, 0)))
denom_wmw_coll <- wmw_samp$lfp[wmw_samp$YEAR == end_yr] - wmw_samp$lfp[wmw_samp$YEAR == base_yr]
print(paste0("BoTE 3: Contr. of MB to rise in WMW's LFP in white collar work: ", num/denom_wmw_coll))


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
