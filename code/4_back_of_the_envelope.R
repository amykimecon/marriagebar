# Back-of-the-envelope calculations
# (How much did the removal of marriage-related institutional barriers
# contribute to the increase in MW's LFP?)
# 2024/03


#____________________________________________________________
# MAIN EFFECT ----
#_____________________________________________________________
# Also checks: which white collar jobs employed married women?

# get shares of each occ that were MW
occs <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter(YEAR == 1930 | YEAR == 1950) %>%
  mutate(NWHITEMW = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
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

# get shares of white MW in occupations likely affected by MBs or not
x <- occs %>% mutate(share_occ_mw    = n_occ_mw/n_occ,
                     share_occ_sw    = n_occ_sw/n_occ,
                     share_wmw_occ   = n_occ_wmw/NWHITEMW,
                     mb_occ          = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400), 1, 0),
                     mb_occ_specific = ifelse(OCC1950 %in% c(301, 302, 305, 350, 390, 730, 731, 93), 1, 0)) %>%
  left_join(occ_crosswalk, by = c("OCC1950"="code"))
# see head: waitresses !!

# back of the envelope: how much did introducing employment protections/removing
# MBs contribute to the rise in (white) married women's LFP in white collar jobs? 

## upper bound: denominator includes only WMW growth in likely MB-specific occupations
x %>% group_by(mb_occ_specific, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) 
num                   <- 1.7*0.2        # from DiD estimate
denom_mb_occ_specific <- 0.0286-0.00713 # from summarize above
print(paste0("UB BoTE: Contr. of MB to rise in WMW's LFP in white collar work: ", num/denom_mb_occ_specific))

## lower bound: denominator includes WMW growth in all clerical work occupations
x %>% group_by(mb_occ, YEAR) %>% 
  summarize(s = sum(share_wmw_occ)) 
num                   <- 1.7*0.2       # from DiD estimate
denom_mb_occ          <- 0.0391-0.0100 # from summarize above
print(paste0("LB BoTE: Contr. of MB to rise in WMW's LFP in white collar work: ", num/denom_mb_occ))



#____________________________________________________________
# MECHANISM MAGNITUDES (SHARE SWT -> MWT vs SHARE MWNT -> MWT) ----
#____________________________________________________________

## share SWT -> MWT ----
## SWT (unmarried women teachers under age 40, white)

# get list of main sample counties (i.e. run 0_dataclean up to matching)
# TEMP: USE countysumm normally
y <- countysumm_gen %>% filter(FIPS %in% mainsamp_list)

# get counts of SWT (county-yr means and yr-treat totals) for...
# ...linked sample
link1 %>% filter(mainsamp == 1) %>% 
  group_by(YEAR, TREAT) %>% 
  summarize(tot_nlink = sum(nlink),
            across(c(nlink, pct_mw, pct_mwt, pct_mwnilf, pct_swt, pct_swnilf), mean))
# ...full sample 
y %>% group_by(YEAR, TREAT) %>% 
  summarize(nswt     = mean(N_SWT), 
            tot_nswt = sum(N_SWT))

# get DiD coefficient: effect of lifting MB on Pr(MWT in 10 yrs) for SWT
coef <- did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), depvar = "pct_mwt", years = c(1920, 1940))
eff_swt = coef$y[2] # 0.019636 

# BoTE: apply DiD estimate to base counts of SWT in 1930
print("BoTE: Est. nr. of SWT who -> MWT in treated counties, using linked sample counts: ")
print(paste0(".. per county: ", eff_swt*36))    
print(paste0(".. in total: ",   eff_swt*7612))   

print("BoTE: Est. nr. of SWT who -> MWT in treated counties, using full sample counts: ")
print(paste0(".. per county: ", eff_swt*90))    
print(paste0(".. in total: ",   eff_swt*19368))   



## share MWNT -> MWT ----
# MWNT (married women non teachers under age 50, white)

# get counts of MWNT (county-yr means and yr-treat totals) for...
# ...linked sample
link3 %>% filter(mainsamp == 1) %>% 
  group_by(YEAR, TREAT) %>% 
  summarize(tot_nlink = sum(nlink),
            across(c(nlink, pct_mw, pct_mwt, pct_mwnt, pct_mwnilf), mean))
# ...full sample
y %>% group_by(YEAR, TREAT) %>% 
  summarize(nmwnt    = mean(N_MWNT), 
            tot_nswt = sum(N_MWNT))

# get DiD coef
coef <- did_graph_data(link3 %>% filter(neighbor_samp == 1 & mainsamp == 1), depvar = "pct_mwt", years = c(1920, 1940))
eff_mwnt <- coef$y[2]

# BoTE: apply DiD estimate to base counts of MWNT in 1930
print("BoTE: Est. nr. of MWNT who -> MWT in treated counties, using linked sample counts: ")
print(paste0(".. per county: ", eff_mwnt*2157))    
print(paste0(".. in total: ",   eff_mwnt*468142))   

print("BoTE: Est. nr. of MWNT who -> MWT in treated counties, using full sample counts: ")
print(paste0(".. per county: ", eff_mwnt*3495))    
print(paste0(".. in total: ",   eff_mwnt*751501))   



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
