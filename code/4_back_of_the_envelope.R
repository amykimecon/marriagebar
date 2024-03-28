# Back-of-the-envelope calculation:
# How much did the removal of marriage-related institutional barriers
# contribute to the increase in MW's LFP? 

# checking what share of people in clerical occupations 
# were married women
occs <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% filter(YEAR == 1930 | YEAR == 1950) %>%
  mutate(NWHITEMW = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
  group_by(OCC1950, YEAR) %>%
  summarize(n_occ = sum(ifelse(AGE >= 18 & AGE <= 64, 1, 0)),
            NWHITEMW = mean(NWHITEMW),
         n_occ_mw = sum(ifelse(demgroup == "MW" & AGE >= 18 & AGE <= 64, 1, 0)),
         n_occ_sw = sum(ifelse(demgroup == "SW" & AGE >= 18 & AGE <= 64, 1, 0)),
         n_occ_wmw = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
  collect()

occ_crosswalk <- read_csv(glue("{root}/occ1950_codes_raw.csv")) %>%
  fill(name1, .direction = "down") %>%
  filter(!is.na(code)) %>%
  mutate(name = ifelse(is.na(name2), name1, glue("{name1}; {name2}")),
         code = as.numeric(code)) %>%
  select(c(code, name))

x <- occs %>% mutate(share_occ_mw = n_occ_mw/n_occ,
                     share_occ_sw = n_occ_sw/n_occ,
                share_wmw_occ = n_occ_wmw/NWHITEMW,
                mb_occ = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400), 1, 0),
                mb_occ_specific = ifelse(OCC1950 %in% c(301, 302, 305, 350, 390, 730, 731, 93), 1, 0)) %>%
  left_join(occ_crosswalk, by = c("OCC1950"="code"))

x %>% group_by(mb_occ_specific, YEAR) %>% summarize(s = sum(share_wmw_occ))



#### COMPARING MAGNITUDES OF EFFECTS BY GROUP (SHARE SWT -> MWT vs SHARE MWNT -> MWT)
# TEMP: USE countysumm normally
y <- countysumm_gen %>% filter(FIPS %in% mainsamp_list)

# SWT (unmarried women teachers under age 40, white)
link1 %>% filter(mainsamp == 1) %>% group_by(YEAR, TREAT) %>% 
  summarize(tot_nlink = sum(nlink),
            across(c(nlink, pct_mw, pct_mwt, pct_mwnilf, pct_swt, pct_swnilf), mean))

y %>% group_by(YEAR, TREAT) %>% 
  summarize(nswt = mean(N_SWT), tot_nswt = sum(N_SWT))

did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), depvar = "pct_mwt", years = c(1920, 1940))

eff_swt = 0.019636 
eff_swt*36 #linked avg 
eff_swt*7612 #linked tot
eff_swt*90 #full count avg
eff_swt*19368 #full count tot

# MWNT (married women non teachers under age 50, white)
link3 %>% filter(mainsamp == 1) %>% group_by(YEAR, TREAT) %>% 
  summarize(
    tot_nlink = sum(nlink),
    across(c(nlink, pct_mw, pct_mwt, pct_mwnt, pct_mwnilf), mean))

y %>% group_by(YEAR, TREAT) %>% 
  summarize(nmwnt = mean(N_MWNT), tot_nswt = sum(N_MWNT))

countysumm %>% filter(mainsamp == 1) %>% group_by(YEAR, TREAT) %>% summarize(num = sum(num_Teacher))
did_graph_data(link3 %>% filter(neighbor_samp == 1 & mainsamp == 1), depvar = "pct_mwt", years = c(1920, 1940))

eff_mwnt = 0.0006409
eff_mwnt*2157
eff_mwnt*468142
eff_mwnt*3495 #full count avg
eff_mwnt*751501 #full count tot

## linking rates
7612/19368 #swt treated
208369/361220 #swt non treated

468142/751501 #mwnt treated
10719571/19525320 #mwnt non treated
