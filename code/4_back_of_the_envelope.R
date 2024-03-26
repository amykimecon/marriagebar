# Back-of-the-envelope calculation:
# How much did the removal of marriage-related institutional barriers
# contribute to the increase in MW's LFP? 

# checking what share of people in clerical occupations 
# were married women
occs <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% filter(YEAR == 1930 | YEAR == 1950) %>%
  mutate(NWHITEMW = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
  group_by(OCC1950, YEAR) %>%
  summarize(n_occ = n(),
            NWHITEMW = mean(NWHITEMW),
         n_occ_mw = sum(ifelse(demgroup == "MW" & AGE >= 18 & AGE <= 64, 1, 0)),
         n_occ_wmw = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0))) %>%
  collect()

x <- occs %>% mutate(share_occ_mw = n_occ_mw/n_occ,
                share_wmw_occ = n_occ_wmw/NWHITEMW,
                mb_occ = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400), 1, 0),
                mb_occ_specific = ifelse(OCC1950 %in% c(301, 302, 305, 350, 390, 730, 731, 93), 1, 0))

x %>% group_by(mb_occ_specific, YEAR) %>% summarize(s = sum(share_wmw_occ))