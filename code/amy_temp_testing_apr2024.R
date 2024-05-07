# temp testing
neighbor   <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1)

did_graph_data(neighbor, "LFP_WMW")
did_graph_data(neighbor, "LFP_WSW")


did_graph(neighbor %>% mutate(weight = NWHITEMW), "LFP_WMW", depvarnames = c("MW"), colors = c(mw_col))

did_graph(neighbor %>% filter(YEAR %in% c(1930, 1940)) %>% mutate(weight = NWHITEMW), "LFP_WMW", depvarnames = c("MW"), colors = c(mw_col), years = c(1940))


did_graph(neighbor, "LFP_WSW", depvarnames = c("SW"), colors = c(sw_col))

did_graph(neighbor %>% mutate(weight = NWHITEMW), "pct_Teacher_mw_1000", depvarnames = c("MW"), colors = c(mw_col))
did_graph(neighbor %>% mutate(weight = NWHITESW), "pct_Teacher_sw_1000", depvarnames = c("MW"), colors = c(mw_col))

did_graph(neighbor %>% mutate(weight = NWHITESW_YOUNG), "pct_Teacher_sw_young_1000", depvarnames = c("MW"), colors = c(mw_col))

did_graph(neighbor, "NWHITESW_YOUNG", depvarnames = c("MW"), colors = c(mw_col))

did_graph(neighbor, "pct_Teacher_sw_mid_1000", depvarnames = c("MW"), colors = c(mw_col))


did_graph(neighbor, "num_sw_Teacher", depvarnames = c("MW"), colors = c(mw_col))

neighbor_link <- link2 %>% filter(neighbor_samp == 1 & mainsamp == 1)
did_graph(neighbor_link, "pct_t", depvarnames = c("MW"), colors = c(mw_col), years = c(1920, 1940))
did_graph(neighbor_link, "pct_mw", depvarnames = c("MW"), colors = c(mw_col), years = c(1920, 1940))

did_graph(neighbor_link, "pct_swt", depvarnames = c("MW"), colors = c(mw_col), years = c(1920, 1940))

did_graph(neighbor_link, "pct_swnt", depvarnames = c("MW"), colors = c(mw_col), years = c(1920, 1940))
did_graph(neighbor_link, "pct_swnilf", depvarnames = c("MW"), colors = c(mw_col), years = c(1920, 1940))


## Testing: remaking swt linked graphs 
neighbor_link1 <- link1 %>% filter(neighbor_samp == 1 & mainsamp == 1)
did_graph(neighbor_link1 %>% mutate(weight = nlink), c("pct_nilf", "pct_sw_cond_nilf", "pct_mw_cond_nilf"), depvarnames = c("nilf", "sw", "mw"), colors = gg_color_hue(3),
          years = c(1920,1940))

did_graph(neighbor_link1 %>% mutate(weight = nlink), c("pct_lf", "pct_sw_cond_inlf", "pct_mw_cond_inlf"), depvarnames = c("nilf", "sw", "mw"), colors = gg_color_hue(3),
          years = c(1920,1940))

did_graph(neighbor_link1, c("avg_occscore", "avg_occscore_sw", "avg_occscore_mw"), depvarnames = c("occscore", "sw", "swnt"), colors = c("grey", mw_col, sw_col),
          years = c(1920,1940))

did_graph(neighbor_link1, c("avg_occscore", "avg_occscore_sw", "avg_occscore_swnt"), depvarnames = c("occscore", "sw", "swnt"), colors = c("grey", mw_col, sw_col),
          years = c(1920,1940))

did_graph(neighbor_link1, c("pct_swt","pct_swnt", "pct_swnilf","pct_mwt","pct_mwnt","pct_mwnilf"), 
          depvarnames = c("swt", "swnt", "swnilf","mwt", "mwnt", "mwnilf"), colors = gg_color_hue(6), years = c(1920, 1940))

did_graph(neighbor_link1 %>% mutate(weight = nlink), c("pct_swt_cond","pct_swnt_cond", "pct_swnilf_cond","pct_mwt_cond","pct_mwnt_cond","pct_mwnilf_cond"), 
          depvarnames = c("swt", "swnt", "swnilf","mwt", "mwnt", "mwnilf"), colors = gg_color_hue(6), years = c(1920, 1940))

neighbor_link4 <- link4 %>% filter(neighbor_samp == 1 & mainsamp == 1)

did_graph(neighbor_link5, c("pct_swt","pct_swnt", "pct_swnilf","pct_mwt","pct_mwnt","pct_mwnilf"), 
          depvarnames = c("swt", "swnt", "swnilf","mwt", "mwnt", "mwnilf"), colors = gg_color_hue(6), years = c(1920, 1940))

# did_graph(neighbor_link1, c("pct_swnt", "pct_swclerical", "pct_swsales", "pct_swoperative", "pct_swprof"), 
#           depvarnames = c("non-teacher", "clerical", "sales","operative", "professional"), colors = gg_color_hue(5), years = c(1920, 1940))

did_graph(neighbor_link1, c("pct_swnt", "pct_swwhitecollar", "pct_swbluecollar"), 
          depvarnames = c("non-teacher", "white collar", "blue collar"), colors = gg_color_hue(3), years = c(1920, 1940))

did_graph(neighbor_link1, c("pct_swnt_cond", "pct_swwhitecollar_cond", "pct_swbluecollar_cond"), 
          depvarnames = c("non-teacher", "white collar", "blue collar"), colors = gg_color_hue(3), years = c(1920, 1940))


neighbor_link3 <- link3 %>% filter(neighbor_samp == 1 & mainsamp == 1)
did_graph(neighbor_link3, c("pct_mwnt", "pct_mwwhitecollar", "pct_mwbluecollar"), 
          depvarnames = c("non-teacher", "white collar", "blue collar"), colors = gg_color_hue(3), years = c(1920, 1940))
did_graph(neighbor_link3, c("pct_mwt", "pct_mwwhitecollar", "pct_mwbluecollar"), 
          depvarnames = c("non-teacher", "white collar", "blue collar"), colors = gg_color_hue(3), years = c(1920, 1940))

neighbor_sec1 <- link1sec %>% filter(neighbor_samp == 1 & mainsamp == 1)
did_graph(neighbor_sec1, c("pct_sws","pct_swns", "pct_swnilf","pct_mws","pct_mwns","pct_mwnilf"), 
          depvarnames = c("swt", "swnt", "swnilf","mwt", "mwnt", "mwnilf"), colors = gg_color_hue(6), years = c(1920, 1940))

did_graph(neighbor_sec1, c("pct_swns", "pct_swwhitecollar", "pct_swbluecollar"), 
          depvarnames = c("non-secretary", "white collar", "blue collar"), colors = gg_color_hue(3), years = c(1920, 1940))

## testing: link3 for other occs
nblink2 <- linkview %>% 
  addvars_indiv_linked() %>%
  filter(LABFORCE_base == 1 & demgroup_base == "SW" & AGE_base <= 40 & RACE_base == 1) %>%
  group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
  summarize(nlink = n(),
            lfp = sum(worker_link)/n(),
            share_teacher = sum(teacher_link)/n(),
            share_nurse = sum(ifelse(OCC1950_link == 58, 1, 0))/n(),
            share_op = sum(ifelse(OCC1950_link == 690, 1, 0))/n(),
            share_other_occ = sum(ifelse(OCC1950_link %in% c(350, 490, 390, 310, 290, 58), 1, 0))/n()) %>%
  collect() %>% 
    rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  addvars_county() 

x <- mainlinksamp(nblink2)
did_graph(nblink2 %>% filter(neighbor_samp == 1 & FIPS %in% x), 
          depvarlist = c("share_teacher","share_other_occ","share_nurse"),
          depvarnames = c("teacher", "nursing/clerical", "operative"),
          colors = c(mw_col,sw_col,men_col),
          years = c(1920, 1940))

nblink3 <- linkview %>% filter(LABFORCE_base == 1 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) %>%
  group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link, OCC1950_link) %>%
  summarize(n=n()) %>% 
  group_by(STATEICP_base, COUNTYICP_base, YEAR_base, YEAR_link) %>%
  mutate(share_occ = n/sum(n)) %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  collect() %>% addvars_county()

did_graph(nblink3 %>% filter(neighbor_samp == 1 & OCC1950_link == 310), 
          depvarlist = c("share_occ"),
          depvarnames = "mwnilf in t-10",
          colors = mw_col,
          years = c(1920, 1940))

## Testing: by age
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)
test1 <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>% 
  filter(RACE == 1 & AGE >= 18 & AGE <= 64) %>%
  mutate(agegrp = case_when(AGE < 18 ~ "<18",
                            AGE < 24 ~ "18-24",
                            AGE < 30 ~ "25-29",
                            AGE < 35 ~ "30-34",
                            AGE < 40 ~ "35-39",
                            AGE >= 40 ~ "40+")) %>%
  group_by(demgroup, agegrp, YEAR, STATEICP, COUNTYICP) %>%
  summarize(lfp = sum(worker)/n(),
            share_teacher = sum(teacher)/n(),
            share_nurse = sum(ifelse(OCC1950 == 58, 1, 0))/n(),
            share_other_occ = sum(ifelse(OCC1950 %in% c(350, 490, 390, 310, 290, 58), 1, 0))/n()) %>%
  collect() %>% addvars_county() %>%
  filter(neighbor_samp == 1)

did_graph(test1 %>% filter(demgroup == "MW" & agegrp == "35-39"),  depvarlist = c("lfp"), depvarnames = c("MW"), colors = mw_col)

did_graph(test1 %>% filter(demgroup == "MW" & agegrp == "25-29"),  depvarlist = c("share_other_occ"), depvarnames = c("MW"), colors = mw_col)

did_graph(test1 %>% filter(demgroup == "MW" & agegrp == "25-29"),  depvarlist = c("share_teacher","share_other_occ"), 
          depvarnames = c("teacher","clerical/nurse"), colors = c(mw_col,sw_col))

did_graph(test1 %>% filter(demgroup == "SW" & agegrp == "40+"),  depvarlist = c("share_teacher","share_other_occ"), 
          depvarnames = c("teacher","clerical/nurse"), colors = c(mw_col,sw_col))

test2 <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>% 
  filter(RACE == 1 & AGE >= 18 & AGE <= 64) %>%
  mutate(agegrp = case_when(AGE < 18 ~ "<18",
                            AGE < 24 ~ "18-24",
                            AGE < 30 ~ "25-29",
                            AGE < 35 ~ "30-34",
                            AGE < 40 ~ "35-39",
                            AGE >= 40 ~ "40+")) %>%
  group_by(demgroup, YEAR, STATEICP, COUNTYICP) %>%
  summarize(n = n(),
            lfp = sum(worker)/n(),
            share_teacher = sum(teacher)/n(),
            share_nurse = sum(ifelse(OCC1950 == 58, 1, 0))/n(),
            share_op = sum(ifelse(OCC1950 == 690, 1, 0))/n(),
            share_other_occ = sum(ifelse(OCC1950 %in% c(350, 490, 390, 310, 290, 58), 1, 0))/n()) %>%
  collect() %>% addvars_county() %>%
  filter(neighbor_samp == 1)

did_graph(test2 %>% filter(demgroup == "SW") %>% mutate(weight = n),  depvarlist = c("share_teacher","share_other_occ", "share_op"), 
          depvarnames = c("teacher","clerical/nurse", "op/kindred"), colors = c(mw_col,sw_col, men_col))

did_graph(test2 %>% filter(demgroup == "SW"),  depvarlist = c("lfp"), 
          depvarnames = c("lf"), colors = c(mw_col))

did_graph(test2 %>% filter(demgroup == "MW") %>% mutate(weight = n),  depvarlist = c("share_teacher","share_other_occ", "share_op"), 
          depvarnames = c("teacher","clerical/nurse", "op/kindred"), colors = c(mw_col,sw_col, men_col))

did_graph(test2 %>% filter(demgroup == "MW") %>% mutate(weight = n),  depvarlist = c("lfp"), 
          depvarnames = c("lf"), colors = c(mw_col))

did_graph(test2 %>% filter(demgroup == "M") %>% mutate(weight = n),  depvarlist = c("share_teacher","share_other_occ", "share_op"), 
          depvarnames = c("teacher","clerical/nurse", "op/kindred"), colors = c(mw_col,sw_col, men_col))



test3_raw <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>% 
  filter(RACE == 1 & AGE >= 18 & AGE <= 64)  %>%
  mutate(occgrp = case_when(OCC1950 < 100 ~ "Professional",
                            OCC1950 < 200 ~ "Farming",
                            OCC1950 < 300 ~ "Managers",
                            OCC1950 < 400 ~ "Clerical",
                            OCC1950 < 500 ~ "Sales",
                            OCC1950 < 600 ~ "Craftsmen",
                            OCC1950 < 700 ~ "Operatives",
                            OCC1950 < 800 ~ "Service",
                            OCC1950 < 980 ~ "Laborers",
                            TRUE ~ "Other")) %>%
  group_by(demgroup, YEAR, STATEICP, COUNTYICP, occgrp) %>%
  summarize(n = n()) %>%
  group_by(demgroup, YEAR, STATEICP, COUNTYICP) %>%
  mutate(share_occgrp = n/sum(n),
         ntot = sum(n)) %>%
  collect() %>% addvars_county() 

test3 <- test3_raw %>%
  filter(neighbor_samp == 1) %>%
  pivot_wider(id_cols = c(demgroup, YEAR, FIPS, ntot, TREAT), names_from = occgrp, values_from = c(share_occgrp, n))

occgrp_vars <- names(test3)[which(str_detect(names(test3), "^share_occgrp_") & !str_detect(names(test3), "Service") & 
                                    !str_detect(names(test3), "Farming") & !str_detect(names(test3), "Operatives") &
                                    !str_detect(names(test3), "Other") & !str_detect(names(test3), "Laborers"))]
occgrp_names <- str_remove(occgrp_vars,"^share_occgrp_")

did_graph(test3 %>% filter(demgroup == "MW") %>% mutate(weight = ntot),
          depvarlist = occgrp_vars,
          depvarnames = occgrp_names,
          colors = gg_color_hue(length(occgrp_vars)))

did_graph(test3 %>% filter(demgroup == "SW") %>% mutate(weight = ntot),
          depvarlist = occgrp_vars,
          depvarnames = occgrp_names,
          colors = gg_color_hue(length(occgrp_vars)))

# two questions: does marriage rate change, and does a share analysis for nursing look like anything?
test4 <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter(RACE == 1 & AGE >= 18 & AGE <= 64 & SEX == 2) %>%
  mutate(agegrp = case_when(AGE < 18 ~ "<18",
                            AGE < 24 ~ "18-24",
                            AGE < 30 ~ "25-29",
                            AGE < 35 ~ "30-34",
                            AGE < 40 ~ "35-39",
                            AGE >= 40 ~ "40+")) %>%
  group_by(YEAR, STATEICP, COUNTYICP, agegrp) %>%
  summarize(pct_marr = sum(ifelse(demgroup == "MW", 1, 0))/n()) %>%
  collect() %>%
  addvars_county()

did_graph(test4 %>% filter(neighbor_samp == 1 & agegrp == "25-29"), "pct_marr",
          "pct marriage", mw_col)

test5 <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  filter(RACE == 1 & AGE >= 18 & AGE <= 64) %>%
  mutate(occ = ifelse(OCC1950 %in% c(290), 1, 0)) %>%
  group_by(YEAR, STATEICP, COUNTYICP) %>%
  summarize(pct_mw = sum(ifelse(demgroup == "MW" & occ == 1, 1, 0))/sum(occ),
            pct_sw = sum(ifelse(demgroup == "SW" & occ == 1, 1, 0))/sum(occ),
            pct_m = sum(ifelse(demgroup == "M" & occ == 1, 1, 0))/sum(occ),
            n = sum(occ)
            ) %>%
  collect() %>%
  addvars_county() 

did_graph(test5 %>% filter(neighbor_samp == 1), 
          c("pct_mw", "pct_sw", "pct_m"), c("MW", "SW","Men"),
          c(mw_col,sw_col,men_col))

did_graph(test5 %>% filter(neighbor_samp == 1), c("n"), "num", mw_col)

occs <- tbl(con, "censusrawall") %>% 
  addvars_indiv() %>% 
  filter(YEAR == base_yr | YEAR == end_yr) %>%
  group_by(OCC1950, YEAR) %>%
  summarize(n_occ = sum(ifelse(LABFORCE == 2, 1, 0)),
            n_occ_wmw      = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), #num white MW in each occ
            n_occ_wsw = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 18 & AGE <= 64, 1, 0)), #num white SW in each occ
            n_emp_wmw      = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & LABFORCE == 2, 1, 0)), #num of white MW in labor force in each occ
            n_occ_wmw_coll = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11, 1, 0)), # num white MW w some college in each occ
            n_occ_wsw_coll = sum(ifelse(demgroup == "SW" & RACE == 1 & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11, 1, 0)), # num white MW w some college in each occ
            n_emp_wmw_coll = sum(ifelse(demgroup == "MW" & RACE == 1 & AGE >= 18 & AGE <= 64 & EDUC >= 7 & EDUC <= 11 & LABFORCE == 2, 1, 0)), #num of white MW w some college in LF in each occ
  ) %>%
  ungroup() %>%
  group_by(YEAR) %>% 
  mutate(NWHITEMW         = sum(n_occ_wmw), # total num white MW
         NWHITEMW_coll    = sum(n_occ_wmw_coll), # total num white MW w some college
         NWHITESW = sum(n_occ_wsw),
         NWHITESW_coll = sum(n_occ_wsw_coll),
         NWHITEMW_LF      = sum(n_emp_wmw),
         NWHITEMW_LF_coll = sum(n_emp_wmw_coll)
  ) %>%
  ungroup() %>% 
  collect() %>% #grouping into specific occupation groups
  mutate(share_occ_wmw_coll = n_occ_wmw_coll/n_occ_wmw,
         share_wmw_occ      = n_occ_wmw/NWHITEMW,
         share_wmw_coll_occ = n_occ_wmw_coll/NWHITEMW_coll,
         mb_occ_gen         = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400) | OCC1950 %in% c(730, 731, 56, 58), 1, 0),
         mb_occ             = ifelse((OCC1950 == 93) | (OCC1950 >= 300 & OCC1950 < 400), 1, 0),
         mb_occ_specific    = ifelse(OCC1950 %in% c(301, 302, 305, 350, 93), 1, 0),
         whitecollar_occ    = ifelse(OCC1950 < 500 & !(OCC1950 %in% c(100,123)), 1, 0))

## LOOKING AT HOUSEHOLD LEVEL SUMMARY STATS FOR TEACHERS
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)
hh_teach <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  mutate(HEAD = ifelse(PERNUM == 1, 1, 0)) %>% #for now -- calling pernum 1 head of household
  group_by(YEAR, SERIAL) %>%
  mutate(sizehh = n(),
         workershh = sum(ifelse(LABFORCE == 2, 1, 0)),
         occscorehh = sum(OCCSCORE)-OCCSCORE #combined occscore of other members of household
         ) %>%
  filter(teacher == 1) %>%
  group_by(STATEICP, COUNTYICP, demgroup, YEAR) %>%
  summarize(across(c(sizehh, workershh, occscorehh), mean)) %>%
  collect() %>%
  pivot_wider(id_cols = c(STATEICP, COUNTYICP, YEAR),
              names_from = c(demgroup), 
              values_from = c(sizehh, workershh, occscorehh)) %>%
  addvars_county()
x <- mainsamp(countysumm)
did_graph(hh_teach %>% filter(neighbor_samp == 1 & FIPS %in% x), "occscorehh_MW", depvarnames = "sw occscore", colors = mw_col)

hh_teach_summ <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  mutate(HEAD = ifelse(PERNUM == 1, 1, 0)) %>% #for now -- calling pernum 1 head of household
  group_by(YEAR, SERIAL) %>%
  mutate(sizehh = n(),
         workershh = sum(ifelse(LABFORCE == 2, 1, 0)),
         occs = sum(OCCSCORE)) %>%
  filter(SEX == 2 & worker == 1) %>%
  ungroup() %>%
  group_by(teacher, demgroup, YEAR) %>%
  summarize(across(c(sizehh, workershh, occs), mean)) %>%
  collect()



