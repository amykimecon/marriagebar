# File Description: TRYING INDIVIDUAL-LEVEL ANALYSIS
# Author: Amy Kim
# Date Created: Thu Mar 13 11:04:52 2025

## run main file first for paths etc.
library(fixest)

# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)


# samp 
teach_samp_indiv <- tbl(con, "censusrawall") %>%
  addvars_indiv() %>%
  mutate(mwt = ifelse(teacher == 1 & demgroup == "MW", 1, 0),
         swt = ifelse(teacher == 1 & demgroup == "SW", 1, 0),
         mt = ifelse(teacher == 1 & demgroup == "M" ,1, 0),
         SOUTH           = ifelse(STATEICP %in% c(54, 41, 46, 51, 40, 56, 47, 11, 52, 48, 43, 44), 1, 0), #us census regions east south central and south atlantic
         TREAT           = ifelse(STATEICP == 47 | STATEICP == 51, 1, 0), # indicator for treated states
         neighbor_samp   = ifelse(STATEICP %in% c(47, 51, 40, 48, 54, 56), 1, 0), # indicator for treated or neighbor control
         neighbor_sampNC = ifelse(STATEICP %in% c(47, 48, 40, 54), 1, 0), #neighbors for NC: SC, VA, TN
         neighbor_sampKY = ifelse(STATEICP %in% c(51, 54, 56, 21, 24, 22), 1, 0) #neighbors for KY: TN, WV, IL, IN, OH
  ) %>%
  filter(neighbor_samp == 1) #%>%
  # group_by(YEAR, STATEICP, COUNTYICP) %>%
  # mutate(UNEMP_RATE      = sum(ifelse(worker != 0 & OCC1950 >= 980, 1, 0))/sum(ifelse(worker != 0, 1, 0))) %>%
  # ungroup()
  
#samp <- teach_samp_indiv %>% slice_sample(n=100000) %>% collect()
fulldata_samp <- teach_samp_indiv %>% filter(AGE >= 16 & AGE <= 65) #%>% collect()
#women_samp <- fulldata_samp %>% filter(demgroup != "M") %>% collect() %>% addvars_county()
teacher_samp <- fulldata_samp %>% filter(teacher == 1) %>% collect() %>% addvars_county()

## APP TABLE B2: INDIV W MODS
t <- teacher_samp %>% 
  left_join(countysumm %>% select(STATEICP, COUNTYICP, YEAR, mainsampall, mainsampwht, mainsampblk,
                     share_manuf, share_ag, unemp_rate, POP), 
            by = c('STATEICP', 'COUNTYICP', 'YEAR')) %>% add_did_dummies() %>% 
  group_by(YEAR, STATEICP, COUNTYICP) %>% mutate(nteach = n(), 
                                                 nonwhite = ifelse(RACE != 1, 1,0),
                                                 urb = ifelse(URBAN == 2, 1, 0)) %>%
  filter(mainsampall == 1)

models = list(
  feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
        data = t),
  feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t),
  feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t %>% filter(mainsampblk == 1)),
  feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 + share_manuf + share_ag + log(POP) + unemp_rate| STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t %>% filter(YEAR %in% c(1930, 1940))),
  feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t %>% add_tva_ind() %>% filter(tva == 0)),
  feols(mwt ~ factor(nonwhite) +  factor(nonwhite)*Year1910+factor(nonwhite)*Year1920+factor(nonwhite)*Year1940+factor(nonwhite)*Year1950 + factor(nonwhite)*TREATx1910+factor(nonwhite)*TREATx1920+factor(nonwhite)*TREATx1940+factor(nonwhite)*TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t),
  feols(mwt ~ factor(urb) +  factor(urb)*Year1910+factor(urb)*Year1920+factor(urb)*Year1940+factor(urb)*Year1950 + factor(urb)*TREATx1910+factor(urb)*TREATx1920+factor(urb)*TREATx1940+factor(urb)*TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t),
  feols(mwt ~ factor(urb)*factor(nonwhite) +  factor(urb)*factor(nonwhite)*Year1910+factor(urb)*factor(nonwhite)*Year1920+factor(urb)*factor(nonwhite)*Year1940+factor(urb)*factor(nonwhite)*Year1950 + factor(urb)*factor(nonwhite)*TREATx1910+factor(urb)*factor(nonwhite)*TREATx1920+factor(urb)*factor(nonwhite)*TREATx1940+factor(urb)*factor(nonwhite)*TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t),
  feols(mwt ~ AGE +  AGE*Year1910+AGE*Year1920+AGE*Year1940+AGE*Year1950 + AGE*TREATx1910+AGE*TREATx1920+AGE*TREATx1940+AGE*TREATx1950 | STATEICP^COUNTYICP, 
        weights = ~1/nteach,
        data = t %>% mutate(AGE = AGE - 30))
)

esttex(models, keep = c("TREATx1940"), fitstat = c("n","ar2"))



# # overall effects
# women <- fulldata_analysis %>% filter(demgroup!="M")
# a1 <- feols(teacher ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
#       data =  women %>% add_did_dummies() %>% mutate(married = ifelse(demgroup == "MW", 1, 0)))
# 
# a2 <- feols(teacher ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
#             data = women %>% filter(RACE == 1) %>% add_did_dummies() %>% mutate(married = ifelse(demgroup == "MW", 1, 0)))
# 
# a3 <- feols(teacher ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
#             data = women %>% filter(RACE == 2) %>% add_did_dummies() %>% mutate(married = ifelse(demgroup == "MW", 1, 0)))
# 
# esttex(list(a1, a2, a3))
# 
# teacher_samp <- fulldata_analysis %>% filter(teacher == 1)

teacher_samp_withct <- teacher_samp %>% 
  left_join(countysumm %>% filter(YEAR == 1930) %>% mutate(urbanind = ifelse(URBAN > 0.5, 1, 0)) %>% select(STATEICP, COUNTYICP, urbanind), 
            by = c('STATEICP', 'COUNTYICP'))

years = c(1910, 1920, 1940, 1950)
interact_vars <- glue("TREATx{years}")
yearvars <- glue("Year{years}")

teacher_samp_ct <- teacher_samp_withct %>% group_by(YEAR, STATEICP, COUNTYICP, TREAT, neighbor_samp) %>%
  summarize(mwt = sum(mwt)/n(), swt = sum(ifelse(demgroup == "SW", 1, 0))/n(), mt = sum(ifelse(demgroup == "M", 1, 0))/n(),
            nteach = n(), urbanind = mean(urbanind))

teacher_samp_ct_w <- teacher_samp_withct %>% group_by(YEAR, STATEICP, COUNTYICP, TREAT, neighbor_samp) %>%
  filter(RACE == 1) %>%
  summarize(mwt = sum(mwt)/n(), swt = sum(ifelse(demgroup == "SW", 1, 0))/n(), mt = sum(ifelse(demgroup == "M", 1, 0))/n(),
            nteach = n(), urbanind = mean(urbanind))

teacher_samp_ct_b <- teacher_samp_withct %>% group_by(YEAR, STATEICP, COUNTYICP, TREAT, neighbor_samp) %>%
  filter(RACE == 2) %>%
  summarize(mwt = sum(mwt)/n(), swt = sum(ifelse(demgroup == "SW", 1, 0))/n(), mt = sum(ifelse(demgroup == "M", 1, 0))/n(),
            nteach = n(), urbanind = mean(urbanind))

models = list()
headers = list()
dfs = list(teacher_samp_withct, teacher_samp_withct %>% filter(RACE == 1), 
           teacher_samp_withct %>% filter(RACE == 2))
dfnames = c('all', 'white','black')
urbnames = c('rural','urban')
i = 1
for (dfnum in 1:3){
  for (urban in 0:1){
    models[[i]] <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
                         weights = ~1/nteach,
                         data = dfs[[dfnum]] %>% filter(urbanind == urban) %>% #& !(FIPS %in% c(47157, 45019, 51760, 47037, 21111))) %>%
                           add_did_dummies() %>% group_by(YEAR, STATEICP, COUNTYICP) %>% mutate(nteach = n()))  
    headers[[i]] <- glue("{dfnames[dfnum]} - {urbnames[urban + 1]}")
    i = i + 1
  }
}

esttex(models, keep = c("TREATx1940", "TREATx1950"))


# main model
a <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
      data = teacher_samp %>% add_did_dummies() %>% group_by(YEAR, STATEICP, COUNTYICP) %>% mutate(nteach = n()) %>% ungroup())

b <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
      data = teacher_samp %>% add_did_dummies() %>% filter(RACE == 1) %>% group_by(YEAR, STATEICP, COUNTYICP) %>% mutate(nteach = n()) %>% ungroup())

c <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
      data = teacher_samp %>% add_did_dummies() %>% filter(RACE == 2) %>% group_by(YEAR, STATEICP, COUNTYICP) %>% mutate(nteach = n()) %>% ungroup())

esttex(list(a,b,c), keep = c("TREATx1940", "TREATx1950"))

x <- feols(mwt ~ factor(RACE)*Year1910+factor(RACE)*Year1920+factor(RACE)*Year1940+factor(RACE)*Year1950 + 
        factor(RACE)*TREATx1910+factor(RACE)*TREATx1920+factor(RACE)*TREATx1940+factor(RACE)*TREATx1950 | STATEICP^COUNTYICP, 
      data = teacher_samp %>% add_did_dummies() %>% filter(RACE <= 2))

# main model (county)
ac <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
           data = teacher_samp_ct %>% add_did_dummies())

bc <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
           data = teacher_samp_ct_w %>% add_did_dummies()) 

cc <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
           data = teacher_samp_ct_b %>% add_did_dummies())

esttex(list(ac,bc,cc), keep = c("TREATx1940", "TREATx1950"))

# urban/rural (county)
aurb <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
              data = teacher_samp_ct %>% add_did_dummies() %>% filter(urbanind == 1))

arur <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
              data = teacher_samp_ct %>% add_did_dummies() %>% filter(urbanind == 0))

burb <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
              data = teacher_samp_ct_w %>% add_did_dummies() %>% filter(urbanind == 1))

brur <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
              data = teacher_samp_ct_w %>% add_did_dummies() %>% filter(urbanind == 0))

curb <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
              data = teacher_samp_ct_b %>% add_did_dummies() %>% filter(urbanind == 1))

crur <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
              data = teacher_samp_ct_b %>% add_did_dummies() %>% filter(urbanind == 0))

esttex(list(aurb, arur, burb, brur, curb, crur), keep = c("TREATx1940", "TREATx1950"))


# overall
women_samp <- teach_samp_indiv %>% filter(demgroup != "M" & RACE == 1 & AGE >= 18 & AGE <= 65) %>% collect() 



model_marr <- feols(married ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
               data = women_samp %>% add_did_dummies() %>% mutate(married = ifelse(demgroup == "MW", 1, 0)))
model_teach_all <- feols(teacher ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 | STATEICP^COUNTYICP, 
                    data = women_samp %>% add_did_dummies() %>% mutate(married = ifelse(demgroup == "MW", 1, 0)))

model <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950 + 
                 i(YEAR, URBAN)| STATEICP^COUNTYICP, 
                data = women_samp %>% add_did_dummies() %>% filter(STATEICP != 40 & STATEICP != 48 & demgroup == "MW"))


teachsamp <- teach_samp_indiv %>% filter(RACE == 1 & AGE >= 18 & AGE <= 65 & teacher == 1) %>% collect()
model_teach <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950| STATEICP^COUNTYICP, 
               data = teachsamp %>%
                 add_did_dummies())

x <- teach_samp_indiv %>% filter(teacher == 1) %>% group_by(demgroup, YEAR, TREAT) %>% summarize(n=n()) %>% collect()
y <- x %>% group_by(YEAR, TREAT) %>% mutate(ntot = sum(n)) %>% ungroup() %>% mutate(share = n/ntot)
ggplot(y, aes(x = YEAR, y = share, color = demgroup)) + geom_point() + facet_wrap(~TREAT)


x <- teach_samp_indiv %>% filter(demgroup != "M" & RACE == 1 & AGE >= 18 & AGE <= 65) %>% 
  group_by(teacher, demgroup, YEAR, TREAT) %>% summarize(n=n()) %>% collect()
y <- x %>% group_by(YEAR, TREAT, demgroup) %>% mutate(ntot = sum(n)) %>% ungroup() %>% mutate(share = n/ntot)
ggplot(y %>% filter(teacher == 1), aes(x = YEAR, y = share, color = demgroup)) + geom_point() + facet_wrap(~TREAT)


## LINKED ANALYSIS
linkview <-  tbl(con, "linkedall") %>% 
  addvars_indiv_linked() %>%
  mutate(NCHILD_base = `NCHILD`, 
         NCHILD_link = `NCHILD_1`,
         URBAN_base = `URBAN`,
         URBAN_link = `URBAN_1`,
         OCCSCORE_base = `OCCSCORE`,
         OCCSCORE_link = `OCCSCORE_1`) %>% #temporary while NCHILD isn't explicitly relabelled in duckdb
  dplyr::select(c(ends_with("_base"),ends_with("_link"))) %>% #only keeping variables that have been selected (see duckdb_init)
  filter(SEX_base == SEX_link & RACE_base == RACE_link &  #only keeping links with consistent sex and race (drops 1.2% of links)
           AGE_base <= AGE_link - 5 & AGE_base >= AGE_link - 15) #and consistent age (age in base year 5-15 years less than age in link year) -- drops an additional 2.2% of links

link1_filt <- linkview %>% 
  filter(teacher_base == 1 & worker_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) 

link1_ct <- link1_filt %>% summlinks(n=5)

link1 <- link1_filt %>% collect() %>% 
  left_join(link1_ct %>% select(STATEICP, COUNTYICP, YEAR_base, YEAR, 
                                                      mainsamp, neighbor_samp, neighbor_sampNC, neighbor_sampKY,
                                                      FIPS, SOUTH, TREAT, nlink),
                                  by = c("STATEICP_base" = "STATEICP", "COUNTYICP_base" = "COUNTYICP",
                                         "YEAR_base" = "YEAR_base", "YEAR_link" = "YEAR")) %>%
  mutate(
    married_link = ifelse(demgroup_link == "MW", 1, 0),
    mwt_link = ifelse(teacher_link & married_link, 1, 0),
         mwnt_link = ifelse(teacher_link == 0 & worker_link == 1 & married_link == 1, 1, 0),
         mwnilf_link = ifelse(nilf_link & married_link, 1, 0)) %>% 
  filter(neighbor_samp == 1 & mainsamp == 1)

link2_filt <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) 

link2_ct <- link2_filt %>% summlinks()

link2 <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) %>%
  mutate(teacher_link   = ifelse(OCC1950_link == 93 & CLASSWKR_link == 2 & worker_link == 1, 1, 0),
         married_link     = ifelse(MARST_link == 1 | MARST_link == 2, 1, 0), 
         nilf_link = ifelse(worker_link == 0, 1, 0),
         SOUTH           = ifelse(STATEICP_base %in% c(54, 41, 46, 51, 40, 56, 47, 11, 52, 48, 43, 44), 1, 0), #us census regions east south central and south atlantic
         TREAT           = ifelse(STATEICP_base == 47 | STATEICP_base == 51, 1, 0), # indicator for treated states
         neighbor_samp   = ifelse(STATEICP_base %in% c(47, 51, 40, 48, 54, 56), 1, 0), # indicator for treated or neighbor control
         neighbor_sampNC = ifelse(STATEICP_base %in% c(47, 48, 40, 54), 1, 0), #neighbors for NC: SC, VA, TN
         neighbor_sampKY = ifelse(STATEICP_base %in% c(51, 54, 56, 21, 24, 22), 1, 0) #neighbors for KY: TN, WV, IL, IN, OH
  ) %>% filter(neighbor_samp == 1) %>% collect() %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  mutate(mwt_link = ifelse(teacher_link & married_link, 1, 0),
         mwnt_link = ifelse(teacher_link == 0 & nilf_link == 0 & married_link == 1, 1, 0),
         mwnilf_link = ifelse(nilf_link & married_link, 1, 0)) %>%
  retailsales() 

link3 <- linkview %>% 
  filter(LABFORCE_base == 1 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) %>%
  mutate(teacher_link   = ifelse(OCC1950_link == 93 & CLASSWKR_link == 2 & worker_link == 1, 1, 0),
         married_link     = ifelse(MARST_link == 1 | MARST_link == 2, 1, 0), 
         nilf_link = ifelse(worker_link == 0, 1, 0),
         SOUTH           = ifelse(STATEICP_base %in% c(54, 41, 46, 51, 40, 56, 47, 11, 52, 48, 43, 44), 1, 0), #us census regions east south central and south atlantic
         TREAT           = ifelse(STATEICP_base == 47 | STATEICP_base == 51, 1, 0), # indicator for treated states
         neighbor_samp   = ifelse(STATEICP_base %in% c(47, 51, 40, 48, 54, 56), 1, 0), # indicator for treated or neighbor control
         neighbor_sampNC = ifelse(STATEICP_base %in% c(47, 48, 40, 54), 1, 0), #neighbors for NC: SC, VA, TN
         neighbor_sampKY = ifelse(STATEICP_base %in% c(51, 54, 56, 21, 24, 22), 1, 0) #neighbors for KY: TN, WV, IL, IN, OH
  ) %>% filter(neighbor_samp == 1) %>% collect() %>%
  rename(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>%
  mutate(mwt_link = ifelse(teacher_link & married_link, 1, 0),
         mwnt_link = ifelse(teacher_link == 0 & nilf_link == 0 & married_link == 1, 1, 0),
         mwnilf_link = ifelse(nilf_link & married_link, 1, 0)) %>%
  retailsales() 



years = c(1920, 1940)
interact_vars <- glue("TREATx{years}")
yearvars <- glue("Year{years}")

teacher_coefs = c()
for (age in 18:40){
  model <- feols(teacher_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
                 data = link1 %>% filter(neighbor_sampKY==1) %>% add_did_dummies() %>% filter(AGE_base == age))
  teacher_coefs <- c(teacher_coefs, model$coefficients['TREATx1940'])
}
ggplot(data = data.frame(age = 18:40, coef = teacher_coefs), aes(x = age, y = coef)) + geom_point()


mar_coefs = c()
for (age in 18:40){
  model <- feols(mwnilf_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
                 data = link1 %>% add_did_dummies() %>% filter(neighbor_sampKY == 1 & AGE_base == age))
  mar_coefs <- c(mar_coefs, model$coefficients['TREATx1940'])
}
ggplot(data = data.frame(age = 18:40, coef = mar_coefs), aes(x = age, y = coef)) + geom_point()

feols(teacher_link ~ Year1920+Year1940+TREATx1920+TREATx1940 + AGE_base+ I(AGE_base^2)| STATEICP^COUNTYICP, 
      data = link1 %>% add_did_dummies() %>% filter(AGE_base <= 30))

feols(married_link ~ Year1920+Year1940+TREATx1920+TREATx1940 + AGE_base+ I(AGE_base^2)| STATEICP^COUNTYICP, 
      data = link1 %>% add_did_dummies() %>% filter(AGE_base <= 30))

feols(mwt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 + AGE_base + I(AGE_base^2)| STATEICP^COUNTYICP, 
      data = link1 %>% add_did_dummies() %>% filter(AGE_base <= 30))

feols(mwnilf_link ~ Year1920+Year1940+TREATx1920+TREATx1940 + AGE_base + I(AGE_base^2)| STATEICP^COUNTYICP, 
      data = link1 %>% add_did_dummies() %>% filter(AGE_base <= 30))


feols(teacher_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link2 %>% add_did_dummies())
feols(married_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link2 %>% add_did_dummies())
feols(mwt_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link2 %>% add_did_dummies())
feols(mwnilf_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link2 %>% add_did_dummies())

feols(teacher_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link3 %>% add_did_dummies())
feols(mwt_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link3 %>% add_did_dummies())
feols(mwnilf_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
      data = link3 %>% add_did_dummies())


model <- feols(mwt_link ~ Year1920+Year1940+TREATx1920+TREATx1940| STATEICP^COUNTYICP, 
               data = link1 %>% add_did_dummies())

model_teach <- feols(mwt ~ Year1910+Year1920+Year1940+Year1950 + TREATx1910+TREATx1920+TREATx1940+TREATx1950| STATEICP^COUNTYICP, 
                     data = fulldata_samp %>% filter(teacher == 1) %>%
                       add_did_dummies())

model_link1 <- feols()

