# random stuff 6/5/25
# pct teacher mw
## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("pct_mw_Teacher", "pct_Teacher_mw_100")){
  out_did        <- did_graph_data(neighbor, coefname, table = TRUE) #returns list of [model, cov matrix]
  if (coefname == "pct_Teacher_mw_100"){
    out_did <- did_graph_data(neighbor %>% mutate(weight = NWHITEMW), coefname, table = TRUE) #returns list of [model, cov matrix]
  }
  models[[i]]    <- out_did[[1]]
  ses[[i]]       <- sqrt(diag(out_did[[2]]))
  
  if (coefname == "pct_Teacher_mw_100"){
    sharereg_means <- c(sharereg_means, weighted.mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]],filter(neighbor, YEAR == 1930 & TREAT == 1)$NWHITEMW))
  }
  else{
    sharereg_means <- c(sharereg_means, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]]))
  }
  i = i + 1
}

# gen table
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950")
## num teacher by race
## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (df in list(countysumm, countysumm_blk, countysumm_wht)){#c("pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "num_Teacher")){
  out_did        <- did_graph_data(df %>% filter(neighbor_samp == 1 & mainsampall == 1), "num_Teacher", table = TRUE) #returns list of [model, cov matrix]
  models[[i]]    <- out_did[[1]]
  ses[[i]]       <- sqrt(diag(out_did[[2]]))
  
  sharereg_means <- c(sharereg_means, mean(filter(df, neighbor_samp == 1 & mainsampall == 1 & YEAR == 1930 & TREAT == 1)[[coefname]]))
  
  i = i + 1
}
# gen table
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950"))


## checking school attendance (weird stuff w kentucky)

con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)

# checking unemp
tbl(con, 'censusrawall') %>% filter(YEAR <= 1930) %>%
  group_by(EMPSTAT, YEAR) %>% summarize(n=n()) %>% collect()

# getting border counties for KY
borders_ky <- read.table(glue("{root}/county_adjacency2010.txt"), sep = "\t", col.names = c("county_name","FIPS","border_name","border_FIPS")) %>%
  mutate(county_name = ifelse(county_name == "", NA, county_name),
         FIPS = str_pad(as.character(FIPS), 5, "left", pad = "0"),
         border_FIPS = str_pad(as.character(border_FIPS), 5, "left", pad = "0")) %>%
  fill(c(county_name, FIPS), .direction = "down") %>%
  mutate(state = substr(FIPS, 1, 2),
         border_state = substr(border_FIPS, 1, 2),
         border = ifelse(state != border_state, 1, 0),
         border_ctrl = ifelse(border & border_state %in% c("21"), 1, 0),
         border_treat = ifelse(border & state %in% c("21"), 1, 0)) 

border_treat_ky <- filter(borders_ky, border_treat == 1)$FIPS
border_ctrl_ky <- unique(filter(borders_ky, border_ctrl == 1)$FIPS)

children <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>%
  filter(STATEICP %in% c(47, 51, 40, 48, 54, 56) & AGE >= 6 & AGE <= 18)

# number of children 6-18
children_count <- children %>% group_by(STATEICP, COUNTYICP, YEAR) %>% summarize(n=n()) %>% collect() %>%
  addvars_county()

ggplot(children_count %>% group_by(AB, YEAR) %>% summarize(n=sum(n)), 
       aes(x = YEAR, y = n, color = AB)) + geom_point() + geom_line()

#border counties
children_count_border <- children %>% group_by(STATEICP, COUNTYICP, YEAR) %>%
  summarize(n=n()) %>% collect() %>%
  addvars_county() %>% filter(FIPS %in% border_treat_ky | FIPS %in% border_ctrl_ky) 

ggplot(children_count_border %>% group_by(AB, YEAR) %>% summarize(n=sum(n)), 
       aes(x = YEAR, y = n, color = AB)) + geom_point() + geom_line()


# number of children reporting each value of school
children_school <- children %>% group_by(STATEICP, COUNTYICP, YEAR, SCHOOL) %>% summarize(n=n()) %>% collect() %>%
  addvars_county()

ggplot(children_school %>% group_by(AB, YEAR, SCHOOL) %>% summarize(n=sum(n)) %>% filter(SCHOOL != 0) %>%
         ungroup() %>% group_by(AB, YEAR) %>% mutate(ntot = sum(n), share = n/ntot) %>%
         filter(SCHOOL == 2),
       aes(x = YEAR, color = AB, y = share)) + geom_point() + geom_line()

# border counties
children_school_border <- children %>% group_by(STATEICP, COUNTYICP, YEAR, SCHOOL) %>% summarize(n=n()) %>% collect() %>%
  addvars_county() %>% filter(FIPS %in% border_treat_ky | FIPS %in% border_ctrl_ky) 

ggplot(children_school_border %>% group_by(AB, YEAR, SCHOOL) %>% summarize(n=sum(n)) %>% filter(SCHOOL != 0) %>%
         ungroup() %>% group_by(AB, YEAR) %>% mutate(ntot = sum(n), share = n/ntot) %>%
         filter(SCHOOL == 2),
       aes(x = YEAR, color = AB, y = share)) + geom_point() + geom_line()

# number of children reporting each value of school at each age x race
children_school_age <- children %>% group_by(STATEICP, COUNTYICP, YEAR, SCHOOL, AGE) %>% summarize(n=n()) %>% collect() %>%
  addvars_county()

ggplot(children_school_age %>% group_by(AB, YEAR, SCHOOL, AGE) %>% summarize(n= sum(n)) %>% filter(SCHOOL > 0) %>%
         ungroup() %>% group_by(AB, YEAR, AGE) %>% mutate(ntot = sum(n), share = n/ntot) %>% filter(SCHOOL == 2),
       aes(x = AGE, color = AB, y = share)) + geom_point() + geom_line() + facet_wrap(~YEAR)

# now looking at full sample
all <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>%
  filter(STATEICP %in% c(47, 51, 40, 48, 54, 56))

# educational attainment
educ <- all %>%
  filter(AGE <= 30 & AGE >= 16) %>%
  mutate(educcat = case_when(EDUC == 0 ~ 99,
                             EDUC <= 2 ~ 1,
                             EDUC <= 6 ~ 2,
                             EDUC <= 11 ~ 3,
                             EDUC == 99 ~ 99),
                       somehs = case_when(EDUC == 0 | EDUC == 99 | is.na(EDUC) ~ 99,
                                          EDUC <= 2 ~ 0,
                                          TRUE ~ 1)) %>%
  group_by(STATEICP, COUNTYICP, somehs, YEAR, AGE) %>% summarize(n=n()) %>% collect() %>% addvars_county()

ggplot(educ %>% group_by(AB, YEAR, somehs, AGE) %>% summarize(n=sum(n)) %>% filter(somehs != 99) %>%
         ungroup() %>% group_by(AB, YEAR) %>% mutate(ntot = sum(n), share = n/ntot) %>% filter(somehs == 1),
       aes(x = AGE, color = AB, y = share)) + geom_point() + geom_line() + facet_wrap(~YEAR)

# mapping outcomes
children_summ <- tbl(con, "censusrawall") %>% #taking table from DuckDB
  addvars_indiv() %>%
  filter(AGE >= 6 & AGE <= 18 & SCHOOL > 0) %>%
  group_by(STATEICP, COUNTYICP, SCHOOL, YEAR) %>%
  summarize(n=n()) %>%
  ungroup() %>% 
  group_by(STATEICP, COUNTYICP, YEAR) %>%
  mutate(ntot = sum(n), share = n/ntot) %>%
  collect() %>%
  addvars_county()


exclude_st = c("CA", "CO", "OR", "TX", "WA", 
               "ND", "WY", "UT", "NM", "NV", 
               "SD", "MT", "ID", "AZ", "AK", 
               "HI", "KS", "NE", "OK",
               "MN", "AR", "MO", "IA", "LA")
yr = 1940
nq = 8
df <- children_summ %>% ungroup() %>% filter((YEAR == 1930 | YEAR == 1940) & SCHOOL == 2) %>% filter(neighbor_samp == 1)%>%
  mutate(fips = FIPS, weights = as.character(cut(share, 
                                                 quantile(share, probs = seq(0,1,1/nq))))) %>%
  filter(YEAR == yr) %>%
  select(fips, weights) %>% filter(!is.na(fips))


samp = c("KY","NC","VA","WV","SC","TN")
plot_usmap(data=df, values = "weights", include = samp,#exclude = exclude_st, 
           regions = "counties") + scale_fill_manual(values = colorRampPalette(c('white',control_col))(nq)) + 
  ggtitle(glue('year {yr}'))


