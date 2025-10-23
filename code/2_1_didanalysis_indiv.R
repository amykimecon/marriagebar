### REPLICATING 2_didanalysis.R with individual-level regressions
### (see helper.R for helper functions to run regressions and create graphs)
### AUTHOR: AMY KIM

# opening connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"), read_only=TRUE)

# INITIALIZING DATASETS: LINKED ----
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
           AGE_base <= AGE_link - 5 & AGE_base >= AGE_link - 15) %>% #and consistent age (age in base year 5-15 years less than age in link year) -- drops an additional 2.2% of links
  mutate(move = ifelse(STATEICP_base != STATEICP_link | COUNTYICP_base != COUNTYICP_link, 1, 0),
         move_state = ifelse(STATEICP_base != STATEICP_link, 1, 0),
         urbanind_link = ifelse(URBAN_link == 2, 1, 0),
         urbanind_base = ifelse(URBAN_base == 2, 1, 0),
         big_ct = case_when(STATEICP_base == 51 & COUNTYICP_base %in% c(1110) ~ 1,
                            STATEICP_base == 54 & COUNTYICP_base %in% c(1570, 370, 930, 650) ~ 1,
                            STATEICP_base == 40 & COUNTYICP_base %in% c(7600) ~ 1,
                            STATEICP_base == 56 & COUNTYICP_base %in% c(390) ~ 1,
                            STATEICP_base == 47 & COUNTYICP_base %in% c(810, 1190) ~ 1,
                            STATEICP_base == 48 & COUNTYICP_base %in% c(830) ~ 1,
                            TRUE ~ 0))

retailsales_ct <- countysumm %>% group_by(STATEICP, COUNTYICP) %>% summarize() %>% retailsales() %>%
  select(c(STATEICP, COUNTYICP, RRTSAP29, RRTSAP39)) %>%
  pivot_longer(cols = c(RRTSAP29, RRTSAP39), 
               names_prefix = "RRTSAP", names_to = "rrtsap_year", values_to = "retailsales") %>%
  mutate(YEAR = ifelse(rrtsap_year == 29, 1930, 1940))

countysumm_stats <- countysumm %>% select(YEAR, STATEICP, COUNTYICP, POP, LFP, match_weight1, match_weight2, match_weight3) %>%
  left_join(retailsales_ct)

link1_filt <- linkview %>% 
  filter(teacher_base == 1 & worker_base == 1 & demgroup_base == "SW" & RACE_base == 1 & AGE_base <= 40) 

link1_ct <- link1_filt %>% summlinks(n=5) %>% matching_join(matchlist)

link1 <- link1_filt %>% collect() %>% 
  left_join(link1_ct %>% select(STATEICP, COUNTYICP, YEAR_base, YEAR, 
                                mainsamp, neighbor_samp, neighbor_sampNC, neighbor_sampKY,
                                FIPS, SOUTH, TREAT, nlink),
            by = c("STATEICP_base" = "STATEICP", "COUNTYICP_base" = "COUNTYICP",
                   "YEAR_base" = "YEAR_base", "YEAR_link" = "YEAR")) %>%
  left_join(countysumm_stats, by = c("YEAR_link" = "YEAR", 
                                     "STATEICP_base" = "STATEICP", 
                                     "COUNTYICP_base" = "COUNTYICP")) %>%
  mutate(
    married_link = ifelse(demgroup_link == "MW", 1, 0),
    mwt_link = ifelse(teacher_link & married_link, 1, 0),
    mwnt_link = ifelse(teacher_link == 0 & worker_link == 1 & married_link == 1, 1, 0),
    mwnilf_link = ifelse(worker_link == 0 & married_link, 1, 0)) 


link2_filt <- linkview %>% 
  filter(worker_base == 0 & demgroup_base == "SW" & AGE_base <= 40 & AGE_base >= 8 & RACE_base == 1) 

link2_ct <- link2_filt %>% summlinks()

link2 <- link2_filt %>% collect() %>% 
  left_join(link2_ct %>% select(STATEICP, COUNTYICP, YEAR_base, YEAR, 
                                mainsamp, neighbor_samp, neighbor_sampNC, neighbor_sampKY,
                                FIPS, SOUTH, TREAT, nlink,pct_mw, pct_mwt, pct_mwnt, pct_mwnilf),
            by = c("STATEICP_base" = "STATEICP", "COUNTYICP_base" = "COUNTYICP",
                   "YEAR_base" = "YEAR_base", "YEAR_link" = "YEAR")) %>%
  left_join(countysumm_stats, by = c("YEAR_link" = "YEAR", 
                                     "STATEICP_base" = "STATEICP", 
                                     "COUNTYICP_base" = "COUNTYICP")) %>%
  mutate(
    married_link = ifelse(demgroup_link == "MW", 1, 0),
    mwt_link = ifelse(teacher_link & married_link, 1, 0),
    mwnt_link = ifelse(teacher_link == 0 & worker_link == 1 & married_link == 1, 1, 0),
    mwnilf_link = ifelse(worker_link == 0 & married_link, 1, 0)) 

link2_grp_test <- link2 %>% group_by(STATEICP_base, COUNTYICP_base, YEAR_link, TREAT,
                                     mainsamp, neighbor_samp, pct_mw, pct_mwt, pct_mwnt, pct_mwnilf) %>%
  summarize(across(c(married_link, mwt_link, mwnt_link, mwnilf_link), mean), nlink = n())

link3_filt <- linkview %>% 
  filter(LABFORCE_base == 1 & demgroup_base == "MW" & AGE_base <= 50 & AGE_base >= 18 & RACE_base == 1) 

link3_ct <- link3_filt %>% summlinks()

link3 <- link3_filt %>% collect() %>% 
  left_join(link3_ct %>% select(STATEICP, COUNTYICP, YEAR_base, YEAR, 
                                mainsamp, neighbor_samp, neighbor_sampNC, neighbor_sampKY,
                                FIPS, SOUTH, TREAT, nlink),
            by = c("STATEICP_base" = "STATEICP", "COUNTYICP_base" = "COUNTYICP",
                   "YEAR_base" = "YEAR_base", "YEAR_link" = "YEAR")) %>%
  left_join(countysumm_stats, by = c("YEAR_link" = "YEAR", 
                                     "STATEICP_base" = "STATEICP", 
                                     "COUNTYICP_base" = "COUNTYICP")) %>%
  mutate(
    married_link = ifelse(demgroup_link == "MW", 1, 0),
    mwt_link = ifelse(teacher_link & married_link, 1, 0),
    mwnt_link = ifelse(teacher_link == 0 & worker_link == 1 & married_link == 1, 1, 0),
    mwnilf_link = ifelse(worker_link == 0 & married_link, 1, 0)) 

# TESTING INDIV VS COUNTY
# county unweighted
feols(pct_mwnt ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_samp == 1)  %>% 
        add_did_dummies())

# county weighted by nlink
feols(pct_mwnt ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weights = ~nlink,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_samp == 1)  %>% 
        add_did_dummies())

feols(pct_mwnt ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weights = ~nlink,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_samp == 1)  %>% 
        add_did_dummies())

feols(pct_mwwhitecollar ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weights = ~nlink,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_samp == 1)  %>% 
        add_did_dummies())

feols(pct_mwbluecollar ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weights = ~nlink,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_samp == 1)  %>% 
        add_did_dummies())

feols(pct_mwnt ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weights = ~nlink,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_sampKY == 1)  %>% 
        add_did_dummies())


feols(pct_mwnt ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weights = ~nlink,
      data = link2_ct %>% filter(mainsamp == 1 & neighbor_sampNC == 1)  %>% 
        add_did_dummies())

# individual
feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      data = link2 %>% filter(mainsamp == 1 & neighbor_samp == 1) %>% 
        mutate(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>% 
        add_did_dummies())

# individual inverse weighted
feols(mwnt_link ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP,
      weight = ~ 1/nlink,
      data = link2 %>% filter(mainsamp == 1 & neighbor_samp == 1) %>% 
        mutate(STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base, YEAR = YEAR_link) %>% 
        add_did_dummies())

# county density plot
ggplot(data = link2_grp_test %>% filter(mainsamp == 1 & neighbor_samp == 1 & YEAR_link != 1920),
       aes(x = pct_mwnt, color = factor(TREAT), fill = factor(TREAT))) + 
  geom_density(alpha = 0.3) + facet_wrap(~YEAR_link)

ggplot(data = link2_grp_test %>% filter(mainsamp == 1 & neighbor_samp == 1 & YEAR_link != 1920),
       aes(x = pct_mwnt, color = factor(TREAT), fill = factor(TREAT), weights = nlink)) + 
  geom_density(alpha = 0.3) + facet_wrap(~YEAR_link)

# raw means
ggplot(data = link2 %>% group_by(STATEICP_base, YEAR_link) %>% 
         filter(neighbor_samp == 1 & mainsamp == 1) %>%
         summarize(pct_mwnt_indiv = mean(mwnt_link)), 
       aes(x = YEAR_link, y = pct_mwnt_indiv, color = factor(STATEICP_base))) + 
  geom_point()

ggplot(data = link2_ct %>% group_by(TREAT, YEAR) %>% 
         filter((FIPS %in% border_treat | FIPS %in% border_ctrl) & mainsamp == 1) %>%
         summarize(pct_mwnt_ct = mean(pct_mwnt)), 
       aes(x = YEAR, y = pct_mwnt_ct, color = factor(TREAT))) + 
  geom_point()
#______________________________________________________
# RESULT 1: DIRECT EFFECT ON MARRIED WOMEN (TODO) ----
#______________________________________________________
# OUTCOME: P(teacher|MW) (only for neighbor sample)
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_mw_Teacher"),
          depvarnames = c("Married Women"),
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          pointtypes = c(17),
          ymin = -0.015, ymax = 0.06,
          filename = "shareteachmw_neighbor")

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
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950"),#omit = c("Constant","cluster*", "factor*"), 
          out = glue("./tables/mwregs.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          dep.var.labels.include = FALSE,
          column.labels = c("Share Teach Mar. Wom.", "MW Teach/100 MW"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 2: MECHANISMS FOR MARRIED WOMEN ----
#______________________________________________________

## stargazer tables ----
datalist = list(link1, link3, link2)
datanames = c("Sample 1: SWT in t-10", "Sample 2: MWNILF in t-10", "Sample 3: SWNILF in t-10")
coefs = c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")
coefnames = c("0) P[Married]", "1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]")
ctrls = c("", "+ AGE_base+ I(AGE_base^2)", "+ urbanind_link", "+ POP + I(POP^2)", "+ retailsales", "+ AGE_base+ I(AGE_base^2) + urbanind_link + POP + I(POP^2) + retailsales")
ctrlnames = c("None", "Age + Age Sq.", "Urban/Rural", "County Population + Pop Sq.", "Retail Sales", "All Ctrls")

modelcoefs = list()
modelses = list()
samplelabs = list()
outcomelabs = list()
ctrllabs = list()
means = list()
ns = list()
rsqs = list()

# prepare data 
i = 1
for (dataind in 1:3){
  for (coefind in 1:4){
    #for (ctrlind in 1:length(ctrls)){
      #print(ctrlnames[ctrlind])
      model <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
                     data = datalist[[dataind]] %>%
                       mutate(YEAR = YEAR_link, STATEICP = STATEICP_base, COUNTYICP = COUNTYICP_base) %>% 
                       filter(neighbor_samp == 1 & mainsamp == 1) %>%
                       add_did_dummies())
      print(glue("Sample: {datanames[dataind]}"))
      print(glue("Outcome: {coefnames[coefind]}"))
      print(model)
      modelcoefs[[i]] <- model$coeftable['TREATx1940','Estimate']
      modelses[[i]] <- summary(model)$se['TREATx1940']
      samplelabs[[i]] <- datanames[dataind]
      outcomelabs[[i]] <- coefnames[coefind]
      #ctrllabs[[i]] <- ctrlnames[ctrlind]
      means[[i]] <- mean(filter(datalist[[dataind]], YEAR_link == 1930 & TREAT == 1)[[coefs[coefind]]], na.rm=TRUE)
      print(means[[i]])
      ns[[i]] <- model$nobs 
      rsqs[[i]] <- r2(model)['ar2']
      #linkreg_means <- c(linkreg_means1, mean(filter(link1, YEAR == 1930 & TREAT == 1)[[coefname]]))
      i = i + 1 
    #}
  }
}

outdata <- data.frame(coefs = unlist(modelcoefs), 
                      ses = unlist(modelses),
                      sample = unlist(samplelabs), 
                      outcomevar = unlist(outcomelabs),
                      mean = unlist(means),
                      n = unlist(ns),
                      rsq = unlist(rsqs)) %>%
  mutate(ylb = coefs - ses*1.96,
         yub = coefs + ses*1.96)

# ggplot(data = outdata,
#        aes(x = outcomevar, y = coefs, color = ctrls, shape = ctrls)) +
#   geom_hline(yintercept = 0, alpha = 0.3) +
#   geom_point(position = position_dodge(0.2)) +
#   geom_errorbar(aes(ymin = ylb, ymax = yub), width = 0.1,position = position_dodge(0.2)) +
#   facet_wrap(~sample, nrow = 3)

ggplot(data = outdata,
       aes(x = outcomevar, y = coefs)) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = ylb, ymax = yub), width = 0.1,position = position_dodge(0.2)) +
  facet_wrap(~sample, nrow = 3)

## BORDER COUNTIES
borders <- read.table(glue("{root}/county_adjacency2010.txt"), sep = "\t", col.names = c("county_name","FIPS","border_name","border_FIPS")) %>%
  mutate(county_name = ifelse(county_name == "", NA, county_name),
         FIPS = str_pad(as.character(FIPS), 5, "left", pad = "0"),
         border_FIPS = str_pad(as.character(border_FIPS), 5, "left", pad = "0")) %>%
  fill(c(county_name, FIPS), .direction = "down") %>%
  mutate(state = substr(FIPS, 1, 2),
         border_state = substr(border_FIPS, 1, 2),
         border = ifelse(state != border_state, 1, 0),
         border_ctrl = ifelse(border & border_state %in% c("37", "21"), 1, 0),
         border_treat = ifelse(border & state %in% c("37", "21"), 1, 0)) 

border_treat <- filter(borders, border_treat == 1)$FIPS
border_ctrl <- unique(filter(borders, border_ctrl == 1)$FIPS)


datalist = list(link1_ct, 
                link2_ct, 
                link3_ct)
datanames = c("Sample 1: SWT in t-10", "Sample 2: MWNILF in t-10", "Sample 3: SWNILF in t-10")
coefs = c("pct_mw", "pct_mwt", "pct_mwnt", "pct_mwnilf")
coefnames = c("0) P[Married]", "1) P[Married Teacher]", "2) P[Married Non-Teacher in LF]", "3) P[Married Not in LF]")

modelcoefs = list()
modelses = list()
samplelabs = list()
outcomelabs = list()


# prepare data 
i = 1
for (dataind in 1:3){
  for (coefind in 1:4){
    model <- feols(as.formula(glue("{coefs[coefind]} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
                   data = datalist[[dataind]] %>% 
                     filter(STATEICP %in% c( 40, 47, 48, 51, 56)) %>%
                     filter(mainsamp == 1) %>% add_did_dummies(),
                   weight = ~nlink)
    modelcoefs[[i]] <- model$coeftable['TREATx1940','Estimate']
    modelses[[i]] <- model$coeftable['TREATx1940','Std. Error']
    samplelabs[[i]] <- datanames[dataind]
    outcomelabs[[i]] <- coefnames[coefind]
    i = i + 1 
  }
}

outdata <- data.frame(coefs = unlist(modelcoefs), 
                      ses = unlist(modelses),
                      sample = unlist(samplelabs), 
                      outcomevar = unlist(outcomelabs)) %>%
  mutate(ylb = coefs - ses*1.96,
         yub = coefs + ses*1.96)

ggplot(data = outdata,
       aes(x = outcomevar, y = coefs)) +
  geom_hline(yintercept = 0, alpha = 0.3) +
  geom_point(position = position_dodge(0.2)) +
  geom_errorbar(aes(ymin = ylb, ymax = yub), width = 0.1,position = position_dodge(0.2)) +
  facet_wrap(~sample, nrow = 3)
# outdata %>% pivot_wider(id_cols = sample, 
#                         names_from = outcomevar, 
#                         values_from = c(coefs, ses),
#                         names_vary = 'slowest') %>%
#   View()

# for (coefname in c("married_link", "mwt_link", "mwnt_link", "mwnilf_link")){
#   # for link 1
#   # models1[[i]] <- feols(as.formula(glue("{coefname} ~ Year1920+Year1940+TREATx1920+TREATx1940 + AGE_base+ I(AGE_base^2)| STATEICP^COUNTYICP")),
#   #                     weights = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1) %,
#   #                       data = link1 %>% 
#   #                         mutate(YEAR = YEAR_link, STATEICP = STATEICP_link, COUNTYICP = COUNTYICP_link,
#   #                                w = 1/nlink) %>% filter(neighbor_samp == 1 & mainsamp == 1 & move == 0) %>%
#   #                         add_did_dummies())
#   # #linkreg_means1 <- c(linkreg_means1, mean(filter(link1, YEAR == 1930 & TREAT == 1)[[coefname]]))
#   # 
#   # # # for link 2
#   # models2[[i]] <- feols(as.formula(glue("{coefname} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
#   #                       weights = ~ w,
#   #                       data = link2 %>% mutate(YEAR = YEAR_link, STATEICP = STATEICP_link, COUNTYICP = COUNTYICP_link,
#   #                                               w = 1/nlink) %>% filter(neighbor_samp == 1 & mainsamp == 1 & move == 0)%>% 
#   #                         add_did_dummies())
#   # #linkreg_means2 <- c(linkreg_means2, mean(filter(link2, YEAR == 1930 & TREAT == 1)[[coefname]]))
# 
#   # for link 3
#   models3[[i]] <- feols(as.formula(glue("{coefname} ~ Year1920+Year1940+TREATx1920+TREATx1940 | STATEICP^COUNTYICP")),
#                         weights = filter(link3%>% mutate(YEAR = YEAR_link, STATEICP = STATEICP_link, COUNTYICP = COUNTYICP_link,
#                                                   w = -nlink),neighbor_samp == 1 & mainsamp == 1 & move == 0)$w,
#                         data = link3%>% mutate(YEAR = YEAR_link, STATEICP = STATEICP_link, COUNTYICP = COUNTYICP_link,
#                                                w = -nlink)  %>% filter(neighbor_samp == 1 & mainsamp == 1 & move == 0)%>% 
#                           add_did_dummies())
#   #linkreg_means3 <- c(linkreg_means3, mean(filter(link3, YEAR == 1930 & TREAT == 1)[[coefname]]))
# 
#   i = i + 1
# }


## APPENDIX TABLE: GROUPS 2.5 and 3.5

## stargazer tables ----
models1app        <- list()
ses1app           <- list()
linkreg_means1app <- c()
models2app        <- list()
ses2app           <- list()
linkreg_means2app <- c()
# prepare data 
i = 1
for (coefname in c("pct_mw", "pct_mwt", "pct_mwnt", "pct_mwnilf")){
  # for link 1
  out_did <- did_graph_data(link2point5 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                            coefname, years = c(1920,1940), table = TRUE)
  models1app[[i]] <- out_did[[1]]
  ses1app[[i]] <- sqrt(diag(out_did[[2]]))
  linkreg_means1app <- c(linkreg_means1app, mean(filter(link2point5, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2
  out_did2 <- did_graph_data(link3point5 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models2app[[i]] <- out_did2[[1]]
  ses2app[[i]] <- sqrt(diag(out_did2[[2]]))
  linkreg_means2app <- c(linkreg_means2app, mean(filter(link3point5, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}

# gen tables
stargazer(models1app, se=ses1app, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_swnt_app.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1app))),
          table.layout = "=lc#-t-as=")

stargazer(models2app, se=ses2app, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregsmw_mwnt_app.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Married in t", "Married Teacher in t", 
                            "Married Non-Teacher in LF in t", 
                            "Married Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2app))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 3: DIRECT EFFECT ON MEN/UNMARRIED WOMEN ----
#______________________________________________________
# OUTCOME: SHARE TEACHERS MW/SW/M 
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          ymin        = -0.065, 
          ymax        = 0.06,
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("shareteach_neighbor"))  %>% print()


## stargazer table (neighbor sample only) ----
models         <- list()
ses            <- list()
sharereg_means <- c() #dep var mean in 1930
# prepare table inputs
i = 1
for (coefname in c("pct_mw_Teacher","pct_m_Teacher","pct_sw_Teacher", "num_Teacher")){
  out_did        <- did_graph_data(neighbor, coefname, table = TRUE) #returns list of [model, cov matrix]
  if (coefname == "pct_Teacher_mw_1000"){
    out_did <- did_graph_data(neighbor %>% mutate(weight = NWHITEMW), coefname, table = TRUE) #returns list of [model, cov matrix]
  }
  models[[i]]    <- out_did[[1]]
  ses[[i]]       <- sqrt(diag(out_did[[2]]))
  
  if (coefname == "pct_Teacher_mw_1000"){
    sharereg_means <- c(sharereg_means, weighted.mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]],filter(neighbor, YEAR == 1930 & TREAT == 1)$NWHITEMW))
  }
  else{
    sharereg_means <- c(sharereg_means, mean(filter(neighbor, YEAR == 1930 & TREAT == 1)[[coefname]]))
  }
  i = i + 1
}
# gen table
stargazer(models, se=ses, keep = c("TREATx1940", "TREATx1950"),#omit = c("Constant","cluster*", "factor*"), 
          out = glue("./tables/shareregs.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dependent Variable:",
          dep.var.labels.include = FALSE,
          column.labels = c("\\% Teach Mar. Wom.", "\\% Teach Men", 
                            "\\% Teach Unmar. Wom.","\\# Teachers"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)",
                               "Treated $\\times$ 1950 ($\\gamma_{1950}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Treated Mean", formatC(sharereg_means))),
          table.layout = "=lc#-t-as=")



#______________________________________________________
# RESULT 4: MECHANISMS FOR UNMARRIED WOMEN DECREASE ----
#______________________________________________________
# SAMPLE: SWT IN BASE YEAR; OUTCOME: SW x workingstatus
did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_swt", "pct_swnt", "pct_swnilf"),
          depvarnames = c("Unmarried Teacher in t", "Unmarr. Non-Teacher in LF in t", "Unmarr. Not in LF in t"),
          colors      = c(men_col, mw_col, sw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swt"))

did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_swwhitecollar", "pct_swbluecollar"),
          depvarnames = c("Unmarried Non-Teacher White Collar Occ. in t", "Blue Collar Occ. in t"),
          colors      = c(mw_col, "grey"),
          pointspan = 1,
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swt_inlf"))

link2combined <- link2 %>% filter(neighbor_samp == 1 & mainsamp == 1) %>% 
  select(c(YEAR, FIPS, STATEICP, COUNTYICP, pct_swt, TREAT)) %>% rename(pct_swt_link2 = pct_swt) %>%
  left_join(link2point5 %>% filter(neighbor_samp == 1 & mainsamp == 1) %>% 
              select(c(YEAR, FIPS, STATEICP, COUNTYICP, pct_swt, TREAT)) %>% rename(pct_swt_link2point5 = pct_swt))

did_graph(dataset     = link2combined,
          depvarlist  = c("pct_swt_link2", "pct_swt_link2point5"),
          depvarnames = c("Sample 2: Un", "unmarried 2 point 5"),
          colors      = c(mw_col, sw_col),
          pointspan = 1,
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("linked_swnilf_swt"))


## stargazer tables ----
models1        <- list()
ses1           <- list()
linkreg_means1 <- c()
models2        <- list()
ses2           <- list()
linkreg_means2 <- c()
models3        <- list()
ses3           <- list()
linkreg_means3 <- c()
# prepare data 
i = 1
for (coefname in c("pct_sw","pct_swt", "pct_swnt", "pct_swnilf")){
  # for link 1
  out_did <- did_graph_data(link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                            coefname, years = c(1920,1940), table = TRUE)
  models1[[i]] <- out_did[[1]]
  ses1[[i]] <- sqrt(diag(out_did[[2]]))
  linkreg_means1 <- c(linkreg_means1, mean(filter(link1, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2.5 (unmarried women non teachers in the labor force)
  out_did2 <- did_graph_data(link2point5 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models2[[i]] <- out_did2[[1]]
  ses2[[i]] <- sqrt(diag(out_did2[[2]]))
  linkreg_means2 <- c(linkreg_means2, mean(filter(link2point5, YEAR == 1930 & TREAT == 1)[[coefname]]))
  # for link 2 (unmarried women not in the labor force)
  out_did3 <- did_graph_data(link2 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
                             coefname, years = c(1920,1940), table = TRUE)
  models3[[i]] <- out_did3[[1]]
  ses3[[i]] <- sqrt(diag(out_did3[[2]]))
  linkreg_means3 <- c(linkreg_means3, mean(filter(link2, YEAR == 1930 & TREAT == 1)[[coefname]]))
  i = i + 1
}
# gen tables
stargazer(models1, se=ses1, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregssw_swt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Unmarried in t", "Unmarried Teacher in t", 
                            "Unmarried Non-Teacher in LF in t", 
                            "Unmarried Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means1))),
          table.layout = "=lc#-t-as=")

stargazer(models2, se=ses2, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregssw_swnilf.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Unmarried in t", "Unmarried Teacher in t", 
                            "Unmarried Non-Teacher in LF in t", 
                            "Unmarried Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means2))),
          table.layout = "=lc#-t-as=")

stargazer(models3, se=ses3, omit = c("Constant","cluster*", "factor*", "Year*", "TREATx1920"),
          out = glue("./tables/linkregssw_swnt.tex"),
          float = FALSE,
          keep.stat = c('n','adj.rsq'),
          dep.var.caption = "Dep. Var.: fill in by panel",
          dep.var.labels.include = FALSE,
          column.labels = c("Unmarried in t", "Unmarried Teacher in t", 
                            "Unmarried Non-Teacher in LF in t", 
                            "Unmarried Not in LF in t"),
          column.separate = c(1,1,1,1,1),
          covariate.labels = c("Treated $\\times$ 1940 ($\\gamma_{1940}^{DD}$)"),
          add.lines = list(c("Dep. Var. 1930 Mean", formatC(linkreg_means3))),
          table.layout = "=lc#-t-as=")


#______________________________________________________
# RESULT 5: NET EFFECTS ----
#______________________________________________________
# lfp results
did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_lf", "pct_mw_cond_inlf", "pct_sw_cond_inlf"),
          depvarnames = c("LFP in t", "LFP in t | Married in t", "LFP in t | Unmarried in t"),
          colors      = c("darkgrey", mw_col, sw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("overall_effects_lfp_neighbor"))

did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("pct_lf"),
          depvarnames = c("LFP in t"),
          colors      = c(mw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("overall_effects_lfp_uncond_neighbor"))

# occscore results
did_graph(dataset     = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1),
          depvarlist  = c("avg_occscore", "avg_occscore_mw", "avg_occscore_sw"),
          depvarnames = c("Mean Occupational Score in t", "Mean Occ. Score in t | Married in t", "Mean Occ. Score in t | Unmarried in t"),
          colors      = c("darkgrey", mw_col, sw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Unmarried Women Teachers in t-10"),
          verbose     = FALSE, #set to true to see regression coefficients at the very end of output stream
          filename    = glue("overall_effects_occscore"))

#______________________________________________________
# RESULT 6: FERTILITY ----
#______________________________________________________
# children
did_graph(dataset     = neighbor,
          depvarlist  = c("pctw_wc_Teacher"),
          depvarnames = c("At least one child"),
          colors      = c(mw_col),
          years       = c(1920, 1940),
          yvar        = glue("DiD Estimate: Share Women Teachers with Children"),
          filename = "sharewteach_children")

did_graph_data(dataset = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
               depvar = "pct_wc",
               years = c(1920, 1940))


did_graph(dataset = link1 %>% filter(neighbor_samp == 1 & mainsamp == 1), 
          depvarlist = "pct_wc", depvarnames = c("At least one child in t"), colors = c(mw_col),
          years = c(1920, 1940),
          yvar = "DiD Estimate: Share Unmarried Women Teachers in t-10",
          filename = "pct_swt_children")

# close log ----
sink()