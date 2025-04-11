## TEST: STAGGERED DID WITH FISHER TEST
neighbor_alt   <- countysumm %>% filter(neighbor_samp == 1 & mainsamp == 1) %>%
  mutate(firsttreat = ifelse(TREAT == 1, 4, 10),
         YEAR = case_when(YEAR == 1910 ~ 1,
                          YEAR == 1920 ~ 2,
                          YEAR == 1930 ~ 3,
                          YEAR == 1940 ~ 4,
                          YEAR == 1950 ~ 5),
         FIPS = as.numeric(FIPS))

staggered(neighbor_alt %>% select(c(FIPS, YEAR, firsttreat, pct_mw_Teacher)), 
          i = "FIPS", t = "YEAR",  g = "firsttreat", y = "pct_mw_Teacher", 
          estimand = "simple",
          compute_fisher = T)

## SUN AND ABRAHAM
library(fixest)
x <- summary(lm(data = neighbor_alt, pct_mw_Teacher ~ factor(YEAR)*TREAT + factor(FIPS)))
summary(feols(pct_mw_Teacher ~ factor(YEAR)*TREAT | factor(FIPS), data = neighbor_alt))

summary(feols(pct_mw_Teacher ~ sunab(firsttreat, YEAR) | factor(FIPS) + factor(YEAR), data = neighbor_alt))
did_graph_data(neighbor_alt, "pct_mw_Teacher")

staggered(data.frame(i = neighbor$FIPS, t = neighbor$YEAR, g = neighbor$firsttreat, y = neighbor$pct_mw_Teacher), estimand = "simple")

staggered(df %>% mutate(first_trained = ifelse(first_trained == 25, 25, Inf)), 
          i = "uid", t = "period", g = "first_trained", y = "complaints", estimand = "simple")

library(fwildclusterboot)

regdata <- neighbor %>% 
  add_did_dummies() %>% 
  filter(YEAR %in% seq(1910,1950,10))

years <- c(1910, 1920, 1940, 1950)
interact_vars <- glue("TREATx{years}")
yearvars <- glue("Year{years}")

# run reg: include year and county FE + interaction terms + any controls
did_reg <- lm(glue("pct_mw_Teacher ~ {glue_collapse(yearvars, sep = '+')} + factor(FIPS) + 
                       {glue_collapse(interact_vars, sep = '+')}"), 
              data = regdata)

x <- boottest(did_reg, B = 9999, param = "TREATx1940", clustid = "FIPS")

