## BORDER COUNTY ANALYSIS

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

countysumm_border <- countysumm %>% filter(FIPS %in% border_treat | FIPS %in% border_ctrl) %>%
  mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))

# countysumm_border <- countysumm %>% filter(TREAT == 1 | FIPS %in% border_ctrl) %>%
#   mutate(TREAT = ifelse(FIPS %in% border_ctrl, 0, 1))

graph_treatment(countysumm_border)

did_graph(dataset     = countysumm_border %>% filter(mainsamp == 1),
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers (Border Counties)",
          ymin        = -0.065, 
          ymax        = 0.06)


did_graph(dataset     = neighbor,
          depvarlist  = c("pct_m_Teacher", "pct_mw_Teacher", "pct_sw_Teacher"), 
          depvarnames = c("Men", "Married Women", "Single Women"),
          colors      = c(men_col, mw_col, sw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          ymin        = -0.065, 
          ymax        = 0.06)

