# temporary stuff 5/6

countysumm  <- read_csv(glue("{cleandata}/countysumm.csv")) # county X year-level stats (most analysis uses this)
countysumm_blk  <- read_csv(glue("{cleandata}/countysumm_blk.csv")) # county X year-level stats (most analysis uses this)
countysumm_all  <- read_csv(glue("{cleandata}/countysumm_all.csv")) # county X year-level stats (most analysis uses this)

# retail sales per capita use base population of 0.9* 1940 pop + 0.1*1930 pop
countysumm_pops <- countysumm %>% 
  select(YEAR, STATEICP, COUNTYICP, FIPS, AB, POP, RRTSAP29, RRTSAP39, RRTSAP33, RLDF3929, RLDF3329, RLDF3933, TREAT, neighbor_samp) %>%
  pivot_wider(id_cols = c(STATEICP, COUNTYICP, FIPS, AB, RRTSAP29, RRTSAP33, RRTSAP39, RLDF3929,  RLDF3329, RLDF3933, TREAT, neighbor_samp), 
              names_from = "YEAR", values_from = "POP") %>%
  mutate(denom29 = `1930` - 0.1*(`1940`-`1930`),
         retail_sales29 = RRTSAP29*denom29,
         denom39 = `1940`*0.9 + `1930`*0.1,
         retail_sales39 = RRTSAP39*denom39,
         denom33 = `1940`*0.3 + `1930`*0.7,
         retail_sales33 = RRTSAP33*denom33)

bystate <- countysumm_pops %>% filter(neighbor_samp == 1) %>% group_by(AB, STATEICP, TREAT) %>%
  summarize(across(c(denom29, retail_sales29, denom39, retail_sales39, denom33, retail_sales33), function(.x) sum(.x, na.rm=TRUE))) %>%
  mutate(rspc29 = retail_sales29/denom29,
         rspc39 = retail_sales39/denom39,
         rspc33 = retail_sales33/denom33,
         retail_sales_percap_growth_29_33 = log(rspc33)-log(rspc29),
         retail_sales_percap_growth_33_39 = log(rspc39) - log(rspc33))

bystate_long <- bystate %>% pivot_longer(cols = rspc29:rspc33, names_to = "yr", names_prefix = "rspc", values_to = 'rspc')

ggplot(data = bystate_long %>% mutate(yr = as.numeric(yr)), aes(x = yr, y = rspc, color = AB)) + geom_point() + geom_line()

countysumm_rs <- countysumm %>% left_join(countysumm_pops %>% select(c(STATEICP, COUNTYICP, FIPS, denom29, retail_sales29,
                                                                       denom39, retail_sales39, RRTSAP29, RRTSAP39, RLDF3929, RRTSAP33,
                                                                       RLDF3329, RLDF3933)))

countysumm_stats <- countysumm_rs %>%
  #filter(mainsamp == 1) %>% #main sample (all counties)
  mutate(POP_THOUS            = POP/1000, 
         WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
         STUDENT_PER_TEACH    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA),
         summgroup            = "All") %>%
  rbind(countysumm_rs %>% filter(SOUTH == 1) %>% #mainsamp == 1 & SOUTH == 1) %>%  # southern counties only
          mutate(POP_THOUS            = POP/1000, 
                 WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
                 STUDENT_PER_TEACH    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA), 
                 summgroup            = "South")) %>%
  rbind(countysumm_rs %>% filter(neighbor_samp == 1) %>% #mainsamp == 1 & neighbor_samp == 1) %>% #neighboring & treated counties (separately)
          mutate(POP_THOUS            = POP/1000, 
                 WHITESCHOOLPOP_THOUS = WHITESCHOOLPOP/1000, 
                 STUDENT_PER_TEACH    = ifelse(WHITESCHOOLPOP != 0, WHITESCHOOLPOP/num_Teacher, NA), 
                 summgroup            = ifelse(TREAT == 1, "Treated", "Neighb. Sth."))) %>%
  mutate(summgroup = factor(summgroup, levels = c("All", "South", "Treated", "Neighb. Sth.")))

varnames_1930 = c("POP_THOUS","WHITESCHOOLPOP_THOUS", "URBAN", 
                  "LFP_MW", "LFP_WMW", "NCHILD", "RLDF3929", "RRTSAP29", "RRTSAP33", "RRTSAP39", "RLDF3933", "RLDF3329",
                  "STUDENT_PER_TEACH","pct_m_Teacher", 
                  "pct_sw_Teacher", "pct_mw_Teacher")
varlabs_1930 = c("Population (Thous.)", "White School-Age Pop. (Thous.)", "Share Urban", 
                 "LFP of Married Women", "LFP of White Married Women", "Num. Children of Marr. Wom.", 
                 "Retail Sales Growth 29-39", "Retail Sales Level 29", "Retail sales level 34", "retail sales level 39",
                 "retail sales growth 33-39", "retail sales growth 29-33",
                 "Students/Teachers", "Share Men", 
                 "Share Single Women", "Share Married Women")

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

# summary statistics by state group
summ_stats <- countysumm_stats %>%
  filter(YEAR == 1930 & (FIPS %in% border_treat | FIPS %in% border_ctrl)) %>%
  group_by(summgroup) %>% 
  summarize(OBS = n(),
            across(all_of(varnames_1930), .fns = c(~mean(.x, na.rm=TRUE), #mean
                                                   ~sd(.x, na.rm=TRUE)/sqrt(OBS))))

summ_stats_out <- as.data.frame(t(summ_stats))


neighbor <- countysumm_rs %>% filter(neighbor_samp == 1 & mainsamp == 1)
did_graph(dataset     = neighbor, 
          depvarlist  = c("pct_mw_Teacher"),
          depvarnames = c("Married Women"),
          controls = "+ URBAN + RLDF3929 +RRTSAP33",
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          pointtypes = c(17),
          ymin = -0.015, ymax = 0.06)

neighboralt <- countysumm_rs %>% filter(SOUTH == 1 & mainsamp == 1)
did_graph(dataset     = neighboralt, 
          depvarlist  = c("pct_mw_Teacher"),
          depvarnames = c("Married Women"),
          controls = "+ URBAN + RLDF3929",
          colors      = c(mw_col),
          yvar        = "DiD Estimate: Share of Teachers",
          pointtypes = c(17),
          ymin = -0.015, ymax = 0.06)
