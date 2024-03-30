# Marriage bar inference from full-count census data
library(haven)
library(tidyverse)
library(glue)

path = "/Users/amykim/Dropbox (Princeton)/marriage_bars"
# 
# raw1900 <- read_csv()
# raw1920 <- read_csv(glue("{path}/census_1920.csv"))
# raw1930 <- read_csv(glue("{path}/census_1930.csv"))
# raw1940 <- read_csv(glue("{path}/census_1940.csv"))
# 
# # keeping only teachers and combining all years
# teachers_raw <- raw1920 %>% group_by(CITY) %>% mutate(POP = n()) %>% 
#   filter(OCC1950 == 93) %>% mutate(AGEMARR = NA) %>% 
#   rbind(filter(raw1930 %>% group_by(CITY) %>% mutate(POP = n()), OCC1950 == 93)) %>%
#   mutate(INCWAGE = NA) %>% 
#   rbind(filter(raw1940 %>% group_by(CITY) %>% mutate(POP = n()), OCC1950 == 93))

allyearsteach <- list()
allyearssec <- list()
i = 0
for (year in seq(1900,1940,10)){
  i = i + 1
    raw <- read_csv(glue("{path}/census_{year}.csv"))
    teachers <- raw %>% group_by(CITY) %>% mutate(POP = n()) %>% ungroup() %>% 
      filter(OCC1950 == 93) %>% select(c(YEAR, CITY, COUNTYICP, STATEICP, AGE, MARST, SEX, RACE, POP))
    secretaries <- raw %>% group_by(CITY) %>% mutate(POP = n()) %>% ungroup() %>% 
      filter(OCC1950 == 350) %>% select(c(YEAR, CITY, COUNTYICP, STATEICP, AGE, MARST, SEX, RACE, POP))
    allyearsteach[[i]] <- teachers
    allyearssec[[i]] <- secretaries
}

teachers_raw <- bind_rows(allyearsteach, .id = "column_label")
sec_raw <- bind_rows(allyearssec, .id = "column_label")
write_csv(teachers_raw, glue("{path}/teachers_raw.csv"))

# determining marriage bar status by city

#teachers_raw <-read_csv(glue("{path}/teachers_raw.csv"))
teachers_grp <- teachers_raw %>% filter(CITY != 0) %>% #taking out if unidentifiable city
  filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
  group_by(YEAR, CITY, SEX, STATEICP) %>%
  summarize(count = n(),
            youngct = sum(ifelse(AGE <= 28, 1, 0)),
            pct_single = sum(ifelse(MARST == 6, 1, 0))/count,
            pct_young_single = sum(ifelse(MARST == 6 & AGE <= 28, 1, 0))/youngct,
            avgage = mean(AGE, na.rm=TRUE),
            occ = "Teacher") #%>%
    #pivot_wider(id_cols = c(YEAR, CITY), names_from = SEX, values_from = c(count, pct_single))

secretaries_grp <- sec_raw %>% filter(CITY != 0) %>% #taking out if unidentifiable city
  filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
  group_by(YEAR, CITY, SEX, STATEICP) %>%
  summarize(count = n(),
            youngct = sum(ifelse(AGE <= 28, 1, 0)),
            pct_single = sum(ifelse(MARST == 6, 1, 0))/count,
            pct_young_single = sum(ifelse(MARST == 6 & AGE <= 28, 1, 0))/youngct,
            avgage = mean(AGE, na.rm=TRUE),
            occ = "Secretary") #%>%

all_grp <- rbind(teachers_grp, secretaries_grp)

# graphing dist of pct of female/male teachers/secretaries who are unmarried in a city, by year
marbar_plot <- ggplot(teachers_grp, aes(x = pct_single, group = factor(SEX), color = factor(SEX))) + 
  geom_density() + 
  facet_wrap(~YEAR)

sec_plot <- ggplot(all_grp %>% filter(SEX == 2), aes(x = pct_single, group = factor(occ), color = factor(occ))) + 
  geom_density() + 
  facet_wrap(~YEAR)
 

# marbar_young_plot <- ggplot(teachers_grp, aes(x = pct_young_single, group = factor(SEX), color = factor(SEX))) + 
#   geom_density() + 
#   facet_wrap(~YEAR)

marbar_mo_plot <- ggplot(teachers_grp %>% filter(STATEICP == 34), 
                         aes(x = pct_single, group = factor(SEX), color = factor(SEX))) + 
  geom_density() + 
  facet_wrap(~YEAR)

marbar_wv_plot <- ggplot(teachers_grp %>% filter(STATEICP == 56), 
                         aes(x = pct_single, group = factor(SEX), color = factor(SEX))) + 
  geom_density() + 
  facet_wrap(~YEAR)

# grouped by city year
nomarbn1914 <- c(1190, 4630, 3730, 4150, 2990, 1710, 6970, 4930, 2530, 4530)
cityyear_grp <- teachers_grp %>% group_by(YEAR, CITY, STATEICP) %>% 
  summarize(pct_female = sum(ifelse(SEX == 2, count, 0))/sum(count),
            pct_single_f = sum(ifelse(SEX == 2, pct_single, 0)),
            pct_young_single_f = sum(ifelse(SEX == 2, pct_young_single, 0)),
            count = sum(count))

cityyear_all <- all_grp %>% group_by(YEAR, CITY, STATEICP, occ) %>%
  summarize(pct_female = sum(ifelse(SEX == 2, count, 0))/sum(count),
            pct_single_f = sum(ifelse(SEX == 2, pct_single, 0)),
            pct_young_single_f = sum(ifelse(SEX == 2, pct_young_single, 0)),
            count = sum(count))

cityyear_wide <- cityyear_all %>% pivot_wider(id_cols = c(YEAR, CITY, STATEICP), names_from = occ, values_from = c(pct_female, pct_single_f))
teach_sec_scatter <- ggplot(cityyear_wide, aes(x = pct_single_f_Teacher, y = pct_single_f_Secretary)) + geom_point() + geom_smooth(method = "lm") + facet_wrap(~YEAR)

gender_plot <- ggplot(cityyear_grp, aes(x = pct_female)) + geom_density() + facet_wrap(~YEAR) 

gender_mo_plot <- ggplot(cityyear_grp %>% filter(STATEICP == 34), aes(x = pct_female)) + geom_density() + facet_wrap(~YEAR) 

gender_wv_plot <- ggplot(cityyear_grp %>% filter(STATEICP == 56), aes(x = pct_female)) + geom_density() + facet_wrap(~YEAR) 

# grouped by city
city_grp <- cityyear_grp %>% pivot_wider(id_cols = CITY, names_from = YEAR, values_from = c(pct_female, pct_single_f, popgroup)) %>%
  mutate(pct_female_diff = pct_female_1940 - pct_female_1920,
         pct_single_diff = pct_single_f_1940 - pct_single_f_1920)

female_single_diffs <- ggplot(city_grp, aes(x = pct_female_diff, y = pct_single_diff, group = factor(popgroup_1940), color = factor(popgroup_1940))) +
  geom_point()

female_single_diffs_largepop <- ggplot(city_grp %>% filter(popgroup_1940 <= 2), aes(x = pct_single_diff, y = pct_female_diff, group = factor(popgroup_1940), color = factor(popgroup_1940))) +
  geom_point() + geom_smooth()

# grouped by year
year_grp <- teachers_grp %>% 
  filter(SEX == 2) %>%
  #pivot_wider(id_cols = c(YEAR, CITY, POP), names_from = SEX, values_from = c(count, pct_single)) %>%
  mutate(marbar = ifelse(pct_single >= 0.85, 1, 0),
         marbaryoung = ifelse(pct_young_single >= 0.9, 1, 0)) %>% #grouping into population categories
  group_by(YEAR, popgroup) %>%
  summarize(pct_marbar = sum(marbar)/n(),
            pct_marbaryoung = sum(marbaryoung)/n(),
            count = n())


year_grp2 <- teachers_grp %>% 
  filter(SEX == 2) %>%
  #pivot_wider(id_cols = c(YEAR, CITY, POP), names_from = SEX, values_from = c(count, pct_single)) %>%
  mutate(marbar = ifelse(pct_single >= 0.87, 1, 0),
         marbaryoung = ifelse(pct_young_single >= 0.9, 1, 0)) %>% #grouping into population categories
  group_by(YEAR) %>%
  summarize(pct_marbar = sum(marbar)/n(),
            pct_marbaryoung = sum(marbaryoung)/n(),
            count = n())

# targets: in 1927-8, ~61% of grp 1 cities had marriage bars; 

# county-level grouping 
teachers_county <- teachers_raw %>% 
  filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
  group_by(YEAR, STATEICP, COUNTYICP) %>% 
  summarize(num_teach = n(),
            pct_female_teach = sum(ifelse(SEX == 2, 1, 0))/num_teach,
            pct_female_single_teach = sum(ifelse(SEX == 2 & MARST == 6, 1, 0))/sum(ifelse(SEX == 2, 1, 0)))

sec_county <- sec_raw %>% 
  filter(MARST != 3 & MARST != 4 & MARST != 5) %>% # taking out those who are divorced/widowed/separated for now
  group_by(YEAR, STATEICP, COUNTYICP) %>% 
  summarize(num_sec = n(),
            pct_female_sec = sum(ifelse(SEX == 2, 1, 0))/num_sec,
            pct_female_single_sec = sum(ifelse(SEX == 2 & MARST == 6, 1, 0))/sum(ifelse(SEX == 2, 1, 0)))

all_county <- inner_join(teachers_county, sec_county, by = c("YEAR", "STATEICP", "COUNTYICP")) %>%
  mutate(STATEGROUP = ifelse(STATEICP %in% c(34, 56, 48, 54, 24, 22, 21), 2, ifelse(STATEICP == 47 | STATEICP == 51, 1, 0))) %>%
  filter(!is.na(pct_female_single_sec) & !is.na(pct_female_single_teach))


#### GRAPHING HISTOGRAMS OF SHARE OF FEMALE TEACHERS WHO WERE SINGLE ####
marbar_plot <- ggplot(all_county, aes(x = pct_female_single_teach)) + 
  geom_density() + 
  facet_wrap(~YEAR)

marbar_plot_group <- ggplot(all_county %>% filter(STATEGROUP != 0), aes(x = pct_female_single_teach, group = factor(STATEGROUP), color = factor(STATEGROUP))) + 
  geom_density() + 
  facet_wrap(~YEAR)

# # comparing w secretaries -- TO FIX
# sec_plot <- ggplot(all_county %>% pivot_longer(cols = c(COUNTYICP, STATEICP), names_to), 
#                    aes(x = pct_single, group = factor(occ), color = factor(occ))) + 
#   geom_density() + 
#   facet_wrap(~YEAR)


##### HISTOGRAMS OF RESIDUALS -- TEACHERS ON SECRETARIES ###
# residuals
allresids <- list()
i = 0
for (year in seq(1900,1940,10)){
  i = i + 1
  datayear <- all_county %>% filter(YEAR == year)
  sec_teach <- lm(data = datayear, pct_female_single_teach ~ pct_female_single_sec)
  datayear$resids <- sec_teach$residuals
  sec_teach_fe <- lm(data = datayear, pct_female_single_teach ~ pct_female_single_sec + factor(STATEICP))
  datayear$resids_fe <- sec_teach_fe$residuals
  allresids[[i]] <- datayear
}

# plotting residuals
allresids <- bind_rows(allresids)
teach_resid_plot <- ggplot(data = allresids, aes(x = resids)) + geom_density() + facet_wrap(~YEAR)

teach_resid_plot_group <- ggplot(data = allresids %>% filter(STATEGROUP != 0), 
                                 aes(x = resids, group = factor(STATEGROUP), color = factor(STATEGROUP))) + 
  geom_density() + facet_wrap(~YEAR)

### DIFF IN DIFF
did_data <- all_county %>% filter(STATEGROUP != 0) %>% 
  mutate(k = factor(YEAR - 1930),
         treat = ifelse(STATEGROUP == 1, 1, 0))

did_reg <- lm(pct_female_single_teach ~ factor(YEAR) + treat*k, data = did_data)





