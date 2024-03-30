## TESTING FASTER DATA ANALYSIS WITH DUCKDB
library(duckdb)

# creating connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))

# reading in all ipums census data to create raw tables by year
for (year in seq(1900,1940,10)){
  dbExecute(con, glue("CREATE OR REPLACE TABLE censusraw{year} AS FROM read_csv_auto ('{rawdata}/census_{year}.csv')"))
  dbExecute(con, glue("ALTER TABLE censusraw{year} ADD COLUMN YEAR INTEGER"))
  dbExecute(con, glue("UPDATE censusraw{year} SET YEAR = {year}"))
}

# concatenating into one table
dbExecute(con, paste("CREATE OR REPLACE TABLE censusrawall AS",
                     "SELECT * FROM censusraw1900 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1910 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1920 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1930 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1940"))

link_colnames <- c("YEAR","SERIAL","STATEICP", "COUNTYICP", "SEX", "AGE", "MARST", "RACE", "LABFORCE", "CLASSWKR", "OCC1950")
# reading in all census tree linking files and creating linked tables
for (linkyear in seq(1910,1940,10)){
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE link{linkyear} AS FROM read_csv_auto('{rawdata}/{linkyear-10}_{linkyear}.csv')"))
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE linkedtemp{linkyear} AS SELECT * FROM censusraw{linkyear - 10} JOIN link{linkyear} ON (HISTID = histid{linkyear - 10})"))
  dbExecute(con, glue("CREATE OR REPLACE TABLE linked{linkyear} AS SELECT * FROM linkedtemp{linkyear} linkedtemp{linkyear} JOIN censusraw{linkyear} ON (histid{linkyear} = censusraw{linkyear}.HISTID)"))
  for (colname in link_colnames){
    if (linkyear != 1910 | !(colname %in% c("LABFORCE", "CLASSWKR"))){ #dropping missing variables in 1900
      dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN {colname} TO {colname}_base"))
      dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN '{colname}:1' TO {colname}_link"))
    }
  }
}

# concatenating into one table
dbExecute(con, paste("CREATE OR REPLACE TABLE linkedall AS",
                     "SELECT * FROM linked1920 UNION ALL BY NAME",
                     "SELECT * FROM linked1930 UNION ALL BY NAME",
                     "SELECT * FROM linked1940"))

dbGetQuery(con, "DESCRIBE linkedall")

## CREATING VIEWS
dbExecute(con, "CREATE OR REPLACE VIEW filteredbind AS SELECT * FROM censusrawall WHERE OCC1950 == 93 OR OCC1950 == 350")
dbExecute(con, "CREATE OR REPLACE VIEW linkedteach AS SELECT * FROM linkedall WHERE OCC1950_base == 93 OR OCC1950_link == 93 OR OCC1950_base == 350 OR OCC1950_link == 350")
dbExecute(con, "CREATE OR REPLACE VIEW filteredbind AS SELECT * FROM censusrawall WHERE OCC1950 == 93 OR OCC1950 == 350")
dbExecute(con, "CREATE OR REPLACE VIEW filteredbind AS SELECT * FROM censusrawall WHERE OCC1950 == 93 OR OCC1950 == 350")


## FILTERING INTO DATAFRAMES (note: need to fix years first)
# all census years -- filtering on occupation (only teachers and secretaries)
teacher_sample <- tbl(con, "censusrawall") %>% filter(OCC1950 == 93 | OCC1950 == 350) %>% as.data.frame()
# linked sample -- filtering on occupation (only teachers and secretaries)
linked_teach <- tbl(con, "linkedall") %>% filter(OCC1950_base == 93 | OCC1950_link == 93 | OCC1950_base == 350 | OCC1950_link == 350) %>% as.data.frame()
# linked sample -- filtering to only young single women (in base year)
linked_sw <- tbl(con, "linkedall") %>% filter(SEX_base == 2 & MARST_base != 1 & MARST_base != 2 & AGE_base >= 15 & AGE_base <= 33)
# linked sample -- filtering to only married women under age 50 (in base year)
linked_mw <- tbl(con, "linkedall") %>% filter(SEX_base == 2 & (MARST_base == 1 | MARST_base == 2) & AGE_base < 55)

## LINKED INDIV MANUALLY FOR NOW
linked_teach_list = list()
linked_sw_list = list()
i = 1
for (linkyear in seq(1920,1940,10)){
  linked_teach_list[[i]] <- tbl(con,glue("linked{linkyear}")) %>% filter(OCC1950_base == 93 | OCC1950_link == 93 | OCC1950_base == 350 | OCC1950_link == 350) %>%
    as.data.frame() %>% mutate(YEAR_base = linkyear - 10, YEAR_link = linkyear)
  linked_sw_list[[i]] <- tbl(con, glue("linked{linkyear}")) %>% filter(AGE_base <= 33 & AGE_base >= 15 & SEX_base == 2 & MARST_base != 1 & MARST_base != 2 & OCC1950_base != 93) %>%
    as.data.frame() %>% mutate(YEAR_base = linkyear - 10, YEAR_link = linkyear)
  i = i + 1
}
linked_teach <- bind_rows(linked_teach_list) %>%
  mutate(teacher_base = ifelse(OCC1950_base == 93 & CLASSWKR_base == 2 & AGE_base >= 18 & AGE_base <= 64,1,0),
         teacher_link = ifelse(OCC1950_link == 93 & CLASSWKR_link == 2 & AGE_link >= 18 & AGE_link <= 64,1,0),
         sec_base = ifelse(OCC1950_base == 350 & CLASSWKR_base == 2 & AGE_base >= 18 & AGE_base <= 64,1,0),
         sec_link = ifelse(OCC1950_link == 350 & CLASSWKR_link == 2 & AGE_link >= 18 & AGE_link <= 64,1,0),
         worker_base =  ifelse(LABFORCE_base == 2 & AGE_base >= 18 & AGE_base <= 64, 1, 0),
         worker_link =  ifelse(LABFORCE_link  == 2 & AGE_link  >= 18 & AGE_link  <= 64, 1, 0),
         demgroup_base = case_when(SEX_base == 1 ~ "M",
                                   SEX_base == 2 & MARST_base != 1 & MARST_base != 2 ~ "SW",
                                   TRUE ~ "MW"),
         demgroup_link = case_when(SEX_link == 1 ~ "M",
                                   SEX_link == 2 & MARST_link != 1 & MARST_link != 2 ~ "SW",
                                   TRUE ~ "MW"))

write_csv(linked_teach, glue("{outdata}/linkedteach.csv"))

linked_sw <- bind_rows(linked_sw_list) %>%
  mutate(teacher_base = ifelse(OCC1950_base == 93 & CLASSWKR_base == 2 & AGE_base >= 18 & AGE_base <= 64,1,0),
         teacher_link = ifelse(OCC1950_link == 93 & CLASSWKR_link == 2 & AGE_link >= 18 & AGE_link <= 64,1,0),
         sec_base = ifelse(OCC1950_base == 350 & CLASSWKR_base == 2 & AGE_base >= 18 & AGE_base <= 64,1,0),
         sec_link = ifelse(OCC1950_link == 350 & CLASSWKR_link == 2 & AGE_link >= 18 & AGE_link <= 64,1,0),
         worker_base =  ifelse(LABFORCE_base == 2 & AGE_base >= 18 & AGE_base <= 64, 1, 0),
         worker_link =  ifelse(LABFORCE_link  == 2 & AGE_link  >= 18 & AGE_link  <= 64, 1, 0),
         demgroup_base = case_when(SEX_base == 1 ~ "M",
                                   SEX_base == 2 & MARST_base != 1 & MARST_base != 2 ~ "SW",
                                   TRUE ~ "MW"),
         demgroup_link = case_when(SEX_link == 1 ~ "M",
                                   SEX_link == 2 & MARST_link != 1 & MARST_link != 2 ~ "SW",
                                   TRUE ~ "MW"))

write_csv(linked_sw, glue("{outdata}/linkedsw.csv"))

## EDUCATION LINKED SAMPLE
educ_linked <- tbl(con, "linked1940") %>% filter(EDUC >= 7) %>% as.data.frame()
