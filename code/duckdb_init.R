### INITIAL CREATION OF DUCKDB DATABASES
### Only edit/rerun if importing new raw data (e.g. additional years of census, new linking, etc.)
### AUTHOR: AMY KIM

# creating connection to duckdb database
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))

## IPUMS CENSUS RAW FILES
for (year in seq(1900,1940,10)){
  # reading in census CSV and creating/replacing initial table
  dbExecute(con, glue("CREATE OR REPLACE TABLE censusraw{year} AS FROM read_csv_auto ('{rawdata}/census_{year}.csv')"))
  
  # editing to include year (delete if IPUMS extract includes variable YEAR)
  dbExecute(con, glue("ALTER TABLE censusraw{year} ADD COLUMN YEAR INTEGER"))
  dbExecute(con, glue("UPDATE censusraw{year} SET YEAR = {year}"))
}

# concatenating all census years into one table
dbExecute(con, paste("CREATE OR REPLACE TABLE censusrawall AS",
                     "SELECT * FROM censusraw1900 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1910 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1920 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1930 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1940"))

## CENSUSTREE LINKING
# names of columns we want to keep in linked dataset
link_colnames <- c("YEAR","STATEICP", "COUNTYICP", "SEX", "AGE", "MARST", "RACE", "LABFORCE", "CLASSWKR", "OCC1950")

# linking baseyear (year t - 10) to linkyear (year t)
for (linkyear in seq(1910,1940,10)){
  # reading in censustree linking file
  dbExecute(con, glue("CREATE OR REPLACE TABLE link{linkyear} AS FROM read_csv_auto('{rawdata}/{linkyear-10}_{linkyear}.csv')"))
  
  # # renaming histid{year} to format histid_base and histid_link
  # dbExecute(con, glue("ALTER TABLE link{linkyear} RENAME COLUMN histid{linkyear-10} TO histid_base"))
  # dbExecute(con, glue("ALTER TABLE link{linkyear} RENAME COLUMN histid{linkyear} TO histid_link"))
  # 
  # # editing to include base and link year
  # dbExecute(con, glue("ALTER TABLE link{linkyear} ADD COLUMN YEAR_base INTEGER"))
  # dbExecute(con, glue("ALTER TABLE link{linkyear} ADD COLUMN YEAR_link INTEGER"))
  # dbExecute(con, glue("UPDATE link{linkyear} SET YEAR_base = {linkyear - 10}"))
  # dbExecute(con, glue("UPDATE link{linkyear} SET YEAR_link = {linkyear}"))
  
  # inner join of linking file with baseyear census data (only keeping links) as temporary table
  dbExecute(con, glue("CREATE OR REPLACE TABLE linkedtemp{linkyear} AS SELECT * FROM censusraw{linkyear - 10} JOIN link{linkyear} ON (HISTID = histid{linkyear - 10})"))

  # inner join of linking file + baseyear census with linkyear census data (only keeping links)
  dbExecute(con, glue("CREATE OR REPLACE TABLE linked{linkyear} AS SELECT * FROM linkedtemp{linkyear} linkedtemp{linkyear} JOIN censusraw{linkyear} ON (histid{linkyear} = censusraw{linkyear}.HISTID)"))

  # renaming (kind of hack-y, should change if can find better solution)
  #   when joining two tables with duplicate column names (e.g. YEAR), column version in first table kept as YEAR and second version renamed to YEAR:1 -- want to change so first version
  #   (base year) is YEAR_base, second version (link year) is YEAR_link
  for (colname in link_colnames){
    if (linkyear != 1910 | !(colname %in% c("LABFORCE", "CLASSWKR"))){ #dropping missing variables in 1900
      dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN {colname} TO {colname}_base"))
      dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN '{colname}:1' TO {colname}_link"))
    }
    else{
      dbExecute(con, glue("ALTER TABLE linked{linkyear} RENAME COLUMN {colname} TO {colname}_link")) # dont have labforce and classwkr in 1900
    }
  }
}

# concatenating into one table
dbExecute(con, paste("CREATE OR REPLACE TABLE linkedall AS",
                     "SELECT * FROM linked1910 UNION ALL BY NAME",
                     "SELECT * FROM linked1920 UNION ALL BY NAME",
                     "SELECT * FROM linked1930 UNION ALL BY NAME",
                     "SELECT * FROM linked1940"))

# closing database
dbDisconnect(con, shutdown = TRUE)