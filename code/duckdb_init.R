### INITIAL CREATION OF DUCKDB DATABASES
### Only edit/rerun if importing new raw data (e.g. additional years of census, new linking, etc.)
### AUTHOR: AMY KIM

## note: all code is edited to exclude:
## (1) 1900 data, everywhere
## (2) 1910 data, in the linked data only

# open log ----
sink("./logs/log_duckdb_init.txt", append=FALSE)

# creating connection to duckdb database ----
con <- dbConnect(duckdb(), dbdir = glue("{root}/db.duckdb"))
#for Carolyn, in case connection on Dropbox isn't working:
#con <- dbConnect(duckdb(), dbdir = "C:\\Users\\ctsao\\Documents\\test_duckdb/db.duckdb", read_only=FALSE) 

#________________________________________________________
# IPUMS CENSUS RAW FILES ----
#________________________________________________________
for (year in seq(1910,1950,10)){
  # reading in census CSV and creating/replacing initial table
  tic(glue("Create or replace table censusraw{year}..."))
  dbExecute(con, glue("CREATE OR REPLACE TABLE censusraw{year} AS 
                        FROM read_csv_auto ('{rawdata}/census_{year}.csv')"))
  toc()
  
  # creating temporary spouse database
  tic(glue("Create temp spouse database for censusraw{year}..."))
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE censusrawspouse{year} AS
                        SELECT SERIAL, PERNUM AS SPLOC, 
                          OCCSCORE AS OCCSCORE_SP, OCC1950 AS OCC1950_SP, 
                          AGE AS AGE_SP, RACE AS RACE_SP, SEX AS SEX_SP,
                          CLASSWKR AS CLASSWKR_SP, LABFORCE AS LABFORCE_SP
                        FROM censusraw{year}"))
  toc()

  # spouse linking: left join of census raw with itself by serial (same household id) 
  # and by spousenum = personnum (spousenum ids the person number of one's spouse)
  tic(glue("Link spouses for censusraw{year}..."))
  dbExecute(con, glue("CREATE OR REPLACE TABLE censusraw{year} AS 
                        SELECT * FROM censusraw{year} 
                          LEFT JOIN censusrawspouse{year} 
                          USING (SERIAL, SPLOC)"))
  toc()
  
  # TEMP FIX 20240206
  if({year}==1950) {
    dbExecute(con, "ALTER TABLE censusraw1950 DROP COLUMN YEAR")
  }
  # editing to include year (delete if IPUMS extract includes variable YEAR)
  dbExecute(con, glue("ALTER TABLE censusraw{year} ADD COLUMN YEAR INTEGER"))
  dbExecute(con, glue("UPDATE censusraw{year} SET YEAR = {year}"))
}

# concatenating all census years into one table
tic(glue("Concatenate all census years together..."))
dbExecute(con, paste("CREATE OR REPLACE TABLE censusrawall AS",
                     #"SELECT * FROM censusraw1900 UNION ALL BY NAME", ##! Shall we remove this?
                     "SELECT * FROM censusraw1910 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1920 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1930 UNION ALL BY NAME",
                     "SELECT * FROM censusraw1940 UNION ALL BY NAME", 
                     "SELECT * FROM censusraw1950"))
toc()

#________________________________________________________
# CENSUSTREE LINKING ----
#________________________________________________________
# names of columns we want to keep in linked dataset
link_colnames <- c("YEAR","STATEICP", "COUNTYICP", 
                   "SEX", "AGE", "MARST", "RACE", 
                   "LABFORCE", "CLASSWKR", "OCC1950")

# linking baseyear (year t - 10) to linkyear (year t)
for (linkyear in seq(1920,1940,10)){
  # reading in censustree linking file
  tic(glue("Create or replace CensusTree linking file for link year {linkyear}..."))
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE link{linkyear} AS 
                        FROM read_csv_auto('{rawdata}/{linkyear-10}_{linkyear}.csv')"))
  toc()
  
  # inner join of linking file with baseyear census data (only keeping links) as temporary table
  tic(glue("Merge CensusTree linking file to base year {linkyear-10}..."))
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE linkedtemp{linkyear} AS 
                        SELECT * FROM censusraw{linkyear - 10} 
                        JOIN link{linkyear} ON (HISTID = histid{linkyear - 10})"))
  toc()

  # inner join of linking file + baseyear census with linkyear census data (only keeping links)
  tic(glue("Merge CensusTree linking file to link year {linkyear}..."))
  dbExecute(con, glue("CREATE OR REPLACE TABLE linked{linkyear} AS 
                        SELECT * FROM linkedtemp{linkyear} linkedtemp{linkyear} 
                        JOIN censusraw{linkyear} ON (histid{linkyear} = censusraw{linkyear}.HISTID)"))
  toc()

  # renaming (kind of hack-y, should change if can find better solution)
  #   when joining two tables with duplicate column names (e.g. YEAR), 
  #   column version in first table kept as YEAR and second version 
  #   renamed to YEAR:1 -- ideally, want to change so first version
  #   (base year) is YEAR_base, second version (link year) is YEAR_link
  for (colname in link_colnames){
    if (linkyear != 1910 | !(colname %in% c("LABFORCE", "CLASSWKR"))){ #dropping missing variables in 1900 ##! 1910?
      dbExecute(con, glue("ALTER TABLE linked{linkyear} 
                            RENAME COLUMN {colname} TO {colname}_base"))
      dbExecute(con, glue("ALTER TABLE linked{linkyear} 
                            RENAME COLUMN '{colname}:1' TO {colname}_link"))
    }
    else{
      dbExecute(con, glue("ALTER TABLE linked{linkyear} 
                            RENAME COLUMN {colname} TO {colname}_link")) # don't have labforce and classwkr in 1900
    }
  }
}

# concatenating into one table
tic(glue("Concatenating linked datasets..."))
dbExecute(con, paste("CREATE OR REPLACE TABLE linkedall AS",
                     #"SELECT * FROM linked1910 UNION ALL BY NAME",
                     "SELECT * FROM linked1920 UNION ALL BY NAME",
                     "SELECT * FROM linked1930 UNION ALL BY NAME",
                     "SELECT * FROM linked1940"))
toc()

# closing database ----
dbDisconnect(con, shutdown = TRUE)
# close log ----
sink()
