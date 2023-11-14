### HELPER FUNCTIONS FOR MARRIAGEBAR PROJECT ###

## DATA HELPERS
# Takes a dataset with variables STATEICP and COUNTYICP and outputs new dataset with FIPS (county fips code) and AB (state abbreviation) variables
# Useful if using any mapping functions that only take FIPS or merging with another dataset using FIPS codes, or if a unique county id is required
#   (note that COUNTYICP is not unique -- needs to be combined with STATEICP for unique county identifiers)
mergefips <- function(dataset){
  # crosswalk from state icpsr to fips codes
  fipscrosswalk <- read_xls(glue("{root}/StateFIPSicsprAB.xls"))
  
  # joining data with crosswalk and creating unique fips from state fips + county icpsr 
  #   (county fips = first 3 digits of county icpsr except if county disappears over time, in which case FIPS = NA)
  outdata <- left_join(dataset, fipscrosswalk, by = c("STATEICP" = "STICPSR"))  %>% 
    mutate(FIPSRAW = paste0(str_pad(FIPS, 2, "left", pad = "0"),str_pad(COUNTYICP, 4, "left", pad = "0")),
           FIPS = ifelse(substr(FIPSRAW,6,6) == "0", substr(FIPSRAW,1,5), NA)) %>% select(-c(NAME, STNAME, FIPSRAW))
  
  return(outdata)
}