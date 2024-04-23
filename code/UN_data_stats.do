* Processing UN data
* 2024/04 

global world_data "/Users/carolyn/Library/CloudStorage/Dropbox/marriagebar_data/world_dataset"

import delimited "$world_data/UN_women_data_hub.csv", clear 
* this data gives rate of labor force participation by gender/marital status 

gsort maritalstatuscode sexcode obs_value
list ref_areadescription obs_value if maritalstatuscode=="MIU" & sexcode=="FEMALE"
list ref_areadescription obs_value if maritalstatuscode=="MIU" & sexcode=="MALE"
