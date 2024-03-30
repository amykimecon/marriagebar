import pandas as pd
import numpy as np

root = "~/Dropbox (Princeton)/marriagebar"
git = "~/GitHub/marriagebar"
rawdata = root + "/ipums_raw/"
outdata = root + "/clean_data/"
outfigs = git + "/figures/"

cleanbind = []
filteredbind = []
for year in range(1910,1950,10):
    raw = pd.read_csv(rawdata + "census_" + year + ".csv")
    raw['demgroup'] = raw.apply()