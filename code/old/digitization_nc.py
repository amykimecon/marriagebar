import os
import pandas as pd
from typing import List

# importing helper functions
os.chdir("/Users/amykim/GitHub/marriagebar/code")
import documentai_helper as docai

# initializing global variables for processor
project_id = "hallowed-name-293714"
location = "us"
processor_id = "22b1d123eb00c51b"
mime_type = "image/png"
processor_version_id = "pretrained-form-parser-v2.0-2022-11-10"
file_in_root = "/Users/amykim/Dropbox (Princeton)/marriagebar/raw_scans/NC/"
file_out_root = "/Users/amykim/Dropbox (Princeton)/marriagebar/clean_data/NC/"

# # SALARY DATA
# for year in range(1930, 1933):
#     print(year)
#     yearlist = []
#     for pg in range(1,6):
#         file_path = file_in_root + "salaries/processed/" + str(year) + "_p" + str(pg) + ".png"
#         (df, header_row_values) = docai.make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type)

#         if year == 1932 and pg == 2:
#             df = df.iloc[:,:7]

#         print(df.shape)
#         #print(df)
#         yearlist = yearlist + [df]

#     df_all = pd.concat(yearlist)
#     df_all.columns = header_row_values
#     df_all.to_csv(file_out_root + "salaries/" + str(year) + ".csv", index=False)

# for year in range(1933, 1938):
#     print(year)
#     for gr in ["elem","hs"]:
#         print(gr)
#         #yearlist = []
#         for pg in range(1,6):
#             file_path = file_in_root + "salaries/processed/" + str(year) + "_p" + str(pg) + "_" + gr + ".png"
#             (df, header_row_values) = docai.make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type)

#             print(df.shape)
#             #print(df)
#             #print(header_row_values)
#             #yearlist = yearlist + [df]

#             df.columns = header_row_values
#             df.to_csv(file_out_root + "salaries/ocr" + str(year) + "_p" + str(pg) + "_" + gr + ".csv", index=False)

#         #df_all = pd.concat(yearlist)
#         #df_all.columns = header_row_values
#         #df_all.to_csv(file_out_root + "salaries/" + str(year) + "_" + gr + ".csv", index=False)

# TEACHER COUNT DATA
for year in range(1930, 1938):
    print(year)
    if year != 1931:
        for pg in range(1,6):
            for pt in ["1","2"]:
                file_path = file_in_root + "counts/intermediate/" + str(year) + "_p" + str(pg) + "_pt" + pt + ".png"
                (df, header_row_values) = docai.make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type)

                print(df.shape)
                #print(df)
                if df.size != 0:
                    df.columns = header_row_values
                    df.to_csv(file_out_root + "counts/ocr/" + str(year) + "_p" + str(pg) + "_pt" + pt + ".pdf", index = False)
    else:
        for pg in range(1,6):
            file_path = file_in_root + "counts/processed/" + str(year) + "_p" + str(pg) + ".png"
            (df, header_row_values) = docai.make_df(project_id, location, processor_id, processor_version_id, file_path, mime_type)

            print(df.shape)
            #print(df)

            df.columns = header_row_values
            df.to_csv(file_out_root + "counts/ocr/" + str(year) + "_p" + str(pg) + ".pdf", index = False)



