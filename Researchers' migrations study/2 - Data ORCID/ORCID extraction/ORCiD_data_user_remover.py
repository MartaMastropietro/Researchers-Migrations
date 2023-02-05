#name of the output .csv file
filename = 'ORCID_2021_10_activities_2_extract'
new_filename = 'ORCID_2021_10_activities_2_extract_clean'

ids_to_remove = ['0000-0002-2103-7692']

import os, sys
import pandas as pd

#check we are in the right place
print(os.getcwd())

df = pd.read_csv(filename)

for id in ids_to_remove:
    df = df[df.ORCiD != str(id)]

df.to_csv(new_filename)
