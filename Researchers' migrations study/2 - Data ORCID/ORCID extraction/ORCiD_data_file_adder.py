#name of the output .csv file
dataset_name = 'ORCID_2021_10_activities_2_extract'
file_names = ['0000-0002-2103-7692_employments_3086073.xml']
ids = ['0000-0002-2103-7692']
flags = ['EMP']
new_dataset_name = 'ORCID_2021_10_activities_2_extract_final'

import os, sys
import pandas as pd
import copy
import xml.etree.ElementTree as ET

def get_affiliation_data(filepath, flag, id, has_published, phd):
    tree = ET.parse(filepath)
    root = tree.getroot()
    profile_data = []

    source = None
    org = None
    role = None
    startdate = None
    enddate = None

    for child in root:
        if child.tag == '{http://www.orcid.org/ns/common}source':
            source = child
        elif child.tag == '{http://www.orcid.org/ns/common}organization':
            org = child
        elif child.tag == '{http://www.orcid.org/ns/common}role-title':
            role = child
        elif child.tag == '{http://www.orcid.org/ns/common}start-date':
            startdate = child
        elif child.tag == '{http://www.orcid.org/ns/common}end-date':
            enddate = child

    #ORCiD
    #try:
    #    orcid_id = str(source.find('{http://www.orcid.org/ns/common}source-orcid').find('{http://www.orcid.org/ns/common}path').text)
    #    row = [orcid_id]
    #except:
    #    row=[None]
    row = [str(id)]
    #POSITION: PhD, Bcs, Associate Professor, Member of Technical Staff...
    try:
        row.append(str(role.text))
    except:
        row.append(None)


    if not(org is None):
        try:
            row.append(str(org.find('{http://www.orcid.org/ns/common}name').text))
        except:
            row.append(None)


        #country of affiliation
        try:
            row.append(str(org.find('{http://www.orcid.org/ns/common}address').find('{http://www.orcid.org/ns/common}country').text))
        except:
            row.append(None)

        #region of affiliation
        try:
            row.append(str(org.find('{http://www.orcid.org/ns/common}address').find('{http://www.orcid.org/ns/common}region').text))
        except:
            row.append(None)

        #city of affiliation
        try:
            row.append(str(org.find('{http://www.orcid.org/ns/common}address').find('{http://www.orcid.org/ns/common}city').text))
        except:
            row.append(None)
    else:
        for _ in range(4):
            row.append(None)

    if not(startdate is None):
        #starting year
        try:
            row.append(str(startdate.find('{http://www.orcid.org/ns/common}year').text))
        except:
            row.append(None)

        #starting month
        try:
            row.append(str(startdate.find('{http://www.orcid.org/ns/common}month').text))
        except:
            row.append(None)

    else:
        row.append(None)
        row.append(None)

    if not(enddate is None):
        #ending year
        try:
            row.append(str(enddate.find('{http://www.orcid.org/ns/common}year').text))
        except:
            row.append(None)

        #ending month
        try:
            row.append(str(enddate.find('{http://www.orcid.org/ns/common}month').text))
        except:
            row.append(None)

    else:
        row.append(None)
        row.append(None)

    #affiliation_type
    row.append(str(flag))

    row.append(str(has_published))
    row.append(phd)

    return row

#check we are in the right place
print(os.getcwd())

df = pd.read_csv(dataset_name)
header = ['ORCiD', 'role_title', 'org_name', 'country', 'region', 'city', 'start_year', 'start_month',
                      'end_year', 'end_month', 'aff_type', 'has_published', 'is_phd']

for i in range(len(ids)):
    for index in range(df.shape[0]):

        sys.stdout.write('\r' + str((index + 1) * 100 / df.shape[0]) + '%')
        if ids[i] == df.ORCiD[index]:
            newrow = get_affiliation_data(file_names[i], flags[i], ids[i], df.has_published[index], df.is_phd[index])

            new_row = pd.DataFrame(columns=header)
            new_row[0] = newrow
            df = pd.concat([df.loc[0:index], copy.deepcopy(new_row), df.loc[index:]], ignore_index=True)
            break


df.to_csv(new_dataset_name)
