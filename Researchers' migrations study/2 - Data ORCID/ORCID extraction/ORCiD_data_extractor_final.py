#folder which contains all folders for groups of IDs
data_dir = 'ORCID_2021_10_activities_2'
#name of the output .csv file
filename = 'ORCID_2021_10_activities_2_extract'

import os, sys
import pandas as pd
import xml.etree.ElementTree as ET


#check we are in the right place
print(os.getcwd())

useful_directories = ['educations', 'employments', 'services']

def get_person_data(data, orcid_code_dir, id):
    has_published = False
    for _, pub_finder in enumerate(os.listdir(orcid_code_dir)):
        if pub_finder == 'works':
            has_published = True
    for n_dir, i in enumerate(os.listdir(orcid_code_dir)):
        if i in useful_directories:
            orcid_indir_dir = os.path.join(orcid_code_dir, i)
            if i == 'educations':
                flag = 'EDU'
            elif i == 'employments':
                flag = 'EMP'
            else:# i == 'services'
                flag = 'SER'

            for n, j in enumerate(os.listdir(orcid_indir_dir)):

                filepath = os.path.join(orcid_indir_dir, j)

                if filepath.endswith(".xml"):
                    try:
                        tree = ET.parse(filepath)
                    except:
                        print('\n' + "####")
                        print("Check " + str(filepath) + " manually")
                        print("####")
                    else:
                        root = tree.getroot()
                        data.append(get_affiliation_data(root, flag, id, has_published))

#row: ORCiD, role_title, org_name, country, region, city, start year, start month, end year, end mont
def get_affiliation_data(root, flag, id, has_published):
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

    return row

def run_through_people(data, dataset_directory):
    n_tot = 0
    for _, _ in enumerate(os.listdir(dataset_directory)):
        n_tot+=1
    for n, i in enumerate(os.listdir(dataset_directory)):
        block_directory = os.path.join(dataset_directory, i)
        for _, id_dir in enumerate(os.listdir(block_directory)):
            orcid_code_dir = os.path.join(block_directory, id_dir)
            get_person_data(data, orcid_code_dir, id_dir)
        sys.stdout.flush()
        sys.stdout.write('\r' + str((n+1)*100/n_tot) + '%')

data = []

run_through_people(data, data_dir)

header = ['ORCiD', 'role_title', 'org_name', 'country', 'region', 'city', 'start_year', 'start_month', 'end_year', 'end_month', 'aff_type', 'has_published']

df = pd.DataFrame(data, columns = header)

def is_phd(role):
    ''' After lowercasing the affiliation_role string, look for
        terms that indicate it is a Ph.D. degree.
        Yes, regular expressions are tidier, but also unreadable
        to most people and harder to debug. '''
    # These are international synonyms for the Ph.D. degree
    synonyms = ("phd", "ph.d", "dphil", "d.phil", "rer. nat",
                "rer, nat", "doctor rerum", "doktor rerum")
    # This catches things like "Doctorate en Chimie" but
    # excludes "Postdoctoral Fellow" and "Medical Doctorate"
    special_cases_allowed = ("doctor", "doktor")
    special_cases_disallowed = ("pre", "post", "med")
    if type(role) == str:
        # lowercase the string
        role = role.lower()
        # Look for Ph.D. synonyms
        if any([(i in role) for i in synonyms]):
            return True
        # Look for special cases
        if any([(i in role) for i in special_cases_allowed]) and \
        not any([(i in role) for i in special_cases_disallowed]):
            return True
    # Otherwise call it False
    return False

df["is_phd"] = df.role_title.apply(is_phd)

df.to_csv(filename)
