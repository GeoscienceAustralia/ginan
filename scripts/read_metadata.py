'''
Read metadata SINEX file to get various satellite information 
(Block, COSPAR ID, SVN - PRN relations, Comments, ...)

Author: Ronald Maj 
2020-10-22 21:54
'''

from pathlib import Path
import wget
import pandas as pd
from datetime import datetime, timedelta
import subprocess


def get_metadata_file(suff):
    '''
    Function used to check for metadata file and if not present download it
    '''
    
    if Path('metadata').is_dir():

        if Path(f'metadata/igs_metadata_{suff}.snx').is_file():
            return
        else:
            files = list(Path('metadata').glob('*'))

    else:
        Path('metadata').mkdir()
        files = []
    

    if files:
        for f in files:
            f.unlink()

    url = 'https://files.igs.org/pub/station/general/igs_satellite_metadata.snx'
    wget.download(url)
    Path('igs_satellite_metadata.snx').rename(f'metadata/igs_metadata_{suff}.snx')


def df_sat_info(col_filter = (0,0) ):
    '''
    Function to get satellite info from metadata file. 

    The optional input variable 'col_filter' is a tuple of form (col, val)
    Therefore, the dataframe returned can be filtered by column 'col' (str) and value/s 'val' (list)

    '''
    # If the metadata file is not present, download and place in metadata directory
    dtoday = datetime.now().strftime('%Y_%m_%d')
    get_metadata_file(dtoday)


    with open(Path(f'metadata/igs_metadata_{dtoday}.snx'),'rb') as f:
        # Exit flag
        ex = 0

        while ex == 0:
            # Read lines until the satellite identifier block is reached
            line = f.readline()
            
            if b'+SATELLITE/IDENTIFIER' in line:
                
                # Skip next line
                f.readline()

                # Read line to get column info
                line = f.readline()
                col_str = str(line)
                cols = ''.join([x for x in col_str[3:-3].split('_') if x != '']).split(' ')
                cols[1] += '_' + cols.pop(2)

                # Set up dataframe to store info
                df = pd.DataFrame(columns = cols)

                # Skip line
                f.readline()

                # Read in the data and append to list of data
                data_list = []

                while b'-SATELLITE/IDENTIFIER' not in line:
                    
                    line = f.readline()
                    
                    
                    if b'-SATELLITE/IDENTIFIER' in line:
                        continue
                    elif b'*' in line:
                        continue
                    else:
                        # Create row of data and input into dataframe
                        row = [x for x in str(line)[3:-3].split('  ') if x != '']
                        svn, cid = row[0].split(' ')
                        satcat, block = row[1].split(' ')
                        comm = row[2]

                        data = {
                            'SVN':svn,
                            'COSPAR_ID':cid,
                            'SatCat':satcat,
                            'Block':block,
                            'Comment':comm
                        }

                        data_list.append(data)

                # Append dateframe with satellite data
                df = df.append(data_list)
                # Activate exit flag
                ex = 1
            
            else:
                continue
        
        f.close()

    # Return the desired information from the dataframe (if no filter info was given)
    if col_filter == (0,0):
        return df
    else:
        col, val = col_filter
        return df[df[col].isin(val)]


def df_svn_prn():
    '''
    Return the SVN/PRN relationships over time in a pandas DataFrame from the igs_metadata file
    '''
    # If the metadata file is not present, download and place in metadata directory
    dtoday = datetime.now().strftime('%Y_%m_%d')
    get_metadata_file(dtoday)
    
    with open(Path(f'metadata/igs_metadata_{dtoday}.snx'),'rb') as f:
        # Exit flag initiation:
        ex = 0

        while ex == 0:
            # Read lines until the satellite svn/prn block is reached
            line = f.readline()
            
            if b'+SATELLITE/PRN' in line:
                ex = 1
                # Skip next line
                f.readline()

                # Read line to get column info
                line = f.readline()
                col_str = str(line)
                cols = [x for x in col_str[3:-3].split(' ')]
                cols = [x.split('__')[0] for x in cols if x != '']
                cols[0] = cols[0][:-1]

                # Set up dataframe to store info
                df = pd.DataFrame(columns = cols)

                # Skip line
                f.readline()

                # Read in the data and append to list of data
                data_list = []

                while b'-SATELLITE/PRN' not in line:
                    
                    line = f.readline()
                    
                    
                    if b'-SATELLITE/PRN' in line:
                        continue
                    elif b'*' in line:
                        continue
                    else:
                        # Create row of data and input into dataframe
                        row = [x for x in str(line)[3:-3].split(' ') if x != '']
                        svn, st_date, end_date, prn = row[:4]

                        try: 
                            comment = ' '.join(row[4:])
                        except IndexError:
                            comment = None
                        
                        # Convert the datetime strings to datetime obj
                        st_date = datetime.strptime(st_date[:8],'%Y:%j') + timedelta(seconds = int(st_date[9:]))

                        if end_date == '0000:000:00000':
                            end_date = datetime.now()
                        else:
                            end_date = datetime.strptime(end_date[:8],'%Y:%j') + timedelta(seconds = int(end_date[9:]))

                        # Data dict
                        data = {
                            'SVN':svn,
                            'Valid_From':st_date,
                            'Valid_To':end_date,
                            'PRN':prn,
                            'Comment':comment
                        }
                        # Append data dict to list
                        data_list.append(data)

                # Append dateframe with satellite data
                df = df.append(data_list)
                # Activate exit flag
                ex = 1
            
            else:
                continue
        
        f.close()
    
    return df 


def svn_prn_dates(svn, dt_st, dt_end):
    '''
    Function used to determine the PRN for a given SVN and datetime

    Input:
    svn - the satellite vehicle number/s (SVN) of the satellite of interest - str or list of str
    dt_st  - datetime start of the time period of interest - datetime obj
    dt_end - datetime end of the time period of interest - datetime obj

    Output:
    DataFrame of dates of active use and the corresponding PRNs for the inputted SVN
    '''
    # Filter for SVN of interest
    df = df_svn_prn()
    df_svn = df[df['SVN'] == svn]

    # If the time period fits in in exactly of the rows, then filter for it:
    mask = (df_svn['Valid_From'] <= dt_st) & (df_svn['Valid_To'] >= dt_end)

    return df_svn[mask]