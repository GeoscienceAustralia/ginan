import argparse
from io import BytesIO as _BytesIO
from pathlib import Path
import numpy as _np
import pandas as _pd

from datetime import datetime
from scipy.interpolate import InterpolatedUnivariateSpline

from gn_lib.gn_io.common import path2bytes

import wget


def gpsweekD(yr,doy):
    """
    Convert year, day-of-year to GPS week format: WWWWD
    Based on code from Kristine Larson's gps.py
    https://github.com/kristinemlarson/gnssIR_python/gps.py
    
    Input:
    yr - year (int)
    doy - day-of-year (int)

    Output:
    GPS Week in WWWWD format - weeks since 7 Jan 1980 + day of week number (str)
    """

    # Set up the date and time variables
    yr = int(yr)
    doy = int(doy)
    dt_str = f"{yr}-{doy} 01"
    dt = datetime.strptime(dt_str,"%Y-%j %H")
    
    wkday = dt.weekday() + 1

    if wkday == 7:
        wkday = 0
    
    mn, dy = dt.month, dt.day
    hr = dt.hour
    
    if mn <= 2:
        yr = yr-1
        mn = mn+12

    A = _np.floor(365.25*yr)
    B = _np.floor(30.6001*(mn+1))
    C = hr/24.0
    JD = A + B + dy + C + 1720981.5
    GPS_wk = _np.floor((JD-2444244.5)/7.0)
    GPS_wk = _np.int(GPS_wk)
    
    return str(GPS_wk)+str(wkday)


def extrap_LOD_data(df):
    '''
    Extrapolate LOD and LODsig date from ERP-like structured dataframe
    '''
    for para in ['LOD','LODsig']:

        xs = df[para][~df[para].isna()].index.values
        ys = df[para][~df[para].isna()].values

        order = 2
        s = InterpolatedUnivariateSpline(xs, ys, k=order)
        x_fill = df[para][df[para].isna()].index.values

        fill_dict = {k:v for k,v in zip(x_fill,s(x_fill))}
        df.loc[:,para] = df[para].fillna(value=fill_dict)
    

def mjd_convert(dt):
    '''Convert datetime dt to the corresponding MJD
    51544.00 == 2000-01-01 00:00:00'''
    st_dt = _np.datetime64('2000-01-01 00:00:00')
    return 51544.00 + (dt-st_dt)/_np.timedelta64(1, 'D')



def erp_outfile(datetime_str,file_suffix='_12'):
    '''
    Input datetime string of format "YY-MM-DD hh:mm:ss"
    Currently the time must be 00:00:00 for the file to output correctly
    Will introduce 6-hourly ability in next iteration
    '''
    dt64 = _np.datetime64(datetime_str)
    mjd = mjd_convert(dt64)
    
    if Path('finals.daily.iau2000.txt').is_file():
        Path('finals.daily.iau2000.txt').unlink()
    iers_url = 'https://datacenter.iers.org/data/latestVersion/finals.daily.iau2000.txt'
    dwn_out = wget.download(iers_url)
    byte_file = path2bytes(dwn_out)

    iers_df = _pd.read_fwf(_BytesIO(byte_file),
                 widths=[2,2,2,9,3,9,9,10,9,3,10,10,8,7,7,6,9,10,9,10,10,11,10,10],
                 usecols=[3,5,6,7,8,10,11,12,13]+list(range(15,19)),
                 header=None,dtype=float)
    Path(dwn_out).unlink()

    cols = [
        'MJD',
        'Xpole',
        'Xsig',
        'Ypole',
        'Ysig',
        'UT1-UTC',
        'UTsig',
        'LOD',
        'LODsig',
        'Xrt',
        'Xrtsig',
        'Yrt',
        'Yrtsig'
    ]
    iers_df.columns = cols
    
    erp_df = iers_df[(iers_df['MJD']>mjd-10)&(iers_df['MJD']<mjd+3)]

    erp_df.loc[:,'Xpole'] = erp_df.loc[:,'Xpole']*10**6
    erp_df.loc[:,'Xsig'] = erp_df.loc[:,'Xsig']*10**6

    erp_df.loc[:,'Ypole'] = erp_df.loc[:,'Ypole']*10**6
    erp_df.loc[:,'Ysig'] = erp_df.loc[:,'Ysig']*10**6

    erp_df.loc[:,'UT1-UTC'] = erp_df.loc[:,'UT1-UTC']*10**7
    erp_df.loc[:,'UTsig'] = erp_df.loc[:,'UTsig']*10**7

    erp_df.loc[:,'Xrt'] = erp_df.loc[:,'Xrt']*10**3
    erp_df.loc[:,'Xrtsig'] = erp_df.loc[:,'Xrtsig']*10**3

    erp_df.loc[:,'Yrt'] = erp_df.loc[:,'Yrt']*10**3
    erp_df.loc[:,'Yrtsig'] = erp_df.loc[:,'Yrtsig']*10**3
    
    erp_df.loc[:,'LOD'] = erp_df.loc[:,'LOD']*10**4
    erp_df.loc[:,'LODsig'] = erp_df.loc[:,'LODsig']*10**4

    cols_order = [
        'MJD',
        'Xpole',
        'Ypole',
        'UT1-UTC',
        'LOD',
        'Xsig',
        'Ysig',
        'UTsig',
        'LODsig',
        'Xrt',
        'Yrt',
        'Xrtsig',
        'Yrtsig'
    ] 
    
    if erp_df[erp_df['MJD']==mjd+1]['LOD'].isnull().values[0]:
        erp_extra = erp_df[(erp_df['MJD']>mjd-35)&(erp_df['MJD']<mjd+3)]
        extrap_LOD_data(erp_extra)
        erp_out = erp_extra[(erp_extra['MJD']==mjd)|(erp_extra['MJD']==mjd+1)]
    else:
        erp_out = erp_df[(iers_df['MJD']==mjd)&(erp_df['MJD']==mjd+1)]
    
    erp_out = erp_out[cols_order]
    erp_out.insert(loc=9,column='Nt',value=[0.0,0.0])
    erp_out.insert(loc=9,column='Nf',value=[0.0,0.0])
    erp_out.insert(loc=9,column='Nr',value=[0.0,0.0])
    out_vals = erp_out.astype(int).values
    
    
    
    # Write file out, with template header of IGU format
    template=['version 2\n',
     'Source: Xpole,Ypole,Xrt,Yrt,LOD: weighted average of centres;\n',
     '        UT1-UTC: integrated from the 5th day prior to Bull. A\n',
     '                 last non-predicted value.\n',
     '\n',
     'Orbits: to be used with the IGS Ultra Rapid Orbits (IGU)\n',
     '\n',
     '  MJD      Xpole   Ypole  UT1-UTC    LOD  Xsig  Ysig   UTsig  LODsig  Nr Nf Nt    Xrt    Yrt  Xrtsig  Yrtsig\n',
     '             (10**-6")       (0.1 usec)    (10**-6")     (0.1 usec)              (10**-6"/d)    (10**-6"/d)\n']

    for row in out_vals:
        template+=[str(int(_np.floor(row[0])))+'.00'+"   "+"   ".join(row[1:].astype(str))+'\n']
        
    dtc = dt64.astype(datetime)

    gps_date = gpsweekD(dtc.strftime('%Y'),dtc.strftime('%j'))
        
    with open(Path(f'igu{gps_date}{file_suffix}.erp'),'w') as out_file:
        out = out_file.writelines(template)    
    
    #if not mjd%1:

if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(
        description = 'Create an EPR file based on IERS daily data'
        )
    
    # Command line function arguments
    parser.add_argument("datetime_string", 
        help = """
        DateTime string of format: 
        'YYYY-MM-DD hh:mm:ss'.
        
        Pass argument in brackets: 
        e.g. create_erp_file.py "2021-04-17 00:00:00"
        
        At the moment time must be passed as 00:00:00
        """
        )

    parser.add_argument("-file_suff", "--file_suff", 
        help = "Change filename suffix. Default: '_12' "
    )
    
    args = parser.parse_args()
    dt_str = args.datetime_string
    f_suff = args.file_suff
    
    if f_suff:
        erp_outfile(dt_str,file_suffix=f_suff)
    else:
        erp_outfile(dt_str,file_suffix='_12')
