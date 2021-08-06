'''
Get the necessary files for running the PEA from CDDIS, 
given a station, date and download location

Input:
station - IGS station in RINEX3 style format - str
year - year in YYYY format - str
date - date in DDD day-of-year format (or MM-DD format when -md option selected) - str
dwndir - download location - str

Output:
SNX, SP3, clk_30s, RINEX3 obs, and RINEX nav files

 --- TO DO ---
To clean up code need to:
    - check rapid file presence before connection / download attempt
    - move to faster ftp/ftps/rclone from wget (partially done)
    - consolidate and reduce code length by having general functions for
        downloading 'product' and 'data' files
    - output dict of files to pass into create_yaml_PPP.py function (done for rapid)
 -------------
Ronald Maj
2020-11-14 14:23
'''

import argparse
from datetime import datetime, timedelta

from ftplib import FTP_TLS as _FTP_TLS
from pathlib import Path as _Path
import subprocess as _sp
import urllib.request as _rqs
import pandas as _pd
import numpy as _np
import sys as _sys



import matplotlib.pyplot as _plt
import matplotlib.dates as mdates
from datetime import timedelta as _timedelta

from gn_lib.gn_datetime import j20002datetime
from gn_lib.gn_io.rinex import _read_rnx, _rnx_pos
from gn_lib.gn_io.sp3 import read_sp3 as _read_sp3
from gn_lib.gn_transform import xyz2llh_heik, llh2rot
from gn_lib.gn_download import check_n_download_url, dates_type_convert, download_rinex3, download_prod, download_most_recent, gpswkD2dt


def download_pea_prods(
    dates,
    dest,
    ac='igs',
    out_dict=False,
    trop_vmf3=False,
    brd_typ='igs',
    snx_typ='igs',
    clk_sel='clk',
    most_recent=False):
    '''
    Download necessary pea product files for date/s provided
    '''

    if most_recent: 
        snx_vars_out = download_most_recent(dest=dest, f_type='snx', ac=snx_typ, dwn_src='cddis', f_dict_out=True, gpswkD_out=True, ftps_out=True)
        f_dict, gpswkD_out, ftps = snx_vars_out

        clk_vars_out = download_most_recent(dest=dest, f_type=clk_sel, ac=ac, dwn_src='cddis', f_dict_out=True, gpswkD_out=True, ftps_out=True)
        f_dict_update, gpswkD_out, ftps = clk_vars_out
        f_dict.update(f_dict_update)
        gpswkD = gpswkD_out['clk_gpswkD'][0]
        
        if most_recent == True:
            num = 1
        else:
            num = most_recent
        
        dt0 = gpswkD2dt(gpswkD)
        dtn = dt0 - _timedelta(days=num-1)
        
        if dtn==dt0:
            dt_list = [dt0]
        else:
            dates = _pd.date_range(start=str(dtn),end=str(dt0),freq='1D')
            dates = list(dates)
            dates.reverse()
            dt_list = dates_type_convert(dates)
    else:
        dt_list = dates_type_convert(dates)
    
    dest_pth = _Path(dest)
    # Output dict for the files that are downloaded
    if not out_dict:
        out_dict = {
            'atxfiles':['igs14.atx'],
            'blqfiles':['OLOAD_GO.BLQ']
        }

    # Get the ATX file if not present already:
    if not (dest_pth/'igs14.atx').is_file():
        if not dest_pth.is_dir():
            dest_pth.mkdir(parents=True)
        url = 'https://files.igs.org/pub/station/general/igs14.atx'
        check_n_download_url(url,dwndir=dest)
    
    # Get the BLQ file if not present already:
    if not (dest_pth/'OLOAD_GO.BLQ').is_file():
        url = 'https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/OLOAD_GO.BLQ'
        check_n_download_url(url,dwndir=dest)
    
    # For the troposphere, have two options: gpt2 or vmf3. If flag is set to True, download 6-hourly trop files:
    if trop_vmf3:
        # If directory for the Tropospheric model files doesn't exist, create it:
        if not (dest_pth/'grid5').is_dir():
            (dest_pth/'grid5').mkdir(parents=True)
        for dt in dt_list:
            year = dt.strftime('%Y')
            # Create urls to the four 6-hourly files associated with the tropospheric model
            begin_url = f'https://vmf.geo.tuwien.ac.at/trop_products/GRID/5x5/VMF3/VMF3_OP/{year}/'
            f_begin = 'VMF3_' + dt.strftime('%Y%m%d') + '.H'
            urls = [ begin_url+f_begin+en for en in ['00','06','12','18'] ]
            urls.append(begin_url+'VMF3_' + (dt+_timedelta(days=1)).strftime('%Y%m%d') + '.H00')
            # Run through model files, downloading if they are not in directory
            for url in urls:
                if not (dest_pth/f'grid5/{url[-17:]}').is_file():
                    check_n_download_url(url,dwndir=str(dest_pth/'grid5'))
    else:
        # Otherwise, check for GPT2 model file or download if necessary:
        if not (dest_pth/'gpt_25.grd').is_file():
            url = 'https://peanpod.s3-ap-southeast-2.amazonaws.com/pea/examples/EX03/products/gpt_25.grd'
            check_n_download_url(url,dwndir=dest)
    
    standards = ['sp3','erp',clk_sel]
    ac_typ_dict = {ac_sel:[] for ac_sel in [ac,brd_typ,snx_typ]}
    for typ in standards:
        ac_typ_dict[ac].append(typ)
    ac_typ_dict[brd_typ].append('rnx')
    
    if not most_recent:
        f_dict = {}
        ac_typ_dict[snx_typ].append('snx')

    # Download product files of each type from CDDIS for the given dates:
    for ac in ac_typ_dict:
        if most_recent:
            f_dict_update = download_prod(dates=dt_list, dest=dest, ac=ac, f_type=ac_typ_dict[ac], dwn_src='cddis', f_dict=True, ftps=ftps)
        else:
            f_dict_update = download_prod(dates=dt_list, dest=dest, ac=ac, f_type=ac_typ_dict[ac], dwn_src='cddis', f_dict=True)
        f_dict.update(f_dict_update)

    f_types=[]
    for el in list(ac_typ_dict.values()):
        for typ in el:
            f_types.append(typ)

    # Prepare the output dictionary based on the downloaded files:
    for f_type,name in zip(sorted(f_types),['clk', 'erp', 'nav', 'snx', 'sp3']):
        out_dict[f'{name}files'] = sorted(f_dict[f_type])

    return out_dict




'''
Information from: 
https://cddis.nasa.gov/Data_and_Derived_Products/GNSS/RINEX_Version_3.html


d = Hatanaka-compressed observation data
f = Beidou navigation message data
g = GLONASS navigation message data
h = SBAS payload navigation message data
l = GALILEO navigation message data
m = meteorological data
n = GPS navigation message data
o = observation data
p = mixed GNSS navigation message data
q = QZSS navigation message data
s = observation summary files (extracted from RINEX header)

Information from: 
ftp://ftp.igs.org/pub/data/format/rinex304.pdf (pg. 56)

GO - GPS Obs.,
RO - GLONASS Obs.
EO - Galileo Obs.
JO - QZSS Obs.
CO - BDS Obs.
IO – IRNSS Obs.
SO - SBAS Obs.

MO - Mixed Obs.

CN - BDS Nav.
RN - GLONASS Nav.
SN - SBAS Nav.
IN – IRNSS Nav.
EN - Galileo Nav.
GN - Nav. GPS
JN - QZSS Nav.
MM - Meteorological Observation

MN - Nav. All GNSS Constellations)

'''


daily_opts = {
    'MO':'d',
    'CN':'f',
    'RN':'g',
    'SN':'h',
    'IN':'i',
    'EN':'l',
    'MM':'m',
    'GN':'n',
    'MN':'p',
    'JN':'q',
}



if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(
        description = 'Get snx, clk, sp3, obs and nav files from CDDIS necessary for running pea'
        )
    
    # Command line function arguments
    parser.add_argument("station", 
        help = "GPS station name - must be new RINEX3 format - 9 characters"
        )
        
    parser.add_argument("year", 
        help = "Year in YYYY format"
    )
    
    parser.add_argument("doy", 
        help = "Day-of-year in DDD format - include leading zero if doy < 100"
    )

    parser.add_argument("dwndir", 
        help = "Download directory"
    )  
    
    parser.add_argument("-md", "--month_day", action="store_true", 
        help = "Option to replace doy with month_day input with format MM-DD"
    )   

    parser.add_argument("-vmf3", "--vmf3_flag", action="store_true", 
        help = "Option to get VMF3 files for the troposphere"
    )   

    parser.add_argument("-rapid", "--rapid_dwn", action = "store_true",
        help = "Option to download rapid files instead. Set doy to current day to find most recent rapid files"
    )

    # Get command line args:
    args = parser.parse_args()

    # Check that inputs are the correct format:
    station = check_station(args)
    year = check_year(args)

    # Download directory:
    dwndir = args.dwndir
    
    # If md flag was selected, convert Month-Day input to DOY
    # Otherwise, just check that DOY is correct format
    if args.month_day:
        mn_dy = check_monthday(args)
        month = mn_dy[:2]
        day = mn_dy[3:]
        dt_str = f'{year}-{month}-{day}'
        dt = datetime.strptime(dt_str,"%Y-%m-%d")
        doy = dt.strftime("%j")         
    else:
        doy = check_doy(args)
    
    # Get the VMF3 model files flag - if True will get 6 hourly files from UT-Wien servers
    vmf3_flag = args.vmf3_flag

    # Get the rapid files download flag
    rapid = args.rapid_dwn
    
    # Get the pea files:
    if rapid:
        if vmf3_flag:
            out = get_pea_files(station, year, doy, dwndir, trop_vmf3 = True, rapid = rapid)
        else:
            out = get_pea_files(station, year, doy, dwndir, rapid = rapid)
    else:
        if vmf3_flag:
            get_pea_files(station, year, doy, dwndir, trop_vmf3 = True)
        else:
            get_pea_files(station, year, doy, dwndir)