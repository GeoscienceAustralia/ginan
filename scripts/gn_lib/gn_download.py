'''
Functions to download files necessary for Ginan processing:
sp3
erp
clk
rnx (including transformation from crx to rnx)
'''
from datetime import datetime as _datetime
from pathlib import Path as _Path
import requests as _rqs
import subprocess as _sp
import numpy as _np



def check_file_present(comp_filename, dwndir):
    '''Check if file comp_filename already present in directory dwndir'''
    
    if dwndir[-1] != '/':
        dwndir += '/'

    if comp_filename.endswith('.gz'):
        uncomp_file = _Path(dwndir+comp_filename[:-3])
    elif comp_filename.endswith('.Z'):
        uncomp_file = _Path(dwndir+comp_filename[:-2])

    if comp_filename.endswith('.crx.gz'):
        rnx_Z_file = _Path(str(uncomp_file)[:-3]+'rnx')
        if rnx_Z_file.is_file():
            print(f'File {rnx_Z_file.name} already present in {dwndir}')
            return True
    
    if uncomp_file.is_file():
        print(f'File {uncomp_file.name} already present in {dwndir}')
        present = True
    else:
        present = False
    
    return present


def check_n_download(comp_filename, dwndir, ftps, uncomp=True, remove_comp=False):
    ''' Download compressed file to dwndir if not already present and uncompress'''
    
    comp_file = _Path(dwndir+comp_filename)

    if dwndir[-1] != '/':
        dwndir += '/'

    if not check_file_present(comp_filename, dwndir):
        print(f'Downloading {comp_filename} from CDDIS')
        
        with open(comp_file, 'wb') as local_f:
            ftps.retrbinary(f'RETR {comp_filename}', local_f.write)
        
        if uncomp:
            _sp.run(['uncompress',f'{comp_file}'])
            print(f'Downloaded and uncompressed {comp_filename}')
        else:
            print(f'Downloaded {comp_filename}')
        
        if remove_comp:
            comp_file.unlink()


def gpsweekD(yr, doy, wkday_suff=False):
    """
    Convert year, day-of-year to GPS week format: WWWWD or WWWW
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
    dt = _datetime.strptime(f"{yr}-{doy:03d} 01","%Y-%j %H")
    
    wkday = dt.weekday() + 1

    if wkday == 7:
        wkday = 0
    
    mn, dy, hr = dt.month, dt.day, dt.hour

    if mn <= 2:
        yr = yr-1
        mn = mn+12

    JD = _np.floor(365.25*yr) + _np.floor(30.6001*(mn+1)) + dy + hr/24.0 + 1720981.5
    GPS_wk = _np.int(_np.floor((JD-2444244.5)/7.0))
    
    if wkday_suff:
        return str(GPS_wk)+str(wkday)
    else:
        return str(GPS_wk)



def get_install_crx2rnx(override=False):
    '''
    Check for presence of crx2rnx in /bin.
    If not present, download and extract to /bin.
    If override = True, will download if present or not
    '''
    if (not _Path('/home/ubuntu/bin/crx2rnx').is_file()) or (override):

        tmp_dir = _Path('tmp')
        if not tmp_dir.is_dir():
            tmp_dir.mkdir()

        url = 'https://terras.gsi.go.jp/ja/crx2rnx/RNXCMP_4.0.8_src.tar.gz'
        rq = _rqs.get(url,allow_redirects=True)
        with open(_Path('tmp/RNXCMP_4.0.8_src.tar.gz'),'wb') as f:
            f.write(rq.content)

        _sp.run(['tar', '-xvf', 'tmp/RNXCMP_4.0.8_src.tar.gz', '-C', 'tmp'])
        cp = ['gcc','-ansi','-O2','-static','tmp/RNXCMP_4.0.8_src/source/crx2rnx.c','-o','crx2rnx']
        _sp.run(cp)
        _sp.run(['rm','-r','tmp'])
        _sp.run(['mv','crx2rnx','/bin'])

    else:
        print('crx2rnx present in /bin')



def get_sp3(yr, doy, dest, ac='igs'):
    '''
    Function used to get the sp3 orbit file from the CDDIS server
    Will search first for an IGS final product, if not present, then search for IGS rapid

    Input:
    yr - Year (int)
    doy - Day-of-year (int)
    dest - destination (str)
    ac - Analysis Center of choice (e.g. igs, cod, jpl, gfz, esa, etc. default = igs)
    '''
    # Filename we are looking for:
    gpswkD = gpsweekD(yr,doy)
    filename = f'{ac}{gpswkD}.sp3.Z'  
    sp3_files = dest

    # Check if the sp3 file already exists:
    if Path(f"{sp3_files}/{filename[:-2]}").is_file():
        print('\nsp3 file already exists')
        print(f"{sp3_files}/{filename[:-2]}")
        return
    else:
        # Looks for final product first -- AC of choice, then IGS rapid if choice unavailable
        # Try download from CDDIS:
        try:
            try:
                Z_file = download_sp3_cddis(filename, yr, doy)
            except:
                filename = f'igr{gpswkD}.sp3.Z'    
                Z_file = download_sp3_cddis(filename, yr, doy)
        except:
            print('Please try a different day')
            return

        # Uncompress and move the .sp3 to appropriate directory
        _sp.run(["uncompress", Z_file])

        # Move the file to directory sp3_files directory
        # Check that the download directory exists, if not create it
        if not Path.exists(Path(f'{sp3_files}')):
            Path.mkdir(Path(f'{sp3_files}')) 
        _sp.run(["mv",filename[:-2],f"{sp3_files}"])

        # Check download and extraction was successful
        if Path(f"{sp3_files}/{filename[:-2]}").is_file():
            print('\nsp3 file sucessfully downloaded to')
            print(f"{sp3_files}/{filename[:-2]}")
        else:
            print('->->-sp3 file missing - did not download or extract correctly---')