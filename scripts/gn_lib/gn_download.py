'''
Functions to download files necessary for Ginan processing:
sp3
erp
clk
rnx (including transformation from crx to rnx)
'''
from datetime import date as _date
from datetime import datetime as _datetime
from datetime import timedelta as _timedelta
from ftplib import FTP_TLS as _FTP_TLS
from pathlib import Path as _Path
import urllib.request as _rqs
import urllib.error as _err
import subprocess as _sp
import pandas as _pd
import numpy as _np
import sys as _sys
import logging
import os as _os
import shutil

from .gn_datetime import gpsweekD, dt2gpswk, gpswkD2dt


import threading
import boto3
from boto3.s3.transfer import TransferConfig


MB = 1024 * 1024

# s3client = boto3.client('s3', region_name='eu-central-1')


class TransferCallback:
    """
    Handle callbacks from the transfer manager.

    The transfer manager periodically calls the __call__ method throughout
    the upload and download process so that it can take action, such as
    displaying progress to the user and collecting data about the transfer.
    """

    def __init__(self, target_size):
        self._target_size = target_size
        self._total_transferred = 0
        self._lock = threading.Lock()
        self.thread_info = {}

    def __call__(self, bytes_transferred):
        """
        The callback method that is called by the transfer manager.

        Display progress during file transfer and collect per-thread transfer
        data. This method can be called by multiple threads, so shared instance
        data is protected by a thread lock.
        """
        thread = threading.current_thread()
        with self._lock:
            self._total_transferred += bytes_transferred
            if thread.ident not in self.thread_info.keys():
                self.thread_info[thread.ident] = bytes_transferred
            else:
                self.thread_info[thread.ident] += bytes_transferred

            target = self._target_size
            _sys.stdout.write(
                f"\r{self._total_transferred / MB:.2f} MB of {target / MB:.2f} MB transferred "
                f"({(self._total_transferred / target) * 100:.2f}%).")
            _sys.stdout.flush()


def upload_with_chunksize_and_meta(local_file_path:_Path, bucket_name:str, object_key:str,
                                   public_read=False, metadata=None,verbose=True):
    """
    Upload a file from a local folder to an Amazon S3 bucket, setting a
    multipart chunk size and adding metadata to the Amazon S3 object.

    The multipart chunk size controls the size of the chunks of data that are
    sent in the request. A smaller chunk size typically results in the transfer
    manager using more threads for the upload.

    The metadata is a set of key-value pairs that are stored with the object
    in Amazon S3.
    """
    s3 = boto3.resource('s3')
    file_size = local_file_path.stat().st_size

    transfer_callback = TransferCallback(file_size) if verbose else None

    config = TransferConfig(multipart_chunksize=1 * MB)

    extra_args = {}
    if metadata is not None:
        extra_args['Metadata'] = metadata
    if public_read:
        extra_args['ACL'] = 'public-read'
    
    s3.Bucket(bucket_name).upload_file(
        str(local_file_path),
        object_key,
        Config=config,
        ExtraArgs=extra_args,
        Callback=transfer_callback)
    if verbose: _sys.stdout.write('\n')
    # return transfer_callback.thread_info


def request_checksum(url):
    '''requests md5checksum metadata over https (AWS S3 bucket)
    Returns None if file not found (404) or if no md5checksum metadata exists'''
    logging.info(f'requesting checksum for "{url}"')
    md5_checksum_response = None
    try:
        with _rqs.urlopen(url) as response:
            if response.status == 200:
                logging.info(msg = 'server says OK')
                md5_checksum_response =  response.getheader('x-amz-meta-md5checksum')
                logging.info(f'Got "{md5_checksum_response}"')
    except _err.HTTPError as e:
        logging.error(e) # TODO add a more explicit message
    return md5_checksum_response

def download_url(url,destfile):
    logging.info(f'requesting "{url}"')
    try:
        with _rqs.urlopen(url) as response:
            if response.status == 200:
                logging.info('downloading from {} to {}'.format(url, destfile))
                with open(destfile, 'wb') as out_file:
                    shutil.copyfileobj(response, out_file)
        return 0
    except _err.HTTPError as e:
        logging.error(e) # TODO add a more explicit message
        return None


def gen_uncomp_filename(comp_filename):
    '''Name of Uncompressed filename given the compressed name'''
    if comp_filename.endswith('.crx.gz'):
        uncomp_file = comp_filename[:-6]+'rnx'
    elif comp_filename.endswith('.gz'):
        uncomp_file = comp_filename[:-3]
    elif comp_filename.endswith('.Z'):
        uncomp_file = comp_filename[:-2]
    else:
        uncomp_file = comp_filename
    return uncomp_file



def gen_prod_filename(dt,pref,suff,f_type,wkly_file=False,repro3=False):
    '''
    Generate a product filename based on the inputs
    '''
    gpswk, gpswkD = dt2gpswk(dt,both=True)
    
    if repro3:
        if f_type == 'erp':
            f = f'{pref.upper()}0R03FIN_{dt.year}{dt.strftime("%j")}0000_01D_01D_{f_type.upper()}.{f_type.upper()}.gz'
        elif f_type == 'clk':
            f = f'{pref.upper()}0R03FIN_{dt.year}{dt.strftime("%j")}0000_01D_30S_{f_type.upper()}.{f_type.upper()}.gz'
        elif f_type == 'bia':
            f = f'{pref.upper()}0R03FIN_{dt.year}{dt.strftime("%j")}0000_01D_01D_OSB.{f_type.upper()}.gz'
        elif f_type == 'sp3':
            f = f'{pref.upper()}0R03FIN_{dt.year}{dt.strftime("%j")}0000_01D_05M_ORB.{f_type.upper()}.gz'
        elif f_type == 'snx':
            f = f'{pref.upper()}0R03FIN_{dt.year}{dt.strftime("%j")}0000_01D_01D_SOL.{f_type.upper()}.gz'
        elif f_type == 'rnx':
            f=f'BRDC00{pref.upper()}_R_{dt.year}{dt.strftime("%j")}0000_01D_MN.rnx.gz'
    elif (pref == 'igs') & (f_type == 'snx') & wkly_file:
        f = f'{pref}{str(dt.year)[2:]}P{gpswk}.{f_type}.Z'
    elif (pref == 'igs') & (f_type == 'snx'):
        f = f'{pref}{str(dt.year)[2:]}P{gpswkD}.{f_type}.Z'
    elif f_type == 'rnx':
        f=f'BRDC00{pref.upper()}_R_{dt.year}{dt.strftime("%j")}0000_01D_MN.rnx.gz'
    elif wkly_file:
        f = f'{pref}{gpswk}{suff}.{f_type}.Z'
    else:
        f = f'{pref}{gpswkD}{suff}.{f_type}.Z'
    return f, gpswk


def dates_type_convert(dates):
    '''Convert the input variable (dates) to a list of datetime objects'''
    typ_dt = type(dates)
    if  typ_dt == _date:
        dates = [dates]
        typ_dt = type(dates)
    elif  typ_dt == _datetime:
        dates = [dates]
        typ_dt = type(dates)
    elif typ_dt == _np.datetime64:
        dates = [dates.astype(_datetime)]
        typ_dt = type(dates)
    elif typ_dt == str:
        dates = [_np.datetime64(dates)]
        typ_dt = type(dates)

    if (type(dates) == list) or (type(dates) == _np.ndarray) or (type(dates)==_pd.core.indexes.datetimes.DatetimeIndex):
        dt_list = []
        for dt in dates:
            if type(dt) == _datetime:
                dt_list.append(dt)
            elif type(dt) == _date:
                dt_list.append(dt)
            elif type(dt) == _np.datetime64:
                dt_list.append(dt.astype(_datetime))
            elif type(dt) == _pd.Timestamp:
                dt_list.append(dt.to_pydatetime())
            elif type(dt) == str:
                dt_list.append(_np.datetime64(dt).astype(_datetime))

    return dt_list



def check_file_present(comp_filename, dwndir):
    '''Check if file comp_filename already present in directory dwndir'''
    
    if dwndir[-1] != '/':
        dwndir += '/'

    uncomp_filename = gen_uncomp_filename(comp_filename)
    uncomp_file = _Path(dwndir+uncomp_filename)
    
    if uncomp_file.is_file():
        logging.debug(f'File {uncomp_file.name} already present in {dwndir}')
        present = True
    else:
        present = False
    
    return present


def check_n_download_url(url, dwndir, filename=False):
    '''
    Download single file given URL to download from. 
    Optionally provide filename if different from url name
    '''
    if dwndir[-1] != '/':
        dwndir += '/'

    if not filename:
        filename = url[url.rfind('/')+1:]
    
    if not check_file_present(filename, dwndir):
        logging.debug(f'Downloading {_Path(url).name}')
        out_f = _Path(dwndir)/filename
        _rqs.urlretrieve(url,out_f)



def check_n_download(comp_filename, dwndir, ftps, uncomp=True, remove_crx=False, no_check=False):
    '''Download compressed file to dwndir if not already present and optionally uncompress'''
    
    comp_file = _Path(dwndir+comp_filename)

    if dwndir[-1] != '/':
        dwndir += '/'

    if no_check or (not check_file_present(comp_filename, dwndir)):

        logging.debug(f'Downloading {comp_filename}')

        with open(comp_file, 'wb') as local_f:
            ftps.retrbinary(f'RETR {comp_filename}', local_f.write)
        if uncomp:
            _sp.run(['uncompress',f'{comp_file}'])
            # If RINEX file, need to convert from Hatanaka compression
            if comp_filename.endswith('.crx.gz'):
                get_install_crx2rnx()
                crx_file = _Path(dwndir+comp_filename[:-3])
                if _sys.path[0][-1] == '/':
                    run_str = f'{_sys.path[0]}crx2rnx'
                else:
                    run_str = f'{_sys.path[0]}/crx2rnx'
                _sp.run([run_str,f'{str(crx_file)}'])

            logging.debug(f'Downloaded and uncompressed {comp_filename}')
        else:
            logging.debug(f'Downloaded {comp_filename}')

        if remove_crx:
            if comp_filename.endswith('.crx.gz'):
                crx_file.unlink()
        success=True
        # except:
        #     print(f'Failed to download {comp_filename}')
        #     success=False
    else:
        success=True
    
    return success



def get_install_crx2rnx(override=False,verbose=False):
    '''
    Check for presence of crx2rnx in PATH.
    If not present, download and extract to python environment PATH location.
    If override = True, will download if present or not
    '''
    if (not _Path(f'{_sys.path[0]}/crx2rnx').is_file()) or (override):
        if verbose:
            logging.info(f'Installing crx2rnx at {_sys.path[0]}')
        tmp_dir = _Path('tmp')
        if not tmp_dir.is_dir():
            tmp_dir.mkdir()

        url = 'https://terras.gsi.go.jp/ja/crx2rnx/RNXCMP_4.0.8_src.tar.gz'
        out_f = _Path('tmp/RNXCMP_4.0.8_src.tar.gz')
        _rqs.urlretrieve(url,out_f)

        _sp.run(['tar', '-xvf', 'tmp/RNXCMP_4.0.8_src.tar.gz', '-C', 'tmp'])
        cp = ['gcc','-ansi','-O2','-static','tmp/RNXCMP_4.0.8_src/source/crx2rnx.c','-o','crx2rnx']
        _sp.run(cp)
        _sp.run(['rm','-r','tmp'])
        _sp.run(['mv','crx2rnx',_sys.path[0]])
    else:
        if verbose:
            logging.info(f'crx2rnx already present in {_sys.path[0]}')



def connect_cddis(verbose=False):
    '''
    Output an FTP_TLS object connected to the cddis server root
    '''
    if verbose:
        logging.info('\nConnecting to CDDIS server...')
    
    ftps = _FTP_TLS('gdc.cddis.eosdis.nasa.gov')
    ftps.login()
    ftps.prot_p()
    
    if verbose:
        logging.info('Connected.')
    
    return ftps



def select_mr_file(mr_files,f_typ,ac):
    '''
    Given a list of most recent files, find files matching type and AC of interest
    '''
    if ac == 'any':
        search_str = f'.{f_typ}.Z'
        mr_typ_files=[f for f in mr_files if f.endswith(search_str)]
    else:
        search_str_end = f'.{f_typ}.Z'
        search_str_sta = f'{ac}'
        mr_typ_files=[f for f in mr_files if ((f.startswith(search_str_sta))&(f.endswith(search_str_end)))]

    return mr_typ_files



def find_mr_file(dt, f_typ, ac, ftps):
    '''Given connection to the ftps server, find the most recent file of type f_typ and analysis centre ac'''
    c_gpswk = dt2gpswk(dt)
    
    ftps.cwd(f'gnss/products/{c_gpswk}')
    mr_files = ftps.nlst()
    mr_typ_files = select_mr_file(mr_files,f_typ,ac)
    
    if mr_typ_files == []:
        while mr_typ_files == []:
            logging.info(f'GPS Week {c_gpswk} too recent')
            logging.info(f'No {ac} {f_typ} files found in GPS week {c_gpswk}')
            logging.info(f'Moving to GPS week {int(c_gpswk) - 1}')
            c_gpswk = str(int(c_gpswk) - 1)
            ftps.cwd(f'../{c_gpswk}')
            mr_files = ftps.nlst()
            mr_typ_files = select_mr_file(mr_files,f_typ,ac)
    mr_file = mr_typ_files[-1]
    return mr_file, ftps, c_gpswk



def download_most_recent(dest, f_type, ftps=None, ac='any', dwn_src='cddis', f_dict_out=False, gpswkD_out=False,ftps_out=False):
    '''
    Download the most recent version of a product file
    '''
    # File types should be converted to lists if not already a list
    if type(f_type)==list:
        f_types = f_type
    else:
        f_types = [f_type]

    # Create directory if doesn't exist:
    if not _Path(dest).is_dir():
        _Path(dest).mkdir(parents=True)

    # Create list to hold filenames that will be downloaded:
    if f_dict_out:
        f_dict = {f_typ:[] for f_typ in f_types}
    if gpswkD_out:
        gpswk_dict={f_typ+'_gpswkD':[] for f_typ in f_types}
    # Connect to ftps if not already:
    if not ftps:
        # Connect to chosen server
        if dwn_src=='cddis':
            ftps = connect_cddis()
            
            for f_typ in f_types:
                logging.info(f'\nSearching for most recent {ac} {f_typ}...\n')
                
                dt = (_np.datetime64('today')-1).astype(_datetime)
                mr_file, ftps, c_gpswk = find_mr_file(dt,f_typ,ac,ftps)
                check_n_download(mr_file, dwndir=dest, ftps=ftps, uncomp=True)
                ftps.cwd(f'/')
                if f_dict_out:
                    f_uncomp = gen_uncomp_filename(mr_file)
                    if f_uncomp not in f_dict[f_typ]:
                        f_dict[f_typ].append(f_uncomp)
                c_gpswkD = mr_file[3:8]
                if gpswkD_out:
                    gpswk_dict[f_typ+'_gpswkD'].append(c_gpswkD)                
            
            ret_vars = []
            if f_dict_out:
                ret_vars.append(f_dict)
            if gpswkD_out:
                ret_vars.append(gpswk_dict)
            if ftps_out:
                ret_vars.append(ftps)

            return ret_vars



def download_prod(dates, dest, ac='igs', suff='', f_type='sp3', dwn_src='cddis', ftps=False, f_dict=False, wkly_file=False, repro3=False):
    '''
    Function used to get the product file/s from download server of choice, default: CDDIS

    Input:
    dest - destination (str)
    ac - Analysis Center / product of choice (e.g. igs, igr, cod, jpl, gfz, default = igs)
    suff - optional suffix added to file name (e.g. _0 or _06 for ultra-rapid products) 
    f_type - file type to download (e.g. clk, cls, erp, sp3, sum, default = sp3)
    dwn_src - Download Source (e.g. cddis, ga)
    ftps - Optionally input active ftps connection object
    wkly_file - optionally grab the weekly file rather than the daily
    repro3 - option to download the REPRO3 version of the file

    '''

    # Convert input to list of datetime dates (if not already)
    if (type(dates) == list) & (type(dates[0]) == _date):
        dt_list = dates
    else:
        dt_list = dates_type_convert(dates)

    # File types should be converted to lists also, if not already so
    if type(f_type)==list:
        f_types = f_type
    else:
        f_types = [f_type]

    # Create directory if doesn't exist:
    if not _Path(dest).is_dir():
        _Path(dest).mkdir(parents=True)

    # Create list to hold filenames that will be downloaded:
    if f_dict:
        f_dict = {f_typ:[] for f_typ in f_types}

    # Connect to ftps if not already:
    if not ftps:
        # Connect to chosen server
        if dwn_src=='cddis':
            logging.info('\nGathering product files...')
            ftps = connect_cddis(verbose=True)
            p_gpswk = 0
    else:
        p_gpswk = 0

    for dt in dt_list:
        for f_typ in f_types:
            
            if dwn_src=='cddis':

                if repro3:
                    f, gpswk = gen_prod_filename(dt, pref=ac, suff=suff, f_type=f_typ, repro3=True)
                elif (ac=='igs') and (f_typ=='erp'):
                    f, gpswk = gen_prod_filename(dt, pref=ac, suff='7', f_type=f_typ, wkly_file=True)
                elif f_typ=='snx':
                    mr_file, ftps, gpswk = find_mr_file(dt, f_typ, ac, ftps)
                    f = mr_file
                elif wkly_file:
                    f, gpswk = gen_prod_filename(dt, pref=ac, suff=suff, f_type=f_typ, wkly_file=True)
                else:
                    f, gpswk = gen_prod_filename(dt, pref=ac, suff=suff, f_type=f_typ)

                if not check_file_present(comp_filename=f, dwndir=dest):
                    # gpswk = dt2gpswk(dt)
                    if gpswk != p_gpswk:
                        ftps.cwd('/')
                        ftps.cwd(f'gnss/products/{gpswk}')
                        if repro3:
                            ftps.cwd(f'repro3')

                    if f_typ == 'rnx':
                        ftps.cwd('/')
                        ftps.cwd(f'gnss/data/daily/{dt.year}/brdc')
                        success = check_n_download(f, dwndir=dest, ftps=ftps, uncomp=True, remove_crx=True, no_check=True)
                        ftps.cwd('/')
                        ftps.cwd(f'gnss/products/{gpswk}')
                    else:
                        success = check_n_download(f, dwndir=dest, ftps=ftps, uncomp=True, remove_crx=True, no_check=True)
                    p_gpswk=gpswk
                else:
                    success = True
                if f_dict and success:
                    f_uncomp = gen_uncomp_filename(f)
                    if f_uncomp not in f_dict[f_typ]:
                        f_dict[f_typ].append(f_uncomp)

            else:
                for dt in dt_list:
                    for f_typ in f_types:
                        f = gen_prod_filename(dt, pref=ac, suff=suff, f_type=f_typ)
                        success = check_n_download(f, dwndir=dest, ftps=ftps, uncomp=True, remove_crx=True, no_check=True)
                        if f_dict and success:
                            f_uncomp = gen_uncomp_filename(f)
                            if f_uncomp not in f_dict[f_typ]:
                                f_dict[f_typ].append(f_uncomp)
    if f_dict:
        return f_dict



def download_pea_prods(
    dest, 
    most_recent=True, 
    dates=None, 
    ac='igs', 
    out_dict=False, 
    trop_vmf3=False, 
    brd_typ='igs', 
    snx_typ='igs', 
    clk_sel='clk', 
    repro3=False):
    '''
    Download necessary pea product files for date/s provided
    '''
    if dest[-1] != '/':
        dest+='/'
    
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
            dt_list = sorted(dates_type_convert(dates))
    else:
        dt_list = sorted(dates_type_convert(dates))

    dest_pth = _Path(dest)
    # Output dict for the files that are downloaded
    if not out_dict:
        out_dict = {
            'dates':dt_list,
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
    
    if repro3:
        snx_typ = ac
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
        elif repro3:
            f_dict_update = download_prod(dates=dt_list, dest=dest, ac=ac, f_type=ac_typ_dict[ac], dwn_src='cddis', f_dict=True, repro3=True)
        else:
            f_dict_update = download_prod(dates=dt_list, dest=dest, ac=ac, f_type=ac_typ_dict[ac], dwn_src='cddis', f_dict=True)
        f_dict.update(f_dict_update)

    f_types=[]
    for el in list(ac_typ_dict.values()):
        for typ in el:
            f_types.append(typ)
    if most_recent:
        f_types.append('snx')

    # Prepare the output dictionary based on the downloaded files:
    for f_type in f_types:
        if f_type=='rnx':
            out_dict[f'navfiles'] = sorted(f_dict[f_type])
        out_dict[f'{f_type}files'] = sorted(f_dict[f_type])

    return out_dict



def download_rinex3(dates, stations, dest, dwn_src='cddis', ftps=False, f_dict=False):
    '''
    Function used to get the RINEX3 observation file from download server of choice, default: CDDIS
    '''
    if dest[-1] != '/':
        dest+='/'
    # Convert input to list of datetime dates (if not already)
    dt_list = dates_type_convert(dates)

    if type(stations) == str:
        stations = [stations]

    # Create directory if doesn't exist:
    if not _Path(dest).is_dir():
        _Path(dest).mkdir(parents=True)

    if f_dict:
        f_dict = {'rnxfiles':[]}

    # Connect to ftps if not already:
    if not ftps:
        # Connect to chosen server
        if dwn_src=='cddis':
            logging.info('\nGathering RINEX files...')
            ftps = connect_cddis(verbose=True)
            p_date = 0

            for dt in dt_list:
                for station in stations:
                    
                    f_pref = f'{station}_R_'
                    f_suff_crx = f'0000_01D_30S_MO.crx.gz'
                    f = f_pref+dt.strftime('%Y%j')+f_suff_crx

                    if not check_file_present(comp_filename=f, dwndir=dest):
                        if p_date == dt:
                            try:
                                success = check_n_download(f, dwndir=dest, ftps=ftps, uncomp=True, remove_crx=True, no_check=True)
                            except:
                                logging.error(f'Download of {f} failed - file not found')
                                success = False
                        else:
                            ftps.cwd('/')
                            ftps.cwd(f"gnss/data/daily{dt.strftime('/%Y/%j/%yd/')}")
                            try:
                                success = check_n_download(f, dwndir=dest, ftps=ftps, uncomp=True, remove_crx=True, no_check=True)
                            except:
                                logging.error(f'Download of {f} failed - file not found')
                                success = False
                            p_date = dt
                    else:
                        success = True
                    if f_dict and success:
                        f_dict['rnxfiles'].append(gen_uncomp_filename(f))
    else:
        for dt in dt_list:
            for station in stations:
                f_pref = f'{station}_R_'
                f_suff_crx = f'0000_01D_30S_MO.crx.gz'
                f = f_pref+dt.strftime('%Y%j')+f_suff_crx
                if not check_file_present(comp_filename=f, dwndir=dest):
                    success = check_n_download(f, dwndir=dest, ftps=ftps, uncomp=True, remove_crx=True,no_check=True)
                else:
                    success = True
                if f_dict and success:
                    f_dict['rnxfiles'].append(gen_uncomp_filename(f))
    if f_dict:
        return f_dict


