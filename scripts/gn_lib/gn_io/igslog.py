'''IGS log files parser'''
import glob as _glob
import re as _re
from multiprocessing import Pool as _Pool

import numpy as _np
import pandas as _pd
from p_tqdm.p_tqdm import tqdm

from ..gn_datetime import datetime2yydoysec, j20002datetime
from ..gn_frame import get_frame_of_day
from ..gn_transform import xyz2llh_heik
from .aux_dicts import  igs_rec_tbl, atx_ant_tbl, atx_rad_tbl,\
                        translation_rec, translation_ant, translation_rad,\
                        translation_country
from .sinex import llh2snxdms, logllh2snxdms

_REGEX_ID  = _re.compile(rb'''
    (?:Four\sCharacter\sID|Site\sID)\s+\:\s*(\w{4}).*\W+
    .*\W+
    (?:\s{25}.+\W+|)
    IERS.+\:\s*(\w{9}|)
    ''', _re.IGNORECASE|_re.VERBOSE)

_REGEX_LOC = _re.compile(rb'''
    2.+\W+City\sor\sTown\s+\:\s*(\w[^\(\n\,/\?]+|).*\W+
    State.+\W+Country\s+\:\s*([^\(\n\,\d]+|).*\W+(?:\s{25}.+\W+|)
    Tectonic.+\W+(?:\s{25}.+\W+|).+\W+
    X.{22}\:?\s*([\d\-\+\.\,]+|).*\W+
    Y.{22}\:?\s*([\d\-\+\.\,]+|).*\W+
    Z.{22}\:?\s*([\d\-\+\.\,]+|).*\W+
    Latitude.+\:\s*([\d\.\,\-\+]+|).*\W+
    Longitud.+\:\s*([\d\.\,\-\+]+|).*\W+
    Elevatio.+\:\s*([\d\.\,\-\+]+|).*
    ''', _re.IGNORECASE|_re.VERBOSE)


_REGEX_REC = _re.compile(  rb'''
    3\.\d+[ ]+Receiver[ ]Type\W+\:[ ]*([\+\-\w\ ]+|)\W+                           # Receiver Type line
    (?:Satellite[ ]System[ ]+\:[ ]*(?:(.+|)[ ]*[\r\n ]+[ ]+)|)      # Satellite System (normally present)
    Serial[ ]Number[ ]+\:[ ]*(\w+|).*\W+                                              # Receiver S/N line
    Firmware[ ]Version[ ]+\:[ ]*([\w\.\/ ]+|).*\W+                     # Receiver Firmware Version line
    ()()()                                             # 3 empty groups to align with antenna block
    (?:Elevation[ ]Cutoff\sSetting[ ]*\:[ ]*(?:.+|)|)\W+ # Elevation Cutoff Setting (normally present)
    Date[ ]Installed\W+\:[ ]*(\d{4}.+|).*\W+                                    # Date Installed line
    (?:Date[ ]Removed\W+\:(?:[ ]*(\d{4}.+|))|)                 # Date Removed line (normally present)
    ''', _re.IGNORECASE|_re.VERBOSE)

_REGEX_ANT = _re.compile(  rb'''
    4\.\d+[ ]+Antenna[ ]Type\W+:[\t ]*([\/\_\S]+|)[ \t]*(\w+|)[\,?.]*\W+            # Antenna Type line
    Serial[ ]Number[ ]+:[ ]*(\S+|\S+[ ]\S+|\S+[ ]\S+[ ]\S+|).*\W+                      # Antenna S/N line
    (?:Antenna[ ]Height.+\W+|)                                        # Antenna H (normally present)
    (?:Antenna[ ]Ref.+\W+|)                                  # Antenna Ref. Point (normally present)
    (?:Degree.+\W+|)                                             # Degree offset line (rarely used)
    (?:Marker->ARP[ ]Up.+\:[ ]*([\-\d\.]+|).*\W+
    Marker->ARP[ ]North.+\:[ ]*([\-\d\.]+|).*\W+
    Marker->ARP[ ]East.+\:[ ]*([\-\d\.]+|).*\W+|)               # Marker Ecc block (normally present)
    (?:Alignment.+[\n\r](?:[ ]{25}.+[\r\n]+|)\W+|)   # Alignment from True N line (normally present)
    Antenna[ ]Rad.+\:[ ]?(.+|)(?:\(.+\)|)\W+                               # Antenna Radome Type line
    (?:(?:(?:Rad|Antenna[ ]Rad).+\W+|)         # Radome S/N or Antenna Radome S/N (normally present)
    Ant.+[\n\r]+(?:[ ]{25}.+[\r\n]+|)\W+                                   # Antenna Cable Type line
    Ant.+[\n\r]+(?:[ ]{25}.+[\r\n]+|)\W+|)                               # Antenna Cable Length line
    Date[ ]Installed[ ]+\:[ ]*(\d{4}.+|).*\W+                                    # Date Installed line
    (?:Date[ ]Removed[ ]+\:(?:[ ]*(\d{4}.+|))|)                 # Date Removed line (normally present)
    ''', _re.IGNORECASE|_re.VERBOSE)

_REGEX_REC_PRESENT = _re.compile(rb"3\.1")

_REGEX_ANT_PRESENT = _re.compile(rb"4\.1")

_REGEX_LOGNAME = r'(?:.*\/)(\w{4})(?:\w+_(\d{8})|_(\d{8})\-?\w?|(\d{8})|_.*|\d+|)\.log'

_BLK_NAMES = _np.asarray(['ID','LOC','REC','ANT'])

def find_recent_logs(logs_glob_path:str,rnx_glob_path=None)->_pd.DataFrame:
    '''Takes glob expression to get the list of log files,
     parses names into site and date and selects the ones
     with most recent date
    /data/station_logs/station_logs_IGS/*/*.log
    /data/acs/pea/proc/exs/data/*.rnx'''
    paths  = _pd.Series(_glob.glob(pathname=logs_glob_path,recursive=False),name='PATH')

    logs_df  = paths.str.extract(expand=True,pat=_REGEX_LOGNAME)
    logs_df = _pd.concat([  logs_df[0].str.upper(),
                            logs_df.iloc[:,1:].astype(float).sum(axis=1),
                            paths],axis=1)
    logs_df.columns = ['CODE','DATE','PATH']
    logs_df = logs_df[~logs_df.CODE.isna()].sort_values(['CODE','DATE'])
    recent_logs_df = logs_df[~logs_df.CODE.duplicated(keep='last')]
    if rnx_glob_path is not None:
        if isinstance(rnx_glob_path,list):
            rnx_stations = rnx_glob_path
        if isinstance(rnx_glob_path,str):
            rnx_files = sorted(_glob.glob(rnx_glob_path))
            assert len(rnx_files)!=0, f"No rnx files were found using '{rnx_glob_path}'"
            rnx_stations =  _pd.Series(rnx_files).str.extract(r'(\w{4})[^\/]+$',expand=False).to_list()
        return recent_logs_df[recent_logs_df.CODE.isin(rnx_stations).values]
    return recent_logs_df

def parse_igs_log(filename_array:_np.ndarray)->_np.ndarray:
    '''Parses igs log and outputs ndarray with parsed data
    Expects ndarray of the form [CODE DATE PATH]'''
    file_code, __, file_path = filename_array

    with open(file_path, 'rb') as file:
        data = file.read()

    blk_id = _REGEX_ID.search(data)
    if blk_id is None:
        tqdm.write(f'ID rejected from {file_path}')
        return _np.array([]).reshape(0,12)

    blk_id = [blk_id[1].decode().upper(),blk_id[2].decode().upper()] #no .groups() thus 1 and 2
    code = blk_id[0]
    if code!=file_code:
        tqdm.write(f'{code}!={file_code} at {file_path}')
        return _np.array([]).reshape(0,12)

    blk_loc = _REGEX_LOC.search(data)
    if blk_loc is None:
        tqdm.write(f'LOC rejected from {file_path}')
        return _np.array([]).reshape(0,12)

    blk_rec = _REGEX_REC.findall(data)
    if blk_rec == []:
        tqdm.write(f'REC rejected from {file_path}')
        return _np.array([]).reshape(0,12)

    blk_ant = _REGEX_ANT.findall(data)
    if blk_ant == []:
        tqdm.write(f'ANT rejected from {file_path}')
        return _np.array([]).reshape(0,12)

    blk_loc = [group.decode(encoding='utf8',errors='ignore') for group in blk_loc.groups()]
    blk_rec = _np.asarray(blk_rec,dtype=str)
    blk_ant = _np.asarray(blk_ant,dtype=str)

    len_recs = blk_rec.shape[0]
    len_ants = blk_ant.shape[0]

    blk_id_loc = _np.asarray([0] + blk_id + blk_loc,dtype=object)[_np.newaxis]

    code = [code]
    blk_rec = _np.concatenate([_np.asarray([1]*len_recs,dtype=object)[:,_np.newaxis],
                               _np.asarray(code*len_recs,dtype=object)[:,_np.newaxis],
                               blk_rec,],axis=1)
    blk_ant = _np.concatenate([_np.asarray([2]*len_ants,dtype=object)[:,_np.newaxis],
                               _np.asarray(code*len_ants,dtype=object)[:,_np.newaxis],
                               blk_ant,],axis=1)
    blk_uni = _np.concatenate([blk_id_loc,blk_rec,blk_ant],axis=0)
    file_path_arr = _np.asarray([file_path]*(1+len_ants+len_recs))[:,_np.newaxis]
    return _np.concatenate([blk_uni,file_path_arr],axis=1)


def igslogdate2datetime64(stacked_rec_ant_dt:_np.ndarray):
    '''2010-01-01T00:00
    - can be any non-space character. If parsing fails - None'''
    dt_array_float = (_pd.Series(stacked_rec_ant_dt)
                      .str.extract(pat=r'(\d{4})\S?(\d{2})\S?(\d+)\D?(?:(\d{1,2})\:(\d{1,2})\D?|)')
                      .values.astype(float))

    dt_array_float[_np.isnan(dt_array_float[:,0])] = [2100.,1.,1.,0.,0.]
    hh_mm = dt_array_float[:,[3,4]]
    hh_mm[_np.isnan(hh_mm[:,0])] = [0,0]
    dt_array_float[:,[3,4]] = hh_mm
    dt_array_int = dt_array_float.astype(int)

    wrong_31 = (_pd.Series(dt_array_int[:,1]).isin([4,6,9,11]).values) & (dt_array_int[:,2] > 30)
    wrong_30 = (dt_array_int[:,1]==2) & (_pd.Series(dt_array_int[:,2]) > 29)

    valid_mask =  ((dt_array_int[:,3]<24)
                & (dt_array_int[:,4] < 60)
                & (dt_array_int[:,2] < 32)
                & (dt_array_int[:,2] > 0)
                & (dt_array_int[:,1] < 13)
                & (dt_array_int[:,1] > 0)
                & ~wrong_31
                & ~wrong_30).values


    dt_datetime64 = _np.empty(dt_array_int.shape[0],dtype='datetime64[m]')
    dt_datetime64.fill(_np.NaN)

    df_dt_valid = _pd.DataFrame(dt_array_int[valid_mask],dtype=str)
    dt_datetime64[valid_mask] = (df_dt_valid[0].str.zfill(4)
                            + '-'+ df_dt_valid[1].str.zfill(2)
                            + '-'+ df_dt_valid[2].str.zfill(2)
                            + ' '+ df_dt_valid[3].str.zfill(2)
                            + ':'+ df_dt_valid[4].str.zfill(2) ).values.astype('datetime64')
    return dt_datetime64



def translate_series(series,translation):
    '''changes values in the series according to the dictionary of old_value-new_value'''
    series = series.copy()
    series.index = series.values
    series.update(translation)
    return series

def gather_metadata(logs_glob_path = '/data/station_logs/station_logs_IGS/*/*.log',
                    rnx_glob_path=None,
                    num_threads=1):
    '''parses logiles found with glob expression'''
    parsed_filenames = find_recent_logs(logs_glob_path=logs_glob_path,
                                        rnx_glob_path=rnx_glob_path).values

    total = parsed_filenames.shape[0]
    if num_threads == 1:
        gather = []
        for file in tqdm(parsed_filenames,miniters=total//100,total=total):
            gather.append(parse_igs_log(file))
    else:
        with _Pool(processes=num_threads) as pool:
            gather = list(tqdm(pool.imap_unordered(parse_igs_log, parsed_filenames),
                               total=total, miniters=total//100))

    gather_raw = _np.concatenate(gather)

    rec_ant_mask = gather_raw[:, 0] != 0  # id_loc = 0, rec = 1, ant = 2
    gather_id_loc = gather_raw[~rec_ant_mask][:, 1:]
    gather = gather_raw[rec_ant_mask]

    stacked_rec_ant_dt = _np.concatenate(
        [gather[:, -3], gather[:, -2]], axis=0)

    stacked_rec_ant_dt = igslogdate2datetime64(stacked_rec_ant_dt)
    snx_date = datetime2yydoysec(stacked_rec_ant_dt)

    gather = _np.concatenate([gather, snx_date.reshape(2, gather.shape[0]).T], axis=1)
    stacked_rec_ant_dt_beg_end = stacked_rec_ant_dt.reshape(2, gather.shape[0])  # also deals with nans as no equal sign
    # same foes for station start being empty as it becomes year 2100
    valid_mask_dt = stacked_rec_ant_dt_beg_end[0] < stacked_rec_ant_dt_beg_end[1]

    bad_dt_stations = _np.unique(gather[~valid_mask_dt][:, 1])

    rec_mask = gather[:, 0] == 1
    rec_df = _pd.DataFrame( _np.delete(arr=gather[rec_mask], axis=1, obj=[0, 6, 7, 8]),
                            columns=['CODE','RECEIVER','GNSS','S/N','FW','BEGIN_RAW','END_RAW',
                                    'PATH','BEGIN_SNX','END_SNX'])
    ant_df = _pd.DataFrame( gather[~rec_mask][:, 1:],
                            columns=['CODE','ANTENNA','RADOME','S/N','EccU','EccN','EccE',
                                    'RADOME2','BEGIN_RAW','END_RAW','PATH','BEGIN_SNX', 'END_SNX'])

    # ID LOC
    id_loc_df = _pd.DataFrame(gather_id_loc,columns=['CODE','DOMES_N','CITY','COUNTRY',
                                                    'X','Y','Z','LAT','LON','HEI','PATH'])

    id_loc_df.CITY[id_loc_df.CITY == ''] = 'N/A'
    id_loc_df.CITY = id_loc_df.CITY.str.rstrip().str.upper()
    id_loc_df.COUNTRY = translate_series(id_loc_df.COUNTRY.str.rstrip().str.upper(),
                                        translation_country).values
    id_loc_df.DOMES_N[id_loc_df.DOMES_N == ''] = '---------'


    xyz_array = (id_loc_df[['X','Y','Z']].stack()
                            .str.replace(',','.')
                            .replace({'':None})
                            .unstack().values.astype(float))


    valid_mask = _np.all((( xyz_array != 0) & ~_np.isnan(xyz_array)),axis=1)

    xyz_norm = (xyz_array[valid_mask] ** 2).sum(axis=1) **0.5

    # print(valid_mask.shape)
    # print(valid_mask[valid_mask == True].shape)
    # print(xyz_norm.shape)
    valid_mask[valid_mask] = (xyz_norm > 6000000) &(xyz_norm < 6500000)


    llh = xyz2llh_heik(xyz_array[valid_mask],deg=True)
    llh_snx = llh2snxdms(llh)

    llh2  = id_loc_df[~valid_mask][['LAT','LON','HEI']]
    llh2_snx = logllh2snxdms(llh2)
    snxdms = _np.empty(valid_mask.shape,dtype=object)
    snxdms[valid_mask] = llh_snx
    # snxdms[valid_mask] =' 000 00 00.0  00 00 00.0   000.0'
    snxdms[~valid_mask] = llh2_snx#
    # snxdms[~valid_mask] = ' 000 00 00.0  00 00 00.0   000.0'#llh2_snx
    # bad_loc_stations = id_loc_df.CODE[snxdms == ''].values
    id_loc_df['LOC'] = snxdms

    ecc = ant_df[['EccU','EccN','EccE']].values
    ecc[ecc == ''] = 0
    ant_df[['EccU','EccN','EccE']] = ecc.astype(float)

    rec_df.RECEIVER = rec_df.RECEIVER.str.rstrip().str.upper()
    ant_df.ANTENNA  = ant_df.ANTENNA.str.rstrip().str.upper()
    ant_df.RADOME   = ant_df.RADOME.str.rstrip().str.upper()
    ant_df.RADOME2  = ant_df.RADOME2.str.rstrip().str.upper()

    no_rad2_mask = ~ant_df.RADOME.isin(atx_rad_tbl)
    ant_df.RADOME[no_rad2_mask] = ant_df.RADOME2[no_rad2_mask]
    # translation_ant.index.name= None
    antennas = translate_series(ant_df.ANTENNA,translation_ant)
    invalid_ant_mask = ~antennas.index.isin(atx_ant_tbl)
    bad_antenna_stations = ant_df[invalid_ant_mask]['CODE'].unique()

    receivers = translate_series(rec_df.RECEIVER,translation_rec)
    invalid_rec_mask = ~receivers.index.isin(igs_rec_tbl)
    bad_rec_stations = rec_df[invalid_rec_mask]['CODE'].unique()

    radomes = translate_series(ant_df.RADOME,translation_rad)

    invalid_radomes_mask = ~radomes.index.isin(atx_rad_tbl)
    bad_radome_stations = ant_df[invalid_radomes_mask]['CODE'].unique()

    ant_df.ANTENNA  = antennas.values
    ant_df.RADOME   = radomes.values
    rec_df.RECEIVER = receivers.values

    bad_stations = _np.unique(bad_dt_stations.tolist()
                              + bad_radome_stations.tolist()
                              + bad_antenna_stations.tolist()
                              + bad_rec_stations.tolist())

    rec_df = rec_df[~rec_df.CODE.isin(bad_stations)].copy()
    ant_df = ant_df[~ant_df.CODE.isin(bad_stations)].copy()
    id_loc_df = id_loc_df[~id_loc_df.CODE.isin(bad_stations)].copy()

    return id_loc_df,rec_df,ant_df

def frame2snx_string(frame_of_day):
    '''frame_of_day dataframe to ESTIMATE sinex block'''
    code_pt =frame_of_day.index.to_series().str.split('_',expand=True)#.to_frame().values
    code_pt.columns = ['CODE','PT']
    frame_dt = j20002datetime(frame_of_day.attrs['REF_EPOCH'])
    frame = _pd.concat([frame_of_day,code_pt],axis=1)#.reindex()
    frame_est = frame[['STAX','STAY','STAZ']].stack(0).to_frame().reset_index(level=1)
    frame_est.columns = ['TYPE','EST']
    frame = frame_est.join(frame[['SOLN','CODE','PT']])

    dt_snx = datetime2yydoysec(frame_dt)

    frame.reset_index(drop=True,inplace=True)
    frame['STD'] = 0
    frame['INDEX'] = (frame.index +1)
    frame.SOLN =  frame.SOLN.apply(r'{:>4}'.format)

    epoch_str_series = (' ' + frame.CODE + '  ' + frame.PT + ' ' + frame.SOLN
    + ' C 00:000:00000 00:000:00000 ' + dt_snx + '\n').to_list()

    frame_str_series =(frame.INDEX.apply(r'{:6} '.format)
    + frame.TYPE + '   ' + frame.CODE + '  ' + frame.PT + ' '
    + frame.SOLN + ' ' + dt_snx + ' m    2 '
    + frame.EST.apply(r'{:>21.14E}'.format) + ' 0.00000E+00\n').to_list()

    buf = (
      ['*-------------------------------------------------------------------------------\n']
    + ['+SOLUTION/EPOCHS\n']
    + ['*CODE PT SOLN T _DATA_START_ __DATA_END__ _MEAN_EPOCH_\n']
    + epoch_str_series
    + ['-SOLUTION/EPOCHS\n']
    + ['*-------------------------------------------------------------------------------\n']
    + ['+SOLUTION/ESTIMATE\n']
    + ['*INDEX _TYPE_ CODE PT SOLN _REF_EPOCH__ UNIT S ___ESTIMATED_VALUE___ __STD_DEV__\n']
    + frame_str_series
    + ['-SOLUTION/ESTIMATE\n'])

    return buf

def meta2sting(id_loc_df,rec_df,ant_df):
    '''Converts three metadata dataframe to sinex blocks (string)'''
    rec_df['S/N'] = rec_df['S/N'].str.slice(0,5)
    rec_df['FW']  = rec_df['FW'].str.slice(0,11)

    ant_df['S/N'] = ant_df['S/N'].str.slice(0,5)
    ant_df.ANTENNA = ant_df.ANTENNA.str.ljust(15)#(r'{:15s}'.format)
    ant_df.RADOME = ant_df.RADOME.str.ljust(4)
    ant_df[['EccU','EccN','EccE']] = ant_df[['EccU','EccN','EccE']].applymap(r'{0:8.4f}'.format)

    location = (id_loc_df.CITY.str.slice(0,16).str.rstrip() + ', '
                + id_loc_df.COUNTRY).str.slice(0,22).str.ljust(22,' ').values

    id_str_list = (' ' + id_loc_df.CODE.values + '  A ' + id_loc_df.DOMES_N.values + ' P '
                   + location + id_loc_df.LOC.values + '\n').tolist()

    rec_str_list = (' ' + rec_df.CODE + '  A ---- P '
                    + rec_df.BEGIN_SNX + ' ' + rec_df.END_SNX + ' ' +
                    + rec_df.RECEIVER.str.ljust(20,' ') + ' '
                    + rec_df['S/N'].str.ljust(5,' ') + ' ' + rec_df.FW+'\n').to_list()

    ant_str_list = (' ' + ant_df.CODE + '  A ---- P '
                    + ant_df.BEGIN_SNX + ' ' + ant_df.END_SNX + ' '
                    + ant_df.ANTENNA   + ' ' + ant_df.RADOME  + ' '
                    + ant_df['S/N'] +'\n').to_list()

    ecc_str_list = (' ' + ant_df.CODE + '  A ---- P '
                    + ant_df.BEGIN_SNX + ' ' + ant_df.END_SNX
                    + ' UNE ' + ant_df.EccU +' '+ ant_df.EccN +' ' + ant_df.EccE +'\n').to_list()

    buf = (
      ['*-------------------------------------------------------------------------------\n']
    + ['+SITE/ID\n']
    + ['*CODE PT __DOMES__ T _STATION DESCRIPTION__ APPROX_LON_ APPROX_LAT_ _APP_H_\n']
    + id_str_list + ['-SITE/ID\n']

    + ['*-------------------------------------------------------------------------------\n']
    + ['+SITE/RECEIVER\n']
    + ['*SITE PT SOLN T DATA_START__ DATA_END____ DESCRIPTION_________ S/N__ FIRMWARE___\n']
    + rec_str_list + ['-SITE/RECEIVER\n']

    + ['*-------------------------------------------------------------------------------\n']
    + ['+SITE/ANTENNA\n' ]
    + ['*SITE PT SOLN T DATA_START__ DATA_END____ DESCRIPTION_________ S/N__\n']
    + ant_str_list + ['-SITE/ANTENNA\n' ]

    + ['*-------------------------------------------------------------------------------\n']
    + ['+SITE/ECCENTRICITY\n']
    + ['*SITE PT SOLN T DATA_START__ DATA_END____ AXE UP______ NORTH___ EAST____\n']
    + ecc_str_list + ['-SITE/ECCENTRICITY\n'])

    return buf

def write_meta_gather_master(logs_glob_path='/data/station_logs/*/*.log',
                             rnx_glob_path='/data/acs/pea/proc/exs/data/*.rnx',
                             frame_datetime=None,
                             frame_snx_path='/data/ITRF/itrf2014/ITRF2014-IGS-TRF.SNX.gz',
                             frame_soln_path='/data/ITRF/itrf2014/ITRF2014-soln-gnss.snx',
                             frame_psd_path='/data/ITRF/itrf2014/ITRF2014-psd-gnss.snx',
                             out_path = '/data/meta_gather.snx',
                             num_threads = None):

    if frame_datetime is None: 
        frame_datetime = _np.datetime64('today')
    else: frame_datetime =_np.datetime64(frame_datetime)

    id_loc_df, rec_df, ant_df = gather_metadata(logs_glob_path=logs_glob_path,
                                                rnx_glob_path=rnx_glob_path,
                                                num_threads=num_threads)

    sites_meta = rec_df.CODE.unique()

    gather_itrf = None
    if (frame_snx_path is not None) and (frame_soln_path is not None):
        gather_itrf = get_frame_of_day(frame_datetime,
                        itrf_path_or_df=frame_snx_path,
                        list_path_or_df=sites_meta,
                        discon_path_or_df = frame_soln_path,
                        psd_path_or_df    = frame_psd_path)


    buf = []
    #gen header
    gather_dt = datetime2yydoysec(frame_datetime)
    trf_header = ''
    if gather_itrf is not None:
        trf_header += (
            f'of which {gather_itrf.shape[0]} were projected to {frame_datetime} using:\n'
            +f'Frame: {frame_snx_path}\n'
            +f'Discon:{frame_soln_path}\n'
            +f'PSD:   {frame_psd_path}\n')


    buf.extend([f'%=SNX 2.01 IGS {gather_dt} IGS 00:000:00000 00:000:00000 P 00000 0\n'
            +'+FILE/REFERENCE\n'
            +'DESCRIPTION        merged metadata\n'
            +'OUTPUT             historical sinex header file\n'
            +'CONTACT            bogdan.matviichuk@ga.gov.au\n'
            +'SOFTWARE           LOG2SNX v0.1.2\n'
            +'HARDWARE           AWS\n'
            +'INPUT              igs ftp site logs\n'
            +'-FILE/REFERENCE\n'
            +'+FILE/COMMENT\n'
            +f'Metadata extracted from {sites_meta.shape[0]} station logs\n'
            + trf_header
            +'-FILE/COMMENT\n'
            +'+INPUT/ACKNOWLEDGMENTS\n'
            +'IGS International GNSS Service, GA\n'
            +'-INPUT/ACKNOWLEDGMENTS\n'])
    #ant/rec
    buf.extend(meta2sting(id_loc_df,rec_df,ant_df))
    #projected coordinates
    if gather_itrf is not None:
        buf.extend(frame2snx_string(gather_itrf))
    buf.extend([f'%ENDSNX\n'])

    with open(out_path,'w') as file:
        file.write(''.join(buf))
