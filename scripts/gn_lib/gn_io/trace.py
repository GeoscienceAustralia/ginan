'''TRACE file parser. Note the separate functions for values and residuals'''
import os as _os
import re as _re
from io import BytesIO as _BytesIO
from itertools import product
from multiprocessing import Pool as _Pool

import numpy as _np
import pandas as _pd

from ..gn_datetime import gpsweeksec2datetime as _gpsweeksec2datetime
from .common import path2bytes

_RE_TRACE = _re.compile(
    rb'POS,(.+)\n\$CLK,\d+,\d+\.\d+,\d,\d(.+)\n\$TROP,\d+,\d+\.\d+,\d,\d(.+)\n')
_RE_TRACE_HEAD = _re.compile(
    rb'station\s*\:\s*(.{4})\n\w+\s*\:\s*(.+|)\n\w+\s*\:\s*(.+|)\n\w+\s*\:\s*(\d)\n\w+\s*\:\s*(.+)')
_RE_TRACE_RES = _re.compile(
    rb"\+\sResiduals\n\*.+((?:\n.+)+)\n\-\sRes.+\n+DOING.+\nSigma.+\n(?:Deweighting.+\n|)\n\$POS\,(\d{4})\,(\d+)")
_RE_TRACE_LC = _re.compile(rb'PDE\sform\sLC.+((?:\n.+)+)')
_RE_EL = _re.compile(rb'\*2 PDE-CS GPST\s+\w+\s+(\d+)\s+(\d+).0\s+(\w\d\d)\s+(\d+.\d+)')

def _find_trace(output_path: str) -> ([], []):
    '''Scans output path for TRACE files'''
    station_names = set()
    trace_paths = []
    _re_station_name = _re.compile(r'\-(.{4})\d+.TRACE')

    for file in _os.scandir(path=output_path):
        if file.path.endswith('TRACE'):
            station_names.add(_re_station_name.findall(file.path)[0])
            trace_paths.append(file.path)

    station_names = sorted(station_names)
    trace_paths = sorted(trace_paths)
    return station_names, trace_paths

def _read_trace(trace_path, station_categories=None, drop_aux=True):
    '''Reads $POS, $CLK and $TROP lines from trace file. Outputs J2000 indexed df
    drop_aux=False will keep the original WEEK/Time_Of_Week values'''
    trace_content = path2bytes(trace_path)  # will accept .trace.Z also
    #print(trace_content)
    station_name = _RE_TRACE_HEAD.findall(string=trace_content)[
        0][0].decode(encoding='utf8').upper()
    #print("station name")
    #print(station_name)
    trace_list = _RE_TRACE.findall(string=trace_content)
    #print("Am I here")
    #print(trace_list)
    trace_df = _pd.DataFrame(trace_list).sum(axis=1).str.decode(encoding='utf8')\
        .str.rsplit(pat=',', expand=True).astype(
        {
            # POS
            0: _np.int16, 1: _np.float_, 2: _np.int8,
            3: _np.float_, 4: _np.float_, 5: _np.float_,
            6: _np.float_, 7: _np.float_, 8: _np.float_,
            # CLK
            9: _np.float_, 10: _np.float_, 11: _np.float_, 12: _np.float_,
            # TROP
            13: _np.float_, 14: _np.float_
        })

    datetime_j2000 = _gpsweeksec2datetime(
        gps_week=trace_df[0], tow=trace_df[1], as_j2000=True)

    station_name_array = _np.ndarray((trace_df.shape[0]), dtype='<U4')
    station_name_array.fill(station_name)
    station_index = _pd.CategoricalIndex(
        data=station_name_array,
        categories=station_categories if station_categories is not None else None)

    trace_df.columns = _pd.MultiIndex.from_arrays([
        ['AUX', 'AUX', 'AUX', 'EST', 'EST', 'EST', 'VAR', 'VAR',
            'VAR', 'EST', 'EST', 'VAR', 'VAR', 'EST', 'VAR'],
        ['WEEK', 'TOW', 'solStat', 'X', 'Y', 'Z', 'X', 'Y', 'Z', 'rClk', 'rClkG', 'rClk', 'rClkG', 'trop', 'trop']])

    if drop_aux:
        trace_df.drop(columns='AUX', level=0, inplace=True)
    return trace_df.set_index([station_index, datetime_j2000])

def _read_trace_res(trace_path, station_categories=None) -> _pd.DataFrame:
    '''Reads residuals from trace file (last block in each epoch.
    TODO: trace_level check, activate station categories switch, create gather func'''
    trace_content = path2bytes(trace_path)
    trace_res_list = _RE_TRACE_RES.findall(string=trace_content)
    res_array = _np.asarray(trace_res_list)

    count = _np.ndarray(res_array.shape[0], dtype=int)
    for i in range(res_array.shape[0]):
        count[i] = res_array[i][0].count(b'\n')

    gps_week_raw = res_array[:, 1].astype('timedelta64[W]')
    if _np.unique(gps_week_raw).shape[0] == 1:
        gps_week = _np.ndarray(count.sum(), dtype='timedelta64[W]')
        gps_week.fill(gps_week_raw[0])
    else:
        gps_week = _np.repeat(gps_week_raw, count)

    tow = _np.repeat(res_array[:, 2].astype('timedelta64[s]'), count)
    datetime_j2000 = _gpsweeksec2datetime(
        gps_week=gps_week, tow=tow, as_j2000=True)

    residuals = b''.join(res_array[:, 0].tolist())
    df_res = _pd.read_csv(_BytesIO(residuals), delim_whitespace=True, header=None, usecols=[1, 2, 3, 4, 5, 6, 7])\
        .astype({1: 'category', 2: 'category', 3: '<U1', 4: '<U2', 5: float, 6: float, 7: float})
    df_res[3] = (df_res[3] + df_res[4]).astype('category')  # L 5 -> L5 etc

    df_res.drop(columns=4, inplace=True)
    df_res.columns = ['CODE', 'PRN', 'OBS', 'PREFIT', 'POSTFIT', 'VAR']
    return df_res.set_index(['OBS', datetime_j2000])

def gather_trace(output_path, n_threads=1, drop_aux=True):
    '''Parses all available trace files in the output path and outputs
     a single dataframe. Residuals are ignored here'''
    station_names, trace_paths = _find_trace(output_path)
    n_traces = len(trace_paths)
    n_threads = n_traces if n_threads > n_traces else n_threads

    if n_threads == 1:
        trace_gather = []
        for trace in trace_paths:
            trace_gather.append(_read_trace(
                trace_path=trace, station_categories=station_names))
    else:
        with _Pool(processes=n_threads) as _p:
            trace_gather = _p.starmap(_read_trace, product(
                trace_paths, [station_names], [drop_aux]))

    return _pd.concat(trace_gather, axis=0)

def _read_trace_LC(trace_path):
    '''Parses the LC combo block of the trace files producing
     a single dataframe. WORK-IN-PROGRESS'''    
    # regex search string
    trace_content = path2bytes(trace_path)
    trace_LC_list = _RE_TRACE_LC.findall(string=trace_content)
    LC_bytes = b''.join(trace_LC_list)
    LC_bytes = LC_bytes.replace(b'=',b'') #getting rif of '='
    
    df_LC = _pd.read_csv(_BytesIO(LC_bytes),delim_whitespace=True,header=None,usecols=[1,2,4,6,8,9,10,11,12,13]).astype(
        {
            1: _np.int16, 2:_np.int32, 4: '<U3',
            6: '<U1', 8: '<U4',
            9: _np.float_, 10: '<U4', 11: _np.float_,
            12: '<U4', 13: _np.float_
        })
    
    df_LC.columns = ['W','S','PRN','LP',8,9,10,11,12,13]
    df_LC['time'] = _gpsweeksec2datetime(gps_week = df_LC.W, 
                                                tow = df_LC.S, 
                                                as_j2000=True)
    df_LC.drop(columns=['W','S'],inplace=True)

    df1 = df_LC[['time','PRN','LP',8,9]]
    df1.columns = ['time','PRN','LP','combo','value']

    df2 = df_LC[['time','PRN','LP',10,11]]
    df2.columns = ['time','PRN','LP','combo','value']

    df3 = df_LC[['time','PRN','LP',12,13]]
    df3.columns = ['time','PRN','LP','combo','value']

    df_LC = _pd.concat([df1,df2,df3],axis=0)
    
    
    return df_LC.set_index(['time'])

def _read_trace_el(trace_path):
    "Get elevation angles for satellites from trace file"
    trace_content = path2bytes(trace_path)
    trace_EL_list = _RE_EL.findall(string=trace_content)
    
    el_df = _pd.DataFrame(trace_EL_list).astype({0:_np.int16, 1:_np.int32, 2:bytes, 3:_np.float})
    el_df[2] = el_df[2].str.decode("utf-8")
    el_df['time'] = _gpsweeksec2datetime(gps_week=el_df[0], tow=el_df[1], as_j2000=True)
    el_df.drop(columns=[0,1],inplace=True)
    el_df.columns = ['PRN','el','time']
    
    return el_df.set_index(['time'])
