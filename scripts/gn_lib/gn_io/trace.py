'''TRACE file parser. Note the separate functions for values and residuals'''
import logging as _logging
import os as _os
import re as _re
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd

from gn_lib import gn_const as _gn_const
from gn_lib import gn_datetime as _gn_datetime
from gn_lib import gn_io as _gn_io

def _trace_extract(path_or_bytes,blk_name):
    # 'States', 'Residuals'
    trace_bytes = _gn_io.common.path2bytes(path_or_bytes) #path2bytes passes through bytes 

    begin = end = 0
    buf=[]

    blk_begin = (f'+ {blk_name}').encode()
    blk_end   = (f'- {blk_name}').encode()

    while True:
        begin = trace_bytes.find(blk_begin,end)
        begin_full =  trace_bytes.find(b'\n',begin)
        if begin==-1:
            break
        end = trace_bytes.find(blk_end,begin_full)

        blk_content = trace_bytes[begin_full+1:end]         # needs +1 not to start with '\n'
        blk_type = b'\t' + trace_bytes[begin+2:begin_full] + b'\n'	# needs +2 to remove ' +'
        blk_content_w_type = blk_type.join(blk_content.splitlines()) + blk_type
        buf.append(blk_content_w_type)

    content = b''.join(buf)
    if len(content) == 0:
        _logging.error(f'"{blk_name}" data not found')
        return None
    return content

def _read_trace_states(path_or_bytes):
    states = _trace_extract(path_or_bytes,blk_name='States')
    if states is None:
        return None
    df = _pd.read_csv(_BytesIO(states),delimiter='\t',usecols=[1,2,3,4,5,6,7,8,10],skipinitialspace=True,dtype={'SAT':_gn_const.PRN_CATEGORY,'TYPE':_gn_const.STATE_TYPES_CATEGORY},keep_default_na=False,
                    comment='#',header=None,names = ['TIME','TYPE','SITE','SAT','NUM','EST','VAR','ADJ','BLK'],parse_dates=['TIME']) # type:ignore
    df.TIME = _gn_datetime.datetime2j2000(df.TIME.values)

    empty_mask = df.TYPE.values.notna() # dropping ONE type
    if (~empty_mask).sum()>0:
        df = df[empty_mask]

    return df.set_index(['TIME','SITE','TYPE','SAT','NUM','BLK'])

def _read_trace_residuals(path_or_bytes,it_max_only=True):
    residuals = _trace_extract(path_or_bytes,blk_name='Residuals')
    if residuals is None:
        return None
    df = _pd.read_csv(_BytesIO(residuals),delimiter='\t',comment='#',header=None,usecols=[1,2,3,4,5,6,7,8,9],skipinitialspace=True,keep_default_na=False,
            names = ['It','TIME','SITE','SAT','TYPE','PREFIT','POSTFIT','STD','BLK'],parse_dates=['TIME'],dtype={'It':int,'SAT':_gn_const.PRN_CATEGORY}) # type:ignore
    df.TIME = _gn_datetime.datetime2j2000(df.TIME.values)

    empty_mask = df.SITE.values.astype(bool) # may be removed in the future when the pivot is removed from PEA
    if (~empty_mask).sum()>0:
        df = df[empty_mask]
        
    if not it_max_only:
        return df.set_index(['TIME','SITE','TYPE','SAT','BLK'])
    # to get max_ind values pandas >= 1.1 is required
    it_max_ind=df[['TIME','It']].groupby(['TIME']).max().reset_index().values.tolist()
    return df.set_index(['TIME','It']).loc[it_max_ind].reset_index().set_index(['TIME','SITE','TYPE','SAT','BLK'])



_RE_TRACE_HEAD = _re.compile(
    rb'station\s*\:\s*(.{4})\n\w+\s*\:\s*(.+|)\n\w+\s*\:\s*(.+|)\n\w+\s*\:\s*(\d)\n\w+\s*\:\s*(.+)')
_RE_TRACE_LC = _re.compile(rb'PDE\sform\sLC.+((?:\n.+)+)')
_RE_EL = _re.compile(rb'\*2 PDE-CS GPST\s+\w+\s+(\d+)\s+(\d+).0\s+(\w\d\d)\s+(\d+.\d+)')

def _find_trace(output_path: str) -> tuple:
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

def _read_trace_LC(path_or_bytes):
    '''Parses the LC combo block of the trace files producing
     a single dataframe. WORK-IN-PROGRESS'''    
    # regex search string
    if isinstance(path_or_bytes, str):
        trace_content = _gn_io.common.path2bytes(path_or_bytes) # will accept .trace.Z also
    else:
        trace_content = path_or_bytes
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
    df_LC['time'] = _gn_datetime.gpsweeksec2datetime(gps_week = df_LC.W, 
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

def _read_trace_el(path_or_bytes):
    "Get elevation angles for satellites from trace file"
    if isinstance(path_or_bytes, str):
        trace_content = _gn_io.common.path2bytes(path_or_bytes) # will accept .trace.Z also
    else:
        trace_content = path_or_bytes
    trace_EL_list = _RE_EL.findall(string=trace_content)
    
    el_df = _pd.DataFrame(trace_EL_list).astype({0:_np.int16, 1:_np.int32, 2:bytes, 3:_np.float})
    el_df[2] = el_df[2].str.decode("utf-8")
    el_df['time'] = _gn_datetime.gpsweeksec2datetime(gps_week=el_df[0], tow=el_df[1], as_j2000=True)
    el_df.drop(columns=[0,1],inplace=True)
    el_df.columns = ['PRN','el','time']
    return el_df.set_index(['time'])
