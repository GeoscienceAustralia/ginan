'''IO functions for reading RINEX files '''

import re as _re
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd

from ..gn_datetime import datetime2j2000 as _datetime2j2000
from .common import path2bytes

_RE_RNX = _re.compile(rb'^\>(.+)\n((?:[^\>]+)+)',_re.MULTILINE)
_RE_RNX_HEADER = _re.compile(rb'\n(\w)\s+(\d+)(.+)OBS\sTYPES((?:\W\s{6}.+|))')
_RE_RNX_POSITION = _re.compile(rb'\n(.+)APPROX\sPOSITION\sXYZ')

def _read_rnx(rnx_path):
    '''Read RINEX file into pandas DataFrame taking into account
    the signal strength and loss-of-lock field keys.
    Assumes that rinex had been previously Hatanaka decompressed'''
    rnx_content = path2bytes(str(rnx_path))
    data_blocks = _np.asarray(_RE_RNX.findall(string=rnx_content))
    header_blocks = _RE_RNX_HEADER.findall(string=rnx_content)

    header_blocks = _pd.DataFrame(_np.asarray(header_blocks).astype(str))
    rnx_head = (header_blocks[2] + header_blocks[3]).str.extractall(pat=r'(\w\d\w)',).squeeze()

    cols = rnx_head.count(level=0).max()

    rnx_head = rnx_head.unstack().set_index(header_blocks[0])
    dates = data_blocks[:,0]
    data = data_blocks[:,1]
    counts = _np.char.count(data, b'\n')

    epochs_dt = _pd.to_datetime(_pd.Series(dates).str.slice(1,20).values.astype(str),
                                format=r'%Y %m %d %H %M %S')

    dt_index = _np.repeat(a=_datetime2j2000(epochs_dt),repeats=counts)
    b_string = b''.join(data.tolist())

    b_series = _pd.Series(b_string.splitlines())

    data_raw = b_series.str[3:]
    missing = (16*cols - data_raw.str.len()).values.astype(object) #has to be square for method to work

    m = (data_raw.values + missing*b' ').astype(bytes).view('S16').reshape((cols,-1))
    rnx_df = rnx_vals2df(m)
    prn = b_series.str[:3].values.astype(str)
    prn_code = prn.astype('U1')
    rnx_df = rnx_df.set_index([dt_index,prn])
    rnx_df.columns = _pd.MultiIndex.from_product([list(range(cols)),['EST','STRG']])

    buf = []
    for constel in rnx_head.index:
        gnss_df = rnx_head.loc[constel]
        n_cols_gnss = gnss_df[~gnss_df.isna()].shape[0]
        gnss_rnx_df = rnx_df[(prn_code == gnss_df.name)]
        valid_columns = (gnss_rnx_df.loc(axis=1)[:,'EST']
                            .columns.get_level_values(0)[:n_cols_gnss])

        gnss_rnx_df = gnss_rnx_df[valid_columns].copy()
        gnss_rnx_df.columns = _pd.MultiIndex.from_product([gnss_df[~gnss_df.isna()].to_list(),['EST','STRG']])
        buf.append(gnss_rnx_df)
    return _pd.concat(buf,keys=rnx_head.index,axis=0)

def rnx_vals2df(m):
    m_flat = m.flatten()
    t1 = m_flat.astype('S14')[:,_np.newaxis]
    t1[t1 == b'              ']= b''
    t2 = m_flat.astype(bytes).view(('S1')).reshape(m_flat.shape[0],-1)[:,15:16]
    t2[t2 == b' '] = b''
    t3 = _np.hstack([t1,t2]).astype(object)
    t3[t3 == b'']= _np.NaN
    rnx_df = _pd.DataFrame(t3.astype(float,).reshape([m.shape[1], m.shape[0]*2]))
    return rnx_df

def _rnx_pos(rnx_path):
    '''Read RINEX file and output APPROX POSITION'''
    rnx_content = path2bytes(str(rnx_path))
    pos_line = _RE_RNX_POSITION.findall(string=rnx_content)
    coords = []
    for val in pos_line[0].decode("utf-8").split(' '):
        try:
            coords.append(float(val))
        except ValueError:
            continue
    return _np.array(coords).reshape(3,1)