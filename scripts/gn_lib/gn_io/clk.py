'''RINEX CLK file parsing function'''
import sys as _sys
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd
import re as _re

from ..gn_datetime import _J2000_ORIGIN
from .common import path2bytes

CLK_TYPE_CATEGORY = _pd.CategoricalDtype(['CR','DR','AR','AS','MS'])

_RE_LINE = _re.compile(rb'(AS[ ]G.+)') # GPS SV line (other GNSS may not have STD)

def read_clk(clk_path):
    content = path2bytes(str(clk_path))
    data_b = content.find(b'END OF HEADER')+13
    data_b += content[data_b:data_b+20].find(b'\n') + 1

    data = content[data_b:]
    data_line = _RE_LINE.search(data)
    assert data_line is not None
    
    len_line = len(data_line.groups()[0]) # need to get a line and check the length

    clk_cols = [0,1,2,3,4,5,6,7,9]
    clk_names =['A','CODE','Y','M','D','h','m','s','EST']
    if len_line > 59: # if len over 59 -> expect STD column presence
        clk_cols += [10]
        clk_names +=['STD']

    clk_df = _pd.read_csv(_BytesIO(data),
                delim_whitespace=True,header=None,usecols=clk_cols,names = clk_names,# type:ignore
                dtype = {'A':CLK_TYPE_CATEGORY,'CODE':object,
                         'Y':_np.uint16,'M':_np.uint16,'D':_np.uint16,
                         'h':_np.int32,'m':_np.int32,'s':_np.float_,})

    date = (((clk_df.Y.values - 1970).astype('datetime64[Y]').astype('datetime64[M]') 
            + clk_df.M.values - 1).astype('datetime64[D]')
            + clk_df.D.values - 1)

    time = (clk_df.h.values * 3600 + clk_df.m.values * 60 + clk_df.s.values).astype('timedelta64[s]')

    j2000time =  (date + time - _J2000_ORIGIN).astype(int)
    clk_df.drop(columns=['Y','M','D','h','m','s'],inplace=True)
    clk_df.set_index(['A',j2000time,'CODE'],inplace=True)
    clk_df.index.names = (['A','J2000','CODE'])
    return clk_df

def norm_clk(clk_unst,sv):
    '''Takes an unstacked clk_df and normalizes satellite data (AS) by \
    a set of satellite values using a specified name (e.g. G01)'''
    norm = clk_unst.values - _np.broadcast_to(clk_unst[sv].values[:,None],clk_unst.shape) #lots faster then stacking columns
    return _pd.DataFrame(norm,index=clk_unst.index,columns=clk_unst.columns)