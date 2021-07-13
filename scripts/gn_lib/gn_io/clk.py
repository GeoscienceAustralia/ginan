'''RINEX CLK file parsing function'''
import sys as _sys
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd

from ..gn_datetime import _J2000_ORIGIN
from .common import path2bytes

PYGINANPATH = '/data/acs/pea/python/source/'
if PYGINANPATH not in _sys.path:
    _sys.path.insert(0, PYGINANPATH)

CLK_TYPE_CATEGORY = _pd.CategoricalDtype(['CR','DR','AR','AS','MS'])
def read_clk(clk_path):
    content = path2bytes(str(clk_path))
    data_b = content.find(b'END OF HEADER')+13
    data_b += content[data_b:data_b+20].find(b'\n') + 1

    # header = content[:data_b]
    clk_df = _pd.read_csv(_BytesIO(content[data_b:]),
                delim_whitespace=True,header=None,usecols=[0,1,2,3,4,5,6,7,9,10],
                names = ['A', 'CODE', 'Y', 'M', 'D', 'h', 'm', 's', 'EST', 'STD'],
                dtype = {'A':CLK_TYPE_CATEGORY,'CODE':object,
                         'Y':_np.uint16,'M':_np.uint16,'D':_np.uint16,
                         'h':_np.int32,'m':_np.int32,'s':_np.float_,}
                )
    date = (((clk_df.Y.values - 1970).astype('datetime64[Y]').astype('datetime64[M]') 
            + clk_df.M.values - 1).astype('datetime64[D]')
            + clk_df.D.values - 1)

    time = (clk_df.h.values * 3600 + clk_df.m.values * 60 + clk_df.s.values).astype('timedelta64[s]')

    j2000time =  (date + time - _J2000_ORIGIN).astype(int)
    clk_df.drop(columns=['Y','M','D','h','m','s'],inplace=True)
    clk_df.set_index(['A',j2000time,'CODE'],inplace=True)
    clk_df.index.names = ([None,None,None])
    return clk_df
