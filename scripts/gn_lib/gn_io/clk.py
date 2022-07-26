'''RINEX CLK file parsing function'''
import re as _re
import sys as _sys
from io import BytesIO as _BytesIO
from typing import Union as _Union

import numpy as _np
import pandas as _pd

from gn_lib import gn_const as _gn_const
from gn_lib import gn_datetime as _gn_datetime
from gn_lib import gn_io as _gn_io

_RE_LINE = _re.compile(rb'(AS[ ]G.+)') # GPS SV line (other GNSS may not have STD)

def read_clk(clk_path):
    content = _gn_io.common.path2bytes(str(clk_path))
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
                dtype = {'A':_gn_const.CLK_TYPE_CATEGORY,'CODE':object,
                         'Y':_np.uint16,'M':_np.uint16,'D':_np.uint16,
                         'h':_np.int32,'m':_np.int32,'s':_np.float_,})

    date = (((clk_df.Y.values - 1970).astype('datetime64[Y]').astype('datetime64[M]') 
            + clk_df.M.values - 1).astype('datetime64[D]')
            + clk_df.D.values - 1)

    time = (clk_df.h.values * 3600 + clk_df.m.values * 60 + clk_df.s.values).astype('timedelta64[s]')

    j2000time =  _gn_datetime.datetime2j2000(date + time)
    clk_df.drop(columns=['Y','M','D','h','m','s'],inplace=True)
    clk_df.set_index(['A',j2000time,'CODE'],inplace=True)
    clk_df.index.names = (['A','J2000','CODE'])
    return clk_df

def get_AS_entries(clk_df):
    # fastest method to grab a specific category!same as clk_df.EST.loc['AS'] but >6 times faster
    AS_cat_code = clk_df.index.levels[0].categories.get_loc('AS')
    mask = clk_df.index.codes[0] == AS_cat_code
    return _pd.Series(data=clk_df.values[:,0][mask],index=clk_df.index.droplevel(0)[mask])

def rm_epoch_gnss_bias(clk_df_unst:_pd.DataFrame):
    cols_original = clk_df_unst.columns.values
    cols_gnss = cols_original.astype('<U1')
    clk_df_unst.columns = cols_gnss
    clk_df_unst -= clk_df_unst.mean(axis=1,level=0).reindex(cols_gnss,axis=1).values
    clk_df_unst.columns = cols_original

def rm_daily_sv_bias(clk_df_unst:_pd.DataFrame):
    idx_original = clk_df_unst.index.values
    idx_days = _gn_datetime.j20002j2000days(idx_original)
    clk_df_unst.index = idx_days
    clk_df_unst -= clk_df_unst.mean(axis=0,level=0).reindex(idx_days).values
    clk_df_unst.index = idx_original

def rm_sv_bias(clk_df_unst:_pd.DataFrame,sv:_Union[list,str,_np.ndarray]):
    '''Takes an unstacked clk_df and normalizes satellite data (AS) by \
    a set satellite clk offsets, specific to constellation - G01 for GPS, R01 per GLONASS etc that are taken from the per_gnss_svs list
    
    Takes an unstacked clk_df and normalizes satellite data (AS) by \
    a set of satellite values using a specified name (e.g. G01)
    '''
    if type(sv) in [list,_np.ndarray]:
        svs_df = clk_df_unst[sv]
        svs_df.columns = svs_df.columns.values.astype('<U1')
        clk_df_unst -= svs_df.reindex(clk_df_unst.columns.values.astype('<U1'),axis=1).values
    elif type(sv) == str:
        clk_df_unst -= _np.broadcast_to(clk_df_unst[sv].values[:,None],clk_df_unst.shape) #lots faster then stacking columns
    else:
        raise ValueError('check normalization arguments')


def rm_clk_bias(clk_df,norm_type='both'):
    if (type(norm_type) in [_np.ndarray,list]) or (norm_type in _gn_const.PRN_CATEGORY.categories):
        rm_sv_bias(clk_df,sv=norm_type)
    elif norm_type == 'both':
        rm_daily_sv_bias(clk_df)
        rm_epoch_gnss_bias(clk_df)
    elif norm_type == 'daily':
        rm_daily_sv_bias(clk_df)
    elif norm_type == 'epoch':
        rm_epoch_gnss_bias(clk_df)

def select_norm_svs_per_gnss(clk_a_unst:_pd.DataFrame,clk_b_unst:_pd.DataFrame)->_np.ndarray:
    """
    Selects best common SVs per GNSS across two unstacked clk DataFrames e.g., G01 for GPS, R01 for GLO, E01 for GAL etc.
    Procedure is based on the sum of STDs of each satellite clk offsets over the two DataFrames.
    In addition, the selected SVs must not have gaps.
    TODO might select SVs with smallest number of gaps if no single SV with continuous data is present.
    
    Parameters
    ----------
    clk_a_unst : unstacked clk dataframe a
        Input DataFrame a.
    clk_b_unst : unstacked clk dataframe b
        Input DataFrame b.

    Returns
    -------
    bounds : ndarray
        Returns array of single SV per constellation which are the best for normalisation.
    """
    sum_std = clk_a_unst.std() + clk_b_unst.std()
    sum_std.index = [sum_std.index,sum_std.index.values.astype('<U1')]
    min_std_sum = sum_std.min(level=[1,0])
    mask = ~min_std_sum.index.droplevel(1).duplicated(keep='first')
    sv_selected = min_std_sum[mask].index.droplevel(0).values
    return sv_selected