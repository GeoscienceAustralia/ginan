'''Auxiliary functions'''
import logging as _logging
from typing import Union as _Union, Tuple as _Tuple

import numpy as _np
import pandas as _pd


def update_mindex(dataframe, lvl_name,loc=0,axis=1):
    '''Inserts a level named as lvl_name into dataframe df in loc position.
    Level can be inserted either in columns (default axis=1) or index (axis=0)'''

    mindex_df = dataframe.columns if axis == 1 else dataframe.index
    mindex_df =  mindex_df.to_frame(index=False)

    if loc == -1:
        loc = mindex_df.shape[1] #can insert below levels

    mindex_df.insert(loc = loc,column = 'add',value = lvl_name)
    mindex_df_updated = _pd.MultiIndex.from_arrays(mindex_df.values.T)

    if axis == 1:
        dataframe.columns = mindex_df_updated
    else:
        dataframe.index = mindex_df_updated
    return dataframe

def get_common_index(*dfs,level=None):
    index_sets = [set(df.index.values if level is None else df.index.levels[level].values) for df in dfs]
    return set.intersection(*index_sets)

def sync_snx_sites(*dfs):
    '''Finds common sites present in all gathers and outputs
    a list of gathers with common sites only'''
    sites = get_common_index(*dfs,level=0)
    # index.remove_unused_levels() may be required
    return [snx_df.loc[sites] for snx_df in dfs]

def code_pt_comboindex(vec):
    '''returns combo index as CODE + PT'''
    tmp_index = vec.index
    site_code = tmp_index.droplevel([1,2])
    site_pt   = tmp_index.droplevel([0,1])
    return _pd.Index(site_code.values + site_pt.values.astype(object))

def sync_pt_vec(vec1,vec2):
    '''returns sinex vectors synced on the common site name
    and takes care of PT monument type'''
    cindex1 = code_pt_comboindex(vec1)
    cindex2 = code_pt_comboindex(vec2)
    return vec1[cindex1.isin(cindex2)],vec2[cindex2.isin(cindex1)]

def unique_cols(df:_pd.DataFrame)->_np.ndarray:
    '''returns True for a df row with all duplicates'''
    a = df.to_numpy() # df.values (pandas<0.24)
    return (a[:,0][:,None] == a).all(1)

def rm_duplicates_df(df=_Union[_pd.DataFrame,_pd.Series],rm_nan_level:_Union[int,str,None]=None):
    """
    Takes in a clk/sp3/other dataframe and removes any duplicate indices. 
    Optionally, removes level_values from the index which contain NaNs 
    (useful for sp3 dataframes that need to be transformed to RAC).
    TODO Expand to level being a list

    Parameters
    ----------
    df: DataFrame or Series
        Requires at least 2-level MultiIndex
    remove_nan_levels : int or None, default None
        Switch to enable the removal of level keys with NaNs, any int or str is treated as a level to unstack and search for missing (NaN) values
    """
    if df.index.duplicated().sum() > 0:
        df = df[~df.index.duplicated()]

    if rm_nan_level is not None:
        attrs = df.attrs
        df_unstacked = df.unstack(level=rm_nan_level) # previous step insures successful unstacking
        cols2check = df_unstacked.columns.get_level_values(-1) # -1 is the recently unstacked level

        nan_mask = ~df_unstacked.set_axis(cols2check,axis=1).isna().sum(axis=0).sum(level=0).astype(bool)
        # if multiple cols with same name are present - sum(level=0) will group by same name

        if (~nan_mask).sum() != 0:
            sv_complete = nan_mask.index.values[nan_mask.values]
            _logging.warning(f'removed {nan_mask.index.values[~nan_mask.values]} as incomplete')
            df = df_unstacked.loc(axis=1)[:,:,sv_complete].stack(-1)
            df.attrs = attrs # copy over attrs which get lost in stack/unstack
            df.index = df.index.remove_unused_levels() # removed levels are still present in the index so remove

    return df

def sync_idx_dfs(df_a:_pd.DataFrame,df_b:_pd.DataFrame):
    """Finds common index between the two dataframes and returns filtered dataframes"""
    if not df_a.index.is_monotonic_increasing: df_a = df_a.sort_index(axis=0,inplace=False)
    if not df_b.index.is_monotonic_increasing: df_b = df_b.sort_index(axis=0,inplace=False)
    idx_a, idx_b = df_a.index, df_b.index

    n_levels = len(idx_a.levels)
    idx_levels = list(range(n_levels))

    mask_a, mask_b = [], []

    for i in range(n_levels):
        level_intersect = idx_a.levels[i].intersection(idx_b.levels[i])
        if len(level_intersect) == 0:
            raise RuntimeError(f"no common idx for level {i}")

        removed = idx_levels.pop(i) # pop the index that is assessed, so could use droplevel which is more efficient than get_level_values(i)

        mask_a.append(idx_a.droplevel(idx_levels).isin(level_intersect))
        mask_b.append(idx_b.droplevel(idx_levels).isin(level_intersect))

        idx_levels.insert(removed,i)

    df_a, df_b = df_a[_np.all(mask_a,axis=0)], df_b[_np.all(mask_b,axis=0)]
    df_a.index = df_b.index = df_a.index.remove_unused_levels() # sp3a.index equals sp3b.index at this point
    return df_a, df_b

def get_sampling(arr:_np.ndarray)->_Union[int,None]:
    """
    Simple function to compute sampling of the J2000 array

    Parameters
    ----------
    arr : ndarray of J2000 values
        returns a median of all the dt values which is a sampling. Checks if this value is close to integer seconds and returns None if not.
    """
    median_dt = _np.median(arr[1:] - arr[:-1])
    return int(median_dt) if (median_dt % 1) == 0 else None

def array_equal_unordered(a1:_np.ndarray,a2:_np.ndarray)->bool:
    """
    True if two arrays have the same shape and elements, False otherwise. Elements can be in any order within the two arrays.
    Use only for relatively small arrays as function uses lists sorting.

    Parameters
    ----------
    a1, a2 : array_like
        Input arrays.

    Returns
    -------
    b : bool
        Returns True if the arrays are equal.
    """
    if a1.shape == a2.shape:
        return sorted(a1.tolist()) == sorted(a2.tolist())
    else:
        _logging.debug(
            msg=f"array_equal_unordered:{a1.shape} and {a2.shape} shapes are different")
        return False


def rms(arr: _Union[_pd.DataFrame, _pd.Series], axis: _Union[None, int] = 0, level: _Union[None, int] = None)->_Union[_pd.Series,_pd.DataFrame]:
    """Trivial function to compute root mean square"""
    return (arr**2).mean(axis=axis, level=level)**0.5


def get_std_bounds(a: _np.ndarray, axis: _Union[None,int,_Tuple[int, ...]] = None, sigma_coeff: int = 3):
    """
    Returns the bounds across the the flattened array of along the specified axis/axes. 
    Adds a dimension to if axis is provided for convenience in case a was originally a 
    pandas DataFrame which could be then filtered using directly the returned bounds.

    Parameters
    ----------
    a : array_like
        Input array.
    axis : None or int, optional
        Axis or axes along which to compute the bounds. The default is to
        compute bounds over the flattened array.
    sigma_coeff : int
        Coefficient to multiply the computed sigma

    Returns
    -------
    bounds : array_like or scalar
        Returns array or single value of the absolute bound (mean + sigma_coeff*sigma) to be used for filtering.
    """
    bounds = _np.nanmean(a=a,axis=axis) + sigma_coeff*_np.nanstd(a=a,axis=axis)
    return bounds if axis is None else _np.expand_dims(a=bounds,axis=axis)