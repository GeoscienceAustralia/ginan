'''Auxiliary functions'''
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
