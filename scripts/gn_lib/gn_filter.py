'''Selection/Filter etc functions'''

import numpy as _np

def get_common_sites(*snx_dfs):
    '''Finds common sites present in all gathers and outputs
    a list of gathers with common sites only'''
    sites = snx_dfs[0].index.levels[0]
    for i in range(len(snx_dfs)-1):
        sites = _np.intersect1d(ar1=sites,ar2=snx_dfs[i+1].index.levels[0],assume_unique=True)
    snx_out = []
    for snx_df in snx_dfs:
        tmp = snx_df.loc[sites]
        tmp.index = tmp.index.remove_unused_levels()
        snx_out.append(tmp)
    return snx_out
