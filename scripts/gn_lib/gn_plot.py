import matplotlib.dates as _mdates
import matplotlib.units as _munits
import numpy as _np
from matplotlib import cm as _cm
import pandas as _pd

from .gn_const import J2000_ORIGIN as _J2000_ORIGIN


def plot_vec(df:_pd.DataFrame, axes:list, axes_idx:list,legend:bool=True):
    '''Function to plot columnar (single-lvl column names) dataframe to axes list 
    according to axes index provided (needed in case of non-alphabetic order of column names and axes desired). 
    Example snippet below:

    fig = plt.figure(figsize=(10,5)
    gs = fig.add_gridspec(3, hspace=0.2)
    ax = gs.subplots(sharex=True, sharey=False)
    plot_vec(axes=ax,df=a,axes_idx=[1,2,0])
    fig.savefig('blah.png') 
    '''
    converter = _mdates.DateConverter()
    _munits.registry[_np.datetime64] = converter
    locator = _mdates.HourLocator(interval=3)
    formatter = _mdates.ConciseDateFormatter(locator,show_offset=True) # offset with date at the bottom
    formatter.zero_formats[3] = '%d-%b' # the 00:00 label formatter

    df.index = df.index.values + _J2000_ORIGIN # converting J2000 seconds to datetime64
    components = df.columns.levels[0]
    sv_list = df.columns.levels[1]
    
    styl_list=['solid','dotted','dashed','dashdot']

    cmap = _cm.gist_rainbow #hsv in the original plotting script
    for i in range(len(axes_idx)):
        df_c = df[components[i]]
        
        for j in range(len(sv_list)):
            axes[axes_idx[i]].plot(df_c[sv_list[j]],label=sv_list[j] if (i==0 and legend)else '',
                       ls=styl_list[j % 4],color=cmap(j/len(sv_list)))

        axes[axes_idx[i]].set_ylabel(f'{components[i]} (cm)')

    axes[0].xaxis.set_major_locator(locator)
    axes[0].xaxis.set_major_formatter(formatter)

