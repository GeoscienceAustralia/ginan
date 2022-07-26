import matplotlib.dates as _mdates
import matplotlib.units as _munits
import numpy as _np
import pandas as _pd
import plotext as plx
from matplotlib import cm as _cm

from gn_lib.gn_const import J2000_ORIGIN as _J2000_ORIGIN
from gn_lib.gn_datetime import j20002datetime as _j20002datetime


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
    hours = (df.index.values[-1] - df.index.values[0])//3600
    locator = _mdates.HourLocator(interval=(hours//24 + 1)*3)
    formatter = _mdates.ConciseDateFormatter(locator,show_offset=True) # offset with date at the bottom
    formatter.zero_formats[3] = '%d-%b' # the 00:00 label formatter

    df.index = _j20002datetime(df.index.values) # converting J2000 seconds to datetime64
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

def diff2plot(diff, kind=None,title='Unnamed plot'):
    """Function to plot graph to the terminal. Can be scatter or bar plot (Initial test functionality)
    Works only with plotext 4.2, not above"""
    # try:
    #     import plotext as plt
    # except ModuleNotFoundError:
    #     # Error handling
    #     pass
    plx.clear_plot()
    diff = diff.round(2)
    if kind == 'bar':
        # expects a series with index being string names
        mask0 = (diff.values != 0) & ~_np.isnan(diff.values)
        if mask0.any():
            plx.bar(diff.index.values[mask0].tolist(), diff.values[mask0].tolist(),orientation = "h",width=0.3)
            plx.vertical_line(coordinate = 0)
            plx.plotsize(100,diff.shape[0]*2 + 3)
        else:
            return None
    else:
        mask0 = ((diff.values != 0) & ~_np.isnan(diff.values)).any(axis=0)
        if mask0.any():
            cols = diff.columns[mask0]
            x1 = plx.datetime.datetime_to_string(diff.index.astype('timedelta64[ns]')*1000000000 + _J2000_ORIGIN)
            for i in range(cols.shape[0]):
                plx.scatter_date(x1, diff[cols[i]].to_list(), color=i,marker = "hd",label=diff.columns[i])
                plx.plotsize(100,30 + 3)
        else:
            return None

    plx.title(title)
    plx.limit_size(limit_xsize=False,limit_ysize=False)
    plx.show()
