'''Ephemeris functions'''
import os as _os
import re as _re

import numpy as _np
import pandas as _pd

from ..gn_aux import unique_cols as _unique_cols
from ..gn_const import C_LIGHT as _C_LIGHT, SISRE_COEF_DF as _SISRE_COEF_DF
from ..gn_datetime import datetime2gpsweeksec as _datetime2gpsweeksec
from ..gn_datetime import datetime2j2000 as _datetime2j2000
from ..gn_datetime import datetime2mjd as _datetime2mjd
from ..gn_datetime import j20002rnxdt as _j20002rnxdt
from ..gn_transform import ecef2eci as _ecef2eci
from ..gn_transform import eci2rac_rot as _eci2rac_rot
from ..gn_transform import get_helmert7 as _get_helmert7
from ..gn_transform import transform7 as _transform7
from .clk import read_clk as _read_clk, norm_clk as _norm_clk
from .common import path2bytes

_RE_SP3 = _re.compile(rb'^\*(.+)\n((?:[^\*]+)+)',_re.MULTILINE)

# 1st line parser. ^ is start of document, search
_RE_SP3_HEAD = _re.compile(rb'''^\#(\w)(\w)([\d \.]{28})[ ]+
                                    (\d+)[ ]+([\w\+]+)[ ]+(\w+)[ ]+
                                    (\w+)[ ]+(\w+)''',_re.VERBOSE)
#SV names. multiline, findall
_RE_SP3_HEAD_SV = _re.compile(rb'^\+[ ]+(?:[\d]+|)[ ]+((?:[A-Z]\d{2})+)\W',_re.MULTILINE)
# orbits accuracy codes
_RE_SP3_ACC = _re.compile(rb'^\+{2}[ ]+(.{51})\W',_re.MULTILINE)
# File descriptor and clock
_RE_SP3_HEAD_FDESCR = _re.compile(rb'\%c[ ](\w{1})[ ]+cc[ ](\w{3})')

def nanflags2nans(sp3_df):
    nan_mask = sp3_df.iloc[:,1:5].values >= 999999
    nans = nan_mask.astype(float)
    nans[nan_mask] = _np.NAN
    sp3_df.iloc[:,1:5] = sp3_df.iloc[:,1:5].values+nans

def mapparm(old,new):
    oldlen = old[1] - old[0]
    newlen = new[1] - new[0]
    off = (old[1]*new[0] - old[0]*new[1])/oldlen
    scl = newlen/oldlen
    return off, scl

def read_sp3(sp3_path):
    '''Read SP3 file
    Returns STD values converted to proper units (mm/ps) also if present
    # if rinex name -> PG01 to G01 and filter on P
    '''
    content = path2bytes(sp3_path)
    header_end = content.find(b'/*')

    header = content[:header_end]
    content = content[header_end:]

    parsed_header =  parse_sp3_header(header)
    
    fline_b = header.find(b'%f') + 2 #TODO add to header parser
    fline = header[fline_b:fline_b+24].strip().split(b'  ')
    base_xyzc = _np.asarray([float(fline[0])]*3 + [float(fline[1])]) #exponent base

    data_blocks = _np.asarray(_RE_SP3.findall(string=content[:content.rfind(b'EOF')]))

    dates = data_blocks[:,0]
    data = data_blocks[:,1]
    counts = _np.char.count(data, b'\n')

    epochs_dt = _pd.to_datetime(_pd.Series(dates).str.slice(2,21).values.astype(str),
                                format=r'%Y %m %d %H %M %S')

    dt_index = _np.repeat(a=_datetime2j2000(epochs_dt.values),repeats=counts)
    b_string = b''.join(data.tolist())


    series = _pd.Series(b_string.splitlines())
    data_width =  series.str.len()

    missing = b' '*(data_width.max() - data_width).values.astype(object)
    series += missing #rows need to be of equal len 

    idx = series.str[0 if parsed_header.HEAD.loc['PV_FLAG']!='P' else 1:4].values.astype(str).astype(object)


    data_test = series.str[4:60].values.astype('S56').view(('S14')).reshape(series.shape[0],-1).astype(float)

    if data_width.max() > 60:
        std = (series.str[60:69].values+series.str[70:73].values).astype('S12').view('S3').astype(object)
        std[std == b'   '] = None
        std = std.astype(float).reshape(series.shape[0],-1)

        ind = (series.str[75:76].values + series.str[79:80].values).astype('S2').view('S1')
        ind[ind == b' '] = b''

        ind = ind.reshape(series.shape[0],-1).astype(str)

        sp3_df = _pd.DataFrame(_np.column_stack([idx,data_test,std,ind]),).astype(
        {0:"category",1:float,2:float,3:float,4:float,
              5:float,6:float,7:float,8:float,
             9:"category",10:"category"})
        sp3_df.columns = ([['SAT','EST','EST','EST','EST','STD','STD','STD','STD','P_XYZ','P_CLK'],
                       ['','X','Y','Z','CLK','X','Y','Z','CLK','','']])
        sp3_df.STD = base_xyzc ** sp3_df.STD.values
    else:
        sp3_df = _pd.DataFrame(_np.column_stack([idx,data_test]),).astype(
        {0:"category",1:float,2:float,3:float,4:float})
        sp3_df.columns = ([['SAT','EST','EST','EST','EST'],
                           ['','X','Y','Z','CLK',]])

    #999999* None value flag to None
    nanflags2nans(sp3_df)
    # writing header data to dataframe attributes
    sp3_df.attrs['HEADER'] = parsed_header

    sp3_df.set_index([dt_index,'SAT'],inplace=True)
    sp3_df.index.names = ([None,None])
    sp3_df.attrs['path'] = sp3_path
    return sp3_df

def parse_sp3_header(header):
    sp3_heading = list(map( bytes.decode,
                            _RE_SP3_HEAD.search(header).groups()
                          + _RE_SP3_HEAD_FDESCR.search(header).groups()))
    sp3_heading = _pd.Series(sp3_heading,index = ['VERSION','PV_FLAG','DATETIME','N_EPOCHS',
                            'DATA_USED','COORD_SYS','ORB_TYPE','AC','FILE_TYPE','TIME_SYS'])
    head_svs = (_np.asarray(b''.join(_RE_SP3_HEAD_SV.findall(header)))
                                [_np.newaxis].view('S3').astype(str))
    head_svs_std = (_np.asarray(b''.join(_RE_SP3_ACC.findall(header)))
                                [_np.newaxis].view('S3')[:head_svs.shape[0]].astype(int))
    sv_tbl = _pd.Series(head_svs_std,index=head_svs)

    return _pd.concat([sp3_heading,sv_tbl],keys=['HEAD','SV_INFO'],axis=0)


def sp3_vel(sp3_df,deg=35):
    '''takes sp3_df, interpolates the positions for -1s and +1s and outputs velocities'''
    est = sp3_df.unstack(1).EST[['X','Y','Z']]
    x = est.index.values
    y = est.values

    off,scl = mapparm([x.min(), x.max()],[-1,1]) # map from input scale to [-1,1]

    x_new = off + scl*(x)
    coeff = _np.polyfit(x=x_new,y=y,deg=deg)

    x_prev = off + scl*(x-1)
    x_next = off + scl*(x+1)

    xx_prev_combined = _np.broadcast_to((x_prev)[None],(deg+1,x_prev.shape[0]))
    xx_next_combined = _np.broadcast_to((x_next)[None],(deg+1,x_prev.shape[0]))

    inputs_prev =  xx_prev_combined ** _np.flip(_np.arange(deg+1))[:,None]
    inputs_next =  xx_next_combined ** _np.flip(_np.arange(deg+1))[:,None]

    res_prev = coeff.T.dot(inputs_prev)
    res_next = coeff.T.dot(inputs_next)
    vel_i = _pd.DataFrame((((res_prev.T - y) + (y - res_next.T))/2)*10000,columns=est.columns,index=est.index).stack()

    vel_i.columns = [['VELi']*3] + [vel_i.columns.values.tolist()]
    return vel_i

def sp3_vel_ch(sp3_df, len_tails = 3, deg=60):
    '''takes sp3_df, interpolates the positions for -1s and +1s and outputs velocities'''
    est = sp3_df.unstack(1).EST[['X','Y','Z']]

    x = est.index.values
    y = est.values

    # addiing short tails to both ends so polynom gets closer to the actual edgepoints
    steps = (_np.arange(len_tails)+1) #* 900 #15 min steps
    x_tail_begin = x[0] - _np.flip(steps) * 900
    x_tail_end = x[-1] + steps * 900
    y_tail_begin = y[0] - _np.flip(steps)[:,_np.newaxis]*(y[1] - y[0])
    y_tail_end = y[-1] + steps[:,_np.newaxis]*(y[-1] - y[-2])

    x = _np.concatenate([x_tail_begin,x,x_tail_end])
    y = _np.concatenate([y_tail_begin,y,y_tail_end])

    off,scl = mapparm([x.min(), x.max()],[-1,1])
    x_new = off + scl*(x)
    coeff = _np.polynomial.chebyshev.chebfit(x=x_new,y=y,deg=deg)

    x_prev = off + scl*(x-.001)
    x_next = off + scl*(x+.001)

    res_prev = _np.polynomial.chebyshev.chebval(x=x_prev,c = coeff).T
    res_next = _np.polynomial.chebyshev.chebval(x=x_next,c = coeff).T
    vel_i_arr = (((res_prev - y) + (y - res_next))/2)[len_tails:-len_tails]

    # special case for real edge values not to use extrapolated values
    vel_i_arr[0] = (y[len_tails] - res_next[len_tails])
    vel_i_arr[-1] = (res_prev[-1-len_tails] - y[-1-len_tails])

    vel_i = _pd.DataFrame(vel_i_arr*(10000 * 1000),columns=est.columns,index=est.index).stack()
    vel_i.columns = [['VELi']*3] + [vel_i.columns.values.tolist()]
    return vel_i


def gen_sp3_header(sp3_df):

    sp3_j2000 = sp3_df.index.levels[0].values
    sp3_j2000_begin = sp3_j2000[0]

    header = sp3_df.attrs['HEADER']
    head = header.HEAD
    sv_tbl = header.SV_INFO


    #need to update DATETIME outside before writing
    line1 = ([f'#{head.VERSION}{head.PV_FLAG}{_j20002rnxdt(sp3_j2000_begin)[0][3:-2]}'
            + f'{sp3_j2000.shape[0]:>9}{head.DATA_USED:>6}'
            + f'{head.COORD_SYS:>6}{head.ORB_TYPE:>4}{head.AC:>5}\n'])


    gpsweek, gpssec = _datetime2gpsweeksec(sp3_j2000_begin)
    mjd_days, mjd_sec = _datetime2mjd(sp3_j2000_begin)

    line2 = [f'##{gpsweek:5}{gpssec:16.8f}{sp3_j2000[1] - sp3_j2000_begin:15.8f}{mjd_days:6}{mjd_sec:16.13f}\n']

    sats = sv_tbl.index.to_list() 
    n_sats = sv_tbl.shape[0]

    sats_rows = (n_sats//17)+1 if n_sats>(17*5) else 5 #should be 5 but MGEX need more lines (e.g. CODE sp3)
    sats_header = _np.asarray(sats + ['  0']*(17*sats_rows - n_sats),dtype=object).reshape(sats_rows,-1).sum(axis=1) + '\n'

    sats_header[0] = '+ {:4}   '.format(n_sats) + sats_header[0]
    sats_header[1:] = '+        ' + sats_header[1:]


    sv_orb_head = _np.asarray(sv_tbl.astype(str).str.rjust(3).to_list() + ['  0']*(17*sats_rows - n_sats),dtype=object).reshape(sats_rows,-1).sum(axis=1) + '\n'

    sv_orb_head[0] =  '++       '+ sv_orb_head[0]
    sv_orb_head[1:] = '++       '+ sv_orb_head[1:]

    head_c = (   [f'%c {head.FILE_TYPE}  cc {head.TIME_SYS} ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc\n']
                +['%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc\n'])

    head_fi = (  ['%f  1.2500000  1.025000000  0.00000000000  0.000000000000000\n']
                +['%f  0.0000000  0.000000000  0.00000000000  0.000000000000000\n']
                +['%i    0    0    0    0      0      0      0      0         0\n']
                +['%i    0    0    0    0      0      0      0      0         0\n'])

    comment = ['/*\n']*4

    return ''.join(   line1 + line2
                    + sats_header.tolist() + sv_orb_head.tolist()
                    + head_c + head_fi + comment)

def gen_sp3_content(sp3_df):
    est = sp3_df.EST.values
    est[_np.isnan(est)] = 999999.999999
    sp3_df.EST = est #None to 999999
    a_str = sp3_df.EST.round(6).astype(str)

    sp3_content = ('P' + sp3_df.index.get_level_values(1).values.astype(object) + ' '
    + a_str.X.str.rjust(13).values + ' ' 
    + a_str.Y.str.rjust(13).values + ' '
    + a_str.Z.str.rjust(13).values + ' '
    + a_str.CLK.str.rjust(13).values + '\n')

    dt_uniques = sp3_df.index.levels[0].values

    dt_s = _pd.Series(_j20002rnxdt(dt_uniques),index = dt_uniques-1)
    data_s = _pd.Series(sp3_content,index=sp3_df.index.get_level_values(0))

    return ''.join((_pd.concat([dt_s,data_s]).sort_index()).to_list())

def write_sp3(sp3_df,path):

    content = gen_sp3_header(sp3_df) + gen_sp3_content(sp3_df) + 'EOF'
    with open(path,'w') as file:
        file.write(content)

def merge_attrs(df_list):

    df = _pd.concat(list(map(lambda obj: obj.attrs['HEADER'], df_list)),axis=1)

    mask_mixed = ~_unique_cols(df.loc['HEAD'])
    values_if_mixed = _np.asarray(['MIX','MIX','MIX',None,'M',None,'MIX','P','MIX','d'])
    
    head = df[0].loc['HEAD'].values
    head[mask_mixed] = values_if_mixed[mask_mixed]
    
    sv_info = df.loc['SV_INFO'].max(axis=1).values.astype(int)

    return _pd.Series(_np.concatenate([head,sv_info]),index=df.index)

def merge_sp3(sp3_paths,clk_paths=None):
    '''reads in a list of sp3 files and optianl list of clk files
    and merges them into a single sp3 file'''
    sp3_dfs = [read_sp3(sp3_file) for sp3_file in sp3_paths]
    merged_sp3 = _pd.concat(sp3_dfs)
    merged_sp3.attrs['HEADER'] = merge_attrs(sp3_dfs)
    
    if clk_paths is not None:
        clk_dfs = [_read_clk(clk_file) for clk_file in clk_paths]
        merged_sp3.EST.CLK = _pd.concat(clk_dfs).EST.AS * 1000000
        
    return merged_sp3

def sp3_hlm_trans(a:_pd.DataFrame,b:_pd.DataFrame)->tuple((_pd.DataFrame,tuple((_np.ndarray,_pd.DataFrame)))):
    '''Rotates sp3_b into sp3_a. Returns a tuple of updated sp3_b and HLM array with applied computed parameters and residuals'''
    hlm = _get_helmert7(pt1=a.EST[['X','Y','Z']].values,
                        pt2=b.EST[['X','Y','Z']].values)

    hlm = (hlm[0],_pd.DataFrame(hlm[1],columns=[['RES']*3,['X','Y','Z']],index = a.index))

    b.iloc[:,:3] = _transform7(xyz_in=b.EST[['X','Y','Z']].values,helmert_list=hlm[0][0])
    return b, hlm

def sp3dfs2common(sp3a,sp3b):
    level0_intersect =  _np.intersect1d(sp3a.index.levels[0].values, sp3b.index.levels[0].values,assume_unique=True) #common time
    level1_intersect = _np.intersect1d(sp3a.index.levels[1].values, sp3b.index.levels[1].values,assume_unique=True) #common svs
    
    mask_a = sp3a.index.get_level_values(0).isin(level0_intersect) & sp3a.index.get_level_values(1).isin(level1_intersect)
    mask_b = sp3b.index.get_level_values(0).isin(level0_intersect) & sp3b.index.get_level_values(1).isin(level1_intersect)

    return sp3a.iloc[_np.arange(mask_a.shape[0])[mask_a]].sort_index(), sp3b.iloc[_np.arange(mask_b.shape[0])[mask_b]].sort_index()

def diff_sp3_rac(sp3_a,sp3_b,hlm_mode=None):
    hlm_modes = [None, 'ECF', 'ECI']
    if hlm_mode not in hlm_modes:
        raise ValueError(f"Invalid hlm_mode. Expected one of: {hlm_modes}")

    hlm = None #init hlm var
    sp3_a, sp3_b = sp3dfs2common(sp3_a, sp3_b) # produces common sp3 dfs with sorted index
    if hlm_mode == 'ECF':
        sp3_b, hlm = sp3_hlm_trans(sp3_a,sp3_b)

    sp3_a_eci = _ecef2eci(sp3_a)
    sp3_a_eci_vel = _pd.concat([sp3_a_eci,sp3_vel(sp3_df=sp3_a_eci,deg=36)],axis=1) # because of sorted index, no additional manipulation with vel is required - just concat
    sp3_b_eci = _ecef2eci(sp3_b)

    if hlm_mode == 'ECI':
        sp3_b_eci, hlm = sp3_hlm_trans(sp3_a_eci,sp3_b_eci)

    xyz_cols_a = _np.argwhere(sp3_a_eci.columns.isin([('EST','X'),('EST','Y'),('EST','Z')])).ravel() # get indices of EST X Y Z cols
    xyz_cols_b = _np.argwhere(sp3_b_eci.columns.isin([('EST','X'),('EST','Y'),('EST','Z')])).ravel()

    diff_eci = sp3_a_eci.iloc[:,xyz_cols_a] - sp3_b_eci.iloc[:,xyz_cols_b]
    nd_rac = diff_eci.values[:,_np.newaxis] @ _eci2rac_rot(sp3_a_eci_vel)
    df_rac = _pd.DataFrame(nd_rac.reshape(-1,3),
                  index = sp3_a.index,
                  columns=[['EST_RAC']*3,['Radial','Along-track','Cross-track']])

    df_rac.attrs['sp3_a'] = _os.path.basename(sp3_a.attrs['path'])
    df_rac.attrs['sp3_b'] = _os.path.basename(sp3_b.attrs['path'])
    df_rac.attrs['hlm'] = hlm
    df_rac.attrs['hlm_mode'] = hlm_mode
    return df_rac

def get_sisre(sp3_a:_pd.DataFrame,sp3_b:_pd.DataFrame,clk_a:_pd.DataFrame,clk_b:_pd.DataFrame)->_pd.DataFrame:
    '''sp3_a and sp3_b are the dataframes produced by read_sp3 function. clk dataframes are the output of read_clk.
    Outputs a dataframe of signal-in-space range error (SISRE). The SISRE units are meters'''
    rac = diff_sp3_rac(sp3_a,sp3_b,hlm_mode=None) * 1000 # km to meters... should be correct

    rac_dt = _np.unique(rac.index.droplevel(1).values) # TODO looks like could be rac.index.levels[0].values

    # need to sync clk dfs with sp3 dfs.
    clk_a = clk_a.iloc[:,0][clk_a.index.droplevel([1,2]) == 'AS'].droplevel(level=0,axis=0) # faster then clk_a.iloc[:,0].loc['AS']... pandas...
    clk_b = clk_b.iloc[:,0][clk_b.index.droplevel([1,2]) == 'AS'].droplevel(level=0,axis=0)

    common_dt = _np.intersect1d(rac_dt,_np.intersect1d(clk_a.index.levels[0],clk_b.index.levels[0])) # TODO use sets trick to do the intersection

    clk_a_unst = clk_a.unstack('CODE').loc[common_dt]
    clk_b_unst = clk_b.unstack('CODE').loc[common_dt]

    coeffs = _SISRE_COEF_DF.T.reindex(rac.index.droplevel(0).str[0])
    common_svs = _np.intersect1d(clk_a_unst.columns,clk_b_unst.columns) # TODO set.intersect or simply set(concat)

    # clk_diff = (clk_a_unst.loc[common_dt][common_svs] - clk_b_unst.loc[common_dt][common_svs])*_C_LIGHT 
    # reveals a ~10m clk bias in the IGS clk while he expected mean value for GPS is ~0.5 m (not 10.5!!!).
    # Montenbruck, Steigenberger, and Hauschild, “Multi-GNSS Signal-in-Space Range Error Assessment – Methodology and Results.”

    clk_diff = (_norm_clk(clk_a_unst.loc[common_dt][common_svs],sv='G01') - _norm_clk(clk_b_unst.loc[common_dt][common_svs],sv='G01'))*_C_LIGHT
    # Expected mean value for GPS is ~0.05 m because of clocks normalization (e.g., with G01)
    # Kazmierski, Hadas, and Sośnica, “Weighting of Multi-GNSS Observations in Real-Time Precise Point Positioning.”

    # sisre = sqrt( (r*alpha - d_clk*c)^2 + (a^2 + c^2)/beta )
    sisre = (((rac.iloc[:,0] * coeffs.values[:,0] ).unstack(1)- clk_diff)**2 + ((rac.iloc[:,1]**2 + rac.iloc[:,2]**2)/coeffs.values[:,1]).unstack(1))**0.5
    return sisre