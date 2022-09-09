'''Ephemeris functions'''
import os as _os
import re as _re

import numpy as _np
import pandas as _pd
_pd.options.mode.chained_assignment = None  # default='warn'
from gn_lib import gn_aux as _gn_aux
from gn_lib import gn_datetime as _gn_datetime
from gn_lib import gn_io as _gn_io
from gn_lib import gn_transform as _gn_transform
from scipy import interpolate as _interpolate

_RE_SP3 = _re.compile(rb'^\*(.+)\n((?:[^\*]+)+)',_re.MULTILINE)

# 1st line parser. ^ is start of document, search
_RE_SP3_HEAD = _re.compile(rb'''^\#(\w)(\w)([\d \.]{28})[ ]+
                                    (\d+)[ ]+([\w\+]+)[ ]+(\w+)[ ]+
                                    (\w+)(?:[ ]+(\w+)|)''',_re.VERBOSE)
#SV names. multiline, findall
_RE_SP3_HEAD_SV = _re.compile(rb'^\+[ ]+(?:[\d]+|)[ ]+((?:[A-Z]\d{2})+)\W',_re.MULTILINE)
# orbits accuracy codes
_RE_SP3_ACC = _re.compile(rb'^\+{2}[ ]+([\d\s]{50}\d)\W',_re.MULTILINE)
# File descriptor and clock
_RE_SP3_HEAD_FDESCR = _re.compile(rb'\%c[ ]+(\w{1})[ ]+cc[ ](\w{3})')

def nanflags2nans(sp3_df):
    """Converts 999999 or 999999.999999 to NaNs"""
    nan_mask = sp3_df.iloc[:,1:5].values >= 999999
    nans = nan_mask.astype(float)
    nans[nan_mask] = _np.NAN
    sp3_df.iloc[:,1:5] = sp3_df.iloc[:,1:5].values+nans

def mapparm(old,new):
    """scipy function f map values """
    oldlen = old[1] - old[0]
    newlen = new[1] - new[0]
    off = (old[1]*new[0] - old[0]*new[1])/oldlen
    scl = newlen/oldlen
    return off, scl

def read_sp3(sp3_path,pOnly=True):
    '''Read SP3 file
    Returns STD values converted to proper units (mm/ps) also if present.
    by default leaves only P* values (positions), removing the P key itself
    '''
    content = _gn_io.common.path2bytes(sp3_path)
    header_end = content.find(b'/*')

    header = content[:header_end]
    content = content[header_end:]

    parsed_header =  parse_sp3_header(header)
    
    fline_b = header.find(b'%f') + 2 #TODO add to header parser
    fline = header[fline_b:fline_b+24].strip().split(b'  ')
    base_xyzc = _np.asarray([float(fline[0])]*3 + [float(fline[1])]) # exponent base

    data_blocks = _np.asarray(_RE_SP3.findall(string=content[:content.rfind(b'EOF')]))

    dates = data_blocks[:,0]
    data = data_blocks[:,1]
    if not data[-1].endswith(b"\n"): data[-1]+=b"\n"

    counts = _np.char.count(data, b'\n')

    epochs_dt = _pd.to_datetime(_pd.Series(dates).str.slice(2,21).values.astype(str),
                                format=r'%Y %m %d %H %M %S')

    dt_index = _np.repeat(a=_gn_datetime.datetime2j2000(epochs_dt.values),repeats=counts)
    b_string = b''.join(data.tolist())

    series = _pd.Series(b_string.splitlines())
    data_width =  series.str.len()
    missing = b' '*(data_width.max() - data_width).values.astype(object)
    series += missing # rows need to be of equal len 
    data_test = series.str[4:60].values.astype('S56').view(('S14')).reshape(series.shape[0],-1).astype(float)

    if  parsed_header.HEAD.ORB_TYPE in ["FIT",'INT']:
        sp3_df = _pd.DataFrame(data_test).astype(float)
        sp3_df.columns = ([['EST','EST','EST','EST'],
                           ['X','Y','Z','CLK',]])

    else: # parsed_header.HEAD.ORB_TYPE == 'HLM':
        # might need to output log message
        std = (series.str[60:69].values+series.str[70:73].values).astype('S12').view('S3').astype(object)
        std[std == b'   '] = None
        std = std.astype(float).reshape(series.shape[0],-1)

        ind = (series.str[75:76].values + series.str[79:80].values).astype('S2').view('S1')
        ind[ind == b' '] = b''
        ind = ind.reshape(series.shape[0],-1).astype(str)

        sp3_df = _pd.DataFrame(_np.column_stack([data_test,std,ind]),).astype(
        {0:float,1:float,2:float,3:float,4:float,5:float,6:float,7:float,8:"category",9:"category"})
        sp3_df.columns = ([['EST','EST','EST','EST','STD','STD','STD','STD','P_XYZ','P_CLK'],
                       ['X','Y','Z','CLK','X','Y','Z','CLK','','']])
        sp3_df.STD = base_xyzc ** sp3_df.STD.values

    nanflags2nans(sp3_df) # 999999* None value flag to None
    if pOnly or parsed_header.HEAD.loc['PV_FLAG']=='P':
        pMask = series.astype('S1') == b'P'
        sp3_df = sp3_df[pMask].set_index([dt_index[pMask],series.str[1:4].values[pMask].astype(str).astype(object)])
    else:
        sp3_df = sp3_df.set_index([dt_index,
        series.values.astype('U1'),
        series.str[1:4].values.astype(str).astype(object)])
        
    sp3_df.attrs['HEADER'] = parsed_header # writing header data to dataframe attributes
    sp3_df.attrs['path'] = sp3_path
    return sp3_df

def parse_sp3_header(header):
    sp3_heading = _pd.Series( data  = _np.asarray(_RE_SP3_HEAD.search(header).groups() 
                                                + _RE_SP3_HEAD_FDESCR.search(header).groups()).astype(str),
                              index = ['VERSION','PV_FLAG','DATETIME','N_EPOCHS','DATA_USED','COORD_SYS','ORB_TYPE','AC',
                                       'FILE_TYPE','TIME_SYS'])

    head_svs = (_np.asarray(b''.join(_RE_SP3_HEAD_SV.findall(header)))
                                [_np.newaxis].view('S3').astype(str))
    head_svs_std = (_np.asarray(b''.join(_RE_SP3_ACC.findall(header)))
                                [_np.newaxis].view('S3')[:head_svs.shape[0]].astype(int))
    sv_tbl = _pd.Series(head_svs_std,index=head_svs)

    return _pd.concat([sp3_heading,sv_tbl],keys=['HEAD','SV_INFO'],axis=0)

def getVelSpline(sp3Df):
    """returns in the same units as intput, e.g. km/s (needs to be x10000 to be in cm as per sp3 standard"""
    sp3dfECI = sp3Df.EST.unstack(1)[['X','Y','Z']] #_ecef2eci(sp3df)
    datetime = sp3dfECI.index.values
    spline = _interpolate.CubicSpline(datetime, sp3dfECI.values)
    velDf = _pd.DataFrame(  data = spline.derivative(1)(datetime),
                            index = sp3dfECI.index,columns = sp3dfECI.columns).stack(1)
    return _pd.concat([sp3Df,_pd.concat([velDf],keys=['VELi'],axis=1)],axis=1)

def getVelPoly(sp3Df,deg=35):
    '''takes sp3_df, interpolates the positions for -1s and +1s and outputs velocities'''
    est = sp3Df.unstack(1).EST[['X','Y','Z']]
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
    vel_i = _pd.DataFrame(  (((y-res_prev.T)+(res_next.T-y))/2),
                            columns=est.columns,
                            index=est.index).stack()

    vel_i.columns = [['VELi']*3] + [vel_i.columns.values.tolist()]

    return _pd.concat([sp3Df,vel_i],axis=1)

def gen_sp3_header(sp3_df):

    sp3_j2000 = sp3_df.index.levels[0].values
    sp3_j2000_begin = sp3_j2000[0]

    header = sp3_df.attrs['HEADER']
    head = header.HEAD
    sv_tbl = header.SV_INFO


    #need to update DATETIME outside before writing
    line1 = ([f'#{head.VERSION}{head.PV_FLAG}{_gn_datetime.j20002rnxdt(sp3_j2000_begin)[0][3:-2]}'
            + f'{sp3_j2000.shape[0]:>9}{head.DATA_USED:>6}'
            + f'{head.COORD_SYS:>6}{head.ORB_TYPE:>4}{head.AC:>5}\n'])


    gpsweek, gpssec = _gn_datetime.datetime2gpsweeksec(sp3_j2000_begin)
    mjd_days, mjd_sec = _gn_datetime.j20002mjd(sp3_j2000_begin)

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

    dt_s = _pd.Series(_gn_datetime.j20002rnxdt(dt_uniques),index = dt_uniques-1)
    data_s = _pd.Series(sp3_content,index=sp3_df.index.get_level_values(0))

    return ''.join((_pd.concat([dt_s,data_s]).sort_index()).to_list())

def write_sp3(sp3_df,path):
    """sp3 writer, dataframe to sp3 file"""
    content = gen_sp3_header(sp3_df) + gen_sp3_content(sp3_df) + 'EOF'
    with open(path,'w') as file:
        file.write(content)

def merge_attrs(df_list):
    """Merges attributes of a list of sp3 dataframes into a single set of attributes"""
    df = _pd.concat(list(map(lambda obj: obj.attrs['HEADER'], df_list)),axis=1)

    mask_mixed = ~_gn_aux.unique_cols(df.loc['HEAD'])
    values_if_mixed = _np.asarray(['MIX','MIX','MIX',None,'M',None,'MIX','P','MIX','d'])
    head = df[0].loc['HEAD'].values
    head[mask_mixed] = values_if_mixed[mask_mixed]
    sv_info = df.loc['SV_INFO'].max(axis=1).values.astype(int)

    return _pd.Series(_np.concatenate([head,sv_info]),index=df.index)

def merge_sp3(sp3_paths,clk_paths=None):
    '''Reads in a list of sp3 files and optianl list of clk file and merges them into a single sp3 file'''
    sp3_dfs = [read_sp3(sp3_file) for sp3_file in sp3_paths]
    merged_sp3 = _pd.concat(sp3_dfs)
    merged_sp3.attrs['HEADER'] = merge_attrs(sp3_dfs)
    
    if clk_paths is not None:
        clk_dfs = [_gn_io.clk.read_clk(clk_file) for clk_file in clk_paths]
        merged_sp3.EST.CLK = _pd.concat(clk_dfs).EST.AS * 1000000
        
    return merged_sp3

def sp3_hlm_trans(a:_pd.DataFrame,b:_pd.DataFrame)->tuple((_pd.DataFrame,tuple((_np.ndarray,_pd.DataFrame)))):
    '''Rotates sp3_b into sp3_a. Returns a tuple of updated sp3_b and HLM array with applied computed parameters and residuals'''
    hlm = _gn_transform.get_helmert7(pt1=a.EST[['X','Y','Z']].values,
                                     pt2=b.EST[['X','Y','Z']].values)

    hlm = (hlm[0],_pd.DataFrame(hlm[1],columns=[['RES']*3,['X','Y','Z']],index = a.index))

    b.iloc[:,:3] = _gn_transform.transform7(xyz_in=b.EST[['X','Y','Z']].values,helmert_list=hlm[0][0])
	# b.iloc[:,:3] = _transform7(xyz_in=b.EST[['X','Y','Z']].values,helmert_list=hlm[0][0])
    return b, hlm

def diff_sp3_rac(sp3_a,sp3_b,hlm_mode=None,use_cubic_spline = True):
    """
    Computes the difference between the two sp3 files in the radial, along-track and cross-track coordinates
    the interpolator used for computation of the velocities can be based on cubic spline (default) or polynomial
    Breaks if NaNs appear on unstack in getVelSpline function
    """
    hlm_modes = [None, 'ECF', 'ECI']
    if hlm_mode not in hlm_modes:
        raise ValueError(f"Invalid hlm_mode. Expected one of: {hlm_modes}")

    hlm = None #init hlm var
    sp3_a, sp3_b = _gn_aux.sync_idx_dfs(sp3_a, sp3_b) # produces common sp3 dfs with sorted index
    if hlm_mode == 'ECF':
        sp3_b, hlm = sp3_hlm_trans(sp3_a,sp3_b)

    sp3_a_eci = _gn_transform.ecef2eci(sp3_a)
    if use_cubic_spline:
        sp3_a_eci_vel = getVelSpline(sp3Df=sp3_a_eci)
    else:
        sp3_a_eci_vel = getVelPoly(sp3Df=sp3_a_eci,deg=35)
    sp3_b_eci = _gn_transform.ecef2eci(sp3_b)

    if hlm_mode == 'ECI':
        sp3_b_eci, hlm = sp3_hlm_trans(sp3_a_eci,sp3_b_eci)

    xyz_cols_a = _np.argwhere(sp3_a_eci.columns.isin([('EST','X'),('EST','Y'),('EST','Z')])).ravel() # get indices of EST X Y Z cols
    xyz_cols_b = _np.argwhere(sp3_b_eci.columns.isin([('EST','X'),('EST','Y'),('EST','Z')])).ravel()

    diff_eci = sp3_a_eci.iloc[:,xyz_cols_a] - sp3_b_eci.iloc[:,xyz_cols_b]
    nd_rac = diff_eci.values[:,_np.newaxis] @ _gn_transform.eci2rac_rot(sp3_a_eci_vel)
    df_rac = _pd.DataFrame(nd_rac.reshape(-1,3),
                  index = sp3_a.index,
                  columns=[['EST_RAC']*3,['Radial','Along-track','Cross-track']])                  
    
    df_rac.attrs['sp3_a'] = _os.path.basename(sp3_a.attrs['path'])
    df_rac.attrs['sp3_b'] = _os.path.basename(sp3_b.attrs['path'])
    df_rac.attrs['hlm'] = hlm
    df_rac.attrs['hlm_mode'] = hlm_mode    
   
    return df_rac