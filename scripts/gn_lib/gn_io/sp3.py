'''Ephemeris functions'''
import re as _re
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd

from ..gn_aux import unique_cols as _unique_cols
from ..gn_datetime import datetime2gpsweeksec as _datetime2gpsweeksec
from ..gn_datetime import datetime2j2000 as _datetime2j2000
from ..gn_datetime import datetime2mjd as _datetime2mjd
from ..gn_datetime import j20002rnxdt as _j20002rnxdt
from .common import path2bytes
from .clk import read_clk as _read_clk

_RE_SP3 = _re.compile(rb'^\*(.+)\n((?:[^\*]+)+)',_re.MULTILINE)

# 1st line parser. ^ is start of document, search
_RE_SP3_HEAD = _re.compile(rb'''^\#(\w)(\w)([\d \.]{28})[ ]+
                                    (\d+)[ ]+(\w+)[ ]+(\w+)[ ]+
                                    (\w+)[ ]+(\w+)''',_re.VERBOSE)
#SV names. multiline, findall
_RE_SP3_HEAD_SV = _re.compile(rb'^\+[ ]+(?:[\d]+|)[ ]+((?:[A-Z]\d{2})+)\W',_re.MULTILINE)
# orbits accuracy codes
_RE_SP3_ACC = _re.compile(rb'^\+{2}[ ]+((?:[ ]{2}\d{1})+)\W',_re.MULTILINE)
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

def read_sp3(sp3_path,rnx_lbls=True,interp_vel=False,index_vel = None):
    '''Read SP3 file
    Returns STD values converted to proper units (mm/ps) also if present
    # if rinex name -> PG01 to G01 and filter on P
    '''
    content = path2bytes(str(sp3_path))
    header_end = content.find(b'/*')

    header = content[:header_end]
    content = content[header_end:]

    fline_b = header.find(b'%f') + 2
    fline = header[fline_b:fline_b+24].strip().split(b'  ')
    base_xyzc = _np.asarray([float(fline[0])]*3 + [float(fline[1])]) #exponent base

    data_blocks = _np.asarray(_RE_SP3.findall(string=content[:content.rfind(b'EOF')]))

    dates = data_blocks[:,0]
    data = data_blocks[:,1]
    counts = _np.char.count(data, b'\n')

    epochs_dt = _pd.to_datetime(_pd.Series(dates).str.slice(2,21).values.astype(str),
                                format=r'%Y %m %d %H %M %S')

    dt_index = _np.repeat(a=_datetime2j2000(epochs_dt),repeats=counts)
    b_string = b''.join(data.tolist())
    n_last_line = len(b_string[b_string[:-2].rfind(b'\n'):].split())

    sp3_df = _pd.read_csv(  _BytesIO(b_string),
                            dtype={0:'category'},
                            delim_whitespace=True,
                            header=None,
                            names=list(range(n_last_line)))

    #999999* None value flag to None
    nanflags2nans(sp3_df)
    # writing header data to dataframe attributes
    sp3_df.attrs['HEADER'] =  parse_sp3_header(header)

    if sp3_df.shape[1] == 9:
        sp3_df.columns = ([['SAT','EST','EST','EST','EST','STD','STD','STD','STD'],
                           ['','X','Y','Z','CLK','X','Y','Z','CLK']])
        sp3_df.STD = base_xyzc ** sp3_df.STD.values

    elif sp3_df.shape[1] == 11:

        sp3_df.columns = ([['SAT','EST','EST','EST','EST','STD','STD','STD','STD','P_XYZ','P_CLK'],
                           ['','X','Y','Z','CLK','X','Y','Z','CLK','','']])
        clk_std = sp3_df.STD.CLK.values #if clk std is missing, P_XYZ flag value is written

        mv_mask = sp3_df.P_CLK.isna() & ~sp3_df.P_XYZ.isna()
        #moved values due to empty clk std
        #can be P, EP and EV. The next column may be M then this will be empty
        #rule is the rightmost columns is never empty if left is not empty
        sp3_df.P_CLK[mv_mask].update(sp3_df[mv_mask].P_XYZ.values)
        sp3_df.P_XYZ[mv_mask].update(clk_std[mv_mask])
        clk_std[mv_mask] = None

        sp3_df.loc[:,[['STD','CLK']]] = clk_std.astype(float)
        sp3_df.STD = base_xyzc ** sp3_df.STD.values

    elif sp3_df.shape[1] == 7:
        sp3_df.columns = ([['SAT','EST','EST','EST','EST','P_XYZ','P_CLK'],
                    ['','X','Y','Z','CLK','','']])

    else:
        sp3_df.columns = ([['SAT','EST','EST','EST','EST'],
                           ['','X','Y','Z','CLK',]])

    if rnx_lbls:
        sat = sp3_df.SAT
        categories = sat.cat.categories

        p_categories = categories[categories.str.slice(0,1).values == 'P']
        p_mask = sat.isin(p_categories).values

        sp3_df = sp3_df[p_mask]
        sp3_df.SAT.cat.rename_categories(categories.str.slice(1),inplace=True)

    sp3_df.set_index([dt_index,'SAT'],inplace=True)
    sp3_df.index.names = ([None,None])

    if interp_vel:
        if index_vel is not None:
            sp3_df = sp3_df.loc[index_vel]
        return _pd.concat([sp3_df,sp3_vel_ch(sp3_df)],axis=1)

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


def sp3_vel(sp3_df):
    '''takes sp3_df, interpolates the positions for -1s and +1s and outputs velocities'''
    est = sp3_df.unstack(1).EST[['X','Y','Z']]
    x = est.index.values
    y = est.values

    off,scl = mapparm([x.min(), x.max()],[-1,1])

    x_new = off + scl*(x)
    coeff = _np.polyfit(x=x_new,y=y,deg=35)

    x_prev = off + scl*(x-1)
    x_next = off + scl*(x+1)

    xx_prev_combined = _np.repeat((x_prev)[_np.newaxis],len(coeff),axis=0)
    xx_next_combined = _np.repeat((x_next)[_np.newaxis],len(coeff),axis=0)

    inputs_prev =  xx_prev_combined ** _np.flip(_np.arange(len(coeff)))[:,_np.newaxis]
    inputs_next =  xx_next_combined ** _np.flip(_np.arange(len(coeff)))[:,_np.newaxis]

    res_prev = coeff.T.dot(inputs_prev)
    res_next = coeff.T.dot(inputs_next)

    vel_i = _pd.DataFrame((((res_prev.T - y) + (y - res_next.T))/2)*10000,columns=est.columns,index=est.index).stack()

    vel_i.columns = [['VELi']*3] + [vel_i.columns.values.tolist()]
    return vel_i

def sp3_vel_ch(sp3_df, len_tails = 3):
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
    coeff = _np.polynomial.chebyshev.chebfit(x=x_new,y=y,deg=60)

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

    sats_header = _np.asarray(sats + ['  0']*(17*5 - n_sats),dtype=object).reshape(5,-1).sum(axis=1) + '\n'

    sats_header[0] = '+ {:4}   '.format(n_sats) + sats_header[0]
    sats_header[1:] = '+        ' + sats_header[1:]


    sv_orb_head = _np.asarray(sv_tbl.astype(str).str.rjust(3).to_list() + ['  0']*(17*5 - n_sats),dtype=object).reshape(5,-1).sum(axis=1) + '\n'

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