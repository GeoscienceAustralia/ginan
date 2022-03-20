import logging as _logging

import numpy as _np
import pandas as _pd

from gn_lib.gn_const import J2000_ORIGIN as _J2000_ORIGIN
from gn_lib.gn_io.common import path2bytes
from gn_lib.gn_io.ionex import read_ionex
from gn_lib.gn_io.sinex import _get_snx_vector
from gn_lib.gn_io.stec import read_stec
from gn_lib.gn_io.trace import _read_trace_residuals, _read_trace_states

from gn_lib.gn_datetime import j20002datetime as _j20002datetime, datetime2gpsweeksec as _datetime2gpsweeksec


def _valvar2diffstd(valvar1,valvar2,trace=True,std_coeff=1):
    df = _pd.concat([valvar1,valvar2],axis=0,keys=['valvar1','valvar2']).unstack(0) #fastest\
    df_nd = df.values
    diff = df_nd[:,0] - df_nd[:,1]
    nan_mask = ~_np.isnan(diff)

    diff = diff[nan_mask]
    std = std_coeff*_np.sqrt((df_nd[:,3] + df_nd[:,2])[nan_mask])
    df_combo = _pd.DataFrame(_np.vstack([diff,std]).T,columns=['DIFF','STD'],index=df.index[nan_mask])

    if trace:
        sats = df.index.get_level_values('SAT')
        sats_mask = ~sats.isna()
        sats_df = sats[sats_mask].unique()
        
        df_combo.attrs['SAT_MASK'] = sats_mask[nan_mask]
        sats_common = sats[sats_mask & nan_mask]#.unique()
        df_combo.attrs['EXTRA_SATS'] = sats_df[~sats_df.isin(sats_common)].to_list() # is [] if none
    return df_combo

def _diff2msg(diff, tol = None, dt_as_gpsweek=False):
    _pd.set_option("display.max_colwidth", 10000)
    from_valvar = _np.all(_np.isin(['DIFF','STD'],diff.columns.get_level_values(0).values))

    if from_valvar: #if from_valvar else diff.values
        diff_df = diff.DIFF
        std_df  = diff.STD
        std_vals = std_df.values if tol is None else tol
    else:
        diff_df = diff
        assert tol is not None, 'tol can not be None if STD info is missing'
        std_vals = tol

    count_total = (~_np.isnan(diff_df.values)).sum(axis=0)
    mask2d_over_threshold = _np.abs(diff_df) > std_vals

    diff_count = mask2d_over_threshold.sum(axis=0)
    
    mask = diff_count.astype(bool)
    if mask.sum() == 0:
        return None
    mask_some_vals = mask[mask.values].index

    diff_over = diff_df[mask2d_over_threshold][mask_some_vals]
    idx_max = diff_over.idxmax()
    diff_max = _pd.Series(_np.diag(diff_over.loc[idx_max.values].values),index=idx_max.index)
    idx_min = diff_over.idxmin()
    diff_min = _pd.Series(_np.diag(diff_over.loc[idx_min.values].values),index=idx_min.index)

    if from_valvar:
        std_over  =  std_df[mask2d_over_threshold][mask_some_vals]
        std_max = _pd.Series(_np.diag(std_over.loc[idx_max.values].values),index=idx_max.index)
        std_min = _pd.Series(_np.diag(std_over.loc[idx_min.values].values),index=idx_min.index)

    msg = _pd.DataFrame()
    msg['RATIO'] =  (diff_count[mask].astype(str).astype(object) + '/' + count_total[mask].astype(str) 
    + ('(' + (diff_count[mask]/count_total[mask] * 100).round(2).astype(str)).str.ljust(5,fillchar='0') +'%)')

    msg['DIFF/MIN_DIFF'] = (diff_min.round(4).astype(str)
    +('±' + std_min.round(4).astype(str).str.ljust(6,fillchar='0') if from_valvar else '') 
    +' @' + ( (_datetime2gpsweeksec(idx_min.values,as_decimal=True)+1E-7).astype('<U11') if dt_as_gpsweek else _j20002datetime(idx_min.values).astype(str)))

    if (diff_count[mask]>1).sum()>0:
        msg['MAX_DIFF'] = (diff_max.round(4).astype(str).str.rjust(7) 
        +('±' + std_max.round(4).astype(str).str.ljust(6,fillchar='0') if from_valvar else '') 
        +' @' + ( (_datetime2gpsweeksec(idx_min.values,as_decimal=True)+1E-7).astype('<U11') if dt_as_gpsweek else _j20002datetime(idx_min.values).astype(str))) * (diff_count[mask]>1)
        
        msg['MEAN_DIFF'] = (diff_over.mean(axis=0).round(4).astype(str)
        +'±' + diff_over.std(axis=0).round(4).astype(str).str.ljust(6,fillchar='0'))* (diff_count[mask]>1)

    return msg

def _compare_sv_states(diffstd,log_lvl,tol=None):
    diff_sv = diffstd[diffstd.attrs['SAT_MASK']].droplevel('NUM',axis=0).unstack(['TYPE','SITE','SAT'])
    if diff_sv.empty:
        _logging.warning(f':diffutil SVs states not present. Skipping')
        return 0
    bad_sv_states = _diff2msg(diff_sv,tol)
    if bad_sv_states is not None:
        _logging.log(msg=f':diffutil found SV states diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_sv_states.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.info(f':diffutil [OK] SVs states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return 0
    
def _compare_nonsv_states(diffstd,log_lvl,tol=None):
    diff_nonsv = diffstd[~diffstd.attrs['SAT_MASK']].droplevel('SAT',axis=0).unstack(['TYPE','SITE','NUM'])
    if diff_nonsv.empty:
        _logging.warning(f':diffutil non-SVs states not present. Skipping')
        return 0
    bad_nonsv_states = _diff2msg(diff_nonsv,tol=tol)
    if bad_nonsv_states is not None:
        _logging.log(msg=f':diffutil non-SVs states diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_nonsv_states.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.info(f':diffutil [OK] non-SVs states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return 0

def _compare_sat_postfit_residuals(diffstd,log_lvl,tol=None):
    diff_count = diffstd[diffstd.attrs['SAT_MASK']].unstack(['TYPE','SAT','SITE'])
    if diff_count.empty:
        _logging.warning(f':diffutil SVs residuals not present. Skipping')
        return 0
    bad_sv_residuals = _diff2msg(diff_count,tol=tol)
    if bad_sv_residuals is not None:
        _logging.log(msg=f':diffutil found SVs residuals diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_sv_residuals.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.info(f':diffutil [OK] SVs residuals diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return 0

def _compare_nonsat_postfit_residuals(diffstd,log_lvl,tol=None):
    diff_count = diffstd[~diffstd.attrs['SAT_MASK']]
    if diff_count.empty: #no non-sat records found such as Zamb
        _logging.warning(f':diffutil non-SVs residuals not present. Skipping')
        return 0
    diff_count =  diff_count.droplevel('SAT').unstack(['SITE','TYPE'])
    bad_nonsv_residuals = _diff2msg(diff_count,tol=tol)
    if bad_nonsv_residuals is not None:
        _logging.log(msg=f':diffutil found non-SVs residuals diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_nonsv_residuals.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.info(f':diffutil [OK] non-SVs residuals diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return 0

def _compare_stec(diffstd,log_lvl,tol=None):
    stec_diff = diffstd.unstack(level = ('SITE','SAT','LAYER'))
    if stec_diff.empty:
        _logging.warning(f':diffutil stec states not present. Skipping')
        return 0
    bad_sv_states = _diff2msg(stec_diff,tol,dt_as_gpsweek=True)
    if bad_sv_states is not None:
        _logging.log(msg=f':diffutil found stec states diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_sv_states.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.info(f':diffutil [OK] stec states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return 0

def difftrace(trace1_path,trace2_path,atol,std_coeff,states_only,log_lvl):
    '''Compares two trace/sum files'''
    
    trace1 = path2bytes(trace1_path)
    trace2 = path2bytes(trace2_path)
    _logging.info(f':diffutil -----testing trace states-----')
    states1 = _read_trace_states(trace1)
    states2 = _read_trace_states(trace2)

    status = 0
    if (states1 is None) or (states2 is None):
        status += -1 # don't wait, throw error right away as smth bad happened, errors have been logged
    else:
        diffstd_states    = _valvar2diffstd(states1.iloc[:,:2],states2.iloc[:,:2],std_coeff=std_coeff,trace=True)

        
        
        if diffstd_states.attrs['EXTRA_SATS'] != []:
            _logging.warning(msg=f':diffutil found no counterpart for: {sorted(diffstd_states.attrs["EXTRA_SATS"])}')

        status += _compare_sv_states(diffstd_states,tol=atol,log_lvl=log_lvl)
        status += _compare_nonsv_states(diffstd_states,tol=atol,log_lvl=log_lvl)
        # status = _compare_recsysbias_states(diffstd_states,tol=atol)

    if not states_only:
        _logging.info(f':diffutil -----testing trace residuals-----')
        resids1 = _read_trace_residuals(trace1)
        resids2 = _read_trace_residuals(trace2)
        if (resids1 is None) or (resids2 is None): 
            status+= -1 # don't wait, throw error right away as smth bad happened, errors have been logged
        else:
            diffstd_residuals = _valvar2diffstd(resids1.iloc[:,2:],resids2.iloc[:,2:],std_coeff=std_coeff,trace=True)

            status += _compare_sat_postfit_residuals(diffstd_residuals,tol=atol,log_lvl=log_lvl)
            status += _compare_nonsat_postfit_residuals(diffstd_residuals,tol=atol,log_lvl=log_lvl)
    else:
        _logging.info(f':diffutil skipping residuals as got --states_only')
    return status

def diffsnx(snx1_path,snx2_path,atol,std_coeff,log_lvl):
    '''Compares two sinex files '''
    snx1_df = _get_snx_vector(path_or_bytes=snx1_path,stypes=('EST',),snx_format=True,verbose=True)
    snx2_df = _get_snx_vector(path_or_bytes=snx2_path,stypes=('EST',),snx_format=True,verbose=True)
    if (snx1_df is None) or (snx2_df is None): return -1 # don't wait, throw error right away as smth bad happened, errors have been logged

    status = 0
    diff_snx = _valvar2diffstd(snx1_df,snx2_df,trace=False,std_coeff=std_coeff).unstack(['CODE_PT','TYPE'])
    bad_snx_vals = _diff2msg(diff_snx,tol=atol)
    if bad_snx_vals is not None:
        _logging.log(msg=f':diffutil found estimates diffs above {"the extracted STDs" if atol is None else f"{atol:.1E} tolerance"}:\n{bad_snx_vals.to_string(justify="center")}\n',level=log_lvl)
        status += -1
    else:
        _logging.info(f':diffutil [OK] estimates diffs within {"the extracted STDs" if atol is None else f"{atol:.1E} tolerance"}')
    bad_snx_vals = _diff2msg(diff_snx,tol=None)
    return status

def diffstec(path1,path2,atol,std_coeff,log_lvl):
    '''Compares two stec files '''
    stec1 = read_stec(path1)
    stec2 = read_stec(path2)
    status = 0

    diffstd = _valvar2diffstd(stec1,stec2,std_coeff=std_coeff)
    status = _compare_stec(diffstd=diffstd,tol=atol,log_lvl=log_lvl)
    return status

def diffionex(ionex1_path,ionex2_path,atol,log_lvl):
    '''Compares two ionex files '''

    ionex1_df = read_ionex(path_or_bytes=ionex1_path)
    ionex2_df = read_ionex(path_or_bytes=ionex2_path)

    tol = 10**min(ionex1_df.attrs['EXPONENT'],ionex2_df.attrs['EXPONENT']) if atol is None else atol

    status = 0
    diff_ionex = (ionex1_df.unstack(level=('Type','Lat')) - ionex2_df.unstack(level=('Type','Lat'))).swaplevel('Lon','Type',axis=1) # type:ignore output looks cleaner this way

    bad_ionex_vals = _diff2msg(diff_ionex,tol=tol)
    if bad_ionex_vals is not None:
        _logging.log(msg=f':diffutil found IONEX diffs above {f"10^min(exp1,exp2) = {tol:.1E} tolerance" if atol is None else f"{tol:.1E} tolerance"}:\n{bad_ionex_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.info(f':diffutil [OK] estimates diffs within {f"10^min(exp1,exp2) = {tol:.1E} tolerance" if atol is None else f"{tol:.1E} tolerance"}')
    return status
