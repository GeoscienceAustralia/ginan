import logging as _logging

import numpy as _np
import pandas as _pd

from gn_lib.gn_const import J2000_ORIGIN as _J2000_ORIGIN, C_LIGHT as _C_LIGHT, SISRE_COEF_DF as _SISRE_COEF_DF
from gn_lib.gn_io.common import path2bytes as _path2bytes
from gn_lib.gn_io.sp3 import diff_sp3_rac as _diff_sp3_rac, read_sp3 as _read_sp3
from gn_lib.gn_io.ionex import read_ionex as _read_ionex
from gn_lib.gn_io.clk import read_clk as _read_clk
from gn_lib.gn_io.sinex import _get_snx_vector
from gn_lib.gn_io.stec import read_stec as _read_stec
from gn_lib.gn_io.trace import _read_trace_residuals, _read_trace_states
from gn_lib.gn_io.pod import read_pod_out as _read_pod_out

from gn_lib.gn_datetime import j20002datetime as _j20002datetime, datetime2gpsweeksec as _datetime2gpsweeksec

from gn_lib.gn_plot import diff2plot as _diff2plot


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


def _compare_sv_states(diffstd,log_lvl,tol=None,plot=False):
    diff_sv = diffstd[diffstd.attrs['SAT_MASK']].droplevel('NUM',axis=0).unstack(['TYPE','SITE','SAT','BLK'])
    if diff_sv.empty:
        _logging.warning(f':diffutil SVs states not present. Skipping')
        return 0
    if plot:
        # a standard scatter plot
        _diff2plot(diff_sv.DIFF.PHASE_BIAS)

        # a bar plot of mean values
        diffstd_mean = diff_sv.DIFF.PHASE_BIAS.mean(axis=0)
        diffstd_mean.index = diffstd_mean.index.to_series().astype(str)
        _diff2plot(diffstd_mean,kind='bar')
    bad_sv_states = _diff2msg(diff_sv,tol)
    if bad_sv_states is not None:
        _logging.log(msg=f':diffutil found SV states diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_sv_states.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.info(f':diffutil [OK] SVs states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return 0
    
def _compare_nonsv_states(diffstd,log_lvl,tol=None):
    diff_nonsv = diffstd[~diffstd.attrs['SAT_MASK']].droplevel('SAT',axis=0).unstack(['TYPE','SITE','NUM','BLK'])
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

def difftrace(trace1_path,trace2_path,atol,std_coeff,states_only,log_lvl,plot):
    '''Compares two trace/sum files'''
    
    trace1 = _path2bytes(trace1_path)
    trace2 = _path2bytes(trace2_path)
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

        status += _compare_sv_states(diffstd_states,tol=atol,log_lvl=log_lvl,plot=plot)
        status += _compare_nonsv_states(diffstd_states,tol=atol,log_lvl=log_lvl)
        # status = _compare_recsysbias_states(diffstd_states,tol=atol)

    if not states_only:
        _logging.info(f':diffutil -----testing trace residuals-----')
        resids1 = _read_trace_residuals(trace1)
        resids2 = _read_trace_residuals(trace2)
        if (resids1 is None) or (resids2 is None): 
            status += -1 # don't wait, throw error right away as smth bad happened, errors have been logged
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
    assert diff_snx.size != 0, "no corresponding data to compare"

    bad_snx_vals = _diff2msg(diff_snx,tol=atol)
    if bad_snx_vals is not None:
        _logging.log(msg=f':diffutil found estimates diffs above {"the extracted STDs" if atol is None else f"{atol:.1E} tolerance"}:\n{bad_snx_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.info(f':diffutil [OK] estimates diffs within {"the extracted STDs" if atol is None else f"{atol:.1E} tolerance"}')
    return status

def diffstec(path1,path2,atol,std_coeff,log_lvl):
    '''Compares two stec files '''
    stec1,stec2 = _read_stec(path1), _read_stec(path2)
    status = 0
    diffstd = _valvar2diffstd(stec1,stec2,std_coeff=std_coeff)
    status = _compare_stec(diffstd=diffstd,tol=atol,log_lvl=log_lvl)
    return status

def diffionex(ionex1_path,ionex2_path,atol,log_lvl):
    '''Compares two ionex files '''

    ionex1_df = _read_ionex(path_or_bytes=ionex1_path)
    ionex2_df = _read_ionex(path_or_bytes=ionex2_path)

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

def _compare_clk(clk_a,clk_b,sp3_a = None, sp3_b = None, as_sisre=False):
    '''clk dataframes are the output of read_clk. sp3_a and sp3_b are the dataframes produced by read_sp3 function.
    Outputs a dataframe of signal-in-space range error (SISRE). The SISRE units are meters'''
    common_dt = sorted(set(clk_a.index.levels[1].values).intersection(clk_b.index.levels[1].values))
    assert len(common_dt) != 0, "no common epochs between clk files" #TODO assert may be too aggressive

    clk_a_unst = clk_a.iloc[:,0][clk_a.index.droplevel([1,2]) == 'AS'].droplevel(level=0,axis=0).rename_axis(index={'CODE':'SV'}).unstack('SV').loc[common_dt] # faster then clk_a.iloc[:,0].loc['AS']... pandas...
    clk_b_unst = clk_b.iloc[:,0][clk_b.index.droplevel([1,2]) == 'AS'].droplevel(level=0,axis=0).rename_axis(index={'CODE':'SV'}).unstack('SV').loc[common_dt]

    common_svs = sorted(set(clk_a_unst.columns.values).intersection(clk_b_unst.columns.values))

    common_a = clk_a_unst[common_svs]
    common_b = clk_b_unst[common_svs]

    gnss = _np.asarray(common_svs,dtype='U1')
    gnss_uni, gnss_ind, gnss_count = _np.unique(gnss,return_index=True,return_counts=True)

    std_val = _pd.Series(_np.std(common_a.values,axis=0) + _np.std(common_b.values,axis=0),index = [gnss,common_svs]).sort_values(ascending=True)

    gnss_std_ind, sv_std_ind = _np.unique(std_val.index.droplevel(1),return_index=True)
    gnss_uni_minstd = std_val.iloc[sv_std_ind].droplevel(0).index.values #should be sorted so no need for [gnss_uni]
    _logging.info(f"{gnss_uni_minstd} used for normalisation of the respective GNSS clk values")

    norm_a = common_a - common_a[_np.repeat(gnss_uni_minstd,gnss_count)].values
    norm_b = common_b - common_b[_np.repeat(gnss_uni_minstd,gnss_count)].values

    clk_diff = (norm_a - norm_b)* _C_LIGHT
    if not as_sisre:
        return clk_diff.abs() # abs makes it look like sisre, however for sisre sign needs to be kept
    else:
        assert (sp3_a is not None and sp3_b is not None), "please provide sp3 files for sisre analysis"
        rac = _diff_sp3_rac(sp3_a,sp3_b,hlm_mode=None) * 1000 # km to meters... should be correct
        common_dt = sorted(set(rac.index.levels[0].values).intersection(clk_diff.index.values))
        rac = rac.loc(axis=0)[common_dt]
        coeffs = _SISRE_COEF_DF.T.reindex(rac.index.droplevel(0).str[0])
        sisre = (((rac.iloc[:,0] * coeffs.values[:,0] ).unstack(1)- clk_diff.loc(axis=0)[common_dt])**2 + ((rac.iloc[:,1]**2 + rac.iloc[:,2]**2)/coeffs.values[:,1]).unstack(1))**0.5
        return sisre

def diffsp3(sp3_a_path, sp3_b_path, atol, log_lvl):
    """Compares two sp3 files and outputs a dataframe of differences above tolerance if such were found"""
    sp3_a, sp3_b = _read_sp3(sp3_a_path), _read_sp3(sp3_b_path)
    status = 0
    diff_rac = _diff_sp3_rac(sp3_a,sp3_b,hlm_mode=None) * 1000 # km to meters... should be correct

    bad_rac_vals = _diff2msg(diff_rac.unstack(1),tol=atol)
    if bad_rac_vals is not None:
        _logging.log(msg=f':diffutil found estimates diffs above {atol:.1E} tolerance:\n{bad_rac_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.info(f':diffutil [OK] estimates diffs within {atol:.1E} tolerance')
    return status

def diffpodout(pod_out_a_path, pod_out_b_path, atol, log_lvl):
    pod_out_a, pod_out_b = _read_pod_out(pod_out_a_path), _read_pod_out(pod_out_b_path)
    status = 0
    diff_pod_out = (pod_out_a - pod_out_b)

    bad_rac_vals = _diff2msg(diff_pod_out.unstack(1),tol=atol)
    if bad_rac_vals is not None:
        _logging.log(msg=f':diffutil found estimates diffs above {atol:.1E} tolerance:\n{bad_rac_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.info(f':diffutil [OK] estimates diffs within {atol:.1E} tolerance')
    return status

def diffclk(clk_a_path, clk_b_path, atol, log_lvl, sp3_a_path = None, sp3_b_path = None):
    """Compares two clk files and provides a difference above atol if present. If sp3 orbits provided - does analysis using the SISRE values"""
    tol = atol # might find a way to get automatic threshold but only atol for now
    clk_a, clk_b = _read_clk(clk_a_path), _read_clk(clk_b_path)
    as_sisre = False
    if (sp3_a_path is not None) and (sp3_b_path is not None):
        sp3_a, sp3_b = _read_sp3(sp3_a_path), _read_sp3(sp3_b_path)
        as_sisre = True
    else:
        sp3_a = sp3_b = None

    status = 0
    diff_clk = _compare_clk(clk_a,clk_b,sp3_a = sp3_a, sp3_b = sp3_b, as_sisre=as_sisre) # type:ignore output looks cleaner this way

    bad_clk_vals = _diff2msg(diff_clk,tol=tol)
    if bad_clk_vals is not None:
        _logging.log(msg=f':diffutil found {"SISRE values" if as_sisre else "norm clk diffs"} above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_clk_vals.to_string(justify="center")}\n',level=log_lvl)
        status -= 1
    else:
        _logging.info(f':diffutil [OK] {"SISRE values" if as_sisre else "norm clk diffs"} within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}')
    return status