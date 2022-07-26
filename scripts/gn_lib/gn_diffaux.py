import logging as _logging
from typing import Union as _Union

import numpy as _np
import pandas as _pd

from gn_lib import gn_aux as _gn_aux
from gn_lib import gn_const as _gn_const
from gn_lib import gn_datetime as _gn_datetime
from gn_lib import gn_io as _gn_io
from gn_lib import gn_plot as _gn_plot


def _valvar2diffstd(valvar1,valvar2,trace=True,std_coeff=1):
    """Auxiliary function to efficiently difference two dataframes,
    each of them consisting of value-variance pairs"""
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
    +' @' + ( (_gn_datetime.datetime2gpsweeksec(idx_min.values,as_decimal=True)+1E-7).astype('<U11') if dt_as_gpsweek else _gn_datetime.j20002datetime(idx_min.values).astype(str)))

    if (diff_count[mask]>1).sum()>0:
        msg['MAX_DIFF'] = (diff_max.round(4).astype(str).str.rjust(7) 
        +('±' + std_max.round(4).astype(str).str.ljust(6,fillchar='0') if from_valvar else '') 
        +' @' + ( (_gn_datetime.datetime2gpsweeksec(idx_min.values,as_decimal=True)+1E-7).astype('<U11') if dt_as_gpsweek else _gn_datetime.j20002datetime(idx_min.values).astype(str))) * (diff_count[mask]>1)
        
        msg['MEAN_DIFF'] = (diff_over.mean(axis=0).round(4).astype(str)
        +'±' + diff_over.std(axis=0).round(4).astype(str).str.ljust(6,fillchar='0'))* (diff_count[mask]>1)

    return msg


def _compare_sv_states(diffstd,log_lvl,tol=None,plot=False):
    diff_sv = diffstd[diffstd.attrs['SAT_MASK']].droplevel('NUM',axis=0).unstack(['TYPE','SITE','SAT','BLK'])
    if diff_sv.empty:
        _logging.warning(msg=f':diffutil SVs states not present. Skipping')
        return 0
    if plot:
        # a standard scatter plot
        _gn_plot.diff2plot(diff_sv.DIFF.PHASE_BIAS)

        # a bar plot of mean values
        diffstd_mean = diff_sv.DIFF.PHASE_BIAS.mean(axis=0)
        diffstd_mean.index = diffstd_mean.index.to_series().astype(str)
        _gn_plot.diff2plot(diffstd_mean,kind='bar')
    bad_sv_states = _diff2msg(diff_sv,tol)
    if bad_sv_states is not None:
        _logging.log(msg=f':diffutil found SV states diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_sv_states.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.log(msg=f':diffutil [OK] SVs states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
    return 0
    
def _compare_nonsv_states(diffstd,log_lvl,tol=None):
    diff_nonsv = diffstd[~diffstd.attrs['SAT_MASK']].droplevel('SAT',axis=0).unstack(['TYPE','SITE','NUM','BLK'])
    if diff_nonsv.empty:
        _logging.warning(msg=f':diffutil non-SVs states not present. Skipping')
        return 0
    bad_nonsv_states = _diff2msg(diff_nonsv,tol=tol)
    if bad_nonsv_states is not None:
        _logging.log(msg=f':diffutil non-SVs states diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_nonsv_states.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.log(msg=f':diffutil [OK] non-SVs states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
    return 0

def _compare_sat_postfit_residuals(diffstd,log_lvl,tol=None):
    diff_count = diffstd[diffstd.attrs['SAT_MASK']].unstack(['TYPE','SAT','SITE','BLK'])
    if diff_count.empty:
        _logging.warning(f':diffutil SVs residuals not present. Skipping')
        return 0
    bad_sv_residuals = _diff2msg(diff_count,tol=tol)
    if bad_sv_residuals is not None:
        _logging.log(msg=f':diffutil found SVs residuals diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_sv_residuals.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.log(msg=f':diffutil [OK] SVs residuals diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
    return 0

def _compare_nonsat_postfit_residuals(diffstd,log_lvl,tol=None):
    diff_count = diffstd[~diffstd.attrs['SAT_MASK']]
    if diff_count.empty: #no non-sat records found such as Zamb
        _logging.warning(f':diffutil non-SVs residuals not present. Skipping')
        return 0
    diff_count =  diff_count.droplevel('SAT').unstack(['SITE','TYPE','BLK'])
    bad_nonsv_residuals = _diff2msg(diff_count,tol=tol)
    if bad_nonsv_residuals is not None:
        _logging.log(msg=f':diffutil found non-SVs residuals diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_nonsv_residuals.to_string(justify="center")}\n',level=log_lvl)
        return -1
    _logging.log(msg=f':diffutil [OK] non-SVs residuals diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
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
    _logging.log(msg=f':diffutil [OK] stec states diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
    return 0

def difftrace(trace1_path,trace2_path,atol,std_coeff,log_lvl,plot):
    '''Compares two trace/sum files'''
    trace1 = _gn_io.common.path2bytes(trace1_path)
    trace2 = _gn_io.common.path2bytes(trace2_path)

    _logging.log(msg=f':diffutil -----testing trace states-----',level=log_lvl)
    states1 = _gn_io.trace._read_trace_states(trace1)
    states2 = _gn_io.trace._read_trace_states(trace2)
    status = 0
    if (states1 is None) and (states2 is None):
        _logging.log(msg=f':diffutil both compared files are missing states data -> OK',level=log_lvl)
    elif (states1 is None) or (states2 is None):
        status += -1 # don't wait, throw error right away as smth bad happened, errors have been logged
    else:
        diffstd_states    = _valvar2diffstd(states1.iloc[:,:2],states2.iloc[:,:2],std_coeff=std_coeff,trace=True)
        if diffstd_states.attrs['EXTRA_SATS'] != []:
            _logging.warning(msg=f':diffutil found no counterpart for: {sorted(diffstd_states.attrs["EXTRA_SATS"])}')
        status += _compare_sv_states(diffstd_states,tol=atol,log_lvl=log_lvl,plot=plot)
        status += _compare_nonsv_states(diffstd_states,tol=atol,log_lvl=log_lvl)

    _logging.log(msg=f':diffutil -----testing trace residuals-----',level=log_lvl)
    resids1 = _gn_io.trace._read_trace_residuals(trace1)
    resids2 = _gn_io.trace._read_trace_residuals(trace2)
    if (resids1 is None) and (resids2 is None):
        _logging.log(msg=f':diffutil both compared files are missing residuals data -> OK',level=log_lvl)
    elif (resids1 is None) or (resids2 is None): 
        status += -1 # don't wait, throw error right away as smth bad happened, errors have been logged
    else:
        diffstd_residuals = _valvar2diffstd(resids1.iloc[:,2:],resids2.iloc[:,2:],std_coeff=std_coeff,trace=True)
        status += _compare_sat_postfit_residuals(diffstd_residuals,tol=atol,log_lvl=log_lvl)
        status += _compare_nonsat_postfit_residuals(diffstd_residuals,tol=atol,log_lvl=log_lvl)

    return status

def diffsnx(snx1_path,snx2_path,atol,std_coeff,log_lvl):
    '''Compares two sinex files '''
    snx1_df = _gn_io.sinex._get_snx_vector(path_or_bytes=snx1_path,stypes=('EST',),snx_format=True,verbose=True)
    snx2_df = _gn_io.sinex._get_snx_vector(path_or_bytes=snx2_path,stypes=('EST',),snx_format=True,verbose=True)

    if (snx1_df is None) or (snx2_df is None): return -1 # don't wait, throw error right away as smth bad happened, errors have been logged

    status = 0
    diff_snx = _valvar2diffstd(snx1_df,snx2_df,trace=False,std_coeff=std_coeff).unstack(['CODE_PT','TYPE'])
    assert diff_snx.size != 0, "no corresponding data to compare"

    bad_snx_vals = _diff2msg(diff_snx,tol=atol)
    if bad_snx_vals is not None:
        _logging.log(msg=f':diffutil found estimates diffs above {"the extracted STDs" if atol is None else f"{atol:.1E} tolerance"}:\n{bad_snx_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.log(msg=f':diffutil [OK] estimates diffs within {"the extracted STDs" if atol is None else f"{atol:.1E} tolerance"}',level=_logging.INFO)
    return status

def diffstec(path1,path2,atol,std_coeff,log_lvl):
    '''Compares two stec files '''
    stec1,stec2 = _gn_io.stec.read_stec(path1), _gn_io.stec.read_stec(path2)
    status = 0
    diffstd = _valvar2diffstd(stec1,stec2,std_coeff=std_coeff)
    status = _compare_stec(diffstd=diffstd,tol=atol,log_lvl=log_lvl)
    return status

def diffionex(ionex1_path,ionex2_path,atol,log_lvl):
    '''Compares two ionex files '''

    ionex1_df = _gn_io.ionex.read_ionex(path_or_bytes=ionex1_path)
    ionex2_df = _gn_io.ionex.read_ionex(path_or_bytes=ionex2_path)

    tol = 10**min(ionex1_df.attrs['EXPONENT'],ionex2_df.attrs['EXPONENT']) if atol is None else atol

    status = 0
    diff_ionex = (ionex1_df.unstack(level=('Type','Lat')) - ionex2_df.unstack(level=('Type','Lat'))).swaplevel('Lon','Type',axis=1) # type:ignore output looks cleaner this way

    bad_ionex_vals = _diff2msg(diff_ionex,tol=tol)
    if bad_ionex_vals is not None:
        _logging.log(msg=f':diffutil found IONEX diffs above {f"10^min(exp1,exp2) = {tol:.1E} tolerance" if atol is None else f"{tol:.1E} tolerance"}:\n{bad_ionex_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.log(msg=f':diffutil [OK] estimates diffs within {f"10^min(exp1,exp2) = {tol:.1E} tolerance" if atol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
    return status

def _compare_clk(clk_a:_pd.DataFrame,clk_b:_pd.DataFrame,
                 norm_type:str='both',
                 ext_dt :_Union[_np.ndarray,_pd.Index,None]=None,
                 ext_svs:_Union[_np.ndarray,_pd.Index,None]=None)->_pd.DataFrame:
    """
    clk DataFrames are the output of read_clk. sp3_a and sp3_b are the
    DataFrames produced by read_sp3 function. Outputs a DataFrame of
    signal-in-space range error (SISRE). The SISRE units are meters
    """

    clk_a = _gn_io.clk.get_AS_entries(clk_a)
    clk_b = _gn_io.clk.get_AS_entries(clk_b)

    if ext_dt is None:
        common_dt = clk_a.index.levels[0]
    else:
        common_dt = clk_a.index.levels[0].intersection(ext_dt)
        if len(common_dt) == 0: raise ValueError("no common epochs between clk_a and external dt")

    common_dt = common_dt.intersection(clk_b.index.levels[0])
    if len(common_dt) == 0:raise ValueError("no common epochs between clk_a and clk_b")

    clk_a_unst = _gn_aux.rm_duplicates_df(clk_a.loc[common_dt]).unstack(1)
    clk_b_unst = _gn_aux.rm_duplicates_df(clk_b.loc[common_dt]).unstack(1)


    if ext_svs is None:
        common_svs = clk_a_unst.columns # assuming ext_svs is lots smaller than count of svs in 
    else:
        common_svs = clk_a_unst.columns.intersection(ext_svs)
    if not _gn_aux.array_equal_unordered(common_svs,clk_b_unst.columns.values):
        common_svs = common_svs.intersection(clk_b_unst.columns)
        clk_a_unst = clk_a_unst[common_svs]
        clk_b_unst = clk_b_unst[common_svs]
    else:
        _logging.debug('_compare_clk: skipping svs sync for clk_b_unst as the same as common_svs')
        if not _gn_aux.array_equal_unordered(common_svs,clk_a_unst.columns.values):
            _logging.debug('_compare_clk: syncing clk_a_unst with common_svs as not equal')
            clk_a_unst = clk_a_unst[common_svs]


    if norm_type == 'sv':
        norm_type = _gn_io.clk.select_norm_svs_per_gnss(clk_a_unst=clk_a_unst,clk_b_unst=clk_b_unst)
        #get the sv to use for norm and overwrite norm_type value with sv prn code
    _gn_io.clk.rm_clk_bias(clk_a_unst,norm_type=norm_type)
    _gn_io.clk.rm_clk_bias(clk_b_unst,norm_type=norm_type)

    clk_diff = (clk_a_unst - clk_b_unst)*_gn_const.C_LIGHT
    return clk_diff


def sisre(sp3_a: _pd.DataFrame, sp3_b: _pd.DataFrame, 
          clk_a: _Union[_pd.DataFrame, None] = None,
          clk_b: _Union[_pd.DataFrame, None] = None,
          norm_type: str = 'both', output_mode: str = 'rms',
          clean: bool = True, cutoff: _Union[int, float, None] = None,
          use_rms: bool = False):
    """
    Computes SISRE metric for the combination of orbits and clock offsets. Note,
    if clock offsets were not provided computes orbit SISRE. Ignores clock
    offset values which could available in the orbit files (sp3). 
    TODO Add support for sp3 clock offset values, that could be overridden 
    TODO by proper clk input. Add a 'force' option to use sp3 clock offsets.

    Returns SISRE metric computed using the equation of Steigenberger &
    Montenbruck (2020) SISRE = sqrt( (w₁²R² - 2w₁RT + T²) + w₂²(A² + C²) )
    according to  which is the same as sqrt((αR - cT)² + (A² + C²)/β), with 
    w₁ = α and w₂ = sqrt(1/β).
    α and β are given in the table below:
        BDS(GEO/IGSO)   BDS(MEO)    GPS     GLO     GAL
    α   0.99            0.98        0.98    0.98    0.98
    β   127             54          49      45      61
    *QZSS (J) is being ignored
    *BeiDou different coeffs for MEO/GEO not implemented yet
        
    Parameters
    ----------
    sp3_a : sp3 DataFrame a
        Output of read_sp3 function or a similar sp3 DataFrame.
    sp3_b : sp3 DataFrame b
        Output of read_sp3 function or a similar sp3 DataFrame.
    clk_a : clk DataFrame a (optinal)
        Output of read_clk function or a similar clk DataFrame.
    clk_b : clk DataFrame b (optional)
        Output of read_clk function or a similar clk DataFrame.
    norm_type : str
        a norm_type parameter used for the clk values normalisations before
        differencing.
    output_mode : str
        controls at what stage to output SISRE
    clean : bool
        switch to use sigma filtering on the data.
    cutoff : int or float, default None
        A cutoff value in meters that is used to clip the values above it in
        both RAC frame values and clk offset differences. Operation is skipped
        if None is provided (default).
    use_rms : bool, default False
        A switch to compute RMS timeseries of RAC and T per each GNSS before
        computing SISRE.

    Returns
    -------
    sisre : DataFrame or Series depending in the output_mode selection
        output_mode = 'rms'  : Series of RMS SISRE values, value per GNSS.
        output_mode = 'gnss' : DataFrame of epoch-wise RMS SISRE values per GNSS.
        output_mode = 'sv'   : DataFrame of epoch-wise SISRE values per SV.
    """
    if output_mode not in ['rms', 'sv', 'gnss']:
        raise ValueError('incorrect output_mode given: %s' % output_mode)

    rac = _gn_io.sp3.diff_sp3_rac(_gn_aux.rm_duplicates_df(sp3_a, rm_nan_level=1),
                                 _gn_aux.rm_duplicates_df(sp3_b, rm_nan_level=1),
                                 hlm_mode=None).EST_RAC.unstack() * 1000  # km to meters,
                                 # sync is being done within the function. 
                                 # Filters with std over XYZ separately and all satellites together
    if clean:
        if cutoff is not None:
            rac = rac[rac.abs() < cutoff]
        rac = rac[rac.abs() < _gn_aux.get_std_bounds(
            rac, axis=0, sigma_coeff=3)]

    if (clk_a is not None) & (clk_b is not None):  # check if clk data is present
        clk_diff = _compare_clk(clk_a, clk_b, norm_type=norm_type,
                                ext_dt=rac.index, ext_svs=rac.columns.levels[1])  # units are meters
        if clean:
            if cutoff is not None:
                clk_diff = clk_diff[clk_diff.abs() < cutoff]
            clk_diff = clk_diff[clk_diff.abs() < _gn_aux.get_std_bounds(
                clk_diff, axis=0, sigma_coeff=3)]
        common_epochs_RAC_T = rac.index.intersection(
            clk_diff.index.values)  # RAC epochs not present in clk_diff
        common_svs = rac.columns.levels[1].intersection(
            clk_diff.columns)   # RAC SVs not present in clk_diff
        # common_epochs_RAC_T here might be not required. TODO
        clk_diff = clk_diff.loc[common_epochs_RAC_T][common_svs]
        rac = rac.loc[common_epochs_RAC_T].loc(axis=1)[:, common_svs]
    else:
        clk_diff = 0
        _logging.debug(msg='computing orbit SISRE as clk offsets not given')

    if use_rms:  # compute rms over each constellation svs at each epoch before moving on
        rac.columns = [rac.columns.droplevel(
            1), rac.columns.droplevel(0).values.astype('<U1')]
        clk_diff.columns = clk_diff.columns.values.astype('<U1')
        rac = _gn_aux.rms(arr=rac, axis=1, level=[0, 1])
        clk_diff = _gn_aux.rms(clk_diff, axis=1, level=0)

    radial, along, cross = _np.hsplit(rac.values, 3)
    coeffs_df = _gn_const.SISRE_COEF_DF.reindex(
        columns=rac.Radial.columns.values.astype('<U1'))
    alpha, beta = _np.vsplit(coeffs_df.values, indices_or_sections=2)

    sisre = _pd.DataFrame(data=_np.sqrt((alpha*radial + clk_diff)**2 + (along**2 + cross**2)/beta),
                          index=rac.index, columns=rac.Radial.columns)
    if output_mode == 'sv':
        return sisre  # returns per gnss if use_rms was selected
    if output_mode in ['gnss', 'rms']:
        if not use_rms:  # with use_rms, cols are already GNSS capitals
            sisre.columns = sisre.columns.values.astype('<U1')
        # rms over all SVs of each constellation
        rms_sisre = _gn_aux.rms(sisre, axis=1, level=0)
        if output_mode == 'gnss':
            return rms_sisre
        # rms over all epochs, a single value per constellation
        return _gn_aux.rms(rms_sisre, axis=0)

def diffsp3(sp3_a_path, sp3_b_path, atol, log_lvl, clk_a_path, clk_b_path):
    """Compares two sp3 files and outputs a dataframe of differences above tolerance if such were found"""
    sp3_a, sp3_b = _gn_io.sp3.read_sp3(sp3_a_path), _gn_io.sp3.read_sp3(sp3_b_path)

    as_sisre = False # the switch only needed for logging msg
    clk_a = clk_b = None
    if (clk_a_path is not None) and (clk_b_path is not None):
        clk_a, clk_b = _gn_io.clk.read_clk(clk_a_path), _gn_io.clk.read_clk(clk_b_path)
        as_sisre = True

    status = 0
    diff_rac = sisre(sp3_a=sp3_a.iloc[:,:3],sp3_b=sp3_b.iloc[:,:3],clk_a=clk_a,clk_b=clk_b,norm_type='both',output_mode='sv',clean=False)

    bad_rac_vals = _diff2msg(diff_rac,tol=atol)
    if bad_rac_vals is not None:
        _logging.log(msg=f':diffutil found {"SISRE values" if as_sisre else "estimates"} estimates diffs above {atol:.1E} tolerance:\n{bad_rac_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.log(msg=f':diffutil [OK] {"SISRE values" if as_sisre else "estimates"} diffs within {atol:.1E} tolerance',level=_logging.INFO)
    return status

def diffpodout(pod_out_a_path, pod_out_b_path, atol, log_lvl):
    pod_out_a, pod_out_b = _gn_io.pod.read_pod_out(pod_out_a_path), _gn_io.pod.read_pod_out(pod_out_b_path)
    status = 0
    diff_pod_out = (pod_out_a - pod_out_b)

    bad_rac_vals = _diff2msg(diff_pod_out.unstack(1),tol=atol)
    if bad_rac_vals is not None:
        _logging.log(msg=f':diffutil found estimates diffs above {atol:.1E} tolerance:\n{bad_rac_vals.to_string(justify="center")}\n',level=log_lvl)
        status = -1
    else:
        _logging.log(msg=f':diffutil [OK] estimates diffs within {atol:.1E} tolerance',level=_logging.INFO)
    return status

def diffclk(clk_a_path, clk_b_path, atol, log_lvl):
    """Compares two clk files and provides a difference above atol if present. If sp3 orbits provided - does analysis using the SISRE values"""
    tol = atol # might find a way to get automatic threshold but only atol for now
    clk_a, clk_b = _gn_io.clk.read_clk(clk_a_path), _gn_io.clk.read_clk(clk_b_path)

    status = 0
    diff_clk = _compare_clk(clk_a=clk_a,clk_b=clk_b,norm_type='both',cutoff=1) # type:ignore output looks cleaner this way

    bad_clk_vals = _diff2msg(diff_clk,tol=tol)
    if bad_clk_vals is not None:
        _logging.log(msg=f':diffutil found norm clk diffs above {"the extracted STDs" if tol is None else f"{tol:.1E} tolerance"}:\n{bad_clk_vals.to_string(justify="center")}\n',level=log_lvl)
        status -= 1
    else:
        _logging.log(msg=f':diffutil [OK] norm clk diffs within the {"extracted STDs" if tol is None else f"{tol:.1E} tolerance"}',level=_logging.INFO)
    return status
