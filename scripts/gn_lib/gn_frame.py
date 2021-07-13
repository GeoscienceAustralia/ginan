'''frame of day generation module'''
import numpy as _np
import pandas as _pd

from .gn_aux import update_mindex
from .gn_const import J2000_ORIGIN as _J2000_ORIGIN
from .gn_const import SEC_IN_YEAR as _SEC_IN_YEAR
from .gn_io.discon import _read_discontinuities
from .gn_io.psd import _get_psd_df
from .gn_io.sinex import _get_snx_vector_gzchunks
from .gn_transform import llh2rot, xyz2llh_heik


def _get_core_list(core_list_path):
    '''itrf2014_to_itrf2008_stations '''
    # need to check if solution numbers are consistent with discontinuities selection
    core_df = _pd.read_csv(core_list_path,
                           delim_whitespace=True,
                           skiprows=4,
                           comment='-',
                           usecols=[0, 1, 2, 3],
                           names=['CODE', 'DOMES_NO', 'SOLN', 'TECH'])
    return core_df


def get_frame_of_day(date_or_j2000, itrf_path_or_df, discon_path_or_df, psd_path_or_df=None, list_path_or_df=None):
    '''Main function to propagate frame into datetime of interest
    tech_name is one of ['GPS', 'VLBI', 'SLR', 'DORIS']'''
    # assert tech_name in ['GPS', 'VLBI', 'SLR',
    #                      'DORIS'], f'{tech_name} not in the TECH list'

    if isinstance(date_or_j2000, _np.int64):
        date_J2000 = date_or_j2000
    else:
        date_J2000 = (_np.datetime64(date_or_j2000) -
                      _J2000_ORIGIN).astype(int)

    # discontinuities file
    if isinstance(discon_path_or_df,_pd.DataFrame):
        discon_df = discon_path_or_df
    elif isinstance(discon_path_or_df,str):
        discon_df = _read_discontinuities(discon_path_or_df)
    else:
        print('check discon_path_or_df')

    #itrf sinex file
    if isinstance(itrf_path_or_df, _pd.DataFrame):
        output = itrf_path_or_df
    elif isinstance(itrf_path_or_df, str):
        output = _get_snx_vector_gzchunks(
            filename=itrf_path_or_df, block_name='SOLUTION/ESTIMATE')
    else:
        print('check itrf_path_or_df')

    discon_valid = discon_df[(discon_df.MODEL == 'P')
                           & (discon_df.BEGIN != -999999999)
                           & (discon_df.BEGIN < date_J2000)
                           & (discon_df.END > date_J2000)]

    comboindex = _pd.Index(output.CODE.values + '_' + output.PT.values.astype(object)
                           + output.SOLN.values.astype(object))

    itrf_code_pt = _pd.Index(output.CODE.values + '_' +
                             output.PT.values.astype(object), name=None).unique()
    soln_series = _pd.Series(index=itrf_code_pt, data='1')

    # clean discontinuities file from stations missing in the frame
    comboindex_dv = _pd.Index(
        discon_valid.CODE.values + '_' + discon_valid.PT.values)
    dv_mask = comboindex_dv.isin(itrf_code_pt)

    # overwrite indices on discont
    soln_series.loc[comboindex_dv[dv_mask]] = discon_valid[dv_mask].SOLN
    out = output[_pd.Index(comboindex).isin(
        soln_series.index.values + soln_series.values)]

    if list_path_or_df is not None:
        if isinstance(list_path_or_df,_pd.DataFrame):
            core_df = list_path_or_df
            core_list = core_df.CODE.values
        elif isinstance(list_path_or_df,str):
            core_df = _get_core_list(list_path_or_df)
            core_list = core_df.CODE.values
        elif isinstance(list_path_or_df,_np.ndarray) or isinstance(list_path_or_df,list):
            core_list = list_path_or_df
        else:
            print('check list_path_or_df')
        out_mask = out.CODE.isin(core_list)
        if out_mask.sum()>0:
            out = out[out_mask] 
        else:
            print('list stations are not in frame')
            return None

    # test3 =  test3[~test3.index.str.contains(pat='P\d{3}.')] #remove thise weird sites P104 etc

    combo_index = out.CODE.values + '_' + out.PT.values.astype(object)
    combo_index = _pd.MultiIndex.from_arrays([combo_index,out.TYPE])

    duplicated_mask = combo_index.duplicated()
    if duplicated_mask.sum() > 0:
        print(f'Removed duplicates of stations: {combo_index[duplicated_mask].get_level_values(0).unique().tolist()}')
        out_xyzvel = out.EST[~duplicated_mask].copy()
        out_xyzvel.index = combo_index[~duplicated_mask]
    else:
        out_xyzvel = out.EST.copy()
        out_xyzvel.index = combo_index
        
    out_xyzvel = out_xyzvel.unstack(level=1)
    itrf_reference = out['REF_EPOCH'].unique()[0]
    time_seconds = date_J2000 - itrf_reference

    position = out_xyzvel.iloc[:, :3]
    velocities = out_xyzvel.iloc[:, 3:6]
    out = position + velocities.values * (time_seconds/_SEC_IN_YEAR)
    out.columns = out.columns.astype(str)
    out['SOLN'] = ''
    out['SOLN'] += soln_series
    out.attrs['REF_EPOCH'] = date_J2000

    if psd_path_or_df is not None:
        if isinstance(psd_path_or_df,_pd.DataFrame):
            psd_df = psd_path_or_df
        elif isinstance(psd_path_or_df,str):
            psd_df = _get_psd_df(psd_path_or_df)
        else:
            print('check psd_path_or_df')
        out = psd2frame(frame_of_day=out, psd_df=psd_df)
    return out


def psd2frame(frame_of_day, psd_df):
    '''ref_epoch is extracted from frame_of_day attribute
    Outputs EST
    |STAX|STAY|STAZ|'''
    psd_df_ref = _get_psd_enu(psd_df=psd_df,
                          date_J2000=frame_of_day.attrs['REF_EPOCH'])
    frame_codes = frame_of_day.index.str.split(
        '_', expand=True).get_level_values(level=0)

    # if site has more than one monument - all monuments use same psd
    psd_enu = _pd.DataFrame(index=frame_codes).join(
        other=psd_df_ref).set_index(frame_of_day.index)
    # select only those sites that have psd event
    psd_enu = psd_enu[psd_enu.any(axis=1)]

    llh = xyz2llh_heik(xyz_array=frame_of_day[['STAX','STAY','STAZ']].loc[psd_enu.index].values)
    phi, lam = llh[:, 0], llh[:, 1]
    rot = llh2rot(phi=phi, lamb=lam)

    psd_xyz = _pd.DataFrame(data=_np.squeeze(psd_enu.values[:, _np.newaxis] @ rot, axis=1),
                            index=psd_enu.index, columns=['STAX', 'STAY', 'STAZ'])
    psd_xyz['SOLN'] = ''
    frame_of_day.loc[psd_xyz.index] += psd_xyz
    return frame_of_day


def _get_psd_enu(psd_df, date_J2000):
    '''Reads psd file and computes psd values at each of east, north and up components for the data_J2000
    Ignores the monument information as should be the same for different monuments of the same stations'''
    ref_epochs = psd_df.index.get_level_values(level=1)
    valid_mask = ref_epochs < date_J2000

    psd_coeff = psd_df[valid_mask].copy()
    psd_coeff['dt_years'] = (date_J2000 - ref_epochs[valid_mask])/_SEC_IN_YEAR

    log_part = psd_coeff['ALOG'] * \
        _np.log(1 + psd_coeff[['dt_years']].values / psd_coeff['TLOG'].values)
    exp_part = psd_coeff['AEXP'] * \
        (1 - _np.exp(-(psd_coeff[['dt_years']
                                 ].values / psd_coeff['TEXP'].values)))
    log_part_grouped = log_part.groupby(axis=0, level=0).sum()
    exp_part_grouped = exp_part.groupby(axis=0, level=0).sum()

    out = log_part_grouped.add(exp_part_grouped, fill_value=0)/1000
    # if log or exp part is missing for the component, .add should take care of the nans being added
    # .rename(columns={'E':'EAST','N':'NORTH','H':'UP'})
    return out[['E', 'N', 'H']]

# gather_reader
def read_frame_snx_all(*file_paths,core_sites=None):
    buf=[]
    for path in file_paths:
        buf.append(_get_snx_vector_gzchunks(filename=path,block_name='SOLUTION/ESTIMATE'))
    all_frame = _pd.concat(buf)
    if core_sites is not None:
        return all_frame[all_frame.CODE.isin(core_sites)]
    return all_frame

def read_disc_all(*file_paths,core_sites=None):
    buf=[]
    for path in file_paths:
        buf.append(_read_discontinuities(path))
    all_discon = _pd.concat(buf)
    all_discon = all_discon[all_discon.MODEL.values =='P']
    
    if core_sites is not None:
        return all_discon[all_discon.CODE.isin(core_sites)]
    return all_discon 

def read_psd_all(*file_paths,core_sites=None):
    buf=[]
    for path in file_paths:
        buf.append(_get_psd_df(path))
    all_psd = _pd.concat(buf)
    if core_sites is not None:   
        psd_sites = all_psd.index.levels[0]
        return all_psd.loc[psd_sites[psd_sites.isin(core_sites)]]
    return all_psd