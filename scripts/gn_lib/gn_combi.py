import numpy as _np
import pandas as _pd
import logging as _logging

from .gn_io.sinex import _get_snx_matrix, _get_snx_vector, get_variance_factor
from .gn_transform import get_helmert7, transform7
from .gn_io.common import path2bytes


def cova2neq(cova:_np.ndarray, variance_factor):
    """Function to convert COVA matrix to NEQ matrix as per Bernese ADDNEQ"""
    neqm = _np.linalg.inv(cova / variance_factor)
    # neqv = neqm @ (vec.EST.values - vec.APR.values)
    # return neqm, neqv
    return neqm

def corr2cova(corr:_np.ndarray) -> _np.ndarray:
    """Converts sinex CORR matrix to COVA using the diagonal STD values"""
    D = corr.diagonal() * corr.diagonal()[:,None]
    _np.fill_diagonal(corr,1) # fill COVA diagonal with 1 so we only multiply with D to get COVA
    return corr * D


def get_neq(path_or_bytes):
    snx_bytes = path2bytes(path_or_bytes)
    # TODO read and parse sinex header

    neq = _get_snx_matrix(path_or_bytes=snx_bytes, stypes=["NEQ"], verbose=False)

    vec = b""  # to silence the pylance
    if neq is not None:
        vec = _get_snx_vector(
            path_or_bytes=snx_bytes, stypes=["APR", "EST", "NEQ"], verbose=False,snx_format=None
        )
        # revisit this vec thing
        return neq[0][0], vec  # NEQ matrix and vector are present so just return
    # return N and N_vec if they exist, else -> magic
    _logging.warning(
        msg="No NEQ was found. Generating from COVA/CORR as not strict"
    )

    apr_est = _get_snx_matrix(path_or_bytes=snx_bytes, stypes=["APR", "EST"], verbose=False)

    if apr_est is not None:
        matrices, stype_dict = apr_est
    else:
        raise ValueError

    variance_factor = get_variance_factor(path_or_bytes)
    
    if variance_factor is None:
        variance_factor = 1
        _logging.warning(
            msg="No variance factor found. Considering it 1"
        )

    a_e = _get_snx_vector(path_or_bytes = snx_bytes, stypes=["EST", "APR"], verbose=False,snx_format=None)
    if a_e is None:
        raise ValueError

    if not 'APR' in stype_dict.keys():
        std = a_e.STD.values
        mat_apr = _np.identity(std.shape[0])
        _np.fill_diagonal(mat_apr,std*std)
        neq_apr = cova2neq(mat_apr,variance_factor)
    else:
        if stype_dict['APR'] == 'CORR':
            neq_apr = cova2neq(corr2cova(matrices[list(stype_dict.keys()).index('APR')]),variance_factor=variance_factor)
        elif stype_dict['APR'] == 'COVA': # K_constr
            neq_apr = cova2neq(matrices[list(stype_dict.keys()).index('APR')],variance_factor=variance_factor)
        elif stype_dict['APR'] == 'INFO': # N_constr
            neq_apr = matrices[list(stype_dict.keys()).index('APR')]
        else:
            raise ValueError

    if stype_dict['EST'] == 'CORR':
        neq_est = cova2neq(corr2cova(matrices[list(stype_dict.keys()).index('EST')]),variance_factor=variance_factor)
    elif stype_dict['EST'] == 'COVA': # K_xx
        print('cova2neq')
        neq_est = cova2neq(corr2cova(matrices[list(stype_dict.keys()).index('EST')]),variance_factor=variance_factor)
    elif stype_dict['EST'] == 'INFO': # N_total
        neq_est = matrices[list(stype_dict.keys()).index('EST')]
    else:
        raise ValueError

    neq = neq_est - neq_apr # N_total - N_constr

    neqv = neq @ (a_e.EST.values - a_e.APR.values) # (a.APR + _np.linalg.solve(a=neqm,b=neqv)) - a.EST # to check
    a_e['NEQ'] = neqv
    return neq, a_e

def neq_elim_dim(neq_mat: _np.ndarray, neq_vec: _np.ndarray, i: int):
    """Eliminates 'i' dimension in NEQ system"""
    elim_row = neq_mat[i][_np.newaxis]
    elim_col = neq_mat[:, i]
    elim_centr = elim_col[i]
    neq_mat -= (
        elim_row * (elim_col / elim_centr)[:, _np.newaxis]
    )  # division done on 1dim vector first
    neq_vec -= elim_col * neq_vec[i] / elim_centr


def prepare_neq(neq_m, vec_apr_neq, frame_of_day):
    """Eliminate the non-XYZ parameters from the NEQ system and align a priori XYZ values to the frame of choice if frame_of_day is given"""
    neq_mat = neq_m.copy()
    vec_apr_neq = vec_apr_neq.copy()
    neq_vec = vec_apr_neq.NEQ.values
    xyz_mask = _np.isin(vec_apr_neq.TYPE.values, ["STAX", "STAY", "STAZ"])
    idx2elim = vec_apr_neq.index.values[~xyz_mask]

    # neq_mat,neq_vec = neq, neq_vec
    for i in range(
        idx2elim.shape[0]
    ):  # need to be positive and sorted from bigger to smaller
        neq_elim_dim(neq_mat, neq_vec, idx2elim[i])

    neq_mat_elim = neq_mat[xyz_mask, :][:, xyz_mask]
    neq_vec_elim = neq_vec[xyz_mask]
    vec_apr_neq = vec_apr_neq[xyz_mask]

    aprioris = vec_apr_neq.APR
    index_combo = _pd.MultiIndex.from_arrays(
        [vec_apr_neq.CODE.values + "_" + vec_apr_neq.PT.values.astype(str), vec_apr_neq.TYPE]
    )
    aprioris.index = index_combo
    aprioris = aprioris.unstack(level=1)
    aprioris_missing_mask = aprioris.STAX.values == 0
    aprioris_vals_mask = ~aprioris_missing_mask

    if aprioris_missing_mask.sum() > 0:
        estimates = vec_apr_neq.EST
        estimates.index = index_combo
        aprioris[aprioris_missing_mask] += estimates.unstack(level=1).values[aprioris_missing_mask]

    if frame_of_day is not None:
        common = _np.intersect1d(aprioris[aprioris_vals_mask].index.values, frame_of_day.index.values)
        hlm = get_helmert7(pt1=frame_of_day.loc[common].iloc[:, :3].values, pt2=aprioris[aprioris_vals_mask].loc[common].values)  # could not work if the order of XYZ is changed to YXZ etc
        # copy over estimate values to 0-aprioris here and rotate them using the computed hlm coeff
        new_aprioris = _pd.DataFrame(data = transform7(xyz_in=aprioris.values, helmert_list=hlm[0][0] * -1),
                                     index = aprioris.index, columns = aprioris.columns)
    else:
        new_aprioris = aprioris # we later use substr with a mask and fill_value 0 to make all masked values 0

    d_apr = new_aprioris.subtract(aprioris[aprioris_vals_mask],fill_value=0).stack()[index_combo].values  # the aprioris index is sorted, so need to align with original index

    neq_vec_elim -= neq_mat_elim @ d_apr
    vec_apr_neq[["APR", "NEQ"]] = _np.vstack([new_aprioris.stack()[index_combo].values, neq_vec_elim]).T # index_combo is needed to preserve the order that may have been changed by stack()
    return neq_mat_elim, vec_apr_neq


def insert_neq(master_neq, master_vec_neq, ind, neq, neq_vec):

    bool_arr = _np.zeros(shape=master_neq.shape[0], dtype=bool)
    bool_arr[ind] = 1
    mask = bool_arr[_np.newaxis] * bool_arr[:, _np.newaxis]
    master_neq[mask] += neq.flatten()  # check the flatten order. Could change to +=1
    master_vec_neq[
        bool_arr
    ] += neq_vec  # Could chage to +=1 for number of solutions plot


def vec2comboind(vec):
    return vec.CODE.values + vec.PT.values + vec.TYPE.values.astype(str)


def get_uniind(vec_list):
    buf = []
    for vec in vec_list:
        buf.append(
            vec.set_index(
                vec.CODE.values + vec.PT.values.astype(str) + vec.TYPE.values.astype(str)
            ).APR
        )
    combo_uni = (
        _pd.concat(buf, axis=1).groupby(level=0, axis=1).mean()
    )  # need to specify level to stay in df
    combo_uni["IND"] = _np.arange(combo_uni.shape[0])  # for further stacking
    return combo_uni, buf


def addneq(snx_filelist, frame_of_day):
    buf_neq, buf_vec = [], []
    for file in snx_filelist:
        neq, vec = get_neq(file)
        neq, vec = prepare_neq(neq, vec, frame_of_day)
        buf_neq.append(neq)
        buf_vec.append(vec)

    combo_uni, combo_list = get_uniind(buf_vec)
    arr = _np.zeros(shape=(combo_uni.shape[0], combo_uni.shape[0]))
    arr_vec = _np.zeros(shape=(combo_uni.shape[0]))

    for i in range(len(combo_list)):
        insert_neq(
            master_neq=arr,
            master_vec_neq=arr_vec,
            ind=combo_uni.IND.loc[combo_list[i].index].values,
            neq=buf_neq[i],
            neq_vec=buf_vec[i].NEQ.values,
        )

    # return combo_uni,arr,arr_vec
    return combo_uni.APR + _np.linalg.lstsq(arr, arr_vec, rcond=None)[0]
