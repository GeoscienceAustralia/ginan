'''IO functions for various formats used: trace, sinex etc '''

import glob as _glob
import re as _re
import zlib
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd
from p_tqdm import p_map as _p_map
from p_tqdm.p_tqdm import tqdm as _tqdm

from ..gn_const import PT_CATEGORY, TYPE_CATEGORY
from ..gn_datetime import yydoysec2datetime as _yydoysec2datetime
from .common import path2bytes

_RE_BLK_HEAD = _re.compile(rb'\+S\w+\/\w+(\s[LU]|)\s*(CORR|COVA|INFO|)\n(?:\*.+\n|)')

def _get_valid_stypes(stypes, verbose=True):
    '''Returns only stypes in allowed list
    Fastest if stypes size is small'''
    allowed_stypes = set()
    not_allowed_stypes = set()
    for stype in stypes:
        if stype in {'APR', 'EST', 'NEQ'}:
            allowed_stypes.add(stype)
        else:
            if verbose:
                not_allowed_stypes.add(stype)
    if verbose and len(not_allowed_stypes) > 0:
        print(f'{not_allowed_stypes} not supported')
    return sorted(list(allowed_stypes))

def _snx_extract_blk(snx_bytes, blk_name, remove_header=False):
    '''
    Extracts a blk content from a sinex databytes using the + and - blk_name bounds
    Works for both vector and matrix blks.
    Returns blk content (with or without header), count of content lines (ignooring the header),
    matrix form [L or U] and matrix content type [INFO, COVA, CORR].
    The latter two are empty in case of vector blk'''
    blk_begin = snx_bytes.find(f'+{blk_name}'.encode())
    blk_end = snx_bytes.find(f'-{blk_name}'.encode(), blk_begin)
    if blk_begin == -1:
        # _tqdm.write(f'{blk_name} blk missing')
        return None  #if there is no block begin bound -> None is returned
    if blk_end == -1:
        # _tqdm.write(f'{blk_name} blk corrupted')
        return None

    head_search = _RE_BLK_HEAD.search(string=snx_bytes, pos=blk_begin)
    ma_form, ma_content = head_search.groups()

    blk_content = snx_bytes[head_search.end():blk_end]
    # blk content without header (usual request)
    lines_count = blk_content.count(b'\n')
    #may be skipped for last/first block (TODO)
    if not remove_header:
        blk_content = snx_bytes[head_search.span(2)[1]:blk_end]
        # if header requested (1st request only)
    return blk_content, lines_count, ma_form, ma_content
    # ma_form, ma_content only for matrix

def _snx_extract(snx_bytes, stypes, obj_type, verbose=True):
    #     obj_type= matrix or vector
    if obj_type == 'MATRIX':
        stypes_dict = {
            'EST': 'SOLUTION/MATRIX_ESTIMATE',
            'APR': 'SOLUTION/MATRIX_APRIORI',
            'NEQ': 'SOLUTION/NORMAL_EQUATION_MATRIX'
        }
    elif obj_type == 'VECTOR':
        stypes_dict = {
            'EST': 'SOLUTION/ESTIMATE',
            'APR': 'SOLUTION/APRIORI',
            'NEQ': 'SOLUTION/NORMAL_EQUATION_VECTOR',
            'ID' : 'SITE/ID'
        }

    snx_buffer = b''
    stypes_form, stypes_content, stypes_rows = {}, {}, {}
    objects_in_buf = 0
    for stype in stypes:
        if stype in stypes_dict.keys():
            stype_extr = _snx_extract_blk(snx_bytes=snx_bytes,
                                          blk_name=stypes_dict[stype],
                                          remove_header=objects_in_buf != 0)
            # print(objects_in_buf != 0)
            if stype_extr is not None:
                snx_buffer += stype_extr[0]
                stypes_rows[stype] = stype_extr[1]
                stypes_form[stype] = stype_extr[2]  #dict of forms
                stypes_content[stype] = stype_extr[3]  #dict of content
                objects_in_buf += 1
            else:
                if verbose:
                    print(f'{stype} ({stypes_dict[stype]}) blk not found')
                return None

        else:
            if verbose:
                print(f'{stype} blk not supported')
    stypes = list(stypes_rows.keys())
    n_stypes = len(stypes)  #existing stypes only
    if n_stypes == 0:
        if verbose:
            print('nothing found')
        return None
    return _BytesIO(snx_buffer), stypes_rows, stypes_form, stypes_content

def _get_snx_matrix(path_or_bytes,
                    stypes=('APR', 'EST'),
                    n_elements=None,
                    verbose=True):
    '''
    stypes = "APR","EST","NEQ"
    APRIORY, ESTIMATE, NORMAL_EQUATION
    Would want ot extract apriori in the very same run with only single parser call
    If you use the INFO type this block should contain the normal equation matrix of the
    constraints applied to your solution in SOLUTION/ESTIMATE.
    n_elements is useful for the igs sinex files when matrix has missing end rows.\
    Fetch it from estimates vector
    '''
    if isinstance(path_or_bytes, str):
        snx_bytes = path2bytes(path_or_bytes)
    else:
        snx_bytes = path_or_bytes

    snx_buffer, stypes_rows, stypes_form, stypes_content = _snx_extract(snx_bytes=snx_bytes,
                                  stypes=stypes,
                                  obj_type='MATRIX',
                                  verbose=verbose)

    matrix_raw = _pd.read_csv(snx_buffer,
                              delim_whitespace=True,
                              dtype={
                                  0: _np.int16,
                                  1: _np.int16,
                              })  #can be 4 and 5 columns; only 2 first int16

    output = []
    prev_idx = 0
    for i in range(len(stypes_rows)):
        idx = stypes_rows[stypes[i]]
        # Where to get the n-elements for the apriori matrix? Should be taken from estimates matrix
        ma_sq = _matrix_raw2square(
            matrix_raw=matrix_raw[prev_idx:prev_idx + idx],
            matrix_content_type=stypes_content[stypes[i]],
            stypes_form=stypes_form[stypes[i]],
            n_elements=n_elements)
        output.append(ma_sq)
        prev_idx += idx
    return output, stypes

def _get_snx_vector(path_or_bytes, stypes=('APR', 'EST'), snx_format=True,verbose=True):
    '''stypes = "APR","EST","NEQ"
    APRIORY, ESTIMATE, NORMAL_EQUATION
    '''
    path = None
    if isinstance(path_or_bytes, str):
        path = path_or_bytes
        snx_bytes = path2bytes(path)
    elif isinstance(path_or_bytes, list):
        path, stypes, snx_format,verbose = path_or_bytes
        snx_bytes = path2bytes(path)
    else:
        snx_bytes = path_or_bytes

    stypes = _get_valid_stypes(stypes, verbose=verbose)
    if stypes == ('NEQ'):
        stypes = ('APR','NEQ')
        #should always return NEQ vector with APR above it
        if verbose:
            print('Prepending APR')

    extracted = _snx_extract(snx_bytes=snx_bytes, stypes=stypes, obj_type='VECTOR', verbose=verbose)
    if extracted is None:
        return None
    snx_buffer, stypes_rows, stypes_form, stypes_content = extracted

    try:
        vector_raw = _pd.read_csv(
            snx_buffer,
            delim_whitespace=True,
            comment=b'*',
            header=None,
            usecols=[1, 2, 3, 4, 5, 8, 9],
            names=['TYPE', 'CODE', 'PT', 'SOLN', 'REF_EPOCH', 'EST', 'STD'],
            dtype={
                1: TYPE_CATEGORY,
                2: object,
                3: PT_CATEGORY,
                4: 'category', #can not be int as may be '----'
                5: object,
                8: _np.float_,
                9: _np.float_
            })

    except ValueError as _e:
        if _e.args[0][:33] == 'could not convert string to float':
            _tqdm.write(f'{path} data corrupted. Skipping', end=' | ')
            return None
        else:
            raise _e

    if path is not None:
        del snx_buffer #need to test this better

    # removing possible str values (->0) and converting to int8
    # vector_raw.loc[~vector_raw['SOLN'].str.isnumeric(),'SOLN'] = 0
    # vector_raw['SOLN'] = vector_raw['SOLN'].apply(_np.int8)

    output = []
    prev_idx = 0
    for i in range(len(stypes_rows)):
        stype = stypes[i]
        idx = stypes_rows[stype]
        vec_df = (vector_raw[prev_idx:prev_idx + idx]).copy()
        if i == 0:
            vec_df.REF_EPOCH = _yydoysec2datetime(vec_df.REF_EPOCH,
                                              recenter=True,
                                              as_j2000=True)
        else:
            # Assuming the vectors are in the same order so blindly concat them
            vec_df = vec_df.iloc[:, 5:].reset_index(drop=True)
        if stype in ['APR', 'NEQ']:
            vec_df.rename(columns={'EST': stype}, inplace=True)
            vec_df.drop(columns='STD', inplace=True)
        output.append(vec_df)
        prev_idx += idx
    output = _pd.concat(output, axis=1)

    if snx_format:
        return output

    values = output.columns.intersection(['APR','EST','STD','NEQ'])
    types_mask = output.TYPE.isin(['STAX','STAY', 'STAZ', 'VELX', 'VELY', 'VELZ',])
    output = output.loc[types_mask]
    output['CODE_PT'] = output.CODE.values + '_' + output.PT.values.astype(object)
    return output.drop(columns=['CODE','PT','SOLN']).set_index(['CODE_PT', 'REF_EPOCH','TYPE']).unstack(2)

def _matrix_raw2square(matrix_raw,matrix_content_type,stypes_form,n_elements=None):
    if matrix_content_type == b'CORR':
        raise ValueError('CORR parsing not implemented yet')
    else:
        if stypes_form == b'U':
            print('U matrix detected. Not tested!')
        idx = matrix_raw.iloc[:,:2].values - 1
        #last element of first index column. Should be specified for IGS APR matrices (?)
        n_elements = idx[-1,0] + 1 if n_elements is None else n_elements

        rows = idx[:,0]
        cols = idx[:,1]

        values = matrix_raw.iloc[:,2:].values.flatten(order='F')
        nanmask = ~_np.isnan(values)

        rows = _np.concatenate((rows,rows,rows))
        cols = _np.concatenate((cols,cols+1,cols+2))

        matrix = _np.ndarray((n_elements,n_elements),dtype=values.dtype)
        matrix.fill(0)
        matrix[rows[nanmask],cols[nanmask]] = values[nanmask]

        # shouldn't care if lower or upper triangle
        matrix_square = matrix.T + matrix
        _np.fill_diagonal(matrix_square,_np.diag(matrix))
        return matrix_square

def _unc_snx_neq(path_or_bytes):
    vector = _get_snx_vector(path_or_bytes=path_or_bytes,stypes=['APR','EST','NEQ'],verbose=False)
    matrix = _get_snx_matrix(path_or_bytes=path_or_bytes,stypes=['NEQ'],
                             n_elements=vector.shape[0],verbose=False)

    neqm = matrix[0][0]
    neqv = vector.NEQ.values
    aprv = vector.APR.values
    vector.drop(columns='NEQ',inplace=True)
    vector['UNC'] = aprv + _np.linalg.solve(a=neqm,b=neqv)
    return vector

def _unc_snx_cova(path_or_bytes):
    vector = _get_snx_vector(path_or_bytes=path_or_bytes,stypes=['APR','EST'],verbose=False)
    matrix = _get_snx_matrix(path_or_bytes=path_or_bytes,stypes=['APR','EST'],
                             n_elements=vector.shape[0],verbose=False)

    aprm = matrix[0][0]
    estm = matrix[0][1]
    aprv = vector.APR.values
    estv = vector.EST.values

    vector['UNC'] = aprv + (_np.linalg.solve(aprm,aprm-estm) @ (estv - aprv))
    return vector

def unc_snx(path,snx_format=True):
    '''removes constrains from snx estimates using either COVA or NEQ method'''
    snx_bytes = path2bytes(path)
    if snx_bytes.find(b'NORMAL_EQUATION_MATRIX') == -1:
        output =  _unc_snx_cova(snx_bytes)
    output =  _unc_snx_neq(snx_bytes)
    if snx_format:
        return output
    values = output.columns
    values = values[_np.isin(values,['APR','EST','STD','UNC'])] #Do we need NEQ?
    return output.loc[['STAX', 'STAZ',
                       'STAY']].pivot_table(index=['CODE', 'REF_EPOCH'],
                                            columns=['TYPE'],
                                            values=values)

def _read_snx_solution(path_or_bytes):
    '''_get_snx_vector template to get a df with multiIndex columns as:
    | APR | EST | STD |
    |X|Y|Z|X|Y|Z|X|Y|Z|'''
    return _get_snx_vector(path_or_bytes=path_or_bytes,
                           stypes=('APR', 'EST'),
                           snx_format=False,
                           verbose=False)

def gather_sinex(glob_expr, n_threads=4, unconstrain=False):
    '''Expects a glob.glob() expression (e.g. '/data/cddis/*/esa*.snx.Z')'''

    files = sorted(_glob.glob(glob_expr))
    n_files = len(files)
    if not unconstrain:
        data = _p_map(_get_snx_vector,
                     files, [('APR', 'EST')] * n_files,
                     [True] * n_files, [False] * n_files,
                     num_cpus=n_threads)
    else:
        data = _p_map(unc_snx, files, [False] * n_files, num_cpus=4)
    return data
    # return _pd.concat(data, axis=0).pivot(index=['CODE','TYPE'],columns='REF_EPOCH').T

def _get_snx_vector_gzchunks(filename,block_name='SOLUTION/ESTIMATE',size_lookback=100):
    '''extract block from a large gzipped sinex file e.g. ITRF2014 sinex'''
    block_open = False
    block_bytes = b''
    stop = False

    gzip_file = filename.endswith('.gz')
    if gzip_file:
        decompressor_zlib = zlib.decompressobj(16+zlib.MAX_WBITS)

    with open(file=filename,mode='rb') as compressed_file:
        i=0
        while not stop:  # until EOF
            uncompressed = compressed_file.read(8192)
            if gzip_file:
                uncompressed = decompressor_zlib.decompress(uncompressed)
            if i>0:
                old_chunk = chunk[-size_lookback:]
                chunk = old_chunk + uncompressed
            else:
                chunk = uncompressed
            if chunk.find(f'+{block_name}'.encode()) != -1:
                block_open = True
#                     print('found')
            if block_open:
                block_bytes += chunk[size_lookback if i>0 else 0:]

                if chunk.find(f'-{block_name}'.encode()) != -1:
                    block_open = False
                    stop=True
            i+=1

    return _get_snx_vector(path_or_bytes=block_bytes,stypes=['EST'])


#SINEX ID BLOCK
def degminsec2decdeg(series):
    '''Converts degrees/minutes/seconds to decimal degrees'''
    _deg = series.str[:-8].values.astype(float)
    _min = series.str[-8:-5].values.astype(float)
    _sec = series.str[-5:].values.astype(float)
    sign = _np.sign(_deg)
    return _deg + sign*_min/60 + sign*_sec/3600

def _get_snx_id(path):
    snx_bytes = path2bytes(path)
    site_id   = _snx_extract_blk(snx_bytes=snx_bytes,blk_name='SITE/ID',remove_header=True)[0]
    site_id = site_id.decode(encoding='utf8',errors='ignore').encode()
    id_df = _pd.read_fwf(_BytesIO(site_id),header=None,
            colspecs=[(0,5),(5,8),(8,18),(18,20),(20,44),(44,55),(55,68),(68,76)])

    id_df.columns = ['CODE','PT','DOMES','T','LOCATION','LON','LAT','H']
    id_df.LON = degminsec2decdeg(id_df.LON)
    id_df.LAT = degminsec2decdeg(id_df.LAT)
    return id_df

def llh2snxdms(llh):
    '''converts llh ndarray to degree-minute-second snx id block format
    LAT LON HEI
    '''
    ll_dd = llh[:,:2]
    ll_dd[:,1] %=360

    # zero_mask = _np.any(ll_dd == 0.0, axis=1)
    # print(llh[zero_mask])

    sign =  _np.sign(ll_dd)
    ll_dd = _np.abs(ll_dd)
    hei = llh[:,2]

    minutes,seconds = _np.divmod(ll_dd*3600,60)
    degrees,minutes = _np.divmod(minutes,60)

    degrees *= sign
    array = _np.concatenate([degrees,minutes,seconds.round(1),llh[:,[2,]].round(1)],axis=1)

    llh_dms_df = _pd.DataFrame(array,dtype=object,
                               columns=[['LAT','LON','LAT','LON','LAT','LON','HEI'],
                                        ['D','D','M','M','S','S','']])
    llh_dms_df.iloc[:,:4] = llh_dms_df.iloc[:,:4].astype(int)
    llh_dms_df = llh_dms_df.astype(str)
    n_rows = llh_dms_df.shape[0]

    ll_stack = _pd.concat([llh_dms_df.LON, llh_dms_df.LAT],axis=0)
    ll_stack = (  ll_stack.D.str.rjust(4).values
                + ll_stack.M.str.rjust(3).values
                + ll_stack.S.str.rjust(5).values)
    buf = ll_stack[:n_rows] + ll_stack[n_rows:] + llh_dms_df.HEI.str.rjust(8).values

    buf[(hei>8000) | (hei<-2000) ] = ' 000 00 00.0  00 00 00.0   000.0' #| zero_mask
    return buf

def logllh2snxdms(llh):
    '''Converts igs logfile-formatted lat-lon-height to the format needed for sinex ID block'''
    n_rows = llh.shape[0]
    latlon = _pd.concat([llh.LON,llh.LAT],axis=0)
    step1 = latlon.str.extract(pat=r'([\+\-]?\d{2,3})(\d{2})(\d{2}\.\d)')
    step1_mask  = (   ~step1.iloc[:n_rows,0].isna().values
                    & ~step1.iloc[n_rows:,0].isna().values
                    & ~llh.HEI.isna())

    step1_mask_stack =  _np.tile(step1_mask,2)

    step2 = step1[step1_mask_stack].copy().values
    n_rows = step2.shape[0]//2

    dd = degminsec2decdeg(_pd.Series(step2[:,0] + ' ' +step2[:,1] + ' '+step2[:,2]))
    hei = llh[step1_mask].HEI.values
    hei[hei == ''] = 9999

    llh_dec = _np.vstack([ dd[n_rows:], dd[:n_rows],hei.astype(float)]).T

    buf = llh2snxdms(llh_dec)

    out = _np.empty(llh.shape[0],dtype='<U34')
    out[step1_mask] = buf
    out[~step1_mask] = ' 000 00 00.0  00 00 00.0   000.0'
    return out.astype(object)
