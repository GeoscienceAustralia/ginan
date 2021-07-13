'''Trop sinex files reader/parser'''
from io import BytesIO as _BytesIO
from p_tqdm.p_tqdm import tqdm as _tqdm

import numpy as _np
import pandas as _pd
from .common import path2bytes

from ..gn_datetime import yydoysec2datetime as _yydoysec2datetime
from .sinex import _snx_extract_blk


def _read_tro_solution(path: str, recenter: bool = True) -> _pd.DataFrame:
    '''Parses tro snx file into a dataframe.
    Enabling recenter overrides the default SOD values to 43200 s'''
    snx_bytes = path2bytes(path)
    tro_estimate = _snx_extract_blk(snx_bytes=snx_bytes,blk_name='TROP/SOLUTION',remove_header=True)
    if tro_estimate is None:
        _tqdm.write(f'bounds not found in {path}. Skipping.', end=' | ')
        return None
    tro_estimate = tro_estimate[0] #only single block is in tro so bytes only

    try:
        solution_df = _pd.read_csv(_BytesIO(tro_estimate), delim_whitespace=True,
                                   comment=b'*', index_col=False, header=None,
                                   names=['CODE', 'REF_EPOCH',
                                          2, 3, 4, 5, 6, 7],
                                   dtype={0: 'category',  1: object,
                                          2: _np.float_,  3: _np.float32,
                                          4: _np.float32, 5: _np.float32,
                                          6: _np.float32, 7: _np.float32, })
    except ValueError as _e:
        if _e.args[0][:33] == 'could not convert string to float':
            _tqdm.write(f'{path} data corrupted. Skipping', end=' | ')
            return None

    solution_df.REF_EPOCH = _yydoysec2datetime(
        solution_df.REF_EPOCH, recenter=recenter, as_j2000=True)
    solution_df.set_index(['CODE', 'REF_EPOCH'], inplace=True)
    solution_df.columns = _pd.MultiIndex.from_product(
        [['TROTOT', 'TGNTOT', 'TGETOT'], ['VAL', 'STD']])
    return solution_df