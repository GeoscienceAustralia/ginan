'''Parser of frame discontinuity file'''
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd
from pandas import CategoricalDtype as _CategoricalDtype

from ..gn_datetime import yydoysec2datetime as _yydoysec2datetime
from .common import path2bytes
from .sinex import _snx_extract_blk

MODEL_CATEGORY  = _CategoricalDtype(categories = ['P','V','A','S','E','X'])


def _read_discontinuities(path):
    snx_bytes = path2bytes(path)
    block = _snx_extract_blk(
        snx_bytes=snx_bytes, blk_name='SOLUTION/DISCONTINUITY', remove_header=True)[0]
    out_df = _pd.read_csv(filepath_or_buffer=_BytesIO(block),
                          usecols=[0, 1, 2, 4, 5, 6],
                          delim_whitespace=True,
                          header=None,
                          names=['CODE', 'PT', 'SOLN',
                                 'BEGIN', 'END', 'MODEL'],
                          dtype={
        0: object, 1: object, 2: object,
        4: object, 5: object, 6: MODEL_CATEGORY})

    begin_j2000 = _yydoysec2datetime(out_df['BEGIN'],as_j2000=True,recenter=False)
    begin_j2000[begin_j2000 == -129600] = -999999999
    #overwriting 00:000:00000 values with new boundaries

    end_j2000 = _yydoysec2datetime(out_df['END'],as_j2000=True,recenter=False)
    end_j2000[end_j2000 == -129600] = 999999999

    out_df['BEGIN'] = begin_j2000
    out_df['END'] = end_j2000
    #     return out_df
    #     return out_df.set_index(out_df.CODE.values + out_df.PT.values + out_df.SOLN.values.astype(str))
    return out_df.set_index(out_df.CODE.values + '_' + out_df.PT.values)
