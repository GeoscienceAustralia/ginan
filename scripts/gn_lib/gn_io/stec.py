from io import BytesIO as _BytesIO

import pandas as _pd

from ..gn_const import PRN_CATEGORY
from ..gn_datetime import gpsweeksec2datetime as _gpsweeksec2datetime
from .common import path2bytes


def read_stec(path_or_bytes):
    stec = _pd.read_csv(_BytesIO(path2bytes(path_or_bytes)),comment='#',header=None,usecols=[1,2,3,4,5,6,7],names=['WEEK','TOW','SITE','SAT','VAL','VAR','LAYER'], # type:ignore
    dtype={1:int,2:int,3:object,4:PRN_CATEGORY,5:float,6:float,7:int}) # type:ignore
    datetime = _gpsweeksec2datetime(stec.WEEK.values,stec.TOW.values,as_j2000=True)
    return stec.drop(columns=['WEEK','TOW']).set_index([datetime,'SITE','SAT','LAYER'])
