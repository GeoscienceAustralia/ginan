from io import BytesIO as _BytesIO

import pandas as _pd
from gn_lib import gn_const as _gn_const
from gn_lib import gn_datetime as _gn_datetime
from gn_lib import gn_io as _gn_io


def read_stec(path_or_bytes):
    stec = _pd.read_csv(_BytesIO(_gn_io.common.path2bytes(path_or_bytes)),comment='#',header=None,usecols=[1,2,3,4,5,6,7],names=['WEEK','TOW','SITE','SAT','VAL','VAR','LAYER'], # type:ignore
    dtype={1:int,2:int,3:object,4:_gn_const.PRN_CATEGORY,5:float,6:float,7:int}) # type:ignore
    datetime = _gn_datetime.gpsweeksec2datetime(stec.WEEK.values,stec.TOW.values,as_j2000=True)
    return stec.drop(columns=['WEEK','TOW']).set_index([datetime,'SITE','SAT','LAYER'])
