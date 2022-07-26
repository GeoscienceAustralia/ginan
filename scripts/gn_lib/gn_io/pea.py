from io import BytesIO as _BytesIO

import pandas as _pd
from gn_lib import gn_const as _gn_const
from gn_lib import gn_datetime as _gn_datetime
from gn_lib import gn_io as _gn_io


def read_pea_partials(path):
    partials = _gn_io.common.path2bytes(path)
    begin = partials.find(b"End_of_Header") + 13
    df = _pd.read_csv(_BytesIO(partials[begin:]),header=None,delim_whitespace=True,usecols=[0,1,2,9,10,11],names=[None,'MJD','TOD','X','Y','Z'])
    df_done = df[['X','Y','Z']].set_index([_gn_datetime.mjd2j2000(df.MJD.values,df.TOD.values,pea_partials=True),df.iloc[:,0].astype(_gn_const.PRN_CATEGORY)])
    df_partials = _pd.concat([df_done],keys=['EST'],axis=1)/1000
    df_partials.attrs['path'] = path
    return df_partials
