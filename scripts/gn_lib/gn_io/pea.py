from io import BytesIO as _BytesIO
import pandas as _pd

from .common import path2bytes
from ..gn_datetime import mjd2j2000 as _mjd2j2000
from ..gn_const import PRN_CATEGORY as _PRN_CATEGORY


def read_pea_partials(path):
	partials = path2bytes(path)
	begin = partials.find(b"End_of_Header") + 13
	df = _pd.read_csv(_BytesIO(partials[begin:]),header=None,delim_whitespace=True,usecols=[0,1,2,9,10,11],names=[None,'MJD','TOD','X','Y','Z'])
	df_done = df[['X','Y','Z']].set_index([_mjd2j2000(df.MJD.values,df.TOD.values,pea_partials=True),df.iloc[:,0].astype(_PRN_CATEGORY)])
	df_partials = _pd.concat([df_done],keys=['EST'],axis=1)/1000
	df_partials.attrs['path'] = path
	return df_partials