import re as _re

import numpy as _np
import pandas as _pd

from gn_lib import gn_const as _gn_const
from gn_lib import gn_io as _gn_io

_POD_ITRF_RMS = _re.compile(r"RMS-XYZ ITRF CMP[ ]*([A-Z\d]+)[ ]+([\d\.]+)[ ]+([\d\.]+)[ ]+([\d\.]+)")

def pod_get_IC_dt(pod_out:bytes)->int:
    begin = pod_out.find(b"IC Epoch:") + 9
    end = pod_out.find(b"\n",begin)
    date = _pd.Series(pod_out[begin:end].strip().decode()).str.split(pat=r"\s+")
    year,month,day,frac = date.tolist()[0]
    dt_value = (_np.datetime64('-'.join([year,month.zfill(2),day])) - _gn_const.J2000_ORIGIN).astype(int) 
    return dt_value + int(86400*float(frac))

def read_pod_out(path):
    """reads pod.out file. No SV categories implemented as file is small"""
    content = _gn_io.common.path2bytes(path)
    df = _pd.DataFrame(
        data=_POD_ITRF_RMS.findall(content.decode()), #working with string not bytes
        columns=['SV','X','Y','Z']
        )
    return df.set_index([[pod_get_IC_dt(content)]*df.shape[0],'SV']).astype(float)

