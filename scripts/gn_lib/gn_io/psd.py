'''ITRF2014+ postseismic deformation file'''
from .sinex import _get_snx_vector

def _get_psd_df(psd_snx_path):
    '''we ignore the monument'''
    psd_df = _get_snx_vector(path_or_bytes=psd_snx_path,stypes=['EST'],snx_format=None)
    # a['CODE_PT']=a.CODE.values + '_' + a.PT.values.astype(object)
    #monument is always A in psd file, cumcount is used as index if n
    # parameters of the same type are present for the same event
    psd_df['key']=psd_df.groupby(['CODE','REF_EPOCH','TYPE']).cumcount()
    psd_df = psd_df.set_index(['TYPE','CODE','REF_EPOCH','key'])['EST'].unstack(0)
    psd_df.columns = psd_df.columns.astype(object).str.split('_',expand=True)
    return psd_df
