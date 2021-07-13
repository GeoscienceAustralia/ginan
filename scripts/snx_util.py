'''sinex read and gather routines'''

# from gn_lib.gn_filter import get_common_sites as _get_common_sites
from gn_lib.gn_io.sinex import gather_sinex as _gather_sinex


def snx2df(glob_expr, n_threads=4, unconstrain=False):
    '''An example demo function that can be run on sinex files dataset'''
    return _gather_sinex(glob_expr=glob_expr,
                         n_threads=n_threads,
                         unconstrain=unconstrain)


# HOW TO USE. DEMO
# Need to first download the sample files
# This can be done with rclone. CDDIS ftp instance needs explicit TLS
# ./rclone sync cddis:pub/gps/products /data/cddis/ --include "/{19[8-9][0-9],20[0-3][0-9]}/{cod,esa,jpl}*[0-9][0-9][0-9][0-9][0-9].snx.Z" -vv --transfers 10 --checkers 10
# ./rclone sync cddis:pub/gps/products /data/cddis/ --include "/{19[8-9][0-9],20[0-3][0-9]}/igs*[0-9][0-9][0-9][0-9][0-9].ssc.Z" -vv --transfers 10 --checkers 10
# cod_gather = snx2df('/data/cddis/*/cod*.snx.Z')
# esa_gather = snx2df('/data/cddis/*/esa*.snx.Z')
# igs_gather = snx2df('/data/cddis/*/igs*.ssc.Z')
# jpl_gather = snx2df('/data/cddis/*/jpl*.snx.Z')
# cod_gather_c,esa_gather_c,igs_gather_c,jpl_gather_c = get_common_sites(cod_gather,esa_gather,igs_gather,jpl_gather)
