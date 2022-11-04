#!/usr/bin/env python3

'''sinex stations quick view'''
import argparse
import logging as _logging
import os as _os

import pandas as _pd
import plotly.express as px

from gn_lib.gn_io.sinex import _get_snx_id


def parse_arguments():
    parser = argparse.ArgumentParser(description='Parse sinex SITE/ID block and create html map.')
    parser.add_argument('-i', '--snxpath', type=file_path,help='path to sinex file (.snx/.ssc). Can be compressed with LZW (.Z)',nargs="+",required=True)
    parser.add_argument('-o', '--outdir',   type=dir_path,help='path to output dir',default=None)
    return parser.parse_args()

def dir_path(path):
    if _os.path.isdir(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")
        
def file_path(path):
    if _os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")

def snxid2html(paths, outdir = '/data/acs/pea/output/'):
    '''Creates sinex station map html'''
    size = 0.5
    buf = []
    title=''

    _logging.getLogger().setLevel(_logging.INFO)
    _logging.info(msg=paths)

    for path in paths:
        basename = _os.path.basename(path)
        tmp_df = _get_snx_id(path)
        tmp_df['SIZE'] = size
        tmp_df['SNXFILE'] = basename
        title += f'{tmp_df.shape[0]} stations [{basename}] <br>'
        buf.append(tmp_df)
        size **=1.8
    id_df = _pd.concat(buf)

    fig = px.scatter_geo(id_df, lon='LON',lat='LAT',title=title,
                    size='SIZE',color='SNXFILE',size_max=9,
                    hover_name="CODE", # column added to hover information
                    hover_data = ['PT','DOMES'],
                    projection="natural earth")


    filename = ('gather_map' if len(paths)>1 else _os.path.basename(paths[0]))+'.html'
    save_path = _os.path.join(_os.path.curdir if outdir is None else outdir,filename)
    fig.write_html(save_path)
    _logging.info(msg=f'html saved to {save_path}')

if __name__ == "__main__":
    parsed_args = parse_arguments()
    snxid2html(paths=parsed_args.snxpath,outdir=parsed_args.outdir)
