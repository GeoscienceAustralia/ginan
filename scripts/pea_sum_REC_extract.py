''' Script for extracting last epoch in PEA.SUM file to analyse fit
'''
import re as _re
import argparse as _ap
from pathlib import Path as _Path
from io import BytesIO as _BytesIO

import numpy as _np
import pandas as _pd
import subprocess as _sp
import matplotlib.pyplot as _plt

from gn_lib.gn_io.common import path2bytes

_RE_EXTRACT = _re.compile(rb'(?:\+ States\n)((?:\*.*\n){2})((?:\*              REC_POS.*\n)*)((?:\*.*\n)*)(?:\- States\n)',_re.MULTILINE)


def get_last_RECPOS(in_file, out_JSON=False, out_JSON_dir=False):
    
    in_path = _Path(in_file)
    txt_path = _Path('tail.txt')
    
    with open(txt_path, "w") as f:
        _sp.run(['tail','-n','5000',str(in_path)], stdout=f)
    
    content = path2bytes(str(txt_path))
    data_blocks = _np.asarray(_RE_EXTRACT.findall(string=content))
    
    df = _pd.read_csv(_BytesIO(data_blocks[-1][1]),delim_whitespace=True,header=None)
    df = df.drop(labels=0,axis=1)
    dfs = df[[2,3,4]]
    dfs.columns = ['STATION','CRD','VAL']
    
    dfp = dfs.pivot(index='STATION',columns='CRD',values='VAL')
    dfp['3D_DIST'] = _np.sqrt(dfp[0]**2+dfp[1]**2+dfp[2]**2)
    dfp.columns = ['X','Y','Z','3D']
    
    txt_path.unlink()
    
    if out_JSON or out_JSON_dir:
        
        if out_JSON == True:
            
            jpath = str(_Path.cwd())+f'/{in_path.stem}.JSON'
            dfp.to_json(path_or_buf=jpath, orient='index', indent=4)
            return
        
        elif out_JSON_dir:
            
            if out_JSON_dir[-1] != '/':
                out_JSON_dir += '/'
                
            jpath = out_JSON_dir+f'{in_path.stem}.JSON'
            dfp.to_json(path_or_buf=jpath, orient='index', indent=4)
            return
        
        else:
            dfp.to_json(path_or_buf=out_JSON,orient='index',indent=4)
            return
    
    return dfp
    

def plot_top_disp(in_file, out_dir=str(_Path.cwd()), out_path=False):

    in_path = _Path(in_file)
    dfp = get_last_RECPOS(in_file)
    dfp.columns = ['X','Y','Z','3D']
    
    f1, a1 = _plt.subplots(figsize=(12,10))
    dfp.sort_values(by='3D').tail(10).plot(y=['X','Y','Z','3D'],ax=a1,kind='bar')
    a1.set_ylabel('Displacement (m)',fontsize=15)
    a1.grid('on')
    a1.set_title(f'Top 10 Displacements - {in_path.stem}')
    if out_path:
        f1.savefig(out_path,facecolor='w')
    else:
        f1.savefig(f'{str(out_dir)}/{in_path.stem}_top_displacements.png',facecolor='w')


if __name__ == "__main__":

    # Introduce command line parser
    parser = _ap.ArgumentParser(
        description = 'Given path to .SUM file, produce a JSON file of all station displacement at last epoch and/or plot of top 10 displacements'
    )
    # Command line function arguments
    parser.add_argument("in_path", 
        help = "Path to .SUM file"
    )
    parser.add_argument("out_dir", 
        help = "Output directory for JSON and/or plot"
    )
    parser.add_argument("-p", "--plot_flag", action='store_true',
        help = "Option to output plot of top 10 station displacements with default filename (same as .SUM)"
    )
    parser.add_argument("-j", "--json_flag",action='store_true',
        help = "Option to output JSON file with default filename (same as .SUM)"
    )
    parser.add_argument("-p_file", "--plot_out",
        help = "Option to output plot of top 10 station displacements with supplied filename"
    )
    parser.add_argument("-j_file", "--json_out",
        help = "Option to output JSON file with supplied filename"
    )

    # Get command line args:
    args = parser.parse_args()
    # Assign variables from command line args:
    in_path = args.in_path
    out_dir = args.out_dir
    plot_flag = args.plot_flag
    json_flag = args.json_flag
    plot_out = args.plot_out
    json_out = args.json_out
    
    if out_dir[-1] != '/':
        out_dir+='/'
    
    if json_flag:
        get_last_RECPOS(in_path, out_JSON_dir=out_dir)
    if plot_flag:
        plot_top_disp(in_path, out_dir=out_dir)

    if json_out:
        get_last_RECPOS(in_path, out_JSON=out_dir+json_out)
    if plot_out:
        plot_top_disp(in_path, out_path=out_dir+plot_out)      
    
    
    
