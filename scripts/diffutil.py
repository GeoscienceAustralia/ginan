#!/usr/bin/env python3

'''Utility for testing trace files equivalence.'''
import argparse
import logging as _logging
import os as _os
import sys as _sys

from gn_lib import gn_diffaux as _gn_diffaux

#TODO convert argparse to click
def parse_arguments():
    parser = argparse.ArgumentParser(description='''Compares the content of two files, and returns errors on difference. \
Supports trace (either .trace or .SUM), sinex, ionex and stec files. The user is required to set a file type with -t option.''')
    parser.add_argument('-i','--file1',type=file_path,required=True,
        help='path to a compared file (.trace|.snx|.I|.stec). Can be compressed with LZW (.Z) or gzip (.gz)')
    parser.add_argument('-o','--file2',type=file_path,required=True,
        help='path to a file to compare to - e.g. a corresponding solution file')
    parser.add_argument('-t','--type', type=str,action='store',default="auto",required=False,
        choices=["auto","sinex","trace","ionex","stec","clk","sp3","pod"],
        help = "type of files for comparison")
    parser.add_argument('-a','--atol', type=float,default=None,
        help='absolute tolerance')
    parser.add_argument('-c','--coef', type=float,default=1,
        help='std coefficient. Default value is 1 so bounds are +- std')
    parser.add_argument('-p','--plot', action='store_true',
        help='output a plot to terminal. Requires plotext')
    parser.add_argument(     '--passthrough',action='store_true',
        help='passthrough or return 0 even if failed. Useful for pipeline runs.')
    parser.add_argument('--aux1', type=file_path,default=None,
        help='path to aux "a" file')
    parser.add_argument('--aux2', type=file_path,default=None,
        help='path to aux "b" file')
    return parser.parse_args()

def file_path(path):
    if _os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")

def path2type(path):
    """
    Returns a suffix of a file from a path,
    Uses a dict to correct for known suffix issues file types. 
    If not present in dict -> return suffix as extracted.
    """
    basename = _os.path.basename(path)
    suffix = basename.split('.')[1].lower()
    filetype_dict = {'snx':'sinex','sum':'trace','eph':'sp3'}
    if suffix in filetype_dict.keys():
        return filetype_dict[suffix]
    elif suffix == 'out':
        return basename[:3]
    elif suffix[:2].isdigit and suffix[2]=='i':
        return 'ionex'
    return suffix

if __name__ == "__main__":
    parsed_args = parse_arguments()
    _logging.getLogger().setLevel(_logging.INFO)
    _logging.info(f":diffutil ========== STARTING DIFFUTIL ==========")
    _logging.info(f":diffutil input1 [-i]: {_os.path.abspath(parsed_args.file1)}")
    _logging.info(f":diffutil input2 [-o]: {_os.path.abspath(parsed_args.file2)}" )

    types = parsed_args.file1
    if parsed_args.type == 'auto':
        types = path2type(parsed_args.file1)
        _logging.info(f":diffutil determined '{types}' based on input1 as auto selected" )
    _logging.info(f":diffutil selected {'STD values' if parsed_args.atol == None else f'{parsed_args.atol:.1E} tolerance'} threshold")

    status = 0
    log_lvl = 40 if parsed_args.atol is None else 30 # 40 is error, 30 is warning. Constant tolerance differences are reported as warnings
    if types == 'trace':
        status = _gn_diffaux.difftrace(trace1_path=parsed_args.file1,trace2_path=parsed_args.file2,atol=parsed_args.atol,std_coeff=parsed_args.coef,log_lvl=log_lvl,plot=parsed_args.plot)
    elif types == 'sinex':
        status = _gn_diffaux.diffsnx(snx1_path = parsed_args.file1,snx2_path = parsed_args.file2,atol=parsed_args.atol,std_coeff=parsed_args.coef,log_lvl=log_lvl)
    elif types == 'ionex':
        status = _gn_diffaux.diffionex(ionex1_path=parsed_args.file1,ionex2_path=parsed_args.file1,atol=parsed_args.atol,log_lvl=log_lvl)
    elif types == 'stec':
        status = _gn_diffaux.diffstec(path1 = parsed_args.file1,path2 = parsed_args.file2,atol=parsed_args.atol,std_coeff=parsed_args.coef,log_lvl=log_lvl)
    elif types == 'clk':
        status = _gn_diffaux.diffclk(clk_a_path=parsed_args.file1,clk_b_path=parsed_args.file2,atol=parsed_args.atol,log_lvl=log_lvl)
    elif types == 'sp3':
        status = _gn_diffaux.diffsp3(sp3_a_path=parsed_args.file1,sp3_b_path=parsed_args.file2,clk_a_path=parsed_args.aux1,clk_b_path=parsed_args.aux2,tol=parsed_args.atol,log_lvl=log_lvl)
    elif types == 'pod':
        status = _gn_diffaux.diffpodout(pod_out_a_path=parsed_args.file1,pod_out_b_path=parsed_args.file2,tol=parsed_args.atol,log_lvl=log_lvl)
    if status:
        if not parsed_args.passthrough:
            _logging.error(msg = f':diffutil failed [{_os.path.abspath(parsed_args.file1)}]\n')
            _sys.exit(status)
        else:
            _logging.info(msg = ':diffutil failed but no sys.exit as passthrough enabled\n')
    else:
        _logging.info(':diffutil [ALL OK]')
