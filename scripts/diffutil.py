#!/usr/bin/env python3

'''Utility for testing trace files equivalence. Checks estimate blocks (marked with $) and SV residuals (+-Residuals)
The functionality is based on assert_frame_equal method (https://pandas.pydata.org/docs/reference/api/pandas.testing.assert_frame_equal.html)'''
import argparse
import logging as _logging
import os as _os
import sys as _sys

from gn_lib.gn_diffaux import diffionex, diffsnx, diffstec, difftrace


def parse_arguments():
    parser = argparse.ArgumentParser(description='''Compares the content of two files, and returns errors on difference. \
Supports trace (either .trace or .SUM), sinex, ionex and stec files. The user is required to set a file type with -t option.''')
    parser.add_argument('-i', '--file1', type=file_path,help='path to a compared file (.trace|.snx|.I|.stec). Can be compressed with LZW (.Z) or gzip (.gz)')
    parser.add_argument('-o', '--file2', type=file_path,help='path to a file to compare to - e.g. a corresponding solution file')
    parser.add_argument('-t','--type', action='store',type=str,help = "type of files for comparison",required = True,choices=["sinex","trace","ionex","stec"])
    parser.add_argument('-a', '--atol',   type=float,help='absolute tolerance',default=None)
    parser.add_argument('-c', '--coef',   type=float,help='std coefficient. Default value is 1 so bounds are +- std',default=1)
    parser.add_argument('-p', '--passthrough', action='store_true',help='passthrough or return 0 even if failed. Useful for pipeline runs.')
    # parser.add_argument('--warn_errors', action='store_false',help='log errors as warnings. Useful to "hide" optional checks') # Not used
    parser.add_argument('--states_only', action='store_true',help='analyse only states blocks (trace files option only)')
    return parser.parse_args()

def file_path(path):
    if _os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")

if __name__ == "__main__":
    parsed_args = parse_arguments()
    _logging.getLogger().setLevel(_logging.INFO)
    _logging.info(f':diffutil {parsed_args.type} configuration selected')
    _logging.info(f':diffutil testing of {_os.path.abspath(parsed_args.file1)} using {f"STD values" if parsed_args.atol == None else f"{parsed_args.atol:.1E} tolerance"}')

    status = 0
    log_lvl = 40 if parsed_args.atol is None else 30 # 40 is error, 30 is warning. Constant tolerance differences are reported as warnings
    if parsed_args.type == 'trace':
        status = difftrace(trace1_path=parsed_args.file1,trace2_path=parsed_args.file2,atol=parsed_args.atol,std_coeff=parsed_args.coef,states_only=parsed_args.states_only,log_lvl=log_lvl)
    elif parsed_args.type == 'sinex':
        status = diffsnx(snx1_path = parsed_args.file1,snx2_path = parsed_args.file2,atol=parsed_args.atol,std_coeff=parsed_args.coef,log_lvl=log_lvl)
    elif parsed_args.type == 'ionex':
        status = diffionex(ionex1_path=parsed_args.file1,ionex2_path=parsed_args.file1,atol=parsed_args.atol,log_lvl=log_lvl)
    elif parsed_args.type == 'stec':
        status = diffstec(path1 = parsed_args.file1,path2 = parsed_args.file2,atol=parsed_args.atol,std_coeff=parsed_args.coef,log_lvl=log_lvl)

    if status:
        if not parsed_args.passthrough:
            _logging.error(msg = f':diffutil failed [{_os.path.abspath(parsed_args.file1)}]\n')
            _sys.exit(status)
        else:
            _logging.info(msg = ':diffutil failed but no sys.exit as passthrough enabled\n')
    else:
        _logging.info(':diffutil [ALL OK]')
