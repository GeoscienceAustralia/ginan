'''rclone config file (content from rclone.conf):
{
    "cddis": {
        "explicit_tls": "true",
        "host": "gdc.cddis.eosdis.nasa.gov",
        "pass": "9MRceLq8yzJYM_QgC-lvqyCrLuwY",
        "type": "ftp",
        "user": "anonymous"
    },
    "igs": {
        "host": "igs-rf.ign.fr",
        "pass": "fwo4zh4gA9npVM5kMsT9R4rrfq1J",
        "type": "ftp",
        "user": "anonymous"
    },
    "itrf": {
        "host": "itrf-ftp.ign.fr",
        "pass": "E10y_Y3Zg3pNnGNJAjT1QnUq3G5D",
        "type": "ftp",
        "user": "anonymous"
    }'''

import argparse
import os as _os

from gn_lib.gn_io.igslog import write_meta_gather_master
description = (
'''IGS log files parsing utility. Globs over log files using LOGGLOB expression and outputs SINEX metadata file.
If provided with frame and frame discontinuity files (soln),
 will project the selected stations present in the frame to the datetime specified.
How to get the logfiles: 
rclone sync igs:pub/sitelogs/ /data/station_logs/station_logs_IGS -vv
How to get the frame files: 
rclone sync itrf:pub/itrf/itrf2014 /data/ITRF/itrf2014/ -vv --include "*{gnss,IGS-TRF}*" --transfers=10
rclone sync igs:pub/ /data/TRF/ -vv --include "{IGS14,IGb14,IGb08,IGS08}/*"
see rclone config options inside this script file
Alternatively, use s3 bucket link to download all the files needed s3://peanpod/aux/''')

def parse_arguments():
    parser = argparse.ArgumentParser(description=description)
    parser.add_argument('-l', '--logglob', help='logs glob path (required)')
    parser.add_argument('-r', '--rnxglob', help='rinex glob path (optional)',default=None,nargs="+")
    parser.add_argument('-o', '--outfile', help='output file path (optional)',default= './meta_gather.snx')
    parser.add_argument('-fs', '--frame_snx', type=file_path, help='frame sinex file path (optional)',default=None)
    parser.add_argument('-fd', '--frame_dis', type=file_path, help='frame discontinuities file path (required with --frame_snx)',default=None)
    parser.add_argument('-fp', '--frame_psd', type=file_path, help='frame psd file path (optional)',default=None)
    parser.add_argument('-d', '--datetime', help='date to which project frame coordinates, default is today (optional)',default=None)
    parser.add_argument('-n', '--num_threads', type=int, help='number of threads to run in parallel',default=None)
    return parser.parse_args()

def file_path(path):
    if _os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")

if __name__ == "__main__":
    parsed_args = parse_arguments()
    rnxglob = parsed_args.rnxglob

    if isinstance(rnxglob,list):
        if (len(rnxglob)==1) & (rnxglob[0].find('*')!=-1): # it's rnx_glob expression (may be better to check if star is present)
            rnxglob = rnxglob[0]

    write_meta_gather_master(logs_glob_path=parsed_args.logglob,
            rnx_glob_path=rnxglob,
            out_path=parsed_args.outfile,
            frame_snx_path = parsed_args.frame_snx,
            frame_soln_path = parsed_args.frame_dis,
            frame_psd_path = parsed_args.frame_psd,
            frame_datetime=parsed_args.datetime,
            num_threads=parsed_args.num_threads)
