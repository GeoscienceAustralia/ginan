'''sinex stations quick view'''
import argparse
import os as _os


from gn_lib.gn_io.sp3 import merge_sp3, write_sp3

def parse_arguments():
    parser = argparse.ArgumentParser(description='Merge sinex SITE/ID block and create html map.')
    parser.add_argument('-s', '--sp3list',help='sp3 files paths',nargs="+",default=[])
    parser.add_argument('-c', '--clklist',help='clk paths',nargs="+",default=None)
    parser.add_argument('-o', '--outpath',help='path to output dir',default=_os.curdir + '/merge.sp3')
    return parser.parse_args()

def file_path(path):
    if _os.path.isfile(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")



if __name__ == "__main__":
    parsed_args = parse_arguments()
    print(parsed_args.outpath)
    if parsed_args.sp3list:
        merged_df = merge_sp3(sp3_paths = parsed_args.sp3list, clk_paths = parsed_args.clklist)
        write_sp3(sp3_df = merged_df, path = parsed_args.outpath)
    else:
        print('sp3 list is empty')



