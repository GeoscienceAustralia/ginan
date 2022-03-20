'''Utility for comparing sp3 files'''
import argparse
import os as _os

import matplotlib.pyplot as plt

from gn_lib.gn_io.sp3 import diff_sp3_rac, read_sp3
from gn_lib.gn_plot import plot_vec

def parse_arguments():
    parser = argparse.ArgumentParser(description='Compares two sp3 files and outputs RAC residuals plot. Sp3 files can be LZW (.Z) or Gzip (.gz) compressed. \
        If -hlm parameter provided, will do the helmert transormation of sp3_b file into the frame of sp3_a and append sp3_b residuals plot to the bottom. \
        The helmert inversion mode is used to selected at which step to do the parameter computation and transfomation: ECF (as in sp3) or ECI.')
    parser.add_argument('sp3_a',type=file_path,help='path to the main sp3 file which will be used to interpolate velocities and compute RAC rotation matrix')
    parser.add_argument('sp3_b',type=file_path,help='path to another sp3 file')
    parser.add_argument('-hlm', '--hlm_mode', help='helmert inversion mode',default=None, choices=(None, 'ECF', 'ECI'))
    parser.add_argument('-o', '--output', help='plot output path',default=None)
    return parser.parse_args()

def file_path(path):
    if _os.path.exists(path):
        return path
    else:
        raise argparse.ArgumentTypeError(f"{path} is not a valid path")

if __name__ == "__main__":
    parsed_args = parse_arguments()

    sp3_a = read_sp3(parsed_args.sp3_a)
    sp3_b = read_sp3(parsed_args.sp3_b)
    a = diff_sp3_rac(sp3_a, sp3_b,hlm_mode=parsed_args.hlm_mode)

    extended_plot = a.attrs['hlm_mode'] is not None

    fig = plt.figure(figsize=(10, 5+5*extended_plot),dpi=100)
    gs = fig.add_gridspec(3+3*extended_plot, hspace=0.2)
    ax = gs.subplots(sharex=True, sharey=False)
    plot_vec(axes=ax,df=a.unstack()['EST_RAC'] * 100000,axes_idx=[1,2,0])

    if extended_plot: # append hlm residuals plot if transformation has been selected 
        line = plt.Line2D([0,1],[0.49,0.49], transform=fig.transFigure, color="black",ls='--')
        plt.text(0.015,0.485,va = 'top',
        s=f"{a.attrs['sp3_b']} - {a.attrs['sp3_b']}" + f" (HLM in {a.attrs['hlm_mode']})\nResiduals shown are in ECI frame",
        rotation=90, transform=fig.transFigure, fontfamily='monospace')
        fig.add_artist(line)

        plot_vec(axes=ax,df=a.attrs['hlm'][1].RES.unstack() * 100000,axes_idx=[3,4,5],legend=False)

        hlm = a.attrs['hlm'][0][0].reshape(-1).tolist()
        hlm_txt = ('HLM coeffiecients:\n\n' 
                    + f'Tx {hlm[0]:13.5e}\nTy {hlm[1]:13.5e}\nTz {hlm[2]:13.5e}\n'
                    + f'Rx {hlm[0]:13.5e}\nRy {hlm[1]:13.5e}\nRz {hlm[2]:13.5e}\n'
                    + f'μ  {hlm[0]:13.5e}')
        text2 = plt.text(0.815,0.485,s=hlm_txt,va = 'top', transform=fig.transFigure, fontfamily='monospace')

    fig.suptitle(a.attrs['sp3_a'] + ' - ' + a.attrs['sp3_b'] + (f" (HLM in {a.attrs['hlm_mode']})" if extended_plot else ""),y=0.92)
    fig.patch.set_facecolor('w') #white background (non-transparent)
    fig.legend(bbox_to_anchor = (.955,.89),ncol=2,fontsize=8)
    plt.subplots_adjust(right=0.8)

    fig.savefig(f"{a.attrs['sp3_a'].split('.')[0]}-{a.attrs['sp3_b'].split('.')[0]}.pdf" if parsed_args.output is None else parsed_args.output)
