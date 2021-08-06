'''
Given relevant directories and stations (and dates if not gathering most recent):
- Download the necessary files to run the PEA
- Create a YAML config file based on input template
- Run the PEA with the customised example
'''
import numpy as _np
import pandas as _pd
import argparse as _ap
import subprocess as _sp
from pathlib import Path as _Path
from datetime import datetime as _datetime
from datetime import timedelta as _timedelta

from create_yaml_custom import create_yaml_file
from plotting.trace_plot import pos_plot, trop_plot

from gn_lib.gn_const import J2000_ORIGIN
from gn_lib.gn_io.trace import _read_trace 
from gn_lib.gn_download import download_pea_prods, download_rinex3



if __name__ == "__main__":

    # Introduce command line parser
    parser = _ap.ArgumentParser(
        description = 'Run the PEA using custom settings - will get necessary product and data files from CDDIS, create yaml and run'
        )
    
    # Command line function arguments

    parser.add_argument("ginan_dir", 
        help = "Path to Ginan - pea exe will then be found in <ginan_dir>/bin/pea"
    )

    parser.add_argument("yaml_template_path", 
        help = "Path to YAML file to use as a template for the PEA run"
    )

    parser.add_argument("output_dir", 
        help = "Output directory of the PEA run"
    )

    parser.add_argument("data_dir", 
        help = "Directory to download data station (rnx) files - station_data in YAML"
    )

    parser.add_argument("stations", 
        help = "GNSS station/s - must be new RINEX3 format (9 char) and comma-separated for lists"
    )

    parser.add_argument("prod_dir", 
        help = "Directory to download products (sp3,snx,erp,etc.) - input_files in YAML"
    )

    parser.add_argument("--AC_pref", "-ac", default='igs',
        help = "Preference of Analysis Centre product to download - can be comma-separated to list preferences"
    )

    parser.add_argument("--start_datetime", "-st", default=False,
        help = "Start epoch - 'YYYY-DOY_hh:mm:ss' format"
    )
    
    parser.add_argument("--end_datetime", "-en", default=False,
        help = "End epoch - 'YYYY-DOY_hh:mm:ss' format"
    )

    parser.add_argument("--most_recent_x_days", "-m_r", default=False,
        help = "Rather than specifying precise dates find the most recent x-day period that has CDDIS products"
    )

    parser.add_argument("--month_day", "-md", action = "store_true", 
        help = "Option to replace DOY with month_day input with format MM-DD"
    )

    parser.add_argument("--trace_file_name", "-t_fnm",
        help = "Option to provide a file name for the trace files"
    )

    parser.add_argument("--plot_flag", "-p", action = 'store_true',
        help = "Option to output plots"
    )

    parser.add_argument("--plot_most_recent", "-p_m_r", action = "store_true", default=False,
        help = "Option to produce ZTD and XYZ plots for the most recent run, displaying only the most recent day"
    )

    parser.add_argument("--process_noise","-proc_n", action = 'store', nargs = 2,
        help = """Option to edit the process noise of the POS Kalman filter. Must give two arguments.
                Process Noise value in (m) followed by rate (hour, second, etc)"""
    )

    parser.add_argument("--out_yaml_path", "-o_yml", default='ex-custom.yaml',
        help = "Option to set path of output yaml file "
    )

    parser.add_argument("-del_yaml", "--delete_yaml", action = "store_true", default=True,
        help = "Option to delete the yaml file after the PEA has run"
    )

    parser.add_argument("--vmf3_flag", "-vmf", action = 'store_true', default=False,
        help = "Option to switch to vmf3 tropospheric model"
    )

    parser.add_argument("--snx_AC", "-ac_s", default='igs',
        help = "Option to choose Analysis Centre SINEX product to download - default: igs"
    )

    parser.add_argument("--broadcast_AC", "-ac_b", default='igs',
        help = "Option to choose Analysis Centre broadcast product to download - default: igs"
    )

    parser.add_argument("--clk_type","-c_typ",default='clk',
        help = "Option to change the clock file type downloaded - default: clk"
    )


    # Get command line args:
    args = parser.parse_args()
    
    # Assign variables from command line args:
    ginan_dir = args.ginan_dir
    output_dir = args.output_dir
    data_dir = args.data_dir
    prod_dir = args.prod_dir
    
    yml_path = args.yaml_template_path
    out_yaml_path = args.out_yaml_path
    del_yaml = args.delete_yaml 

    stations = args.stations.split(',')
    ac_pref = args.AC_pref.split(',')
    
    st_dt_str = args.start_datetime
    en_dt_str = args.end_datetime
    m_r_xdays = args.most_recent_x_days
    md_flag = args.month_day
    
    plt_flag = args.plot_flag
    plt_m_r_1 = args.plot_most_recent
    
    trace_fn = args.trace_file_name
    proc_n = args.process_noise
    vmf3 = args.vmf3_flag
    
    snx_ac = args.snx_AC
    brd_ac = args.broadcast_AC
    clk_type = args.clk_type

    if not proc_n:
        proc_n = None

    # Establish date range (if specified)
    if st_dt_str:
        if md_flag:
            date_format = '%Y-%m-%d_%H:%M:%S'
        else:
            date_format = '%Y-%j_%H:%M:%S'
        st_date = _datetime.strptime(st_dt_str,date_format)
        en_date = _datetime.strptime(en_dt_str,date_format)
        dt_list = _pd.date_range(start=str(st_date.date()),end=str(en_date.date()),freq='1D')
    else:
        dt_list = None

    # Select top analysis centre preference. If rapid or ultra-rapid, ensure sinex is specified appropriately
    ac=ac_pref[0]
    if (ac == 'igr') | (ac == 'igu'):
        snx_ac = 'igs'
    
    if m_r_xdays == 'True':
        m_r_xdays = True
    elif type(m_r_xdays)==str:
        m_r_xdays = int(m_r_xdays)

    # Download Product Files
    file_dict = download_pea_prods(
        dest=prod_dir,
        most_recent=m_r_xdays,
        dates=dt_list,
        ac=ac,
        out_dict=False,
        trop_vmf3=vmf3,
        brd_typ=brd_ac,
        snx_typ=snx_ac,
        clk_sel=clk_type
    )

    # Download Data Files
    rnx_update = download_rinex3(
        dates = file_dict['dates'], 
        stations = stations, 
        dest = data_dir, 
        dwn_src='cddis', 
        ftps=False, 
        f_dict=True
    )
    file_dict.update(rnx_update)
    if not trace_fn:
        trace_fn = f"CUSTOM-{file_dict['dates'][0].strftime('%Y%j')}-{file_dict['dates'][-1].strftime('%Y%j')}"

    # Create the YAML file
    f_yaml = create_yaml_file(
        input_prod_dir = prod_dir,
        input_data_dir = data_dir,
        output_dir = output_dir,
        in_yml_temp_path = yml_path,
        out_yml_path = out_yaml_path,
        file_input_dict = file_dict,
        start_datetime = file_dict['dates'][0],
        end_datetime = file_dict['dates'][-1]+_timedelta(hours=23,minutes=59,seconds=30),
        trace_file_name = trace_fn
    )

    # Create output directory and write shell script:
    if not _Path(output_dir).is_dir():
        _Path(output_dir).mkdir(parents=True)
    scr = f'#!/bin/sh \n{ginan_dir}/bin/pea --config {f_yaml}'
    with open('run_pea_script.sh', 'w') as f:
        f.write(scr)
    
    # Run the PEA custom example
    _sp.run(['chmod','a+rx','run_pea_script.sh'])
    _sp.run(['./run_pea_script.sh'])
    
    # Remove temp shell script file and if selected, also config file:
    _Path('run_pea_script.sh').unlink()
    if del_yaml:
        _Path(f_yaml).unlink()
    
    # if plt_m_r_1: # Plot most recent results
    #     data = _read_trace(output_dir+'/'+trace_fn+'.TRACE')
    #     st_epoch = 2880 # Start plotting from day 2
    #     epochs = data.iloc[st_epoch:].index.get_level_values(1).values
    #     epoch1 = J2000_ORIGIN + _np.timedelta64(epochs[0],'s')
    #     date_str = epoch1.astype(_datetime).strftime("%Y-DOY-%j")
        
    #     for station in stations:
            
    #         # Directory for plots:
    #         plt_path = _Path(output_dir)/_Path('m_r_prod_plots/'+date_str)
    #         if not plt_path.is_dir():
    #             plt_path.mkdir(parents=True)

    #         pos_plot(data, station, st_epoch = st_epoch, save_fig = str(plt_path)+'/')
    #         trop_plot(data, station, st_epoch = st_epoch, save_fig = str(plt_path)+'/')
    # elif plt_flag:
    #     data = _read_trace(output_dir+'/'+trace_fn+'.TRACE')
        