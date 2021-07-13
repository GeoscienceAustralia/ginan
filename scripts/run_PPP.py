'''
Given a station, year, date, download and output directory for the pea:
- Download the necessary files to run the pea
- Create a YAML config file to run the PPP use case
- Run the pea using the downloaded files and YAML config


Input:
station - IGS station in RINEX3 style format                                        - str
year - year in YYYY format                                                          - str
date - date in DDD day-of-year format (or MM-DD format when -md option selected)    - str
input_dir - directory to download and read input files from                         - str
output_dir - directory to output pea processed files                                - str


Output:
TRACE file containing PPP results for each epoch

Ronald Maj
2020-11-18 15:41
'''
import argparse
import subprocess
import numpy as np

from pathlib import Path
from ftplib import FTP, FTP_TLS

from gn_lib.gn_const import GPS_ORIGIN, J2000_ORIGIN
from gn_lib.gn_io.trace import _read_trace as read_trace

from trace_plot import pos_plot, trop_plot
from create_yaml_PPP import create_yaml_file


from datetime import datetime, timedelta

from get_pea_files import get_pea_files, gpsweekD



def find_pea_exe():
    '''
    Find the location of the pea executable

    Searches through from the current directory back 
    and finds the pea executable file using pathlib library
    Output is Path object
    '''
    pwd = Path.cwd()
    dir_tree = [pwd] + list(pwd.parents)
    
    for path in dir_tree:
        search_path = list(path.glob('*pea'))

        if not search_path == []:
            pea_path = search_path[0]

    for path in pea_path.rglob('*pea'):
        if path.is_file():
            pea_exe = path

    return pea_exe


if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(
        description = 'Get snx, clk, sp3, obs and nav files from CDDIS necessary for running pea'
        )
    
    # Command line function arguments
    parser.add_argument("stations", 
        help = "GNSS station/s - must be new RINEX3 format (9 char) and comma-separated for lists"
        )

    parser.add_argument("start_datetime", 
        help = "Start epoch - 'YYYY-DOY_hh:mm:ss' format"
    )
    
    parser.add_argument("end_datetime", 
        help = "End epoch - 'YYYY-DOY_hh:mm:ss' format"
    )

    parser.add_argument("input_dir", 
        help = "Input / Download directory"
    )  

    parser.add_argument("output_dir", 
        help = "Output directory"
    )  

    parser.add_argument("-md", "--month_day", action = "store_true", 
        help = "Option to replace DOY with month_day input with format MM-DD"
    )

    parser.add_argument("-f_pref", "--file_prefix", 
        help = "Option to provide a file prefix for the trace files"
    )

    parser.add_argument("-plot", "--plot_flag", action = 'store_true',
        help = "Option to output plots"
    )

    parser.add_argument("-proc_n", "--process_noise", action = 'store', nargs = 2,
        help = """Option to edit the process noise of the POS Kalman filter. Must give two arguments.
                Process Noise value in (m) followed by rate (hour, second, etc)"""
    )

    parser.add_argument("-rapid", "--rapid_files", action = "store_true",
        help = "Option to run with rapid files instead. Set doy to current day to find most recent rapid files"
    )

    parser.add_argument("-del_yaml", "--delete_yaml", action = "store_true",
        help = "Option to delete the yaml file after the PEA has run"
    )

    parser.add_argument("-m_r", "--most_recent_2days", action = "store_true",
        help = "Option to find the most recent 2-day period that has CDDIS products, then download and run"
    )

    parser.add_argument("-plt_m_r", "--plot_most_recent", action = "store_true",
        help = "Option to produce ZTD and XYZ plots for the most recent run, displaying only the most recent day"
    )

    # Get command line args:
    args = parser.parse_args()
    
    # Assign variables from command line args:
    output_dir = args.output_dir
    input_dir = args.input_dir
    
    start_datetime_str = args.start_datetime
    end_datetime_str = args.end_datetime

    station = args.stations
    stations = station.split(',')

    file_prefix = args.file_prefix
    md_flag = args.month_day
    plot_flag = args.plot_flag
    proc_n = args.process_noise
    rapid = args.rapid_files
    delete_yaml = args.delete_yaml 
    m_r_2days = args.most_recent_2days
    plt_m_r = args.plot_most_recent

    if not proc_n:
        proc_n = None

    if not rapid:
        rapid = False

    if not file_prefix:
        file_prefix = f'{station[:4]}'

    if not m_r_2days:
        m_r_2days = False
        rapid = True

    if not plt_m_r:
        plt_m_r = False


    if m_r_2days | plt_m_r:
        # Find the most recent (mr) two days available:
        print(f'\nSearching files on CDDIS for most recent 2-day period')
        # Connect to CDDIS server
        print('\nConnecting to CDDIS server...')
        ftps = FTP_TLS('gdc.cddis.eosdis.nasa.gov')
        ftps.login()
        ftps.prot_p()
        print('Connected.')
        ftps.cwd('gnss/products')
        print('Searching for most recent rapid files')
        
        dir_list = ftps.nlst()
        digits = ['0','1','2','3','4','5','6','7','8','9']
        weeks_dir = [int(x) for x in dir_list if x[0] in digits]
        ftps.cwd(str(max(weeks_dir)))
        
        mr_files_list = ftps.nlst()
        mr_clk_files = [f for f in mr_files_list if f.endswith('.clk.Z')]
        
        if mr_clk_files == []:
            ftps.cwd(f'../{max(weeks_dir) - 1}')
            mr_files_list = ftps.nlst()
            mr_clk_files = [f for f in mr_files_list if f.endswith('.clk.Z')]

        max_day = 0
        for clk_file in mr_clk_files:
            gps_wkday = int(clk_file[3:8])
            if gps_wkday > max_day:
                max_day = gps_wkday

        mr_wk = int(str(max_day)[:-1])
        mr_day = int(str(max_day)[-1])
        TD = np.timedelta64
        mr_date = GPS_ORIGIN + TD(mr_wk,"W") + TD(mr_day,'D')

        start_datetime_str = (mr_date - TD(1,'D')).astype(datetime).strftime('%Y-%j_%H:%M:%S')
        end_datetime_str = mr_date.astype(datetime).strftime('%Y-%j_')+'23:59:30'
        md_flag = False
        file_prefix = f'M_R_run_{station[:4]}'
        print(f'\nFound most recent - running now from {start_datetime_str} to {end_datetime_str} UTC')
        

    # Get the necessary pea files:
    if md_flag:
        date_format = '%Y-%m-%d_%H:%M:%S'
    else:
        date_format = '%Y-%j_%H:%M:%S'
    dt_start = datetime.strptime(start_datetime_str,date_format)
    dt_end = datetime.strptime(end_datetime_str,date_format)
    year = dt_start.strftime('%Y')
    doys = [dt_start + timedelta(days=x) for x in range((dt_end-dt_start).days+1)]
    doys = list(reversed(doys))
    
    # Run through dates in list and get the pea files:
    files_dict = {}
    doys_actual = []
    for date in doys:
        for station in stations:
            files_dict, dt = get_pea_files(station, date.strftime('%Y'), date.strftime('%j'), input_dir, rapid=rapid, out_dict=files_dict)
            doys_actual.append(dt)
    
    doys_actual = list(reversed(doys_actual))

    # Create suffix for yaml file
    doy_strt = doys_actual[0].strftime('%j')
    file_suff = f'_PPP_{year}-{doy_strt}'
    
    if len(doys)>1:
        doy_end = doys_actual[-1].strftime('%j')
        file_suff += f'-{doy_end}'
    
    if proc_n:
        proc_val,proc_rate = proc_n
        file_suff += f'_procn-{"".join(proc_val.split("."))}-{proc_rate[0]}'

    if rapid:
        file_suff += f'_rapid'

    dt_start_str = f'{doys_actual[0].strftime("%Y-%j")}_{dt_start.strftime("%H:%M:%S")}'
    dt_start = datetime.strptime(dt_start_str,'%Y-%j_%H:%M:%S')

    dt_end_str = f'{doys_actual[-1].strftime("%Y-%j")}_{dt_end.strftime("%H:%M:%S")}'
    dt_end = datetime.strptime(dt_end_str,'%Y-%j_%H:%M:%S')
    
    # Create the yaml file
    f_yaml = create_yaml_file(
        stations, 
        dt_start, 
        dt_end, 
        input_dir, 
        output_dir, 
        file_prefix, 
        file_suff, 
        proc_n=proc_n, 
        rapid=rapid,
        man_file_input=files_dict)

    # Create output directory if it doesn't exist:
    if not Path(output_dir).is_dir():
        Path(output_dir).mkdir(parents=True)

    # Write temp shell script to run the pea:
    scr = f'#!/bin/sh \n{find_pea_exe()} --config {f_yaml}'
    with open('run_pea_script.sh', 'w') as f:
        f.write(scr)
    
    # Run the pea by writing a temp shell script
    subprocess.run(['chmod','a+rx','run_pea_script.sh'])
    subprocess.run(['./run_pea_script.sh'])
    
    # Remove temp shell script file and if selected, also config file:
    #Path('run_pea_script.sh').unlink()
    if delete_yaml:
        Path(f_yaml).unlink()
    
    if plt_m_r: # Plot most recent results
        data = read_trace(output_dir+'/ExTemplate/'+file_prefix+'_'+file_suff+'.TRACE')
        st_epoch = 2880 # Start plotting from day 2
        epochs = data.iloc[st_epoch:].index.get_level_values(1).values
        epoch1 = J2000_ORIGIN + np.timedelta64(epochs[0],'s')
        date_str = epoch1.astype(datetime).strftime("%Y-DOY-%j")
        
        for station in stations:
            
            # Directory for plots:
            plt_path = Path(output_dir)/Path('m_r_prod_plots/'+date_str)
            if not plt_path.is_dir():
                plt_path.mkdir(parents=True)

            pos_plot(data, station, st_epoch = st_epoch, save_fig = str(plt_path)+'/')
            trop_plot(data, station, st_epoch = st_epoch, save_fig = str(plt_path)+'/')
        

    '''
    # If plot flag on, plot the figures of POS:
    if plot_flag:

        if multiday_flag:
            # Even if the span is over multiple weeks, just compare against the initial SNX solution from IGS
            gpswk = gpsweekD(year,doy_strt,False)
            # Input necessary PPP and SNX files to create plots
            
            if proc_n:
                f1, f2 = plot_PPP(
                    f'{output_dir}/EX01_IF/EX01_IF-{station[:4]}{year}{doy_strt}00_multi_{(doy[-1]-doy[0]).days+1}D_procn-{"".join(proc_val.split("."))}.TRACE',
                    input_dir+f'/products/igs{year[2:]}P{gpswk}.snx',
                    multi=multiday_flag
                )
            else:
                f1, f2 = plot_PPP(
                    f'{output_dir}/EX01_IF/EX01_IF-{station[:4]}{year}{doy_strt}00_multi_{(doy[-1]-doy[0]).days+1}D.TRACE',
                    input_dir+f'/products/igs{year[2:]}P{gpswk}.snx',
                    multi=multiday_flag
                )            

        else:
            # Get GPS week for given date
            gpswk = gpsweekD(year,doy,False)
            # Plot using PPP file and sinex for given GPS week
            f1, f2 = plot_PPP(
                f'{output_dir}/EX01_IF/EX01_IF-{station[:4]}{year+doy}00.TRACE',
                input_dir+f'/products/igs{year[2:]}P{gpswk}.snx'
            )
        
        # Create a plots directory if it doesn't exist
        if not Path(output_dir+'/EX01_IF/plots').is_dir():
            Path(output_dir+'/EX01_IF/plots').mkdir(parents=True)  
        
        # Move files to plots directory
        subprocess.run(['mv',f1,output_dir+'/EX01_IF/plots'])
        subprocess.run(['mv',f2,output_dir+'/EX01_IF/plots'])
    ''' 

