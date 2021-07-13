'''
Get RINEX 3.0 file (or read existing) and plot out chosen observables vs. inputted time period 

Usage:
    python3 obs_code_plot.py SSSSSSSSS YYYY DDD CCC[,CCC,CCC...] NNN[,NNN,NNN,...] h1:m1 h2:m2 --y_ax_label --plot_type
    OR
    python3 obs_code_plot.py SSSSSSSSS YYYY MM_DD -md CCC[,CCC,CCC...] NNN[,NNN,NNN,...] h1:m1 h2:m2 --y_ax_label --plot_type

This will plot an SNR vs time plot for: 
    station 'SSSSSSSSS'
    date 'YYYY' (year)
    with either:
            'DDD' (day-of-year [doy]) OR
            'MM' (month) 'DD' (day) 

    observation code/s to plot in form 'CCC' or for multiple codes 'CCC,CCC,...' or 'ALL' for all
    satellite number/s to plot in form 'NNN' or for multiple numbers 'NNN,NNN,...' or 'ALL' for all
    start time 'h1:m1'
    end time   'h2:m2'
    --optional label of y-axis of the plot/s '--y_ax_label'
    --optional choice of plot type, default is 'scatter', other options are 'line_plot'

This will be saved in directory: 'snr_plots'

Date: 2020-09-12 
Author: Ronald Maj 
'''
import wget
import argparse
import numpy as np
import subprocess
from datetime import datetime
from pathlib import Path
import georinex as gr
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from pandas import to_datetime 


def plot_code_vs_time(time_data, code_data, station, year, doy, sat_nums, start_time, end_time, ylabel = None, plot_type = None):
    # Plot the chosen code (could be multiple satellites) against time
    
    # Initiate figure
    fig1, ax1 = plt.subplots(1) 
        
    # If line_plot is chosen, plot a line plot
    if plot_type == 'line_plot':
        ax1.plot(
        time_data, 
        code_data
        )       
        # Add a legend:
        ax1.legend(sat_nums.split(','))
    # Otherwise create scatter plot
    else:
        for sat in sat_nums.split(','):
            ax1.scatter(
                time_data, 
                code_data.sel(sv=sat),
                label = sat
            )
        # Add a legend:
        ax1.legend()

    # Adjust x-limits to within those of the dataset
    ax1.set_xlim(
        left = time_data[0], 
        right = time_data[-1]) 
    # Ensure times alog x-axis are formatted nicel
    myFmt = mdates.DateFormatter('%H:%M')
    ax1.xaxis.set_major_formatter(myFmt) 
    fig1.autofmt_xdate() 

    # Unless otherwise specified, y axis label is just the observable code:
    if not ylabel:
        ylabel = code_data.name

    # Label the axes:
    ax1.set_xlabel('Time (hh:mm)') 
    ax1.set_ylabel(ylabel) 
    # And label the figure itself
    if len(sat_nums) > 20:
        fig1.suptitle(f'{station[:4]}-{year}-{doy}_{code_data.name}_PRN-{sat_nums[:21]}_{start_time}-{end_time} UTC') 
    else:
        fig1.suptitle(f'{station[:4]}-{year}-{doy}_{code_data.name}_PRN-{sat_nums}_{start_time}-{end_time} UTC')


    # Write figure to file in 'snr_plots' directory:
    st = f'{start_time[:2]}{start_time[3:]}'
    et = f'{end_time[:2]}{end_time[3:]}'
    if len(sat_nums) < 8: 
        out_filepath = f'snr_plots/{year}D{doy}_{station[:4]}_{st}-{et}_{sat_nums}_{code_data.name}.png'
    else:
        out_filepath = f'snr_plots/{year}D{doy}_{station[:4]}_{st}-{et}_MultiSat_{code_data.name}.png'
    fig1.savefig(out_filepath,format='png')

    return fig1.show()


if __name__ == "__main__":

    try:
        # Introduce command line parser
        parser = argparse.ArgumentParser(
            description = 'Get RINEX 3.0 file (or read existing) and plot out chosen observables vs. inputted time period'
            )
        
        # Command line function arguments
        parser.add_argument(
            "station", 
            help = "GPS station name - must be new RINEX3 format - 9 characters"
            )        
        
        parser.add_argument(
            "year", 
            help = "Year in YYYY format"
            )
        
        parser.add_argument(
            "doy", 
            help = "Day-of-year in DDD format - include leading zero if doy < 100"
            )
        
        parser.add_argument(
            "-md", 
            "--month_day", 
            action="store_true", 
            help = "Option to replace doy with month_day input with format MM-DD"
            )

        parser.add_argument(
            "obs_codes",
            help = 'RINEX3 Observation code/s to plot - comma separated list with no space, e.g. "C1C,L1C,S1W,S2W" or "ALL" to plot all obs codes'
            )        

        parser.add_argument(
            "sat_nums",
            help = "PRN Sat number/s to plot - comma separated list with no spaces, e.g. 'G26,G05,G16' or 'ALL' to plot all satellites"
            )

        parser.add_argument(
            "start_time",
            help = "Start Time in format hh:mm"
            )

        parser.add_argument(
            "end_time",
            help = "End Time in format hh:mm"
            )

        parser.add_argument(
            "--y_ax_label",
            default = None,
            help = "Label for the Y-axis on the plot"
            )
        
        parser.add_argument(
            "--plot_type",
            default = None,
            help = "Choice of plot, default = 'scatter' other options are: 'line_plot'"
            )

        # Get command line args:
        args = parser.parse_args()
        # And start assigning to variables:
        station = args.station
        year = args.year
        
        # Check if the RINEX file already exists, if not, download:
        if args.month_day:
            subprocess.run(["python3", "get_rinex3.py", station, year, args.doy, "-md"])
            month = args.doy[:2]
            day = args.doy[3:]
            dt_str = f'{year}-{month}-{day}'
            dt = datetime.strptime(dt_str,"%Y-%m-%d")
            doy = dt.strftime("%j")
        else:
            doy = args.doy
            subprocess.run(["python3", "get_rinex3.py", station, year, doy])
        # Get the filepath to the Rinex file (either to be read or downloaded to)
        rnx_filepath = Path(f"rinex3_files/{station}_R_{year}{doy}0000_01D_30S_MO.crx")
        
        # Assign other Command Line arguments:
        codes = args.obs_codes
        sat_nums = args.sat_nums
        start_time = args.start_time
        end_time = args.end_time

        # Convert start and end times to proper format:
        dt_start = to_datetime(
            f'{year}-{doy} {start_time}', format = '%Y-%j %H:%M'
        ).strftime('%Y-%m-%dT%H:%M')
        
        dt_end = to_datetime(
            f'{year}-{doy} {end_time}', format = '%Y-%j %H:%M'
        ).strftime('%Y-%m-%dT%H:%M')

        # Load the RINEX file
        obs = gr.load(
            rnx_filepath,
            tlim = [dt_start, dt_end],
            meas = codes.split(',')
            )
        
        # Check snr plots directory exists, if not create it:
        if not Path.exists(Path('snr_plots')):
            Path.mkdir(
                Path('snr_plots')
                ) 

        # Create and save figure for each obs. code
        
        # If ALL option is chosen for obs_codes AND sat_nums, plot all:
        if (codes == 'ALL') & (sat_nums == 'ALL'):
            # Get all satellites from Rinex file:
            sat_list = [x for x in obs.sv.values if 'G' in x]
            sat_nums = ','.join(sat_list)
            # Run through all observables (data variables) in Rinex file
            for code in list(obs.data_vars.keys()):
                plot_code_vs_time(
                    obs.time, 
                    obs[code].sel(sv = sat_nums.split(',')), 
                    station, 
                    year, 
                    doy, 
                    sat_nums, 
                    start_time, 
                    end_time,
                    args.y_ax_label,
                    args.plot_type
                    )
        
        # If ALL codes but not all satellites (SV):
        elif (codes == 'ALL') & (sat_nums != 'ALL'):
            # Run through all observables (data variables) in Rinex file
            for code in list(obs.data_vars.keys()):
                plot_code_vs_time(
                    obs.time, 
                    obs[code].sel(sv = sat_nums.split(',')),
                    station, 
                    year, 
                    doy, 
                    sat_nums, 
                    start_time, 
                    end_time,
                    args.y_ax_label,
                    args.plot_type
                    )
        
        # If ALL satellites (SV) but not all codes:
        elif (codes != 'ALL') & (sat_nums == 'ALL'):
            # Get all satellites from Rinex file:
            sat_list = [x for x in obs.sv.values if 'G' in x]
            sat_nums = ','.join(sat_list)
            # Run through chosen observables
            for code in codes.split(','):
                plot_code_vs_time(
                    obs.time, 
                    obs[code].sel(sv = sat_nums.split(',')),
                    station, 
                    year, 
                    doy, 
                    sat_nums, 
                    start_time, 
                    end_time,
                    args.y_ax_label,
                    args.plot_type
                    )        
        
        # Else just go through the chosen codes and satellites:
        for code in codes.split(','):
            plot_code_vs_time(
                obs.time, 
                obs[code].sel(sv=sat_nums.split(',')), 
                station, 
                year, 
                doy, 
                sat_nums, 
                start_time, 
                end_time,
                args.y_ax_label,
                args.plot_type
                )

    except IndexError:
        print('->->-Need to specify arguments correctly')
