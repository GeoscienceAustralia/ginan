'''
NEEDS TO BE RE-WRITTEN TO USE NEW DIRECTORY STRUCTURE (pull functions from gn_io.gn_download)

Given a station and time range, find flex events.

With flex events it is important to remember only GPS Block IIR-M and onwards have this capability
Therefore, we need to choose GPS satellites that can actually have flex power events
'''
import argparse
import numpy as _np
import pandas as _pd

import matplotlib.pyplot as _plt
import matplotlib.dates as mdates
from datetime import timedelta as _timedelta
from pathlib import Path as _Path


from gn_lib.gn_io.rinex import _read_rnx, _rnx_pos
from gn_lib.gn_io.sp3 import read_sp3 as _read_sp3
from gn_lib.gn_download import download_rinex3, download_sp3
from gn_lib.gn_datetime import j20002datetime
from gn_lib.gn_transform import xyz2llh_heik, llh2rot


def load_rnxs(rnxs):
    '''
    rnxs: List of paths to RINEX files to load
    '''
    obs = _read_rnx(rnxs[0])
    if len(rnxs)>1:
        for rnx in rnxs[1:]:
            obs_add = _read_rnx(rnx)
            obs = obs.combine_first(obs_add)
    return obs


def load_sp3s(sp3s, rec_loc=None, add_nad=True, add_el=True, add_az=True, add_dist=True):
    '''
    sp3s: List of paths to SP3 files to load
    rec_loc: The receiver location in XYZ coords
    add_*: If rec_loc given, choose which options are added
    '''
    orb = _read_sp3(sp3s[0])
    if len(sp3s)>1:
        for sp3 in sp3s[1:]:
            orb_add = _read_sp3(sp3)
            orb = orb.combine_first(orb_add)
    orb = orb.EST

    if (type(rec_loc)==list) or (type(rec_loc)==_np.ndarray):
        add_all_angs(
            df=orb, 
            station_pos=rec_loc,
            nad=add_nad,
            el=add_el,
            az=add_az,
            dist=add_dist)
    return orb


def get_load_rnxsp3(start_date, end_date, station, directory, sp3pref='igs'):
    '''
    Start date format: "YYYY-MM-DD" - str
    End   date format: "YYYY-MM-DD" - str
    Frequency  format: "xU"        - str
    x -> number of units U
    U -> unit of time, e.g. "S" (sec), "H" (hour), "D" (day), etc.
    directory : path to download files to - str
    '''
    
    dt_list = _pd.date_range(
        start = start_date,
        end = end_date,
        freq = '1D'
    ).to_pydatetime()
    
    download_rinex3(dates=dt_list, station=station, dest=directory, dwn_src='cddis')
    download_sp3(dates=dt_list, dest=directory, pref=sp3pref, dwn_src='cddis')
    
    rnxs = list(_Path(directory).glob('*.rnx'))
    print('Loading rnx files ...')
    df_rnx = load_rnxs(rnxs)
    df_rnx = df_rnx.swaplevel(axis=1).EST

    sp3s = list(_Path(directory).glob('*.sp3'))
    rec_pos = _rnx_pos(rnxs[0])
    print('Loading sp3 files ...')
    df_sp3 = load_sp3s(sp3s, rec_loc=rec_pos)
    
    # Create long index for sp3 data (30-sec spacing)
    dt_index = _pd.date_range(
        start = dt_list[0].strftime('%Y-%m-%d %H:%M:%S'),
        end = dt_list[0].strftime('%Y-%m-%d')+' 23:59:30',
        freq='30S'
    )

    for dt in dt_list[1:]:
        dt_index = dt_index.append(_pd.date_range(start=dt, end=dt.strftime('%Y-%m-%d')+' 23:59:30', freq='30S'))
    dt_count = len(dt_index)

    sv_index = []
    for val in df_sp3.index.get_level_values(1).unique().values:
        sv_index += [val]*dt_count

    dt_idx = list(dt_index.values)*len(df_sp3.index.get_level_values(1).unique().values)
    
    df_sp3_reset = df_sp3.reset_index()
    colsp3 = df_sp3_reset.columns
    df_sp3_reset.columns = ['time','sv']+list(colsp3[2:])
    df_sp3_reset['time'] = j20002datetime(df_sp3_reset['time'].values)
    df_sp3 = df_sp3_reset.set_index(['time','sv'])

    long_indices = _pd.MultiIndex.from_arrays([dt_idx,sv_index],names=['time','sv'])

    # Interpolate angle data in sp3 dataframe
    df_long_sp3 = _pd.merge(_pd.DataFrame(index=long_indices),df_sp3[['nad_ang','el_ang','az_ang','dist']],how='outer',on=['time','sv'])
    df_long_sp3['nad_ang'] = df_long_sp3.reset_index().pivot(index='time',columns='sv',values='nad_ang').interpolate(method='cubic').melt(ignore_index=False)[['value']].set_index(long_indices).value
    df_long_sp3['el_ang'] = df_long_sp3.reset_index().pivot(index='time',columns='sv',values='el_ang').interpolate(method='cubic').melt(ignore_index=False)[['value']].set_index(long_indices).value
    df_long_sp3['az_ang'] = df_long_sp3.reset_index().pivot(index='time',columns='sv',values='az_ang').interpolate(method='cubic').melt(ignore_index=False)[['value']].set_index(long_indices).value
    df_long_sp3['dist'] = df_long_sp3.reset_index().pivot(index='time',columns='sv',values='dist').interpolate(method='cubic').melt(ignore_index=False)[['value']].set_index(long_indices).value

    # Combine rnx and sp3 data - output dataframe
    df_rnx_reset = df_rnx.reset_index()
    df_rnx_reset['level_1'] = j20002datetime(df_rnx_reset['level_1'].values)

    cols = df_rnx_reset.columns
    df_rnx_reset = df_rnx_reset[cols[1:]]
    df_rnx_reset.columns = ['time','sv'] + list(cols[3:])

    return _pd.merge(df_rnx_reset.set_index(['time','sv']).sort_index(),df_long_sp3[['nad_ang','el_ang','az_ang','dist']],how='outer',on=['time','sv'])


def find_flex_events(
    df_in, 
    codes, 
    station='None',
    start_floor=33.0, 
    end_floor=30.0, 
    jump=5.0, 
    el_min=15.0,
    GPS_flex=True,
    csv_out=True,
    csv_dest=False,
    csv_name=False,
    plot=False, 
    plot_dest=False, 
    plot_spread=1500,
    file_nameorder=['date','prn','code']):
    '''
    Return a DataFrame with "flex" events given the DataFrame df_in
    Output a .csv file with DataFrame information
    '''
    # Set up the dataframe that will be exported:
    cols = ['Station','Time','Event_type','PRN', 'Code']
    df_fe = _pd.DataFrame(columns = cols)        
    
    if el_min:
        df_c = df_in[df_in['el_ang']>el_min]
    else:
        df_c = df_in

    if GPS_flex:
        df_c = df_c[df_c.index.get_level_values('sv').str.startswith('G')]

    for code in codes:

        # Filter the df_in for the code input:
        dfc = df_c[[code]]

        for prn in dfc.index.get_level_values('sv').unique():
        
            # The values of the code being investigated
            prn_mask = dfc.index.get_level_values('sv') == prn
            dfv = dfc[prn_mask].droplevel('sv')
            vals = dfv.to_numpy(dtype=float)
            
            # Look for flex events by comparing current value to four increments ago
            for i,v in enumerate(vals[4:]):
                
                if (v > vals[i]+jump) & (v > start_floor):
                        new_row = {
                            'Station':station,
                            'Time':dfv.index[i+4],
                            'Event_type':'Start',
                            'PRN':prn,
                            'Code':code
                        }
                        df_fe = df_fe.append(new_row,ignore_index=True)
                
                elif (v < vals[i]-jump) & (v > end_floor):
                        new_row = {
                            'Station':station,
                            'Time':dfv.index[i+4],
                            'Event_type':'End',
                            'PRN':prn,
                            'Code':code
                        }
                        df_fe = df_fe.append(new_row,ignore_index=True)
    
    # Remove duplicates and consecutive time periods finds
    df_fe = df_fe.drop_duplicates()
    df_out = _pd.DataFrame(columns=['Station','Time','Event_type','PRN','Code'])
    
    for i,t in enumerate(df_fe['Time'][1:]):
        if t == df_fe['Time'][i]+_pd.Timedelta(seconds=30):
            continue
        else:
            df_out = df_out.append(df_fe.iloc[i])
    
    if csv_out:
        if not csv_dest:
            csv_dest = _Path().cwd()
        else:
            csv_dest = _Path(csv_dest)
            if not csv_dest.is_dir():
                csv_dest.mkdir(parents=True)

        if not csv_name:
            f1 = df_in.index.get_level_values('time')[0].date().strftime('%Y%j')
            f2 = df_in.index.get_level_values('time')[-1].date().strftime('%Y%j')
            csv_f = csv_dest/f'Flex_{station}_{f1}-{f2}.csv'    
            df_out.to_csv(str(csv_f),index=False)
        else:
            csv_f = csv_dest/csv_name
            df_out.to_csv(str(csv_f))


    if plot:
        if not plot_dest:
            plot_dest = _Path().cwd()
        else:
            plot_dest = _Path(plot_dest)
            if not plot_dest.is_dir():
                plot_dest.mkdir(parents=True)

        for code in codes:

            # Filter the df_in for the code input:
            dfc = df_c[[code]]
            dfp = dfc.reset_index().pivot(index='time',columns='sv',values=code)
            
            for i in df_out.index:
                fig1,ax1 = _plt.subplots()
                ax1.tick_params(axis='x', labelsize=14)
                ax1.tick_params(axis='y', labelsize=14)
                
                row = df_out.loc[i]
                date_str = row['Time'].date().strftime('%Y-%m-%d')
                datetime_str = row['Time'].strftime('%Y%m%d-%H%M')
                cond1 = dfp.index > row['Time']-_pd.Timedelta(seconds=plot_spread)
                cond2 = dfp.index < row['Time']+_pd.Timedelta(seconds=plot_spread)
                dfp[cond1 & cond2][row['PRN']].plot(figsize=(12,8),ax=ax1)
                
                ax1.set_xlabel('Time',fontsize=16)
                ax1.set_ylabel(f'$C/N_0$ {row["Code"]} (dB-Hz)',fontsize=16)
                ax1.set_title(f'Flex Event - {row["Event_type"]} - {row["Code"]} - {row["PRN"]} - {date_str}',fontsize=18)

                plt_names = []
                for name in file_nameorder:
                    if name == 'date':
                        plt_names.append(datetime_str)
                    elif name == 'prn':
                        plt_names.append(row["PRN"])
                    elif name == 'code':
                        plt_names.append(row["Code"])
    
                out_f = plot_dest/f'Flex_{"_".join(plt_names)}.png'
                fig1.savefig(str(out_f),facecolor='w',bbox_inches="tight")
                _plt.close(fig=fig1)

    return df_out


def add_all_angs(
    df,
    station_pos,
    nad=True,
    el=True,
    az=True,
    dist=True,
    return_lists=False):
    '''
    Add new columns to dataframe (dist, nad_ang, el_ang and/or az_ang) after 
    calculating nadir, elevation and/or azimuth angles and distance between satellite
    and given site 'station_pos'
    Default is to add all options these options but can be set to False (nad, el, az, dist)
    The raw data can also be returned as a tuple of lists (nad_angs, el_angs, az_angs, distances)
    '''
    
    sat_pos_arr = df[['X','Y','Z']].to_numpy(dtype=float)
    disp_vec_arr = sat_pos_arr - (station_pos.reshape(1,3)/1000)
    
    nad_angs = []
    el_angs = []
    az_angs = []
    distances = []
    
    sat_rec_dist =_np.linalg.norm(disp_vec_arr,axis=1)
    sat_norm =_np.linalg.norm(sat_pos_arr,axis=1)

    if nad:
        df['nad_ang'] =_np.arccos(_np.einsum("ij,ij->j", sat_pos_arr.T, disp_vec_arr.T)/(sat_norm*sat_rec_dist))*(180/_np.pi)    
    
    if el or az:
        station_llh = xyz2llh_heik(station_pos.T)[0]
        R = llh2rot(_np.array([station_llh[0]]),_np.array([station_llh[1]]))

        enu_pos =_np.matmul(R[0],disp_vec_arr.T).T
        enu_bar = enu_pos /_np.linalg.norm(enu_pos,axis=1).reshape(len(enu_pos),1)
        
        if el:
            df['el_ang'] =_np.arcsin(enu_bar[:,2])*(180/_np.pi)
        if az:
            df['az_ang'] =_np.arctan2(enu_bar[:,0],enu_bar[:,1])
    
    if dist:
        df['dist'] = sat_rec_dist

    if return_lists:
        return nad_angs, el_angs, az_angs, distances



if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(
        description = 'Given a station and a time period, find possible flex power events'
        )
    
    # Command line function arguments
    parser.add_argument(
        "station", 
        help = "IGS GPS station name - must be new RINEX3 format - 9 characters"
        )        
    
    parser.add_argument(
        "st_date", 
        help = "Start Date in YYYY-MM-DD format"
        )
    
    parser.add_argument(
        "en_date", 
        help = "End Date in YYYY-MM-DD format"
        )

    parser.add_argument(
        "-doy",
        "--day_of_year", 
        action='store_true', default=False,
        help = "Enter Dates in YYYY-DOY format instead"
        )

    parser.add_argument(
        "obs_codes",
        help = 'Comma separated RINEX3 Observation code/s to search, e.g. S1W,S2W '
        )

    parser.add_argument(
        "data_dir",
        help = 'The download directory for RINEX3 and sp3 files'
        )

    parser.add_argument(
        "-c",
        "--csv",
        action='store_true', default=True,
        help = 'Produce plots of Flex events'
    )

    parser.add_argument(
        "-c_dir",
        "--flex_csv_dir",
        action='store', default='pwd',
        help = 'The directory to save csv files with list of flex events'
        )

    parser.add_argument(
        "-p",
        "--plot",
        action='store_true', default=False,
        help = 'Produce plots of Flex events'
    )

    parser.add_argument(
        "-p_dir",
        "--flex_plt_dir",
        action='store', default='pwd',
        help = 'The directory to save png plot files of flex events'
        )

    parser.add_argument(
        "-p_span",
        "--plot_time_span", type=float,
        action='store', default=1500.0,
        help = 'The time span for the plot (+/- seconds from time of the flex event)'
        )

    parser.add_argument(
        "-p_name_ord",
        "--plot_naming_order",
        action='store', default='date,prn,code',
        help = '''Plot naming convention - comma separated and must include "date", "prn" and "code"
        Default: date,prn,code '''
        )

    parser.add_argument(
        "-j",
        "--jump", type=float,
        action='store', default=5.0,
        help = 'Increase/decrease of C/N0 used to identify Start/End of event'
        )

    parser.add_argument(
        "-el_min",
        "--elevation_min", type=float,
        action='store', default=0.0,
        help = 'Min. elevation angle of satellite to consider (anything below ignored)'
        )

    parser.add_argument(
        "-st_fl",
        "--start_floor", type=float,
        action='store', default=33.0,
        help = 'Min. Decibel-Hertz level at which to search for start of flex events (anything below ignored)'
        )

    parser.add_argument(
        "-en_fl",
        "--end_floor", type=float,
        action='store', default=30.0,
        help = 'Min. Decibel-Hertz level at which to search for end of flex events (anything below ignored)'
        )
    


    # Get command line args:
    args = parser.parse_args()
    # And start assigning to variables:
    station = args.station
    st_date = args.st_date
    en_date = args.en_date
    doy = args.day_of_year
    codes = args.obs_codes
    st_lvl = args.start_floor
    en_lvl = args.end_floor
    el_min = args.elevation_min
    jump = args.jump
    dwn_dir = args.data_dir
    csv_flag = args.csv
    plot_flag = args.plot
    p_span = args.plot_time_span
    p_name_list = args.plot_naming_order.split(',')
    
    if args.flex_csv_dir == 'pwd':
        c_dir = False
    else:
        c_dir = args.flex_csv_dir

    if args.flex_plt_dir == 'pwd':
        p_dir = False
    else:
        p_dir = args.flex_plt_dir

    if doy:
        st_date = _pd.to_datetime(st_date).strftime('%Y-%m-%d')
        en_date = _pd.to_datetime(en_date).strftime('%Y-%m-%d')

    # Download and load RINEX and SP3 files into DataFrame:
    df = get_load_rnxsp3(start_date=st_date,end_date=en_date,station=station,directory=dwn_dir,sp3pref='igr')
    # Find flex events, plot if chosen:
    df_out = find_flex_events(
        df_in=df, 
        codes=codes.split(','), 
        station=station,
        start_floor=st_lvl, 
        end_floor=en_lvl, 
        jump=jump, 
        el_min=el_min,
        GPS_flex=True,
        csv_out=csv_flag,
        csv_dest=c_dir,
        csv_name=False,
        plot=plot_flag, 
        plot_dest=p_dir, 
        plot_spread=p_span,
        file_nameorder=p_name_list)
            




'''
For the new find_flex_events 
- X get sp3 and rinex 3 files 
- X load using new read_sp3 and read_rnx functions 
- X combine using code I've used in jupyter notebook
- X add all angles and distances
- X filter based on elevation
- search for flex events 
'''