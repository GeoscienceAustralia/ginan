'''
Create a new YAML file for the PEA for a PPP use case

Input:
station - IGS station in RINEX3 style format                                        - str
year - year in YYYY format                                                          - str
date - date in DDD day-of-year format (or MM-DD format when -md option selected)    - str
input_dir - directory to download and read input files from                         - str
output_dir - directory to output pea processed files                                - str


Output:
YAML config file used as input in the pea which defines the parameters for the pea run

Ronald Maj
2020-11-17 10:01
'''

import ruamel.yaml
import numpy as np
import subprocess
import argparse
from pathlib import Path
from datetime import datetime
from get_pea_files import get_pea_files, gpsweekD



def find_yaml_temp(ex_name):
    '''
    Find the location of the example 'ex_name' yaml file

    Output is Path object
    '''
    pwd1 = Path.cwd()
    up_dir_tree = [pwd1] + list(pwd1.parents)
    
    for path in up_dir_tree:
        search_path = list(path.rglob(f'*{ex_name}.yaml'))

        if not search_path == []:
            for path in search_path:
                if '/.' in str(path):
                    continue
                else:
                    yaml_path = path
            break

    return yaml_path



def create_yaml_file(stations,
                    start_datetime,
                    end_datetime,
                    input_dir, 
                    output_dir, 
                    file_pref = 'EXTEMP-IF-PPP',
                    file_suff = '', 
                    ex_name = 'template',
                    trop_vmf3 = False, 
                    proc_n = None, 
                    rapid = False,
                    acs_prefs = ['igs','jpl','gfz','cod'],
                    man_file_input = None):
    '''
    Given the inputs, create a YAML file to run pea in the mode
    determined by 'ex_name', i.e. the yaml file used as a template.

    Dates should be datetime format covering the period of interest
    '''
    
    # Load yaml file as template:
    f_path = str(find_yaml_temp(ex_name))
    with open(f_path,'r') as f:
        template = ruamel.yaml.round_trip_load(f, preserve_quotes=True)

    # Assign directories in the yaml template:
    template['input_files']['root_input_directory'] = input_dir + '/products/'
    template['station_data']['root_stations_directory'] = input_dir + '/data/'
    template['output_files']['root_output_directory'] = output_dir + '/<CONFIG>/'
    
    # Assign start / end epochs
    template['processing_options']['start_epoch'] = start_datetime
    template['processing_options']['end_epoch'] = end_datetime

    # Output options:
    template['output_files']['output_config'] = True
    template['output_files']['trace_filename'] = f'{file_pref}_{file_suff}.TRACE'

    if proc_n: # change process noise value and rate
        proc_val, proc_rate = proc_n
        proc_val = float(proc_val)
        template['default_filter_parameters']['stations']['pos']['proc_noise'][0] = proc_val
        template['default_filter_parameters']['stations']['pos']['proc_noise_dt'][0] = proc_rate


    if man_file_input: # for manually chosen files
        template['station_data']['rnxfiles'] = man_file_input['rnxfiles']
        
        for prods in ['sp3files','clkfiles','snxfiles','erpfiles','navfiles','atxfiles','blqfiles']:
            template['input_files'][prods] = man_file_input[prods]
            template['input_files'][prods] = sorted(list(set(template['input_files'][prods])))

    else: # otherwise glob option
        if stations == ['ALL']:
            template['station_data']['rnxfiles'].append(ruamel.yaml.scalarstring.DoubleQuotedScalarString("*.rnx"))
        else:
            for station in stations:
                template['station_data']['rnxfiles'].append(ruamel.yaml.scalarstring.DoubleQuotedScalarString(f"{station}*.rnx"))
        template['station_data']['rnxfiles'].pop(0)

        if rapid: # change file prefix 
            for prods in ['sp3files','clkfiles']:
                prod = prods[:3]
                template['input_files'][prods][0] = f"igr*.{prod}"


    # Write template to file:
    with open(f'{file_pref}{file_suff}.yaml', 'w') as out_f:
        ruamel.yaml.round_trip_dump(data = template, stream = out_f)

    return f'{file_pref}{file_suff}.yaml'



if __name__ == "__main__":

    # Introduce command line parser
    parser = argparse.ArgumentParser(
        description = 'Create a YAML config file to run PEA'
        )
    
    # Command line function arguments
    parser.add_argument("stations", 
        help = """GNSS station names - 4 or 9 character format acceptable
        If all available stations to be processed, input 'ALL' otherwise
        comma-separated list: 'ALIC,HOB2,MDO1' """
        )
        
    parser.add_argument("start_datetime", 
        help = "Start epoch - 'YYYY-MM-DD_hh:mm:ss' format"
    )
    
    parser.add_argument("end_datetime", 
        help = "End epoch - 'YYYY-MM-DD_hh:mm:ss' format"
    )

    parser.add_argument("input_dir", 
        help = "Input / Download directory"
    )

    parser.add_argument("output_dir", 
        help = "Output directory"
    )

    parser.add_argument("-file_pref", "--file_pref", 
        help = "Filename Prefix"
    )

    parser.add_argument("-file_suff", "--file_suff", 
        help = "Filename Suffix"
    )

    parser.add_argument("-ex_name", "--ex_name", 
        help = "Option to use specific example as template for yaml file"
    )

    parser.add_argument("-trop_vmf3", "--trop_vmf3", action="store_true", 
        help = "Option to use VMF3 model for the troposphere"
    )

    parser.add_argument("-proc_n", "--process_noise", action = 'store', nargs = 2,
        help = """Option to edit the process noise of the POS Kalman filter. Must give two arguments.
                Process Noise value in (m) followed by rate (/hour, /second, etc)
                """
    )

    parser.add_argument("-rapid", "--rapid", action="store_true", 
        help = "Option to use rapid versions of product files instead (.clk, .sp3)"
    )

    # Get command line args:
    args = parser.parse_args()
    
    # Assign variables from command line args:
    stations = args.stations.split(',')
    start_datetime_str = args.start_datetime
    end_datetime_str = args.end_datetime
    input_dir = args.input_dir
    output_dir = args.output_dir
    file_pref = args.file_pref
    file_suff = args.file_suff
    ex_name = args.ex_name
    trop_vmf3 = args.trop_vmf3
    proc_n = args.process_noise
    rapid = args.rapid

    if not file_pref:
        file_pref = 'EXTEMP-IF-PPP'
    if not file_suff:
        file_suff = ''
    if not ex_name:
        ex_name = 'template'
    if not proc_n:
        proc_n = None
    if not rapid:
        rapid = False

    # Convert to Datetime obj:
    st_dt = datetime.strptime(start_datetime_str,'%Y-%m-%d_%H:%M:%S')
    en_dt = datetime.strptime(end_datetime_str,'%Y-%m-%d_%H:%M:%S')

    # Create the yaml file:
    create_yaml_file(stations, st_dt, en_dt, input_dir, output_dir, file_pref, file_suff, proc_n=proc_n, ex_name=ex_name, rapid=rapid)


