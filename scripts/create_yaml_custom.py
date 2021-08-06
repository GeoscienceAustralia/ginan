'''
Create a new YAML file for the PEA based on an input "template" file
'''

import ruamel.yaml
import numpy as np
import subprocess
import argparse
from pathlib import Path
from datetime import datetime


def create_yaml_file(input_prod_dir, 
                    input_data_dir,
                    output_dir, 
                    in_yml_temp_path = '',
                    out_yml_path = 'ex-custom.yaml',
                    file_input_dict = None,
                    epoch_interval = 30,
                    start_datetime = None,
                    end_datetime = None,
                    stations=['ALL'],
                    output_trace=True,
                    trace_file_name = 'EXAMPLE',
                    trace_level = 3,
                    trop_vmf3 = False, 
                    pos_proc_n = None):
    '''
    Given the inputs, create a YAML file to run pea in the mode
    determined by 'ex_name', i.e. the yaml file used as a template.

    Dates should be datetime format covering the period of interest
    '''
    
    # Load yaml file as template:
    with open(in_yml_temp_path,'r') as f:
        template = ruamel.yaml.round_trip_load(f, preserve_quotes=True)

    # Assign directories in the yaml template:
    template['input_files']['root_input_directory'] = input_prod_dir
    template['station_data']['root_stations_directory'] = input_data_dir
    template['output_files']['root_output_directory'] = output_dir
    
    # Assign start / end epochs
    template['processing_options']['epoch_interval'] = epoch_interval
    if start_datetime:
        template['processing_options']['start_epoch'] = start_datetime
    if end_datetime:
        template['processing_options']['end_epoch'] = end_datetime

    # Output options:
    template['output_files']['output_trace'] = output_trace
    template['output_files']['trace_level'] = trace_level
    template['output_files']['trace_filename'] = f'{trace_file_name}.TRACE'

    if pos_proc_n: # change process noise value and rate
        proc_val, proc_rate = pos_proc_n
        proc_val = float(proc_val)
        template['default_filter_parameters']['stations']['pos']['proc_noise'][0] = proc_val
        template['default_filter_parameters']['stations']['pos']['proc_noise_dt'] = proc_rate

    if file_input_dict: # for manually chosen files
        
        if 'rnxfiles' in file_input_dict:
            template['station_data']['rnxfiles'] = file_input_dict['rnxfiles']
        elif stations == ['ALL']:
            template['station_data']['rnxfiles'].append(ruamel.yaml.scalarstring.DoubleQuotedScalarString("*.rnx"))
            template['station_data']['rnxfiles'].pop(0)
        else:
            for station in stations:
                template['station_data']['rnxfiles'].append(ruamel.yaml.scalarstring.DoubleQuotedScalarString(f"{station}*.rnx"))
            template['station_data']['rnxfiles'].pop(0)

        for prods in ['sp3files','clkfiles','snxfiles','erpfiles','navfiles','atxfiles','blqfiles']:
            template['input_files'][prods] = file_input_dict[prods]
            template['input_files'][prods] = sorted(list(set(template['input_files'][prods])))

    if trop_vmf3:
        template['processing_options']['troposphere']['model'] = 'vmf3'
        template['processing_options']['troposphere']['vmf3dir'] = trop_vmf3
        template['processing_options']['troposphere']['orography'] = 'orography_ell_5x5'
        template['processing_options']['troposphere'].pop('gpt2grid',None)
    else:
        template['processing_options']['troposphere']['model'] = 'gpt2'
        template['processing_options']['troposphere']['gpt2grid'] = 'gpt_25.grd'
        template['processing_options']['troposphere'].pop('vmf3dir',None)
        template['processing_options']['troposphere'].pop('orography',None)

    # else: # otherwise glob option
    #     if stations == ['ALL']:
    #         template['station_data']['rnxfiles'].append(ruamel.yaml.scalarstring.DoubleQuotedScalarString("*.rnx"))
    #     else:
    #         for station in stations:
    #             template['station_data']['rnxfiles'].append(ruamel.yaml.scalarstring.DoubleQuotedScalarString(f"{station}*.rnx"))
    #     template['station_data']['rnxfiles'].pop(0)

    # Write template to file:
    with open(f'{out_yml_path}', 'w') as out_f:
        ruamel.yaml.round_trip_dump(data = template, stream = out_f)

    return f'{out_yml_path}'



# if __name__ == "__main__":

#     # Introduce command line parser
#     parser = argparse.ArgumentParser(
#         description = 'Create a YAML config file to run PEA'
#         )
    
#     # Command line function arguments
#     parser.add_argument("stations", 
#         help = """GNSS station names - 4 or 9 character format acceptable
#         If all available stations to be processed, input 'ALL' otherwise
#         comma-separated list: 'ALIC,HOB2,MDO1' """
#         )
        
#     parser.add_argument("start_datetime", 
#         help = "Start epoch - 'YYYY-MM-DD_hh:mm:ss' format"
#     )
    
#     parser.add_argument("end_datetime", 
#         help = "End epoch - 'YYYY-MM-DD_hh:mm:ss' format"
#     )

#     parser.add_argument("input_dir", 
#         help = "Input / Download directory"
#     )

#     parser.add_argument("output_dir", 
#         help = "Output directory"
#     )

#     parser.add_argument("-file_pref", "--file_pref", 
#         help = "Filename Prefix"
#     )

#     parser.add_argument("-file_suff", "--file_suff", 
#         help = "Filename Suffix"
#     )

#     parser.add_argument("-ex_name", "--ex_name", 
#         help = "Option to use specific example as template for yaml file"
#     )

#     parser.add_argument("-trop_vmf3", "--trop_vmf3", action="store_true", 
#         help = "Option to use VMF3 model for the troposphere"
#     )

#     parser.add_argument("-proc_n", "--process_noise", action = 'store', nargs = 2,
#         help = """Option to edit the process noise of the POS Kalman filter. Must give two arguments.
#                 Process Noise value in (m) followed by rate (/hour, /second, etc)
#                 """
#     )

#     parser.add_argument("-rapid", "--rapid", action="store_true", 
#         help = "Option to use rapid versions of product files instead (.clk, .sp3)"
#     )

#     # Get command line args:
#     args = parser.parse_args()
    
#     # Assign variables from command line args:
#     stations = args.stations.split(',')
#     start_datetime_str = args.start_datetime
#     end_datetime_str = args.end_datetime
#     input_dir = args.input_dir
#     output_dir = args.output_dir
#     file_pref = args.file_pref
#     file_suff = args.file_suff
#     ex_name = args.ex_name
#     trop_vmf3 = args.trop_vmf3
#     proc_n = args.process_noise
#     rapid = args.rapid

#     if not file_pref:
#         file_pref = 'EXTEMP-IF-PPP'
#     if not file_suff:
#         file_suff = ''
#     if not ex_name:
#         ex_name = 'template'
#     if not proc_n:
#         proc_n = None
#     if not rapid:
#         rapid = False

#     # Convert to Datetime obj:
#     st_dt = datetime.strptime(start_datetime_str,'%Y-%m-%d_%H:%M:%S')
#     en_dt = datetime.strptime(end_datetime_str,'%Y-%m-%d_%H:%M:%S')

#     # Create the yaml file:
#     create_yaml_file(stations, st_dt, en_dt, input_dir, output_dir, file_pref, file_suff, proc_n=proc_n, ex_name=ex_name, rapid=rapid)


