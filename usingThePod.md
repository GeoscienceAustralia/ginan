
# Using the POD Module

POD uses configuration files in YAML to control its processing. After installing the software and its dependencies (see the README.md in the GitHub repository) and compiling building the POD application, POD processing can be started by typing the command.

    ./pod -y <path_to_config_file>

The POD module has two main modes of operation, the orbit fitting mode and the orbit integration/prediction mode. In orbit fitting mode, precise orbit parameters are calculated from, potentially inaccurate, satellite position pseudo-observations. In orbit integration mode, precise satellite positions are estimated/predicted from precise orbit parameters.

## Using the POD for orbit fitting

The orbit fitting mode can be selected by setting both `pod_mode_fit:` and `ic_input_format: sp3:` to `true`. 

In this mode, the POD will take satellite position pseudo-measurements from a SP3 formatted file and estimate the orbit state of each satellite contained in the SP3 file. The SP3 file containing a priory satellite position needs to be specified as the `pseudobs_orbit_filename` parameter.

The orbit state in POD is represented by a set of parameters consisting of 

* Satellite position (in ITRF or ICRF) at the first epoch in the SP3 file
* Satellite velocity (in ITRF or ICRF) at the first epoch in the SP3 file
* Up to 9 parameters describing the Solar Radiation Pressure over the fitting time

These initial conditions, and the models described in Ginan Science Manual will allow for the precise determination of satellite positions over the fitting arc (set by the `orbit_arc_determination` parameter).

The main outputs from this mode of operation are the a-posteriori satellite position in SP3 format, and the orbit partials of satellite positions with respect to the initial conditions. 
The output SP3 file which can be found on `output_directory/gagWWWWD.sp3` where `WWWW` is the GPS week and `D` is the GPS day of the first epoch on the SP3 files.
The orbit partials are written in Ginan's proprietary Initial Conditions File (ICF) format, and can be found in  `output_directory/gagWWWWD_orbit_partials.out`.
Configuration files, `ex21_pod_fit_gps.yaml` and `ex22_pod_fit_gnss.yaml`,  for this mode of operation are included in the Ginan `examples` folder.

## Using the POD for orbit integration/prediction

The orbit fitting mode can be selected by setting both `pod_mode_ic_int` and `ic_input_format_icf` to `true`. 
In this mode, the POD will take the initial conditions contained in the ICF formatted files and propagates the satellite positions forward over the time period specified by the sum of `orbit_arc_determination` and `orbit_arc_prediction` parameters. 
It also propagates the satellite position backwards by a number of hours specified by the `orbit_arc_backwards` parameter.
The ICF file containing the satellites initial condition and radiation pressure parameters needs to be specified as the `ic_input_format_ic_filename` parameter.

It is to note that the orbit fitting mode will also use the orbit integration operation after estimating the initial conditions from pseudo-observations. 
The  mode `pod_mode_fit` will only integrated for a number of hours specified  by `orbit_arc_determination`, but will also do the backwards arc specified by `orbit_arc_backwards` and a prediction arc specified by `orbit_arc_prediction`.
Selecting the `pod_mode_predict` will propagate the initial conditions a number of hours specified by the sum of `orbit_arc_determination` and `orbit_arc_prediction`.
The integrated/predicted satellite position will be output to a SP3 formatted file located in `output_directory/gagWWWWD.sp3`.

The example configuration file to perform orbit integration/prediction from SP3 files is `ex23_pod_prd_gps.yaml`.  
The example configuration file to perform orbit integration/prediction from ICF files is `ex24_pod_ic_gps.yaml`. Both are located in the Ginan `examples` folder.