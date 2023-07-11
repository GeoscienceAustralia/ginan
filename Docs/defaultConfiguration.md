
# Default Configuration

This document outlines the major configuration options available in ginan that are most applicable to general users. For more advanced configuration options and their defaults, use the `-Y <level>` option at the command line to view increasing levels of advanced configurations.
## satellite_options:

###### **`satellite_options:`**
 ` `


> Options to configure individual satellites, systems, or global configs

---

###### **`satellite_options:global:`**
 ` `


---

###### **`satellite_options:global:clock:`**
 ` `


---

###### **`satellite_options:global:clock:sources:`**
 [`[E_Source]`](#e_source) `[KALMAN, PRECISE, BROADCAST] `


List of sources to use for clocks

---

###### **`satellite_options:global:pos:`**
 ` `


---

###### **`satellite_options:global:exclude:`**
 `false `


(bool) Exclude satellite from processing

---

## station_options:

###### **`station_options:`**
 ` `


> Options to configure individual stations or global configs

---

###### **`station_options:global:`**
 ` `


---

## inputs:

###### **`inputs:`**
 ` `


> This section of the yaml file specifies the lists of files to be used for general metadata inputs, and inputs of external product data.


---

###### **`inputs:atx_files:`**
 `[] `


[string] List of atx files to use

---

###### **`inputs:blq_files:`**
 `[] `


[string] List of blq files to use

---

###### **`inputs:egm_files:`**
 `[] `


[string] List of egm files to use

---

###### **`inputs:erp_files:`**
 `[] `


[string] List of erp files to use

---

###### **`inputs:igrf_files:`**
 `[] `


[string] List of igrf files to use

---

###### **`inputs:include_yamls:`**
 `[] `


[string] List of yaml files to include before this one

---

###### **`inputs:ion_files:`**
 `[] `


[string] List of ion files to use

---

###### **`inputs:jpl_files:`**
 `[] `


[string] List of jpl files to use

---

###### **`inputs:pseudo_observations:`**
 ` `


> Use data from pre-processed data products as observations. Useful for combining and comparing datasets

---

###### **`inputs:pseudo_observations:sp3_inputs:`**
 ` `


---

###### **`inputs:pseudo_observations:inputs_root:`**
 `"./" `


(string) Root path to be added to all other pseudo obs data files (unless they are absolute)

---

###### **`inputs:snx_files:`**
 `[] `


[string] List of snx files to use

---

###### **`inputs:tide_files:`**
 `[] `


---

###### **`inputs:ionosphere:`**
 ` `


> Files specifying ionospheric model inputs

---

###### **`inputs:ionosphere:ion_files:`**
 `[] `


[string] List of IONEX files for VTEC input

---

###### **`inputs:root_directory:`**
 `"./" `


(string) Root path to be added to all other input files (unless they are absolute)

---

###### **`inputs:root_stream_url:`**
 `"" `


(string) Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.

---

###### **`inputs:satellite_data:`**
 ` `


---

###### **`inputs:satellite_data:bsx_files:`**
 `[] `


[string] List of biassinex  files to use

---

###### **`inputs:satellite_data:clk_files:`**
 `[] `


[string] List of clock      files to use

---

###### **`inputs:satellite_data:dcb_files:`**
 `[] `


[string] List of dcb        files to use

---

###### **`inputs:satellite_data:nav_files:`**
 `[] `


[string] List of ephemeris  files to use

---

###### **`inputs:satellite_data:obx_files:`**
 `[] `


[string] List of orbex      files to use

---

###### **`inputs:satellite_data:rtcm_inputs:`**
 `[] `


[string] List of rtcm       inputs to use for corrections

---

###### **`inputs:satellite_data:sp3_files:`**
 `[] `


[string] List of sp3        files to use

---

###### **`inputs:satellite_data:inputs_root:`**
 `"./" `


(string) Root path to be added to all other satellite data files (unless they are absolute)

---

###### **`inputs:troposphere:`**
 ` `


> Files specifying tropospheric model inputs

---

###### **`inputs:troposphere:gpt2grid_files:`**
 `"" `


---

###### **`inputs:troposphere:orography_files:`**
 `"" `


---

###### **`inputs:troposphere:vmf_files:`**
 `[] `


[string] List of vmf files to use

---

###### **`inputs:gnss_observations:`**
 ` `


> Signal observation data from gnss receivers to be used as measurements

---

###### **`inputs:gnss_observations:rnx_inputs:`**
 ` `


---

###### **`inputs:gnss_observations:rtcm_inputs:`**
 ` `


---

###### **`inputs:gnss_observations:inputs_root:`**
 `"./" `


(string) Root path to be added to all other gnss data inputs (unless they are absolute)

---

## outputs:

###### **`outputs:`**
 ` `


> Specifies options to enable outputs and specify file locations.

Each section typically contains an option to `output` the filetype, and a `directory` to place the files named `filename`, along with any ancillary options.


---

###### **`outputs:bias_sinex:`**
 ` `


> Rinex formatted bias sinex files

---

###### **`outputs:bias_sinex:output:`**
 `false `


(bool) Output bias sinex files

---

###### **`outputs:clocks:`**
 ` `


> Rinex formatted clock files

---

###### **`outputs:clocks:output:`**
 `false `


(bool) Output clock files

---

###### **`outputs:cost:`**
 ` `


> COST format files are used to export troposhere products, such as ZTD and delay gradients.

---

###### **`outputs:cost:output:`**
 `false `


(bool) Enable data exporting to troposphere COST file

---

###### **`outputs:erp:`**
 ` `


> Earth rotation parameters can be output to file

---

###### **`outputs:erp:output:`**
 `false `


(bool) Enable exporting of erp data

---

###### **`outputs:gpx:`**
 ` `


> GPX files contain point data that may be easily viewed in GIS mapping software

---

###### **`outputs:gpx:output:`**
 `false `


(bool) 

---

###### **`outputs:ionex:`**
 ` `


> IONEX formatted ionospheric mapping and modelling outputs

---

###### **`outputs:ionex:output:`**
 `false `


(bool) Enable exporting ionospheric model data

---

###### **`outputs:ionstec:`**
 ` `


---

###### **`outputs:ionstec:output:`**
 `false `


(bool) 

---

###### **`outputs:log:`**
 ` `


> Log files store console output in files

---

###### **`outputs:log:output:`**
 `false `


(bool) Enable console output logging

---

###### **`outputs:metadata:`**
 ` `


> Options for setting metadata for inputs and outputs

---

###### **`outputs:metadata:config_description:`**
 `"Pea" `


(string) ID for this config, used to replace <CONFIG> tags in other options

---

###### **`outputs:metadata:pass:`**
 `"" `


(string) Password for connecting to NTRIP casters

---

###### **`outputs:metadata:user:`**
 `"" `


(string) Username for connecting to NTRIP casters

---

###### **`outputs:ppp_sol:`**
 ` `


---

###### **`outputs:ppp_sol:output:`**
 `false `


(bool) 

---

###### **`outputs:sinex:`**
 ` `


---

###### **`outputs:sinex:output:`**
 `false `


(bool) 

---

###### **`outputs:slr_obs:`**
 ` `


> SLR_OBS files are used as temporary files to arrange SLR observations by time. SLR observations are taken from CRD files, which are not strictly in time-order).

---

###### **`outputs:slr_obs:output:`**
 `false `


(bool) Enable data exporting to tabular SLR obs file

---

###### **`outputs:sp3:`**
 ` `


> SP3 files contain orbital and clock data of satellites and receivers

---

###### **`outputs:sp3:output:`**
 `false `


(bool) Enable SP3 file outputs

---

###### **`outputs:streams:`**
 ` `


---

###### **`outputs:streams:root_url:`**
 `"" `


(string) Root url to be prepended to all other streams specified in this section. If the streams used have individually specified root urls, usernames, or passwords, this should not be used.

---

###### **`outputs:trop_sinex:`**
 ` `


> Troposphere SINEX files are used to export troposhere products, such as ZTD and delay gradients.

---

###### **`outputs:trop_sinex:output:`**
 `false `


(bool) Enable data exporting to troposphere SINEX file

---

###### **`outputs:root_directory:`**
 `"./" `


(string) Directory that outputs will be placed in

---

###### **`outputs:trace:`**
 ` `


> Trace files are used to document processing

---

###### **`outputs:trace:directory:`**
 `"./" `


(string) Directory to output trace files to

---

###### **`outputs:trace:level:`**
 `0 `


(int) Threshold level for printing messages (0-6). Increasing this increases the amount of data stored in all trace files

---

###### **`outputs:trace:output_config:`**
 `false `


(bool) Output configuration files to top of trace files

---

###### **`outputs:trace:output_residual_chain:`**
 `true `


(bool) Output component-wise details for measurement residuals

---

###### **`outputs:trace:output_residuals:`**
 `false `


(bool) Output measurements and residuals

---

###### **`outputs:trace:output_network:`**
 `false `


(bool) Output trace files for complete network of stations, inclucing kalman filter results and statistics

---

###### **`outputs:trace:output_satellites:`**
 `false `


(bool) Output trace files for individual satellites processing

---

###### **`outputs:trace:output_stations:`**
 `false `


(bool) Output trace files for individual stations processing

---

###### **`outputs:trace:network_filename:`**
 `"<STATION>-<LOGTIME>.trace" `


(string) Template filename for network trace files

---

###### **`outputs:trace:satellite_filename:`**
 `"<STATION>-<LOGTIME>.trace" `


(string) Template filename for satellite trace files

---

###### **`outputs:trace:station_filename:`**
 `"<STATION>-<LOGTIME>.trace" `


(string) Template filename for station trace files

---

## processing_options:

###### **`processing_options:`**
 ` `


> Various sections and parameters to specify how the observations are processed

---

###### **`processing_options:filter_options:`**
 ` `


> Configurations for the kalman filter and its sub processes

---

###### **`processing_options:filter_options:outlier_screening:`**
 ` `


> Statistical checks allow for detection of outliers that exceed their confidence intervals.

---

###### **`processing_options:filter_options:outlier_screening:max_filter_iterations:`**
 `2 `


---

###### **`processing_options:filter_options:rts:`**
 ` `


> RTS allows reverse smoothing of estimates such that early estimates can make use of later data.

---

###### **`processing_options:filter_options:rts:enable:`**
 `false `


(bool) Perform backward smoothing of states to improve precision of earlier states

---

###### **`processing_options:minimum_constraints:`**
 ` `


> Station coodinates may be aligned to reference frames with minimal external constraints

---

###### **`processing_options:minimum_constraints:enable:`**
 `false `


(bool) Transform states by minimal constraints to selected station coordinates

---

###### **`processing_options:minimum_constraints:rotation:`**
 ` `


> Estimation and application of angular offsets

---

###### **`processing_options:minimum_constraints:rotation:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:rotation:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:rotation:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`processing_options:minimum_constraints:rotation:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`processing_options:minimum_constraints:scale:`**
 ` `


> Estimation and application of scaling factor

---

###### **`processing_options:minimum_constraints:scale:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:scale:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:scale:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`processing_options:minimum_constraints:scale:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`processing_options:minimum_constraints:translation:`**
 ` `


> Estimation and application of CoG offsets

---

###### **`processing_options:minimum_constraints:translation:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:translation:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:translation:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`processing_options:minimum_constraints:translation:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`processing_options:ssr_corrections:`**
 ` `


> This section specifies how State State Representation (SSR) corrections are calculated before being published to an NTRIP caster.

---

###### **`processing_options:ssr_corrections:clock_sources:`**
 [`[E_Source]`](#e_source) `[KALMAN] `


Sources for SSR clocks

---

###### **`processing_options:ssr_corrections:code_bias_sources:`**
 [`[E_Source]`](#e_source) `[PRECISE] `


Sources for SSR code biases

---

###### **`processing_options:ssr_corrections:ephemeris_sources:`**
 [`[E_Source]`](#e_source) `[PRECISE] `


Sources for SSR ephemeris

---

###### **`processing_options:ssr_corrections:ionosphere_sources:`**
 [`[E_Source]`](#e_source) `[NONE] `


Sources for SSR ionosphere

---

###### **`processing_options:ssr_corrections:phase_bias_sources:`**
 [`[E_Source]`](#e_source) `[NONE] `


Sources for SSR phase biases

---

###### **`processing_options:ssr_inputs:`**
 ` `


> This section specifies how State State Representation (SSR) corrections are applied after they are downloaded from an NTRIP caster.

---

###### **`processing_options:ssr_inputs:code_bias_validity_time:`**
 `3600 `


(double) Valid time period of SSR code biases

---

###### **`processing_options:ssr_inputs:global_vtec_valid_time:`**
 `300 `


(double) Valid time period of global VTEC maps

---

###### **`processing_options:ssr_inputs:local_stec_valid_time:`**
 `120 `


(double) Valid time period of local STEC corrections

---

###### **`processing_options:ssr_inputs:one_freq_phase_bias:`**
 `false `


(bool)   Used stream have one SSR phase bias per frequency

---

###### **`processing_options:ssr_inputs:phase_bias_validity_time:`**
 `30 `


(double) Valid time period of SSR phase biases

---

###### **`processing_options:ssr_inputs:ssr_antenna_offset:`**
 [`E_OffsetType`](#e_offsettype) `UNSPECIFIED `


Ephemeris type that is provided in the listed SSR stream, i.e. satellite antenna-phase-centre (APC) or centre-of-mass (COM). This information is listed in the NTRIP Caster's sourcetable {unspecified,apc,com}

---

###### **`processing_options:epoch_control:`**
 ` `


> Specifies the rate and duration of data processing

---

###### **`processing_options:epoch_control:epoch_interval:`**
 `1 `


(float) Desired time step between each processing epoch

---

###### **`processing_options:epoch_control:max_epochs:`**
 `0 `


(int)   Maximum number of epochs to process

---

###### **`processing_options:epoch_control:start_epoch:`**
 `"" `


(date) The time of the first epoch to process (all observations before this will be skipped)

---

###### **`processing_options:epoch_control:end_epoch:`**
 `"" `


(date) The time of the last epoch to process (all observations after this will be skipped)

---

###### **`processing_options:gnss_general:`**
 ` `


> Options to specify the processing of gnss observations

---

###### **`processing_options:gnss_general:elevation_mask:`**
 `0.174533 `


(float) Minimum elevation for satellites to be processed. Config in degrees, however default is displayed in radians

---

###### **`processing_options:gnss_general:sys_options:`**
 ` `


---

###### **`processing_options:gnss_general:sys_options:bds:`**
 ` `


> Options for the BDS constellation

---

###### **`processing_options:gnss_general:sys_options:bds:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:bds:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:bds:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_general:sys_options:gal:`**
 ` `


> Options for the GAL constellation

---

###### **`processing_options:gnss_general:sys_options:gal:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:gal:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:gal:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_general:sys_options:glo:`**
 ` `


> Options for the GLO constellation

---

###### **`processing_options:gnss_general:sys_options:glo:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:glo:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:glo:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_general:sys_options:gps:`**
 ` `


> Options for the GPS constellation

---

###### **`processing_options:gnss_general:sys_options:gps:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:gps:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:gps:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_general:sys_options:leo:`**
 ` `


> Options for the LEO constellation

---

###### **`processing_options:gnss_general:sys_options:leo:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:leo:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:leo:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_general:sys_options:qzs:`**
 ` `


> Options for the QZS constellation

---

###### **`processing_options:gnss_general:sys_options:qzs:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:qzs:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:qzs:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_general:sys_options:sbs:`**
 ` `


> Options for the SBS constellation

---

###### **`processing_options:gnss_general:sys_options:sbs:ambiguity_resolution:`**
 `false `


(bool) Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:sbs:code_priorities:`**
 `[] `


List of observation codes to use in processing

---

###### **`processing_options:gnss_general:sys_options:sbs:process:`**
 `false `


(bool) Process this constellation

---

###### **`processing_options:gnss_models:`**
 ` `


---

###### **`processing_options:gnss_models:ionospheric_component:`**
 ` `


> Ionospheric models produce frequency-dependent effects

---

###### **`processing_options:gnss_models:ionospheric_component:common_ionosphere:`**
 `true `


(bool) Use the same ionosphere state for code and phase observations

---

###### **`processing_options:gnss_models:ionospheric_component:use_gf_combo:`**
 `false `


(bool) Combine 'uncombined' measurements to simulate a geometry-free solution

---

###### **`processing_options:gnss_models:ionospheric_component:use_if_combo:`**
 `false `


(bool) Combine 'uncombined' measurements to simulate an ionosphere-free solution

---

###### **`processing_options:model_error_checking:`**
 ` `


> The kalman filter is capable of automatic statistical integrity modelling

---

###### **`processing_options:model_error_checking:ambiguities:`**
 ` `


> Cycle slips in ambiguities are primary cause of incorrect gnss modelling and may be reinitialised

---

###### **`processing_options:model_error_checking:ambiguities:outage_reset_limit:`**
 `10 `


(int) Maximum number of epochs with missed phase measurements before the ambiguity associated with the measurement is reset.

---

###### **`processing_options:model_error_checking:ambiguities:phase_reject_limit:`**
 `10 `


(int) Maximum number of phase measurements to reject before the ambiguity associated with the measurement is reset.

---

###### **`processing_options:model_error_checking:ambiguities:reinit_on_all_slips:`**
 `false `


(bool) Any detected slips cause removal and reinitialisation of ambiguities

---

###### **`processing_options:model_error_checking:deweighting:`**
 ` `


> Measurements that are outside the expected confidence bounds may be deweighted so that outliers do not contaminate the filtered solution

---

###### **`processing_options:model_error_checking:deweighting:deweight_factor:`**
 `100 `


(float) Factor to downweight the variance of measurements with statistically detected errors

---

## estimation_parameters:

###### **`estimation_parameters:`**
 ` `


---

###### **`estimation_parameters:satellites:`**
 ` `


---

###### **`estimation_parameters:satellites:clk:`**
 ` `


> Clocks

---

###### **`estimation_parameters:satellites:clk:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:clk:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:clk:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:clk:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:clk_rate:`**
 ` `


> Clock rates

---

###### **`estimation_parameters:satellites:clk_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:clk_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:clk_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:clk_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:satellites:code_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:code_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:code_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:code_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_0:`**
 ` `


> Empirical accleration bias 

---

###### **`estimation_parameters:satellites:emp_dyb_0:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_0:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_0:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_0:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_1c:`**
 ` `


> Empirical accleration 1 per rev cosine term 

---

###### **`estimation_parameters:satellites:emp_dyb_1c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_1c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_1c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_1c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_1s:`**
 ` `


> Empirical accleration 1 per rev sine term 

---

###### **`estimation_parameters:satellites:emp_dyb_1s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_1s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_1s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_1s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_2c:`**
 ` `


> Empirical accleration 2 per rev cosine term 

---

###### **`estimation_parameters:satellites:emp_dyb_2c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_2c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_2c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_2c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_2s:`**
 ` `


> Empirical accleration 2 per rev sine term 

---

###### **`estimation_parameters:satellites:emp_dyb_2s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_2s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_2s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_2s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_3c:`**
 ` `


> Empirical accleration 3 per rev cosine term 

---

###### **`estimation_parameters:satellites:emp_dyb_3c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_3c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_3c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_3c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_3s:`**
 ` `


> Empirical accleration 3 per rev sine term 

---

###### **`estimation_parameters:satellites:emp_dyb_3s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_3s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_3s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_3s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_4c:`**
 ` `


> Empirical accleration 4 per rev cosine term 

---

###### **`estimation_parameters:satellites:emp_dyb_4c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_4c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_4c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_4c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:emp_dyb_4s:`**
 ` `


> Empirical accleration 4 per rev sine term 

---

###### **`estimation_parameters:satellites:emp_dyb_4s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:emp_dyb_4s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:emp_dyb_4s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:emp_dyb_4s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:`**
 ` `


---

###### **`estimation_parameters:satellites:global:clk:`**
 ` `


> Clocks

---

###### **`estimation_parameters:satellites:global:clk:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:clk:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:clk:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:clk:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:clk_rate:`**
 ` `


> Clock rates

---

###### **`estimation_parameters:satellites:global:clk_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:clk_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:clk_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:clk_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:satellites:global:code_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:code_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:code_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:code_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_0:`**
 ` `


> Empirical accleration bias 

---

###### **`estimation_parameters:satellites:global:emp_dyb_0:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_0:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_0:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_0:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_1c:`**
 ` `


> Empirical accleration 1 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_1c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_1c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_1c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_1c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_1s:`**
 ` `


> Empirical accleration 1 per rev sine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_1s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_1s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_1s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_1s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_2c:`**
 ` `


> Empirical accleration 2 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_2c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_2c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_2c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_2c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_2s:`**
 ` `


> Empirical accleration 2 per rev sine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_2s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_2s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_2s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_2s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_3c:`**
 ` `


> Empirical accleration 3 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_3c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_3c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_3c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_3c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_3s:`**
 ` `


> Empirical accleration 3 per rev sine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_3s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_3s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_3s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_3s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_4c:`**
 ` `


> Empirical accleration 4 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_4c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_4c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_4c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_4c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:emp_dyb_4s:`**
 ` `


> Empirical accleration 4 per rev sine term 

---

###### **`estimation_parameters:satellites:global:emp_dyb_4s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:emp_dyb_4s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:emp_dyb_4s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:emp_dyb_4s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:orbit:`**
 ` `


> Orbital state

---

###### **`estimation_parameters:satellites:global:orbit:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:orbit:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:orbit:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:orbit:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:satellites:global:phase_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:phase_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:phase_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:phase_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:pos:`**
 ` `


> Position (prefer orbit)

---

###### **`estimation_parameters:satellites:global:pos:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:pos:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:pos:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:pos:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:pos_rate:`**
 ` `


> Velocity (prefer orbit)

---

###### **`estimation_parameters:satellites:global:pos_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:pos_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:pos_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:pos_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_0:`**
 ` `


> Emprical Solar Radiation scaled bias 

---

###### **`estimation_parameters:satellites:global:srp_dyb_0:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_0:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_0:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_0:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_1c:`**
 ` `


> Emprical Solar Radiation scaled 1 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_1c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_1c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_1c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_1c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_1s:`**
 ` `


> Emprical Solar Radiation scaled 1 per rev sine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_1s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_1s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_1s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_1s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_2c:`**
 ` `


> Emprical Solar Radiation scaled 2 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_2c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_2c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_2c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_2c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_2s:`**
 ` `


> Emprical Solar Radiation scaled 2 per rev sine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_2s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_2s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_2s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_2s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_3c:`**
 ` `


> Emprical Solar Radiation scaled 3 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_3c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_3c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_3c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_3c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_3s:`**
 ` `


> Emprical Solar Radiation scaled 3 per rev sine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_3s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_3s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_3s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_3s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_4c:`**
 ` `


> Emprical Solar Radiation scaled 4 per rev cosine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_4c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_4c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_4c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_4c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:srp_dyb_4s:`**
 ` `


> Emprical Solar Radiation scaled 4 per rev sine term 

---

###### **`estimation_parameters:satellites:global:srp_dyb_4s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:srp_dyb_4s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:srp_dyb_4s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:global:srp_dyb_4s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:global:code_sigmas:`**
 `[0] `


[floats] Standard deviation of code measurements

---

###### **`estimation_parameters:satellites:global:phase_sigmas:`**
 `[0] `


[floats] Standard deviation of phase measurmeents

---

###### **`estimation_parameters:satellites:orbit:`**
 ` `


> Orbital state

---

###### **`estimation_parameters:satellites:orbit:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:orbit:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:orbit:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:orbit:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:satellites:phase_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:phase_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:phase_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:phase_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:pos:`**
 ` `


> Position (prefer orbit)

---

###### **`estimation_parameters:satellites:pos:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:pos:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:pos:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:pos:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:pos_rate:`**
 ` `


> Velocity (prefer orbit)

---

###### **`estimation_parameters:satellites:pos_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:pos_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:pos_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:pos_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_0:`**
 ` `


> Emprical Solar Radiation scaled bias 

---

###### **`estimation_parameters:satellites:srp_dyb_0:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_0:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_0:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_0:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_1c:`**
 ` `


> Emprical Solar Radiation scaled 1 per rev cosine term 

---

###### **`estimation_parameters:satellites:srp_dyb_1c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_1c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_1c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_1c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_1s:`**
 ` `


> Emprical Solar Radiation scaled 1 per rev sine term 

---

###### **`estimation_parameters:satellites:srp_dyb_1s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_1s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_1s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_1s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_2c:`**
 ` `


> Emprical Solar Radiation scaled 2 per rev cosine term 

---

###### **`estimation_parameters:satellites:srp_dyb_2c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_2c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_2c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_2c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_2s:`**
 ` `


> Emprical Solar Radiation scaled 2 per rev sine term 

---

###### **`estimation_parameters:satellites:srp_dyb_2s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_2s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_2s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_2s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_3c:`**
 ` `


> Emprical Solar Radiation scaled 3 per rev cosine term 

---

###### **`estimation_parameters:satellites:srp_dyb_3c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_3c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_3c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_3c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_3s:`**
 ` `


> Emprical Solar Radiation scaled 3 per rev sine term 

---

###### **`estimation_parameters:satellites:srp_dyb_3s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_3s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_3s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_3s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_4c:`**
 ` `


> Emprical Solar Radiation scaled 4 per rev cosine term 

---

###### **`estimation_parameters:satellites:srp_dyb_4c:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_4c:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_4c:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_4c:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:srp_dyb_4s:`**
 ` `


> Emprical Solar Radiation scaled 4 per rev sine term 

---

###### **`estimation_parameters:satellites:srp_dyb_4s:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:srp_dyb_4s:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:srp_dyb_4s:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:satellites:srp_dyb_4s:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:satellites:code_sigmas:`**
 `[0] `


[floats] Standard deviation of code measurements

---

###### **`estimation_parameters:satellites:phase_sigmas:`**
 `[0] `


[floats] Standard deviation of phase measurmeents

---

###### **`estimation_parameters:stations:`**
 ` `


---

###### **`estimation_parameters:stations:amb:`**
 ` `


> Integer phase ambiguities

---

###### **`estimation_parameters:stations:amb:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:amb:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:amb:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:amb:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:clk:`**
 ` `


> Clocks

---

###### **`estimation_parameters:stations:clk:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:clk:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:clk:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:clk:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:clk_rate:`**
 ` `


> Clock rates

---

###### **`estimation_parameters:stations:clk_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:clk_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:clk_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:clk_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:stations:code_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:code_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:code_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:code_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:`**
 ` `


---

###### **`estimation_parameters:stations:global:amb:`**
 ` `


> Integer phase ambiguities

---

###### **`estimation_parameters:stations:global:amb:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:amb:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:amb:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:amb:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:clk:`**
 ` `


> Clocks

---

###### **`estimation_parameters:stations:global:clk:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:clk:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:clk:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:clk:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:clk_rate:`**
 ` `


> Clock rates

---

###### **`estimation_parameters:stations:global:clk_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:clk_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:clk_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:clk_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:stations:global:code_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:code_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:code_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:code_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:ion_stec:`**
 ` `


> Ionospheric slant delay

---

###### **`estimation_parameters:stations:global:ion_stec:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:ion_stec:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:ion_stec:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:ion_stec:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:stations:global:phase_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:phase_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:phase_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:phase_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:pos:`**
 ` `


> Position

---

###### **`estimation_parameters:stations:global:pos:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:pos:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:pos:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:pos:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:pos_rate:`**
 ` `


> Velocity

---

###### **`estimation_parameters:stations:global:pos_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:pos_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:pos_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:pos_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:trop:`**
 ` `


> Troposphere corrections

---

###### **`estimation_parameters:stations:global:trop:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:trop:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:trop:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:trop:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:trop_grads:`**
 ` `


> Troposphere gradients

---

###### **`estimation_parameters:stations:global:trop_grads:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:global:trop_grads:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:global:trop_grads:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:global:trop_grads:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:global:code_sigmas:`**
 `[1] `


[floats] Standard deviation of code measurements

---

###### **`estimation_parameters:stations:global:phase_sigmas:`**
 `[0.0015] `


[floats] Standard deviation of phase measurmeents

---

###### **`estimation_parameters:stations:ion_stec:`**
 ` `


> Ionospheric slant delay

---

###### **`estimation_parameters:stations:ion_stec:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:ion_stec:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:ion_stec:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:ion_stec:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:stations:phase_bias:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:phase_bias:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:phase_bias:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:phase_bias:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:pos:`**
 ` `


> Position

---

###### **`estimation_parameters:stations:pos:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:pos:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:pos:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:pos:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:pos_rate:`**
 ` `


> Velocity

---

###### **`estimation_parameters:stations:pos_rate:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:pos_rate:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:pos_rate:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:pos_rate:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:trop:`**
 ` `


> Troposphere corrections

---

###### **`estimation_parameters:stations:trop:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:trop:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:trop:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:trop:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:trop_grads:`**
 ` `


> Troposphere gradients

---

###### **`estimation_parameters:stations:trop_grads:estimated:`**
 `[false] `


[bools] Estimate state in kalman filter

---

###### **`estimation_parameters:stations:trop_grads:sigma:`**
 `[0] `


[floats] Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:stations:trop_grads:process_noise:`**
 `[0] `


[floats] Process noise sigmas

---

###### **`estimation_parameters:stations:trop_grads:apriori_value:`**
 `[0] `


[floats] Apriori state values

---

###### **`estimation_parameters:stations:code_sigmas:`**
 `[1] `


[floats] Standard deviation of code measurements

---

###### **`estimation_parameters:stations:phase_sigmas:`**
 `[0.0015] `


[floats] Standard deviation of phase measurmeents

---

## mongo:

###### **`mongo:`**
 ` `


> Mongo is a database used to store results and intermediate values for later analysis and inter-process communication

---

###### **`mongo:delete_history:`**
 `false `


(bool) Drop the collection in the database at the beginning of the run to only show fresh data

---

###### **`mongo:output_components:`**
 `false `


(bool) Output components of measurements

---

###### **`mongo:output_measurements:`**
 `false `


(bool) Output measurements and their residuals

---

###### **`mongo:output_states:`**
 `false `


(bool) Output states

---

###### **`mongo:enable:`**
 `false `


(bool) Enable and connect to mongo database

---

# Enum Details

---

### E_ARmode

Valid enum values are:
- `off`
- `round`
- `iter_rnd`
- `bootst`
- `lambda`
- `lambda_alt`
- `lambda_al2`
- `lambda_bie`

For options:

- [`processing_options:ambiguity_resolution:wide_lane:mode:`](#processing_optionsambiguity_resolutionwide_lanemode)
- [`processing_options:ambiguity_resolution:mode:`](#processing_optionsambiguity_resolutionmode)
---

### E_ChiSqMode

Valid enum values are:
- `none`
- `innovation`
- `measurement`
- `state`

For options:

- [`processing_options:minimum_constraints:outlier_screening:chi_square_mode:`](#processing_optionsminimum_constraintsoutlier_screeningchi_square_mode)
- [`processing_options:filter_options:outlier_screening:chi_square_mode:`](#processing_optionsfilter_optionsoutlier_screeningchi_square_mode)
---

### E_Inverter

Valid enum values are:
- `none`
- `inv`
- `llt`
- `ldlt`
- `colpivhqr`
- `bdcsvd`
- `jacobisvd`
- `fullpivlu`
- `first_unsupported`
- `fullpivhqr`

For options:

- [`processing_options:minimum_constraints:inverter:`](#processing_optionsminimum_constraintsinverter)
- [`processing_options:filter_options:inverter:`](#processing_optionsfilter_optionsinverter)
- [`processing_options:filter_options:rts:inverter:`](#processing_optionsfilter_optionsrtsinverter)
---

### E_IonoMapFn

Valid enum values are:
- `slm`
- `mslm`
- `mlm`
- `klobuchar`

For options:

- [`processing_options:gnss_models:ionospheric_component:mapping_function:`](#processing_optionsgnss_modelsionospheric_componentmapping_function)
---

### E_IonoMode

Valid enum values are:
- `off`
- `broadcast` : Values derived from broadcast ephemeris streams/files
- `sbas`
- `iono_free_linear_combo`
- `estimate`
- `total_electron_content`
- `qzs`
- `lex`
- `stec`

For options:

- [`processing_options:gnss_models:ionospheric_component:corr_mode:`](#processing_optionsgnss_modelsionospheric_componentcorr_mode)
---

### E_IonoModel

Valid enum values are:
- `none`
- `meas_out`
- `bspline`
- `spherical_caps`
- `spherical_harmonics`
- `local`

For options:

- [`processing_options:gnss_models:ionospheric_model:model:`](#processing_optionsgnss_modelsionospheric_modelmodel)
---

### E_NavMsgType

Valid enum values are:
- `none`
- `lnav`
- `fdma`
- `fnav`
- `inav`
- `ifnv`
- `d1`
- `d2`
- `d1d2`
- `sbas`
- `cnav`
- `cnv1`
- `cnv2`
- `cnv3`
- `cnvx`

For options:

- [`processing_options:gnss_general:sys_options:gps:used_nav_type:`](#processing_optionsgnss_generalsys_optionsgpsused_nav_type)
- [`processing_options:gnss_general:sys_options:gal:used_nav_type:`](#processing_optionsgnss_generalsys_optionsgalused_nav_type)
- [`processing_options:gnss_general:sys_options:glo:used_nav_type:`](#processing_optionsgnss_generalsys_optionsgloused_nav_type)
- [`processing_options:gnss_general:sys_options:qzs:used_nav_type:`](#processing_optionsgnss_generalsys_optionsqzsused_nav_type)
- [`processing_options:gnss_general:sys_options:sbs:used_nav_type:`](#processing_optionsgnss_generalsys_optionssbsused_nav_type)
- [`processing_options:gnss_general:sys_options:bds:used_nav_type:`](#processing_optionsgnss_generalsys_optionsbdsused_nav_type)
- [`processing_options:gnss_general:sys_options:leo:used_nav_type:`](#processing_optionsgnss_generalsys_optionsleoused_nav_type)
---

### E_NoiseModel

Valid enum values are:
- `uniform`
- `elevation_dependent`

For options:

- [`processing_options:gnss_general:error_model:`](#processing_optionsgnss_generalerror_model)
- [`estimation_parameters:stations:error_model:`](#estimation_parametersstationserror_model)
- [`estimation_parameters:stations:global:error_model:`](#estimation_parametersstationsglobalerror_model)
- [`estimation_parameters:stations:global:error_model:`](#estimation_parametersstationsglobalerror_model)
- [`estimation_parameters:stations:XMPL:error_model:`](#estimation_parametersstationsXMPLerror_model)
---

### E_ObsCode

Valid enum values are:
- `none`
- `l1c`
- `l1p`
- `l1w`
- `l1y`
- `l1m`
- `l1n`
- `l1s`
- `l1l`
- `l1e`
- `l1a`
- `l1b`
- `l1x`
- `l1z`
- `l2c`
- `l2d`
- `l2s`
- `l2l`
- `l2x`
- `l2p`
- `l2w`
- `l2y`
- `l2m`
- `l2n`
- `l5i`
- `l5q`
- `l5x`
- `l7i`
- `l7q`
- `l7x`
- `l6a`
- `l6b`
- `l6c`
- `l6x`
- `l6z`
- `l6s`
- `l6l`
- `l8i`
- `l8q`
- `l8x`
- `l2i`
- `l2q`
- `l6i`
- `l6q`
- `l3i`
- `l3q`
- `l3x`
- `l1i`
- `l1q`
- `l4a`
- `l4b`
- `l4x`
- `l6e`
- `l1d`
- `l5d`
- `l5p`
- `l9a`
- `l9b`
- `l9c`
- `l9x`
- `l5a`
- `l5b`
- `l5c`
- `l5z`
- `l6d`
- `l6p`
- `l7d`
- `l7p`
- `l7z`
- `l8d`
- `l8p`
- `l8z`

For options:

- [`station_options:global:rnx_code_conversions:gps:NONE:`](#station_optionsglobalrnx_code_conversionsgpsNONE)
- [`station_options:global:rnx_phase_conversions:gps:NONE:`](#station_optionsglobalrnx_phase_conversionsgpsNONE)
- [`station_options:global:rnx_code_conversions:gps:P1:`](#station_optionsglobalrnx_code_conversionsgpsP1)
- [`station_options:global:rnx_phase_conversions:gps:P1:`](#station_optionsglobalrnx_phase_conversionsgpsP1)
- [`station_options:global:rnx_code_conversions:gps:P2:`](#station_optionsglobalrnx_code_conversionsgpsP2)
- [`station_options:global:rnx_phase_conversions:gps:P2:`](#station_optionsglobalrnx_phase_conversionsgpsP2)
- [`station_options:global:rnx_code_conversions:gps:C1:`](#station_optionsglobalrnx_code_conversionsgpsC1)
- [`station_options:global:rnx_phase_conversions:gps:C1:`](#station_optionsglobalrnx_phase_conversionsgpsC1)
- [`station_options:global:rnx_code_conversions:gps:C2:`](#station_optionsglobalrnx_code_conversionsgpsC2)
- [`station_options:global:rnx_phase_conversions:gps:C2:`](#station_optionsglobalrnx_phase_conversionsgpsC2)
- [`station_options:global:rnx_code_conversions:gps:C3:`](#station_optionsglobalrnx_code_conversionsgpsC3)
- [`station_options:global:rnx_phase_conversions:gps:C3:`](#station_optionsglobalrnx_phase_conversionsgpsC3)
- [`station_options:global:rnx_code_conversions:gps:C4:`](#station_optionsglobalrnx_code_conversionsgpsC4)
- [`station_options:global:rnx_phase_conversions:gps:C4:`](#station_optionsglobalrnx_phase_conversionsgpsC4)
- [`station_options:global:rnx_code_conversions:gps:C5:`](#station_optionsglobalrnx_code_conversionsgpsC5)
- [`station_options:global:rnx_phase_conversions:gps:C5:`](#station_optionsglobalrnx_phase_conversionsgpsC5)
- [`station_options:global:rnx_code_conversions:gps:C6:`](#station_optionsglobalrnx_code_conversionsgpsC6)
- [`station_options:global:rnx_phase_conversions:gps:C6:`](#station_optionsglobalrnx_phase_conversionsgpsC6)
- [`station_options:global:rnx_code_conversions:gps:C7:`](#station_optionsglobalrnx_code_conversionsgpsC7)
- [`station_options:global:rnx_phase_conversions:gps:C7:`](#station_optionsglobalrnx_phase_conversionsgpsC7)
- [`station_options:global:rnx_code_conversions:gps:C8:`](#station_optionsglobalrnx_code_conversionsgpsC8)
- [`station_options:global:rnx_phase_conversions:gps:C8:`](#station_optionsglobalrnx_phase_conversionsgpsC8)
- [`station_options:global:rnx_code_conversions:gps:L1:`](#station_optionsglobalrnx_code_conversionsgpsL1)
- [`station_options:global:rnx_phase_conversions:gps:L1:`](#station_optionsglobalrnx_phase_conversionsgpsL1)
- [`station_options:global:rnx_code_conversions:gps:L2:`](#station_optionsglobalrnx_code_conversionsgpsL2)
- [`station_options:global:rnx_phase_conversions:gps:L2:`](#station_optionsglobalrnx_phase_conversionsgpsL2)
- [`station_options:global:rnx_code_conversions:gps:L3:`](#station_optionsglobalrnx_code_conversionsgpsL3)
- [`station_options:global:rnx_phase_conversions:gps:L3:`](#station_optionsglobalrnx_phase_conversionsgpsL3)
- [`station_options:global:rnx_code_conversions:gps:L4:`](#station_optionsglobalrnx_code_conversionsgpsL4)
- [`station_options:global:rnx_phase_conversions:gps:L4:`](#station_optionsglobalrnx_phase_conversionsgpsL4)
- [`station_options:global:rnx_code_conversions:gps:L5:`](#station_optionsglobalrnx_code_conversionsgpsL5)
- [`station_options:global:rnx_phase_conversions:gps:L5:`](#station_optionsglobalrnx_phase_conversionsgpsL5)
- [`station_options:global:rnx_code_conversions:gps:L6:`](#station_optionsglobalrnx_code_conversionsgpsL6)
- [`station_options:global:rnx_phase_conversions:gps:L6:`](#station_optionsglobalrnx_phase_conversionsgpsL6)
- [`station_options:global:rnx_code_conversions:gps:L7:`](#station_optionsglobalrnx_code_conversionsgpsL7)
- [`station_options:global:rnx_phase_conversions:gps:L7:`](#station_optionsglobalrnx_phase_conversionsgpsL7)
- [`station_options:global:rnx_code_conversions:gps:L8:`](#station_optionsglobalrnx_code_conversionsgpsL8)
- [`station_options:global:rnx_phase_conversions:gps:L8:`](#station_optionsglobalrnx_phase_conversionsgpsL8)
- [`station_options:global:rnx_code_conversions:gps:LA:`](#station_optionsglobalrnx_code_conversionsgpsLA)
- [`station_options:global:rnx_phase_conversions:gps:LA:`](#station_optionsglobalrnx_phase_conversionsgpsLA)
- [`station_options:global:rnx_code_conversions:gal:NONE:`](#station_optionsglobalrnx_code_conversionsgalNONE)
- [`station_options:global:rnx_phase_conversions:gal:NONE:`](#station_optionsglobalrnx_phase_conversionsgalNONE)
- [`station_options:global:rnx_code_conversions:gal:P1:`](#station_optionsglobalrnx_code_conversionsgalP1)
- [`station_options:global:rnx_phase_conversions:gal:P1:`](#station_optionsglobalrnx_phase_conversionsgalP1)
- [`station_options:global:rnx_code_conversions:gal:P2:`](#station_optionsglobalrnx_code_conversionsgalP2)
- [`station_options:global:rnx_phase_conversions:gal:P2:`](#station_optionsglobalrnx_phase_conversionsgalP2)
- [`station_options:global:rnx_code_conversions:gal:C1:`](#station_optionsglobalrnx_code_conversionsgalC1)
- [`station_options:global:rnx_phase_conversions:gal:C1:`](#station_optionsglobalrnx_phase_conversionsgalC1)
- [`station_options:global:rnx_code_conversions:gal:C2:`](#station_optionsglobalrnx_code_conversionsgalC2)
- [`station_options:global:rnx_phase_conversions:gal:C2:`](#station_optionsglobalrnx_phase_conversionsgalC2)
- [`station_options:global:rnx_code_conversions:gal:C3:`](#station_optionsglobalrnx_code_conversionsgalC3)
- [`station_options:global:rnx_phase_conversions:gal:C3:`](#station_optionsglobalrnx_phase_conversionsgalC3)
- [`station_options:global:rnx_code_conversions:gal:C4:`](#station_optionsglobalrnx_code_conversionsgalC4)
- [`station_options:global:rnx_phase_conversions:gal:C4:`](#station_optionsglobalrnx_phase_conversionsgalC4)
- [`station_options:global:rnx_code_conversions:gal:C5:`](#station_optionsglobalrnx_code_conversionsgalC5)
- [`station_options:global:rnx_phase_conversions:gal:C5:`](#station_optionsglobalrnx_phase_conversionsgalC5)
- [`station_options:global:rnx_code_conversions:gal:C6:`](#station_optionsglobalrnx_code_conversionsgalC6)
- [`station_options:global:rnx_phase_conversions:gal:C6:`](#station_optionsglobalrnx_phase_conversionsgalC6)
- [`station_options:global:rnx_code_conversions:gal:C7:`](#station_optionsglobalrnx_code_conversionsgalC7)
- [`station_options:global:rnx_phase_conversions:gal:C7:`](#station_optionsglobalrnx_phase_conversionsgalC7)
- [`station_options:global:rnx_code_conversions:gal:C8:`](#station_optionsglobalrnx_code_conversionsgalC8)
- [`station_options:global:rnx_phase_conversions:gal:C8:`](#station_optionsglobalrnx_phase_conversionsgalC8)
- [`station_options:global:rnx_code_conversions:gal:L1:`](#station_optionsglobalrnx_code_conversionsgalL1)
- [`station_options:global:rnx_phase_conversions:gal:L1:`](#station_optionsglobalrnx_phase_conversionsgalL1)
- [`station_options:global:rnx_code_conversions:gal:L2:`](#station_optionsglobalrnx_code_conversionsgalL2)
- [`station_options:global:rnx_phase_conversions:gal:L2:`](#station_optionsglobalrnx_phase_conversionsgalL2)
- [`station_options:global:rnx_code_conversions:gal:L3:`](#station_optionsglobalrnx_code_conversionsgalL3)
- [`station_options:global:rnx_phase_conversions:gal:L3:`](#station_optionsglobalrnx_phase_conversionsgalL3)
- [`station_options:global:rnx_code_conversions:gal:L4:`](#station_optionsglobalrnx_code_conversionsgalL4)
- [`station_options:global:rnx_phase_conversions:gal:L4:`](#station_optionsglobalrnx_phase_conversionsgalL4)
- [`station_options:global:rnx_code_conversions:gal:L5:`](#station_optionsglobalrnx_code_conversionsgalL5)
- [`station_options:global:rnx_phase_conversions:gal:L5:`](#station_optionsglobalrnx_phase_conversionsgalL5)
- [`station_options:global:rnx_code_conversions:gal:L6:`](#station_optionsglobalrnx_code_conversionsgalL6)
- [`station_options:global:rnx_phase_conversions:gal:L6:`](#station_optionsglobalrnx_phase_conversionsgalL6)
- [`station_options:global:rnx_code_conversions:gal:L7:`](#station_optionsglobalrnx_code_conversionsgalL7)
- [`station_options:global:rnx_phase_conversions:gal:L7:`](#station_optionsglobalrnx_phase_conversionsgalL7)
- [`station_options:global:rnx_code_conversions:gal:L8:`](#station_optionsglobalrnx_code_conversionsgalL8)
- [`station_options:global:rnx_phase_conversions:gal:L8:`](#station_optionsglobalrnx_phase_conversionsgalL8)
- [`station_options:global:rnx_code_conversions:gal:LA:`](#station_optionsglobalrnx_code_conversionsgalLA)
- [`station_options:global:rnx_phase_conversions:gal:LA:`](#station_optionsglobalrnx_phase_conversionsgalLA)
- [`station_options:global:rnx_code_conversions:glo:NONE:`](#station_optionsglobalrnx_code_conversionsgloNONE)
- [`station_options:global:rnx_phase_conversions:glo:NONE:`](#station_optionsglobalrnx_phase_conversionsgloNONE)
- [`station_options:global:rnx_code_conversions:glo:P1:`](#station_optionsglobalrnx_code_conversionsgloP1)
- [`station_options:global:rnx_phase_conversions:glo:P1:`](#station_optionsglobalrnx_phase_conversionsgloP1)
- [`station_options:global:rnx_code_conversions:glo:P2:`](#station_optionsglobalrnx_code_conversionsgloP2)
- [`station_options:global:rnx_phase_conversions:glo:P2:`](#station_optionsglobalrnx_phase_conversionsgloP2)
- [`station_options:global:rnx_code_conversions:glo:C1:`](#station_optionsglobalrnx_code_conversionsgloC1)
- [`station_options:global:rnx_phase_conversions:glo:C1:`](#station_optionsglobalrnx_phase_conversionsgloC1)
- [`station_options:global:rnx_code_conversions:glo:C2:`](#station_optionsglobalrnx_code_conversionsgloC2)
- [`station_options:global:rnx_phase_conversions:glo:C2:`](#station_optionsglobalrnx_phase_conversionsgloC2)
- [`station_options:global:rnx_code_conversions:glo:C3:`](#station_optionsglobalrnx_code_conversionsgloC3)
- [`station_options:global:rnx_phase_conversions:glo:C3:`](#station_optionsglobalrnx_phase_conversionsgloC3)
- [`station_options:global:rnx_code_conversions:glo:C4:`](#station_optionsglobalrnx_code_conversionsgloC4)
- [`station_options:global:rnx_phase_conversions:glo:C4:`](#station_optionsglobalrnx_phase_conversionsgloC4)
- [`station_options:global:rnx_code_conversions:glo:C5:`](#station_optionsglobalrnx_code_conversionsgloC5)
- [`station_options:global:rnx_phase_conversions:glo:C5:`](#station_optionsglobalrnx_phase_conversionsgloC5)
- [`station_options:global:rnx_code_conversions:glo:C6:`](#station_optionsglobalrnx_code_conversionsgloC6)
- [`station_options:global:rnx_phase_conversions:glo:C6:`](#station_optionsglobalrnx_phase_conversionsgloC6)
- [`station_options:global:rnx_code_conversions:glo:C7:`](#station_optionsglobalrnx_code_conversionsgloC7)
- [`station_options:global:rnx_phase_conversions:glo:C7:`](#station_optionsglobalrnx_phase_conversionsgloC7)
- [`station_options:global:rnx_code_conversions:glo:C8:`](#station_optionsglobalrnx_code_conversionsgloC8)
- [`station_options:global:rnx_phase_conversions:glo:C8:`](#station_optionsglobalrnx_phase_conversionsgloC8)
- [`station_options:global:rnx_code_conversions:glo:L1:`](#station_optionsglobalrnx_code_conversionsgloL1)
- [`station_options:global:rnx_phase_conversions:glo:L1:`](#station_optionsglobalrnx_phase_conversionsgloL1)
- [`station_options:global:rnx_code_conversions:glo:L2:`](#station_optionsglobalrnx_code_conversionsgloL2)
- [`station_options:global:rnx_phase_conversions:glo:L2:`](#station_optionsglobalrnx_phase_conversionsgloL2)
- [`station_options:global:rnx_code_conversions:glo:L3:`](#station_optionsglobalrnx_code_conversionsgloL3)
- [`station_options:global:rnx_phase_conversions:glo:L3:`](#station_optionsglobalrnx_phase_conversionsgloL3)
- [`station_options:global:rnx_code_conversions:glo:L4:`](#station_optionsglobalrnx_code_conversionsgloL4)
- [`station_options:global:rnx_phase_conversions:glo:L4:`](#station_optionsglobalrnx_phase_conversionsgloL4)
- [`station_options:global:rnx_code_conversions:glo:L5:`](#station_optionsglobalrnx_code_conversionsgloL5)
- [`station_options:global:rnx_phase_conversions:glo:L5:`](#station_optionsglobalrnx_phase_conversionsgloL5)
- [`station_options:global:rnx_code_conversions:glo:L6:`](#station_optionsglobalrnx_code_conversionsgloL6)
- [`station_options:global:rnx_phase_conversions:glo:L6:`](#station_optionsglobalrnx_phase_conversionsgloL6)
- [`station_options:global:rnx_code_conversions:glo:L7:`](#station_optionsglobalrnx_code_conversionsgloL7)
- [`station_options:global:rnx_phase_conversions:glo:L7:`](#station_optionsglobalrnx_phase_conversionsgloL7)
- [`station_options:global:rnx_code_conversions:glo:L8:`](#station_optionsglobalrnx_code_conversionsgloL8)
- [`station_options:global:rnx_phase_conversions:glo:L8:`](#station_optionsglobalrnx_phase_conversionsgloL8)
- [`station_options:global:rnx_code_conversions:glo:LA:`](#station_optionsglobalrnx_code_conversionsgloLA)
- [`station_options:global:rnx_phase_conversions:glo:LA:`](#station_optionsglobalrnx_phase_conversionsgloLA)
- [`station_options:global:rnx_code_conversions:qzs:NONE:`](#station_optionsglobalrnx_code_conversionsqzsNONE)
- [`station_options:global:rnx_phase_conversions:qzs:NONE:`](#station_optionsglobalrnx_phase_conversionsqzsNONE)
- [`station_options:global:rnx_code_conversions:qzs:P1:`](#station_optionsglobalrnx_code_conversionsqzsP1)
- [`station_options:global:rnx_phase_conversions:qzs:P1:`](#station_optionsglobalrnx_phase_conversionsqzsP1)
- [`station_options:global:rnx_code_conversions:qzs:P2:`](#station_optionsglobalrnx_code_conversionsqzsP2)
- [`station_options:global:rnx_phase_conversions:qzs:P2:`](#station_optionsglobalrnx_phase_conversionsqzsP2)
- [`station_options:global:rnx_code_conversions:qzs:C1:`](#station_optionsglobalrnx_code_conversionsqzsC1)
- [`station_options:global:rnx_phase_conversions:qzs:C1:`](#station_optionsglobalrnx_phase_conversionsqzsC1)
- [`station_options:global:rnx_code_conversions:qzs:C2:`](#station_optionsglobalrnx_code_conversionsqzsC2)
- [`station_options:global:rnx_phase_conversions:qzs:C2:`](#station_optionsglobalrnx_phase_conversionsqzsC2)
- [`station_options:global:rnx_code_conversions:qzs:C3:`](#station_optionsglobalrnx_code_conversionsqzsC3)
- [`station_options:global:rnx_phase_conversions:qzs:C3:`](#station_optionsglobalrnx_phase_conversionsqzsC3)
- [`station_options:global:rnx_code_conversions:qzs:C4:`](#station_optionsglobalrnx_code_conversionsqzsC4)
- [`station_options:global:rnx_phase_conversions:qzs:C4:`](#station_optionsglobalrnx_phase_conversionsqzsC4)
- [`station_options:global:rnx_code_conversions:qzs:C5:`](#station_optionsglobalrnx_code_conversionsqzsC5)
- [`station_options:global:rnx_phase_conversions:qzs:C5:`](#station_optionsglobalrnx_phase_conversionsqzsC5)
- [`station_options:global:rnx_code_conversions:qzs:C6:`](#station_optionsglobalrnx_code_conversionsqzsC6)
- [`station_options:global:rnx_phase_conversions:qzs:C6:`](#station_optionsglobalrnx_phase_conversionsqzsC6)
- [`station_options:global:rnx_code_conversions:qzs:C7:`](#station_optionsglobalrnx_code_conversionsqzsC7)
- [`station_options:global:rnx_phase_conversions:qzs:C7:`](#station_optionsglobalrnx_phase_conversionsqzsC7)
- [`station_options:global:rnx_code_conversions:qzs:C8:`](#station_optionsglobalrnx_code_conversionsqzsC8)
- [`station_options:global:rnx_phase_conversions:qzs:C8:`](#station_optionsglobalrnx_phase_conversionsqzsC8)
- [`station_options:global:rnx_code_conversions:qzs:L1:`](#station_optionsglobalrnx_code_conversionsqzsL1)
- [`station_options:global:rnx_phase_conversions:qzs:L1:`](#station_optionsglobalrnx_phase_conversionsqzsL1)
- [`station_options:global:rnx_code_conversions:qzs:L2:`](#station_optionsglobalrnx_code_conversionsqzsL2)
- [`station_options:global:rnx_phase_conversions:qzs:L2:`](#station_optionsglobalrnx_phase_conversionsqzsL2)
- [`station_options:global:rnx_code_conversions:qzs:L3:`](#station_optionsglobalrnx_code_conversionsqzsL3)
- [`station_options:global:rnx_phase_conversions:qzs:L3:`](#station_optionsglobalrnx_phase_conversionsqzsL3)
- [`station_options:global:rnx_code_conversions:qzs:L4:`](#station_optionsglobalrnx_code_conversionsqzsL4)
- [`station_options:global:rnx_phase_conversions:qzs:L4:`](#station_optionsglobalrnx_phase_conversionsqzsL4)
- [`station_options:global:rnx_code_conversions:qzs:L5:`](#station_optionsglobalrnx_code_conversionsqzsL5)
- [`station_options:global:rnx_phase_conversions:qzs:L5:`](#station_optionsglobalrnx_phase_conversionsqzsL5)
- [`station_options:global:rnx_code_conversions:qzs:L6:`](#station_optionsglobalrnx_code_conversionsqzsL6)
- [`station_options:global:rnx_phase_conversions:qzs:L6:`](#station_optionsglobalrnx_phase_conversionsqzsL6)
- [`station_options:global:rnx_code_conversions:qzs:L7:`](#station_optionsglobalrnx_code_conversionsqzsL7)
- [`station_options:global:rnx_phase_conversions:qzs:L7:`](#station_optionsglobalrnx_phase_conversionsqzsL7)
- [`station_options:global:rnx_code_conversions:qzs:L8:`](#station_optionsglobalrnx_code_conversionsqzsL8)
- [`station_options:global:rnx_phase_conversions:qzs:L8:`](#station_optionsglobalrnx_phase_conversionsqzsL8)
- [`station_options:global:rnx_code_conversions:qzs:LA:`](#station_optionsglobalrnx_code_conversionsqzsLA)
- [`station_options:global:rnx_phase_conversions:qzs:LA:`](#station_optionsglobalrnx_phase_conversionsqzsLA)
- [`station_options:global:rnx_code_conversions:sbs:NONE:`](#station_optionsglobalrnx_code_conversionssbsNONE)
- [`station_options:global:rnx_phase_conversions:sbs:NONE:`](#station_optionsglobalrnx_phase_conversionssbsNONE)
- [`station_options:global:rnx_code_conversions:sbs:P1:`](#station_optionsglobalrnx_code_conversionssbsP1)
- [`station_options:global:rnx_phase_conversions:sbs:P1:`](#station_optionsglobalrnx_phase_conversionssbsP1)
- [`station_options:global:rnx_code_conversions:sbs:P2:`](#station_optionsglobalrnx_code_conversionssbsP2)
- [`station_options:global:rnx_phase_conversions:sbs:P2:`](#station_optionsglobalrnx_phase_conversionssbsP2)
- [`station_options:global:rnx_code_conversions:sbs:C1:`](#station_optionsglobalrnx_code_conversionssbsC1)
- [`station_options:global:rnx_phase_conversions:sbs:C1:`](#station_optionsglobalrnx_phase_conversionssbsC1)
- [`station_options:global:rnx_code_conversions:sbs:C2:`](#station_optionsglobalrnx_code_conversionssbsC2)
- [`station_options:global:rnx_phase_conversions:sbs:C2:`](#station_optionsglobalrnx_phase_conversionssbsC2)
- [`station_options:global:rnx_code_conversions:sbs:C3:`](#station_optionsglobalrnx_code_conversionssbsC3)
- [`station_options:global:rnx_phase_conversions:sbs:C3:`](#station_optionsglobalrnx_phase_conversionssbsC3)
- [`station_options:global:rnx_code_conversions:sbs:C4:`](#station_optionsglobalrnx_code_conversionssbsC4)
- [`station_options:global:rnx_phase_conversions:sbs:C4:`](#station_optionsglobalrnx_phase_conversionssbsC4)
- [`station_options:global:rnx_code_conversions:sbs:C5:`](#station_optionsglobalrnx_code_conversionssbsC5)
- [`station_options:global:rnx_phase_conversions:sbs:C5:`](#station_optionsglobalrnx_phase_conversionssbsC5)
- [`station_options:global:rnx_code_conversions:sbs:C6:`](#station_optionsglobalrnx_code_conversionssbsC6)
- [`station_options:global:rnx_phase_conversions:sbs:C6:`](#station_optionsglobalrnx_phase_conversionssbsC6)
- [`station_options:global:rnx_code_conversions:sbs:C7:`](#station_optionsglobalrnx_code_conversionssbsC7)
- [`station_options:global:rnx_phase_conversions:sbs:C7:`](#station_optionsglobalrnx_phase_conversionssbsC7)
- [`station_options:global:rnx_code_conversions:sbs:C8:`](#station_optionsglobalrnx_code_conversionssbsC8)
- [`station_options:global:rnx_phase_conversions:sbs:C8:`](#station_optionsglobalrnx_phase_conversionssbsC8)
- [`station_options:global:rnx_code_conversions:sbs:L1:`](#station_optionsglobalrnx_code_conversionssbsL1)
- [`station_options:global:rnx_phase_conversions:sbs:L1:`](#station_optionsglobalrnx_phase_conversionssbsL1)
- [`station_options:global:rnx_code_conversions:sbs:L2:`](#station_optionsglobalrnx_code_conversionssbsL2)
- [`station_options:global:rnx_phase_conversions:sbs:L2:`](#station_optionsglobalrnx_phase_conversionssbsL2)
- [`station_options:global:rnx_code_conversions:sbs:L3:`](#station_optionsglobalrnx_code_conversionssbsL3)
- [`station_options:global:rnx_phase_conversions:sbs:L3:`](#station_optionsglobalrnx_phase_conversionssbsL3)
- [`station_options:global:rnx_code_conversions:sbs:L4:`](#station_optionsglobalrnx_code_conversionssbsL4)
- [`station_options:global:rnx_phase_conversions:sbs:L4:`](#station_optionsglobalrnx_phase_conversionssbsL4)
- [`station_options:global:rnx_code_conversions:sbs:L5:`](#station_optionsglobalrnx_code_conversionssbsL5)
- [`station_options:global:rnx_phase_conversions:sbs:L5:`](#station_optionsglobalrnx_phase_conversionssbsL5)
- [`station_options:global:rnx_code_conversions:sbs:L6:`](#station_optionsglobalrnx_code_conversionssbsL6)
- [`station_options:global:rnx_phase_conversions:sbs:L6:`](#station_optionsglobalrnx_phase_conversionssbsL6)
- [`station_options:global:rnx_code_conversions:sbs:L7:`](#station_optionsglobalrnx_code_conversionssbsL7)
- [`station_options:global:rnx_phase_conversions:sbs:L7:`](#station_optionsglobalrnx_phase_conversionssbsL7)
- [`station_options:global:rnx_code_conversions:sbs:L8:`](#station_optionsglobalrnx_code_conversionssbsL8)
- [`station_options:global:rnx_phase_conversions:sbs:L8:`](#station_optionsglobalrnx_phase_conversionssbsL8)
- [`station_options:global:rnx_code_conversions:sbs:LA:`](#station_optionsglobalrnx_code_conversionssbsLA)
- [`station_options:global:rnx_phase_conversions:sbs:LA:`](#station_optionsglobalrnx_phase_conversionssbsLA)
- [`station_options:global:rnx_code_conversions:bds:NONE:`](#station_optionsglobalrnx_code_conversionsbdsNONE)
- [`station_options:global:rnx_phase_conversions:bds:NONE:`](#station_optionsglobalrnx_phase_conversionsbdsNONE)
- [`station_options:global:rnx_code_conversions:bds:P1:`](#station_optionsglobalrnx_code_conversionsbdsP1)
- [`station_options:global:rnx_phase_conversions:bds:P1:`](#station_optionsglobalrnx_phase_conversionsbdsP1)
- [`station_options:global:rnx_code_conversions:bds:P2:`](#station_optionsglobalrnx_code_conversionsbdsP2)
- [`station_options:global:rnx_phase_conversions:bds:P2:`](#station_optionsglobalrnx_phase_conversionsbdsP2)
- [`station_options:global:rnx_code_conversions:bds:C1:`](#station_optionsglobalrnx_code_conversionsbdsC1)
- [`station_options:global:rnx_phase_conversions:bds:C1:`](#station_optionsglobalrnx_phase_conversionsbdsC1)
- [`station_options:global:rnx_code_conversions:bds:C2:`](#station_optionsglobalrnx_code_conversionsbdsC2)
- [`station_options:global:rnx_phase_conversions:bds:C2:`](#station_optionsglobalrnx_phase_conversionsbdsC2)
- [`station_options:global:rnx_code_conversions:bds:C3:`](#station_optionsglobalrnx_code_conversionsbdsC3)
- [`station_options:global:rnx_phase_conversions:bds:C3:`](#station_optionsglobalrnx_phase_conversionsbdsC3)
- [`station_options:global:rnx_code_conversions:bds:C4:`](#station_optionsglobalrnx_code_conversionsbdsC4)
- [`station_options:global:rnx_phase_conversions:bds:C4:`](#station_optionsglobalrnx_phase_conversionsbdsC4)
- [`station_options:global:rnx_code_conversions:bds:C5:`](#station_optionsglobalrnx_code_conversionsbdsC5)
- [`station_options:global:rnx_phase_conversions:bds:C5:`](#station_optionsglobalrnx_phase_conversionsbdsC5)
- [`station_options:global:rnx_code_conversions:bds:C6:`](#station_optionsglobalrnx_code_conversionsbdsC6)
- [`station_options:global:rnx_phase_conversions:bds:C6:`](#station_optionsglobalrnx_phase_conversionsbdsC6)
- [`station_options:global:rnx_code_conversions:bds:C7:`](#station_optionsglobalrnx_code_conversionsbdsC7)
- [`station_options:global:rnx_phase_conversions:bds:C7:`](#station_optionsglobalrnx_phase_conversionsbdsC7)
- [`station_options:global:rnx_code_conversions:bds:C8:`](#station_optionsglobalrnx_code_conversionsbdsC8)
- [`station_options:global:rnx_phase_conversions:bds:C8:`](#station_optionsglobalrnx_phase_conversionsbdsC8)
- [`station_options:global:rnx_code_conversions:bds:L1:`](#station_optionsglobalrnx_code_conversionsbdsL1)
- [`station_options:global:rnx_phase_conversions:bds:L1:`](#station_optionsglobalrnx_phase_conversionsbdsL1)
- [`station_options:global:rnx_code_conversions:bds:L2:`](#station_optionsglobalrnx_code_conversionsbdsL2)
- [`station_options:global:rnx_phase_conversions:bds:L2:`](#station_optionsglobalrnx_phase_conversionsbdsL2)
- [`station_options:global:rnx_code_conversions:bds:L3:`](#station_optionsglobalrnx_code_conversionsbdsL3)
- [`station_options:global:rnx_phase_conversions:bds:L3:`](#station_optionsglobalrnx_phase_conversionsbdsL3)
- [`station_options:global:rnx_code_conversions:bds:L4:`](#station_optionsglobalrnx_code_conversionsbdsL4)
- [`station_options:global:rnx_phase_conversions:bds:L4:`](#station_optionsglobalrnx_phase_conversionsbdsL4)
- [`station_options:global:rnx_code_conversions:bds:L5:`](#station_optionsglobalrnx_code_conversionsbdsL5)
- [`station_options:global:rnx_phase_conversions:bds:L5:`](#station_optionsglobalrnx_phase_conversionsbdsL5)
- [`station_options:global:rnx_code_conversions:bds:L6:`](#station_optionsglobalrnx_code_conversionsbdsL6)
- [`station_options:global:rnx_phase_conversions:bds:L6:`](#station_optionsglobalrnx_phase_conversionsbdsL6)
- [`station_options:global:rnx_code_conversions:bds:L7:`](#station_optionsglobalrnx_code_conversionsbdsL7)
- [`station_options:global:rnx_phase_conversions:bds:L7:`](#station_optionsglobalrnx_phase_conversionsbdsL7)
- [`station_options:global:rnx_code_conversions:bds:L8:`](#station_optionsglobalrnx_code_conversionsbdsL8)
- [`station_options:global:rnx_phase_conversions:bds:L8:`](#station_optionsglobalrnx_phase_conversionsbdsL8)
- [`station_options:global:rnx_code_conversions:bds:LA:`](#station_optionsglobalrnx_code_conversionsbdsLA)
- [`station_options:global:rnx_phase_conversions:bds:LA:`](#station_optionsglobalrnx_phase_conversionsbdsLA)
- [`station_options:global:rnx_code_conversions:leo:NONE:`](#station_optionsglobalrnx_code_conversionsleoNONE)
- [`station_options:global:rnx_phase_conversions:leo:NONE:`](#station_optionsglobalrnx_phase_conversionsleoNONE)
- [`station_options:global:rnx_code_conversions:leo:P1:`](#station_optionsglobalrnx_code_conversionsleoP1)
- [`station_options:global:rnx_phase_conversions:leo:P1:`](#station_optionsglobalrnx_phase_conversionsleoP1)
- [`station_options:global:rnx_code_conversions:leo:P2:`](#station_optionsglobalrnx_code_conversionsleoP2)
- [`station_options:global:rnx_phase_conversions:leo:P2:`](#station_optionsglobalrnx_phase_conversionsleoP2)
- [`station_options:global:rnx_code_conversions:leo:C1:`](#station_optionsglobalrnx_code_conversionsleoC1)
- [`station_options:global:rnx_phase_conversions:leo:C1:`](#station_optionsglobalrnx_phase_conversionsleoC1)
- [`station_options:global:rnx_code_conversions:leo:C2:`](#station_optionsglobalrnx_code_conversionsleoC2)
- [`station_options:global:rnx_phase_conversions:leo:C2:`](#station_optionsglobalrnx_phase_conversionsleoC2)
- [`station_options:global:rnx_code_conversions:leo:C3:`](#station_optionsglobalrnx_code_conversionsleoC3)
- [`station_options:global:rnx_phase_conversions:leo:C3:`](#station_optionsglobalrnx_phase_conversionsleoC3)
- [`station_options:global:rnx_code_conversions:leo:C4:`](#station_optionsglobalrnx_code_conversionsleoC4)
- [`station_options:global:rnx_phase_conversions:leo:C4:`](#station_optionsglobalrnx_phase_conversionsleoC4)
- [`station_options:global:rnx_code_conversions:leo:C5:`](#station_optionsglobalrnx_code_conversionsleoC5)
- [`station_options:global:rnx_phase_conversions:leo:C5:`](#station_optionsglobalrnx_phase_conversionsleoC5)
- [`station_options:global:rnx_code_conversions:leo:C6:`](#station_optionsglobalrnx_code_conversionsleoC6)
- [`station_options:global:rnx_phase_conversions:leo:C6:`](#station_optionsglobalrnx_phase_conversionsleoC6)
- [`station_options:global:rnx_code_conversions:leo:C7:`](#station_optionsglobalrnx_code_conversionsleoC7)
- [`station_options:global:rnx_phase_conversions:leo:C7:`](#station_optionsglobalrnx_phase_conversionsleoC7)
- [`station_options:global:rnx_code_conversions:leo:C8:`](#station_optionsglobalrnx_code_conversionsleoC8)
- [`station_options:global:rnx_phase_conversions:leo:C8:`](#station_optionsglobalrnx_phase_conversionsleoC8)
- [`station_options:global:rnx_code_conversions:leo:L1:`](#station_optionsglobalrnx_code_conversionsleoL1)
- [`station_options:global:rnx_phase_conversions:leo:L1:`](#station_optionsglobalrnx_phase_conversionsleoL1)
- [`station_options:global:rnx_code_conversions:leo:L2:`](#station_optionsglobalrnx_code_conversionsleoL2)
- [`station_options:global:rnx_phase_conversions:leo:L2:`](#station_optionsglobalrnx_phase_conversionsleoL2)
- [`station_options:global:rnx_code_conversions:leo:L3:`](#station_optionsglobalrnx_code_conversionsleoL3)
- [`station_options:global:rnx_phase_conversions:leo:L3:`](#station_optionsglobalrnx_phase_conversionsleoL3)
- [`station_options:global:rnx_code_conversions:leo:L4:`](#station_optionsglobalrnx_code_conversionsleoL4)
- [`station_options:global:rnx_phase_conversions:leo:L4:`](#station_optionsglobalrnx_phase_conversionsleoL4)
- [`station_options:global:rnx_code_conversions:leo:L5:`](#station_optionsglobalrnx_code_conversionsleoL5)
- [`station_options:global:rnx_phase_conversions:leo:L5:`](#station_optionsglobalrnx_phase_conversionsleoL5)
- [`station_options:global:rnx_code_conversions:leo:L6:`](#station_optionsglobalrnx_code_conversionsleoL6)
- [`station_options:global:rnx_phase_conversions:leo:L6:`](#station_optionsglobalrnx_phase_conversionsleoL6)
- [`station_options:global:rnx_code_conversions:leo:L7:`](#station_optionsglobalrnx_code_conversionsleoL7)
- [`station_options:global:rnx_phase_conversions:leo:L7:`](#station_optionsglobalrnx_phase_conversionsleoL7)
- [`station_options:global:rnx_code_conversions:leo:L8:`](#station_optionsglobalrnx_code_conversionsleoL8)
- [`station_options:global:rnx_phase_conversions:leo:L8:`](#station_optionsglobalrnx_phase_conversionsleoL8)
- [`station_options:global:rnx_code_conversions:leo:LA:`](#station_optionsglobalrnx_code_conversionsleoLA)
- [`station_options:global:rnx_phase_conversions:leo:LA:`](#station_optionsglobalrnx_phase_conversionsleoLA)
- [`station_options:global:rnx_code_conversions:gps:NONE:`](#station_optionsglobalrnx_code_conversionsgpsNONE)
- [`station_options:global:rnx_phase_conversions:gps:NONE:`](#station_optionsglobalrnx_phase_conversionsgpsNONE)
- [`station_options:global:rnx_code_conversions:gps:P1:`](#station_optionsglobalrnx_code_conversionsgpsP1)
- [`station_options:global:rnx_phase_conversions:gps:P1:`](#station_optionsglobalrnx_phase_conversionsgpsP1)
- [`station_options:global:rnx_code_conversions:gps:P2:`](#station_optionsglobalrnx_code_conversionsgpsP2)
- [`station_options:global:rnx_phase_conversions:gps:P2:`](#station_optionsglobalrnx_phase_conversionsgpsP2)
- [`station_options:global:rnx_code_conversions:gps:C1:`](#station_optionsglobalrnx_code_conversionsgpsC1)
- [`station_options:global:rnx_phase_conversions:gps:C1:`](#station_optionsglobalrnx_phase_conversionsgpsC1)
- [`station_options:global:rnx_code_conversions:gps:C2:`](#station_optionsglobalrnx_code_conversionsgpsC2)
- [`station_options:global:rnx_phase_conversions:gps:C2:`](#station_optionsglobalrnx_phase_conversionsgpsC2)
- [`station_options:global:rnx_code_conversions:gps:C3:`](#station_optionsglobalrnx_code_conversionsgpsC3)
- [`station_options:global:rnx_phase_conversions:gps:C3:`](#station_optionsglobalrnx_phase_conversionsgpsC3)
- [`station_options:global:rnx_code_conversions:gps:C4:`](#station_optionsglobalrnx_code_conversionsgpsC4)
- [`station_options:global:rnx_phase_conversions:gps:C4:`](#station_optionsglobalrnx_phase_conversionsgpsC4)
- [`station_options:global:rnx_code_conversions:gps:C5:`](#station_optionsglobalrnx_code_conversionsgpsC5)
- [`station_options:global:rnx_phase_conversions:gps:C5:`](#station_optionsglobalrnx_phase_conversionsgpsC5)
- [`station_options:global:rnx_code_conversions:gps:C6:`](#station_optionsglobalrnx_code_conversionsgpsC6)
- [`station_options:global:rnx_phase_conversions:gps:C6:`](#station_optionsglobalrnx_phase_conversionsgpsC6)
- [`station_options:global:rnx_code_conversions:gps:C7:`](#station_optionsglobalrnx_code_conversionsgpsC7)
- [`station_options:global:rnx_phase_conversions:gps:C7:`](#station_optionsglobalrnx_phase_conversionsgpsC7)
- [`station_options:global:rnx_code_conversions:gps:C8:`](#station_optionsglobalrnx_code_conversionsgpsC8)
- [`station_options:global:rnx_phase_conversions:gps:C8:`](#station_optionsglobalrnx_phase_conversionsgpsC8)
- [`station_options:global:rnx_code_conversions:gps:L1:`](#station_optionsglobalrnx_code_conversionsgpsL1)
- [`station_options:global:rnx_phase_conversions:gps:L1:`](#station_optionsglobalrnx_phase_conversionsgpsL1)
- [`station_options:global:rnx_code_conversions:gps:L2:`](#station_optionsglobalrnx_code_conversionsgpsL2)
- [`station_options:global:rnx_phase_conversions:gps:L2:`](#station_optionsglobalrnx_phase_conversionsgpsL2)
- [`station_options:global:rnx_code_conversions:gps:L3:`](#station_optionsglobalrnx_code_conversionsgpsL3)
- [`station_options:global:rnx_phase_conversions:gps:L3:`](#station_optionsglobalrnx_phase_conversionsgpsL3)
- [`station_options:global:rnx_code_conversions:gps:L4:`](#station_optionsglobalrnx_code_conversionsgpsL4)
- [`station_options:global:rnx_phase_conversions:gps:L4:`](#station_optionsglobalrnx_phase_conversionsgpsL4)
- [`station_options:global:rnx_code_conversions:gps:L5:`](#station_optionsglobalrnx_code_conversionsgpsL5)
- [`station_options:global:rnx_phase_conversions:gps:L5:`](#station_optionsglobalrnx_phase_conversionsgpsL5)
- [`station_options:global:rnx_code_conversions:gps:L6:`](#station_optionsglobalrnx_code_conversionsgpsL6)
- [`station_options:global:rnx_phase_conversions:gps:L6:`](#station_optionsglobalrnx_phase_conversionsgpsL6)
- [`station_options:global:rnx_code_conversions:gps:L7:`](#station_optionsglobalrnx_code_conversionsgpsL7)
- [`station_options:global:rnx_phase_conversions:gps:L7:`](#station_optionsglobalrnx_phase_conversionsgpsL7)
- [`station_options:global:rnx_code_conversions:gps:L8:`](#station_optionsglobalrnx_code_conversionsgpsL8)
- [`station_options:global:rnx_phase_conversions:gps:L8:`](#station_optionsglobalrnx_phase_conversionsgpsL8)
- [`station_options:global:rnx_code_conversions:gps:LA:`](#station_optionsglobalrnx_code_conversionsgpsLA)
- [`station_options:global:rnx_phase_conversions:gps:LA:`](#station_optionsglobalrnx_phase_conversionsgpsLA)
- [`station_options:global:rnx_code_conversions:gal:NONE:`](#station_optionsglobalrnx_code_conversionsgalNONE)
- [`station_options:global:rnx_phase_conversions:gal:NONE:`](#station_optionsglobalrnx_phase_conversionsgalNONE)
- [`station_options:global:rnx_code_conversions:gal:P1:`](#station_optionsglobalrnx_code_conversionsgalP1)
- [`station_options:global:rnx_phase_conversions:gal:P1:`](#station_optionsglobalrnx_phase_conversionsgalP1)
- [`station_options:global:rnx_code_conversions:gal:P2:`](#station_optionsglobalrnx_code_conversionsgalP2)
- [`station_options:global:rnx_phase_conversions:gal:P2:`](#station_optionsglobalrnx_phase_conversionsgalP2)
- [`station_options:global:rnx_code_conversions:gal:C1:`](#station_optionsglobalrnx_code_conversionsgalC1)
- [`station_options:global:rnx_phase_conversions:gal:C1:`](#station_optionsglobalrnx_phase_conversionsgalC1)
- [`station_options:global:rnx_code_conversions:gal:C2:`](#station_optionsglobalrnx_code_conversionsgalC2)
- [`station_options:global:rnx_phase_conversions:gal:C2:`](#station_optionsglobalrnx_phase_conversionsgalC2)
- [`station_options:global:rnx_code_conversions:gal:C3:`](#station_optionsglobalrnx_code_conversionsgalC3)
- [`station_options:global:rnx_phase_conversions:gal:C3:`](#station_optionsglobalrnx_phase_conversionsgalC3)
- [`station_options:global:rnx_code_conversions:gal:C4:`](#station_optionsglobalrnx_code_conversionsgalC4)
- [`station_options:global:rnx_phase_conversions:gal:C4:`](#station_optionsglobalrnx_phase_conversionsgalC4)
- [`station_options:global:rnx_code_conversions:gal:C5:`](#station_optionsglobalrnx_code_conversionsgalC5)
- [`station_options:global:rnx_phase_conversions:gal:C5:`](#station_optionsglobalrnx_phase_conversionsgalC5)
- [`station_options:global:rnx_code_conversions:gal:C6:`](#station_optionsglobalrnx_code_conversionsgalC6)
- [`station_options:global:rnx_phase_conversions:gal:C6:`](#station_optionsglobalrnx_phase_conversionsgalC6)
- [`station_options:global:rnx_code_conversions:gal:C7:`](#station_optionsglobalrnx_code_conversionsgalC7)
- [`station_options:global:rnx_phase_conversions:gal:C7:`](#station_optionsglobalrnx_phase_conversionsgalC7)
- [`station_options:global:rnx_code_conversions:gal:C8:`](#station_optionsglobalrnx_code_conversionsgalC8)
- [`station_options:global:rnx_phase_conversions:gal:C8:`](#station_optionsglobalrnx_phase_conversionsgalC8)
- [`station_options:global:rnx_code_conversions:gal:L1:`](#station_optionsglobalrnx_code_conversionsgalL1)
- [`station_options:global:rnx_phase_conversions:gal:L1:`](#station_optionsglobalrnx_phase_conversionsgalL1)
- [`station_options:global:rnx_code_conversions:gal:L2:`](#station_optionsglobalrnx_code_conversionsgalL2)
- [`station_options:global:rnx_phase_conversions:gal:L2:`](#station_optionsglobalrnx_phase_conversionsgalL2)
- [`station_options:global:rnx_code_conversions:gal:L3:`](#station_optionsglobalrnx_code_conversionsgalL3)
- [`station_options:global:rnx_phase_conversions:gal:L3:`](#station_optionsglobalrnx_phase_conversionsgalL3)
- [`station_options:global:rnx_code_conversions:gal:L4:`](#station_optionsglobalrnx_code_conversionsgalL4)
- [`station_options:global:rnx_phase_conversions:gal:L4:`](#station_optionsglobalrnx_phase_conversionsgalL4)
- [`station_options:global:rnx_code_conversions:gal:L5:`](#station_optionsglobalrnx_code_conversionsgalL5)
- [`station_options:global:rnx_phase_conversions:gal:L5:`](#station_optionsglobalrnx_phase_conversionsgalL5)
- [`station_options:global:rnx_code_conversions:gal:L6:`](#station_optionsglobalrnx_code_conversionsgalL6)
- [`station_options:global:rnx_phase_conversions:gal:L6:`](#station_optionsglobalrnx_phase_conversionsgalL6)
- [`station_options:global:rnx_code_conversions:gal:L7:`](#station_optionsglobalrnx_code_conversionsgalL7)
- [`station_options:global:rnx_phase_conversions:gal:L7:`](#station_optionsglobalrnx_phase_conversionsgalL7)
- [`station_options:global:rnx_code_conversions:gal:L8:`](#station_optionsglobalrnx_code_conversionsgalL8)
- [`station_options:global:rnx_phase_conversions:gal:L8:`](#station_optionsglobalrnx_phase_conversionsgalL8)
- [`station_options:global:rnx_code_conversions:gal:LA:`](#station_optionsglobalrnx_code_conversionsgalLA)
- [`station_options:global:rnx_phase_conversions:gal:LA:`](#station_optionsglobalrnx_phase_conversionsgalLA)
- [`station_options:global:rnx_code_conversions:glo:NONE:`](#station_optionsglobalrnx_code_conversionsgloNONE)
- [`station_options:global:rnx_phase_conversions:glo:NONE:`](#station_optionsglobalrnx_phase_conversionsgloNONE)
- [`station_options:global:rnx_code_conversions:glo:P1:`](#station_optionsglobalrnx_code_conversionsgloP1)
- [`station_options:global:rnx_phase_conversions:glo:P1:`](#station_optionsglobalrnx_phase_conversionsgloP1)
- [`station_options:global:rnx_code_conversions:glo:P2:`](#station_optionsglobalrnx_code_conversionsgloP2)
- [`station_options:global:rnx_phase_conversions:glo:P2:`](#station_optionsglobalrnx_phase_conversionsgloP2)
- [`station_options:global:rnx_code_conversions:glo:C1:`](#station_optionsglobalrnx_code_conversionsgloC1)
- [`station_options:global:rnx_phase_conversions:glo:C1:`](#station_optionsglobalrnx_phase_conversionsgloC1)
- [`station_options:global:rnx_code_conversions:glo:C2:`](#station_optionsglobalrnx_code_conversionsgloC2)
- [`station_options:global:rnx_phase_conversions:glo:C2:`](#station_optionsglobalrnx_phase_conversionsgloC2)
- [`station_options:global:rnx_code_conversions:glo:C3:`](#station_optionsglobalrnx_code_conversionsgloC3)
- [`station_options:global:rnx_phase_conversions:glo:C3:`](#station_optionsglobalrnx_phase_conversionsgloC3)
- [`station_options:global:rnx_code_conversions:glo:C4:`](#station_optionsglobalrnx_code_conversionsgloC4)
- [`station_options:global:rnx_phase_conversions:glo:C4:`](#station_optionsglobalrnx_phase_conversionsgloC4)
- [`station_options:global:rnx_code_conversions:glo:C5:`](#station_optionsglobalrnx_code_conversionsgloC5)
- [`station_options:global:rnx_phase_conversions:glo:C5:`](#station_optionsglobalrnx_phase_conversionsgloC5)
- [`station_options:global:rnx_code_conversions:glo:C6:`](#station_optionsglobalrnx_code_conversionsgloC6)
- [`station_options:global:rnx_phase_conversions:glo:C6:`](#station_optionsglobalrnx_phase_conversionsgloC6)
- [`station_options:global:rnx_code_conversions:glo:C7:`](#station_optionsglobalrnx_code_conversionsgloC7)
- [`station_options:global:rnx_phase_conversions:glo:C7:`](#station_optionsglobalrnx_phase_conversionsgloC7)
- [`station_options:global:rnx_code_conversions:glo:C8:`](#station_optionsglobalrnx_code_conversionsgloC8)
- [`station_options:global:rnx_phase_conversions:glo:C8:`](#station_optionsglobalrnx_phase_conversionsgloC8)
- [`station_options:global:rnx_code_conversions:glo:L1:`](#station_optionsglobalrnx_code_conversionsgloL1)
- [`station_options:global:rnx_phase_conversions:glo:L1:`](#station_optionsglobalrnx_phase_conversionsgloL1)
- [`station_options:global:rnx_code_conversions:glo:L2:`](#station_optionsglobalrnx_code_conversionsgloL2)
- [`station_options:global:rnx_phase_conversions:glo:L2:`](#station_optionsglobalrnx_phase_conversionsgloL2)
- [`station_options:global:rnx_code_conversions:glo:L3:`](#station_optionsglobalrnx_code_conversionsgloL3)
- [`station_options:global:rnx_phase_conversions:glo:L3:`](#station_optionsglobalrnx_phase_conversionsgloL3)
- [`station_options:global:rnx_code_conversions:glo:L4:`](#station_optionsglobalrnx_code_conversionsgloL4)
- [`station_options:global:rnx_phase_conversions:glo:L4:`](#station_optionsglobalrnx_phase_conversionsgloL4)
- [`station_options:global:rnx_code_conversions:glo:L5:`](#station_optionsglobalrnx_code_conversionsgloL5)
- [`station_options:global:rnx_phase_conversions:glo:L5:`](#station_optionsglobalrnx_phase_conversionsgloL5)
- [`station_options:global:rnx_code_conversions:glo:L6:`](#station_optionsglobalrnx_code_conversionsgloL6)
- [`station_options:global:rnx_phase_conversions:glo:L6:`](#station_optionsglobalrnx_phase_conversionsgloL6)
- [`station_options:global:rnx_code_conversions:glo:L7:`](#station_optionsglobalrnx_code_conversionsgloL7)
- [`station_options:global:rnx_phase_conversions:glo:L7:`](#station_optionsglobalrnx_phase_conversionsgloL7)
- [`station_options:global:rnx_code_conversions:glo:L8:`](#station_optionsglobalrnx_code_conversionsgloL8)
- [`station_options:global:rnx_phase_conversions:glo:L8:`](#station_optionsglobalrnx_phase_conversionsgloL8)
- [`station_options:global:rnx_code_conversions:glo:LA:`](#station_optionsglobalrnx_code_conversionsgloLA)
- [`station_options:global:rnx_phase_conversions:glo:LA:`](#station_optionsglobalrnx_phase_conversionsgloLA)
- [`station_options:global:rnx_code_conversions:qzs:NONE:`](#station_optionsglobalrnx_code_conversionsqzsNONE)
- [`station_options:global:rnx_phase_conversions:qzs:NONE:`](#station_optionsglobalrnx_phase_conversionsqzsNONE)
- [`station_options:global:rnx_code_conversions:qzs:P1:`](#station_optionsglobalrnx_code_conversionsqzsP1)
- [`station_options:global:rnx_phase_conversions:qzs:P1:`](#station_optionsglobalrnx_phase_conversionsqzsP1)
- [`station_options:global:rnx_code_conversions:qzs:P2:`](#station_optionsglobalrnx_code_conversionsqzsP2)
- [`station_options:global:rnx_phase_conversions:qzs:P2:`](#station_optionsglobalrnx_phase_conversionsqzsP2)
- [`station_options:global:rnx_code_conversions:qzs:C1:`](#station_optionsglobalrnx_code_conversionsqzsC1)
- [`station_options:global:rnx_phase_conversions:qzs:C1:`](#station_optionsglobalrnx_phase_conversionsqzsC1)
- [`station_options:global:rnx_code_conversions:qzs:C2:`](#station_optionsglobalrnx_code_conversionsqzsC2)
- [`station_options:global:rnx_phase_conversions:qzs:C2:`](#station_optionsglobalrnx_phase_conversionsqzsC2)
- [`station_options:global:rnx_code_conversions:qzs:C3:`](#station_optionsglobalrnx_code_conversionsqzsC3)
- [`station_options:global:rnx_phase_conversions:qzs:C3:`](#station_optionsglobalrnx_phase_conversionsqzsC3)
- [`station_options:global:rnx_code_conversions:qzs:C4:`](#station_optionsglobalrnx_code_conversionsqzsC4)
- [`station_options:global:rnx_phase_conversions:qzs:C4:`](#station_optionsglobalrnx_phase_conversionsqzsC4)
- [`station_options:global:rnx_code_conversions:qzs:C5:`](#station_optionsglobalrnx_code_conversionsqzsC5)
- [`station_options:global:rnx_phase_conversions:qzs:C5:`](#station_optionsglobalrnx_phase_conversionsqzsC5)
- [`station_options:global:rnx_code_conversions:qzs:C6:`](#station_optionsglobalrnx_code_conversionsqzsC6)
- [`station_options:global:rnx_phase_conversions:qzs:C6:`](#station_optionsglobalrnx_phase_conversionsqzsC6)
- [`station_options:global:rnx_code_conversions:qzs:C7:`](#station_optionsglobalrnx_code_conversionsqzsC7)
- [`station_options:global:rnx_phase_conversions:qzs:C7:`](#station_optionsglobalrnx_phase_conversionsqzsC7)
- [`station_options:global:rnx_code_conversions:qzs:C8:`](#station_optionsglobalrnx_code_conversionsqzsC8)
- [`station_options:global:rnx_phase_conversions:qzs:C8:`](#station_optionsglobalrnx_phase_conversionsqzsC8)
- [`station_options:global:rnx_code_conversions:qzs:L1:`](#station_optionsglobalrnx_code_conversionsqzsL1)
- [`station_options:global:rnx_phase_conversions:qzs:L1:`](#station_optionsglobalrnx_phase_conversionsqzsL1)
- [`station_options:global:rnx_code_conversions:qzs:L2:`](#station_optionsglobalrnx_code_conversionsqzsL2)
- [`station_options:global:rnx_phase_conversions:qzs:L2:`](#station_optionsglobalrnx_phase_conversionsqzsL2)
- [`station_options:global:rnx_code_conversions:qzs:L3:`](#station_optionsglobalrnx_code_conversionsqzsL3)
- [`station_options:global:rnx_phase_conversions:qzs:L3:`](#station_optionsglobalrnx_phase_conversionsqzsL3)
- [`station_options:global:rnx_code_conversions:qzs:L4:`](#station_optionsglobalrnx_code_conversionsqzsL4)
- [`station_options:global:rnx_phase_conversions:qzs:L4:`](#station_optionsglobalrnx_phase_conversionsqzsL4)
- [`station_options:global:rnx_code_conversions:qzs:L5:`](#station_optionsglobalrnx_code_conversionsqzsL5)
- [`station_options:global:rnx_phase_conversions:qzs:L5:`](#station_optionsglobalrnx_phase_conversionsqzsL5)
- [`station_options:global:rnx_code_conversions:qzs:L6:`](#station_optionsglobalrnx_code_conversionsqzsL6)
- [`station_options:global:rnx_phase_conversions:qzs:L6:`](#station_optionsglobalrnx_phase_conversionsqzsL6)
- [`station_options:global:rnx_code_conversions:qzs:L7:`](#station_optionsglobalrnx_code_conversionsqzsL7)
- [`station_options:global:rnx_phase_conversions:qzs:L7:`](#station_optionsglobalrnx_phase_conversionsqzsL7)
- [`station_options:global:rnx_code_conversions:qzs:L8:`](#station_optionsglobalrnx_code_conversionsqzsL8)
- [`station_options:global:rnx_phase_conversions:qzs:L8:`](#station_optionsglobalrnx_phase_conversionsqzsL8)
- [`station_options:global:rnx_code_conversions:qzs:LA:`](#station_optionsglobalrnx_code_conversionsqzsLA)
- [`station_options:global:rnx_phase_conversions:qzs:LA:`](#station_optionsglobalrnx_phase_conversionsqzsLA)
- [`station_options:global:rnx_code_conversions:sbs:NONE:`](#station_optionsglobalrnx_code_conversionssbsNONE)
- [`station_options:global:rnx_phase_conversions:sbs:NONE:`](#station_optionsglobalrnx_phase_conversionssbsNONE)
- [`station_options:global:rnx_code_conversions:sbs:P1:`](#station_optionsglobalrnx_code_conversionssbsP1)
- [`station_options:global:rnx_phase_conversions:sbs:P1:`](#station_optionsglobalrnx_phase_conversionssbsP1)
- [`station_options:global:rnx_code_conversions:sbs:P2:`](#station_optionsglobalrnx_code_conversionssbsP2)
- [`station_options:global:rnx_phase_conversions:sbs:P2:`](#station_optionsglobalrnx_phase_conversionssbsP2)
- [`station_options:global:rnx_code_conversions:sbs:C1:`](#station_optionsglobalrnx_code_conversionssbsC1)
- [`station_options:global:rnx_phase_conversions:sbs:C1:`](#station_optionsglobalrnx_phase_conversionssbsC1)
- [`station_options:global:rnx_code_conversions:sbs:C2:`](#station_optionsglobalrnx_code_conversionssbsC2)
- [`station_options:global:rnx_phase_conversions:sbs:C2:`](#station_optionsglobalrnx_phase_conversionssbsC2)
- [`station_options:global:rnx_code_conversions:sbs:C3:`](#station_optionsglobalrnx_code_conversionssbsC3)
- [`station_options:global:rnx_phase_conversions:sbs:C3:`](#station_optionsglobalrnx_phase_conversionssbsC3)
- [`station_options:global:rnx_code_conversions:sbs:C4:`](#station_optionsglobalrnx_code_conversionssbsC4)
- [`station_options:global:rnx_phase_conversions:sbs:C4:`](#station_optionsglobalrnx_phase_conversionssbsC4)
- [`station_options:global:rnx_code_conversions:sbs:C5:`](#station_optionsglobalrnx_code_conversionssbsC5)
- [`station_options:global:rnx_phase_conversions:sbs:C5:`](#station_optionsglobalrnx_phase_conversionssbsC5)
- [`station_options:global:rnx_code_conversions:sbs:C6:`](#station_optionsglobalrnx_code_conversionssbsC6)
- [`station_options:global:rnx_phase_conversions:sbs:C6:`](#station_optionsglobalrnx_phase_conversionssbsC6)
- [`station_options:global:rnx_code_conversions:sbs:C7:`](#station_optionsglobalrnx_code_conversionssbsC7)
- [`station_options:global:rnx_phase_conversions:sbs:C7:`](#station_optionsglobalrnx_phase_conversionssbsC7)
- [`station_options:global:rnx_code_conversions:sbs:C8:`](#station_optionsglobalrnx_code_conversionssbsC8)
- [`station_options:global:rnx_phase_conversions:sbs:C8:`](#station_optionsglobalrnx_phase_conversionssbsC8)
- [`station_options:global:rnx_code_conversions:sbs:L1:`](#station_optionsglobalrnx_code_conversionssbsL1)
- [`station_options:global:rnx_phase_conversions:sbs:L1:`](#station_optionsglobalrnx_phase_conversionssbsL1)
- [`station_options:global:rnx_code_conversions:sbs:L2:`](#station_optionsglobalrnx_code_conversionssbsL2)
- [`station_options:global:rnx_phase_conversions:sbs:L2:`](#station_optionsglobalrnx_phase_conversionssbsL2)
- [`station_options:global:rnx_code_conversions:sbs:L3:`](#station_optionsglobalrnx_code_conversionssbsL3)
- [`station_options:global:rnx_phase_conversions:sbs:L3:`](#station_optionsglobalrnx_phase_conversionssbsL3)
- [`station_options:global:rnx_code_conversions:sbs:L4:`](#station_optionsglobalrnx_code_conversionssbsL4)
- [`station_options:global:rnx_phase_conversions:sbs:L4:`](#station_optionsglobalrnx_phase_conversionssbsL4)
- [`station_options:global:rnx_code_conversions:sbs:L5:`](#station_optionsglobalrnx_code_conversionssbsL5)
- [`station_options:global:rnx_phase_conversions:sbs:L5:`](#station_optionsglobalrnx_phase_conversionssbsL5)
- [`station_options:global:rnx_code_conversions:sbs:L6:`](#station_optionsglobalrnx_code_conversionssbsL6)
- [`station_options:global:rnx_phase_conversions:sbs:L6:`](#station_optionsglobalrnx_phase_conversionssbsL6)
- [`station_options:global:rnx_code_conversions:sbs:L7:`](#station_optionsglobalrnx_code_conversionssbsL7)
- [`station_options:global:rnx_phase_conversions:sbs:L7:`](#station_optionsglobalrnx_phase_conversionssbsL7)
- [`station_options:global:rnx_code_conversions:sbs:L8:`](#station_optionsglobalrnx_code_conversionssbsL8)
- [`station_options:global:rnx_phase_conversions:sbs:L8:`](#station_optionsglobalrnx_phase_conversionssbsL8)
- [`station_options:global:rnx_code_conversions:sbs:LA:`](#station_optionsglobalrnx_code_conversionssbsLA)
- [`station_options:global:rnx_phase_conversions:sbs:LA:`](#station_optionsglobalrnx_phase_conversionssbsLA)
- [`station_options:global:rnx_code_conversions:bds:NONE:`](#station_optionsglobalrnx_code_conversionsbdsNONE)
- [`station_options:global:rnx_phase_conversions:bds:NONE:`](#station_optionsglobalrnx_phase_conversionsbdsNONE)
- [`station_options:global:rnx_code_conversions:bds:P1:`](#station_optionsglobalrnx_code_conversionsbdsP1)
- [`station_options:global:rnx_phase_conversions:bds:P1:`](#station_optionsglobalrnx_phase_conversionsbdsP1)
- [`station_options:global:rnx_code_conversions:bds:P2:`](#station_optionsglobalrnx_code_conversionsbdsP2)
- [`station_options:global:rnx_phase_conversions:bds:P2:`](#station_optionsglobalrnx_phase_conversionsbdsP2)
- [`station_options:global:rnx_code_conversions:bds:C1:`](#station_optionsglobalrnx_code_conversionsbdsC1)
- [`station_options:global:rnx_phase_conversions:bds:C1:`](#station_optionsglobalrnx_phase_conversionsbdsC1)
- [`station_options:global:rnx_code_conversions:bds:C2:`](#station_optionsglobalrnx_code_conversionsbdsC2)
- [`station_options:global:rnx_phase_conversions:bds:C2:`](#station_optionsglobalrnx_phase_conversionsbdsC2)
- [`station_options:global:rnx_code_conversions:bds:C3:`](#station_optionsglobalrnx_code_conversionsbdsC3)
- [`station_options:global:rnx_phase_conversions:bds:C3:`](#station_optionsglobalrnx_phase_conversionsbdsC3)
- [`station_options:global:rnx_code_conversions:bds:C4:`](#station_optionsglobalrnx_code_conversionsbdsC4)
- [`station_options:global:rnx_phase_conversions:bds:C4:`](#station_optionsglobalrnx_phase_conversionsbdsC4)
- [`station_options:global:rnx_code_conversions:bds:C5:`](#station_optionsglobalrnx_code_conversionsbdsC5)
- [`station_options:global:rnx_phase_conversions:bds:C5:`](#station_optionsglobalrnx_phase_conversionsbdsC5)
- [`station_options:global:rnx_code_conversions:bds:C6:`](#station_optionsglobalrnx_code_conversionsbdsC6)
- [`station_options:global:rnx_phase_conversions:bds:C6:`](#station_optionsglobalrnx_phase_conversionsbdsC6)
- [`station_options:global:rnx_code_conversions:bds:C7:`](#station_optionsglobalrnx_code_conversionsbdsC7)
- [`station_options:global:rnx_phase_conversions:bds:C7:`](#station_optionsglobalrnx_phase_conversionsbdsC7)
- [`station_options:global:rnx_code_conversions:bds:C8:`](#station_optionsglobalrnx_code_conversionsbdsC8)
- [`station_options:global:rnx_phase_conversions:bds:C8:`](#station_optionsglobalrnx_phase_conversionsbdsC8)
- [`station_options:global:rnx_code_conversions:bds:L1:`](#station_optionsglobalrnx_code_conversionsbdsL1)
- [`station_options:global:rnx_phase_conversions:bds:L1:`](#station_optionsglobalrnx_phase_conversionsbdsL1)
- [`station_options:global:rnx_code_conversions:bds:L2:`](#station_optionsglobalrnx_code_conversionsbdsL2)
- [`station_options:global:rnx_phase_conversions:bds:L2:`](#station_optionsglobalrnx_phase_conversionsbdsL2)
- [`station_options:global:rnx_code_conversions:bds:L3:`](#station_optionsglobalrnx_code_conversionsbdsL3)
- [`station_options:global:rnx_phase_conversions:bds:L3:`](#station_optionsglobalrnx_phase_conversionsbdsL3)
- [`station_options:global:rnx_code_conversions:bds:L4:`](#station_optionsglobalrnx_code_conversionsbdsL4)
- [`station_options:global:rnx_phase_conversions:bds:L4:`](#station_optionsglobalrnx_phase_conversionsbdsL4)
- [`station_options:global:rnx_code_conversions:bds:L5:`](#station_optionsglobalrnx_code_conversionsbdsL5)
- [`station_options:global:rnx_phase_conversions:bds:L5:`](#station_optionsglobalrnx_phase_conversionsbdsL5)
- [`station_options:global:rnx_code_conversions:bds:L6:`](#station_optionsglobalrnx_code_conversionsbdsL6)
- [`station_options:global:rnx_phase_conversions:bds:L6:`](#station_optionsglobalrnx_phase_conversionsbdsL6)
- [`station_options:global:rnx_code_conversions:bds:L7:`](#station_optionsglobalrnx_code_conversionsbdsL7)
- [`station_options:global:rnx_phase_conversions:bds:L7:`](#station_optionsglobalrnx_phase_conversionsbdsL7)
- [`station_options:global:rnx_code_conversions:bds:L8:`](#station_optionsglobalrnx_code_conversionsbdsL8)
- [`station_options:global:rnx_phase_conversions:bds:L8:`](#station_optionsglobalrnx_phase_conversionsbdsL8)
- [`station_options:global:rnx_code_conversions:bds:LA:`](#station_optionsglobalrnx_code_conversionsbdsLA)
- [`station_options:global:rnx_phase_conversions:bds:LA:`](#station_optionsglobalrnx_phase_conversionsbdsLA)
- [`station_options:global:rnx_code_conversions:leo:NONE:`](#station_optionsglobalrnx_code_conversionsleoNONE)
- [`station_options:global:rnx_phase_conversions:leo:NONE:`](#station_optionsglobalrnx_phase_conversionsleoNONE)
- [`station_options:global:rnx_code_conversions:leo:P1:`](#station_optionsglobalrnx_code_conversionsleoP1)
- [`station_options:global:rnx_phase_conversions:leo:P1:`](#station_optionsglobalrnx_phase_conversionsleoP1)
- [`station_options:global:rnx_code_conversions:leo:P2:`](#station_optionsglobalrnx_code_conversionsleoP2)
- [`station_options:global:rnx_phase_conversions:leo:P2:`](#station_optionsglobalrnx_phase_conversionsleoP2)
- [`station_options:global:rnx_code_conversions:leo:C1:`](#station_optionsglobalrnx_code_conversionsleoC1)
- [`station_options:global:rnx_phase_conversions:leo:C1:`](#station_optionsglobalrnx_phase_conversionsleoC1)
- [`station_options:global:rnx_code_conversions:leo:C2:`](#station_optionsglobalrnx_code_conversionsleoC2)
- [`station_options:global:rnx_phase_conversions:leo:C2:`](#station_optionsglobalrnx_phase_conversionsleoC2)
- [`station_options:global:rnx_code_conversions:leo:C3:`](#station_optionsglobalrnx_code_conversionsleoC3)
- [`station_options:global:rnx_phase_conversions:leo:C3:`](#station_optionsglobalrnx_phase_conversionsleoC3)
- [`station_options:global:rnx_code_conversions:leo:C4:`](#station_optionsglobalrnx_code_conversionsleoC4)
- [`station_options:global:rnx_phase_conversions:leo:C4:`](#station_optionsglobalrnx_phase_conversionsleoC4)
- [`station_options:global:rnx_code_conversions:leo:C5:`](#station_optionsglobalrnx_code_conversionsleoC5)
- [`station_options:global:rnx_phase_conversions:leo:C5:`](#station_optionsglobalrnx_phase_conversionsleoC5)
- [`station_options:global:rnx_code_conversions:leo:C6:`](#station_optionsglobalrnx_code_conversionsleoC6)
- [`station_options:global:rnx_phase_conversions:leo:C6:`](#station_optionsglobalrnx_phase_conversionsleoC6)
- [`station_options:global:rnx_code_conversions:leo:C7:`](#station_optionsglobalrnx_code_conversionsleoC7)
- [`station_options:global:rnx_phase_conversions:leo:C7:`](#station_optionsglobalrnx_phase_conversionsleoC7)
- [`station_options:global:rnx_code_conversions:leo:C8:`](#station_optionsglobalrnx_code_conversionsleoC8)
- [`station_options:global:rnx_phase_conversions:leo:C8:`](#station_optionsglobalrnx_phase_conversionsleoC8)
- [`station_options:global:rnx_code_conversions:leo:L1:`](#station_optionsglobalrnx_code_conversionsleoL1)
- [`station_options:global:rnx_phase_conversions:leo:L1:`](#station_optionsglobalrnx_phase_conversionsleoL1)
- [`station_options:global:rnx_code_conversions:leo:L2:`](#station_optionsglobalrnx_code_conversionsleoL2)
- [`station_options:global:rnx_phase_conversions:leo:L2:`](#station_optionsglobalrnx_phase_conversionsleoL2)
- [`station_options:global:rnx_code_conversions:leo:L3:`](#station_optionsglobalrnx_code_conversionsleoL3)
- [`station_options:global:rnx_phase_conversions:leo:L3:`](#station_optionsglobalrnx_phase_conversionsleoL3)
- [`station_options:global:rnx_code_conversions:leo:L4:`](#station_optionsglobalrnx_code_conversionsleoL4)
- [`station_options:global:rnx_phase_conversions:leo:L4:`](#station_optionsglobalrnx_phase_conversionsleoL4)
- [`station_options:global:rnx_code_conversions:leo:L5:`](#station_optionsglobalrnx_code_conversionsleoL5)
- [`station_options:global:rnx_phase_conversions:leo:L5:`](#station_optionsglobalrnx_phase_conversionsleoL5)
- [`station_options:global:rnx_code_conversions:leo:L6:`](#station_optionsglobalrnx_code_conversionsleoL6)
- [`station_options:global:rnx_phase_conversions:leo:L6:`](#station_optionsglobalrnx_phase_conversionsleoL6)
- [`station_options:global:rnx_code_conversions:leo:L7:`](#station_optionsglobalrnx_code_conversionsleoL7)
- [`station_options:global:rnx_phase_conversions:leo:L7:`](#station_optionsglobalrnx_phase_conversionsleoL7)
- [`station_options:global:rnx_code_conversions:leo:L8:`](#station_optionsglobalrnx_code_conversionsleoL8)
- [`station_options:global:rnx_phase_conversions:leo:L8:`](#station_optionsglobalrnx_phase_conversionsleoL8)
- [`station_options:global:rnx_code_conversions:leo:LA:`](#station_optionsglobalrnx_code_conversionsleoLA)
- [`station_options:global:rnx_phase_conversions:leo:LA:`](#station_optionsglobalrnx_phase_conversionsleoLA)
- [`station_options:XMPL:rnx_code_conversions:gps:NONE:`](#station_optionsXMPLrnx_code_conversionsgpsNONE)
- [`station_options:XMPL:rnx_phase_conversions:gps:NONE:`](#station_optionsXMPLrnx_phase_conversionsgpsNONE)
- [`station_options:XMPL:rnx_code_conversions:gps:P1:`](#station_optionsXMPLrnx_code_conversionsgpsP1)
- [`station_options:XMPL:rnx_phase_conversions:gps:P1:`](#station_optionsXMPLrnx_phase_conversionsgpsP1)
- [`station_options:XMPL:rnx_code_conversions:gps:P2:`](#station_optionsXMPLrnx_code_conversionsgpsP2)
- [`station_options:XMPL:rnx_phase_conversions:gps:P2:`](#station_optionsXMPLrnx_phase_conversionsgpsP2)
- [`station_options:XMPL:rnx_code_conversions:gps:C1:`](#station_optionsXMPLrnx_code_conversionsgpsC1)
- [`station_options:XMPL:rnx_phase_conversions:gps:C1:`](#station_optionsXMPLrnx_phase_conversionsgpsC1)
- [`station_options:XMPL:rnx_code_conversions:gps:C2:`](#station_optionsXMPLrnx_code_conversionsgpsC2)
- [`station_options:XMPL:rnx_phase_conversions:gps:C2:`](#station_optionsXMPLrnx_phase_conversionsgpsC2)
- [`station_options:XMPL:rnx_code_conversions:gps:C3:`](#station_optionsXMPLrnx_code_conversionsgpsC3)
- [`station_options:XMPL:rnx_phase_conversions:gps:C3:`](#station_optionsXMPLrnx_phase_conversionsgpsC3)
- [`station_options:XMPL:rnx_code_conversions:gps:C4:`](#station_optionsXMPLrnx_code_conversionsgpsC4)
- [`station_options:XMPL:rnx_phase_conversions:gps:C4:`](#station_optionsXMPLrnx_phase_conversionsgpsC4)
- [`station_options:XMPL:rnx_code_conversions:gps:C5:`](#station_optionsXMPLrnx_code_conversionsgpsC5)
- [`station_options:XMPL:rnx_phase_conversions:gps:C5:`](#station_optionsXMPLrnx_phase_conversionsgpsC5)
- [`station_options:XMPL:rnx_code_conversions:gps:C6:`](#station_optionsXMPLrnx_code_conversionsgpsC6)
- [`station_options:XMPL:rnx_phase_conversions:gps:C6:`](#station_optionsXMPLrnx_phase_conversionsgpsC6)
- [`station_options:XMPL:rnx_code_conversions:gps:C7:`](#station_optionsXMPLrnx_code_conversionsgpsC7)
- [`station_options:XMPL:rnx_phase_conversions:gps:C7:`](#station_optionsXMPLrnx_phase_conversionsgpsC7)
- [`station_options:XMPL:rnx_code_conversions:gps:C8:`](#station_optionsXMPLrnx_code_conversionsgpsC8)
- [`station_options:XMPL:rnx_phase_conversions:gps:C8:`](#station_optionsXMPLrnx_phase_conversionsgpsC8)
- [`station_options:XMPL:rnx_code_conversions:gps:L1:`](#station_optionsXMPLrnx_code_conversionsgpsL1)
- [`station_options:XMPL:rnx_phase_conversions:gps:L1:`](#station_optionsXMPLrnx_phase_conversionsgpsL1)
- [`station_options:XMPL:rnx_code_conversions:gps:L2:`](#station_optionsXMPLrnx_code_conversionsgpsL2)
- [`station_options:XMPL:rnx_phase_conversions:gps:L2:`](#station_optionsXMPLrnx_phase_conversionsgpsL2)
- [`station_options:XMPL:rnx_code_conversions:gps:L3:`](#station_optionsXMPLrnx_code_conversionsgpsL3)
- [`station_options:XMPL:rnx_phase_conversions:gps:L3:`](#station_optionsXMPLrnx_phase_conversionsgpsL3)
- [`station_options:XMPL:rnx_code_conversions:gps:L4:`](#station_optionsXMPLrnx_code_conversionsgpsL4)
- [`station_options:XMPL:rnx_phase_conversions:gps:L4:`](#station_optionsXMPLrnx_phase_conversionsgpsL4)
- [`station_options:XMPL:rnx_code_conversions:gps:L5:`](#station_optionsXMPLrnx_code_conversionsgpsL5)
- [`station_options:XMPL:rnx_phase_conversions:gps:L5:`](#station_optionsXMPLrnx_phase_conversionsgpsL5)
- [`station_options:XMPL:rnx_code_conversions:gps:L6:`](#station_optionsXMPLrnx_code_conversionsgpsL6)
- [`station_options:XMPL:rnx_phase_conversions:gps:L6:`](#station_optionsXMPLrnx_phase_conversionsgpsL6)
- [`station_options:XMPL:rnx_code_conversions:gps:L7:`](#station_optionsXMPLrnx_code_conversionsgpsL7)
- [`station_options:XMPL:rnx_phase_conversions:gps:L7:`](#station_optionsXMPLrnx_phase_conversionsgpsL7)
- [`station_options:XMPL:rnx_code_conversions:gps:L8:`](#station_optionsXMPLrnx_code_conversionsgpsL8)
- [`station_options:XMPL:rnx_phase_conversions:gps:L8:`](#station_optionsXMPLrnx_phase_conversionsgpsL8)
- [`station_options:XMPL:rnx_code_conversions:gps:LA:`](#station_optionsXMPLrnx_code_conversionsgpsLA)
- [`station_options:XMPL:rnx_phase_conversions:gps:LA:`](#station_optionsXMPLrnx_phase_conversionsgpsLA)
- [`station_options:XMPL:rnx_code_conversions:gal:NONE:`](#station_optionsXMPLrnx_code_conversionsgalNONE)
- [`station_options:XMPL:rnx_phase_conversions:gal:NONE:`](#station_optionsXMPLrnx_phase_conversionsgalNONE)
- [`station_options:XMPL:rnx_code_conversions:gal:P1:`](#station_optionsXMPLrnx_code_conversionsgalP1)
- [`station_options:XMPL:rnx_phase_conversions:gal:P1:`](#station_optionsXMPLrnx_phase_conversionsgalP1)
- [`station_options:XMPL:rnx_code_conversions:gal:P2:`](#station_optionsXMPLrnx_code_conversionsgalP2)
- [`station_options:XMPL:rnx_phase_conversions:gal:P2:`](#station_optionsXMPLrnx_phase_conversionsgalP2)
- [`station_options:XMPL:rnx_code_conversions:gal:C1:`](#station_optionsXMPLrnx_code_conversionsgalC1)
- [`station_options:XMPL:rnx_phase_conversions:gal:C1:`](#station_optionsXMPLrnx_phase_conversionsgalC1)
- [`station_options:XMPL:rnx_code_conversions:gal:C2:`](#station_optionsXMPLrnx_code_conversionsgalC2)
- [`station_options:XMPL:rnx_phase_conversions:gal:C2:`](#station_optionsXMPLrnx_phase_conversionsgalC2)
- [`station_options:XMPL:rnx_code_conversions:gal:C3:`](#station_optionsXMPLrnx_code_conversionsgalC3)
- [`station_options:XMPL:rnx_phase_conversions:gal:C3:`](#station_optionsXMPLrnx_phase_conversionsgalC3)
- [`station_options:XMPL:rnx_code_conversions:gal:C4:`](#station_optionsXMPLrnx_code_conversionsgalC4)
- [`station_options:XMPL:rnx_phase_conversions:gal:C4:`](#station_optionsXMPLrnx_phase_conversionsgalC4)
- [`station_options:XMPL:rnx_code_conversions:gal:C5:`](#station_optionsXMPLrnx_code_conversionsgalC5)
- [`station_options:XMPL:rnx_phase_conversions:gal:C5:`](#station_optionsXMPLrnx_phase_conversionsgalC5)
- [`station_options:XMPL:rnx_code_conversions:gal:C6:`](#station_optionsXMPLrnx_code_conversionsgalC6)
- [`station_options:XMPL:rnx_phase_conversions:gal:C6:`](#station_optionsXMPLrnx_phase_conversionsgalC6)
- [`station_options:XMPL:rnx_code_conversions:gal:C7:`](#station_optionsXMPLrnx_code_conversionsgalC7)
- [`station_options:XMPL:rnx_phase_conversions:gal:C7:`](#station_optionsXMPLrnx_phase_conversionsgalC7)
- [`station_options:XMPL:rnx_code_conversions:gal:C8:`](#station_optionsXMPLrnx_code_conversionsgalC8)
- [`station_options:XMPL:rnx_phase_conversions:gal:C8:`](#station_optionsXMPLrnx_phase_conversionsgalC8)
- [`station_options:XMPL:rnx_code_conversions:gal:L1:`](#station_optionsXMPLrnx_code_conversionsgalL1)
- [`station_options:XMPL:rnx_phase_conversions:gal:L1:`](#station_optionsXMPLrnx_phase_conversionsgalL1)
- [`station_options:XMPL:rnx_code_conversions:gal:L2:`](#station_optionsXMPLrnx_code_conversionsgalL2)
- [`station_options:XMPL:rnx_phase_conversions:gal:L2:`](#station_optionsXMPLrnx_phase_conversionsgalL2)
- [`station_options:XMPL:rnx_code_conversions:gal:L3:`](#station_optionsXMPLrnx_code_conversionsgalL3)
- [`station_options:XMPL:rnx_phase_conversions:gal:L3:`](#station_optionsXMPLrnx_phase_conversionsgalL3)
- [`station_options:XMPL:rnx_code_conversions:gal:L4:`](#station_optionsXMPLrnx_code_conversionsgalL4)
- [`station_options:XMPL:rnx_phase_conversions:gal:L4:`](#station_optionsXMPLrnx_phase_conversionsgalL4)
- [`station_options:XMPL:rnx_code_conversions:gal:L5:`](#station_optionsXMPLrnx_code_conversionsgalL5)
- [`station_options:XMPL:rnx_phase_conversions:gal:L5:`](#station_optionsXMPLrnx_phase_conversionsgalL5)
- [`station_options:XMPL:rnx_code_conversions:gal:L6:`](#station_optionsXMPLrnx_code_conversionsgalL6)
- [`station_options:XMPL:rnx_phase_conversions:gal:L6:`](#station_optionsXMPLrnx_phase_conversionsgalL6)
- [`station_options:XMPL:rnx_code_conversions:gal:L7:`](#station_optionsXMPLrnx_code_conversionsgalL7)
- [`station_options:XMPL:rnx_phase_conversions:gal:L7:`](#station_optionsXMPLrnx_phase_conversionsgalL7)
- [`station_options:XMPL:rnx_code_conversions:gal:L8:`](#station_optionsXMPLrnx_code_conversionsgalL8)
- [`station_options:XMPL:rnx_phase_conversions:gal:L8:`](#station_optionsXMPLrnx_phase_conversionsgalL8)
- [`station_options:XMPL:rnx_code_conversions:gal:LA:`](#station_optionsXMPLrnx_code_conversionsgalLA)
- [`station_options:XMPL:rnx_phase_conversions:gal:LA:`](#station_optionsXMPLrnx_phase_conversionsgalLA)
- [`station_options:XMPL:rnx_code_conversions:glo:NONE:`](#station_optionsXMPLrnx_code_conversionsgloNONE)
- [`station_options:XMPL:rnx_phase_conversions:glo:NONE:`](#station_optionsXMPLrnx_phase_conversionsgloNONE)
- [`station_options:XMPL:rnx_code_conversions:glo:P1:`](#station_optionsXMPLrnx_code_conversionsgloP1)
- [`station_options:XMPL:rnx_phase_conversions:glo:P1:`](#station_optionsXMPLrnx_phase_conversionsgloP1)
- [`station_options:XMPL:rnx_code_conversions:glo:P2:`](#station_optionsXMPLrnx_code_conversionsgloP2)
- [`station_options:XMPL:rnx_phase_conversions:glo:P2:`](#station_optionsXMPLrnx_phase_conversionsgloP2)
- [`station_options:XMPL:rnx_code_conversions:glo:C1:`](#station_optionsXMPLrnx_code_conversionsgloC1)
- [`station_options:XMPL:rnx_phase_conversions:glo:C1:`](#station_optionsXMPLrnx_phase_conversionsgloC1)
- [`station_options:XMPL:rnx_code_conversions:glo:C2:`](#station_optionsXMPLrnx_code_conversionsgloC2)
- [`station_options:XMPL:rnx_phase_conversions:glo:C2:`](#station_optionsXMPLrnx_phase_conversionsgloC2)
- [`station_options:XMPL:rnx_code_conversions:glo:C3:`](#station_optionsXMPLrnx_code_conversionsgloC3)
- [`station_options:XMPL:rnx_phase_conversions:glo:C3:`](#station_optionsXMPLrnx_phase_conversionsgloC3)
- [`station_options:XMPL:rnx_code_conversions:glo:C4:`](#station_optionsXMPLrnx_code_conversionsgloC4)
- [`station_options:XMPL:rnx_phase_conversions:glo:C4:`](#station_optionsXMPLrnx_phase_conversionsgloC4)
- [`station_options:XMPL:rnx_code_conversions:glo:C5:`](#station_optionsXMPLrnx_code_conversionsgloC5)
- [`station_options:XMPL:rnx_phase_conversions:glo:C5:`](#station_optionsXMPLrnx_phase_conversionsgloC5)
- [`station_options:XMPL:rnx_code_conversions:glo:C6:`](#station_optionsXMPLrnx_code_conversionsgloC6)
- [`station_options:XMPL:rnx_phase_conversions:glo:C6:`](#station_optionsXMPLrnx_phase_conversionsgloC6)
- [`station_options:XMPL:rnx_code_conversions:glo:C7:`](#station_optionsXMPLrnx_code_conversionsgloC7)
- [`station_options:XMPL:rnx_phase_conversions:glo:C7:`](#station_optionsXMPLrnx_phase_conversionsgloC7)
- [`station_options:XMPL:rnx_code_conversions:glo:C8:`](#station_optionsXMPLrnx_code_conversionsgloC8)
- [`station_options:XMPL:rnx_phase_conversions:glo:C8:`](#station_optionsXMPLrnx_phase_conversionsgloC8)
- [`station_options:XMPL:rnx_code_conversions:glo:L1:`](#station_optionsXMPLrnx_code_conversionsgloL1)
- [`station_options:XMPL:rnx_phase_conversions:glo:L1:`](#station_optionsXMPLrnx_phase_conversionsgloL1)
- [`station_options:XMPL:rnx_code_conversions:glo:L2:`](#station_optionsXMPLrnx_code_conversionsgloL2)
- [`station_options:XMPL:rnx_phase_conversions:glo:L2:`](#station_optionsXMPLrnx_phase_conversionsgloL2)
- [`station_options:XMPL:rnx_code_conversions:glo:L3:`](#station_optionsXMPLrnx_code_conversionsgloL3)
- [`station_options:XMPL:rnx_phase_conversions:glo:L3:`](#station_optionsXMPLrnx_phase_conversionsgloL3)
- [`station_options:XMPL:rnx_code_conversions:glo:L4:`](#station_optionsXMPLrnx_code_conversionsgloL4)
- [`station_options:XMPL:rnx_phase_conversions:glo:L4:`](#station_optionsXMPLrnx_phase_conversionsgloL4)
- [`station_options:XMPL:rnx_code_conversions:glo:L5:`](#station_optionsXMPLrnx_code_conversionsgloL5)
- [`station_options:XMPL:rnx_phase_conversions:glo:L5:`](#station_optionsXMPLrnx_phase_conversionsgloL5)
- [`station_options:XMPL:rnx_code_conversions:glo:L6:`](#station_optionsXMPLrnx_code_conversionsgloL6)
- [`station_options:XMPL:rnx_phase_conversions:glo:L6:`](#station_optionsXMPLrnx_phase_conversionsgloL6)
- [`station_options:XMPL:rnx_code_conversions:glo:L7:`](#station_optionsXMPLrnx_code_conversionsgloL7)
- [`station_options:XMPL:rnx_phase_conversions:glo:L7:`](#station_optionsXMPLrnx_phase_conversionsgloL7)
- [`station_options:XMPL:rnx_code_conversions:glo:L8:`](#station_optionsXMPLrnx_code_conversionsgloL8)
- [`station_options:XMPL:rnx_phase_conversions:glo:L8:`](#station_optionsXMPLrnx_phase_conversionsgloL8)
- [`station_options:XMPL:rnx_code_conversions:glo:LA:`](#station_optionsXMPLrnx_code_conversionsgloLA)
- [`station_options:XMPL:rnx_phase_conversions:glo:LA:`](#station_optionsXMPLrnx_phase_conversionsgloLA)
- [`station_options:XMPL:rnx_code_conversions:qzs:NONE:`](#station_optionsXMPLrnx_code_conversionsqzsNONE)
- [`station_options:XMPL:rnx_phase_conversions:qzs:NONE:`](#station_optionsXMPLrnx_phase_conversionsqzsNONE)
- [`station_options:XMPL:rnx_code_conversions:qzs:P1:`](#station_optionsXMPLrnx_code_conversionsqzsP1)
- [`station_options:XMPL:rnx_phase_conversions:qzs:P1:`](#station_optionsXMPLrnx_phase_conversionsqzsP1)
- [`station_options:XMPL:rnx_code_conversions:qzs:P2:`](#station_optionsXMPLrnx_code_conversionsqzsP2)
- [`station_options:XMPL:rnx_phase_conversions:qzs:P2:`](#station_optionsXMPLrnx_phase_conversionsqzsP2)
- [`station_options:XMPL:rnx_code_conversions:qzs:C1:`](#station_optionsXMPLrnx_code_conversionsqzsC1)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C1:`](#station_optionsXMPLrnx_phase_conversionsqzsC1)
- [`station_options:XMPL:rnx_code_conversions:qzs:C2:`](#station_optionsXMPLrnx_code_conversionsqzsC2)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C2:`](#station_optionsXMPLrnx_phase_conversionsqzsC2)
- [`station_options:XMPL:rnx_code_conversions:qzs:C3:`](#station_optionsXMPLrnx_code_conversionsqzsC3)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C3:`](#station_optionsXMPLrnx_phase_conversionsqzsC3)
- [`station_options:XMPL:rnx_code_conversions:qzs:C4:`](#station_optionsXMPLrnx_code_conversionsqzsC4)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C4:`](#station_optionsXMPLrnx_phase_conversionsqzsC4)
- [`station_options:XMPL:rnx_code_conversions:qzs:C5:`](#station_optionsXMPLrnx_code_conversionsqzsC5)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C5:`](#station_optionsXMPLrnx_phase_conversionsqzsC5)
- [`station_options:XMPL:rnx_code_conversions:qzs:C6:`](#station_optionsXMPLrnx_code_conversionsqzsC6)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C6:`](#station_optionsXMPLrnx_phase_conversionsqzsC6)
- [`station_options:XMPL:rnx_code_conversions:qzs:C7:`](#station_optionsXMPLrnx_code_conversionsqzsC7)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C7:`](#station_optionsXMPLrnx_phase_conversionsqzsC7)
- [`station_options:XMPL:rnx_code_conversions:qzs:C8:`](#station_optionsXMPLrnx_code_conversionsqzsC8)
- [`station_options:XMPL:rnx_phase_conversions:qzs:C8:`](#station_optionsXMPLrnx_phase_conversionsqzsC8)
- [`station_options:XMPL:rnx_code_conversions:qzs:L1:`](#station_optionsXMPLrnx_code_conversionsqzsL1)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L1:`](#station_optionsXMPLrnx_phase_conversionsqzsL1)
- [`station_options:XMPL:rnx_code_conversions:qzs:L2:`](#station_optionsXMPLrnx_code_conversionsqzsL2)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L2:`](#station_optionsXMPLrnx_phase_conversionsqzsL2)
- [`station_options:XMPL:rnx_code_conversions:qzs:L3:`](#station_optionsXMPLrnx_code_conversionsqzsL3)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L3:`](#station_optionsXMPLrnx_phase_conversionsqzsL3)
- [`station_options:XMPL:rnx_code_conversions:qzs:L4:`](#station_optionsXMPLrnx_code_conversionsqzsL4)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L4:`](#station_optionsXMPLrnx_phase_conversionsqzsL4)
- [`station_options:XMPL:rnx_code_conversions:qzs:L5:`](#station_optionsXMPLrnx_code_conversionsqzsL5)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L5:`](#station_optionsXMPLrnx_phase_conversionsqzsL5)
- [`station_options:XMPL:rnx_code_conversions:qzs:L6:`](#station_optionsXMPLrnx_code_conversionsqzsL6)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L6:`](#station_optionsXMPLrnx_phase_conversionsqzsL6)
- [`station_options:XMPL:rnx_code_conversions:qzs:L7:`](#station_optionsXMPLrnx_code_conversionsqzsL7)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L7:`](#station_optionsXMPLrnx_phase_conversionsqzsL7)
- [`station_options:XMPL:rnx_code_conversions:qzs:L8:`](#station_optionsXMPLrnx_code_conversionsqzsL8)
- [`station_options:XMPL:rnx_phase_conversions:qzs:L8:`](#station_optionsXMPLrnx_phase_conversionsqzsL8)
- [`station_options:XMPL:rnx_code_conversions:qzs:LA:`](#station_optionsXMPLrnx_code_conversionsqzsLA)
- [`station_options:XMPL:rnx_phase_conversions:qzs:LA:`](#station_optionsXMPLrnx_phase_conversionsqzsLA)
- [`station_options:XMPL:rnx_code_conversions:sbs:NONE:`](#station_optionsXMPLrnx_code_conversionssbsNONE)
- [`station_options:XMPL:rnx_phase_conversions:sbs:NONE:`](#station_optionsXMPLrnx_phase_conversionssbsNONE)
- [`station_options:XMPL:rnx_code_conversions:sbs:P1:`](#station_optionsXMPLrnx_code_conversionssbsP1)
- [`station_options:XMPL:rnx_phase_conversions:sbs:P1:`](#station_optionsXMPLrnx_phase_conversionssbsP1)
- [`station_options:XMPL:rnx_code_conversions:sbs:P2:`](#station_optionsXMPLrnx_code_conversionssbsP2)
- [`station_options:XMPL:rnx_phase_conversions:sbs:P2:`](#station_optionsXMPLrnx_phase_conversionssbsP2)
- [`station_options:XMPL:rnx_code_conversions:sbs:C1:`](#station_optionsXMPLrnx_code_conversionssbsC1)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C1:`](#station_optionsXMPLrnx_phase_conversionssbsC1)
- [`station_options:XMPL:rnx_code_conversions:sbs:C2:`](#station_optionsXMPLrnx_code_conversionssbsC2)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C2:`](#station_optionsXMPLrnx_phase_conversionssbsC2)
- [`station_options:XMPL:rnx_code_conversions:sbs:C3:`](#station_optionsXMPLrnx_code_conversionssbsC3)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C3:`](#station_optionsXMPLrnx_phase_conversionssbsC3)
- [`station_options:XMPL:rnx_code_conversions:sbs:C4:`](#station_optionsXMPLrnx_code_conversionssbsC4)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C4:`](#station_optionsXMPLrnx_phase_conversionssbsC4)
- [`station_options:XMPL:rnx_code_conversions:sbs:C5:`](#station_optionsXMPLrnx_code_conversionssbsC5)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C5:`](#station_optionsXMPLrnx_phase_conversionssbsC5)
- [`station_options:XMPL:rnx_code_conversions:sbs:C6:`](#station_optionsXMPLrnx_code_conversionssbsC6)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C6:`](#station_optionsXMPLrnx_phase_conversionssbsC6)
- [`station_options:XMPL:rnx_code_conversions:sbs:C7:`](#station_optionsXMPLrnx_code_conversionssbsC7)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C7:`](#station_optionsXMPLrnx_phase_conversionssbsC7)
- [`station_options:XMPL:rnx_code_conversions:sbs:C8:`](#station_optionsXMPLrnx_code_conversionssbsC8)
- [`station_options:XMPL:rnx_phase_conversions:sbs:C8:`](#station_optionsXMPLrnx_phase_conversionssbsC8)
- [`station_options:XMPL:rnx_code_conversions:sbs:L1:`](#station_optionsXMPLrnx_code_conversionssbsL1)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L1:`](#station_optionsXMPLrnx_phase_conversionssbsL1)
- [`station_options:XMPL:rnx_code_conversions:sbs:L2:`](#station_optionsXMPLrnx_code_conversionssbsL2)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L2:`](#station_optionsXMPLrnx_phase_conversionssbsL2)
- [`station_options:XMPL:rnx_code_conversions:sbs:L3:`](#station_optionsXMPLrnx_code_conversionssbsL3)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L3:`](#station_optionsXMPLrnx_phase_conversionssbsL3)
- [`station_options:XMPL:rnx_code_conversions:sbs:L4:`](#station_optionsXMPLrnx_code_conversionssbsL4)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L4:`](#station_optionsXMPLrnx_phase_conversionssbsL4)
- [`station_options:XMPL:rnx_code_conversions:sbs:L5:`](#station_optionsXMPLrnx_code_conversionssbsL5)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L5:`](#station_optionsXMPLrnx_phase_conversionssbsL5)
- [`station_options:XMPL:rnx_code_conversions:sbs:L6:`](#station_optionsXMPLrnx_code_conversionssbsL6)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L6:`](#station_optionsXMPLrnx_phase_conversionssbsL6)
- [`station_options:XMPL:rnx_code_conversions:sbs:L7:`](#station_optionsXMPLrnx_code_conversionssbsL7)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L7:`](#station_optionsXMPLrnx_phase_conversionssbsL7)
- [`station_options:XMPL:rnx_code_conversions:sbs:L8:`](#station_optionsXMPLrnx_code_conversionssbsL8)
- [`station_options:XMPL:rnx_phase_conversions:sbs:L8:`](#station_optionsXMPLrnx_phase_conversionssbsL8)
- [`station_options:XMPL:rnx_code_conversions:sbs:LA:`](#station_optionsXMPLrnx_code_conversionssbsLA)
- [`station_options:XMPL:rnx_phase_conversions:sbs:LA:`](#station_optionsXMPLrnx_phase_conversionssbsLA)
- [`station_options:XMPL:rnx_code_conversions:bds:NONE:`](#station_optionsXMPLrnx_code_conversionsbdsNONE)
- [`station_options:XMPL:rnx_phase_conversions:bds:NONE:`](#station_optionsXMPLrnx_phase_conversionsbdsNONE)
- [`station_options:XMPL:rnx_code_conversions:bds:P1:`](#station_optionsXMPLrnx_code_conversionsbdsP1)
- [`station_options:XMPL:rnx_phase_conversions:bds:P1:`](#station_optionsXMPLrnx_phase_conversionsbdsP1)
- [`station_options:XMPL:rnx_code_conversions:bds:P2:`](#station_optionsXMPLrnx_code_conversionsbdsP2)
- [`station_options:XMPL:rnx_phase_conversions:bds:P2:`](#station_optionsXMPLrnx_phase_conversionsbdsP2)
- [`station_options:XMPL:rnx_code_conversions:bds:C1:`](#station_optionsXMPLrnx_code_conversionsbdsC1)
- [`station_options:XMPL:rnx_phase_conversions:bds:C1:`](#station_optionsXMPLrnx_phase_conversionsbdsC1)
- [`station_options:XMPL:rnx_code_conversions:bds:C2:`](#station_optionsXMPLrnx_code_conversionsbdsC2)
- [`station_options:XMPL:rnx_phase_conversions:bds:C2:`](#station_optionsXMPLrnx_phase_conversionsbdsC2)
- [`station_options:XMPL:rnx_code_conversions:bds:C3:`](#station_optionsXMPLrnx_code_conversionsbdsC3)
- [`station_options:XMPL:rnx_phase_conversions:bds:C3:`](#station_optionsXMPLrnx_phase_conversionsbdsC3)
- [`station_options:XMPL:rnx_code_conversions:bds:C4:`](#station_optionsXMPLrnx_code_conversionsbdsC4)
- [`station_options:XMPL:rnx_phase_conversions:bds:C4:`](#station_optionsXMPLrnx_phase_conversionsbdsC4)
- [`station_options:XMPL:rnx_code_conversions:bds:C5:`](#station_optionsXMPLrnx_code_conversionsbdsC5)
- [`station_options:XMPL:rnx_phase_conversions:bds:C5:`](#station_optionsXMPLrnx_phase_conversionsbdsC5)
- [`station_options:XMPL:rnx_code_conversions:bds:C6:`](#station_optionsXMPLrnx_code_conversionsbdsC6)
- [`station_options:XMPL:rnx_phase_conversions:bds:C6:`](#station_optionsXMPLrnx_phase_conversionsbdsC6)
- [`station_options:XMPL:rnx_code_conversions:bds:C7:`](#station_optionsXMPLrnx_code_conversionsbdsC7)
- [`station_options:XMPL:rnx_phase_conversions:bds:C7:`](#station_optionsXMPLrnx_phase_conversionsbdsC7)
- [`station_options:XMPL:rnx_code_conversions:bds:C8:`](#station_optionsXMPLrnx_code_conversionsbdsC8)
- [`station_options:XMPL:rnx_phase_conversions:bds:C8:`](#station_optionsXMPLrnx_phase_conversionsbdsC8)
- [`station_options:XMPL:rnx_code_conversions:bds:L1:`](#station_optionsXMPLrnx_code_conversionsbdsL1)
- [`station_options:XMPL:rnx_phase_conversions:bds:L1:`](#station_optionsXMPLrnx_phase_conversionsbdsL1)
- [`station_options:XMPL:rnx_code_conversions:bds:L2:`](#station_optionsXMPLrnx_code_conversionsbdsL2)
- [`station_options:XMPL:rnx_phase_conversions:bds:L2:`](#station_optionsXMPLrnx_phase_conversionsbdsL2)
- [`station_options:XMPL:rnx_code_conversions:bds:L3:`](#station_optionsXMPLrnx_code_conversionsbdsL3)
- [`station_options:XMPL:rnx_phase_conversions:bds:L3:`](#station_optionsXMPLrnx_phase_conversionsbdsL3)
- [`station_options:XMPL:rnx_code_conversions:bds:L4:`](#station_optionsXMPLrnx_code_conversionsbdsL4)
- [`station_options:XMPL:rnx_phase_conversions:bds:L4:`](#station_optionsXMPLrnx_phase_conversionsbdsL4)
- [`station_options:XMPL:rnx_code_conversions:bds:L5:`](#station_optionsXMPLrnx_code_conversionsbdsL5)
- [`station_options:XMPL:rnx_phase_conversions:bds:L5:`](#station_optionsXMPLrnx_phase_conversionsbdsL5)
- [`station_options:XMPL:rnx_code_conversions:bds:L6:`](#station_optionsXMPLrnx_code_conversionsbdsL6)
- [`station_options:XMPL:rnx_phase_conversions:bds:L6:`](#station_optionsXMPLrnx_phase_conversionsbdsL6)
- [`station_options:XMPL:rnx_code_conversions:bds:L7:`](#station_optionsXMPLrnx_code_conversionsbdsL7)
- [`station_options:XMPL:rnx_phase_conversions:bds:L7:`](#station_optionsXMPLrnx_phase_conversionsbdsL7)
- [`station_options:XMPL:rnx_code_conversions:bds:L8:`](#station_optionsXMPLrnx_code_conversionsbdsL8)
- [`station_options:XMPL:rnx_phase_conversions:bds:L8:`](#station_optionsXMPLrnx_phase_conversionsbdsL8)
- [`station_options:XMPL:rnx_code_conversions:bds:LA:`](#station_optionsXMPLrnx_code_conversionsbdsLA)
- [`station_options:XMPL:rnx_phase_conversions:bds:LA:`](#station_optionsXMPLrnx_phase_conversionsbdsLA)
- [`station_options:XMPL:rnx_code_conversions:leo:NONE:`](#station_optionsXMPLrnx_code_conversionsleoNONE)
- [`station_options:XMPL:rnx_phase_conversions:leo:NONE:`](#station_optionsXMPLrnx_phase_conversionsleoNONE)
- [`station_options:XMPL:rnx_code_conversions:leo:P1:`](#station_optionsXMPLrnx_code_conversionsleoP1)
- [`station_options:XMPL:rnx_phase_conversions:leo:P1:`](#station_optionsXMPLrnx_phase_conversionsleoP1)
- [`station_options:XMPL:rnx_code_conversions:leo:P2:`](#station_optionsXMPLrnx_code_conversionsleoP2)
- [`station_options:XMPL:rnx_phase_conversions:leo:P2:`](#station_optionsXMPLrnx_phase_conversionsleoP2)
- [`station_options:XMPL:rnx_code_conversions:leo:C1:`](#station_optionsXMPLrnx_code_conversionsleoC1)
- [`station_options:XMPL:rnx_phase_conversions:leo:C1:`](#station_optionsXMPLrnx_phase_conversionsleoC1)
- [`station_options:XMPL:rnx_code_conversions:leo:C2:`](#station_optionsXMPLrnx_code_conversionsleoC2)
- [`station_options:XMPL:rnx_phase_conversions:leo:C2:`](#station_optionsXMPLrnx_phase_conversionsleoC2)
- [`station_options:XMPL:rnx_code_conversions:leo:C3:`](#station_optionsXMPLrnx_code_conversionsleoC3)
- [`station_options:XMPL:rnx_phase_conversions:leo:C3:`](#station_optionsXMPLrnx_phase_conversionsleoC3)
- [`station_options:XMPL:rnx_code_conversions:leo:C4:`](#station_optionsXMPLrnx_code_conversionsleoC4)
- [`station_options:XMPL:rnx_phase_conversions:leo:C4:`](#station_optionsXMPLrnx_phase_conversionsleoC4)
- [`station_options:XMPL:rnx_code_conversions:leo:C5:`](#station_optionsXMPLrnx_code_conversionsleoC5)
- [`station_options:XMPL:rnx_phase_conversions:leo:C5:`](#station_optionsXMPLrnx_phase_conversionsleoC5)
- [`station_options:XMPL:rnx_code_conversions:leo:C6:`](#station_optionsXMPLrnx_code_conversionsleoC6)
- [`station_options:XMPL:rnx_phase_conversions:leo:C6:`](#station_optionsXMPLrnx_phase_conversionsleoC6)
- [`station_options:XMPL:rnx_code_conversions:leo:C7:`](#station_optionsXMPLrnx_code_conversionsleoC7)
- [`station_options:XMPL:rnx_phase_conversions:leo:C7:`](#station_optionsXMPLrnx_phase_conversionsleoC7)
- [`station_options:XMPL:rnx_code_conversions:leo:C8:`](#station_optionsXMPLrnx_code_conversionsleoC8)
- [`station_options:XMPL:rnx_phase_conversions:leo:C8:`](#station_optionsXMPLrnx_phase_conversionsleoC8)
- [`station_options:XMPL:rnx_code_conversions:leo:L1:`](#station_optionsXMPLrnx_code_conversionsleoL1)
- [`station_options:XMPL:rnx_phase_conversions:leo:L1:`](#station_optionsXMPLrnx_phase_conversionsleoL1)
- [`station_options:XMPL:rnx_code_conversions:leo:L2:`](#station_optionsXMPLrnx_code_conversionsleoL2)
- [`station_options:XMPL:rnx_phase_conversions:leo:L2:`](#station_optionsXMPLrnx_phase_conversionsleoL2)
- [`station_options:XMPL:rnx_code_conversions:leo:L3:`](#station_optionsXMPLrnx_code_conversionsleoL3)
- [`station_options:XMPL:rnx_phase_conversions:leo:L3:`](#station_optionsXMPLrnx_phase_conversionsleoL3)
- [`station_options:XMPL:rnx_code_conversions:leo:L4:`](#station_optionsXMPLrnx_code_conversionsleoL4)
- [`station_options:XMPL:rnx_phase_conversions:leo:L4:`](#station_optionsXMPLrnx_phase_conversionsleoL4)
- [`station_options:XMPL:rnx_code_conversions:leo:L5:`](#station_optionsXMPLrnx_code_conversionsleoL5)
- [`station_options:XMPL:rnx_phase_conversions:leo:L5:`](#station_optionsXMPLrnx_phase_conversionsleoL5)
- [`station_options:XMPL:rnx_code_conversions:leo:L6:`](#station_optionsXMPLrnx_code_conversionsleoL6)
- [`station_options:XMPL:rnx_phase_conversions:leo:L6:`](#station_optionsXMPLrnx_phase_conversionsleoL6)
- [`station_options:XMPL:rnx_code_conversions:leo:L7:`](#station_optionsXMPLrnx_code_conversionsleoL7)
- [`station_options:XMPL:rnx_phase_conversions:leo:L7:`](#station_optionsXMPLrnx_phase_conversionsleoL7)
- [`station_options:XMPL:rnx_code_conversions:leo:L8:`](#station_optionsXMPLrnx_code_conversionsleoL8)
- [`station_options:XMPL:rnx_phase_conversions:leo:L8:`](#station_optionsXMPLrnx_phase_conversionsleoL8)
- [`station_options:XMPL:rnx_code_conversions:leo:LA:`](#station_optionsXMPLrnx_code_conversionsleoLA)
- [`station_options:XMPL:rnx_phase_conversions:leo:LA:`](#station_optionsXMPLrnx_phase_conversionsleoLA)
---

### E_OffsetType

Valid enum values are:
- `unspecified`
- `apc`
- `com`

For options:

- [`processing_options:ssr_inputs:ssr_antenna_offset:`](#processing_optionsssr_inputsssr_antenna_offset)
---

### E_Period

Valid enum values are:
- `second`
- `minute`
- `hour`
- `day`
- `week`
- `year`
- `seconds`
- `minutes`
- `hours`
- `days`
- `weeks`
- `years`
- `sec`
- `min`
- `hr`
- `dy`
- `wk`
- `yr`
- `secs`
- `mins`
- `hrs`
- `dys`
- `wks`
- `yrs`
- `sqrt_sec`
- `sqrt_min`
- `sqrt_hr`
- `sqrt_dy`
- `sqrt_wk`
- `sqrt_yr`
- `sqrt_secs`
- `sqrt_mins`
- `sqrt_hrs`
- `sqrt_dys`
- `sqrt_wks`
- `sqrt_yrs`
- `sqrt_second`
- `sqrt_minute`
- `sqrt_hour`
- `sqrt_day`
- `sqrt_week`
- `sqrt_year`
- `sqrt_seconds`
- `sqrt_minutes`
- `sqrt_hours`
- `sqrt_days`
- `sqrt_weeks`
- `sqrt_years`

For options:

- [`outputs:output_rotation:period_units:`](#outputsoutput_rotationperiod_units)
- [`processing_options:minimum_constraints:scale:proc_noise_dt:`](#processing_optionsminimum_constraintsscaleproc_noise_dt)
- [`processing_options:minimum_constraints:rotation:proc_noise_dt:`](#processing_optionsminimum_constraintsrotationproc_noise_dt)
- [`processing_options:minimum_constraints:translation:proc_noise_dt:`](#processing_optionsminimum_constraintstranslationproc_noise_dt)
- [`estimation_parameters:eop:proc_noise_dt:`](#estimation_parameterseopproc_noise_dt)
- [`estimation_parameters:eop_rates:proc_noise_dt:`](#estimation_parameterseop_ratesproc_noise_dt)
- [`estimation_parameters:ion:proc_noise_dt:`](#estimation_parametersionproc_noise_dt)
- [`mongo:interval_units:`](#mongointerval_units)
- [`mongo:duration_units:`](#mongoduration_units)
- [`mongo:duration_units:`](#mongoduration_units)
- [`remote_mongo:interval_units:`](#remote_mongointerval_units)
- [`remote_mongo:duration_units:`](#remote_mongoduration_units)
- [`remote_mongo:duration_units:`](#remote_mongoduration_units)
- [`estimation_parameters:satellites:clk:proc_noise_dt:`](#estimation_parameterssatellitesclkproc_noise_dt)
- [`estimation_parameters:satellites:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:pos:proc_noise_dt:`](#estimation_parameterssatellitesposproc_noise_dt)
- [`estimation_parameters:satellites:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitespos_rateproc_noise_dt)
- [`estimation_parameters:satellites:orb:proc_noise_dt:`](#estimation_parameterssatellitesorbproc_noise_dt)
- [`estimation_parameters:satellites:pco:proc_noise_dt:`](#estimation_parameterssatellitespcoproc_noise_dt)
- [`estimation_parameters:satellites:ant:proc_noise_dt:`](#estimation_parameterssatellitesantproc_noise_dt)
- [`estimation_parameters:satellites:orbit:proc_noise_dt:`](#estimation_parameterssatellitesorbitproc_noise_dt)
- [`estimation_parameters:satellites:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesion_modelproc_noise_dt)
- [`estimation_parameters:satellites:code_bias:proc_noise_dt:`](#estimation_parameterssatellitescode_biasproc_noise_dt)
- [`estimation_parameters:satellites:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitessrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:clk:proc_noise_dt:`](#estimation_parameterssatellitesL1Wclkproc_noise_dt)
- [`estimation_parameters:satellites:L1W:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesL1Wclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:L1W:pos:proc_noise_dt:`](#estimation_parameterssatellitesL1Wposproc_noise_dt)
- [`estimation_parameters:satellites:L1W:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesL1Wpos_rateproc_noise_dt)
- [`estimation_parameters:satellites:L1W:orb:proc_noise_dt:`](#estimation_parameterssatellitesL1Worbproc_noise_dt)
- [`estimation_parameters:satellites:L1W:pco:proc_noise_dt:`](#estimation_parameterssatellitesL1Wpcoproc_noise_dt)
- [`estimation_parameters:satellites:L1W:ant:proc_noise_dt:`](#estimation_parameterssatellitesL1Wantproc_noise_dt)
- [`estimation_parameters:satellites:L1W:orbit:proc_noise_dt:`](#estimation_parameterssatellitesL1Worbitproc_noise_dt)
- [`estimation_parameters:satellites:L1W:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesL1Wion_modelproc_noise_dt)
- [`estimation_parameters:satellites:L1W:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesL1Wcode_biasproc_noise_dt)
- [`estimation_parameters:satellites:L1W:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesL1Wphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:L1W:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesL1Wsrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:global:clk:proc_noise_dt:`](#estimation_parameterssatellitesglobalclkproc_noise_dt)
- [`estimation_parameters:satellites:global:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesglobalclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:global:pos:proc_noise_dt:`](#estimation_parameterssatellitesglobalposproc_noise_dt)
- [`estimation_parameters:satellites:global:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesglobalpos_rateproc_noise_dt)
- [`estimation_parameters:satellites:global:orb:proc_noise_dt:`](#estimation_parameterssatellitesglobalorbproc_noise_dt)
- [`estimation_parameters:satellites:global:pco:proc_noise_dt:`](#estimation_parameterssatellitesglobalpcoproc_noise_dt)
- [`estimation_parameters:satellites:global:ant:proc_noise_dt:`](#estimation_parameterssatellitesglobalantproc_noise_dt)
- [`estimation_parameters:satellites:global:orbit:proc_noise_dt:`](#estimation_parameterssatellitesglobalorbitproc_noise_dt)
- [`estimation_parameters:satellites:global:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesglobalion_modelproc_noise_dt)
- [`estimation_parameters:satellites:global:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesglobalcode_biasproc_noise_dt)
- [`estimation_parameters:satellites:global:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesglobalphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:global:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesglobalemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:global:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesglobalsrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:clk:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wclkproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:pos:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wposproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wpos_rateproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:orb:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Worbproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:pco:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wpcoproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:ant:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wantproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:orbit:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Worbitproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wion_modelproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wcode_biasproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:global:L1W:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesglobalL1Wsrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:clk:proc_noise_dt:`](#estimation_parameterssatellitesGPSclkproc_noise_dt)
- [`estimation_parameters:satellites:GPS:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesGPSclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:GPS:pos:proc_noise_dt:`](#estimation_parameterssatellitesGPSposproc_noise_dt)
- [`estimation_parameters:satellites:GPS:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesGPSpos_rateproc_noise_dt)
- [`estimation_parameters:satellites:GPS:orb:proc_noise_dt:`](#estimation_parameterssatellitesGPSorbproc_noise_dt)
- [`estimation_parameters:satellites:GPS:pco:proc_noise_dt:`](#estimation_parameterssatellitesGPSpcoproc_noise_dt)
- [`estimation_parameters:satellites:GPS:ant:proc_noise_dt:`](#estimation_parameterssatellitesGPSantproc_noise_dt)
- [`estimation_parameters:satellites:GPS:orbit:proc_noise_dt:`](#estimation_parameterssatellitesGPSorbitproc_noise_dt)
- [`estimation_parameters:satellites:GPS:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesGPSion_modelproc_noise_dt)
- [`estimation_parameters:satellites:GPS:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesGPScode_biasproc_noise_dt)
- [`estimation_parameters:satellites:GPS:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesGPSphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesGPSemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesGPSsrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:clk:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wclkproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:pos:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wposproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wpos_rateproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:orb:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Worbproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:pco:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wpcoproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:ant:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wantproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:orbit:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Worbitproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wion_modelproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wcode_biasproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesGPSL1Wsrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:G01:clk:proc_noise_dt:`](#estimation_parameterssatellitesG01clkproc_noise_dt)
- [`estimation_parameters:satellites:G01:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesG01clk_rateproc_noise_dt)
- [`estimation_parameters:satellites:G01:pos:proc_noise_dt:`](#estimation_parameterssatellitesG01posproc_noise_dt)
- [`estimation_parameters:satellites:G01:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesG01pos_rateproc_noise_dt)
- [`estimation_parameters:satellites:G01:orb:proc_noise_dt:`](#estimation_parameterssatellitesG01orbproc_noise_dt)
- [`estimation_parameters:satellites:G01:pco:proc_noise_dt:`](#estimation_parameterssatellitesG01pcoproc_noise_dt)
- [`estimation_parameters:satellites:G01:ant:proc_noise_dt:`](#estimation_parameterssatellitesG01antproc_noise_dt)
- [`estimation_parameters:satellites:G01:orbit:proc_noise_dt:`](#estimation_parameterssatellitesG01orbitproc_noise_dt)
- [`estimation_parameters:satellites:G01:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesG01ion_modelproc_noise_dt)
- [`estimation_parameters:satellites:G01:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesG01code_biasproc_noise_dt)
- [`estimation_parameters:satellites:G01:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesG01phase_biasproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:G01:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesG01emp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:G01:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesG01srp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:clk:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wclkproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:clk_rate:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wclk_rateproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:pos:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wposproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:pos_rate:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wpos_rateproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:orb:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Worbproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:pco:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wpcoproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:ant:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wantproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:orbit:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Worbitproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:ion_model:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wion_modelproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:code_bias:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wcode_biasproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:phase_bias:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wphase_biasproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:emp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wemp_dyb_4sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_0:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_0proc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_1c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_1cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_1s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_1sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_2c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_2cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_2s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_2sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_3c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_3cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_3s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_3sproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_4c:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_4cproc_noise_dt)
- [`estimation_parameters:satellites:G01:L1W:srp_dyb_4s:proc_noise_dt:`](#estimation_parameterssatellitesG01L1Wsrp_dyb_4sproc_noise_dt)
- [`estimation_parameters:stations:clk:proc_noise_dt:`](#estimation_parametersstationsclkproc_noise_dt)
- [`estimation_parameters:stations:clk_rate:proc_noise_dt:`](#estimation_parametersstationsclk_rateproc_noise_dt)
- [`estimation_parameters:stations:pos:proc_noise_dt:`](#estimation_parametersstationsposproc_noise_dt)
- [`estimation_parameters:stations:pos_rate:proc_noise_dt:`](#estimation_parametersstationspos_rateproc_noise_dt)
- [`estimation_parameters:stations:heading:proc_noise_dt:`](#estimation_parametersstationsheadingproc_noise_dt)
- [`estimation_parameters:stations:orbit:proc_noise_dt:`](#estimation_parametersstationsorbitproc_noise_dt)
- [`estimation_parameters:stations:strain_rate:proc_noise_dt:`](#estimation_parametersstationsstrain_rateproc_noise_dt)
- [`estimation_parameters:stations:amb:proc_noise_dt:`](#estimation_parametersstationsambproc_noise_dt)
- [`estimation_parameters:stations:pco:proc_noise_dt:`](#estimation_parametersstationspcoproc_noise_dt)
- [`estimation_parameters:stations:ant:proc_noise_dt:`](#estimation_parametersstationsantproc_noise_dt)
- [`estimation_parameters:stations:code_bias:proc_noise_dt:`](#estimation_parametersstationscode_biasproc_noise_dt)
- [`estimation_parameters:stations:phase_bias:proc_noise_dt:`](#estimation_parametersstationsphase_biasproc_noise_dt)
- [`estimation_parameters:stations:ion_stec:proc_noise_dt:`](#estimation_parametersstationsion_stecproc_noise_dt)
- [`estimation_parameters:stations:slr_range_bias:proc_noise_dt:`](#estimation_parametersstationsslr_range_biasproc_noise_dt)
- [`estimation_parameters:stations:slr_time_bias:proc_noise_dt:`](#estimation_parametersstationsslr_time_biasproc_noise_dt)
- [`estimation_parameters:stations:trop:proc_noise_dt:`](#estimation_parametersstationstropproc_noise_dt)
- [`estimation_parameters:stations:trop_grads:proc_noise_dt:`](#estimation_parametersstationstrop_gradsproc_noise_dt)
- [`estimation_parameters:stations:global:clk:proc_noise_dt:`](#estimation_parametersstationsglobalclkproc_noise_dt)
- [`estimation_parameters:stations:global:clk_rate:proc_noise_dt:`](#estimation_parametersstationsglobalclk_rateproc_noise_dt)
- [`estimation_parameters:stations:global:pos:proc_noise_dt:`](#estimation_parametersstationsglobalposproc_noise_dt)
- [`estimation_parameters:stations:global:pos_rate:proc_noise_dt:`](#estimation_parametersstationsglobalpos_rateproc_noise_dt)
- [`estimation_parameters:stations:global:heading:proc_noise_dt:`](#estimation_parametersstationsglobalheadingproc_noise_dt)
- [`estimation_parameters:stations:global:orbit:proc_noise_dt:`](#estimation_parametersstationsglobalorbitproc_noise_dt)
- [`estimation_parameters:stations:global:strain_rate:proc_noise_dt:`](#estimation_parametersstationsglobalstrain_rateproc_noise_dt)
- [`estimation_parameters:stations:global:amb:proc_noise_dt:`](#estimation_parametersstationsglobalambproc_noise_dt)
- [`estimation_parameters:stations:global:pco:proc_noise_dt:`](#estimation_parametersstationsglobalpcoproc_noise_dt)
- [`estimation_parameters:stations:global:ant:proc_noise_dt:`](#estimation_parametersstationsglobalantproc_noise_dt)
- [`estimation_parameters:stations:global:code_bias:proc_noise_dt:`](#estimation_parametersstationsglobalcode_biasproc_noise_dt)
- [`estimation_parameters:stations:global:phase_bias:proc_noise_dt:`](#estimation_parametersstationsglobalphase_biasproc_noise_dt)
- [`estimation_parameters:stations:global:ion_stec:proc_noise_dt:`](#estimation_parametersstationsglobalion_stecproc_noise_dt)
- [`estimation_parameters:stations:global:slr_range_bias:proc_noise_dt:`](#estimation_parametersstationsglobalslr_range_biasproc_noise_dt)
- [`estimation_parameters:stations:global:slr_time_bias:proc_noise_dt:`](#estimation_parametersstationsglobalslr_time_biasproc_noise_dt)
- [`estimation_parameters:stations:global:trop:proc_noise_dt:`](#estimation_parametersstationsglobaltropproc_noise_dt)
- [`estimation_parameters:stations:global:trop_grads:proc_noise_dt:`](#estimation_parametersstationsglobaltrop_gradsproc_noise_dt)
- [`estimation_parameters:stations:global:clk:proc_noise_dt:`](#estimation_parametersstationsglobalclkproc_noise_dt)
- [`estimation_parameters:stations:global:clk_rate:proc_noise_dt:`](#estimation_parametersstationsglobalclk_rateproc_noise_dt)
- [`estimation_parameters:stations:global:pos:proc_noise_dt:`](#estimation_parametersstationsglobalposproc_noise_dt)
- [`estimation_parameters:stations:global:pos_rate:proc_noise_dt:`](#estimation_parametersstationsglobalpos_rateproc_noise_dt)
- [`estimation_parameters:stations:global:heading:proc_noise_dt:`](#estimation_parametersstationsglobalheadingproc_noise_dt)
- [`estimation_parameters:stations:global:orbit:proc_noise_dt:`](#estimation_parametersstationsglobalorbitproc_noise_dt)
- [`estimation_parameters:stations:global:strain_rate:proc_noise_dt:`](#estimation_parametersstationsglobalstrain_rateproc_noise_dt)
- [`estimation_parameters:stations:global:amb:proc_noise_dt:`](#estimation_parametersstationsglobalambproc_noise_dt)
- [`estimation_parameters:stations:global:pco:proc_noise_dt:`](#estimation_parametersstationsglobalpcoproc_noise_dt)
- [`estimation_parameters:stations:global:ant:proc_noise_dt:`](#estimation_parametersstationsglobalantproc_noise_dt)
- [`estimation_parameters:stations:global:code_bias:proc_noise_dt:`](#estimation_parametersstationsglobalcode_biasproc_noise_dt)
- [`estimation_parameters:stations:global:phase_bias:proc_noise_dt:`](#estimation_parametersstationsglobalphase_biasproc_noise_dt)
- [`estimation_parameters:stations:global:ion_stec:proc_noise_dt:`](#estimation_parametersstationsglobalion_stecproc_noise_dt)
- [`estimation_parameters:stations:global:slr_range_bias:proc_noise_dt:`](#estimation_parametersstationsglobalslr_range_biasproc_noise_dt)
- [`estimation_parameters:stations:global:slr_time_bias:proc_noise_dt:`](#estimation_parametersstationsglobalslr_time_biasproc_noise_dt)
- [`estimation_parameters:stations:global:trop:proc_noise_dt:`](#estimation_parametersstationsglobaltropproc_noise_dt)
- [`estimation_parameters:stations:global:trop_grads:proc_noise_dt:`](#estimation_parametersstationsglobaltrop_gradsproc_noise_dt)
- [`estimation_parameters:stations:XMPL:clk:proc_noise_dt:`](#estimation_parametersstationsXMPLclkproc_noise_dt)
- [`estimation_parameters:stations:XMPL:clk_rate:proc_noise_dt:`](#estimation_parametersstationsXMPLclk_rateproc_noise_dt)
- [`estimation_parameters:stations:XMPL:pos:proc_noise_dt:`](#estimation_parametersstationsXMPLposproc_noise_dt)
- [`estimation_parameters:stations:XMPL:pos_rate:proc_noise_dt:`](#estimation_parametersstationsXMPLpos_rateproc_noise_dt)
- [`estimation_parameters:stations:XMPL:heading:proc_noise_dt:`](#estimation_parametersstationsXMPLheadingproc_noise_dt)
- [`estimation_parameters:stations:XMPL:orbit:proc_noise_dt:`](#estimation_parametersstationsXMPLorbitproc_noise_dt)
- [`estimation_parameters:stations:XMPL:strain_rate:proc_noise_dt:`](#estimation_parametersstationsXMPLstrain_rateproc_noise_dt)
- [`estimation_parameters:stations:XMPL:amb:proc_noise_dt:`](#estimation_parametersstationsXMPLambproc_noise_dt)
- [`estimation_parameters:stations:XMPL:pco:proc_noise_dt:`](#estimation_parametersstationsXMPLpcoproc_noise_dt)
- [`estimation_parameters:stations:XMPL:ant:proc_noise_dt:`](#estimation_parametersstationsXMPLantproc_noise_dt)
- [`estimation_parameters:stations:XMPL:code_bias:proc_noise_dt:`](#estimation_parametersstationsXMPLcode_biasproc_noise_dt)
- [`estimation_parameters:stations:XMPL:phase_bias:proc_noise_dt:`](#estimation_parametersstationsXMPLphase_biasproc_noise_dt)
- [`estimation_parameters:stations:XMPL:ion_stec:proc_noise_dt:`](#estimation_parametersstationsXMPLion_stecproc_noise_dt)
- [`estimation_parameters:stations:XMPL:slr_range_bias:proc_noise_dt:`](#estimation_parametersstationsXMPLslr_range_biasproc_noise_dt)
- [`estimation_parameters:stations:XMPL:slr_time_bias:proc_noise_dt:`](#estimation_parametersstationsXMPLslr_time_biasproc_noise_dt)
- [`estimation_parameters:stations:XMPL:trop:proc_noise_dt:`](#estimation_parametersstationsXMPLtropproc_noise_dt)
- [`estimation_parameters:stations:XMPL:trop_grads:proc_noise_dt:`](#estimation_parametersstationsXMPLtrop_gradsproc_noise_dt)
---

### E_SSROutTiming

Valid enum values are:
- `gps_time`
- `latest_clock_estimate`

For options:

- [`processing_options:ssr_corrections:output_timing:`](#processing_optionsssr_correctionsoutput_timing)
---

### E_Source

Valid enum values are:
- `none`
- `precise` : Values derived from file-based products such as SP3/CLK/OBX
- `ssr` : Values derived from applying received corrections to broadcast ephemeris
- `kalman` : Values estimated internally by the kalman filter
- `broadcast` : Values derived from broadcast ephemeris streams/files
- `nominal`
- `model`
- `remote`

For options:

- [`outputs:clocks:receiver_sources:`](#outputsclocksreceiver_sources)
- [`outputs:clocks:satellite_sources:`](#outputsclockssatellite_sources)
- [`outputs:sp3:clock_sources:`](#outputssp3clock_sources)
- [`outputs:sp3:orbit_sources:`](#outputssp3orbit_sources)
- [`outputs:orbex:orbit_sources:`](#outputsorbexorbit_sources)
- [`outputs:orbex:clock_sources:`](#outputsorbexclock_sources)
- [`outputs:orbex:attitude_sources:`](#outputsorbexattitude_sources)
- [`outputs:cost:sources:`](#outputscostsources)
- [`outputs:trop_sinex:sources:`](#outputstrop_sinexsources)
- [`processing_options:ssr_corrections:ephemeris_sources:`](#processing_optionsssr_correctionsephemeris_sources)
- [`processing_options:ssr_corrections:clock_sources:`](#processing_optionsssr_correctionsclock_sources)
- [`processing_options:ssr_corrections:code_bias_sources:`](#processing_optionsssr_correctionscode_bias_sources)
- [`processing_options:ssr_corrections:phase_bias_sources:`](#processing_optionsssr_correctionsphase_bias_sources)
- [`processing_options:ssr_corrections:ionosphere_sources:`](#processing_optionsssr_correctionsionosphere_sources)
- [`satellite_options:global:pos:sources:`](#satellite_optionsglobalpossources)
- [`satellite_options:global:clock:sources:`](#satellite_optionsglobalclocksources)
- [`satellite_options:global:attitude:sources:`](#satellite_optionsglobalattitudesources)
- [`satellite_options:global:L1W:pos:sources:`](#satellite_optionsglobalL1Wpossources)
- [`satellite_options:global:L1W:clock:sources:`](#satellite_optionsglobalL1Wclocksources)
- [`satellite_options:global:L1W:attitude:sources:`](#satellite_optionsglobalL1Wattitudesources)
- [`satellite_options:GPS:pos:sources:`](#satellite_optionsGPSpossources)
- [`satellite_options:GPS:clock:sources:`](#satellite_optionsGPSclocksources)
- [`satellite_options:GPS:attitude:sources:`](#satellite_optionsGPSattitudesources)
- [`satellite_options:GPS:L1W:pos:sources:`](#satellite_optionsGPSL1Wpossources)
- [`satellite_options:GPS:L1W:clock:sources:`](#satellite_optionsGPSL1Wclocksources)
- [`satellite_options:GPS:L1W:attitude:sources:`](#satellite_optionsGPSL1Wattitudesources)
- [`satellite_options:G01:pos:sources:`](#satellite_optionsG01possources)
- [`satellite_options:G01:clock:sources:`](#satellite_optionsG01clocksources)
- [`satellite_options:G01:attitude:sources:`](#satellite_optionsG01attitudesources)
- [`satellite_options:G01:L1W:pos:sources:`](#satellite_optionsG01L1Wpossources)
- [`satellite_options:G01:L1W:clock:sources:`](#satellite_optionsG01L1Wclocksources)
- [`satellite_options:G01:L1W:attitude:sources:`](#satellite_optionsG01L1Wattitudesources)
- [`station_options:global:attitude:sources:`](#station_optionsglobalattitudesources)
- [`station_options:global:attitude:sources:`](#station_optionsglobalattitudesources)
- [`station_options:XMPL:attitude:sources:`](#station_optionsXMPLattitudesources)
---

### E_Sys

Valid enum values are:
- `none`
- `gps`
- `gal`
- `glo`
- `qzs`
- `sbs`
- `bds`
- `leo`
- `supported`
- `irn`
- `ims`
- `comb`

For options:

- [`processing_options:gnss_general:rec_reference_system:`](#processing_optionsgnss_generalrec_reference_system)
---

### E_TropModel

Valid enum values are:
- `vmf3`
- `gpt2`

For options:

- [`processing_options:gnss_models:troposphere:model:`](#processing_optionsgnss_modelstropospheremodel)