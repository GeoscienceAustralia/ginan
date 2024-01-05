
# Default Configuration

This document outlines the major configuration options available in ginan that are most applicable to general users. For more advanced configuration options and their defaults, use the `-Y <level>` option at the command line to view increasing levels of advanced configurations.
## inputs:

###### **`inputs:`**
 ` `


> This section of the yaml file specifies the lists of files to be used for general metadata inputs, and inputs of external product data.


---

###### **`inputs:inputs_root:`**
 `"." `


Root path to be added to all other input files (unless they are absolute)

---

###### **`inputs:include_yamls:`**
 `[] `


List of yaml files to include before this one

---

###### **`inputs:gnss_observations:`**
 ` `


> Signal observation data from gnss receivers to be used as measurements

---

###### **`inputs:gnss_observations:gnss_observations_root:`**
 `"<INPUTS_ROOT>" `


Root path to be added to all other gnss data inputs (unless they are absolute)

---

###### **`inputs:gnss_observations:rnx_inputs:`**
 ` `


---

###### **`inputs:gnss_observations:rtcm_inputs:`**
 ` `


---

###### **`inputs:satellite_data:`**
 ` `


---

###### **`inputs:satellite_data:rtcm_inputs:`**
 ` `


> This section specifies how State State Representation (SSR) corrections are applied after they are downloaded from an NTRIP caster.

---

###### **`inputs:satellite_data:rtcm_inputs:rtcm_inputs_root:`**
 `"<SAT_DATA_ROOT>" `


Root path to be added to all other rtcm inputs (unless they are absolute)

---

###### **`inputs:satellite_data:rtcm_inputs:rtcm_inputs:`**
 `[] `


List of rtcm       inputs to use for corrections

---

###### **`inputs:satellite_data:rtcm_inputs:ssr_antenna_offset:`**
 [`E_OffsetType`](#e_offsettype) `UNSPECIFIED `


Ephemeris type that is provided in the listed SSR stream, i.e. satellite antenna-phase-centre (APC) or centre-of-mass (COM). This information is listed in the NTRIP Caster's sourcetable {unspecified,apc,com}

---

###### **`inputs:satellite_data:satellite_data_root:`**
 `"<INPUTS_ROOT>" `


Root path to be added to all other satellite data files (unless they are absolute)

---

###### **`inputs:satellite_data:bsx_files:`**
 `[] `


List of biassinex  files to use

---

###### **`inputs:satellite_data:clk_files:`**
 `[] `


List of clock      files to use

---

###### **`inputs:satellite_data:dcb_files:`**
 `[] `


List of dcb        files to use

---

###### **`inputs:satellite_data:nav_files:`**
 `[] `


List of ephemeris  files to use

---

###### **`inputs:satellite_data:obx_files:`**
 `[] `


List of orbex      files to use

---

###### **`inputs:satellite_data:sp3_files:`**
 `[] `


List of sp3        files to use

---

###### **`inputs:atx_files:`**
 `[] `


List of atx files to use

---

###### **`inputs:erp_files:`**
 `[] `


List of erp files to use

---

## outputs:

###### **`outputs:`**
 ` `


> Specifies options to enable outputs and specify file locations.

Each section typically contains an option to `output` the filetype, and a `directory` to place the files named `filename`, along with any ancillary options.


---

###### **`outputs:outputs_root:`**
 `"." `


Directory that outputs will be placed in

---

###### **`outputs:colourise_terminal:`**
 `true `


Use ascii command codes to highlight warnings and errors

---

###### **`outputs:warn_once:`**
 `true `


Print warnings once only

---

###### **`outputs:metadata:`**
 ` `


> Options for setting metadata for inputs and outputs

---

###### **`outputs:metadata:config_description:`**
 `"Pea" `


ID for this config, used to replace <CONFIG> tags in other options

---

###### **`outputs:metadata:pass:`**
 `"" `


Password for connecting to NTRIP casters

---

###### **`outputs:metadata:user:`**
 `"" `


Username for connecting to NTRIP casters

---

###### **`outputs:trace:`**
 ` `


> Trace files are used to document processing

---

###### **`outputs:trace:directory:`**
 `"<OUTPUTS_ROOT>" `


Directory to output trace files to

---

###### **`outputs:trace:level:`**
 `0 `


(int) Threshold level for printing messages (0-6). Increasing this increases the amount of data stored in all trace files

---

###### **`outputs:trace:output_config:`**
 `false `


Output configuration files to top of trace files

---

###### **`outputs:trace:output_residual_chain:`**
 `true `


Output component-wise details for measurement residuals

---

###### **`outputs:trace:output_residuals:`**
 `false `


Output measurements and residuals

---

###### **`outputs:trace:output_network:`**
 `false `


Output trace files for complete network of receivers, inclucing kalman filter results and statistics

---

###### **`outputs:trace:output_receivers:`**
 `false `


Output trace files for individual receivers processing

---

###### **`outputs:trace:network_filename:`**
 `"<TRACE_DIRECTORY>/<RECEIVER>-<LOGTIME>.trace" `


Template filename for network trace files

---

###### **`outputs:trace:receiver_filename:`**
 `"<TRACE_DIRECTORY>/<RECEIVER>-<LOGTIME>.trace" `


Template filename for receiver trace files

---

###### **`outputs:clocks:`**
 ` `


> Rinex formatted clock files

---

###### **`outputs:clocks:output:`**
 `false `


Output clock files

---

###### **`outputs:gpx:`**
 ` `


> GPX files contain point data that may be easily viewed in GIS mapping software

---

###### **`outputs:gpx:output:`**
 `false `


---

###### **`outputs:log:`**
 ` `


> Log files store console output in files

---

###### **`outputs:log:output:`**
 `false `


Enable console output logging

---

## processing_options:

###### **`processing_options:`**
 ` `


> Various sections and parameters to specify how the observations are processed

---

###### **`processing_options:epoch_control:`**
 ` `


> Specifies the rate and duration of data processing

---

###### **`processing_options:epoch_control:end_epoch:`**
 `"" `


(YYYY-MM-DD hh:mm:ss) The time of the last epoch to process (all observations after this will be skipped)

---

###### **`processing_options:epoch_control:epoch_interval:`**
 `1 `


Desired time step between each processing epoch

---

###### **`processing_options:epoch_control:max_epochs:`**
 `0 `


Maximum number of epochs to process

---

###### **`processing_options:epoch_control:start_epoch:`**
 `"" `


(YYYY-MM-DD hh:mm:ss) The time of the first epoch to process (all observations before this will be skipped)

---

###### **`processing_options:gnss_general:`**
 ` `


> Options to specify the processing of gnss observations

---

###### **`processing_options:gnss_general:sys_options:`**
 ` `


---

###### **`processing_options:gnss_general:sys_options:bds:`**
 ` `


> Options for the BDS constellation

---

###### **`processing_options:gnss_general:sys_options:bds:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:bds:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:bds:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:gal:`**
 ` `


> Options for the GAL constellation

---

###### **`processing_options:gnss_general:sys_options:gal:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:gal:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:gal:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:glo:`**
 ` `


> Options for the GLO constellation

---

###### **`processing_options:gnss_general:sys_options:glo:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:glo:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:glo:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:gps:`**
 ` `


> Options for the GPS constellation

---

###### **`processing_options:gnss_general:sys_options:gps:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:gps:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:gps:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:leo:`**
 ` `


> Options for the LEO constellation

---

###### **`processing_options:gnss_general:sys_options:leo:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:leo:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:leo:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:qzs:`**
 ` `


> Options for the QZS constellation

---

###### **`processing_options:gnss_general:sys_options:qzs:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:qzs:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:qzs:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:gnss_general:sys_options:sbs:`**
 ` `


> Options for the SBS constellation

---

###### **`processing_options:gnss_general:sys_options:sbs:process:`**
 `false `


Process this constellation

---

###### **`processing_options:gnss_general:sys_options:sbs:code_priorities:`**
 [`[E_ObsCode]`](#e_obscode) `[L1C, L1P, L1Y, L1W, L1M, L1N, L1S, L1L, L1X, L2W, L2P, L2Y, L2C, L2M, L2N, L2D, L2S, L2L, L2X, L5I, L5Q, L5X] `


List of observation codes to use in processing [none,l1c,l1p,l1w,l1y,l1m,l1n,l1s,l1l,l1e,l1a,l1b,l1x,l1z,l2c,l2d,l2s,l2l,l2x,l2p,l2w,l2y,l2m,l2n,l5i,l5q,l5x,l7i,l7q,l7x,l6a,l6b,l6c,l6x,l6z,l6s,l6l,l8i,l8q,l8x,l2i,l2q,l6i,l6q,l3i,l3q,l3x,l1i,l1q,l4a,l4b,l4x,l6e,l1d,l5d,l5p,l9a,l9b,l9c,l9x,l5a,l5b,l5c,l5z,l6d,l6p,l7d,l7p,l7z,l8d,l8p,l8z,auto]

---

###### **`processing_options:gnss_general:sys_options:sbs:ambiguity_resolution:`**
 `false `


Solve carrier phase ambiguities for this constellation

---

###### **`processing_options:process_modes:`**
 ` `


> Aspects of the processing flow may be enabled and disabled according to desired type of solutions

---

###### **`processing_options:process_modes:ppp:`**
 `false `


Perform PPP network or end user mode

---

###### **`processing_options:process_modes:preprocessor:`**
 `true `


Preprocessing and quality checks

---

###### **`processing_options:process_modes:spp:`**
 `true `


Perform SPP on receiver data

---

###### **`processing_options:spp:`**
 ` `


> Configurations for the kalman filter and its sub processes

---

###### **`processing_options:spp:max_lsq_iterations:`**
 `12 `


Maximum number of iterations of least squares allowed for convergence

---

###### **`processing_options:spp:outlier_screening:`**
 ` `


> Statistical checks allow for detection of outliers that exceed their confidence intervals.

---

###### **`processing_options:spp:outlier_screening:chi_square:`**
 ` `


---

###### **`processing_options:spp:outlier_screening:postfit:`**
 ` `


---

###### **`processing_options:spp:outlier_screening:postfit:max_iterations:`**
 `2 `


Maximum number of measurements to exclude using postfit checks while iterating filter

---

###### **`processing_options:spp:outlier_screening:prefit:`**
 ` `


---

###### **`processing_options:spp:outlier_screening:prefit:max_iterations:`**
 `2 `


Maximum number of measurements to exclude using prefit checks before attempting to filter

---

###### **`processing_options:spp:sigma_scaling:`**
 `1 `


Scale applied to measurement noise for spp

---

###### **`processing_options:ppp_filter:`**
 ` `


> Configurations for the kalman filter and its sub processes

---

###### **`processing_options:ppp_filter:ionospheric_components:`**
 ` `


> Slant ionospheric components

---

###### **`processing_options:ppp_filter:ionospheric_components:common_ionosphere:`**
 `true `


Use the same ionosphere state for code and phase observations

---

###### **`processing_options:ppp_filter:ionospheric_components:use_gf_combo:`**
 `false `


Combine 'uncombined' measurements to simulate a geometry-free solution

---

###### **`processing_options:ppp_filter:ionospheric_components:use_if_combo:`**
 `false `


Combine 'uncombined' measurements to simulate an ionosphere-free solution

---

###### **`processing_options:ppp_filter:outlier_screening:`**
 ` `


> Statistical checks allow for detection of outliers that exceed their confidence intervals.

---

###### **`processing_options:ppp_filter:outlier_screening:chi_square:`**
 ` `


---

###### **`processing_options:ppp_filter:outlier_screening:postfit:`**
 ` `


---

###### **`processing_options:ppp_filter:outlier_screening:postfit:max_iterations:`**
 `2 `


Maximum number of measurements to exclude using postfit checks while iterating filter

---

###### **`processing_options:ppp_filter:outlier_screening:prefit:`**
 ` `


---

###### **`processing_options:ppp_filter:outlier_screening:prefit:max_iterations:`**
 `2 `


Maximum number of measurements to exclude using prefit checks before attempting to filter

---

###### **`processing_options:minimum_constraints:`**
 ` `


> Receiver coodinates may be aligned to reference frames with minimal external constraints

---

###### **`processing_options:minimum_constraints:outlier_screening:`**
 ` `


> Statistical checks allow for detection of outliers that exceed their confidence intervals.

---

###### **`processing_options:minimum_constraints:outlier_screening:chi_square:`**
 ` `


---

###### **`processing_options:minimum_constraints:outlier_screening:postfit:`**
 ` `


---

###### **`processing_options:minimum_constraints:outlier_screening:postfit:max_iterations:`**
 `2 `


Maximum number of measurements to exclude using postfit checks while iterating filter

---

###### **`processing_options:minimum_constraints:outlier_screening:prefit:`**
 ` `


---

###### **`processing_options:minimum_constraints:outlier_screening:prefit:max_iterations:`**
 `2 `


Maximum number of measurements to exclude using prefit checks before attempting to filter

---

###### **`processing_options:minimum_constraints:enable:`**
 `false `


Transform states by minimal constraints to selected receiver coordinates

---

###### **`processing_options:minimum_constraints:delay:`**
 ` `


> Estimation and application of clock delay adjustment

---

###### **`processing_options:minimum_constraints:delay:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:delay:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:delay:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`processing_options:minimum_constraints:delay:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`processing_options:minimum_constraints:rotation:`**
 ` `


> Estimation and application of angular offsets

---

###### **`processing_options:minimum_constraints:rotation:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:rotation:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:rotation:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`processing_options:minimum_constraints:rotation:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`processing_options:minimum_constraints:scale:`**
 ` `


> Estimation and application of scaling factor

---

###### **`processing_options:minimum_constraints:scale:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:scale:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:scale:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`processing_options:minimum_constraints:scale:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`processing_options:minimum_constraints:translation:`**
 ` `


> Estimation and application of CoG offsets

---

###### **`processing_options:minimum_constraints:translation:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`processing_options:minimum_constraints:translation:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`processing_options:minimum_constraints:translation:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`processing_options:minimum_constraints:translation:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`processing_options:model_error_handling:`**
 ` `


> The kalman filter is capable of automatic statistical integrity modelling

---

###### **`processing_options:model_error_handling:meas_deweighting:`**
 ` `


> Measurements that are outside the expected confidence bounds may be deweighted so that outliers do not contaminate the filtered solution

---

###### **`processing_options:model_error_handling:meas_deweighting:deweight_factor:`**
 `1000 `


Factor to downweight the variance of measurements with statistically detected errors

---

###### **`processing_options:model_error_handling:meas_deweighting:enable:`**
 `true `


Enable deweighting of all rejected measurement

---

###### **`processing_options:model_error_handling:state_deweighting:`**
 ` `


> Any "state" errors cause deweighting of all measurements that reference the state

---

###### **`processing_options:model_error_handling:state_deweighting:deweight_factor:`**
 `1000 `


Factor to downweight the variance of measurements with statistically detected errors

---

###### **`processing_options:model_error_handling:state_deweighting:enable:`**
 `true `


Enable deweighting of all referencing measurements

---

###### **`processing_options:model_error_handling:ambiguities:`**
 ` `


> Cycle slips in ambiguities are primary cause of incorrect gnss modelling and may be reinitialised

---

###### **`processing_options:model_error_handling:ambiguities:outage_reset_limit:`**
 `10 `


Maximum number of epochs with missed phase measurements before the ambiguity associated with the measurement is reset.

---

###### **`processing_options:model_error_handling:ambiguities:phase_reject_limit:`**
 `10 `


Maximum number of phase measurements to reject before the ambiguity associated with the measurement is reset.

---

###### **`processing_options:model_error_handling:ionospheric_components:`**
 ` `


---

###### **`processing_options:model_error_handling:ionospheric_components:outage_reset_limit:`**
 `10 `


Maximum number of epochs with missed measurements before the ionosphere associated with the measurement is reset.

---

## receiver_options:

###### **`receiver_options:`**
 ` `


> Options to configure individual satellites, systems, or global configs

---

###### **`receiver_options:global:`**
 ` `


---

###### **`receiver_options:global:elevation_mask:`**
 `10 `


Minimum elevation for satellites to be processed

---

###### **`receiver_options:global:exclude:`**
 `false `


Exclude receiver from processing

---

###### **`receiver_options:global:error_model:`**
 [`E_NoiseModel`](#e_noisemodel) `ELEVATION_DEPENDENT `


 {uniform,elevation_dependent}

---

###### **`receiver_options:global:code_sigma:`**
 `1 `


Standard deviation of code measurements

---

###### **`receiver_options:global:phase_sigma:`**
 `0.0015 `


Standard deviation of phase measurmeents

---

## satellite_options:

###### **`satellite_options:`**
 ` `


---

###### **`satellite_options:global:`**
 ` `


---

###### **`satellite_options:global:exclude:`**
 `false `


Exclude receiver from processing

---

###### **`satellite_options:G--:`**
 ` `


---

###### **`satellite_options:G--:exclude:`**
 `false `


Exclude receiver from processing

---

###### **`satellite_options:GPS:`**
 ` `


---

###### **`satellite_options:GPS:exclude:`**
 `false `


Exclude receiver from processing

---

## estimation_parameters:

###### **`estimation_parameters:`**
 ` `


---

###### **`estimation_parameters:receivers:`**
 ` `


---

###### **`estimation_parameters:receivers:global:`**
 ` `


---

###### **`estimation_parameters:receivers:global:ambiguities:`**
 ` `


> Integer phase ambiguities

---

###### **`estimation_parameters:receivers:global:ambiguities:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:ambiguities:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:ambiguities:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:ambiguities:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:clock:`**
 ` `


> Clocks

---

###### **`estimation_parameters:receivers:global:clock:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:clock:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:clock:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:clock:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:ion_stec:`**
 ` `


> Ionospheric slant delay

---

###### **`estimation_parameters:receivers:global:ion_stec:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:ion_stec:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:ion_stec:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:ion_stec:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:pos:`**
 ` `


> Position

---

###### **`estimation_parameters:receivers:global:pos:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:pos:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:pos:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:pos:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:pos_rate:`**
 ` `


> Velocity

---

###### **`estimation_parameters:receivers:global:pos_rate:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:pos_rate:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:pos_rate:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:pos_rate:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:trop:`**
 ` `


> Troposphere corrections

---

###### **`estimation_parameters:receivers:global:trop:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:trop:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:trop:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:trop:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:trop_grads:`**
 ` `


> Troposphere gradients

---

###### **`estimation_parameters:receivers:global:trop_grads:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:trop_grads:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:trop_grads:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:trop_grads:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:receivers:global:code_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:code_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:code_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:code_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:receivers:global:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:receivers:global:phase_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:receivers:global:phase_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:receivers:global:phase_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:receivers:global:phase_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:`**
 ` `


---

###### **`estimation_parameters:satellites:global:`**
 ` `


---

###### **`estimation_parameters:satellites:global:clock:`**
 ` `


> Clocks

---

###### **`estimation_parameters:satellites:global:clock:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:clock:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:clock:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:global:clock:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:global:pos:`**
 ` `


> Position

---

###### **`estimation_parameters:satellites:global:pos:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:pos:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:pos:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:global:pos:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:global:pos_rate:`**
 ` `


> Velocity

---

###### **`estimation_parameters:satellites:global:pos_rate:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:pos_rate:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:pos_rate:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:global:pos_rate:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:global:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:satellites:global:code_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:code_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:code_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:global:code_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:global:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:satellites:global:phase_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:global:phase_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:global:phase_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:global:phase_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:G--:`**
 ` `


---

###### **`estimation_parameters:satellites:G--:clock:`**
 ` `


> Clocks

---

###### **`estimation_parameters:satellites:G--:clock:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:G--:clock:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:G--:clock:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:G--:clock:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:G--:pos:`**
 ` `


> Position

---

###### **`estimation_parameters:satellites:G--:pos:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:G--:pos:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:G--:pos:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:G--:pos:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:G--:pos_rate:`**
 ` `


> Velocity

---

###### **`estimation_parameters:satellites:G--:pos_rate:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:G--:pos_rate:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:G--:pos_rate:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:G--:pos_rate:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:G--:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:satellites:G--:code_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:G--:code_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:G--:code_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:G--:code_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:G--:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:satellites:G--:phase_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:G--:phase_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:G--:phase_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:G--:phase_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:GPS:`**
 ` `


---

###### **`estimation_parameters:satellites:GPS:clock:`**
 ` `


> Clocks

---

###### **`estimation_parameters:satellites:GPS:clock:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:GPS:clock:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:GPS:clock:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:GPS:clock:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:GPS:pos:`**
 ` `


> Position

---

###### **`estimation_parameters:satellites:GPS:pos:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:GPS:pos:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:GPS:pos:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:GPS:pos:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:GPS:pos_rate:`**
 ` `


> Velocity

---

###### **`estimation_parameters:satellites:GPS:pos_rate:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:GPS:pos_rate:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:GPS:pos_rate:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:GPS:pos_rate:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:GPS:code_bias:`**
 ` `


> Code bias

---

###### **`estimation_parameters:satellites:GPS:code_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:GPS:code_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:GPS:code_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:GPS:code_bias:apriori_value:`**
 `[0] `


Apriori state values

---

###### **`estimation_parameters:satellites:GPS:phase_bias:`**
 ` `


> Phase bias

---

###### **`estimation_parameters:satellites:GPS:phase_bias:estimated:`**
 `[false] `


Estimate state in kalman filter

---

###### **`estimation_parameters:satellites:GPS:phase_bias:sigma:`**
 `[-1] `


Apriori sigma values - if zero, will be initialised using least squares

---

###### **`estimation_parameters:satellites:GPS:phase_bias:process_noise:`**
 `[0] `


Process noise sigmas

---

###### **`estimation_parameters:satellites:GPS:phase_bias:apriori_value:`**
 `[0] `


Apriori state values

---

## mongo:

###### **`mongo:`**
 ` `


> Mongo is a database used to store results and intermediate values for later analysis and inter-process communication

---

###### **`mongo:enable:`**
 [`E_Mongo`](#e_mongo) `NONE `


Enable and connect to mongo database {none,primary,secondary,both}

---

###### **`mongo:delete_history:`**
 [`E_Mongo`](#e_mongo) `NONE `


Drop the collection in the database at the beginning of the run to only show fresh data {none,primary,secondary,both}

---

###### **`mongo:output_components:`**
 [`E_Mongo`](#e_mongo) `NONE `


Output components of measurements {none,primary,secondary,both}

---

###### **`mongo:output_measurements:`**
 [`E_Mongo`](#e_mongo) `NONE `


Output measurements and their residuals {none,primary,secondary,both}

---

###### **`mongo:output_states:`**
 [`E_Mongo`](#e_mongo) `NONE `


Output states {none,primary,secondary,both}

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

- [`processing_options:ambiguity_resolution:mode:`](#processing_optionsambiguity_resolutionmode)
---

### E_ChiSqMode

Valid enum values are:
- `none`
- `innovation`
- `measurement`
- `state`

For options:

- [`processing_options:minimum_constraints:outlier_screening:chi_square:mode:`](#processing_optionsminimum_constraintsoutlier_screeningchi_squaremode)
- [`processing_options:ppp_filter:outlier_screening:chi_square:mode:`](#processing_optionsppp_filteroutlier_screeningchi_squaremode)
- [`processing_options:ion_filter:outlier_screening:chi_square:mode:`](#processing_optionsion_filteroutlier_screeningchi_squaremode)
- [`processing_options:spp:outlier_screening:chi_square:mode:`](#processing_optionssppoutlier_screeningchi_squaremode)
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
- [`processing_options:minimum_constraints:rts:inverter:`](#processing_optionsminimum_constraintsrtsinverter)
- [`processing_options:ppp_filter:inverter:`](#processing_optionsppp_filterinverter)
- [`processing_options:ppp_filter:rts:inverter:`](#processing_optionsppp_filterrtsinverter)
- [`processing_options:ion_filter:inverter:`](#processing_optionsion_filterinverter)
- [`processing_options:ion_filter:rts:inverter:`](#processing_optionsion_filterrtsinverter)
---

### E_IonoMapFn

Valid enum values are:
- `slm`
- `mslm`
- `mlm`
- `klobuchar`

For options:

- [`receiver_options:global:models:ionospheric_components:mapping_function:`](#receiver_optionsglobalmodelsionospheric_componentsmapping_function)
- [`receiver_options:global:GPS:models:ionospheric_components:mapping_function:`](#receiver_optionsglobalGPSmodelsionospheric_componentsmapping_function)
- [`receiver_options:global:GPS:L1W:models:ionospheric_components:mapping_function:`](#receiver_optionsglobalGPSL1Wmodelsionospheric_componentsmapping_function)
- [`receiver_options:XMPL:models:ionospheric_components:mapping_function:`](#receiver_optionsXMPLmodelsionospheric_componentsmapping_function)
- [`receiver_options:XMPL:GPS:models:ionospheric_components:mapping_function:`](#receiver_optionsXMPLGPSmodelsionospheric_componentsmapping_function)
- [`receiver_options:XMPL:GPS:L1W:models:ionospheric_components:mapping_function:`](#receiver_optionsXMPLGPSL1Wmodelsionospheric_componentsmapping_function)
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

- [`processing_options:ppp_filter:ionospheric_components:corr_mode:`](#processing_optionsppp_filterionospheric_componentscorr_mode)
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

- [`processing_options:ion_filter:model:`](#processing_optionsion_filtermodel)
---

### E_Mincon

Valid enum values are:
- `pseudo_obs`
- `weight_matrix`
- `variance_inverse`
- `covariance_inverse`

For options:

- [`processing_options:minimum_constraints:application_mode:`](#processing_optionsminimum_constraintsapplication_mode)
---

### E_Mongo

Valid enum values are:
- `none`
- `primary`
- `secondary`
- `both`

For options:

- [`mongo:enable:`](#mongoenable)
- [`mongo:output_measurements:`](#mongooutput_measurements)
- [`mongo:output_components:`](#mongooutput_components)
- [`mongo:output_states:`](#mongooutput_states)
- [`mongo:output_config:`](#mongooutput_config)
- [`mongo:output_trace:`](#mongooutput_trace)
- [`mongo:output_test_stats:`](#mongooutput_test_stats)
- [`mongo:output_logs:`](#mongooutput_logs)
- [`mongo:output_ssr_precursors:`](#mongooutput_ssr_precursors)
- [`mongo:delete_history:`](#mongodelete_history)
- [`mongo:cull_history:`](#mongocull_history)
- [`mongo:use_predictions:`](#mongouse_predictions)
- [`mongo:output_predictions:`](#mongooutput_predictions)
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

- [`satellite_options:global:error_model:`](#satellite_optionsglobalerror_model)
- [`satellite_options:global:L1W:error_model:`](#satellite_optionsglobalL1Werror_model)
- [`satellite_options:GPS:error_model:`](#satellite_optionsGPSerror_model)
- [`satellite_options:GPS:L1W:error_model:`](#satellite_optionsGPSL1Werror_model)
- [`satellite_options:G--:error_model:`](#satellite_optionsG--error_model)
- [`satellite_options:G--:L1W:error_model:`](#satellite_optionsG--L1Werror_model)
- [`receiver_options:global:error_model:`](#receiver_optionsglobalerror_model)
- [`receiver_options:global:GPS:error_model:`](#receiver_optionsglobalGPSerror_model)
- [`receiver_options:global:GPS:L1W:error_model:`](#receiver_optionsglobalGPSL1Werror_model)
- [`receiver_options:XMPL:error_model:`](#receiver_optionsXMPLerror_model)
- [`receiver_options:XMPL:GPS:error_model:`](#receiver_optionsXMPLGPSerror_model)
- [`receiver_options:XMPL:GPS:L1W:error_model:`](#receiver_optionsXMPLGPSL1Werror_model)
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
- `auto`

For options:

- [`processing_options:gnss_general:sys_options:gps:code_priorities:`](#processing_optionsgnss_generalsys_optionsgpscode_priorities)
- [`processing_options:gnss_general:sys_options:gal:code_priorities:`](#processing_optionsgnss_generalsys_optionsgalcode_priorities)
- [`processing_options:gnss_general:sys_options:glo:code_priorities:`](#processing_optionsgnss_generalsys_optionsglocode_priorities)
- [`processing_options:gnss_general:sys_options:qzs:code_priorities:`](#processing_optionsgnss_generalsys_optionsqzscode_priorities)
- [`processing_options:gnss_general:sys_options:sbs:code_priorities:`](#processing_optionsgnss_generalsys_optionssbscode_priorities)
- [`processing_options:gnss_general:sys_options:bds:code_priorities:`](#processing_optionsgnss_generalsys_optionsbdscode_priorities)
- [`processing_options:gnss_general:sys_options:leo:code_priorities:`](#processing_optionsgnss_generalsys_optionsleocode_priorities)
- [`satellite_options:global:clock_codes:`](#satellite_optionsglobalclock_codes)
- [`satellite_options:global:L1W:clock_codes:`](#satellite_optionsglobalL1Wclock_codes)
- [`satellite_options:GPS:clock_codes:`](#satellite_optionsGPSclock_codes)
- [`satellite_options:GPS:L1W:clock_codes:`](#satellite_optionsGPSL1Wclock_codes)
- [`satellite_options:G--:clock_codes:`](#satellite_optionsG--clock_codes)
- [`satellite_options:G--:L1W:clock_codes:`](#satellite_optionsG--L1Wclock_codes)
- [`receiver_options:global:clock_codes:`](#receiver_optionsglobalclock_codes)
- [`receiver_options:global:zero_dcb_codes:`](#receiver_optionsglobalzero_dcb_codes)
- [`receiver_options:global:rinex2:rnx_code_conversions:NONE:`](#receiver_optionsglobalrinex2rnx_code_conversionsNONE)
- [`receiver_options:global:rinex2:rnx_phase_conversions:NONE:`](#receiver_optionsglobalrinex2rnx_phase_conversionsNONE)
- [`receiver_options:global:rinex2:rnx_code_conversions:P1:`](#receiver_optionsglobalrinex2rnx_code_conversionsP1)
- [`receiver_options:global:rinex2:rnx_phase_conversions:P1:`](#receiver_optionsglobalrinex2rnx_phase_conversionsP1)
- [`receiver_options:global:rinex2:rnx_code_conversions:P2:`](#receiver_optionsglobalrinex2rnx_code_conversionsP2)
- [`receiver_options:global:rinex2:rnx_phase_conversions:P2:`](#receiver_optionsglobalrinex2rnx_phase_conversionsP2)
- [`receiver_options:global:rinex2:rnx_code_conversions:C1:`](#receiver_optionsglobalrinex2rnx_code_conversionsC1)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C1:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC1)
- [`receiver_options:global:rinex2:rnx_code_conversions:C2:`](#receiver_optionsglobalrinex2rnx_code_conversionsC2)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C2:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC2)
- [`receiver_options:global:rinex2:rnx_code_conversions:C3:`](#receiver_optionsglobalrinex2rnx_code_conversionsC3)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C3:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC3)
- [`receiver_options:global:rinex2:rnx_code_conversions:C4:`](#receiver_optionsglobalrinex2rnx_code_conversionsC4)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C4:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC4)
- [`receiver_options:global:rinex2:rnx_code_conversions:C5:`](#receiver_optionsglobalrinex2rnx_code_conversionsC5)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C5:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC5)
- [`receiver_options:global:rinex2:rnx_code_conversions:C6:`](#receiver_optionsglobalrinex2rnx_code_conversionsC6)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C6:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC6)
- [`receiver_options:global:rinex2:rnx_code_conversions:C7:`](#receiver_optionsglobalrinex2rnx_code_conversionsC7)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C7:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC7)
- [`receiver_options:global:rinex2:rnx_code_conversions:C8:`](#receiver_optionsglobalrinex2rnx_code_conversionsC8)
- [`receiver_options:global:rinex2:rnx_phase_conversions:C8:`](#receiver_optionsglobalrinex2rnx_phase_conversionsC8)
- [`receiver_options:global:rinex2:rnx_code_conversions:L1:`](#receiver_optionsglobalrinex2rnx_code_conversionsL1)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L1:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL1)
- [`receiver_options:global:rinex2:rnx_code_conversions:L2:`](#receiver_optionsglobalrinex2rnx_code_conversionsL2)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L2:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL2)
- [`receiver_options:global:rinex2:rnx_code_conversions:L3:`](#receiver_optionsglobalrinex2rnx_code_conversionsL3)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L3:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL3)
- [`receiver_options:global:rinex2:rnx_code_conversions:L4:`](#receiver_optionsglobalrinex2rnx_code_conversionsL4)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L4:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL4)
- [`receiver_options:global:rinex2:rnx_code_conversions:L5:`](#receiver_optionsglobalrinex2rnx_code_conversionsL5)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L5:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL5)
- [`receiver_options:global:rinex2:rnx_code_conversions:L6:`](#receiver_optionsglobalrinex2rnx_code_conversionsL6)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L6:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL6)
- [`receiver_options:global:rinex2:rnx_code_conversions:L7:`](#receiver_optionsglobalrinex2rnx_code_conversionsL7)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L7:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL7)
- [`receiver_options:global:rinex2:rnx_code_conversions:L8:`](#receiver_optionsglobalrinex2rnx_code_conversionsL8)
- [`receiver_options:global:rinex2:rnx_phase_conversions:L8:`](#receiver_optionsglobalrinex2rnx_phase_conversionsL8)
- [`receiver_options:global:rinex2:rnx_code_conversions:LA:`](#receiver_optionsglobalrinex2rnx_code_conversionsLA)
- [`receiver_options:global:rinex2:rnx_phase_conversions:LA:`](#receiver_optionsglobalrinex2rnx_phase_conversionsLA)
- [`receiver_options:global:GPS:clock_codes:`](#receiver_optionsglobalGPSclock_codes)
- [`receiver_options:global:GPS:zero_dcb_codes:`](#receiver_optionsglobalGPSzero_dcb_codes)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:NONE:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsNONE)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:NONE:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsNONE)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:P1:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsP1)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:P1:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsP1)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:P2:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsP2)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:P2:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsP2)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C1:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC1)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C1:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC1)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C2:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC2)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C2:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC2)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C3:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC3)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C3:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC3)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C4:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC4)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C4:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC4)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C5:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC5)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C5:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC5)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C6:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC6)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C6:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC6)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C7:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC7)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C7:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC7)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:C8:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsC8)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:C8:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsC8)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L1:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL1)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L1:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL1)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L2:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL2)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L2:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL2)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L3:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL3)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L3:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL3)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L4:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL4)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L4:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL4)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L5:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL5)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L5:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL5)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L6:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL6)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L6:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL6)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L7:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL7)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L7:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL7)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:L8:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsL8)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:L8:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsL8)
- [`receiver_options:global:GPS:rinex2:rnx_code_conversions:LA:`](#receiver_optionsglobalGPSrinex2rnx_code_conversionsLA)
- [`receiver_options:global:GPS:rinex2:rnx_phase_conversions:LA:`](#receiver_optionsglobalGPSrinex2rnx_phase_conversionsLA)
- [`receiver_options:global:GPS:L1W:clock_codes:`](#receiver_optionsglobalGPSL1Wclock_codes)
- [`receiver_options:global:GPS:L1W:zero_dcb_codes:`](#receiver_optionsglobalGPSL1Wzero_dcb_codes)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:NONE:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsNONE)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:NONE:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsNONE)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:P1:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsP1)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:P1:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsP1)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:P2:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsP2)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:P2:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsP2)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C1:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC1)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C1:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC1)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C2:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC2)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C2:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC2)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C3:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC3)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C3:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC3)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C4:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC4)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C4:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC4)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C5:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC5)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C5:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC5)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C6:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC6)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C6:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC6)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C7:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC7)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C7:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC7)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:C8:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsC8)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:C8:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsC8)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L1:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL1)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L1:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL1)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L2:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL2)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L2:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL2)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L3:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL3)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L3:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL3)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L4:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL4)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L4:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL4)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L5:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL5)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L5:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL5)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L6:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL6)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L6:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL6)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L7:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL7)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L7:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL7)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:L8:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsL8)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:L8:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsL8)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_code_conversions:LA:`](#receiver_optionsglobalGPSL1Wrinex2rnx_code_conversionsLA)
- [`receiver_options:global:GPS:L1W:rinex2:rnx_phase_conversions:LA:`](#receiver_optionsglobalGPSL1Wrinex2rnx_phase_conversionsLA)
- [`receiver_options:XMPL:clock_codes:`](#receiver_optionsXMPLclock_codes)
- [`receiver_options:XMPL:zero_dcb_codes:`](#receiver_optionsXMPLzero_dcb_codes)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:NONE:`](#receiver_optionsXMPLrinex2rnx_code_conversionsNONE)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:NONE:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsNONE)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:P1:`](#receiver_optionsXMPLrinex2rnx_code_conversionsP1)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:P1:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsP1)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:P2:`](#receiver_optionsXMPLrinex2rnx_code_conversionsP2)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:P2:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsP2)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C1:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC1)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C1:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC1)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C2:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC2)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C2:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC2)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C3:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC3)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C3:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC3)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C4:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC4)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C4:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC4)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C5:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC5)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C5:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC5)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C6:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC6)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C6:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC6)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C7:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC7)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C7:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC7)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:C8:`](#receiver_optionsXMPLrinex2rnx_code_conversionsC8)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:C8:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsC8)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L1:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL1)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L1:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL1)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L2:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL2)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L2:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL2)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L3:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL3)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L3:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL3)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L4:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL4)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L4:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL4)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L5:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL5)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L5:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL5)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L6:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL6)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L6:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL6)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L7:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL7)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L7:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL7)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:L8:`](#receiver_optionsXMPLrinex2rnx_code_conversionsL8)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:L8:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsL8)
- [`receiver_options:XMPL:rinex2:rnx_code_conversions:LA:`](#receiver_optionsXMPLrinex2rnx_code_conversionsLA)
- [`receiver_options:XMPL:rinex2:rnx_phase_conversions:LA:`](#receiver_optionsXMPLrinex2rnx_phase_conversionsLA)
- [`receiver_options:XMPL:GPS:clock_codes:`](#receiver_optionsXMPLGPSclock_codes)
- [`receiver_options:XMPL:GPS:zero_dcb_codes:`](#receiver_optionsXMPLGPSzero_dcb_codes)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:NONE:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsNONE)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:NONE:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsNONE)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:P1:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsP1)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:P1:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsP1)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:P2:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsP2)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:P2:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsP2)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C1:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC1)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C1:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC1)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C2:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC2)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C2:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC2)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C3:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC3)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C3:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC3)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C4:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC4)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C4:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC4)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C5:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC5)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C5:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC5)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C6:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC6)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C6:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC6)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C7:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC7)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C7:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC7)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:C8:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsC8)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:C8:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsC8)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L1:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL1)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L1:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL1)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L2:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL2)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L2:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL2)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L3:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL3)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L3:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL3)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L4:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL4)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L4:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL4)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L5:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL5)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L5:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL5)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L6:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL6)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L6:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL6)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L7:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL7)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L7:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL7)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:L8:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsL8)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:L8:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsL8)
- [`receiver_options:XMPL:GPS:rinex2:rnx_code_conversions:LA:`](#receiver_optionsXMPLGPSrinex2rnx_code_conversionsLA)
- [`receiver_options:XMPL:GPS:rinex2:rnx_phase_conversions:LA:`](#receiver_optionsXMPLGPSrinex2rnx_phase_conversionsLA)
- [`receiver_options:XMPL:GPS:L1W:clock_codes:`](#receiver_optionsXMPLGPSL1Wclock_codes)
- [`receiver_options:XMPL:GPS:L1W:zero_dcb_codes:`](#receiver_optionsXMPLGPSL1Wzero_dcb_codes)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:NONE:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsNONE)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:NONE:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsNONE)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:P1:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsP1)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:P1:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsP1)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:P2:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsP2)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:P2:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsP2)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C1:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC1)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C1:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC1)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C2:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC2)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C2:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC2)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C3:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC3)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C3:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC3)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C4:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC4)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C4:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC4)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C5:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC5)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C5:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC5)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C6:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC6)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C6:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC6)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C7:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC7)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C7:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC7)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:C8:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsC8)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:C8:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsC8)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L1:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL1)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L1:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL1)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L2:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL2)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L2:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL2)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L3:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL3)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L3:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL3)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L4:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL4)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L4:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL4)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L5:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL5)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L5:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL5)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L6:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL6)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L6:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL6)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L7:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL7)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L7:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL7)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:L8:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsL8)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:L8:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsL8)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_code_conversions:LA:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_code_conversionsLA)
- [`receiver_options:XMPL:GPS:L1W:rinex2:rnx_phase_conversions:LA:`](#receiver_optionsXMPLGPSL1Wrinex2rnx_phase_conversionsLA)
---

### E_OffsetType

Valid enum values are:
- `unspecified`
- `apc`
- `com`

For options:

- [`inputs:satellite_data:rtcm_inputs:ssr_antenna_offset:`](#inputssatellite_datartcm_inputsssr_antenna_offset)
---

### E_OrbexRecord

Valid enum values are:
- `pcs`
- `vcs`
- `cpc`
- `cvc`
- `pos`
- `vel`
- `clk`
- `crt`
- `att`

For options:

- [`outputs:orbex:record_types:`](#outputsorbexrecord_types)
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
- [`processing_options:minimum_constraints:delay:process_noise_dt:`](#processing_optionsminimum_constraintsdelayprocess_noise_dt)
- [`processing_options:minimum_constraints:scale:process_noise_dt:`](#processing_optionsminimum_constraintsscaleprocess_noise_dt)
- [`processing_options:minimum_constraints:rotation:process_noise_dt:`](#processing_optionsminimum_constraintsrotationprocess_noise_dt)
- [`processing_options:minimum_constraints:translation:process_noise_dt:`](#processing_optionsminimum_constraintstranslationprocess_noise_dt)
- [`processing_options:predictions:interval_units:`](#processing_optionspredictionsinterval_units)
- [`processing_options:predictions:interval_units:`](#processing_optionspredictionsinterval_units)
- [`processing_options:predictions:duration_units:`](#processing_optionspredictionsduration_units)
- [`processing_options:predictions:duration_units:`](#processing_optionspredictionsduration_units)
- [`estimation_parameters:global_models:eop:process_noise_dt:`](#estimation_parametersglobal_modelseopprocess_noise_dt)
- [`estimation_parameters:global_models:eop_rates:process_noise_dt:`](#estimation_parametersglobal_modelseop_ratesprocess_noise_dt)
- [`estimation_parameters:global_models:ion:process_noise_dt:`](#estimation_parametersglobal_modelsionprocess_noise_dt)
- [`estimation_parameters:satellites:global:orientation:process_noise_dt:`](#estimation_parameterssatellitesglobalorientationprocess_noise_dt)
- [`estimation_parameters:satellites:global:gyro_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalgyro_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:accelerometer_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalaccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:gyro_scale:process_noise_dt:`](#estimation_parameterssatellitesglobalgyro_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:global:accelerometer_scale:process_noise_dt:`](#estimation_parameterssatellitesglobalaccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:global:imu_offset:process_noise_dt:`](#estimation_parameterssatellitesglobalimu_offsetprocess_noise_dt)
- [`estimation_parameters:satellites:global:clock:process_noise_dt:`](#estimation_parameterssatellitesglobalclockprocess_noise_dt)
- [`estimation_parameters:satellites:global:clock_rate:process_noise_dt:`](#estimation_parameterssatellitesglobalclock_rateprocess_noise_dt)
- [`estimation_parameters:satellites:global:pos:process_noise_dt:`](#estimation_parameterssatellitesglobalposprocess_noise_dt)
- [`estimation_parameters:satellites:global:pos_rate:process_noise_dt:`](#estimation_parameterssatellitesglobalpos_rateprocess_noise_dt)
- [`estimation_parameters:satellites:global:orbit:process_noise_dt:`](#estimation_parameterssatellitesglobalorbitprocess_noise_dt)
- [`estimation_parameters:satellites:global:pco:process_noise_dt:`](#estimation_parameterssatellitesglobalpcoprocess_noise_dt)
- [`estimation_parameters:satellites:global:code_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalcode_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:phase_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalphase_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:emp_d_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_d_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_d_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_d_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_d_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_d_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_d_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_d_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_d_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_d_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_y_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_y_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_y_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_y_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_y_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_y_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_y_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_y_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_y_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_y_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_b_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_b_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_b_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_b_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_b_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_b_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_b_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_b_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_b_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_b_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_r_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_r_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_r_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_r_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_r_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_r_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_r_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_r_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_r_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_r_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_t_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_t_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_t_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_t_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_t_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_t_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_t_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_t_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_t_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_t_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_n_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_n_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_n_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_n_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_n_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_n_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_n_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_n_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_n_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_n_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_p_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_p_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_p_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_p_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_p_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_p_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_p_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_p_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_p_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_p_4process_noise_dt)
- [`estimation_parameters:satellites:global:emp_q_0:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_q_0process_noise_dt)
- [`estimation_parameters:satellites:global:emp_q_1:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_q_1process_noise_dt)
- [`estimation_parameters:satellites:global:emp_q_2:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_q_2process_noise_dt)
- [`estimation_parameters:satellites:global:emp_q_3:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_q_3process_noise_dt)
- [`estimation_parameters:satellites:global:emp_q_4:process_noise_dt:`](#estimation_parameterssatellitesglobalemp_q_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:orientation:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Worientationprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:gyro_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wgyro_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:accelerometer_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Waccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:gyro_scale:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wgyro_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:accelerometer_scale:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Waccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:imu_offset:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wimu_offsetprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:clock:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wclockprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:clock_rate:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wclock_rateprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:pos:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wposprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:pos_rate:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wpos_rateprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:orbit:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Worbitprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:pco:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wpcoprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:code_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wcode_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:phase_bias:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wphase_biasprocess_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_d_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_d_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_d_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_d_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_d_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_d_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_d_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_d_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_d_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_d_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_y_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_y_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_y_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_y_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_y_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_y_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_y_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_y_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_y_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_y_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_b_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_b_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_b_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_b_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_b_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_b_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_b_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_b_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_b_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_b_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_r_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_r_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_r_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_r_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_r_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_r_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_r_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_r_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_r_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_r_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_t_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_t_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_t_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_t_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_t_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_t_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_t_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_t_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_t_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_t_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_n_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_n_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_n_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_n_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_n_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_n_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_n_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_n_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_n_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_n_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_p_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_p_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_p_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_p_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_p_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_p_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_p_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_p_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_p_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_p_4process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_q_0:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_q_0process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_q_1:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_q_1process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_q_2:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_q_2process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_q_3:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_q_3process_noise_dt)
- [`estimation_parameters:satellites:global:L1W:emp_q_4:process_noise_dt:`](#estimation_parameterssatellitesglobalL1Wemp_q_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:orientation:process_noise_dt:`](#estimation_parameterssatellitesGPSorientationprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:gyro_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSgyro_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:accelerometer_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSaccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:gyro_scale:process_noise_dt:`](#estimation_parameterssatellitesGPSgyro_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:accelerometer_scale:process_noise_dt:`](#estimation_parameterssatellitesGPSaccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:imu_offset:process_noise_dt:`](#estimation_parameterssatellitesGPSimu_offsetprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:clock:process_noise_dt:`](#estimation_parameterssatellitesGPSclockprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:clock_rate:process_noise_dt:`](#estimation_parameterssatellitesGPSclock_rateprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:pos:process_noise_dt:`](#estimation_parameterssatellitesGPSposprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:pos_rate:process_noise_dt:`](#estimation_parameterssatellitesGPSpos_rateprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:orbit:process_noise_dt:`](#estimation_parameterssatellitesGPSorbitprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:pco:process_noise_dt:`](#estimation_parameterssatellitesGPSpcoprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:code_bias:process_noise_dt:`](#estimation_parameterssatellitesGPScode_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:phase_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSphase_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_d_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_d_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_d_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_d_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_d_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_d_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_d_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_d_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_d_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_d_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_y_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_y_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_y_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_y_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_y_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_y_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_y_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_y_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_y_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_y_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_b_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_b_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_b_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_b_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_b_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_b_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_b_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_b_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_b_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_b_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_r_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_r_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_r_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_r_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_r_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_r_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_r_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_r_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_r_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_r_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_t_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_t_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_t_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_t_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_t_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_t_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_t_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_t_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_t_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_t_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_n_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_n_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_n_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_n_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_n_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_n_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_n_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_n_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_n_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_n_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_p_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_p_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_p_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_p_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_p_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_p_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_p_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_p_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_p_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_p_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_q_0:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_q_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_q_1:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_q_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_q_2:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_q_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_q_3:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_q_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:emp_q_4:process_noise_dt:`](#estimation_parameterssatellitesGPSemp_q_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:orientation:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Worientationprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:gyro_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wgyro_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:accelerometer_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Waccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:gyro_scale:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wgyro_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:accelerometer_scale:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Waccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:imu_offset:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wimu_offsetprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:clock:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wclockprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:clock_rate:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wclock_rateprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:pos:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wposprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:pos_rate:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wpos_rateprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:orbit:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Worbitprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:pco:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wpcoprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:code_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wcode_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:phase_bias:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wphase_biasprocess_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_d_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_d_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_d_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_d_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_d_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_d_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_d_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_d_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_d_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_d_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_y_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_y_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_y_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_y_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_y_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_y_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_y_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_y_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_y_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_y_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_b_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_b_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_b_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_b_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_b_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_b_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_b_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_b_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_b_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_b_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_r_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_r_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_r_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_r_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_r_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_r_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_r_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_r_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_r_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_r_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_t_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_t_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_t_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_t_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_t_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_t_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_t_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_t_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_t_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_t_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_n_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_n_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_n_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_n_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_n_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_n_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_n_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_n_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_n_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_n_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_p_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_p_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_p_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_p_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_p_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_p_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_p_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_p_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_p_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_p_4process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_q_0:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_q_0process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_q_1:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_q_1process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_q_2:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_q_2process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_q_3:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_q_3process_noise_dt)
- [`estimation_parameters:satellites:GPS:L1W:emp_q_4:process_noise_dt:`](#estimation_parameterssatellitesGPSL1Wemp_q_4process_noise_dt)
- [`estimation_parameters:satellites:G--:orientation:process_noise_dt:`](#estimation_parameterssatellitesG--orientationprocess_noise_dt)
- [`estimation_parameters:satellites:G--:gyro_bias:process_noise_dt:`](#estimation_parameterssatellitesG--gyro_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:accelerometer_bias:process_noise_dt:`](#estimation_parameterssatellitesG--accelerometer_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:gyro_scale:process_noise_dt:`](#estimation_parameterssatellitesG--gyro_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:G--:accelerometer_scale:process_noise_dt:`](#estimation_parameterssatellitesG--accelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:G--:imu_offset:process_noise_dt:`](#estimation_parameterssatellitesG--imu_offsetprocess_noise_dt)
- [`estimation_parameters:satellites:G--:clock:process_noise_dt:`](#estimation_parameterssatellitesG--clockprocess_noise_dt)
- [`estimation_parameters:satellites:G--:clock_rate:process_noise_dt:`](#estimation_parameterssatellitesG--clock_rateprocess_noise_dt)
- [`estimation_parameters:satellites:G--:pos:process_noise_dt:`](#estimation_parameterssatellitesG--posprocess_noise_dt)
- [`estimation_parameters:satellites:G--:pos_rate:process_noise_dt:`](#estimation_parameterssatellitesG--pos_rateprocess_noise_dt)
- [`estimation_parameters:satellites:G--:orbit:process_noise_dt:`](#estimation_parameterssatellitesG--orbitprocess_noise_dt)
- [`estimation_parameters:satellites:G--:pco:process_noise_dt:`](#estimation_parameterssatellitesG--pcoprocess_noise_dt)
- [`estimation_parameters:satellites:G--:code_bias:process_noise_dt:`](#estimation_parameterssatellitesG--code_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:phase_bias:process_noise_dt:`](#estimation_parameterssatellitesG--phase_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:emp_d_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_d_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_d_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_d_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_d_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_d_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_d_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_d_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_d_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_d_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_y_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_y_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_y_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_y_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_y_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_y_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_y_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_y_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_y_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_y_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_b_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_b_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_b_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_b_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_b_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_b_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_b_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_b_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_b_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_b_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_r_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_r_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_r_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_r_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_r_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_r_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_r_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_r_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_r_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_r_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_t_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_t_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_t_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_t_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_t_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_t_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_t_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_t_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_t_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_t_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_n_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_n_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_n_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_n_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_n_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_n_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_n_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_n_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_n_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_n_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_p_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_p_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_p_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_p_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_p_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_p_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_p_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_p_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_p_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_p_4process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_q_0:process_noise_dt:`](#estimation_parameterssatellitesG--emp_q_0process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_q_1:process_noise_dt:`](#estimation_parameterssatellitesG--emp_q_1process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_q_2:process_noise_dt:`](#estimation_parameterssatellitesG--emp_q_2process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_q_3:process_noise_dt:`](#estimation_parameterssatellitesG--emp_q_3process_noise_dt)
- [`estimation_parameters:satellites:G--:emp_q_4:process_noise_dt:`](#estimation_parameterssatellitesG--emp_q_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:orientation:process_noise_dt:`](#estimation_parameterssatellitesG--L1Worientationprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:gyro_bias:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wgyro_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:accelerometer_bias:process_noise_dt:`](#estimation_parameterssatellitesG--L1Waccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:gyro_scale:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wgyro_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:accelerometer_scale:process_noise_dt:`](#estimation_parameterssatellitesG--L1Waccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:imu_offset:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wimu_offsetprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:clock:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wclockprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:clock_rate:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wclock_rateprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:pos:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wposprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:pos_rate:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wpos_rateprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:orbit:process_noise_dt:`](#estimation_parameterssatellitesG--L1Worbitprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:pco:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wpcoprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:code_bias:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wcode_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:phase_bias:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wphase_biasprocess_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_d_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_d_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_d_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_d_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_d_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_d_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_d_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_d_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_d_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_d_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_y_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_y_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_y_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_y_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_y_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_y_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_y_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_y_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_y_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_y_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_b_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_b_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_b_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_b_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_b_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_b_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_b_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_b_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_b_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_b_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_r_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_r_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_r_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_r_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_r_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_r_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_r_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_r_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_r_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_r_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_t_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_t_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_t_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_t_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_t_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_t_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_t_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_t_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_t_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_t_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_n_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_n_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_n_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_n_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_n_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_n_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_n_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_n_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_n_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_n_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_p_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_p_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_p_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_p_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_p_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_p_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_p_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_p_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_p_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_p_4process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_q_0:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_q_0process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_q_1:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_q_1process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_q_2:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_q_2process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_q_3:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_q_3process_noise_dt)
- [`estimation_parameters:satellites:G--:L1W:emp_q_4:process_noise_dt:`](#estimation_parameterssatellitesG--L1Wemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:global:orientation:process_noise_dt:`](#estimation_parametersreceiversglobalorientationprocess_noise_dt)
- [`estimation_parameters:receivers:global:gyro_bias:process_noise_dt:`](#estimation_parametersreceiversglobalgyro_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:accelerometer_bias:process_noise_dt:`](#estimation_parametersreceiversglobalaccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:gyro_scale:process_noise_dt:`](#estimation_parametersreceiversglobalgyro_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:global:accelerometer_scale:process_noise_dt:`](#estimation_parametersreceiversglobalaccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:global:imu_offset:process_noise_dt:`](#estimation_parametersreceiversglobalimu_offsetprocess_noise_dt)
- [`estimation_parameters:receivers:global:clock:process_noise_dt:`](#estimation_parametersreceiversglobalclockprocess_noise_dt)
- [`estimation_parameters:receivers:global:clock_rate:process_noise_dt:`](#estimation_parametersreceiversglobalclock_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:pos:process_noise_dt:`](#estimation_parametersreceiversglobalposprocess_noise_dt)
- [`estimation_parameters:receivers:global:pos_rate:process_noise_dt:`](#estimation_parametersreceiversglobalpos_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:orbit:process_noise_dt:`](#estimation_parametersreceiversglobalorbitprocess_noise_dt)
- [`estimation_parameters:receivers:global:pco:process_noise_dt:`](#estimation_parametersreceiversglobalpcoprocess_noise_dt)
- [`estimation_parameters:receivers:global:code_bias:process_noise_dt:`](#estimation_parametersreceiversglobalcode_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:phase_bias:process_noise_dt:`](#estimation_parametersreceiversglobalphase_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:emp_d_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_d_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_d_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_d_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_d_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_d_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_d_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_d_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_d_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_d_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_y_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_y_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_y_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_y_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_y_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_y_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_y_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_y_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_y_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_y_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_b_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_b_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_b_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_b_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_b_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_b_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_b_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_b_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_b_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_b_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_r_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_r_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_r_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_r_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_r_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_r_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_r_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_r_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_r_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_r_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_t_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_t_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_t_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_t_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_t_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_t_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_t_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_t_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_t_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_t_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_n_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_n_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_n_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_n_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_n_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_n_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_n_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_n_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_n_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_n_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_p_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_p_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_p_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_p_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_p_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_p_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_p_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_p_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_p_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_p_4process_noise_dt)
- [`estimation_parameters:receivers:global:emp_q_0:process_noise_dt:`](#estimation_parametersreceiversglobalemp_q_0process_noise_dt)
- [`estimation_parameters:receivers:global:emp_q_1:process_noise_dt:`](#estimation_parametersreceiversglobalemp_q_1process_noise_dt)
- [`estimation_parameters:receivers:global:emp_q_2:process_noise_dt:`](#estimation_parametersreceiversglobalemp_q_2process_noise_dt)
- [`estimation_parameters:receivers:global:emp_q_3:process_noise_dt:`](#estimation_parametersreceiversglobalemp_q_3process_noise_dt)
- [`estimation_parameters:receivers:global:emp_q_4:process_noise_dt:`](#estimation_parametersreceiversglobalemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:global:strain_rate:process_noise_dt:`](#estimation_parametersreceiversglobalstrain_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:ambiguities:process_noise_dt:`](#estimation_parametersreceiversglobalambiguitiesprocess_noise_dt)
- [`estimation_parameters:receivers:global:pcv:process_noise_dt:`](#estimation_parametersreceiversglobalpcvprocess_noise_dt)
- [`estimation_parameters:receivers:global:ion_stec:process_noise_dt:`](#estimation_parametersreceiversglobalion_stecprocess_noise_dt)
- [`estimation_parameters:receivers:global:ion_model:process_noise_dt:`](#estimation_parametersreceiversglobalion_modelprocess_noise_dt)
- [`estimation_parameters:receivers:global:slr_range_bias:process_noise_dt:`](#estimation_parametersreceiversglobalslr_range_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:slr_time_bias:process_noise_dt:`](#estimation_parametersreceiversglobalslr_time_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:trop:process_noise_dt:`](#estimation_parametersreceiversglobaltropprocess_noise_dt)
- [`estimation_parameters:receivers:global:trop_grads:process_noise_dt:`](#estimation_parametersreceiversglobaltrop_gradsprocess_noise_dt)
- [`estimation_parameters:receivers:global:trop_maps:process_noise_dt:`](#estimation_parametersreceiversglobaltrop_mapsprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:orientation:process_noise_dt:`](#estimation_parametersreceiversglobalGPSorientationprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:gyro_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSgyro_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:accelerometer_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSaccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:gyro_scale:process_noise_dt:`](#estimation_parametersreceiversglobalGPSgyro_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:accelerometer_scale:process_noise_dt:`](#estimation_parametersreceiversglobalGPSaccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:imu_offset:process_noise_dt:`](#estimation_parametersreceiversglobalGPSimu_offsetprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:clock:process_noise_dt:`](#estimation_parametersreceiversglobalGPSclockprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:clock_rate:process_noise_dt:`](#estimation_parametersreceiversglobalGPSclock_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:pos:process_noise_dt:`](#estimation_parametersreceiversglobalGPSposprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:pos_rate:process_noise_dt:`](#estimation_parametersreceiversglobalGPSpos_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:orbit:process_noise_dt:`](#estimation_parametersreceiversglobalGPSorbitprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:pco:process_noise_dt:`](#estimation_parametersreceiversglobalGPSpcoprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:code_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPScode_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:phase_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSphase_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_d_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_d_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_d_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_d_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_d_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_d_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_d_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_d_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_d_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_d_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_y_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_y_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_y_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_y_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_y_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_y_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_y_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_y_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_y_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_y_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_b_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_b_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_b_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_b_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_b_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_b_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_b_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_b_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_b_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_b_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_r_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_r_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_r_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_r_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_r_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_r_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_r_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_r_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_r_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_r_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_t_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_t_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_t_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_t_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_t_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_t_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_t_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_t_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_t_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_t_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_n_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_n_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_n_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_n_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_n_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_n_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_n_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_n_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_n_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_n_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_p_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_p_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_p_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_p_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_p_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_p_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_p_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_p_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_p_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_p_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_q_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_q_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_q_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_q_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_q_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_q_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_q_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_q_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:emp_q_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:strain_rate:process_noise_dt:`](#estimation_parametersreceiversglobalGPSstrain_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:ambiguities:process_noise_dt:`](#estimation_parametersreceiversglobalGPSambiguitiesprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:pcv:process_noise_dt:`](#estimation_parametersreceiversglobalGPSpcvprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:ion_stec:process_noise_dt:`](#estimation_parametersreceiversglobalGPSion_stecprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:ion_model:process_noise_dt:`](#estimation_parametersreceiversglobalGPSion_modelprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:slr_range_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSslr_range_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:slr_time_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSslr_time_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:trop:process_noise_dt:`](#estimation_parametersreceiversglobalGPStropprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:trop_grads:process_noise_dt:`](#estimation_parametersreceiversglobalGPStrop_gradsprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:trop_maps:process_noise_dt:`](#estimation_parametersreceiversglobalGPStrop_mapsprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:orientation:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Worientationprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:gyro_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wgyro_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:accelerometer_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Waccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:gyro_scale:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wgyro_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:accelerometer_scale:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Waccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:imu_offset:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wimu_offsetprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:clock:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wclockprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:clock_rate:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wclock_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:pos:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wposprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:pos_rate:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wpos_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:orbit:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Worbitprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:pco:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wpcoprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:code_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wcode_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:phase_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wphase_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_d_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_d_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_d_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_d_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_d_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_d_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_d_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_d_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_d_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_d_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_y_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_y_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_y_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_y_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_y_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_y_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_y_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_y_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_y_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_y_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_b_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_b_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_b_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_b_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_b_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_b_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_b_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_b_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_b_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_b_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_r_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_r_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_r_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_r_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_r_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_r_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_r_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_r_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_r_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_r_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_t_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_t_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_t_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_t_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_t_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_t_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_t_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_t_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_t_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_t_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_n_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_n_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_n_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_n_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_n_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_n_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_n_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_n_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_n_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_n_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_p_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_p_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_p_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_p_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_p_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_p_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_p_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_p_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_p_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_p_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_q_0:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_q_0process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_q_1:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_q_1process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_q_2:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_q_2process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_q_3:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_q_3process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:emp_q_4:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:strain_rate:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wstrain_rateprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:ambiguities:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wambiguitiesprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:pcv:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wpcvprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:ion_stec:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wion_stecprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:ion_model:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wion_modelprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:slr_range_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wslr_range_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:slr_time_bias:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wslr_time_biasprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:trop:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wtropprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:trop_grads:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wtrop_gradsprocess_noise_dt)
- [`estimation_parameters:receivers:global:GPS:L1W:trop_maps:process_noise_dt:`](#estimation_parametersreceiversglobalGPSL1Wtrop_mapsprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:orientation:process_noise_dt:`](#estimation_parametersreceiversXMPLorientationprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:gyro_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLgyro_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:accelerometer_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLaccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:gyro_scale:process_noise_dt:`](#estimation_parametersreceiversXMPLgyro_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:accelerometer_scale:process_noise_dt:`](#estimation_parametersreceiversXMPLaccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:imu_offset:process_noise_dt:`](#estimation_parametersreceiversXMPLimu_offsetprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:clock:process_noise_dt:`](#estimation_parametersreceiversXMPLclockprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:clock_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLclock_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:pos:process_noise_dt:`](#estimation_parametersreceiversXMPLposprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:pos_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLpos_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:orbit:process_noise_dt:`](#estimation_parametersreceiversXMPLorbitprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:pco:process_noise_dt:`](#estimation_parametersreceiversXMPLpcoprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:code_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLcode_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:phase_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLphase_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_d_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_d_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_d_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_d_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_d_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_d_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_d_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_d_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_d_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_d_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_y_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_y_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_y_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_y_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_y_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_y_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_y_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_y_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_y_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_y_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_b_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_b_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_b_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_b_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_b_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_b_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_b_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_b_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_b_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_b_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_r_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_r_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_r_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_r_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_r_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_r_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_r_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_r_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_r_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_r_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_t_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_t_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_t_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_t_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_t_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_t_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_t_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_t_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_t_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_t_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_n_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_n_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_n_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_n_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_n_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_n_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_n_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_n_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_n_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_n_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_p_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_p_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_p_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_p_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_p_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_p_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_p_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_p_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_p_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_p_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_q_0:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_q_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_q_1:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_q_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_q_2:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_q_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_q_3:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_q_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:emp_q_4:process_noise_dt:`](#estimation_parametersreceiversXMPLemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:strain_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLstrain_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:ambiguities:process_noise_dt:`](#estimation_parametersreceiversXMPLambiguitiesprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:pcv:process_noise_dt:`](#estimation_parametersreceiversXMPLpcvprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:ion_stec:process_noise_dt:`](#estimation_parametersreceiversXMPLion_stecprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:ion_model:process_noise_dt:`](#estimation_parametersreceiversXMPLion_modelprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:slr_range_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLslr_range_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:slr_time_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLslr_time_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:trop:process_noise_dt:`](#estimation_parametersreceiversXMPLtropprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:trop_grads:process_noise_dt:`](#estimation_parametersreceiversXMPLtrop_gradsprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:trop_maps:process_noise_dt:`](#estimation_parametersreceiversXMPLtrop_mapsprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:orientation:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSorientationprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:gyro_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSgyro_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:accelerometer_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSaccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:gyro_scale:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSgyro_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:accelerometer_scale:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSaccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:imu_offset:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSimu_offsetprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:clock:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSclockprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:clock_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSclock_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:pos:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSposprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:pos_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSpos_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:orbit:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSorbitprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:pco:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSpcoprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:code_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPScode_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:phase_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSphase_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_d_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_d_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_d_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_d_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_d_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_d_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_d_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_d_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_d_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_d_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_y_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_y_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_y_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_y_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_y_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_y_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_y_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_y_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_y_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_y_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_b_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_b_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_b_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_b_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_b_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_b_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_b_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_b_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_b_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_b_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_r_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_r_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_r_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_r_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_r_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_r_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_r_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_r_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_r_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_r_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_t_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_t_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_t_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_t_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_t_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_t_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_t_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_t_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_t_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_t_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_n_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_n_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_n_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_n_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_n_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_n_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_n_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_n_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_n_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_n_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_p_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_p_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_p_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_p_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_p_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_p_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_p_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_p_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_p_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_p_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_q_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_q_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_q_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_q_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_q_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_q_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_q_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_q_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:emp_q_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:strain_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSstrain_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:ambiguities:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSambiguitiesprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:pcv:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSpcvprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:ion_stec:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSion_stecprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:ion_model:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSion_modelprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:slr_range_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSslr_range_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:slr_time_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSslr_time_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:trop:process_noise_dt:`](#estimation_parametersreceiversXMPLGPStropprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:trop_grads:process_noise_dt:`](#estimation_parametersreceiversXMPLGPStrop_gradsprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:trop_maps:process_noise_dt:`](#estimation_parametersreceiversXMPLGPStrop_mapsprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:orientation:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Worientationprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:gyro_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wgyro_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:accelerometer_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Waccelerometer_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:gyro_scale:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wgyro_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:accelerometer_scale:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Waccelerometer_scaleprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:imu_offset:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wimu_offsetprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:clock:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wclockprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:clock_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wclock_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:pos:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wposprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:pos_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wpos_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:orbit:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Worbitprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:pco:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wpcoprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:code_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wcode_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:phase_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wphase_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_d_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_d_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_d_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_d_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_d_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_d_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_d_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_d_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_d_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_d_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_y_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_y_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_y_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_y_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_y_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_y_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_y_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_y_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_y_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_y_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_b_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_b_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_b_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_b_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_b_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_b_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_b_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_b_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_b_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_b_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_r_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_r_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_r_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_r_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_r_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_r_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_r_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_r_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_r_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_r_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_t_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_t_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_t_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_t_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_t_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_t_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_t_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_t_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_t_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_t_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_n_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_n_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_n_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_n_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_n_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_n_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_n_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_n_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_n_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_n_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_p_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_p_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_p_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_p_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_p_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_p_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_p_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_p_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_p_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_p_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_q_0:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_q_0process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_q_1:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_q_1process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_q_2:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_q_2process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_q_3:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_q_3process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:emp_q_4:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wemp_q_4process_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:strain_rate:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wstrain_rateprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:ambiguities:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wambiguitiesprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:pcv:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wpcvprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:ion_stec:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wion_stecprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:ion_model:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wion_modelprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:slr_range_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wslr_range_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:slr_time_bias:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wslr_time_biasprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:trop:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wtropprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:trop_grads:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wtrop_gradsprocess_noise_dt)
- [`estimation_parameters:receivers:XMPL:GPS:L1W:trop_maps:process_noise_dt:`](#estimation_parametersreceiversXMPLGPSL1Wtrop_mapsprocess_noise_dt)
---

### E_SRPModel

Valid enum values are:
- `none`
- `cannonball`
- `boxwing`

For options:

- [`satellite_options:global:orbit_propagation:solar_radiation_pressure:`](#satellite_optionsglobalorbit_propagationsolar_radiation_pressure)
- [`satellite_options:global:orbit_propagation:albedo:`](#satellite_optionsglobalorbit_propagationalbedo)
- [`satellite_options:global:L1W:orbit_propagation:solar_radiation_pressure:`](#satellite_optionsglobalL1Worbit_propagationsolar_radiation_pressure)
- [`satellite_options:global:L1W:orbit_propagation:albedo:`](#satellite_optionsglobalL1Worbit_propagationalbedo)
- [`satellite_options:GPS:orbit_propagation:solar_radiation_pressure:`](#satellite_optionsGPSorbit_propagationsolar_radiation_pressure)
- [`satellite_options:GPS:orbit_propagation:albedo:`](#satellite_optionsGPSorbit_propagationalbedo)
- [`satellite_options:GPS:L1W:orbit_propagation:solar_radiation_pressure:`](#satellite_optionsGPSL1Worbit_propagationsolar_radiation_pressure)
- [`satellite_options:GPS:L1W:orbit_propagation:albedo:`](#satellite_optionsGPSL1Worbit_propagationalbedo)
- [`satellite_options:G--:orbit_propagation:solar_radiation_pressure:`](#satellite_optionsG--orbit_propagationsolar_radiation_pressure)
- [`satellite_options:G--:orbit_propagation:albedo:`](#satellite_optionsG--orbit_propagationalbedo)
- [`satellite_options:G--:L1W:orbit_propagation:solar_radiation_pressure:`](#satellite_optionsG--L1Worbit_propagationsolar_radiation_pressure)
- [`satellite_options:G--:L1W:orbit_propagation:albedo:`](#satellite_optionsG--L1Worbit_propagationalbedo)
---

### E_SSROutTiming

Valid enum values are:
- `gps_time`
- `latest_clock_estimate`

For options:

- [`outputs:ssr_outputs:output_timing:`](#outputsssr_outputsoutput_timing)
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
- [`outputs:ssr_outputs:ephemeris_sources:`](#outputsssr_outputsephemeris_sources)
- [`outputs:ssr_outputs:clock_sources:`](#outputsssr_outputsclock_sources)
- [`outputs:ssr_outputs:code_bias_sources:`](#outputsssr_outputscode_bias_sources)
- [`outputs:ssr_outputs:phase_bias_sources:`](#outputsssr_outputsphase_bias_sources)
- [`outputs:ssr_outputs:atmospheric:sources:`](#outputsssr_outputsatmosphericsources)
- [`satellite_options:global:models:pos:sources:`](#satellite_optionsglobalmodelspossources)
- [`satellite_options:global:models:clock:sources:`](#satellite_optionsglobalmodelsclocksources)
- [`satellite_options:global:models:attitude:sources:`](#satellite_optionsglobalmodelsattitudesources)
- [`satellite_options:global:L1W:models:pos:sources:`](#satellite_optionsglobalL1Wmodelspossources)
- [`satellite_options:global:L1W:models:clock:sources:`](#satellite_optionsglobalL1Wmodelsclocksources)
- [`satellite_options:global:L1W:models:attitude:sources:`](#satellite_optionsglobalL1Wmodelsattitudesources)
- [`satellite_options:GPS:models:pos:sources:`](#satellite_optionsGPSmodelspossources)
- [`satellite_options:GPS:models:clock:sources:`](#satellite_optionsGPSmodelsclocksources)
- [`satellite_options:GPS:models:attitude:sources:`](#satellite_optionsGPSmodelsattitudesources)
- [`satellite_options:GPS:L1W:models:pos:sources:`](#satellite_optionsGPSL1Wmodelspossources)
- [`satellite_options:GPS:L1W:models:clock:sources:`](#satellite_optionsGPSL1Wmodelsclocksources)
- [`satellite_options:GPS:L1W:models:attitude:sources:`](#satellite_optionsGPSL1Wmodelsattitudesources)
- [`satellite_options:G--:models:pos:sources:`](#satellite_optionsG--modelspossources)
- [`satellite_options:G--:models:clock:sources:`](#satellite_optionsG--modelsclocksources)
- [`satellite_options:G--:models:attitude:sources:`](#satellite_optionsG--modelsattitudesources)
- [`satellite_options:G--:L1W:models:pos:sources:`](#satellite_optionsG--L1Wmodelspossources)
- [`satellite_options:G--:L1W:models:clock:sources:`](#satellite_optionsG--L1Wmodelsclocksources)
- [`satellite_options:G--:L1W:models:attitude:sources:`](#satellite_optionsG--L1Wmodelsattitudesources)
- [`receiver_options:global:models:pos:sources:`](#receiver_optionsglobalmodelspossources)
- [`receiver_options:global:models:clock:sources:`](#receiver_optionsglobalmodelsclocksources)
- [`receiver_options:global:models:attitude:sources:`](#receiver_optionsglobalmodelsattitudesources)
- [`receiver_options:global:GPS:models:pos:sources:`](#receiver_optionsglobalGPSmodelspossources)
- [`receiver_options:global:GPS:models:clock:sources:`](#receiver_optionsglobalGPSmodelsclocksources)
- [`receiver_options:global:GPS:models:attitude:sources:`](#receiver_optionsglobalGPSmodelsattitudesources)
- [`receiver_options:global:GPS:L1W:models:pos:sources:`](#receiver_optionsglobalGPSL1Wmodelspossources)
- [`receiver_options:global:GPS:L1W:models:clock:sources:`](#receiver_optionsglobalGPSL1Wmodelsclocksources)
- [`receiver_options:global:GPS:L1W:models:attitude:sources:`](#receiver_optionsglobalGPSL1Wmodelsattitudesources)
- [`receiver_options:XMPL:models:pos:sources:`](#receiver_optionsXMPLmodelspossources)
- [`receiver_options:XMPL:models:clock:sources:`](#receiver_optionsXMPLmodelsclocksources)
- [`receiver_options:XMPL:models:attitude:sources:`](#receiver_optionsXMPLmodelsattitudesources)
- [`receiver_options:XMPL:GPS:models:pos:sources:`](#receiver_optionsXMPLGPSmodelspossources)
- [`receiver_options:XMPL:GPS:models:clock:sources:`](#receiver_optionsXMPLGPSmodelsclocksources)
- [`receiver_options:XMPL:GPS:models:attitude:sources:`](#receiver_optionsXMPLGPSmodelsattitudesources)
- [`receiver_options:XMPL:GPS:L1W:models:pos:sources:`](#receiver_optionsXMPLGPSL1Wmodelspossources)
- [`receiver_options:XMPL:GPS:L1W:models:clock:sources:`](#receiver_optionsXMPLGPSL1Wmodelsclocksources)
- [`receiver_options:XMPL:GPS:L1W:models:attitude:sources:`](#receiver_optionsXMPLGPSL1Wmodelsattitudesources)
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

### E_ThirdBody

Valid enum values are:
- `mercury`
- `venus`
- `earth`
- `mars`
- `jupiter`
- `saturn`
- `uranus`
- `neptune`
- `pluto`
- `moon`
- `sun`

For options:

- [`satellite_options:global:orbit_propagation:planetary_perturbations:`](#satellite_optionsglobalorbit_propagationplanetary_perturbations)
- [`satellite_options:global:L1W:orbit_propagation:planetary_perturbations:`](#satellite_optionsglobalL1Worbit_propagationplanetary_perturbations)
- [`satellite_options:GPS:orbit_propagation:planetary_perturbations:`](#satellite_optionsGPSorbit_propagationplanetary_perturbations)
- [`satellite_options:GPS:L1W:orbit_propagation:planetary_perturbations:`](#satellite_optionsGPSL1Worbit_propagationplanetary_perturbations)
- [`satellite_options:G--:orbit_propagation:planetary_perturbations:`](#satellite_optionsG--orbit_propagationplanetary_perturbations)
- [`satellite_options:G--:L1W:orbit_propagation:planetary_perturbations:`](#satellite_optionsG--L1Worbit_propagationplanetary_perturbations)
---

### E_TidalComponent

Valid enum values are:
- `east`
- `west`
- `north`
- `south`
- `up`
- `down`

For options:

- [`inputs:tides:atl_blq_row_order:`](#inputstidesatl_blq_row_order)
- [`inputs:tides:otl_blq_row_order:`](#inputstidesotl_blq_row_order)
---

### E_TidalConstituent

Valid enum values are:
- `m2`
- `s2`
- `n2`
- `k2`
- `s1`
- `k1`
- `o1`
- `p1`
- `q1`
- `mf`
- `mm`
- `ssa`

For options:

- [`inputs:tides:atl_blq_col_order:`](#inputstidesatl_blq_col_order)
- [`inputs:tides:otl_blq_col_order:`](#inputstidesotl_blq_col_order)
---

### E_TropModel

Valid enum values are:
- `standard`
- `sbas`
- `vmf3`
- `gpt2`
- `cssr`

For options:

- [`receiver_options:global:models:troposphere:models:`](#receiver_optionsglobalmodelstropospheremodels)
- [`receiver_options:global:GPS:models:troposphere:models:`](#receiver_optionsglobalGPSmodelstropospheremodels)
- [`receiver_options:global:GPS:L1W:models:troposphere:models:`](#receiver_optionsglobalGPSL1Wmodelstropospheremodels)
- [`receiver_options:XMPL:models:troposphere:models:`](#receiver_optionsXMPLmodelstropospheremodels)
- [`receiver_options:XMPL:GPS:models:troposphere:models:`](#receiver_optionsXMPLGPSmodelstropospheremodels)
- [`receiver_options:XMPL:GPS:L1W:models:troposphere:models:`](#receiver_optionsXMPLGPSL1Wmodelstropospheremodels)