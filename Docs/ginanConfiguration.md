
# PEA Configuration File - YAML
    
The PEA processing engine uses YAML files for configuration of all processing options. Details about the format and subset of the most important options available will be described here.

## Ginan Yaml Inspector

To aid in the creating and editing of configuration files, the `pea` is capable of generating a tool for inspecting and outputting yaml files.

Running `./pea -Y 4` will create the `GinanYamlInspector.html` file, which can be opened in a web browser. It will also output the default configuration values used in `pea` in a yaml-compatible format.
The numeric parameter can be changed to show more or less advanced configurations.

To edit a template configuration file, click the `choose file` button, and load an existing yaml file.

The inspector will expand the options to highlight which configurations were contained in the yaml file, and their new values. Other values that were not configured in the file will also be shown alongside their default values.

Hovering the mouse over an option will bring up a tooltip with a description of the parameter, 

To edit the file, check the box for the option to enable it, and set the value accordingly. To return a parameter to its default value, just unselect the checkbox.

Once all edits have been made, clicking the `Generate yaml` button will produce the output for the selected parameters, and the `Save file` button may be used to save it to disc, for use in the `pea`.


## YAML Syntax

The YAML format allows for heirarchical, self descriptive configurations of parameters, and has a straightforward syntax.

White-space (indentation) is used to specify heirarchies, with each level typically indented with 4 space characters.

Square brackets `[ ]` are used for parameters that accept a list of options. In cases where the `pea` expects multiple list values, it will typically use the last list value for all remaining components.

Colons (`:`) are used to separate configuration keys from their values.

Lists may be created by either appending multiple values on a single line, wrapped in square brackets and separated by commas, or, by adding each value on a separate indented line with a dash before the value.

Adding a hash symbol (`#`) to a line will render the remainder of the line as a comment to be ignored by the parser.

Strings with special characters or spaces should be wrapped in quotation marks.

You will see all of these used in the example configuration files, but the files may be re-ordered, or re-formatted to suit your application.

## File lists and Globbing
Files may be specified individually, as lists, or by searching available files using a globbed filename using the star character (`*`)

For options that include a list of files, e.g. `snx_files`, entries from different yaml files are concatenated.

In the case of example `ex204.yaml`, Ginan will read both `tables/igs_satellite_metadata.snx` from `input_metadata.yaml` and `IGS1R03SNX_20191950000_07D_07D_CRD.SNX` from `input_files_network.yaml`.

## Wildcard Tags
Output filenames can include wildcards wrapped in `< >` brackets to allow more generic names to be used. While processing, these tags are replaced with details gathered from processing, and allows for automatic generation of, for example, hourly output files.

#### `<CONFIG>`
This is replaced with the `config_description` value entered in the yaml file or at the command line
#### `<STATION>`
This is replaced with the 4 character station id of a given station.
#### `<STREAM>`
This is replaced with the id of the a given stream where appropriate.
#### `<LOGTIME>`, `<DDD>`,`<D>`, `<WWWW>`, `<YYYY>`, `<YY>`, `<MM>`, `<DD>`, `<HH>`
These are replaced with the (rounded) time of the epochs within the trace file.

If trace file rotation is configured for 1 hour, the `<LOGTIME>` wildcard will be rounded down to the closest hour, and subsequently change value once per hour and generate a separate output file for each hour of processing.

# Helper Configurations

As of June 2023, the number of possible options available to ginan surpasses 3000.
This documents seeks to illustrate the use some of these options through the use of specific examples.
These configurations provide starting points for configuring desired processing modes.

These examples are available on the exampleConfig folder `ginan/exampleConfig/`

The first thing to note on the examples is that yaml files can also serve as inputs to another yaml.
Several of these configurations may be assembled together using an `include_yamls:` option in a master yaml file, along with any overriding configurations specific for the processing run.
This allows the separation of configuration into functional modules.
The examples considered here have common modules e.g. `input_metadata.yaml`, `mongo_outputs.yaml` and `debug_trace.yaml`, and additional modules according to the specifics of the processing run.

Each of the modules contain detailed information of one aspect of Ginan processing as explained below.

## Debug Trace and Outputs

For the examples in this document, the scenario specific outputs have been included in files describing the processing options.
Sensible outputs are closely linked to processing options and thus should be revisited after modifiying the processing configuration.

The file path passed to `outputs:root_directory` will be added as a prefix to all output files, including those on other yaml files. 






### Trace files
The examples described in this document have file-based trace files configured in `debug_trace.yaml`, which defines the debug trace outputs. 
\include "exampleConfigs/debug_trace.yaml"

Debug traces are text files which output information about the internal workings of the Ginan processing.
Its intended use is to help detect and correct potential errors in the input data or processing methods.

The level of information in a trace file can be controled using the `outputs:trace:level` parameter. The higher the number (up to 6) the more verbose the information written to the trace files.
There are two types of trace file, station trace files which can be enable using the `outputs:trace:output_stations` option, and outputs debugging information on a file defined by the `outputs:trace:station_filename` parameter, and the network trace file enabled using the `outputs:trace:output_network` option with `outputs:trace:network_filename` as outputs.

The user can enable certain types of data included to be included in trace files. 
The `output_residuals` option will enable the output of pre-fit and post-fit residual into either the station of network trace file.
Enabling the `output_residual_chain` option will output the observed-minus-computed decomposition described above to the station trace file. 
Setting `output_config` to true will cause Ginan to write the contents of input yaml configuration setting file/s to the top of output trace files.

More discussion of trace files is available [here](page.html?c=on&p=ginanUsage.index#logging-and-outputs)

### Mongo Outputs

The `mongo_outputs.yaml` file configures outpus to the mongo database.

\include "exampleConfigs/mongo_outputs.yaml"

Ginan makes use of Mongo databases as part of its operations. The main uses of Mongo databases in Ginan are as a data sharing mechanism for real-time network solutions and, as is the case of the examples in this text, as a way of monitoring the inputs and processing of Ginan.

Use of mongo databases are enable by using the `mongo:enable` option, the database used is defined by `mongo:database` and `mongo:suffix`. The database location is using the `mongo:uri` option. 

When enabled, mongo will write a varaety of types of data into the specified database.
For monitoring purposes the following data can be written to Mongo databases:
* By setting `output_rtcm_messages` to true, RTCM messages (both received and transmitted) are archived in the database
* By setting `output_states` to true, the state of estimated parameters are archived in the mongo database
* By setting `output_measurements` to true, pre-fit and post-fit residuals are archived in the mongo database
* By setting `output_test_stats` to true, statistics (like average and RMS) of residuals are estimated and included in the database

Data archived into the database can be visualized using the EDA API inluded with Ginan. A detailed description on how to visualize can be found on the [GinanEDA](page.html?c=on&p=scripts.index#ginaneda) section of this manual.

Although not included in the `mongo_outputs.yaml` file, if `mongo:output_ssr_precursors` is enabled parameters needed for the calculation of SSR messages will be loaded into the designated mongo database. 
From here a Ginan instance (the one writing to the file or another that uses the results from the first one) can read the solutions, and assemble SSR messages.

### Example specific outputs

The `ex202.yaml` example uses `process_rover.yaml` to set its outputs and the majority of its processing options.
The output section on `process_rover.yaml` include settings for GPX outputs as well as Ginan's proprietary output.
```
...
outputs:
    root_directory:                 outputs/<CONFIG>/
    ppp_sol:                              
        output:                     true         
        filename:                   <STATION><YYYY><DDD><HH>_<CONFIG>.POS
    gpx:
        output:                     true
        filename:                   <STATION><YYYY><DDD><HH>_<CONFIG>.gpx

...
```

As with other output files, `gpx` and `ppp_sol` outputs are enabled by setting `outputs:ppp_sol:output` or `outputs:gpx:output` to true.
The file name for the outputs can be specified using the  `outputs:ppp_sol:filename` and `outputs:gpx:filename` options.

The `process_network_bias.yaml` file complements that to add additional phase bias output files, while preserving other outputs.
The `output` section of this will only enable and configure phase bias outputs.
```
...
outputs:
    bias_sinex:
        phase_output_interval:       900
...
```
in this case, changing `phase_output_interval` from its default of 0 to 900 seconds, it will instruct Ginan to output phase bias estimates every 15 minutes. 


## Metadata input

### Metadata input files

The `input_metadata.yaml` file describes all of the auxillary data and metadata inputs for a run of Ginan. 
Metadata is data that are expected to remain constant/valid over a long period of time. 

\include "exampleConfigs/input_metadata.yaml"

Included here are:

* **Satellite metadata :** the `igs_satellite_metadata.snx` contains metadata associated with the satellite body (e.g. satellite block type, SVN/PRN, satellite mass) used for attitude models and orbit estimation. The ANTEX file `M20.ATX` contains metadata for satellite and receiver antennas, mainly the PCO and PCV. 

* **Earth tide description :** `fes2004_Cnm_Snm.dat` descrives global ocean tide loading (spherical harmonics up to order-degree 50) models, while `OLOAD_GO.BLQ` describes precise ocean tide loading parameters for specific locations (indexed by CORS station names). Earth tide models are used to adjust station-rover positions.

* **Gravity effects :** `EGM2008.gfc` describes the earth gravity field (spherical harmonics up to order-degree 15) and `DE436.1950.2050` describes the position of celestial bodies. Both files are used to estimate the combined gravity field actingo on a satellite. Gravity field estimate is mainly used in satellite position estimation.

* **Geomagnetic fields :** `igrf13coeffs.txt` describes earth's geomagnetic field near earth (spherical harmonics up to order-degree 13). State of the geomagnetic field is used on the estimation of higher order components of Ionosphereic delay on GNSS signals.

* **Tropospheric model :** `gpt_25.grd` contains global atmospheric pressure and temperature values ($5^o \times 5^o$ gridmaps) as well as hydrostatic and wet mapping coefficients for trpospheric delay modelling. Two tropospheric delay models are supported by Ginan: The GPT2 model, supported by this file, and the VMF3 model, which require it own set of files.  

The file path defined in `inputs:root_directory` will be added as a prefix to ALL files (including those defined in other yaml files). 
Some of the files described here are used for orbit determination and thus not necessary for the presented examples, however including then will not affect Ginan processing. 

### Metadata for Real-time rovers

While metadata inputs contain slowly changing parameters that are expected requirements for a large variety of scenarios others are more specific.

The `input_streams_rover.yaml` file demonstrates a configuration specific to rover with realtime inputs.

\include "exampleConfigs/input_streams_rover.yaml"
 
This configuration file is roughly divided into 3 sections, the `station_options` section, the `inputs` section and the `processing_options` section.

The `station_options` section contains metadata for each GNSS receiver used in this test.
Four CORS are used as receivers for the purposes of this example.
The metadata included is self explanatory.
Only `antenna_type`, which is used to select PCO and PCV values from the ANTEX file, is strictly necessary.
Here, `apriori_position` is included in the example for demonstration purposes, but is not expected in the case of moving receivers. 

The `inputs` section contain the origin of receiver and satellite data to be processed in this example.

The example illustrates how to configure the real-time position estimation of a GNSS receiver.
As such, it uses data streams in place of files to obtain both `gnss_observations` and `satellite_data` using RTCM formatted streams broadcast by Geoscience Australia's NTRIP caster.

The contents of the `inputs_root` (containing username, password and hostname for the NTRIP caster in this example) are applied to streams in the `gnss_observations` and `satallite_data` sections.
For this example the mountpoints corresponding to individual streams are included under `rtcm_inputs`. 
It is possible to include the prefix in these fields, thus setting different host, username and password for each stream. 

Ginan also supports the streaming of data from a local serial port, which would be set as below: 
```inputs:
    gnss_observations:
        rtcm_inputs:
            - serial:///dev/ttyAMA0
```

Ginan supports receiving RTCM messages for broadcast ephemeris and SSR (including IGS-SSR) corrections, and for GNSS observations Ginan supports RTCM MSM messages (not legacy messages like 1004 and 1012) and some UBLOX receiver messages (by using `ubx_inputs`).

The `processing_options` section controls how Ginan interprets and processes the data obtained from input sources.
While the majority of processing options are set on another yaml file, options that help describe the contents of the input streams are included here.

The data for some of Ginan outputs can be taken from different sources. 
One such example are `clock` inputs, sources for satellite and receiver clock estimate can be set by using the `satellite_sources` and `receiver_sources` options. 
Supported options include:
* NONE: disable this output
* KALMAN: output internal estimates from Kalman filter
* PRECISE: output data from input files such as CLK/SP3/OBS.
* BROADCAST output data from broadcast ephemeris files or streams
* SSR: calculate values by adding SSR corrections (from streams or recorded files) to broadcast ephemeris

Ginan can combine outputs from multiple sources. For example, the default configuration of `output:clocks:satelite_sources` is `[KALMAN,PRECISE,BROADCAST]`, in this case Ginan will output estimates from kalman filter where it can, inputs from precise ephemeris files for satellites it cant find estimates for and finally from broadcast ephemeris as last resort.


The `ssr_antenna_offset` option clarifies the reference point the SSR satellite position corresponds to. 
Possible values supported for this option are Centre of Mass (`COM`), or Antenna Phase Centre (`APC`).

The `epoch_interval` option defines the rate at which the input data will be processed by Ginan. It is set as 1 second/epoch to match the data rate of GNSS observation streams.
This option can be overwritten by later yaml files, if set higher than the input data interval Ginan will skip epochs to match the desired data rate.

The `wait_all_stations` option instructs Ginan to wait for the set amount of seconds to data from all receivers to arrive.
This parameters needs to be set to the maximum difference in latency between data streams.
If GNSS observation from a stream fails to arrive within the specified time the receiver will be excluded from processing.

The `error_model` option describes the behaviour of unmodelled errors expected in the GNSS observations. 
The possible values for this option are `constant` and `elevation_dependent`.

The `code_measurements:sigma` config sets the noise level (at the zenith) for pseudorange measurements and `phase_measurements:sigma` sets the noise level (at the zenith) for carrier phase measurements.

#### Metadata for a post-processed network

The `process_network.yaml` sets a basic network solution which estimates satellite clock offsets for GPS and Galileo, and code biases for a minimum number of signals.

An example configuration file for a post-processed network is available in the `input_files_network.yaml` file. Due to its size only a section of this file is included below: 
```inputs:
    snx_files:  [ IGS1R03SNX_20191950000_07D_07D_CRD.SNX    ]
    gnss_observations:
        inputs_root: data/
        rnx_inputs:
            - "USN7*.rnx"
            - "NNOR*.rnx"
            - "RGDG*.rnx"
            - "MBAR*.rnx"
...

processing_options:
    epoch_control:
        epoch_interval:             30
    gnss_general:   
        error_model:        elevation_dependent
        code_measurements:
            sigmas:         [0.3]
        phase_measurements:
            sigmas:         [0.003]
```
The example decribes a CORS network from which the network solutions will be estimated.

The CORS network contains more than 50 stations, thus setting the receiver metadata through a `station_options` section is not practical; Instead a SINEX file is used to input the station metadata. 
The file pointed at by `snx_files` option is expected to contain the station location, receiver, and antenna type for each station in the network.

The `inputs_root` option substitutes the `root_directory` option contained in `input_metadata.yaml` for the files included in the `gnss_observation` section.
The `rnx_inputs` option takes a list of RINEX files (RINEX 2,3 and 4 are supported) as inputs to Ginan processing.

Parameters in the `processing_options` section option serves the same purpose as in the case of `ex202.yaml`.  `epoch_interval` is set to 30 seconds here to reflect the data rate of RINEX files.

Other files used by Ginan for satellite data inputs are the `clk_files` which contain satellite and receiver clock offsets in RINEX CLK format, `bsx_files`, and `dcb_files`, which contain satellite and receiver hardware biases in bias SINEX and RINEX DCB formats respectively.


The outputs section of the `process_network.yaml` file reflects the estimated parameters, and creates file-based outputs accordingly.
```
...
outputs:
    root_directory:         outputs/<CONFIG>/
    sinex:
        output:                     true
        filename:                   GAA0GINRAP_<YYYY><DDD>0000_01D_01D_CRD.SNX
    erp:
        output:                     true
        filename:                   GAA0GINRAP_<YYYY><DDD>0000_01D_01D_ERP.ERP
    trop_sinex:
        output:                     true
    clocks:
        output:                     true
        filename:                   GAA0GINRAP_<YYYY><DDD>0000_01D_30S_CLK.CLK
        satellite_sources:          [KALMAN]
        receiver_sources:           [NONE]
    bias_sinex:
        output:                     true        
        filename:                   GAA0GINRAP_<YYYY><DDD>0000_01D_15M_OSB.BIA
        code_output_interval:       86400.0
        output_rec_bias:            false
...
```
Individual output types can be enabled by setting the corresponding `output` option to true, the output file name can be set using the corresponding `filename` option.
The basic outputs from a network solutions are as follows:
* Enabling `sinex` will output files with estimated station positions following receiver SINEX format.
* Enabling `erp` will output files with earth orientation parameters following IGS ERP format.
* Enabling `trop_sinex` will output files with zenith tropospheric delay following the troposphere SINEX format.
* Enabling `clock` will output files with satellite and receiver clock offset estimates in RINEX CLK format. 
* Enabling `bias_sinex` will output files with satellite and receiver hardware bias using the bias SINEX format. Unlike clock outputs, which will be calculated every processing epoch, the update rate of hardware biases can be set to be longer than the epoch interval. The `code_output_interval` options set the output interval for code biases, setting this to 0 wil disable code bias output. Receiver and satellite bias can be enabled or disabled by using the `output_rec_bias` and `output_sat_bias` options. 


## Processing and Estimation Configuration

### Real-time rover

The `process_rover.yaml` file contains the basic configurations required to estimate rover positions from the input data.
Aside from the `outputs` setion described above, configuration parameters can be divided into two sections. 
The `estimation_parameters` section which describe WHAT parameters are estimated. The `processing_options` describe details on HOW the parameters are processed.

The `estimation_parameters` section of `process_rover.yaml` will contain only receiver or station side parameters:
```
...
estimation_parameters:
    stations:
        global:
            pos:
                estimated:          [true]
                sigma:              [100]
                proc_noise:         [100] 
            clk:
                estimated:          [true]
                sigma:              [1000]
                proc_noise:         [100]
            amb:
                estimated:          [true]
                sigma:              [1000]
                proc_noise:         [0]
            trop:
                estimated:          [true]
                sigma:              [0.3]
                proc_noise:         [0.0001]
            trop_grads:
                estimated:          [true]
                sigma:              [0.03]
                proc_noise:         [1.0E-6]
            ion_stec:
                estimated:          [true]
                sigma:              [200]
                proc_noise:         [10]
            code_bias:
                estimated:          [true]
                sigma:              [20]
                proc_noise:         [0]
            phase_bias:
                estimated:          [true]
                sigma:              [10]
                proc_noise:         [0]
...
```
The parameters being estimated are 
* Receiver position (`station:global:pos`)
* Receiver clock offsets (`station:global:clk`)
* Carrier phase ambiguities (`station:global:amb`)
* Tropospheric zenith delay (`station:global:trop`)
* North-South and East-West gradient on tropospheric zenith delays (`station:global:trop_grad`)
* Ionosphere Slant Total Electron Content along the satellite-receiver path (`station:global:ion_stec`)
* Receiver hardware biases on pseudorange measurements (`station:global:code_bias`)
* Receiver hardware biases on carrier phase measurements (`station:global:phase_bias`)

Every parameter estimated by the Kalman filter is treated as a stochastic process following an Auto-Regressive model.
\begin{equation}
    x(t + t_0) = exp(-\frac{t0}{\tau})(x(t) -\mu) + \frac{w(t)}{\sqrt{t0}} 
\end{equation}
The configuration options in this section help set aspects of this model. 
`apriori_val` (default: 0) and `sigma` (default: Inf) sets the initial conditions.
`tau` (default: Inf) and `mu` (default: 0) sets $\tau$ and $\mu$ respectively.
`proc_noise` (default: 0) set the standard deviation for w(t) (considered a zero mean gausian noise process).
As they are set in the `process_rover.yaml` file (`tau` and `mu` at default values) the parameters are defined as a random walk process.

The `processing_options` is in itself divided into a variety of subsections.
```
...
processing_options:
    process_modes:
        ppp:                            true
    gnss_general:
        rec_reference_system:           gps
        sys_options:
            gps:
                process:                true
                reject_eclipse:         false
                zero_receiver_dcb:      true
                code_priorities:        [ L1C, L2W ]
            gal:
                process:                true
                reject_eclipse:         false
                zero_receiver_dcb:      true
                code_priorities:        [ L1C, L5Q ]
    gnss_models:
        troposphere:
            model:              gpt2
        ionospheric_component2:                              
            enable:             true
        ionospheric_component3:                              
            enable:             true
    filter_options:
        outlier_screening:
            max_filter_iterations:      5 
            max_prefit_removals:        3 
        station_chunking:
            enable:                     true
    model_error_checking:
        deweighting:
            deweight_factor:            1000
        ambiguities:
            outage_reset_limit:         5
            phase_reject_limit:         2
            reinit_on_all_slips:        true    
...
```

`process_modes` is a fundamental setting that control over all processing settings, `ppp` needs to be set to `true` in order to enable Ginan 2 processing

The `gnss_general` subsection controls general aspects of gnss processsing.
On the input files descrived above, it set the expected noise levels of GNSS measurements
In this file it controls which GNSS measurements to use.
 
How each GNSS constellation is handled is descrived in `sys_options`. 
The `process` option enables/disables the use of each constellation. Supported constellations are GPS, GLONASS, Galileo, Beidou and QZSS.

The signal list in `code_priorities` will govern if and how the GNSS signals are used, with signals not on the list not being used. 
By setting `reject_eclipse` to `true` eclipsing satellites can be excluded from processing.

In addition to GNSS signals, parameters constraints or pseudo-observations can be added to the process. 

The receiver clock will be considered to correspond the constellation specified by `rec_reference_system`.
This is acieved by selecting two pseudoranges in the reference constellation and setting the ionosphere-free combination of receiver code bias to zero.

Another pseudo-observation `zero_receiver_dcb` sets the difference betwee two receiver code biases for the specified constellation to zero.
The two codes to be constrained are selected from tracked signals, according to the `code_priorities` list.

The `gnss_models` controls various physical models applied to Ginan processing. The models include Earth tides, relativistic effects, ionospheric and tropospheric delays and phase windup.
Orbit related models, including satellite attitude are in other sections.
In the `process_rover.yaml` file, Ginan is set to use GPT2 as the `troposphere` model and to include second order (`ionospheric_component2`) and third order (ionospheric_component3) ionospheric delay effects.

The `filter_options` subsection configures the Kalman Filter operation.
Possible settings includes the type of numerical inverter, the outlier detection, and Rauch-Tung-Striebel (RTS) smoothing configuration.

Also included in the `process_rover.yaml` file is the `station_chunking` option. 
This option, to be used when processing multiple rovers, separates the filtering process by station thus reducing computational complexity of the solution.

The `model_error_checking` configures the hanlding of outliers detected by the Kalman filter. 

The `epoch_control` subsection, used in input yaml files, controls which epochs to process and which to skip. 


### Post-processed network

The `process_network.yaml` file contains the basic configurations required to estimate network solutions for GPS and Galileo satellites.
The `process_network_biases.yaml` expands the network solution to include code biases for extra signals as well as phase bias estimation.

As with rover processing, the network processing files have the `estimation_parameters` and `processing_options` sections in addition to the `outputs` section.

The `stations` part of `estimation_parameters` is similar to the rover case (with phase_bias in a separate file).
However it does contain additional constraints to eliminate rank deficiencies in the GNSS observation model.

The examples in `process_network.yaml` illustrate how the estimation parameters can be set for individual stations (as is the case for `USN7`) or even signals whitin the station (as with CUSV).

```
...

estimation_parameters:
    stations:
...
        USN7:                                       # Signals on USN7: GPS L1W, L1C, L2W, L2L, L5Q; GAL L1C, L5Q, L7Q; GLO L1C, L1P, L2C, L2P; BDS L2I, L7I, L6I
            clk:
                estimated:          [false]
            code_bias:
                estimated:          [false]
        CUSV:                                       # Signals on CUSV: GPS L1W, L1C, L2W, L2X, L5X; GAL L1X, L5X, L7X; GLO L1C, L1P, L2C, L2P; BDS L2I, L7I, L6I; QZS L1C, L2X, L5X
            GAL:
                L1X:
                    code_bias:
                        estimated:  [false]
                L5X:
                    code_bias:
                        estimated:  [false]
    satellites:
        global:
            clk:
                estimated:          [true]
                sigma:              [1000]
                proc_noise:         [1]

            clk_rate:
                estimated:          [true]
                sigma:              [10]
                proc_noise:         [1e-9]

            code_bias:
                estimated:          [true]
                sigma:              [10]
                proc_noise:         [0]
        GPS:
            L1W:
                code_bias:
                    sigma:              [1e-8]      # this implements B(s,GPS-L1W)=0 
                    process_noise:      [0]
                    apriori_value:      [0]
            L2W:
                code_bias:
                    sigma:              [1e-8]      # this implements B(s,GPS-L2W)=0 
                    process_noise:      [0]
                    apriori_value:      [0]            
        GAL:
            L1C:
                code_bias:
                    sigma:              [1e-8]      # this implements B(s,GPS-L1W)=0 
                    process_noise:      [0]
                    apriori_value:      [0]
            L5Q:
                code_bias:
                    sigma:              [1e-8]      # this implements B(s,GPS-L2W)=0 
                    process_noise:      [0]
                    apriori_value:      [0] 
    eop:
        estimated:  [true]
        sigma:      [10]

    eop_rates:
        estimated:  [true]
        sigma:      [10]
...
```
The main difference between a network solution and a rover solution is that satellite parameters (orbits, clocks, and biases) are estimated.
Thus included in the `satellites` part of `estimation_parameters`, are satellite clock offset (`clk`), satellite clock drift (`clk_rate`), code bias (`code_bias`) for `process_network.yaml` and phase biases (`phase_bias`) in `process_network_bias.yaml`.

In these examples, satellite code biases for specific signals are set to 0, in order to define the satellite clock as referring to two specific signals ([L1W,L2W] for GPS and [L1C,L5Q] for Galileo to match IGS standards).
In addition, the network solution estimates the Earth Orientation Parameters (`eop` and `eop_rate`)

There are few differences in the `process_options` section between `process_network.yaml` and the rover case. 

The number of signals in the `code_priorities` list is different to reflect the fact that different stations/receivers track a different set of codes. Some station track Galileo L1C and L5Q while others track the L1X-L5X pair, so both pairs should be tracked.
Some receivers do not track L1W thus the L1C needs to be added to use thee receivers in network processing.

The `zero_receiver_dcb` sets the differential satellite code biase of the L1C-L5Q and L1X-L5X pairs to zero.
Finally the minimum constraints aligns the reference frame of the Ginan solution with an existing one (determined by the station position on the SINEX file). 
Of particular note here is that unless the `once_per_epoch` option is set to `true`, aligment of reference frames happens after the last epoch of processing.
This means that results from other epochs are in an unconstrained reference frame. If alignment with a standard refernce is required `once_per_epoch` needs to be set to true.

`process_network_bias.yaml` expands the network solutions to include more signals and to estimate phase biases.
```
...
processing_options:
    gnss_general:
        sys_options:
            gps:
                network_amb_pivot:      true
                # code_priorities:        [ L1W, L1C, L2W, L5Q ]
                code_priorities:        [ L1W, L1C, L2W, L2L, L2X, L5Q, L5X ]
            gal:
                network_amb_pivot:      true
                code_priorities:        [ L1C, L5Q, L7Q, L1X, L5X, L7X ]
...
```

The former can be activated by expanding the `code_priorities` list.
The latter is enabled in the `outputs` and `estimation_parameters` section. 

Including phase bias estimation leads to additional rank deficiencies.
The `receiver_amb_pivot` enables a series of contraints (fixing a minimum numbr of ambiguities to integer values) to eliminate such deficiencies.

The `minimum_constraints` subsection configures the alignment of estimated solutions with a standard reference frame.

### Ambiguity Resolution

\include "exampleConfigs/ambiguity_resolution.yaml"

# Complete Ginan Examples

These examples demonstrate basic use cases, combining several smaller configurations to achieve the desired processing modes.


## Ex201 - Basic post-processed Rover Position

\include "exampleConfigs/ex201.yaml"


## Ex202 - Basic realtime Rover Position

The `ex202.yaml` example illustrates the real time estimation of rover positions. 

It gets data from NTRIP streams (as defined by `input_streams_rover.yaml`), and processes it to obtain rover positions (as defined by `process_rover.yaml`).

\include "exampleConfigs/ex202.yaml"


## Ex204 - Network Solution: Clock and Biases

The `ex204.yaml` on the other hand illustrates a post-process network solution, where the estimation of satellite clock offsets and hardware biases is the main objective.

It gets data from RINEX and SP3 files (as defined in `input_files_network.yaml` and `ex204.yaml`), and processes it to obtain satellite clock offsets (as defined by `process_netowrk.yaml`) and satellite hardware biases (as defined by `process_network_bias.yaml`).

\include "exampleConfigs/ex204.yaml"

Unlike in the case of `ex202.yaml` the additional satellite data is included on the `ex204.yaml` file.

This allows switching from a network solution that require precise orbits as inputs, to a solution that estimates precise orbits (see `ex205.yaml`).

The `erp_files` contains Earth Rotation/orientation Parameters which often times accompanies the precise orbits contained in `sp3_files`.

## Ex205 - Network Solution: Orbits and Clocks

\include "exampleConfigs/ex205.yaml"









