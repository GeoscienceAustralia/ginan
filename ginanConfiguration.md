
# PEA Configuration File - YAML

The PEA processing engine uses YAML files for configuration of all processing options. Details about the format and subset of the most important options available will be described here.

## Ginan Yaml Inspector

To aid in the creating and editing of configuration files, the `pea` is capable of generating a tool for inspecting and outputting yaml files.

Running `./pea -Y 4` will create the [GinanYamlInspector.html](GinanYamlInspector.html) file in the local directory, which can be opened in a web browser. It will also output the default configuration values used in `pea` in a yaml-compatible format.
The numeric parameter can be changed to show more or less advanced configurations.

To edit a template configuration file, click the `choose file` button, and load an existing yaml file.

The inspector will expand the options to highlight which configurations were contained in the yaml file, and their new values. Other values that were not configured in the file will also be shown alongside their default values.

Hovering the mouse over an option will bring up a tooltip with a description of the parameter.

To edit the file, check the box for the option to enable it, and set the value accordingly. To return a parameter to its default value, just unselect the checkbox.

Once all edits have been made, clicking the `Generate yaml` button will produce the output for the selected parameters, and the `Save file` button may be used to save it to disc, for use in the `pea`.


## YAML Syntax

The YAML format allows for hierarchical, self descriptive configurations of parameters, and has a straightforward syntax.

White-space (indentation) is used to specify hierarchies, with each level typically indented with 4 space characters.

Square brackets `[ ]` are used for parameters that accept a list of options. In cases where the `pea` expects multiple list values, it will typically use the last list value for all remaining components if for example only one value is passed for a configuration that takes x,y,z components.

Colons (`:`) are used to separate configuration keys from their values.

Lists may be created by either appending multiple values on a single line, wrapped in square brackets and separated by commas, or, by adding each value on a separate indented line with a dash before the value.

Adding a hash symbol (`#`) to a line will render the remainder of the line as a comment to be ignored by the parser.

Strings with special characters or spaces should be wrapped in quotation marks.

You will see all of these used in the example configuration files, but the files may be re-ordered, or re-formatted to suit your application.

## File lists and Globbing
Files may be specified individually, as lists, or by searching available files using a globbed filename using the star character (`*`)

For options that include a list of files, e.g. `snx_files`, entries from different yaml files are concatenated.

## Wildcard Tags
Output filenames can include wildcards wrapped in `< >` brackets to allow more generic names to be used. While processing, these tags are replaced with details gathered from processing, and allows for automatic generation of, for example, hourly output files.

#### `<CONFIG>`, `<AGENCY>`, `<SOFTWARE>`
These are replaced with the `config_description`, `analysis_agency` and `analysis_software` values entered in the yaml file or at the command line
#### `<USER>`, `<PASS>`
These are replaced with the username and password passed at the command line

#### `<RECEIVER>`
This is replaced with the 4 character receiver id of a given receiver.
#### `<STREAM>`
This is replaced with the id of the a given stream where appropriate.

#### `<LOGTIME>`, `<DDD>`, `<D>`, `<WWWW>`, `<YYYY>`, `<YY>`, `<MM>`, `<DD>`, `<HH>`
These are replaced with the (rounded) time of the epochs within the trace file.

If trace file rotation is configured for 1 hour, the `<LOGTIME>` wildcard will be rounded down to the closest hour, and subsequently change value once per hour and generate a separate output file for each hour of processing.

#### `<CWD>`
This tag is replaced with the current working directory.

#### `<XXX_DIRECTORY>`, `<XXX_ROOT>`
These tags appear in default file values and are replaced with the configured values, where XXX is the name of the type of files or root, for example `TRACE_DIRECTORY` and `GNSS_OBS_ROOT` are set by `trace: directory` and `gnss_observations: gnss_obs_root`

#### `<HASH>`, `<BRANCH>`
The git commit hash and branch id of the compiled software








# Major Configurations

As of June 2023, the number of possible options available to Ginan surpasses 3000.
This documents seeks to illustrate the use some of these options through the use of specific examples.
These configurations provide starting points for configuring desired processing modes.

These examples are available on the exampleConfig folder `ginan/exampleConfigs/`

The first thing to note on the examples is that yaml files can also serve as inputs to another yaml.
Several of these configurations may be assembled together using an `include_yamls:` option in a master yaml file, along with any overriding configurations specific for the processing run.

This allows the separation of configuration into functional modules if so desired; a common boxwing model may be saved in `boxwing.yaml`, and different instantiations of the pea might be called such as `./pea -y boxwing.yaml -y ppp_static.yaml` or `./pea -y boxwing.yaml -y ppp_kinematic.yaml`, to avoid repetition of configurations across many files.

## Debug Trace and Outputs

For the examples in this document, the scenario specific outputs have been included in files describing the processing options.
Sensible outputs are closely linked to processing options and thus should be revisited after modifying the processing configuration.


### Trace files
The examples described in this document have file-based trace files configured in `outputs:trace`, which defines the debug trace outputs.

Debug traces are text files which output information about the internal workings of the Ginan processing and are considered the best sources of information about the processed data.

The level of information in a trace file can be controlled using the `outputs:trace:level` parameter. The higher the number (up to 6) the more verbose the information written to the trace files.

There are two types of trace file, receiver trace files which can be enable using the `outputs:trace:output_receivers` option, and outputs debugging information on a file defined by the `outputs:trace:receiver_filename` parameter, and the network trace file enabled using the `outputs:trace:output_network` option with `outputs:trace:network_filename` as outputs.

Details about the computation of estimated ranges and other preprocessing are listed in the receiver trace files, while the details of the combination of those parameters in the kalman filter are recorded in the network trace file.

The primary filtered output of the pea can be seen in detail in the network trace's `STATES` blocks, with the details of filter input data and residuals in the `RESIDUALS` blocks.

The user can enable certain types of data included to be included in trace files.
The `output_residuals` option will enable the output of pre-fit and post-fit residual into the network trace file.
Enabling the `output_residual_chain` option will output the observed-minus-computed decomposition described above to the receiver trace file.
Setting `output_config` to true will cause Ginan to write the contents of input yaml configuration setting file/s to the top of output trace files.

More discussion of trace files is available [here](page.html?c=on&p=ginanUsage.index#logging-and-outputs)

### Mongo Outputs

The `mongo` section of the yaml configures outputs to the mongo database.

Ginan makes use of Mongo databases as part of its operations. The main uses of Mongo databases in Ginan are as a data sharing mechanism for real-time network solutions and, as is the case of the examples in this text, as a way of monitoring the inputs and processing of Ginan.

Use of mongo databases are enabled by using the `mongo:enable` option.
It is possible to confure two instances which can be enabled by setting the options to `primary`, `secondary`, `both` or `none`. This may be useful for configuring an instance that passes some limited data to another instance, while separately recording all trace data in another database.

When enabled, mongo will write a variety of types of data into the specified database.
For monitoring purposes the following data can be written to Mongo databases:
* By setting `output_states`, the state of estimated parameters are archived in the mongo database
* By setting `output_measurements`, pre-fit and post-fit residuals are archived in the mongo database
* By setting `output_test_stats`, statistics (like average and RMS) of residuals are estimated and included in the database
* By setting `output_trace` to true, further computed values of varying verbosity will be output according the the trace level set elsewhere.

Data archived into the database can be visualized using the EDA API included with Ginan. A detailed description on how to visualize can be found on the [GinanEDA](page.html?c=on&p=scripts.index#ginaneda) section of this manual.

Although not included in example yamls, if `mongo:output_ssr_precursors` is enabled parameters needed for the calculation of SSR messages will be loaded into the designated mongo database.
From here a Ginan instance (the one writing to the file or another that uses the results from the first one) can read the solutions, and assemble SSR messages.

### Example specific outputs

The `ppp_example.yaml` config is set up to provide a GPX output, which may be best suited for surveying or kinematic use-cases. The gpx file can be easily loaded into software such as QGIS to provide vector overlays on a street or satellite map, with associted metadata including time and orientation where available.

```
...
outputs:
    outputs_root:                 ./outputs/<CONFIG>
    gpx:
        output:                   true
        filename:                 <CONFIG>_<RECEIVER>_<YYYY><DDD><HH>.GPX
...
```

## Metadata and inputs

### Metadata input files

Ginan requires some auxiliary metadata files to correctly process data.
Metadata files are expected to remain constant/valid over a long period of time and use of the files available in the `ginan/inputData/products/tables` directory is recommended.

Included here are files such as

* **Satellite metadata :** the `igs_satellite_metadata.snx` contains metadata associated with the satellite body (e.g. satellite block type, SVN/PRN, satellite mass) used for attitude models and orbit estimation. The ANTEX file `M20.ATX` contains metadata for satellite and receiver antennas, mainly the PCO and PCV.

* **Earth tide description :** `fes2004_Cnm_Snm.dat` descrives global ocean tide loading (spherical harmonics up to order-degree 50) models, while `OLOAD_GO.BLQ` describes precise ocean tide loading parameters for specific locations (indexed by CORS station names). Earth tide models are used to adjust receiver-rover positions.

* **Gravity effects :** `EGM2008.gfc` describes the earth gravity field (spherical harmonics up to order-degree 15) and `DE436.1950.2050` describes the position of celestial bodies. Both files are used to estimate the combined gravity field acting on a satellite. Gravity field estimate is mainly used in satellite position estimation.

* **Geomagnetic fields :** `igrf13coeffs.txt` describes earth's geomagnetic field near earth (spherical harmonics up to order-degree 13). State of the geomagnetic field is used on the estimation of higher order components of Ionospheric delay on GNSS signals.

* **Tropospheric model :** `gpt_25.grd` contains global atmospheric pressure and temperature values ($5^o \times 5^o$ gridmaps) as well as hydrostatic and wet mapping coefficients for tropospheric delay modelling. Two tropospheric delay models are supported by Ginan: The GPT2 model, supported by this file, and the VMF3 model, which require it own set of files.

The file path defined in `inputs:inputs_root` will be added as a prefix to ALL files (including those defined in other yaml files).

### Metadata for Real-time rovers

While metadata inputs contain slowly changing parameters that are expected requirements for a large variety of scenarios others are more specific.

The `rt_ppp_example.yaml` file demonstrates a configuration specific to rover with real-time inputs.

This configuration file is roughly divided into 3 sections of note, the `receiver_options` section, the `inputs` section and the `processing_options` section.

The `receiver_options` section contains metadata for each GNSS receiver used in this test.
Four CORS are used as receivers for the purposes of this example.
The metadata included is self-explanatory.
Only `antenna_type`, which is used to select PCO and PCV values from the ANTEX file, is strictly necessary.
Here, `apriori_position` is included in the example for demonstration purposes, but is not expected in the case of moving receivers.

The `inputs` section contain the origin of receiver and satellite data to be processed in this example.

The example illustrates how to configure the real-time position estimation of a GNSS receiver.
As such, it uses data streams in place of files to obtain both `gnss_observations` and `satellite_data` using RTCM formatted streams broadcast by Geoscience Australia's NTRIP caster.

The contents of the `gnss_observations_root` (containing username, password and hostname for the NTRIP caster in this example) are applied to streams in the `gnss_observations` and `satallite_data` sections.
For this example the mountpoints corresponding to individual streams are included under `rtcm_inputs`.
It is possible to include the prefix in these fields, thus setting different host, username and password for each stream.

Ginan also supports the streaming of data from a local serial port, which would be set as below:
```inputs:
    gnss_observations:
        rtcm_inputs:
            - serial:///dev/ttyAMA0
```

Ginan supports receiving RTCM messages for broadcast ephemeris and SSR (including IGS-SSR) corrections, and for GNSS observations Ginan supports RTCM MSM messages (not legacy messages like 1004 and 1012) and some UBLOX receiver messages (by using `ubx_inputs`).

The `processing_options`, `receiver_options`, and `satellite_options` sections control how Ginan interprets and processes the data obtained.

The data for some of Ginan outputs can be taken from different sources.
One such example are `clock` states, sources for satellite and receiver clock estimate can be set by using the `sources` options in the relevant sections.
Supported options include:
* NONE: disable this element
* KALMAN: use internal estimates from Kalman filter
* PRECISE: use data from input files such as CLK/SP3/OBS.
* BROADCAST use data from broadcast ephemeris files or streams
* SSR: calculate values by adding SSR corrections (from streams or recorded files) to broadcast ephemeris

Ginan can combine outputs from multiple sources. For example, the default configuration of `outputs:clocks:satellite_sources` is `[KALMAN,PRECISE,BROADCAST]`, in this case Ginan will output estimates from the Kalman filter where it can, inputs from precise ephemeris files for satellites it can't find estimates for and finally from broadcast ephemeris as last resort.


The `ssr_antenna_offset` option clarifies the reference point the SSR satellite position corresponds to.
Possible values supported for this option are Centre of Mass (`COM`), or Antenna Phase Centre (`APC`).

The `epoch_interval` option defines the rate at which the input data will be processed by Ginan. It is set as 1 second/epoch to match the data rate of GNSS observation streams.
This option can be overwritten by later yaml files, if set higher than the input data interval Ginan will skip epochs to match the desired data rate.

The `wait_all_receivers` option instructs Ginan to wait for the set amount of seconds to data from all receivers to arrive.
These parameters needs to be set to the maximum difference in latency between data streams.
If GNSS observation from a stream fails to arrive within the specified time the receiver will be excluded from processing.

The `error_model` option describes the behaviour of unmodelled errors expected in the GNSS observations.
The possible values for this option are `constant` and `elevation_dependent`.

The `code_sigma` config sets the noise level (at the zenith) for pseudorange measurements and `phase_sigma` sets the noise level (at the zenith) for carrier phase measurements.


The `receiver_options` and `satellite_options` control various physical models applied to Ginan processing. The models include Earth tides, relativistic effects, ionospheric and tropospheric delays and phase windup.

The simplest configuration will specify values for `global`, which will be used by all receivers or satellites, however those values may be overridden by other configurations for specific receivers, satellites, block-types, or receiver brands. `ALIC`, `GPS`, `G04`, `GPS-I`, `trimble` etc, which will inherit all other parameters from most general to most specific.


#### Metadata for a post-processed network

The `process_network.yaml` sets a basic network solution which estimates satellite orbits and clock offsets for GPS, and code biases for a minimum number of signals.

The example describes a CORS network from which the network solutions will be estimated.

The CORS network contains more than 50 receivers, thus setting the receiver metadata through a `receiver_options` section is not practical; Instead a SINEX file is used to input the receiver metadata.
The file pointed at by `snx_files` option is expected to contain the receiver location, receiver, and antenna type for each receiver in the network.

The `rnx_inputs` option takes a list of RINEX files (RINEX 2,3 and 4 are supported) as inputs to Ginan processing.

Parameters in the `processing_options` section option serves the same purpose as in the case of `rt_ppp_example.yaml`. `epoch_interval` is set to 30 seconds here to reflect the data rate of RINEX files.

Other files used by Ginan for satellite data inputs are the `clk_files` which contain satellite and receiver clock offsets in RINEX CLK format, `bsx_files`, and `dcb_files`, which contain satellite and receiver hardware biases in bias SINEX and RINEX DCB formats respectively.


The outputs section of the file reflects the estimated parameters, and creates file-based outputs accordingly.
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
* Enabling `sinex` will output files with estimated receiver positions following receiver SINEX format.
* Enabling `erp` will output files with earth orientation parameters following IGS ERP format.
* Enabling `trop_sinex` will output files with zenith tropospheric delay following the troposphere SINEX format.
* Enabling `clock` will output files with satellite and receiver clock offset estimates in RINEX CLK format.
* Enabling `bias_sinex` will output files with satellite and receiver hardware bias using the bias SINEX format. Unlike clock outputs, which will be calculated every processing epoch, the update rate of hardware biases can be set to be longer than the epoch interval. The `code_output_interval` options set the output interval for code biases, setting this to 0 wil disable code bias output. Receiver and satellite bias can be enabled or disabled by using the `output_rec_bias` and `output_sat_bias` options.


## Processing and Estimation

Aside from the `outputs` section described above, the configuration of parameters to be estimated in the kalman filter must be defined.
The `estimation_parameters` section which describe WHAT parameters are estimated. The `processing_options` describe details on HOW the parameters are processed.

### Estimation configuration

The `estimation_parameters` section of `ppp_example.yaml` will contain only receiver-side parameters, while the `pod_example.yaml` will also contain satellite parameters to be estimated.

The parameters being estimated are
* Receiver position (`receivers:global:pos`)
* Receiver clock offsets (`receivers:global:clk`)
* Carrier phase ambiguities (`receivers:global:amb`)
* Tropospheric zenith delay (`receivers:global:trop`)
* North-South and East-West gradient on tropospheric zenith delays (`receivers:global:trop_grad`)
* Ionosphere Slant Total Electron Content along the satellite-receiver path (`receivers:global:ion_stec`)
* Receiver hardware biases on pseudorange measurements (`receivers:global:code_bias`)
* Receiver hardware biases on carrier phase measurements (`receivers:global:phase_bias`)

The main difference between a network solution and a rover solution is that satellite parameters (orbits, clocks, and biases) are estimated.
Thus included in the `satellites` part of `estimation_parameters`, are satellite clock offset (`clock`), satellite clock drift (`clock_rate`), code bias (`code_bias`) and phase biases (`phase_bias`)

A far simplified POD example is also available in `fit_sp3_pseudoobs.yaml`, which only estimates the satellites orbital and eop parameters `orbit`, `emp_x_y`, `eop`.
This uses sp3 pseudoobservations as inputs, and does not require calculation of other parameters such as tropospheres and other offsets.

Every parameter estimated by the Kalman filter is treated as a stochastic process following an Auto-Regressive model.
\begin{equation}
    x(t + t_0) = exp(-\frac{t0}{\tau})(x(t) -\mu) + \frac{w(t)}{\sqrt{t0}}
\end{equation}
The configuration options in this section help set aspects of this model.
`apriori_value` (default: 0) and `sigma` (default: Inf) sets the initial conditions.
`tau` (default: Inf) and `mu` (default: 0) sets $\tau$ and $\mu$ respectively.
`process_noise` (default: 0) set the standard deviation for w(t) (considered a zero mean Gaussian noise process).
As they are set in the `ppp_example.yaml` file (`tau` and `mu` at default values) the parameters are defined as a random walk process.

### Processing configuration

The `processing_options` is in itself divided into a variety of subsections.

`process_modes` is a fundamental setting that control over all processing settings, `ppp` needs to be set to `true` in order to enable Ginan 3 processing

The `gnss_general` subsection controls general aspects of GNSS processing.
On the input files described above, it set the expected noise levels of GNSS measurements
In this file it controls which GNSS measurements to use.

How each GNSS constellation is handled is described in `sys_options`.
The `process` option enables/disables the use of each constellation. Supported constellations are GPS, GLONASS, Galileo, Beidou and QZSS.

The signal list in `code_priorities` will govern if and how the GNSS signals are used, with signals not on the list not being used.
By setting `reject_eclipse` to `true` eclipsing satellites can be excluded from processing.

In addition to GNSS signals, parameters constraints or pseudo-observations can be added to the process.

The `filter_options` subsection configures the Kalman Filter operation.
Possible settings includes the type of numerical inverter, the outlier detection, and Rauch-Tung-Striebel (RTS) smoothing configuration.

Also included in the `ppp_example.yaml` file is the `chunking` option.
This option, to be used when processing multiple rovers, separates the filtering process by receiver thus reducing computational complexity of the solution.

The `model_error_handling` configures the handling of outliers detected by the Kalman filter.

The `epoch_control` subsection, used in input yaml files, controls which epochs to process and which to skip.


### Post-processed network

The `receivers` part of `estimation_parameters` is similar to the rover case
However it does contain additional constraints to eliminate rank deficiencies in the GNSS observation model.


In these examples, satellite code biases for specific signals are set to 0, in order to define the satellite clock as referring to two specific signals ([L1W,L2W] for GPS and [L1C,L5Q] for Galileo to match IGS standards)
In addition, the network solution estimates the Earth Orientation Parameters (`eop` and `eop_rate`)

There are few differences in the `process_options` section between `process_network.yaml` and the rover case.

The number of signals in the `code_priorities` list is different to reflect the fact that different receivers track a different set of codes. Some receivers track Galileo L1C and L5Q while others track the L1X-L5X pair, so both pairs should be tracked.
Some receivers do not track L1W thus the L1C needs to be added to use these receivers in network processing.

The `zero_receiver_dcb` sets the differential satellite code biase of the L1C-L5Q and L1X-L5X pairs to zero.
Finally the minimum constraints aligns the reference frame of the Ginan solution with an existing one (determined by the receiver position on the SINEX file).
Of particular note here is that unless the `once_per_epoch` option is set to `true`, alignment of reference frames happens after the last epoch of processing.
This means that results from other epochs are in an unconstrained reference frame. If alignment with a standard reference is required `once_per_epoch` needs to be set to true.

The `minimum_constraints` subsection configures the alignment of estimated solutions with a standard reference frame.

# Complete Ginan Examples

These examples demonstrate basic use cases which can be used as templates for further configuration


## ppp_example.yaml

### Basic post-processed Rover Position

\include "exampleConfigs/ppp_example.yaml"


## rt_ppp_example.yaml

### Basic real-time Rover Position

The `rt_ppp_example.yaml` example illustrates the real time estimation of rover positions.

It gets data from NTRIP streams, and processes it to obtain rover positions.

\include "exampleConfigs/rt_ppp_example.yaml"

## pod_example.yaml

### Network Solution: Orbits and Clocks

\include "exampleConfigs/pod_example.yaml"

## fit_sp3_pseudoobs.yaml

### Re-filter observations using sp3 file as input

\include "exampleConfigs/fit_sp3_pseudoobs.yaml"





