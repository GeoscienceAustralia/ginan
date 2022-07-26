# PEA Configuration File - YAML
	
The PEA processing engine uses a single YAML file for configuration of all processing options. Details about the format and subset of the most important options available will be described here.

## Default Values and Parameter Descriptions

Many processing options have default values associated with them. To prevent repetition, and to ensure that the values are reported correctly, these values may be printed to the screen by the `pea` application.

The default value and description of almost every config parameter can be printed by using:

    ./pea -Y

The parameters will be output while retaining the required structure within the yaml format.

## Ginan Yaml Inspector

To aid in the creating and editing of configuration files, the `pea` is capable of generating a tool for inspecting and outputting yaml files.

Running `./pea -Y` will create the `GinanYamlInspector.html` file, which can be opened in a web browser.

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

## Globbing
Files may be specified individually, as lists, or by searching available files using a globbed filename using the star character (`*`)

## Wildcard Tags
Output filenames can include wildcards wrapped in `< >` brackets to allow more generic names to be used. While processing, these tags are replaced with details gathered from processing, and allows for automatic generation of, for example, hourly output files.

#### `<CONFIG>`
This is replaced with the 'config_description' value entered in the yaml file.
#### `<STATION>`
This is replaced with the 4 character station id of a given station.
#### `<STREAM>`
This is replaced with the id of the a given stream where appropriate.
#### `<LOGTIME>`, `<DDD>`,`<D>`, `<WWWW>`, `<YYYY>`, `<YY>`, `<MM>`, `<DD>`, `<HH>`
These are replaced with the (rounded) time of the epochs within the trace file.

If trace file rotation is configured for 1 hour, the `<LOGTIME>` wildcard will be rounded down to the closest hour, and subsequently change value once per hour and generate a separate output file for each hour of processing.

## Major YAML configs



#### `root_directory / root_url`
This specifies a root string to be prepended to all other file paths specified in the section. For file paths that are absolute, (ie. starting with a `/`), this parameter is not applied.


#### `inputs:`

This section of the yaml file specifies the lists of files to be used for general metadata inputs, and inputs of external product data.

##### `gnss_observations:`

This section specifies the sources of observation data to be used in positioning.


There are numerous ways that the `pea` can access GNSS observations to process. 
You can specify individual files to process, set it up so that it will search a particular directory, or you can use a command line flag `--rnx <rnxfilename>` to add an additional file to process. 

The data should be uncompressed rinex (gunzipped, and not in hatanaka format), or RTCM3 formatted binary data.


It may consist of RINEX files, or RTCM streams or files, which are specified as follows:

```
	gnss_observations:
		root_stations_directory: /data/acs/ginan/examples/data
		rnx_files:
			- "ALIC*.rnx"
			- "BAKO*.rnx"
			
		#rtcm_files:
		#	- "*-OBS.rtcm3"

		#streams:
		# - "https://<USER>:<PASS>@ntrip.data.gnss.ga.gov.au/ALIC00AUS0"  
```     

The first 4 characters of the filename are used as the receiver ID.

If multiple files are supplied with the same ID, they are all processed in sequence - according to the epoch times specified within the files. In this case, it is advisible to correctly specify the start_epoch for the filter, or the first epoch in the first file will likely be used.



##### `satellite_data:`

This section specifies sources of ephemerides and other satellite data.

```
    satellite_data:
        root_directory:    ../data/recordings/
        
        rtcm_files:
            - "*NAV.rtcm3"
            
        #root_stream_url: "https://<USER>:<PASS>@ntrip.data.gnss.ga.gov.au/
        #streams:
        # - "BCEP00DLR0"
        # - "SSRA02IGS0"
```



#### `outputs:`

This section of the yaml file specifies options to enable outputs and specify file locations.

Each section typically contains an option to `output` the filetype, and a `directory` to place the files named `filename`, along with any ancillary options.

An example of this section follows:

```
outputs:

    root_directory:             <CONFIG>/

    trace:
        output_stations:        true
        level:                  3
        directory:              ./
        station_filename:       <CONFIG>-<STATION><YYYY><DDD><HH>.TRACE
        output_residuals:       true

    sinex:
        output:                 true
        directory:              ./
    
    clocks:                 
        output:                 true
        directory:              ./
        filename:               <CONFIG><LOGTIME>.clk

    output_rotation:
        period:                 6
        period_units:           hours
```


##### `output_rotation:`
Granularity of length of time used for `<LOGTIME>` tags. These parameters may be used such that the filename of an output will change intermittently, and thus distribute the output over multiple files.

The `<LOGTIME>` tag is updated according to the epoch time, not the current clock time.

`output_rotation: period` must be a numeric value, and `period_units` may be one of seconds (default), minutes, hours, days, weeks, years, (with or without plural s).




#### `estimation_parameters`

The majority of estimated states are configured in this section. All elements within the Kalman filter are configured using a consistent format.

The parameters that are available for estimation include:

* `stations:` - per station parameters
    * `pos` - position of station
    * `pos_rate` - rate of change of position of station (velocity)
    * `clk` - clock offset of station
    * `clk_rate` - rate of change of clock offset of station (clock skew)
    * `amb` - carrier phase ambiguity
    * `trop` - vertical troposphere delay at station
    * `trop_grads` - gradients of troposphere in North and East components
* `satellites:` - per satellite parameters
    * `pos` - position of satellite (coming soon)
    * `pos_rate` - rate of change of position (coming soon) of satellite (velocity)
    * `clk` - clock offset of satellite
    * `clk_rate` - rate of change of clock offset of satellite (clock skew)
    * `orb` - orbital corrections to be used with POD module.
* `eop` - earth orientation parameters (polar motion, delta length of day)


The parameters should be added in a list format, using `[ ]`, with each element corresponding to the appropriate `num` within the state, eg `[X,Y,Z]`, `[N,E,D]`. When a list contains fewer than required parameters, the last parameter will be used for all missing parameters. ie. Setting `sigma: [10]` is sufficient to set all x,y,z components of the apriori standard deviation to 10.



```
estimation_parameters:
    stations:

        STATE:
            estimated:          [true]
            sigma:              [0.1]
            proc_noise:         [0] 
            proc_noise_dt:      second
            apriori:            [0]       # usually overwritten with other source, rinex file etc.

    satellites:

        STATE:
            estimated:          [true]
            sigma:              [0.1]
            proc_noise:         [0] 
            proc_noise_dt:      second
            apriori:            [0] 
```

In the case that a specific station or satellite requires an alternate configuration, or to exclude estimates entirely, the override_filter_parameters section may be used to overwrite selected components of the configuration.

Parameters may be reconfigured based on the station or satellite that they correspond with using the same structure within the `overrides` element, for example in the case where the `process_noise` or `sigma` vary between receivers or transmitters.


```
	overrides:

		stations:
			XMPL:
				STATE:
					proc_noise: ...

		satellites
			GXX:
				STATE:
					sigma: ...
```


##### `estimated`:

Boolean(s) to add the state to the Kalman filter for estimation.

##### `sigma`:

List of a-priori sigma values for each of the components of the state.

If the sigma value is left as zero (or not initialised), then the initial variance and value of the state will be estimated by using a least-squares approach.
In this case, the user must ensure that the solution is likely rank-sufficient, else the least-squares initialisation will fail.

##### `proc_noise`:

List of process noises to be added to the state during state transitions. These are typically in m/sqrt(s), but different times may be assigned separately.

##### `proc_noise_dt`:

Unit of measure for process noise. 
May be left undefined for seconds, or using sqrt_second, sqrt_seconds, sqrt_minutes, sqrt_hours, sqrt_days, sqrt_weeks, sqrt_years.

##### Process Noise Guidelines

The units are typically in meters, and they are given as $\sigma$ = $\sqrt{variance}$

For a random walk process noise, the process noise is incremented at each epoch as $\sigma^2\times dt$ where dt is the time step between filter updates.

If you want to allow kinematic processing, then you can increase the process noise e.g.`proc_noise: [0.003]` equates to $0.003\frac{1}{\sqrt{s}}$

Or if you wanted highway speeds 100km/hr = 28 m/s

    proc_noise:       [28]
    proc_noise_dt:    second

A nice value for using VMF as an apriori value is 0.1mm /sqrt(s)

	trop:
	    estimated:          true
	    sigma:              [0.1]
	    proc_noise:         [0.01]
	    proc_noise_dt:      hour
