





recording:
This can be used to read data that has been saved from a stream for later testing.







#### Real-time processing:

To process data in real-time you will need to set up the location, username annd password for the caster that you will be obtaining the input data streams from in the configuration file.

The pea supports obtaining streams from casters that use NTRIP 2.0 over http and https.

	# realtime streaming example
	station_data:
		
		stream_root: "http://<username>:<password>@auscors.ga.gov.au:2101/"

		nav_streams:
			- BCEP00BKG0
			- SSRA00CNE0
			
		obs_streams:
			- STR100AUS0
			
		ssr_input_antenna_offset: APC




processing_options:

    epoch_control:
        start_epoch:                2019-07-18 01:15:00
        end_epoch:                  2019-07-18 22:30:00
        max_epochs:                 12        #0 is infinite # comment for full day run
        epoch_interval:             60          #seconds
   






realtime




#### wait_next_epoch:
Expected time interval between successive epochs data arriving. For real-time this should be set equal to epoch_interval.

#### wait_all_stations:
Window of delay to allow observation data to be received for processing.
Processing will begin at the earliest of:

* Observations received for all stations
* wait_all_stations has elapsed since any station has received observations
* wait_all_stations has elapsed since wait_next_epoch expired.



#### epoch_interval:
Increment in nominal epoch time for each processing epoch. This parameter may be used to sub-sample datasets by using an epoch_interval that is a multiple of the dataset's internal interval between epochs.


## processing_options:

This sections specifies the extent of processing that is performed by the engine.

#### start_epoch
Nominal time of the first epoch to process. Time is formatted as YYYY-MM-DD HH:MM:SS.
This parameter may be left undefined to use the first available data point.

#### end_epoch
Maximum nominal time of the last epoch to process. This parameter may be left undefined.

#### max_epochs:
Maximum epochs to process before completion. This parameter may be left undefined.


#### deweight_factor:
Factor by which measurement variances are increased upon detection of a bad measurement.








#### config_description:

The value entered here is used to complete the \verb|<CONFIG>| wildcard.
This may enable a single change in the yaml file to make changes to many options, including output folders and filenames.

#### analysis_agency, analysis_center, analysis_program, rinex_comment:

String to be written within files during various output files' generation.

## user_filter_parameters, network_filter_parameters, ionosphere_filter_parameters:

	user_filter_parameters:

	    max_filter_iterations:      5
	    max_prefit_removals:        3

	    rts_lag:                    -1      #-ve for full reverse, +ve for limited epochs
	    rts_directory:              ./
	    rts_filename:               PPP-<CONFIG>-<STATION>.rts

	    inverter:                   LLT         #LLT LDLT INV

#### `filter_options:`

The internal operation of the Kalman filter is specified in this section. It has a large impact on the robustness, and associated processing time that the filter will achieve.

	filter_options:

		outlier_screening:
		    max_filter_iterations:      5
		    max_prefit_removals:        3

        rts:
            enable:                 true
            lag:                    -1      #-ve for full reverse, +ve for limited epochs

	    inverter:                   LLT


##### max_filter_iterations:

Maximum number of times to compute the full update stage due to rejections.

This is similar to the max_filter_rejections parameter, but the 4-sigma check is performed with post-fit residuals, which are much more precise.

Rejections that occur in this stage require the entire filter inversion to be repeated, and has an associated performance hit when used excessively.

##### max_prefit_removals:

Maximum number of pre-fit residuals to reject from the filter.

After the vector of residuals has been generated and before the filter update stage is computed, the residuals are compared with the expected values given the existing states and design matrix.
If the values are deemed to be unreasonable - because the variances of the transformed states and measurements do not overlap to with a 4-sigma level of confidence - then these measurements are deweighted by deweight_factor, to prevent the bad values from contaminating the filter.

These measurements are recorded as being rejected, and may have additional consequences according to other configurations such as phase_reject_limit.

#### rts_lag:

Number of future epochs to use in RTS smoothing. 
A larger lag will give more optimal smoothing results, at the expense of a longer lag before they are calculated, and requiring more processing time per epoch.

A negative value indicates that the entire solution should be smoothed at the conclusion of processing. 
This will obtain optimal results, with lowest processing time, but is not suitable for real-time applications.







