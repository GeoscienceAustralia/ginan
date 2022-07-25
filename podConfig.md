
# YAML Configuration for POD

The YAML configuration file for POD allows you to specify how and what data the POD will process and what results and statistics to report at the end.
In order to use the yaml configuration file you will need to specify this at the command line, with the \emph{-y <yaml_filename>} otherwise it will default to the (no longer supported) traditional \emph{POD.in, EQM.in and VEQ.in} option files.

	pod -y example_configuration.yaml

## POD processing options

These options will control how the pod will process the input files, with four different options available. Only one of the options listed below can be set to true, the remainder must be set to false.|

 Option | Values | Comments |
 - | - | - |
 `pod_mode_fit` | true or false | Orbit Determination (pseudo-observations; orbit fitting) |
 `pod_mode_predict` | true or false | Orbit Determination and Prediction |
 `pod_mode_eqm_int` | true or false | Orbit Integration (Equation of Motion only) |
 `pod_mode_ic_int` | true or false | Orbit Integration and Partials (Equation of Motion and Variational Equations) initial condition integration |

*POD YAML: processing options*

	pod_options:
	# Example YAML showing different processing options
	#--------------------------------------------------
		pod_mode_fit:     true   
		pod_mode_predict: false  
		pod_mode_eqm_int: false  
		pod_mode_ic_int:  false  

1. `pod_mode_fit` - this is used to fit an existing sp3 file (this is sometimes referred to as pseudo observations) with the parameters that are set later on. See pod example 1
1. `pod_mode_predict` - determine an orbit from observations and then predict the orbits path
1. `pod_mode_eqm_int` - determine the equations of motion only
1. `pod_mode_ic_int` -set up the initial conditions

## Time scale(time_scale)

Option | Values | Comments |
- | - | - |
`TT_time` | true or false | Terrestrial (TT)
`UTC_time` | true or false | Universal (UTC)
`GPS_time` | true or false | Satellite (GPS)
`TAI_time` | true or false | Atomic (TAI)

*POD YAML: Time scale options*

	time_scale:
		TT_time:  false
		UTC_time: false
		GPS_time: true
		TAI_time: false

## Initial Conditions (IC)

### IC input format (ic_input_filename)

 one is true, the other is false. If icf selected the ic_filename value specifies the path to the file.

 Option | Values | Comments |
 - | - | - |
  sp3 | true or false | sp3 format file|
  icf | true or false | initial conditions file|

*POD YAML: Initial Conditions input format options*

	ic_input_format:
		sp3:  true   # Input a-priori orbit in sp3 format
		icf:  false  # Input a-priori orbit in POD Initial Conditions File (ICF) format
		ic_filename: some_file

### IC input reference system (ic_input_refsys)

reference system for the initial conditions one is true, the other is false. 

Option | Values | Comments |
- | - | - |
`itrf` | true or false | terestrial|
`icrf` | true or false | celestial |
`kepler` | true or false | polar form of celestial|

*POD YAML: Initial Conditions reference system*

	ic_input_refsys:
		itrf: true   # Initial Conditions Reference Frame: ITRF, ICRF
		icrf: false  # Initial Conditions Reference Frame: ITRF, ICRF

## Using Pseudo observations

These options are used to control how pseudo observations are used by the POD.


Option | Values | Comments |
- | - | - |
`pseudobs_orbit_filename` | filename | \emph{path to the observations file}|
`pseudobs_interp_step` | int |  Interval (sec) of the interpolated orbit|
`pseudobs_interp_points` | int | Number of data points used in Lagrange interpolation (at least 2 but recommended 6 to 12)|
`IC_time` | datetime | YYYY MM DD hh mm ss.ss |

*POD YAML: Using pseudo observations*

	pseudobs_orbit_filename: igs19424.sp3 # Pseudo observations orbit filename
	pseudobs_interp_step:    900          # Interval (sec) of the interpolated orbit
	pseudobs_interp_points:  12           # Number of data points used in Lagrange interpolation
	IC_time: 2017 03 10 03 0 0.0

## Orbit arc length

Option | Values | Comments |
- | - | - |
`orbit_arc_determination` | int |\emph{number of hours to integrate}|
`orbit_arc_prediction` | int |  \emph{number of hours to predict at end of orbit arc}|
`orbit_arc_backwards` | int | \emph{number of hours to check before start of orbit arc}|

*POD YAML: Orbit arc options*

	# Orbit arc length (in hours)
		orbit_arc_determination: 24  # Orbit Estimation arc
		orbit_arc_prediction:    12  # Orbit Prediction arc
		orbit_arc_backwards:     2   # Orbit Propagation backwards arc

## External Orbit Comparison
In this section only one of the following options listed below can be set to true, the remainder must be set to false.|

Option | Values | Comments |
- | - | - |
`ext_orbit_enabled`  | true or false | |
`ext_orbit_type_sp3`  | true or false | |
`ext_orbit_type_interp` | true or false | |
`ext_orbit_type_kepler` | true or false | |
`ext_orbit_type_lagrange` | true or false | |
`ext_orbit_type_position_sp3` | true or false | |
`ext_orbit_filename` | filename |  \emph{path to the orbit file}|
`ext_orbit_interp_step` | int | Interval (sec) of the interpolated\/Kepler orbit|
`ext_orbit_interp_points` | int | Number of data points used in Lagrange interpolation (at least 2 but recommended 6 to 12)|

*POD YAML: External orbit options*

	# External Orbit Comparison
		ext_orbit_enabled: true
		ext_orbit_type_sp3:       false        # Orbit data in sp3 format 
		                                       # (including position and velocity vectors)
		ext_orbit_type_interp:    true         # Interpolated orbit based on Lagrange 
		                                       # interpolation of sp3 file
		ext_orbit_type_kepler:    false        # Keplerian orbit
		ext_orbit_type_lagrange:  false        # 3-day Lagrange interpolation
		ext_orbit_type_position_sp3:    false  # Position and SP3 file
		ext_orbit_filename:       igs19424.sp3 # External (comparison) orbit filename
		ext_orbit_interp_step:    900          # Interval (sec) of the interpolated/Kepler orbit
		ext_orbit_interp_points:  12           # Number of data points used 
		                                       # in Lagrange interpolation

### External orbit reference frame (ext_orbit_frame)

Option | Values | Comments |
- | - | - |
`itrf`  | true or false | terrestrial|
`icrf`  | true or false | celestial|
`kepler` | true or false | kepler orbital elements|

*POD YAML: External orbit reference system*

	   ext_orbit_frame:
			itrf: true            # External orbit reference frame - ITRF
			icrf: false           # External orbit reference frame - ICRF
			kepler: false	


## Earth Orientation Parameters
In this section only one of the following options listed below can be set to true, the remainder must be set to false.

### EOP type

Option | Values | Comments |
- | - | - |
`EOP_soln_c04` | true or false | C04 is the IERS solution|
`EOP_soln_rapid` | true or false | Rapid is the rapid\/prediction center solution|
`EOP_soln_igs` | true or false | igs is the ultra-rapid solution using partials. To use this you need both the rapid file and partials file.|
`EOP_soln_c04_file` | filename | |
`EOP_soln_rapid_file` | filename | |
`ERP_soln_igs_file` | filename | |
`EOP_soln_interp_points` | int | |

*POD YAML: Earth Orientation Parameter solution options*

	EOP_soln_c04:   true         # IERS C04 solution : EOP_sol = 1
	EOP_soln_rapid: false        # IERS rapid service/prediction center (RS/PC) Daily : EOP_sol = 2
	EOP_soln_igs:   false        # IGS ultra-rapid ERP + IERS RS/PC Daily (dX,dY) : EOP_sol = 3. Need both rapid_file AND igs_file
	EOP_soln_c04_file: eopc04_14_IAU2000.62-now
	EOP_soln_rapid_file: finals2000A.daily
	ERP_soln_igs_file: igu18543_12.erp
	EOP_soln_interp_points: 4    # EOP solution interpolation points

### IAU Precession-Nutation model


Option | Values | Comments |
- | - | - |
`eop_soln_interp_points` | int | number of data points to be used in an eop interpolation (at least 2!)|
`iau_model_2000` | true or false | |
`iau_model_2006` | true or false | |

*POD YAML: EOP model options*


	# IAU Precession-Nutation model:
	iau_model_2000: true         # IAU2000A: iau_pn_model = 2000
	iau_model_2006: false        # IAU2006/2000A: iau_pn_model = 2006


## Input files


Option | Values | Comments |
- | - | - |
`gravity_model_file` | filename | |
`DE_fname_header` | filename |  Emphemeris header file |
`DE_fanme_data` | filename | Emphemeris data file |
`ocean_tides_model_file` | filename | |
`leapsec_filename` | filename | leapseconds to be added|
`satsinex_filename` | filename | sinex file with satellite meta-data|       

*POD YAML: Input files*

	# Gravity model file
	gravity_model_file: goco05s.gfc  
	# goco05s.gfc, eigen-6s2.gfc, ITSG-Grace2014k.gfc

	# Planetary/Lunar ephemeris - JPL DE Ephemeris
	DE_fname_header: header.430_229
	DE_fname_data:   ascp1950.430

	# Ocean tide model file
	ocean_tides_model_file: fes2004_Cnm-Snm.dat 
	# FES2004 ocean tide model

	# Leap second filename
	leapsec_filename: leap.second

	# Satellite metadata SINEX
	satsinex_filename: igs_metadata_2063.snx	

## Output options

Option | Values | Comments |
- | - | - |
`sp3_velocity` | true or false | if you wish to write out the velocities for comparison |
`partials_velocity` | true or false | if you wish to write velocity vector partials to the output file|

*POD YAML: Output options*


	# Write to sp3 orbit format: Option for write Satellite Velocity vector
	sp3_velocity: false        #  Write Velocity vector to sp3 orbit

	#----------------------------------------------------------------------
	# Write partials of the velocity vector w.r.t. parameters into the orbits_partials output file:
	partials_velocity: false   # Write out velocity vector partials wrt orbital state vector elements

## Variational Equation Options

Option | Values | Comments |
- | - | - |
`veq_integration`| true or false |   pod mode overides it anyway. Ignore.|
`ITRF` | true or false | reference_frame|
`ICRF` | true or false |  one must be true|   
`kepler` | true or false | |    

*POD YAML: VEQ ref system*

	# Variational Equations
	veq_integration: false

	#----------------------------------------------------------------------
	# Reference System for Variational Equations'  - Partials | Parameter Estimation
	veq_refsys:
	itrs: true      # ITRS: Terrestrial Reference System
	icrs: false     # ICRS: Celestial Reference System
	kepler: false

## General Options

Option | Values | Comments |
- | - | - |
`estimator_iterations` | int | integrate this number of times, using the generated initial conditions from the previous run as a start point|   

*POD YAML: general options*

	# Parameter Estimation
	estimator_iterations: 2

## Apriori solar radiation models   

Option | Values | Comments |
- | - | - |
`no_model`  | true or false | point source only|
`cannon_ball_model` | true or false | see \nameref{sec:cannonball_srp}|
`simple_boxwing_model` | true or false | as stated|
`full_boxwing_model` | true or false | as stated|

*POD YAML: Apriori SRP model*

	srp_apriori_model:
		no_model:   false
		cannon_ball_model:   true
		simple_boxwing_model:   false
		full_boxwing_model:     false

### Estimated Solar radiation models

Option | Values | Comments |
- | - | - |
`ECOM1`  | true or false | ECOM1 params only| 
`ECOM2`  | true or false | ECOM2 params only|
`hybrid` | true or false |  any mix of ECOM1 and ECOM2|
`SBOXW`  | true or  false | Simple box wing model|
`EMPirical models` | true or false | Empirical is independent of the other four|

*POD YAML: Estimated SRP models*

	srp_apriori_model:
		no_model:               false
		cannon_ball_model:      true
		simple_boxwing_model:   false
		full_boxwing_model:     false

### gravity_model

Type of gravity model to apply, only one option can be true.

Option | Values | Comments |
- | - | - |
`central_force`            | true or false |  | 
`static_gravity_model`    | true or false |  |
`time_variable_model`     | true or false |  |
`iers_geopotential_model` | true or false |  |

*POD YAML: Gravity Models*


	gravity_model:
		central_force:           false   # Central force gravity field              : gravity_model = 0
		static_gravity_model:    false   # Static global gravity field model        : gravity_model = 1
		time_variable_model:     true    # Time-variable global gravity field model : gravity_model = 2
		iers_geopotential_model: false   # IERS conventional geopotential model     : gravity_model = 3

### stochastic pulse (pulse)

Do not mix pulses in R/T/N (terrestrial) with pulses in (X/X/Z)

Option | Values | Comments |
- | - | - |
`enabled` | true or false | then if true: |
`epoch_number` | int | number of epochs to apply pulses each day |
`offset` | int | seconds until the first pulse of the day |
`interval` | int | seconds between each pulse (after the first)|
directions | | |
`x_direction` |  true or false |  |
`y_direction` |  true or false |  |
`z_direction` |  true or false |  |
`r_direction` |  true or false |  |
`t_direction` |  true or false |  |
`n_direction` |  true or false |  |

*POD YAML:stochastic pulse options*

	pulse:
	enabled:      false
	epoch_number:     1    # number of epochs to apply pulses
	offset:       43200    # since the start of day
	interval:     43200    # repeat every N seconds
	directions:
		x_direction:   true
		y_direction:   true
		z_direction:   true
		r_direction:   false
		t_direction:   false
		n_direction:   false
	reference_frame:
		icrf:          true
		orbital:       false

## Inclusions/Exclusions

The pod can be configured to only do certain constellations, and to include or exclude selected PRNs. For constellations just have true or false after each. Inclusions/exclusions use the same format, and limit to (or exclude) the stated PRNs in the given list


	# optional limiting to constellations 
	#----------------------------------------------------------------------   
	   constellations:
	      GPS:      false
	      GALILEO:  false
	      GLONASS:  true
	      BEIDOU:   false
	      QZSS:     false

	# optional limiting to PRNs 
	#----------------------------------------------------------------------   
	   prn_inclusions (or prn_exlusions):
	      - G01
	      - E01
	      - G23
	      - R22

## EQM/VEQ options

You can set different parameters for EQM and VEQ, but the sections labels are the same.

### Integration Step

Option | Values | Comments |
- | - | - |
`RK4_integrator_method` | true or false |  Do not use RK4 for veq as it is not implemented| 
`RKN7_integrator_method` | true or false | only one can be true| 
`RK8_integrator_method`  | true or false | |
`integrator_step` | int | step size in seconds |

*POD YAML: Integration Step models*


	# Numerical integration method
	# Runge-Kutta-Nystrom 7th order RKN7(6): RKN7, Runge-Kutta 4th order: RK4, Runge-Kutta 8th order RK8(7)13: RK8
	integration_options:
		RK4_integrator_method: false
		RKN7_integrator_method: true
		RK8_integrator_method: false
		integrator_step: 900         # Integrator stepsize in seconds	

### Gravity Field

Option | Values | Comments |
- | - | - |
`enabled` | true or false | and if true:|
`gravity_degree_max` |  | maximum model terms in spherical harmonic expansion | 
`timevar_degree_max` |  | maximum time variable model terms in spherical harmonic expansion |

*POD YAML: Gravity Models*

	# Gravitational Forces
	gravity_field:
		enabled: true
		gravity_degree_max:      15      # Gravity model maximum degree/order (d/o)
		timevar_degree_max:      15      # Time-variable coefficients maximum d/o

### planetary_perturbations:

Option | Values | Comments |
- | - | - |
`enabled` | true or false | Uses the emphemeris | %TODO specify the file

*POD YAML: planetary perturbations*

	# Planetary Gravitational Forces
		planetary_perturbations:
			enabled: true	

### tidal_effects:

Option | Values | Comments |
- | - | - |
`solid_tides_nonfreq` | True or False | frequency independent Solid Earth Tides |
`solid_tides_freq` | True or False | frequency dependent Solid Earth Tides |
`ocean_tides` | True or False | uses the ocean tides file |
`solid_earth_pole_tides` | True or False | tide induced earth spin rotation not about the centre of the ellipsoid |
`ocean_pole_tide` | True or False | ocean response to the above |
`ocean_tides_degree_max` | True or False | maximum model term in spherical harmonic expansion |

*POD YAML: tidal effects*

	tidal_effects:
	enabled: true
	solid_tides_nonfreq:    true   # Solid Earth Tides frequency-independent terms
	solid_tides_freq:       true   # Solid Earth Tides frequency-dependent terms
	ocean_tides:            true   # Ocean Tides
	solid_earth_pole_tides: true   # Solid Earth Pole Tide
	ocean_pole_tide:        true   # Ocean Pole Tide
	ocean_tides_degree_max: 15     # Ocean Tides model maximum degree/order	

## relativistic_effects:

Option | Values | Comments |
- | - | - |
`enabled` | true or false | Lens Thinning, SchwarzChild and deSitter effects, there are no means to separate these effects currently. The Lens Thirring effect is calculated but subsequently ignored in the POD.|

*POD YAML:relativistic_effects*

## non_gravitational_effects:
### Models to be applied:

Option | Values | Comments |
- | - | - |
`solar_radiation` | true or false | radiation push from the sun |
`earth_radiation` | true or false | radiation push from the earth |
`antenna_thrust`  | true or false | reverse thrust from antenna radiation |

*POD YAML: non gravitational effects*

	# Non-gravitational Effects
	non_gravitational_effects:
		enabled: true
		solar_radiation: true
		earth_radiation: true
		antenna_thrust:  true

### Empirical parameters

Option | Values | Comments |
- | - | - |
`ecom_d_bias` | true or false | | 
`ecom_y_bias` | true or false | | 
`ecom_b_bias` | true or false | | 
`ecom_d_cpr` | true or false | (only ECOM1\/hybrid)| 
`ecom_y_cpr` | true or false | (only ECOM1\/hybrid)| 
`ecom_b_cpr` | true or false | |
`ecom_d_2_cpr` | true or false |  (only ECOM2\/hybrid)| 
`ecom_d_4_cpr` | true or false | (only ECOM2\/hybrid) |
`emp_r_bias` | true or false | |
`emp_t_bias` | true or false | | 
`emp_n_bias`  | true or false | |
`emp_r_cpr`  | true or false | |
`emp_t_cpr`  | true or false | |
`emp_n_cpr` | true or false | |
`cpr_count`  | int | empirical cpr count |

*POD YAML: Configuration options for solar radiation pressure models*

	# Non-gravitational Effects
	srp_parameters:
		ECOM_D_bias:  true
		ECOM_Y_bias:  true
		ECOM_B_bias:  true
		EMP_R_bias:   true
		EMP_T_bias:   true
		EMP_N_bias:   true
		ECOM_D_cpr:   true
		ECOM_Y_cpr:   true
		ECOM_B_cpr:   true
		ECOM_D_2_cpr: false
		ECOM_D_4_cpr: false
		EMP_R_cpr:    true
		EMP_T_cpr:    true
		EMP_N_cpr:    true
		cpr_count:    1



NB EQM and VEQ srp parameters MUST be identical. May move into pod_options in future.
overrides are not implemented yet. Ignore for now. We imagine overrides at the system, block (sat type) and individual satellite level


## overides*
`*This section has not yet been implemented in the POD, and is a placeholder for future versions.`

In this section put any system, block or PRN overrides that are different to the ones chosen before

	overrides:
		system:
		GPS:
		srp_apriori_model:
		no_model:   false
		cannon_ball_model:   true
		simple_boxwing_model:   false
		full_boxwing_model:     false
		GAL:
		srp_apriori_model:
		no_model:   false
		cannon_ball_model:   false
		simple_boxwing_model:   false
		full_boxwing_model:     true
		GLO:
		srp_apriori_model:
		no_model:   false
		cannon_ball_model:   false
		simple_boxwing_model:   true
		full_boxwing_model:     false
		BDS:
		srp_apriori_model:
		no_model:   false
		cannon_ball_model:   false
		simple_boxwing_model:   true
		full_boxwing_model:     false
	block:
		GPS-IIF:
			srp_apriori_model:
			no_model:   false
			cannon_ball_model:   false
			simple_boxwing_model:   true
			full_boxwing_model:     false
		# GPS BLK IIF use ECOM2 parameters
		srp_parameters:
			ECOM_D_bias:   true
			ECOM_Y_bias:   true
			ECOM_B_bias:   true
			ECOM_D_2_cpr: false
			ECOM_D_4_cpr: false
			ECOM_B_cpr:   true
	prn:
		G01:
			srp_apriori_model:
				no_model:   false
				cannon_ball_model:   false
				simple_boxwing_model:   false
				full_boxwing_model:     true
		
