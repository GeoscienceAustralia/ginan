
# Overview of the PEA

The `pea` is in essence a configurable, robust, application-specific Kalman filter.

* Kalman filters are known for being the optimal method for estimating parameters - in linear systems, and provided an accurate system model is avaiable.
* The `pea` contains accurate system models and linearisation routines for satellite positioning.
* It performs statistical monitoring and error checking, to ensure the robustness of results that is required for operational use, and
* It has has an intuitive configuration, using heirarchical config files that allow you to tell the software precisely what and how to process gnss data.

The flow through the software is largely sequential, ensuring simplicity of understanding for developers and users alike. The major components of this flow are outlined below:

## Data Input and Synchronisation

Before the processing of data from an epoch is initiated, all other relevant data is accumulated. As this code affects global objects that have effects in multiple places, this code is run in a single thread until data processing is ready to begin.

#### Config

Configurations are defined in YAML files. At the beginning of each epoch the timestamp of the configuration file is read, and if there has been a modification, new parameters in the configuration will be loaded into memory.

#### Product Input

Various external products may be required for operation of the software, as defined in the configuration file. At the beginning of each epoch, if any product input files have been added to the config, or if the inputs are detected to have been modified, they will be re-read into memory.

#### Metadata Input

Metadata such as GNSS ephemerides are available from external sources to augment the capability of the software. This data is ingested at the beginning of each epoch before processing begins.

#### Observation Data Input

Observation data forms the basis for operation of the software. Observations from various sources are synchronised and collated at the beginning of each epoch before processing begins. The software uses class inheritance and polymorphism such that all data type inputs are retrieved using a single common instruction, with backend functions performing any retrieval and parsing required.

Observation data is synchronised by timestamp - when the main function requests data of a specific timestamp all data until that point is parsed (but may be discarded), before the observation data corresponding to that timestamp being used in processing.

#### SSR Data Input

State Space Representation (SSR) messages contain the values of different GNSS error components, such as satellite clocks & orbits, hardware biases and ionosphere delays. These SSR components can be applied to GNSS observations to correct their error components. Streams containing SSR messages are available via NTRIP casters, which enable performing PPP in real-time.

The software can decode SSR messages for the following constellations:
* GPS
* GALILEO
* GLONASS
* BEIDOU
* QZSS
* SBS

For each constellation, the following GNSS error components can be decoded and applied:
* Satellite clocks
* High-rate satellite clocks
* Satellite orbits
* Code & phase biases
* User Range Accuracy (URA)


#### Initialisation of Objects

During the following stages of processing many receiver-specific objects may be created within global objects. To prevent thread collision in the global objects, the receiver-specific objects are created here sequentially.

## Preprocessor

The preprocessor is run on input data to detect the anomalies and other metrics that are available before complete processing of the data is performed. This enables low-latency reporting of issues, and prepares data for more efficient processing in the later stages of operation.

* Cycle Slip Detection
* Low Latency Anomaly Detection
* Missing Data
* Output / Reporting

## Precise Point Positioning

The largest component of the software, the PPP module ingests all of the data available, and applies scientific models to estimate and predict current and future parameters of interest.

Version 1 of the GINAN toolkit satisfies many of the requirements for GNSS modelling, but has been achieved by incrementally adding features as they became available and as scientific models have been developed. Many of the components make assumptions about the outputs of previous computations performed in the software, and require care before adding or making changes to the code, or even setting configuration options.

It is tempting for researchers to apply heuristics or corrections that may have been historically used to assist in computation, but these must be limited to effects that can be modelled and applied through the Kalman filter, in order to maintain the efficiency and robustness that it provides.

As the models required for ‘user’, ‘network’, and even ‘ionosphere’ modes are equivalent, the only distinction between the modes is the extent of modelling to be applied, which can be reduced to a simple configuration change. As such the parallel streams within the software will be eliminated and reduced to a single unified model, with example configurations for common use-cases.

It is intended that the software will be reorganised with the benefit of hindsight, to remove interactions between modules and explicitly execute each processing step in a manner similar to an algebraic formulation used by experts in the field.


#### Force and Dynamic Models

At the beginning of processing of an epoch, parameters with time-dependent models are updated to reflect the time increment since the previous epoch. Simple models will be well defined when initialised, but more complex models will require updating at every epoch.

Ultimate positioning performance largely depends on accurate dynamic models, with development of these models improving predictive capability, and reducing uncertainty and adjustments at every point in time.

* Gravity
* Solar Radiation Pressure
* Other

#### Orbit and State Prediction

Before the available observations for an epoch are utilised, a prediction is made of the parameters of interest by utilising the previous estimates and applying dynamic models through the Kalman filter’s state transition.

#### Phenomena Modelling + Estimation

In order to accurately estimate and predict parameters of interest, all phenomena that affect GNSS/SLR observations must be isolated and modelled, as being components comprising the available measurements.

Where the values of parameters are well known they may be used directly to extract other parameters of interest from the data - such as using published corrections to precisely determine a user’s position.

When data is unavailable, or when it is desired to compute these products for subsequent publication and use, estimates of the values are derived from the available data.

It is the sophistication of the models available and applied that determines the ultimate performance of the software.

The software will be developed to allow for all applicable phenomena to be modelled, estimated, such that user’s desired constraints may be applied and parameters of interest extracted.

#### Initialisation of Parameters

Estimation parameters are initialised on the point of first use, automatically by the Kalman filter module. Their initial value may be selected to be user-defined, extracted from a model or input file, or established using a least-squares estimation.

#### Robust Kalman Filter

It is well known that the Kalman filter is the optimal technique for estimating parameters of interest from sets of noisy data - provided the model is appropriate.

In addition, statistical techniques may be used to detect defects in models or the parameters used to characterise the data, providing opportunities to intervene and make corrections to the model according to the nature of the anomaly.

By incorporating these features into a single generic module, the robustness that was previously available only under certain circumstances may now be automatically applied to all systems to which it is applied. These benefits extend automatically to all related modules (such as RTS), and often perform better than modules designed specifically to address isolated issues.

For further details about the software's robust Kalman filter see the (Ginan Science Manual)[].

#### RTS Smoothing

The intermediate outputs of a Kalman filter are of use for other algorithms such as RTS smoothers. All intermediate values required for such algorithms are to be recorded in a consistent manner, suitable for later processing.

For further details about the software's RTS smoothing algorithm see the Ginan Science Manual.

#### Integer Ambiguity Resolution

GNSS phase measurements allow for very precise measurements of biases but require extra processing steps to disambiguate between cycles. Techniques have been demonstrated that perform acceptably under certain conditions and measurement types, but require substantial bookkeeping and may not easily transfer to different measurement applications.

For further details about the software's ambiguity resolution algorithms see the Ginan Science Manual.

#### Product calculation

In order for estimated and predicted values to be of use to end-users, they must be prepared and distributed in an appropriate format.

Some parameters of interest are not directly estimated by the filter, but may be derived from estimates by secondary operations, which are performed in this section of the code.

In this section, data is written to files or pushed to NTRIP casters and other data sinks.

#### SSR message generation

State Space Representation (SSR) messages contain data on different GNSS error components, such as satellite clocks and orbits.
To generate an SSR message, error components are retrieved from a selection of several sources - such as the Kalman filter (estimated), precise product files, or even other input SSR streams.
Then, messages are formed according to the RTCM 3 standard.
Once ready, the SSR messages are then published to an NTRIP caster to be broadcast to multiple end-users.

The software can generate SSR messages for the following constellations:
* GPS
* GALILEO
* GLONASS
* BEIDOU
* QZSS
* SBS

For each constellation, the following GNSS error components can be generated:
* Satellite clocks
* High-rate satellite clocks
* Satellite orbits
* Code & phase biases
* User Range Accuracy (URA)

For further details on how SSR messages are used on the end-user side, see section `SSR Data Input`.


## Post-processing


#### Smoothing

The RTS Smoothing algorithm is capable of using intermediate states, covariances, and state transition matrices stored during the Kalman filter stage to calculate reverse smoothed estimates of parameters.

The intermediate data is stored in binary files with messages that contain tail blocks containing the length of the message. This allows for the file to be efficiently traversed in reverse; seeking to the beginning of each message as defined by the tail block.

For further details about the software's RTS Smoothing algorithm see the Ginan Science Manual.


#### Minimum Constraints

The minimum constraints algorithm is capable of aligning a network of stations to a reference system without introducing any bias to the positions of the stations.

A subset of stations positions are selected and weighted to create pseudo-observations to determine the optimal rigid transformation between the coordinates and the reference frame. The transformation takes the same algebraic form as a Kalman filter stage and is implemented as such in the software.

For further details about the software's minimum constraints algorithm see the Ginan Science Manual.


#### Unit Testing

The nature of GNSS processing means that well-defined unit tests are difficult to write from first-principles. The software however, is capable of comparing results between runs to determine if the results have changed unexpectedly.

Intermediate variables are tagged throughout the code, and auxiliary files specify which variables should be tested as they are obtained, and the expected values from previous runs.

## Logging and outputs

Details of processing are logged to trace files according to the processing mode in use.

Per-station trace files may be created with intermediate processing values and information, and a single network summary file is generated for the unified filter and combined processing.

In addition to trace files, the `pea` is capable of outputting most of the file-types that it can read, and includes:

* RINEX Clock files
* RINEX Navigation files
* RINEX Observation files
* RTCM Observation files/streams
* RTCM Correction files/streams
* SINEX files
* SINEX Bias files
* SINEX Troposphere files
* SP3 files
* YAML files
* GPX files - an XML format for interchange of GPS data (waypoints, routes, and tracks) used by Google Earth among other software. (https://www.topografix.com/GPX/1/1/)
* JSON files
* Ginan PPP_OUT files

### Station Trace files

The station trace files will contain outputs relevant and limited to a single station.
It will, for example, contain the observed-minus predicted estimation for individual signals observed by the residuals.

```
...
----------------------------------------------------
Measurement for  CODE_MEAS       G31    NNOR       20 L2W

 OBSERVED              +23129117.0340 -> 23129117.0340
 RANGE                 -23214116.5012 ->   -84999.4672
 REC_CLOCK                +78026.0567 ->    -6973.4105
 SAT_CLOCK                 +6944.3768 ->      -29.0337
 REC_ANTENNA_DELTA            +0.0504 ->      -28.9833
 REC_PCO                      +0.0347 ->      -28.9486
 SAT_PCO                      +0.7339 ->      -28.2147
 REC_PCV                      +0.0037 ->      -28.2110
 SAT_PCV                      -0.0022 ->      -28.2131
 TIDES_SOLID                  -0.0599 ->      -28.2731
 TIDES_OTL                    -0.0085 ->      -28.2815
 TIDES_POLE                   -0.0008 ->      -28.2823
 RELATIVITY1                  +5.4878 ->      -22.7946
 RELATIVITY2                  -0.0155 ->      -22.8100
 SAGNAC                      +22.5172 ->       -0.2928
 IONOSPHERIC_COMPONENT        +3.8763 ->        3.5835
 IONOSPHERIC_COMPONENT1       -0.0030 ->        3.5805
 IONOSPHERIC_COMPONENT2       +0.0000 ->        3.5805
 TROPOSPHERE                  -5.1548 ->       -1.5743
 EOP                          -0.0011 ->       -1.5754
 REC_CODE_BIAS                +0.0000 ->       -1.5754
 SAT_CODE_BIAS                -0.0000 ->       -1.5754
 NET_RESIDUAL                 -0.0000 ->       -1.5754
 ...
 ```
 
In this example the pre-fit residual is calculated for L2W code measurement of satellite G31 in station NNOR. 
The modelled/estimated value of various effects (geometric range, clock offsets, antenna characteristics, tide and relativistic effects, atmospheric delays, etc) are presented.
This allows to look for anomalous values on modelled/estimated parameters applied to the GNSS observation model.

### Network Trace files

The network trace files contain outputs relevant to the whole network. For example it will contain the state of parameters estimated by Ginan Processing
```
...
*    2019-07-18 00:05:30              PHASE_BIAS            G31  20        -0.2396864         88.58950070                   L2W
*    2019-07-18 00:05:30               SAT_CLOCK            G32   0       303.8257442         33.42697170                   
*    2019-07-18 00:05:30          SAT_CLOCK_RATE            G32   0         0.0045856          0.00333322                   
*    2019-07-18 00:05:30               CODE_BIAS            G32   1         0.0307764          0.01959992                   L1C
*    2019-07-18 00:05:30               CODE_BIAS            G32   3        -3.216e-17          1.0000e-16                   L1W
*    2019-07-18 00:05:30               CODE_BIAS            G32  20         3.101e-17          1.0000e-16                   L2W
*    2019-07-18 00:05:30               CODE_BIAS            G32  25        -0.7453901          0.14406117                   L5Q
*    2019-07-18 00:05:30              PHASE_BIAS            G32   1         0.6058104         90.18690487                   L1C
*    2019-07-18 00:05:30              PHASE_BIAS            G32   3         0.3273932         94.38893807                   L1W
*    2019-07-18 00:05:30              PHASE_BIAS            G32  20         0.9557970         90.18794887                   L2W
*    2019-07-18 00:05:30              PHASE_BIAS            G32  25        -0.0154065         93.53419260                   L5Q
*    2019-07-18 00:05:30                 REC_POS     AREG         0   1942816.6390803          0.11138497                   
*    2019-07-18 00:05:30                 REC_POS     AREG         1  -5804077.1997447          0.31250347                   
*    2019-07-18 00:05:30                 REC_POS     AREG         2  -1796884.3863768          0.07612933                   
*    2019-07-18 00:05:30               REC_CLOCK     AREG         0     -8133.8558767       3000.65388827                   
*    2019-07-18 00:05:30                    TROP     AREG         0         1.8534086          0.01375931                   
*    2019-07-18 00:05:30               TROP_GRAD     AREG         0        -0.0062538          0.00027328                   
*    2019-07-18 00:05:30               TROP_GRAD     AREG         1        -0.0022097          0.00072508                   
*    2019-07-18 00:05:30               CODE_BIAS     AREG   G--   1         0.0077985          0.01470148                   L1C
*    2019-07-18 00:05:30               CODE_BIAS     AREG   G--   3        -3.533e-10          3.0034e-07                   L1W
*    2019-07-18 00:05:30               CODE_BIAS     AREG   G--  20        -5.338e-10          3.0075e-07                   L2W
*    2019-07-18 00:05:30               CODE_BIAS     AREG   G--  25         0.8660053          0.12206356                   L5Q
*    2019-07-18 00:05:30              PHASE_BIAS     AREG   G--   1         0.4674732         93.48961365                   L1C
*    2019-07-18 00:05:30              PHASE_BIAS     AREG   G--   3         0.4936798         93.49109304                   L1W
*    2019-07-18 00:05:30              PHASE_BIAS     AREG   G--  20         0.7138144         93.49006419                   L2W
*    2019-07-18 00:05:30              PHASE_BIAS     AREG   G--  25         0.6556864         97.10528922                   L5Q
*    2019-07-18 00:05:30               IONO_STEC     AREG   G02   0       -36.2661645        121.59989594                   
*    2019-07-18 00:05:30               AMBIGUITY     AREG   G02   1        -9.2027480       4988.17927778                   L1C
*    2019-07-18 00:05:30               AMBIGUITY     AREG   G02   3        -6.5010596       5121.40868346                   L1W
*    2019-07-18 00:05:30               AMBIGUITY     AREG   G02  20       -11.3644227       3043.48150056                   L2W
*    2019-07-18 00:05:30               IONO_STEC     AREG   G12   0       -21.3198438        121.60048193                   
*    2019-07-18 00:05:30               AMBIGUITY     AREG   G12   1        19.4419675       4988.28296914                   L1C
*    2019-07-18 00:05:30               AMBIGUITY     AREG   G12   3        22.5097301       5121.65630355                   L1W
... 
```

The example above contains (from left to right) the epoch (GPS time), the state name, the station and satellite/constellation if relevant, an index number, the value of the estimated state and its variance.
On the top of the section are the estimates for satellite clock offset, code bias and phase bias for GPS satellites. At the bottom the estimated station parameters: positions, clock offsets, troposphere, hardware biases, ionosphere delays and carrier phase ambiguities for the AREG station. 

### Ginan PPP_OUT files

The `ppp_out` outputs Ginans proprietary position output format.
The format of Ginans position output is as follows:
```
...
-FILE/RAW_CONFIG ex201.yaml
2019-07-18 00:00:00.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052052.6747  4212837.0628 -2545104.0993  -0.0423 -1.0818 -0.5087   0.7804 -0.7672 -0.4830  
2019-07-18 00:00:30.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052052.5220  4212836.6464 -2545104.0855  -0.1950 -0.6655 -0.5225   0.6018 -0.6168 -0.1057  
2019-07-18 00:01:00.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052052.4875  4212836.3258 -2545104.0685  -0.2294 -0.3449 -0.5395   0.4045 -0.5301  0.1346  
2019-07-18 00:01:30.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052052.7767  4212836.2938 -2545104.3220   0.0598 -0.3128 -0.2860   0.1738 -0.3691 -0.1296  
2019-07-18 00:02:00.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052053.1333  4212836.3964 -2545104.5919   0.4164 -0.4155 -0.0160  -0.0121 -0.2508 -0.5322  
2019-07-18 00:02:30.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052053.3348  4212836.4331 -2545104.6877   0.6179 -0.4522  0.0798  -0.1318 -0.2298 -0.7228  
2019-07-18 00:03:00.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052053.3753  4212836.4132 -2545104.7395   0.6583 -0.4323  0.1316  -0.1748 -0.1878 -0.7562  
2019-07-18 00:03:30.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052053.1616  4212836.3498 -2545104.7107   0.4446 -0.3689  0.1027  -0.0647 -0.1364 -0.5670  
2019-07-18 00:04:00.00 ALIC -4052052.7169  4212835.9809 -2545104.6080  -4052053.2056  4212836.4648 -2545104.7807   0.4886 -0.4839  0.1727  -0.0167 -0.1178 -0.6990  
...
```

Each line on the bulk of the file contains, from left to right, the epoch time (in GPS time), the station/receiver name, the apriori position in ECEF, the estimated position in ECEF, the difference between apriori and estimated possition in ECEF and the difference in ENU.
