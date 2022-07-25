
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
* GPX files
* JSON files


