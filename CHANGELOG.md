# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

# [3.1] 2024-09-02

### Added

Boxwing model for the albedo

Sisnet (SouthPan) message support

SLR processing capability

PBO Position (.pos) format file output support

Apple silicon (M-chip) support

VMF3 file download python script (get_vmf3.py)

POS file visualisation python script (plot_pos.py)

### Changed

EDA improvements

Improved documentation

Use case examples updated

Frequency dependent GLONASS receiver code bias estimation enabled

Improved missing/bad data handling

Bias rates from .BIA/BSX files parsed and used

Measurment and State error handling sigma_limit thresholds separated

Config file reorganisation (rec_reference_system: moved to receiver_options:)

Clock code handling modified

### Fixed

Many bug fixes

### Deprecated

### Removed

### Security


# [3.0] 2024-02-05

### Added
IERS 2010 standard tide models implemeted and validated including:
- Solid Earth (SE) Tide,
- SE Pole Tide,
- Ocean Tide Loading (OTL)
- Ocean Pole Tide
- Atmosphere Tide Loading (ATL)

Compact SSR standard correction output encoding/decoding implemented

### Changed
Reorganisation of the YAML configuration file to make it more logical

Improved Satellite attitude models implemented for (GPS/GAL/GLO/BDS/QZS)

Improved Receiver (antenna) attitude model implemented

New standard IERS linear mean pole model implemented

Use case examples updated

EDA improvements

Improved documentation

### Fixed

### Deprecated

### Removed

### Security

# [2.1] 2023-07-04

### Added
Reduced dynamic (Pseudo Stochastic) pulse estimation implemented

Higher(2nd/3rd) order ionosphere modelling

Demonstration of LEO kinematic, reduced dynamic and dynamic orbit estimation

Included pseudo-observation/contrainst to eliminate rank deficiencies in observation models

### Changed
Complete GNSS constelation attitude modelling implemented (GPS/GAL/GLO/BDS/QZS)

Use case examples updated

Python gnssanalysis tools updates

Improved documentation

Yaml config file updates

### Fixed

### Deprecated
Ginan version 1 `user` and `network` modes

### Removed

### Security


# [2.0.1] 2023-06-09
### Added
### Changed
Unified User and Network operation modes (One Observation Model & Filter)

More GNSS constellations – Full Multi-Constellation capability (Ex SBAS)

Better internal frequency indexing (complete Multi-Frequency capability)

UnDifferenced / UnCombined (UDUC) processing (v1 was Combined IF only)

CPP integrated and coupled Precise Orbit Determination (POD) capability

More robust data handling in filter cycle slip and outlier detection and removal

Complete RTCM3 phase 1 and Phase 2 message decoding and encoding

SLR data handling fully implemented

Model & Performance improvements

### Fixed
### Deprecated
### Removed
### Security

# [1.5.4] 2023-05-07
### Added
New dependency - Ginan relies on the python gnssanalysis tools (installable by pip) for running most scripts, although it is not required to run the main products themselves.

### Changed
### Fixed
POD no longer stops if sat is UNHEALTHY set by PEA at end of satellite loop.

### Deprecated
### Removed
### Security

# [1.5.3] 2023-04-04
### Added
### Changed
### Fixed
POD crash using initial conditions file when the first satellite is excluded
POD to use IC ERP data when not enough supplied by standard ERP file.

### Deprecated
### Removed
### Security

# [1.5.2] 2022-12-28
### Added
### Changed
No longer need conda environment to run python scripts
Antenna PCO values now constellation specific

### Fixed
POD does not output clocks in the predicted part of the generated SP3 file

### Deprecated
### Removed
### Security

# [1.5.1] 2022-08-22
### Added
Beidou block ids and SRPs (type 2 and type 3)

### Changed
Allowance for occasional missing pseudo observation data points (rows with all zero)

Checks for unhealthy satellites at epoch 1

Use of different sp3 file for comparison - does not need to contain exactly the same PRNs as the pseudo obs file

### Fixed
Reading of pod sinex metadata file

### Deprecated
### Removed
### Security

# [1.5] 2022-07-26
### Added
Read/write RINEX4 compatability

New pod mode: pod_data_int which uses pod_data section for integrating given IC. New example ex00_pod_test_g01.yaml

Option to output pod sp3 as icrf. sp3_itrf: false (default value is true) in pod_options.

### Changed
PEA YAML config file refactored

Ginan EDA Enhancements

Minimum Constraints algorithm and config options enhanced

Brdc2sp3 application robustness improved

POD ERP/EOP interpolation algorithm improved

Streamlined PEA/POD .erp information handling implemented

RTCM3 SSR orbit and clock IODE rollover interpolation improved

PEA PDE module now cleans and edits all observations, not only sampling interval observations

Improved SPP algorithm

### Fixed
Numerous bugs

### Deprecated
PEA v1.4 YAML config files

### Removed
### Security

# [1.4.2] 2022-07-18
### Fixed
Fixed bug in Glonass (GLO) satellite clocks

# [1.4.1] 2022-05-27
### Added
Updates to documentation that were missed in the release

### Changed
Specify versions of Eigen, Boost, Mongo-cxx

### Fixed
### Deprecated
### Removed
Remove spurious CMakeLists.txt in the root directory

### Security

# [1.4] - 2022-05-24
### Added
YAML config error warnings: PEA now attempts to detect typos in the input config file

### Changed
Improvements to EOP/ERP handling when the integration arc is longer than 1 day - as a result the POD is now twice as fast as before

PEA no longer estimates corrections to ERP parameters but outputs absolute ERP values

Refactoring and simplification of hardware and system Bias handling code

Kalman Filter statistical tests now fully implemented

Mongo C++ library dependencies now required at build time

Ambiguity resolution (AR) algorithm improved

### Fixed
Bugfixes in RTCM and SSR code

Several CMake build issues resolved

### Removed
### Deprecated
### Security

## [1.3] - 2022-03-18
### Added
New fix and hold ambiguity resolution algorithm implemented in the PEA

Earth Orientation Parameter (EOP) rate parameter estimation implemented in PEA

Output of IGS standard Earth Rotation Parameter (.ERP) file format from PEA and POD implemented

Export of RINEX v3 observation and navigation files from RTCM stream capability added to the PEA

Real-time raw RTCM stream binary file recording and playback capability implemented in the PEA

Machine readable JSON format output of RTCM correction stream messages implemented in PEA

Troposphere (.TRO) file output implemented in the PEA

RTCM3 stream monitoring metrics logging capability added to PEA

Seamless integration of updated EOP estimates from PEA into POD reintegrated orbits

Processing of L1/L5 ionosphere free combination data enabled for L1/L5 only receivers

Generalised First Order Gauss Markov (FOGM) Kalman Filter process noise implemented in PEA Kalman Filter

Calculation of Kalman Filter Chi Square and W-test statistics implemented

User definable orbit Initial Condition (IC) time implemented in the POD

Export of RTCM stream navigation messages to .SP3 file format implemented

RINEX broadcast ephemeris to SP3 file standalone utility (brdc2sp3) added

Numerous new Ginan python utilities added (difftrace, diffsnx, duffutil, merge_sp3, log2snx, etc)

Ginan v2 uncombined/undifferenced algorithm code framework implemented

Output listing of all available PEA configuration options implemented with pea -Y option

### Changed
PEA Rauch-Tung-Striebel (RTS) smoothing filter algorithm performance improved

MongoDash visualisation tool renamed Ginan Exploratory Data Analysis tool (GinanEDA), numerous performance and analytical features added

PEA output to MongoDB performance improvement and additional outputs implemented

PEA TRACE and solution summary (.SUM) file format updates. All output is now time tagged with improved formating for easier reading and searching

PEA Kalman Filter performance and stability improvements

Streamlined and improved handling of RTCM input and output data and correction streams

Unification of PEA user mode and network mode filters in the codebase

Updated all Ginan PEA and POD user use-case examples to reflect changes since Ginanv-1.2-Aplha release

Updated all Ginan PEA and POD user use-case examples filenames to better reflect their purpose

Updated Ginan use case example downloader, download_examples.py script updated to allow individual data, products and solutions tar balls to be downloaded separately

CMake.txt file simplification and build stability improvements

New Ginan download, install, and use-case videos completed

### Deprecated
### Removed
### Fixed
Missing .SP3 file entries and “0 position” satellite entries bug fixed in the POD

Improved outlier and cycle slip detection and data downweighing in the PEA Kalman Filter

IONEX file output format bug fixes

Numerous new Ginan python utilities added (difftrace, diffsnx, duffutil, merge_sp3, log2snx, etc)

Default PEA Kalman Filter inverter changed to LDLT

Miscellaneous bug fixes and performance improvements

### Security


## [1.2] - 2021-10-07
### Added
### Changed
small change in pea example 3 config
### Deprecated
### Removed
### Fixed
### Security

## [1.1.1] - 2021-09-29
### Added
this file (CHANGELOG.md)
### Changed
### Deprecated
### Removed
### Fixed
- Eigen v3.4 compilation problem fix
### Security

## [1.1.0] - 2021-08-06
### Added
Ambiguity Resolution
Ntrip casters/Rtcm streams
BiasSinex changes
significant additional documentation in manual
### Changed
Galileo DTOE 600.0 => 1800.0
Some Ionosphere changes
apply coding standards to some units
### Deprecated
### Removed
### Fixed
### Security

## [1.0.0] - 2021-07-13
### First Release
