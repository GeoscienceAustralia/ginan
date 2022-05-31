# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),

## [Unreleased]

# [1.4.1] 2022-05-27
Updates to documentation that were missed in the release

Specify versions of Eigen, Boost, Mongo-cxx

Remove spurious CMakeLists.txt in the root directory

# [1.4] - 2022-05-24
Improvements to EOP/ERP handling when the integration arc is longer than 1 day - as a result the POD is now twice as fast as before

PEA no longer estimates corrections to ERP parameters but outputs absolute ERP values

Refactoring and simplification of hardware and system Bias handling code

Kalman Filter statistical tests now fully implemented

YAML config error warnings: PEA now attempts to detect typos in the input config file

Mongo C++ library dependencies now required at build time

Bugfixes in RTCM and SSR code

Several CMake build issues resolved

Ambiguity resolution (AR) algorithm improved

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
