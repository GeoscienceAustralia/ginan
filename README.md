# Analysis Centre Software - Ginan

## Overview

Ginan is a software toolkit, available as source code, that features two software applications, the POD and the PEA. POD and PEA working together will allow you to estimate your own satellite orbits from a global tracking network.
The *POD (precise orbit determination)* contains all of the source code needed to determine a GNSS satellite’s orbit. You can establish the initial conditions of an orbit from a broadcast ephemeris file, or from an IGS SP3 file. It can then estimate it’s own orbital trajectory based upon the models specified in configuration files, and output an SP3 file, or provide a partial files which can then be updated from a tracking network.
The *PEA (parameter estimation algorithm)* takes raw observations in RINEX format or in RTCM format, to estimate the parameters you are interested in. You can run it a single user mode, taking in orbit and clocks supplied by real-time streams to SP3 files obtained from the IGS to estimate your own position in static and kinematic mode. You can also run the PEA in a network mode, and take in a global network of observations to determine your own orbits and satellite clocks to support your application. In summary:
* The Network Platform accepts carrier, code and ranging observation data from many stations to enable the computation of high accuracy orbits, clocks, satellite phase biases and atmospheric models as well as station coordinates.
* The User Platform accepts high accuracy orbits, clocks, satellite phase biases and atmospheric models in addition to carrier and code observation data to enable single-receiver users to compute high accuracy positions.
The Ginan *Combination Platform* accepts parameter estimates and their variance-covariance information to enable the combination of geodetic solutions including combining station coordinates into multi-day solutions, station coordinates into multi-year solutions with the estimation of station velocities, and time series analysis.
The software is aimed at supporting Australia’s implementation of a national positioning infrastructure that supports the objective of instantaneous GNSS positoning anywhere, anytime, with the highest possible accuracy and the highest possible integrity.

## Software

### The POD 

#### Directory Structure

    precise_orbit_determination/
    ├── LICENSE.md
    ├── INSTALL.md
    ├── README.md
    ├── src/
    ├── bin/  (created)
    ├── lib/  (created)
    ├── config/
    ├── tables/
    ├── scripts/

### The "PEA"

#### Directory Structure

    parameter_estimation_algorithm/
    ├── LICENSE.md
    ├── INSTALL.md
    ├── README.md
    ├── src/
    ├── bin/  (created)
    ├── lib/  (created)
    ├── config/
    ├── tables/
    ├── scripts/

## Instructions to use Ginarn:

First clone this repo:
```git clone https://github.com/GeoscienceAustralia/ginarn```

Second initialise the submodules for the PEA and POD
```git submodule update --init --recursive```

## Examples

We have a number of examples on how to run the different components of the software, a quick overview is given in the README.md 
For some example see examples on how to use the PEA and POD.

## Bugs / Issues

If you find a bug or would like to request a feature please lodge it BUGS
