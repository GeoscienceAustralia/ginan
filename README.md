# Analysis Centre Software - Ginarn

## Overview

The *Analysis Centre Software (ACS)* is a processing package being developed to processes GNSS observations for geodetic 
applications.  

We currently support the processing of:

* the American Global Positioning System (`GPS`)
* the Russian GLONASS system ('GLONASS')
* The European Gallileo system ('Gallileo')
* the Chinese Navigation Satellite System ('Beidou`); and
* The Japanese QZSS develop system ('QZSS')

We are actively developing the ACS to have the following capabilities and features:

* Precise Orbit & Clock determination of GNSS satellites (GNSS POD).
* Precise Point Positioning (PPP) of GNSS stations in network and individual mode.
* Real-Time corrections for PPP users.
* Analyse full, single and multi-frequency, multi-GNSS data.
* Delivering atmospheric products such as ionosphere and troposphere models.
* Servicing a wide range of users and receiver types.
* Delivering outputs usable and accessible by non-experts.
* Providing both a real-time and off-line processing capability.
* Delivering both position and integrity information.
* Routinely produce IGS final, rapid, ultra-rapid and real-time (RT) products. 

The software is broken into two main components:

* the Network Parameter Estimation Algorithm (`PEA-N`); and 
* the Precise Orbit Determination (`POD`).

## Software

The `ACS` Version 0.0.1 beta release supports:

1. The `POD` 

### Directory Structure

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

2. The "PEA"

### Directory Structure

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

3. Instructions to use Ginarn:

First clone this repo:
```git clone https://github.com/GeoscienceAustralia/ginarn```

Second initialise the submodules for the PEA and POD
```git submodule update --init --recursive```

## Examples

We have a number of examples on how to run the different components of the software, a quick overview is given in the README.md 
For some example see examples on how to use the PEA and POD.

## Bugs / Issues

If you find a bug or would like to request a feature please lodge it BUGS

## Developer Resources

See this page for some [cheat sheets] and tips for those developing the PEA.

For notes on the [Algorithms](Link URL) and utilities provided by this repository.

## Roadmap

See this page for what we are planning to be working on next.

