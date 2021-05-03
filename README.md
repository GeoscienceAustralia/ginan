# Analysis Centre Software - POD

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

## POD

The `ACS` Version 0.0.1 beta release supports:

1. The `POD` 

## Directory Structure

    pod/
    ├── LICENSE.md
    ├── INSTALL.md
    ├── README.md
    ├── src/
    ├── bin/  (created)
    ├── lib/  (created)
    ├── config/
    ├── tables/
    ├── scripts/




