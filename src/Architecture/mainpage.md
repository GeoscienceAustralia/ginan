## Automatically generated GINAN code documentation

> For higher-level details of the Ginan project, visit the [main documentation](../index.html) pages.

## Software flow overview

### Architectural documentation

The Ginan__() pseudo-function outlines the structure and flow of the software from a high level.

The color coded block diagram call-graphs may be used to step through the high-level flow before entering the lower-level logical functions.
A Legend__() for the diagrams is found here.

### Ginan program entry point

The ginan() function contains the main processing loop, which initialises required objects and synchronises input operations.

### Per-epoch processing functions

The functions mainOncePerEpoch(), mainOncePerEpochPerStation(), and mainOncePerEpochPerSatellite() perform operations each epoch on enabled objects.

### Main processing modes

The preprocessor(), spp(), and ppp() functions perform the majority of the GNSS-specific processing and filtering, with reveiverPPP() implementing most of the gnss models.

### Key data structures used within the code include:

#### Observations and their signals

- GObs
- Sig

#### Receivers and Satellites

- SatSys
- Receiver

#### Global configuration object

- ACSConfig

#### Kalman filter structure

- KFState
- KFKey
- KFMeas
- KFMeasEntry

#### Satellite emphemeris and status

- Navigation
- SatNav
- SatStat
- SigStat



