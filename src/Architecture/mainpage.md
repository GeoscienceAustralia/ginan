## Automatically generated GINAN code documentation

> For higher-level details of the Ginan project, visit the [main documentation](../index.html) pages.

## Sofware flow overview

### Ginan program entry point

The ginan() function contains the main processing loop, which initialises required objects and synchronises input operations.

### Per-epoch processing functions

The functions mainOncePerEpoch(), mainOncePerEpochPerStation(), and mainOncePerEpochPerSatellite() perform operations each epoch on enabled objects.

### Main processing modes

The preprocessor(), SPP(), and PPP() functions perform the majority of the GNSS-specific processing and filtering, with stationPPP() implementing most of the gnss models.

### Key data structures used within the code include:

#### Observations and their signals

- GObs
- Sig

#### Receivers and Satellites

- SatSys
- Station

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



