The folder contains example Ginan configuration files that showcase the various 
capabilities of the Ginan software.
The configuration files use the industry standard Yet Another Markup Language 
(yaml) format.

Ginan is capable of accepting a single yaml file or multiple yaml files to 
achieve the configuration and processing outcome required.

Currently included examples configurations with this release include:

1) ppp_example.yaml
Post Processed Static Receiver positioning using dual frequency GPS and GAL 
observations, using IGS REPRO3 final orbits clocks and biases. 

1) rt_ppp_example.yaml
Real-Time Static Receiver positioning using dual frequency GPS and GAL 
observations, using the BKG SSRA00BKG0 correction stream

1) pod_example.yaml
Post processed Network Orbits and Clock estimation using dual frequency L1C/L2W 
ionosphere free linearly combined observations.

1) fit_sp3_pseudoobs.yaml 
Orbit fitting from Pseudo Observations using 3-day fit the IGS REPRO3 final 
ECEF (ITRF) orbits in SP3 format.

1) record_streams.yaml
Record RTCM Streams to Files.

1) compare_orbits.yaml
Orbit comparison between two SP3 files.

1) brdc2sp3.yaml
Convert broadcast ephemeris RINEX files to SP3.