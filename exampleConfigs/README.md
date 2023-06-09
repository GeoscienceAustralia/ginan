The folder contains example Ginan configuration files that showcase the various 
capabilities of the Ginan software.
The configuration files use the industry standard Yet Another Markup Language 
(yaml)format.

Ginan is capable of accepting a single yaml file or multiple yaml files to 
achieve the configuration and processing outcome required.

Currently included examples configurations with this release include:

1) ex201.yaml
Post Processed Static Receiver positioning usng dual frequency GPS and GAL 
observations, using IGS REPRO3 final orbits clocks and biases. 

2) ex202.yaml
Real-Time Static Receiver positioning usng dual frequency GPS and GAL 
observations, using the BKG SSRA00BKG0 correction stream

3) ex203.yaml
Post processed Real-Time Kinemeatic Receiver (RTK) positioning using tri 
frequencyGPS, GAL and BDS recorded RTCM3 observation and correction streams. 
The recorded correction stream is from SSRA03IGS0.

4) ex204.yaml 
Post processed Network Clock and Bias estimation using dual frequency 
L1C/L1W/L2W GPS observations, estimating L1W/L2W clocks and L1C bias.

5) ex205.yaml
Post processed Network Orbits and Clock estimation using dual frequency
L1C/L2W ionosphere free linearly combined observations.

6) ex02_fit_sp3_pseudoobs.yaml 
Orbit fitting from Pseudo Observations using 3-day fit the IGS REPRO3 
final ECEF (ITRF) orbits in SP3 format.

7) record_streams.yaml
Record RTCM Streams to Files 
