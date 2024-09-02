
# Flex Events

Certain satellites in the GPS constellation have the ability to change the output power on the signals they transmit toward Earth. Effectively this is achieved by re-distributing the power allocated to each signal component as seen in \cite{steigenberger_flex_2018}. This means that individual signal components can therefore transmit above previously stated maximum \citep[sec. 6.3.1]{IS-GPS-200G_2012}. 

This `flexible power` or `flex power` capability is used as an anti-jamming technique. When flex power is activated (or de-activated) by the Control Segment of the GPS system, it is sometimes referred to as a "flex event". When a series of flex events are associated with a given set of GPS satellites, alter the power spectral distribution in similar ways and/or are targeted over the same geographical region, this is categorised as a flex power mode \citep{steigenberger_flex_2018}. In \cite{steigenberger_flex_2018}, three flex power modes are discussed. This includes a global activation for all healthy GPS Block IIR-M and IIF satellite for a period of 4 days (Mode II) to a geographically localised (regional) activation for 10 out of 12 Block IIF satellites on a continuous basis over a point centred in the Middle East (Mode I).

Since the launch of the first GPS block IIR-M satellite in 2005 (https://www.e-education.psu.edu/geog862/node/1773), every new GPS satellite has had programmable power output capabilities. This therefore includes the newer block satellites IIF and IIIA as well. Other constellations do not have this programmable variation, i.e. the power output is static.

GLONASS has greater variation in the total power output within a generation, i.e. different satellites in a given generation will vary by 10’s of watts. The power output of L1 and L2 frequencies will also differ with gradation of low/medium/high. This is covered in \citep{steigenberger_flex_2018}, but a good summary can be found in \cite{steigenberger_gps_2019}.

Galileo also has some variation in signal outputs across the In-Orbit Validation (IOV) satellites although the Full Operational Capability (FOC) satellites are designed to be constant \citep{bar-sever_why_2019}. According to the  \cite{steigenberger_gnss_2018} the power range for IOV sats is 95 - 134 W, and FOC sats is 254 - 273 W.

## Ginan Code Example
In order to detect flex events, we require both RINEX3 observation files for a given station and time period, and the associated sp3 orbital files which detail the positions of the satellites. The Ginan code base includes Python scripts to automatically download and process files to find these flex events. 

For example, running the following command:

    python3 find_flex_events.py HOB200AUS 2021-02-15 2021-02-18 S2W /home/user/test/test_data/ -c_dir /home/user/test/test_csv/ -p -p_dir /home/user/test/test_plts/ -p_span 2400

will download RINEX3 and sp3 files for the `HOB200AUS` station between the dates `2021-02-15` and `2021-02-18` into the directory `/home/user/test/test_data/`, find any flex events (`Start` and `End`) for all GPS satellites, output a list of these events to a `csv` file in `/home/user/test/testcsv/` and output the results as a series of `.png` plots (one for each event). 

Apart from the station, dates and download directory provided in the command, all others are optional;

* `-c_dir` specifies the directory to save the \texttt{csv} file;
* `-p` specifies that plots are to be produced;
* `-p_dir` specifies the directory to save the \texttt{png} plot files;
* `-p_span` specifies the time span for the plots (in seconds).