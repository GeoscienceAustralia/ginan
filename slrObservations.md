

# SLR Observation Modelling

Satellite Laser Ranging (SLR) is a geodetic technique used for measuring the distance between ground-based stations and Earth-orbiting satellites. This technology involves the transmission of laser beams from a ground station to a satellite equipped with retroreflectors, which then reflect the laser pulses back to Earth. By measuring the round-trip travel time of the laser pulses, the distance between the ground station and the satellite can be accurately determined.

SLR plays a significant role contributing to geodetic products, including the definition and scale of the International Terrestrial Reference Frame (ITRF), monitoring Earth rotation and polar motion to provide the relationship with the International Celestial Reference Frame (CRF), and modelling the temporal and spatial variation of the Earth's gravity field. SLR observations also provide an independent data source to verify the accuracy of GNSS-formed orbits.

The Ginan toolkit has the capability to process SLR observations to produce geodetic products - either alone or alongside GNSS observations. Unlike GNSS observations, SLR utilises a simpler two-way time-of-flight observation, and several GNSS-specific components (e.g. satellite clock biases, ambiguities) do not need to be considered.

In Ginan, SLR measurements are modelled as normally distributed random variables with mean:

\begin{equation} 
\label{eq:slr_mea}
E(S_r^s) 
= 2(\rho_{r}^s 
+ \tau_r^s
+ \kappa)
+ d_r
+ c_{light}(dt_r) 
\end{equation}

with a constant variance. In this equation:

| Variable		| Description																	|
| -				| -																				|
| $S_r^s$		| represents the SLR measurement (m) between satellite $s$ and receiver $r$		|
| $E()$			| expected (mean) value															|
| $\rho_{r}^s$	| geometric distance between satellite $s$ and receiver $r$ (m)					|
| $\tau_r^s $	| slant troposphere delay between satellite $s$ and receiver $r$ (m)			|
| $\kappa $		| relativistic (Shapiro) effect (m)												|
| $d_{r,c}^q$	| receiver range bias (m)														|
| $c_{light}$	| speed of light (m/s)															|
| $dt_r^q$		| receiver time bias (s)														|

Note that SLR observations are 2-way measurements, hence the leading coefficient in the observation equation above. Otherwise, most of the components - including satellite and receiver positions - are modelled and estimated in the same way as for GNSS observations.

## Troposphere modelling (SLR)
SLR tropospheric delays are composed of both hydrostatic and wet components. However, water vapor only contributes a small amount to atmospheric refraction at visible wavelengths, thus only a single mapping function is used for SLR modelling:
\begin{equation}
 \tau_r^s = m(\theta_{el,r}^s) \tau_{ZTD,r} 
\end{equation}

The total tropospheric delay $\tau_{ZTD,r}$ and (total) mapping function $m(\theta_{el,r}^s)$ are assumed to be deterministic, and estimate both hydrostatic and wet components. They are estimated using the Mendes and Pavlis (2004) and Mendes et al. (2002) models respectively.


## Hardware biases (SLR)
SLR stations have hardware biases that affect both range and time measurements. Range biases affect the measured range of the total observation, whereas time biases affect the time-stamp associated with each measurement. Ginan estimates these biases within the Kalman filter, and applies a-priori values when known.

(Note that time biases do not affect the range measurement on a 1:1 basis as with GNSS observations, as SLR uses a two-way observation which cancels out the direct effect of this component. Rather, time biases affect the assumed time of reflection off of the satellite, and thus affect the calculated position of satellite at the time of observation.)

