 

# Observation Modelling

The Ginan toolkit is based on the concept of Precise Point Positioning (PPP).
PPP is a high accuracy positioning method that seeks to correct errors in GNSS positioning and thus improve accuracy.

Unlike differential GNSS techniques, which seek to measure GNSS errors using a nearby reference station, PPP is based in the robust modelling and estimation of systematic errors in the GNSS signals.

In PPP, the GNSS measurements are modelled as normally distributed random variables with mean:

\begin{alignat}{2} 
\label{eq:code_UC_mea}
E(P_{r,c}^s) 
&= \rho_{r}^s 
+ c_{light}(dt_{r}^q - dt^s) 
+ \tau_r^s
+ I^s_r 
+ d_{r,c}^q
+ d_{c}^s
\\
\label{eq:phase_UC_mea}
E(L_{r,c}^s) 
&= \rho_{r}^s 
+ c_{light}(dt_{r}^q - dt^s) 
+ \tau_r^s 
- I^s_r
+ b_{r,c}^q 
- b_{c}^s
+ \lambda_{f} z_{r,c}^s  
+ \phi^s_{r,f}
\end{alignat}

and a constant or elevation dependent variance.

\begin{equation} \label{eq:code_UC_var}
\sigma(P_{r,c}^s) = \frac{\sigma_0}{sin^2(\theta_{el})}
\end{equation}

In these equations:

| Variable		| Description																									|
| -				| -																												|
| $P_{r,f}^s$	| represents the pseudorange measurements (m) between satellite $s$ and receiver $r$ for signal code $c$. 	| 
| $L_{r,f}^s$	| represents the carrier phase measurements (m) 																|
| $E()$			| notates the expectation and  $ \sigma() $ the variance. 														|
| $\rho_{r}^s$	| the geometric distance (m) 																					|
| $c_{light}$	| speed of light (m/s) 																							|
| $dt_r^q$		| receiver clock offset (s) for constellation $q$                                                                                 	|
| $dt^s$		| satellite clock offset (s)                                                                                  	|
| $\tau_r^s $	| slant troposphere delay between satellite $s$ and receiver $r$ (m)                                          	|
| $I^s_r$		| slant ionosphere delay (m)															|
| $d_{r,c}^q$	| receiver hardware bias for pseudoranges (m)																	|
| $d_{c}^s$		| satellite hardware bias for pseudoranges (m)																	|
| $b_{r,c}^q$	| receiver ionosphere-free phase bias (m)																		|
| $b_{c}^s$ 	| satellite ionosphere-free phase bias (m)																		|
| $\lambda_{f}$ | the signal wavelength for carrier frequency $f$(m)															|
| $z_{r,c}^S$	| carrier phase ambiguity (cycle)																				|
| $\phi^s_{r,f}$| Deterministic corrections (PCO,PCV,phase windup, etc.)														|

High accuracy GNSS position estimation requires precise estimation of the geometric distance  $\rho_{r}^s$. The effect of other parameters in the observation equations need to be eliminated either by modelling/estimating or, in the case of ionosphere delays, by using linear combinations.
 
For PPP processing, the geometric distance  $\rho_{r}^s$ is linearized around a-priori values of satellite and receiver positions.
\begin{equation} \label{eq:rho_mean}
\rho_{r}^{s} = \sqrt{X^{s-} - X_r^-}+\Delta X^{s} - \Delta X_r
\end{equation}
where $X^{s-}$ is the a-priory satellite position with respect to the earth centre and $X_r^-$ the receiver/station position.
The satellite position can be assumed known, and read from external sources, or estimated from the GNSS measurements. When estimated by Ginan, the a-priori satellite position is defined as a function of initial satellite position, initial satellite velocity and up to 15 solar radiation pressure parameters as explained in [Orbit Modelling](#orbit-modelling) . Then the each satellite position component is linearized with respect to the orbit parameters.
\begin{equation} \label{eq:sat_pos_linear}
\Delta X^{s} = - \dot {e_{rec}} {\sum{ \frac{\partial X^{s} } {\partial OP_{i} } \Delta OP_{i} }}
\end{equation}
where $e_{rec}$ is the satellite-to-receiver vector, $\frac{\partial X^{s} } {\partial OP_{i}}$ is the partial derivative of the satellite position with respect to the orbital parameter (initial condition or SRP) $OP_{i}$, and  $\Delta OP_{i}$ the difference between the estimated  and the a-priori value of $OP_{i}$

The station/receiver position can be assumed known (and read from a SINEX file), estimated as a constant (in case of a static receiver) or estimated as random walk (in case of a moving receiver) variable. When estimated, a standard precision position (SPP) is used the a-priori receiver position. 
\begin{equation} \label{eq:rec_pos_linear}
\Delta X_r =  \dot {e_{rec}} { \Delta X_r }
\end{equation} 
$\Delta X_r$ is the difference between the estimated and the a-priori value of $X_r$. The SPP is calculated each epoch by applying iterative least squares on pseudorange measurements (SPP is initialised at the centre of earth and updated for each of up to a 10 iterations).

Satellite clock offsets can be assumed known, and obtained from external sources, or estimated as a random walk variable. the receiver clock is modelled as a random walk variable.

## Troposphere modelling
The tropospheric delays are separated into two components: the hydrostatic delay (dependent on temperature and pressure), and the wet delay (dependent also on humidity). Each component can be expressed as the product of a zenith delay and an elevation based mapping. 
\begin{equation}
 \tau_r^s = m_{H}(\theta_{el,r}^s) \tau_{ZHD,r} +  m_{W}(\theta_{el,r}^s) \tau_{ZWD,r} 
\end{equation}
In the case of Ginan, the hydrostatic components are assumed to be deterministic, while the zenith wet delay can be set as deterministic or estimated as part of the PPP solution process. The nominal values of Zenith hydrostatic delays $\tau_{ZHD,r}$ and hydrostatic mapping function $ m_{H}(\theta_{el,r}^s)$ and the wet mapping function $m_{W}(\theta_{el,r}^s)$ are estimated using either the VMF3 (daily measured) or GPT2/GMF (empirical) models.

When estimated, the tropospheric wet delay is modelled by one variable $\tau_{0,r}$ or three variables
\begin{equation}
 \tau_{ZWD,r} =  \tau_{ZWD,r} =  \tau_{0,r}+cot(\theta_{el,r}^s) \Big(cos(\theta_{az,r}^s) grad_{ns,r} + sin(\theta_{az,r}^s) grad_{ew,r}\Big)
\end{equation} 
each variable is modelled as a random walk of a First order Gauss-Markov process.

## Ionosphere modelling
The raw ionosphere delay seem by a GNSS signal is modelled as the sum of up to three components, for pseudoranges:
 \begin{equation}
 I_{r}^{s'} = \frac{40.3 10^{16}}{f^{2}} STEC_r^s + \frac{7527 * 10^{16} \lambda_f (R_B \cdot e_r^s)}{f^{2}}STEC_r^s + \frac{2437.12557 * 10^{16}}{f^{4}} I3_{r}^{s}(STEC_r^{s})
\end{equation}
where $R_B$ is the earths geomagnetic field at the signal ionosphere piercing point, and $e_r^s$ is the normilized satellite-receiver vector. For carrier phase:
 \begin{equation}
 I_{r}^{s'} = -\frac{40.3 10^{16}}{f^{2}} STEC_r^s - \frac{3763.5 * 10^{16} \lambda_f (R_B \cdot e_r^s)}{f^{2}}STEC_r^s - \frac{812.37519 * 10^{16}}{f^{4}} I3_{r}^{s}(STEC_r^{s})
\end{equation}

The way each of the three components is hadled can be sert separately:
* Set to zero
* Estimate it as a deterministic value from an external $STEC_r^s$ source
* Estimate $STEC_r^s$, in this case third order component $I3_{r}^s(STEC_r^s)$ is linearized around an a-priori value. Slant TEC $STEC_r^s$ can be estimated as a random walk variable, or a first order Gauss-Markov process.

In addition to the raw components, estimation of ionosphere maps can also be included
 \begin{equation}
 I_r^{s} = I_r^{s'} + \sum { k_{Ion}(i) Ib_{r}^s(i) }
\end{equation}
in this case the ionosphere modelling coefficients $k_{Ion}(i)$ should be estimated as a random walk variable, while a first order Gauss-Markov model would suit the slant TEC residuals $STEC_r^s$.

It is also possible to set the Ginan to use **ionosphere-free linear combinations** of measurements. When doing so, Ginan forms the following ionosphere-free combinations from any two signals (with diffferent frequencies $C1$ and $C2$) used by the receiver.
\begin{equation}
 P_{r,IF}^{s} = \mu_{C2} P_{r,C1} - \mu_{C1} P_{r,C2} 
\end{equation}
and
\begin{equation}
 L_{r,IF}^{s} = \mu_{C2} L_{r,C1} - \mu_{C1} L_{r,C2} 
\end{equation}
where
\begin{equation}
\mu_{c} = \frac{40.3 10^{16}}{f^{2}} + \frac{7527 * 10^{16} \lambda_f (R_B \cdot e_r^s)}{f^{2}} + \frac{2437.12557 * 10^{16}}{f^{4}} \frac{\partial  I3_{r}^{s}(STEC_r^{s})}{\partial STEC_r^{s}}
\end{equation}
for pseudoranges and 
\begin{equation}
\mu_{c} = -\frac{40.3 10^{16}}{f^{2}} - \frac{3763.5 * 10^{16} \lambda_f (R_B \cdot e_r^s)}{f^{2}} - \frac{812.37519 * 10^{16}}{f^{4}} \frac{\partial  I3_{r}^{s}(STEC_r^{s})}{\partial STEC_r^{s}}
\end{equation}
for carrier phases.

Although Ginan needs to be set to estimate $STEC_r^{s}$ in order to form the $\mu_{c}$ value, the estimate is discarded soon after the Kalman filter is applied.

## Hardware bias and ambiguities
Both pseudorange and carrier phase GNSS measurements are known to include the effect of biases product of delays in satellite and receiver hardware. 
Four biases: code/phase on satellite/receiver side exist for each signal (defined by constellation and ranging code e.g. GPS-L1C, GLO L1C, GAL-L1C, GPS-L2W, GLO-L2p, etc.). 
Ideally the PPP algorithm will seek to separate and estimate these biases. However the system of observation defined above is rank deficient, and thus additional constrains need to be applied to properly solve. 
In the case of Ginan processing a number of pseudo-observations are available for this purpose. 

**Receiver reference constellation**: when selecting a GNSS system $q$ as receiver reference, the ionosphere-free combination of receiver code biases for two signals (with diferent carrier frequencies) is set to 0:
\begin{equation}
\frac{f_1^2}{f_1^2 - f_2^2} d_{r,c1}^q -  \frac{f_2^2}{f_1^2 - f_2^2} d_{r,c2}^q= 0.
\end{equation} 
This synchronises the receiver clock with the reference GNSS constellation. It is expected that receiver hardware biases between constellations to be slow varying.

**Zero receiver DCB**: when this option is on the receiver code biases for two signals, with different carrier frequencies, tracked by the receiver will set to be equal. This pseudo-observation can be used to eliminate the rank deficiency that comes from the correlation between ionosphere delays and Differential code biases. This pseudo-observation should not be used in conjunction with ionosphere mapping.   

**Receiver ambiguity pivot**: when this option is on the one ambiguity per signal in the receiver is set to an arbitrary number (selected as to minimise the receiver phase bias). 
This elimnates the rank deficiency produced by the correlation between ambiguities and receiver phase biases.
This pseudo-observation is meant to be used in end user processng where satellite phase biases are assumed to be known.

**Satellite clock definition**: when this option is on the ionosphere-free combination of receiver code biases for two signals (defined by the ``clock_codes`` parameter, signals with different carrier frequencies must be selected) is set to 0:
\begin{equation}
\frac{f_1^2}{f_1^2 - f_2^2} d_{c1}^s -  \frac{f_2^2}{f_1^2 - f_2^2} d_{c2}^s= 0.
\end{equation} 
This pseudo-observation can be used to eliminate the rank deficiency comming from the correlation between satellite clock and satellite code biases. It also allows the satellite clock estimate to align with established standards, or to adjust the clock to specific users. 

**Zero satellite DCB**: when this option is on the satellite code biases for two signals tracked by the receiver with different carrier frequencies will set to be equal. 
As is the case for receiver DCB, this pseudo-observation responds to ionosphere-DCB rank deficiency and should not be used in conjunction with ionosphere mapping
Note that the code are selected from those tracked by receivers, this is because the ionosphere measurements will be biased by DCBs of signals used to track the signal. 
Use of this pseudo-observation is only recomended for constellations for wich different receivers may track completely different sets of signals (e.g. Galileo and Beidou). 
For other constellations we recommend explicitly setting the satellite code bias for two signals to 0.

**Common satellite phase bias**: when this option is on the satellite phase bias for signals using the same carrier frequency will be considered equal $b_{c1}^s = b_{c2}^s$

**Zero code average**: when this option is on the average of satellite code biases for a constellation will be set to 0. This pseudo-observation eliminates the rank deficieny caused by correlation between satelite and receiver biases. 
Although this rank deficiency can also ve solved by setting the code biases of a pivot station or satellite to zero, this pseudo-observation can be a substitute when there are no reliable stations in the network.  

**Zero phase average**: when this option is on the average of satellite phase biases for a constellation will be set to 0.  

**Network ambiguity pivot**: when this option is on a set of ambiguites is set to arbitrary integers.
A minimum set of ambiguities is selected to eliminate the rank deficiency produced by correlation between ambiguities, satellite phase biases and receiver phase biases.
These pseudo-observations require setting the phase biases for a receiver or satellite to 0.


## Deterministic corrections
Aside from the biases described above, the Ginan software accounts for a number of biases by calculating then using deterministic models.

Solid tide corrections can be applied when estimating the position of static stations. These corrections can include the effect of polar motion and ocean loading.

Relativitistic effects on satellite clock are estimated according to the GPS interface control document. 

Relative phase windup effects are estimated from standard satellite attitude models.  

Antenna phase centre and antenna phase variance characteristics of both satellite and receiver side antennas are obtained from ANTEX files.
