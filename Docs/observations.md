 

# Observation Modelling

The Ginan toolkit is based on the concept of Precise Point Positioning (PPP).
PPP is a high accuracy positioning method that seeks to correct errors in GNSS positioning and thus improve accuracy.

Unlike differential GNSS techniques, which seek to measure GNSS errors using a nearby reference station, PPP is based in the robust modelling and estimation of systematic errors in the GNSS signals.

In PPP, the GNSS measurements are modelled as normally distributed random variables with mean:

###### Observation Equations:

\begin{alignat}{2} 
\label{eq:code_UC_mea}
E(P_{r,f}^s) 
&= \rho_{r}^s 
+ c(dt_{r}^q - dt^s) 
+ \tau_r^s
+ \mu_f I^s_r 
+ d_{r,f}^q
+ d_{f}^s
\\
\label{eq:phase_UC_mea}
E(L_{r,f}^s) 
&= \rho_{r}^s 
+ c(dt_{r}^q - dt^s) 
+ \tau_r^s 
- \mu_f I^s_r
+ b_{r,f}^q 
- b_{f}^s
+ \lambda_{f} z_{r,f}^s  
+ \phi^s_{r,f}
\end{alignat}

and a constant or elevation dependent variance.

\begin{equation} \label{eq:code_UC_var}
\sigma(P_{r,f}^s) = \frac{\sigma_0}{sin^2(\theta_{el})}
\end{equation}

In these equations:

| Variable		| Description																									|
| -				| -																												|
| $P_{r,f}^S$	| represents the pseudorange measurements (m) between satellite $s$ and receiver $r$ for carrier frequency f. 	| 
| $L_{r,f}^s$	| represents the carrier phase measurements (m) 																|
| $E()$			| notates the expectation and  $ \sigma() $ the variance. 														|
| $\rho$		| the geometric distance (m) 																					|
| $c$			| speed of light (m/s) 																							|
| $dt_r^q$		| receiver clock offset (s)                                                                                 	|
| $dt^s$		| satellite clock offset (s)                                                                                  	|
| $\tau_r^s $	| slant troposphere delay between satellite $s$ and receiver $r$ (m)                                          	|
| $\mu_f$		| Ionosphere delay factor for frequency $f$																		|
| $I^s_r$		| slant Ionosphere delay for $s$ and receiver $r$ (m)															|
| $d_{r,f}^q$	| receiver hardware bias for pseudoranges (m)																	|
| $d_{f}^s$		| satellite hardware bias for pseudoranges (m)																	|
| $b_{r,f}^q$	| receiver ionosphere-free phase bias (m)																		|
| $b_{f}^s$ 	| satellite ionosphere-free phase bias (m)																		|
| $\lambda_{f}$ | the signal carrier wavelength for frequency $f$(m)															|
| $z_{r,f}^S$	| carrier phase ambiguity (cycle)																				|

High accuracy GNSS position estimation requires precise estimation of the geometric distance  $\rho_{r}^s$. The effect of other parameters in the observation equations need to be eliminated either by modelling/estimating or, in the case of Ionosphere delays, by using linear combinations.
 
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

## Ionosphere free combination
The current version of the Ginan software, the effect of Ionosphere delay is eliminated using the Ionosphere free combination of two measurements. Thus the Ionosphere-free combination of pseudorange
\begin{equation} \label{eq:code_IF_raw}
P_{r,IF}^s = \frac{\mu_2}{\mu_2 - \mu_1} P_{r,1}^s - \frac{\mu_1}{\mu_2 - \mu_1} P_{r,2}^s = \frac{\lambda^2_2}{\lambda^2_2 - \lambda^2_1} P_{r,1}^s - \frac{\lambda^2_1}{\lambda^2_2 - \lambda^2_1} P_{r,2}^s
\end{equation} 
and carrier phase
\begin{equation} \label{eq:phase_IF_raw}
L_{r,IF}^s = \frac{\mu_2}{\mu_2 - \mu_1} L_{r,1}^s - \frac{\mu_1}{\mu_2 - \mu_1} L_{r,2}^s = \frac{\lambda^2_2}{\lambda^2_2 - \lambda^2_1} L_{r,1}^s - \frac{\lambda^2_1}{\lambda^2_2 - \lambda^2_1} L_{r,2}^s
\end{equation} 
are used instead of their uncombined versions. The combinations above will remove the first order (with respect to frequency) component of the Ionosphere delays, leaving the higher order components known to be smaller then a few centimetres. It also preserves the scale of geometric (positions, clocks and tropospheric) biases.

\begin{equation} \label{eq:code_IF_mea}
E(P_{r,IF}^s) = \rho_{r}^s + c(dt_{r}^q - dt^s) + \tau_r^s + d_{r,IF}^q + d_{IF}^s
\end{equation}
\begin{equation} \label{eq:phase_IF_mea}
E(L_{r,IF}^s) = \rho_{r}^s + c(dt_{r}^q - dt^s) + \tau_r^s  + b_{r,IF}^q - b_{IF}^s+ A_{r,IF}^s  + \phi^s_{r,IF}
\end{equation}

However the carrier phase combination will also remove the integer nature of the ambiguity 
\begin{equation} \label{eq:phase_IF_amb}
A_{r,IF}^s = \frac{\lambda^2_2\lambda_1}{\lambda^2_2 - \lambda^2_1} z_{r,1}^s - \frac{\lambda^2_1\lambda_2}{\lambda^2_2 - \lambda^2_1} z_{r,2}^s = \frac{\lambda_1\lambda_2}{\lambda_1 + \lambda_2}z_{r,1}^s + \frac{\lambda^2_1\lambda_2}{\lambda^2_2 - \lambda^2_1} (z_{r,1}^s - z_{r,2}^s)
\end{equation}
thus another ionosphere-free combination, the Melbourne-Wubenna combination, is used to isolate and estimate the $ (z_{r,1}^s - z_{r,2}^s)$ ambiguity
\begin{equation} \label{eq:phase_MW_mea}
A_{r,IF}^s = E(\frac{L_{r,1}^s}{\lambda_1} - \frac{L_{r,2}^s}{\lambda_2} +\frac{\lambda_2 - \lambda_1}{\lambda_2 + \lambda_1} (\frac{P_{r,1}^s}{\lambda_1} + \frac{P_{r,2}^s}{\lambda_2} ) )= z_{r,1}^s - z_{r,2}^s + d_{r,MW}^s
\end{equation}

## Troposphere modelling
The tropospheric delays are separated into two components: the hydrostatic delay (dependent on temperature and pressure), and the wet delay (dependent also on humidity). Each component can be expressed as the product of a zenith delay and an elevation based mapping. 
\begin{equation}
 \tau_r^s = m_{H}(\theta_{el,r}^s) \tau_{ZHD,r} +  m_{W}(\theta_{el,r}^s) \tau_{ZWD,r} 
\end{equation}
In the case of Ginan, the hydrostatic components are assumed to be deterministic, while the zenith wet delay can be set as deterministic or estimated as part of the PPP solution process. The nominal values of Zenith hydrostatic delays $\tau_{ZHD,r}$ and hydrostatic mapping function $ m_{H}(\theta_{el,r}^s)$ and the wet mapping function $m_{W}(\theta_{el,r}^s)$ are estimated using either the VMF3 (daily measured) or GPT2/GMF (empirical) models.

When estimated, the tropospheric wet delay is modelled by one variable $\tau_{0,r}$ or three variables
\begin{equation}
 \tau_{ZWD,r} =  \tau_{0,r}+cot(\theta_{el,r}^s) \Big(cos(\theta_{az,r}^s) grad_{ns,r} + sin(\theta_{az,r}^s) grad_{ew,r}\Big)
\end{equation} 
each variable is modelled as a random walk of a First order Gauss-Markov process.

## Hardware bias and ambiguities
Both pseudorange and carrier phase GNSS measurements are known to include the effect of biases product of delays in satellite and receiver hardware. Ideally the PPP algorithm will seek to separate and estimate these biases. The system of [observation equations](#observation-equations) defined above are rank deficient (2 x freq biases + other parameters to be solved using 2 x freq. number of equations) however, and thus only a combination of biases can be solved. 
In the case of Ginan processing of GNSS measurements, the clock offsets being estimated correspond to the ionosphere-free combination of two pseudorange measurements.
\begin{equation}
\tilde{cdt_{r}} = cdt_{r} + d_{r,IF}
\end{equation} 
\begin{equation}
\tilde{cdt^s} = cdt^s + d_{IF}^s
\end{equation} 
Also, when estimating GNSS error parameters without the help of an underlining Ionosphere model, the geometry free combination of pseudorange measurements is assimilated into the Ionosphere delay estimate
\begin{equation}
\tilde{I^s_r} = I^s_r - \frac{\lambda^2_1}{\lambda^2_2 - \lambda^2_1} (d_{r,1} - d_{r,2} - d_{1}^s + d_{2}^s)
\end{equation} 
Reparameterisin the observation equations to include $cdt'$ and $I'^s_r $ results in
\begin{equation} \label{eq:code_UC_mod}
E(P_{r,f}^s) = \rho_{r}^s + c(\tilde{dt_{r}^q} - \tilde{dt^{s}}) + \tau_r^s + \mu_f \tilde{I^s_r} 
\end{equation}
\begin{equation} \label{eq:phase_UC_mod}
E(L_{r,f}^s) = \rho_{r}^s + c(\tilde{dt_{r}^q} - \tilde{dt^{s}}) + \tau_r^s - \mu_f I'^s_r + \tilde{b_{r,f}^q} - \tilde{b_{f}^{s}} + \lambda_{f} z_{r,f}^s  + \phi^s_{r,f}
\end{equation}
where 
\begin{equation}\label{eq:mod_phase_bia_rec}
\tilde{b_{r,f}^q} = b_{r,f} + d_{r,f}^q - d_{r,IF}
\end{equation} 
\begin{equation}\label{eq:mod_phase_bia_sat}
\tilde{b_{f}^{s}} = b_{f}^s + d_{f}^s - d_{IF}^s
\end{equation} 
The current version of Ginan performs Ionosphere modelling separately, thus the phase biases estimated alongside geometric parameters like clocks and troposphere correspond to these values

When performing Ionospheric delay modelling, as detailed in [Ionosphere mapping / modelling](#ionosphere-mappingmodelling), the Ginan software generates the an Ionosphere delay map from which to estimate $I^s_r$ and differential pseudorange biases $d_{r,1} - d_{r,2}$ and $d_{1}^s - d_{2}^s$. If the end user intends to use both Ionosphere corrections and ambiguity resolution, the following set of biases needs to be used alongside the Ionosphere maps for $I^s_r$:
\begin{equation}\label{eq:ion_code_bia_sat}
\hat{d_{f}^{s}} = - \frac{\lambda^2_{f}}{\lambda^2_2 - \lambda^2_1} ( d_{1}^s - d_{2}^s) 
\end{equation} 
\begin{equation}\label{eq:ion_phase_bia_sat}
\hat{b_{f}^{s}} = \tilde{b_{f}^{s}} - \hat{d_{f}^{s}}
\end{equation} 

## Deterministic biases
Aside from the biases/errors described above, the Ginan software accounts for a number of biases by calculating then using deterministic models.
Solid tide corrections can be applied when estimating the position of static stations. These corrections can include the effect of polar motion and ocean loading.
Relativitistic effects on satellite clock are estimated according to the GPS interface control document. Receiver side effects are not accounted for.
Relative phase windup effects are estimated from standard satellite attitude models.  
Antenna phase centre and antenna phase variance characteristics of both satellite and receiver side antennas are obtained from ANTEX files.