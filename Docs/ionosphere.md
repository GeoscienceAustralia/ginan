
# Ionosphere Mapping/Modelling

*Ionospheric delay* is the most significant nuisance parameter in GNSS processing. 
The GNSS processing algorithm needs to account for it by estimating, correcting or cancelling its effects.
Single frequency receivers needs to be provided with Ionosphere delay information to perform positioning, and the accuracy of its positioning algorithm will be directly affected by the accuracy of the Ionospheric delay information.

GNSS receivers that track signals from multiple carriers with different frequencies, can estimate and or cancel Ionospheric delays, but the process of jointly estimating the Ionospheric delay and carrier phase ambiguity means it requires up to a few hours for the solutions to converge to centimetre-level precision.

## Ionosphere measurements
Ginan version 1 Alpha, uses Ionosphere-free combinations to estimate most of GNSS error parameters. 
For this reason Ionospheric delays, signal delays due to propagation through the Ionosphere, are not estimated as part of the main Ginan processing.
Ionospheric delay measurement and mapping is thus performed as a separate, complementary process.
Where orbit/clock estimation use ionosphere-free combinations, Ginan 1.0 use geometry-free combinations as a proxy to Ionosphere delay measurements.

Two types of Ionosphere measurements can be used in Ginan 1.0 for Ionosphere delay mapping.
The *Smoothed pseudorange* geometry free measurements are calculated directly from GNSS measurements, independent from the main GNSS processing.
\begin{equation}\label{eq:smoothed_pseud_iono}
\hat{I^s_r} = \frac{\lambda^2_1}{\lambda^2_2 - \lambda^2_1} \left( (L^s_{1,r} - L^s_{2,r}) - \overline{(L^s_{1,r} - L^s_{2,r} + P^s_{1,r} - P^s_{2,r})} \right) 
\end{equation}
where $\overline{x}$ is the average of $x$. If code and phase biases could be considered constant over one satellites visibility arc, then this measurement will asymptote to the biased Ionosphere delay $\tilde{I^s_r}$ ([observation modelling](#observation-modelling)). In reality, the variability of code and phase biases as well as errors in the averaging process.

A more precise measurement can be generated if using the results of the main Ginan processing. After ambiguities are resolved and satellite/station biases resolved, the remaining nuisance parameters in the geometry-free combination of phase biases can be eliminated.
\begin{equation}\label{eq:PPP_phase_iono}
\tilde{I^s_r} = \frac{\lambda^2_1}{\lambda^2_2 - \lambda^2_1} E\left( (L^s_{1,r} - \tilde{b_{1,r}} + \tilde{b_{1}^{s}} - \lambda_{1} z_{1,r}^s  - \phi^s_{1,r}) - (L^s_{2,r} - \tilde{b_{2,r}} + \tilde{b_{2}^{s}} - \lambda_{2} z_{2,r}^s  - \phi^s_{2,r}) \right)
\end{equation}

## Thin layer VTEC maps
Ginan plans to use multiple techniques to map Ionospheric delays in GNSS measurements.
So far thin layer, global vertical total electron content maps has been implemented in Ginan.
Single layer thin layer models like Klobuchar and SBAS/IONEX mapping is widely used for GNSS mapping.
*Total Electron Content (TEC)* is a measure of the number of electrons/ions in cylinder of $1m^2$ transverse section along the satellite-receiver path. 
The first order Ionospheric delay seen by a signal while traversing through the Ionosphere is proportional to the TEC and inversely proportional to the square of the frequency.
\begin{equation}
  I = \frac{\lambda^2_f r_e}{2\pi} TEC
\end{equation}
where $r_e$ is the electron radius. 
The Ionosphere delay used in Ginan $I^s_r$ corresponds to the delay at L1 frequency (1545.75 MHz).
Which means the Ionospheric delay will have $I^s_r \approx 0.1687 TEC $ where $TEC$ is in units of TECu, where $TECu = 10^{16} electrons/m^2$.
In this mapping/modelling method, the electron content are assumed to be concentrated in thin shells at fixed altitudes.
The thin shell model allows the definition of a single piercing point, the point in which the satellite-receiver path intersects the ionosphere shell, for each layer. 
Assuming that the electron content is constant around the piercing point, the total slant TEC can be approximated as:
\begin{equation}
  TEC \approx \sum_{lay} \frac{VTEC_{lay}}{cos\chi_{lay}}
\end{equation}
where $\chi_{lay}$ is the angle from zenith and $VTEC_{lay}$ is the vertical total electron content at the piercing point. 

The vertical total electron content for each layer is mapped using spherical harmonics.
\begin{equation}
  VTEC_{lay}(\varphi_{lay}, \vartheta_{lay}) = \sum_{m,n} Q_{m,n}(\varphi_{lay}) \left( A_{m,n,lay} cos(m \vartheta_{lay}) + B_{m,n,lay} sin(m \vartheta_{lay}) \right)
\end{equation}
where the co-latitude $\varphi_{lay}$ is the angle in latitudinal direction, around an axis orthogonal to both the geographic north and sun direction, between the piercing point and the sun. $\vartheta_{lay}$ is the angle in longitudinal direction, rotation axis orthogonal to the axis of co-latitude and the sun. 
The function $Q_{m,n}$ correspond to the associated Legendre polynomial of degree $n$ and order $m$, re-parameterized in terms of angles. 
The ionosphere modelling module of Ginan, use estimates of Ionosphere delays from smoothed pseudorange or PPP results to estimate the Vertical TEC coefficients $A_{m,n,lay}$ and $B_{m,n,lay}$ alongside with the satellite DCB $d_{r,1} - d_{r,2}$ and receiver DCB $d_{1}^s - d_{2}^s$