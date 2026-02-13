
# Equation Conventions

In this manual we will be adhering to the following conventions:

## List of Symbols

| Variable		| Description																									|
| -				| -																												|
|$i$ or $r$ | Receiver identification|
|$j$ or $s$ | Satellite identification |
|$k$ or $t$ | Epoch number |
|$q$  | GNSS type (GPS,GALILEO,GLONASS,QZSS)|
|$c$  | Speed of light [m/s]|
|$x$  | Vector of parameters to be estimated|
|$y$  | Vector of observations|
|$v$  | Vector of residuals|
|$H$ |  Design matrix|
|$P$  | Covariance matrix|
|$R$  | Measurement noise matrix|
|$Q$  | Process noise matrix|
|$\sigma$  | Standard deviation of observable|
|$\Delta$ |  Increment to a priori values [m]|
|$\lambda$ or $\lambda_1,\lambda_2,\lambda_5$ | Wavelength |
|$f_1,f_2,f_5$ |  frequency|
|$N$  | Ambiguity or $N$} Real valued ambiguity and {$z$}} Integer part of real valued ambiguity|
|$\alpha$  | level of significance|
|$d$  | Code Biases|
|$b$  | Phase Biases|
|$z$ |  (Integer) Carrier phase ambiguities|
|$dt$ |  Clock error [s]|
|$\kappa$  | Correction - relativity|
|$\iota$ or $I$ |  Ionosphere |
|$\tau$ or $T,T_h,T_w$  | Troposphere |
|$m_W$ |  elevation dependent mapping function for the troposphere hydrostatic delay|
|$m_W$  | elevation dependent mapping function for the troposphere wet delay|
|$\xi$ |  Phase wind-up error|
|$\epsilon$  | Error in observations and unmodelled effects [m]|
|$\rho_i^j$  | Geometric distance between satellite and receiver|
|$L_i^j$  | Carrier phase observable (times c) [m]|
|$P_i^j$ |  Pseudo range observable [m]|
|$\psi$ |  Satellite yaw [rad]|
 

Example: for an undifferenced, uncombined float solution, the linearized observation equations for pseudorange and phase observations from satellite $s$ to receiver $r$ can be described as:

\begin{alignat}{2}
\label{eq:codeMeasurement}
\Delta P_{r,f}^{s}
&= u_r^{s} . \Delta x 
+ c . (dt_r^q - dt^{s}) 
+ M_r^{s} . T_r 
+ \mu_f . I_{r}^{s} 
&+ d_{r,f}^q 
- d_f^{s} 
&+ \epsilon_{P,f}^s
\\
\label{eq:phasMeasurement}
\Delta L_{r,f}^{s} 
&= u_r^{s} . \Delta x 
+ c . (\delta t_r^q - \delta t^{s}) 
+ M_r^{s} . T_r 
- \mu_f . I_{r}^{s}
+ \lambda_f . N_{r,f}^{s} 
&+ b_{r,f}^q 
- b_f^{s} 
&+ \epsilon_{L,f}^s 
\end{alignat}

where $\Delta P_{r,f}^{q,s}$ and $\Delta\phi_{r,f}^{q,s}$ are the respective pseudorange and phase measurements on the frequency $f$(f=1,2), from which the computed values are removed;
$u_r^{q,s}$ is the receiver-to-satellite unit vector;
$\Delta x$ is the vector of the receiver position corrections to its preliminary position; 
$dt_r^q$ and $dt^{q,s}$ are the receiver and satellite clock errors respectively;
$c$ is the speed of light in a vacuum
$M_r^{q,s}$ is the elevation dependent mapping function for the troposphere wet delay from the corresponding zenith one $T_r$;
$I_{r,1}^{q,s}$ is the ionosphere delay along the line-of-sight from a receiver to a satellite at the first frequency and $mu_f^q = (\lambda_f^q / \lambda_1^q)^2$;
$\lambda_f^q$ is the wavelength for the frequency $f$ of a GNSS $q$;
$z_{r,f}^{q,s}$ is the phase ambiguity 
$d_{r,f}^q$ and $b_{r,f}^q$ are the receiver hardware delays of code and phase observations respectively;
$d_f^{q,s}$ and $b_f^{q,s}$ are the satellite hardware delays of code and phase observations, respectively;
$\epsilon_{P,f}$ and $\epsilon_{L,f}$ are the code and phase measurement noises respectively. 