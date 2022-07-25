
# Ambiguity Resolution

Estimation and/or resolution of carrier phase ambiguities is central to precise point positioning.
Whereas standard precision positioning is performed using the unambiguous pseudorange measurement, these measurements are known to have an accuracy ranging from decimetres to metres.
Precise positioning thus rely on the use of carrier phase measurements which have an accuracy of millimetres to a few centimetres, but are ambiguous by an integer number of wavelengths.
Thus estimating these ambiguities to centimetre or millimetre level of accuracy is a requirement of precise positioning.
Integer ambiguity resolution offers the following advantages, over just real-valued ambiguity estimation.

* Ambiguities resolved to integer ambiguities have no errors, which include the accuracy of the solution, it also make the measurement model more robust to changes in environmental conditions.
* Ambiguity resolution can be attempted when estimates are of around 5cm of accuracy (one 4th of wavelength) reducing their errors to 0. This results in an acceleration of convergence of PPP solutions.
* Ambiguities are constant unless there are cycle slips eliminating the need to estimate then once resolved, this in turn will simplify/accelerate the estimation of other parameters in real time applications.


As with any integer estimation process the ambiguity resolution for GNSS signals in Ginan will follow the steps below

* The ambiguities are estimates as real numbers (with the other parameters).
* Integer values of the ambiguities are resolved using the results of step 1 (the real-value ambiguities and the VCV matrix).
* The validity of integer ambiguities is tested using statistical tests.
* Application of integer ambiguities

The Ginan software can be set to solve ambiguities in GPS and Galileo measurements. Ambiguity resolution for Beidou and QZSS signal can be expected in the future. Due to it use of FDMA in its system, ambiguity resolution on GLONASS is known to be of a particular challenge. For this reason ambiguity resolution for GLONASS is not planned for Ginan until GLONASS's new CDMA signals are fully operational.


## Real-valued ambiguity estimation

*Reference or pivot chain* is normally done by selecting a station with the most number of observations, however this is not possible to no a-priori for a systems that is designed to run in real-time.


1. Assign a station as anchor, the user is encouraged to select a reliable station.
1. Biases for the anchor station $b_r$ are set to zero
1. Ambiguities for signal in anchor station $z_r^s$ are set to minimise satellite biases
1. Set satellite biases as $b^s = A_r^s - z_r^s - b_r$
1. For each satellite with defined biases:
    1. Find ambiguity measurements $A_r^s$ for which the receiver $b_r$ side bias is undefined
    1. Ambiguities for signal $z_r^s$ are set to minimise receiver biases
    1. Set receiver biases as $b_r = A_r^s - z_r^s - b^s$
1. For each receiver with defined biases:
    1. Find ambiguity measurements $A_r^s$ for which the satellite side bias $b^s$ is undefined
    1. Ambiguities for signal $z_r^s$ are set to minimise satellite biases
    1. Set satellite biases as $b^s = A_r^s - z_r^s - b^s$
1. Repeat steps 5 and 6 until biases are defined for all receivers and satellites

## Integer ambiguity estimation and validation
In the pea we have implemented a number of different ambiguity resolution strategies:

* Integer rounding
* Iterative rounding
* Lambda Integer Least Squares
* Best integer Equivariant (BIE)

<!--
### Integer rounding

The simplest strategy to apply is to round the real-values estimates to the nearest integers, without using any variance co-variance information.

### Iterative rounding

The bootstrapping algorithm takes the first ambiguity and rounds its value to the nearest integer. Having obtained the integer value of this first ambiguity, the real-valued estimates of all remaining ambiguities are then corrected by virtue of their correlation with the first ambiguity. 
Then the second, but now corrected, real-valued ambiguity estimate is rounded to its nearest integer, and the process is then repeated again with both ambiguities held fixed, and the process is continued until all ambiguities are accommodated. 
Thus the bootstrapped estimator reduces to ’integer rounding’ in case correlations are absent.

### Lambda Integer Least Squares

### BIE
The previously mentioned algorithms are known as hard decision algorithms.

## Application of integer ambiguities

The Melbourne-Wubbena linear combination (Melbourne 1985),(Wubbena 1985) is a linear combination of the L1 and L2 carrier phase plus the P1 and P2 pseudorange. The geometry, troposphere and ionosphere are eliminated by it. The Melbourne-Wubbena linear combination can be represented as:

\begin{equation}
E(L_{r,IF}^S) - \frac{cf_2z_{r,w}^s}{f_1^2 - f_2^2} = \rho_r^s + c(dt_{r,IF} - dt_{IF}^s) + \tau_r^s + \lambda_n z_{r,1}^s + (\lambda_{IF}\delta_{r,IF}
\end{equation}

Since ,,\% comprises of both code and phase measurements, it is reasonable to exclude the lower
elevation measurements to avoid the multipath impacts from the code observation. Normally, with
30 degree elevation cut-off, an averaging of 5 minutes of (4) is good enough to fixing the wide-lane
ambiguities [RD 04]. The rests are the wide-lane phase bias, which can be broadcasted to the user for
user side wide-lane ambiguity resolution. Either choosing a pivot receiver bias or a single-differencing
between two satellites can avoid the linear dependency. 


highly correlated need lambda
% \[E(L^S) - \]


With the fixing of the wide-lane ambiguity, equation (1) and (2) can be further deducted as:

The code bias and phase bias in equations \ref{obsEq} and (6) can be lumped into the corresponding receiver
and satellite clock errors. Then equations (5) and (6) become:

with:

By such an reformulation, there are two types of satellite clock: 
1) IGS type clock, but estimated only
from code measurements; 
2) phase clock, estimated using phase measurements and can be used to
support PPP ambiguity resolution on the user side.

The drawback of this approach is that there is no
precise IGS compatible clock after the processing and it has to be derived from the existing PEA
processing

1. A modified phase clock/bias model to improve PPP ambiguity resolution at Wuhan University Journal of Geodesy - Geng et al. (2019)
1. On the interoperability of IGS products for precise point positioning with ambiguity resolution Journal of Geodesy - Simon et al. (2020)
1. Resolution of GPS carrier-phase ambiguities in precise point positioning (PPP) with daily observations Journal of Geodesy - Ge et al. (2008)
1. Real time zero-difference ambiguities fixing and absolute RTK ION NTM - Laurichesse et al. (2008)
1. Undifferenced GPS ambiguity resolution using the decoupled clock model and ambiguity datum fixing Navigation – Collins et al. (2010)
1. Improving the estimation of fractional-cycle biases for ambiguity resolution in precise point positioning Journal of Geodesy – Geng (2012).
1. Modeling and quality control for reliable precise point positioning integer ambiguity resolution with GNSS modernization GPS Solutions – Li et al. (2014).
-->