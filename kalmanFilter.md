
# Kalman Filtering

The Kalman filter is an algorithm that processes observation data over time to produce precise estimates of unknown parameters that may not be directly observable. Named after Rudolf E. Kálmán, the great success of the Kalman filter is due to its small computational requirement, elegant recursive properties, and its status as the optimal estimator for one-dimensional linear systems with Gaussian error statistics.
Kalman filtering is used in a wide range of applications include global positioning system receivers, control systems, smoothing the output from laptop trackpads, and many more.
The PEA and POD software utilities are fundamentally an application-specific Kalman filter that takes observations (namely, GNSS signals) and fuses them together to provide robust estimates of parameters of interest.

## Overview of Kalman Filtering

Kalman filters are typically used to estimate parameters which change with time.
A Kalman filter has measurements $y_t$, with unknown error $\epsilon_t$, and a state vector $x_t$ (or a parameter list) which have specified statistical properties.

The observation equation at time t is given by:
\begin{equation}
    y_t = H_t x_t + \epsilon_t	 \label{eq:kfObs}
\end{equation}

The state transition equation is given by:
\begin{equation}
    x_{t+} = F_t x_t + w_t	
\end{equation}

Kalman filter processing is broken up into two main steps: *Prediction* and *Update*.

### Prediction Step
The *prediction* step uses a model to 'predict' the parameters at the next data epoch. 
The state transition matrix $F$ projects the state vector (parameters) forward to the next epoch, as follows:
\begin{equation}
    \hat{x}_t^{t-1} = F_t \hat{x}_{t-1}^{t-1}
\end{equation}
where $\hat{x}_t$ is the state vector and $F_t$ is the state transition matrix. Subscripts denote the time that a quantity refers to, and superscripts denote the time of the most recent observation used to estimate the quantity. E.g. $\hat{x}_t^{t-1}$ is the state vector estimate for time $t$, using observations up to and including time $t-1$.

Similarly, the state transition matrix is used to project the state covariance matrix (the uncertainty of the state vector) forward in time:

\begin{equation}
    P_t^{t-1} = F_t P_{t-1}^{t-1} F_t^\intercal + Q_t
\end{equation}
where $P$ is the state covariance matrix and $Q_t$ is the process noise covariance matrix. Process noise ($Q$ matrix) is added to reflect the increase in uncertainty caused by projecting states forward in time. Deterministic parameters have $Q$ elements of 0, and stochastic parameters have positive $Q$ elements.

The state transition matrix $F$ can take several forms depending on the nature of the state being estimated - e.g.:

* For random walk states: $F$ = 1
* For states with rate terms: $F$ is the matrix 
    $\begin{bmatrix}
    1 & \delta t\\
    0 & 1
  \end{bmatrix}$
* For first-order Gauss-Markov (FOGM) states: $F$ = $e^{-\delta t \beta}$
* For white noise: $F$ = 0


The *Kalman gain* allocates the differences between the observation at time t+1 and their predicted value at this time based on the current values of the state vector according to the noise in the measurements and the state vector noise.

### Update Step
The *update* step 'updates' the predicted states with observation data from the current epoch.

Firstly, the prefit residual is calculated, which measures the error between the actual observations and the expected observations given the predicted state vector:

\begin{equation}\label{prefit_resid}
    \hat{y}_{t} = z_t - H_t \hat{x}_t^{t-1}
\end{equation}

where $\hat{y}_{t}$ is the prefit residual, $z_k$ is the observation vector (list of observations made this epoch) and $H_t$ is the design matrix. The design matrix transforms from state-space into observation-space - i.e. $H_t \hat{x}$ is the observation you would get if you perfectly observed the state $\hat{x}$.

The corresponding prefit residual covariance matrix is given by:

\begin{equation}
    S_{t} = H_t P_t^{t-1} H_t^T + R_t
\end{equation}
where $R_t$ is the measurement noise matrix - the covariance matrix corresponding to the observation vector.

The optimal Kalman gain is then calculated:

\begin{equation}
    K_{t} = P_t^{t-1} H_t^T + S_t^{-1}
\end{equation}

which is then used to calculate the optimal adjustment to the predicted states to get the updated states:

\begin{equation}
    \hat{x}_t^t = \hat{x}_t^{t-1} + K_t\hat{y}_t
\end{equation}

and corresponding updated state covariance matrix:
\begin{equation}
    P_t^t = (I - K_t H_t) P_t^{t-1}
\end{equation}

## Comparison between Weighted Least Squares and Kalman Filtering

* In Kalman filtering, apriori constraints must be given for all parameters. This is not needed in weighted least squares, but can also be done.
* Kalman filters can allow for 0 variance parameters, this cannot be done in WLS, as this requires the inversion of the constraint matrix.
* Kalman filters can allow for a method of applying absolute constraints, WLS can only tightly constrain parameters.
* Kalman filters are more prone to numerical stability problems, and take longer to run (they have more parameters).
* Process noise models can be implemented in WLS, but they are computationally slow.

## Implementation in the PEA

### Robust Kalman Filter Philosophy

It is well known that the Kalman filter is the optimal technique for estimating parameters of interest from sets of noisy data - provided the model is appropriate.

In addition, statistical techniques may be used to detect defects in models or the parameters used to characterise the data, providing opportunities to intervene and make corrections to the model according to the nature of the anomaly.

By incorporating these features into a single generic module, the robustness that was previously available only under certain circumstances may now be automatically applied to all systems to which it is applied. These benefits extend automatically to all related modules (such as RTS), and often perform better than modules designed specifically to address isolated issues.

### Initialisation

When parameters' initial values are not known a-priori, it is often possible to determine them using a least-squares approach.

To minimise processing times, the minimal subset of existing states, measurements, and covariances are used in least-squares estimation whenever the initial value and variance of a parameter is unspecified.

For rate parameters, multiple epoch’s worth of data are required for an ab-initio initialisation. This logic is incorporated into the filter and is applied automatically as required.

### Outlier detection, Iteration, and Hypothesis Testing

As a statistical machine, the Kalman filter is capable of detecting measurements that do not fit within the system as modelled.

In these cases, the model may be adjusted on-the-fly, to allow all measurements to be continued to be used without contaminating the results in the filter.

A typical example of a modelling error in GNSS processing is a cycle-slip, in which the ambiguity term (which usually modelled with no change over time) has a discontinuity. Other examples may include clock-jumps or satellite burns.

Hypotheses are to be generated for any measurements that are statistical outliers, and the model iterated as required.

### Performance Optimisation

The inversion of large matrices as required by the Kalman filter easily dominates the processing time required during operation. Techniques are available to reduce, and distribute this processing burden across multiple processors.

The Eigen library is used for algebraic manipulation which allows for automatic parallelisation of vector algebra, and improves code robustness by checking matrix dimensions while in use.

#### Chunking

By dividing measurements into multiple smaller sub-matrices, the long inversion times may be reduced, as the inversion order is of $O(n^3)$

#### Blocking
By separating the filter covariance matrix into a block-diagonal form, individual blocks of the filter may be processed individually, without degradation in accuracy. This may improve performance, and may also enable blocks that are relatively independent to be processed separately, albeit with some degradation in accuracy.

## Recommended Reading

1. https://ocw.mit.edu/courses/earth-atmospheric-and-planetary-sciences/12-540-principles-of-the-global-positioning-system-spring-2012/lecture-notes/MIT12\_540S12\_lec13.pdf