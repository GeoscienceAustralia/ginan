
# Ambiguity Resolution

Estimation and/or resolution of carrier phase ambiguities is central to precise point positioning.
Whereas standard precision positioning is performed using the unambiguous pseudorange measurement, these measurements are known to have an accuracy ranging from metres to decimetres.
Precise positioning thus rely on the use of carrier phase measurements which have an accuracy of millimetres to a few centimetres, but are ambiguous by an integer number of wavelengths.
Estimating these ambiguities to centimetre or millimetre level of accuracy is a requirement of precise positioning.
Integer ambiguity resolution offers the following advantages, over just real-valued ambiguity estimation.

* Ambiguities resolved to integer ambiguities have no errors, which improves the accuracy of the solution, it also make the measurement model more robust to changes in environmental conditions.
* Ambiguity resolution can be attempted when estimates are of around 5cm of accuracy (one 4th of wavelength) reducing their errors to 0. This results in an acceleration of convergence of PPP solutions.
* Ambiguities are constant unless there are cycle slips eliminating the need to estimate then once resolved, this in turn will simplify/accelerate the estimation of other parameters in real time applications.


As with any integer estimation process the ambiguity resolution for GNSS signals in Ginan will follow the steps below

* The ambiguities are estimated as real numbers (with the other parameters).
* Integer values of the ambiguities are resolved using the results of step 1 (the real-value ambiguities and the VCV matrix).
* The validity of integer ambiguities is tested using statistical tests.
* Ambiguity states are constrained to the resolved ambiguity

Ginan can be set to solve ambiguities in GPS, Galileo, Beidou and QZSS measurements. Ambiguity resolution are not supported for the GLONASS FDMA signals.


## Real-valued ambiguity estimation

Carrier phase ambiguities and phase biases are estimated as real numbers using carrier phase measurments 
\begin{equation}
E(L_{r,c}^s) 
= \rho_{r}^s 
+ c_{light}(dt_{r}^q - dt^s) 
+ \tau_r^s 
- I^s_r
+ b_{r,c}^q 
- b_{c}^s
+ \lambda_{f} z_{r,c}^s  
+ \phi^s_{r,f}
\end{equation}
and pseudorange measurements $P_{r,c}^s$. 

The range $\rho_{r}^s$, clock offsets $dt_{r}^q$ and $dt^s$, atmospheric delays $\tau_r^s $ and $I^s_r$, can be estimated to adequate accuracy using the pseudoranges, and $\phi^s_{r,f}$ can be corrected using deterministic models. 
However this still leaves the rank deficiency produced by correlations between ambiguities $z_{r,c}^s$ and phase biases $b_{r,c}^q$ and $b_{c}^s$.
Thus, without using extra pseudo-observations, the real valued ambiguities are expected to be contaminated by these phase biases $A_{r,c}^s = \lambda_{f} z_{r,c}^s + b_{r,c}^q - b_{c}^s$

This make it difficult to solve the ambiguites directly. 
Ginan choses instead to make and solve combinations of ambiguities, e.g.:
\begin{equation}
A_{r1,r2,  c}^{s1,s2} = A_{r1,c}^{s1} - A_{r1,c}^{s2} - A_{r2,c}^{s1} - A_{r2,c}^{s2} = \lambda_{f} (z_{r1,c}^{s1} - z_{r1,c}^{s2} - z_{r2,c}^{s1} - z_{r2,c}^{s2}).
\end{equation}
Ginan forms these combination by using the LAMBDA Z-transform/decorrelation, thus only algorithms using the Z-transform are effective in ambiguity resolution without using pivots.
 
For end user processing, where the satellite phase bias $b_{c}^{s1}$ is known, a **receiver ambiguity pivot** can be applied to separate the receiver phase bias from the embiguities.
By setting the ambiguity for one satellite $s1$ to an arbitrary valuse $\tilde{z_{r,c}^{s1}}$ the receiver phase bias will be se to 
$\tilde{b_{r,c}^q}=b_{r,c}^q+\lambda_{f} (\tilde{z_{r,c}^{s1}}-z_{r,c}^{s1})$.
This in turn will make other ambiguity estimate into solvable integers:
\begin{equation}
A_{r,c}^{s1} + b_{c}^{s1} - \tilde{b_{r,c}^q}= \lambda_{f} \tilde{z_{r,c}^{s1}}
\end{equation}
\begin{equation}
A_{r,c}^{s2} + b_{c}^{s2} - \tilde{b_{r,c}^q} = \lambda_{f} (z_{r,c}^{s2}-\tilde{z_{r,c}^{s1}}+z_{r,c}^{s1})
\end{equation}
\begin{equation}
A_{r,c}^{s3} + b_{c}^{s3} - \tilde{b_{r,c}^q} = \lambda_{f} (z_{r,c}^{s3}-\tilde{z_{r,c}^{s1}}+z_{r,c}^{s1})
\end{equation}

The **network ambiguity pivot** performs a similar function for nework processing, where both receiver and satelite biases are defined using a small set of arbitrarily set ambiguities:
1. Assign a station as anchor, and define the receiver bias $b_{r0,c}^q=0$
1. At each epoch, scan all ambiguity estimates $z_{r,c}^{s}$ 
	1. If the phase bias for receiver $b_{r,c}^q$ is defined but the satellite phase bias $b_{c}^{s}$ is not, define $b_{c}^{s}$ by setting $z_{r,c}^{s}$ to an arbitrary value
	1. If the phase bias for satellite $b_{c}^{s}$ is defined but the receiver phase bias $b_{r,c}^q$ is not, define $b_{r,c}^q$ by setting $z_{r,c}^{s}$ to an arbitrary value
1. Repeat until all phase biases are defined 

Using the ambiguity pivots allows real-value estimate of ambiguity to become close to integer values and thus use ambiguity resolution techniques without Z-transform/decorrelation 

## Integer ambiguity estimation and validation
Once the real-valued ambiguity estimates have been calculated, they can be solved into integers. Ginan implements various methods for to perform ambiguity resolution, namely:

* Integer rounding (round)
* Iterative rounding (iter_rnd)
* Integer Bootstrap (bootst)
* Lambda Integer Least Squares (lambda)
* Lambda ILS with ratio test (lambda_alt)
* Lambda ILS with common ambiguity selection (lambda_al2) 
* Lambda best integer Equivariant (lambda_bie)

At its simplest, the `round` method, just rounds selected ambiguities $z(i)$ to their nearest integer $\hat{z}(i)$. Ambiguities are selected based on two criteria, the ratio test:
\begin{equation}
|z(i)-\hat{z}(i)|<\frac{1}{R_{thres}+1}
\end{equation}
and the success rate test:
\begin{equation}
\frac{1}{2}erfc(-\frac{1-|z(i)-\hat{z}(i)|}{\sqrt{2{\bf Q_{zz}}(i,i)}})>S_{thres}
\end{equation}
where $R_{thres}$ and $S_{thres}$ are user defined thresholds, $erfc$ is the complementary error function and $Q_{zz}$ is the covariance matrix of ambiguities. 

The `round` method consider the ambiguities to be independent of each other. 
In reality the ambiguituities are highly correlated with each other, even after eliminating rank deficiencies using abiguity pivots. 
The `iter_rnd` method attempts to mitigate the effect of these correlations by iterating the integer rounding process. 
After solving the subset ${\bf j}\subset {\bf i}$ of amiguities, the estimated ambiguities and its covariance are updated as:
\begin{equation}
{\bf K}={\bf Q_{zz}}({\bf i},{\bf j}) {\bf Q_{zz}}({\bf j},{\bf j})
\end{equation}
\begin{equation}
{\bf z'}={\bf z} - {\bf K}({\bf z}({\bf j}) - {\bf \hat{z}}({\bf j})) 
\end{equation}
\begin{equation}
{\bf Q_{z'z'}}={\bf Q_{zz}} - {\bf K} {\bf Q_{zz}}({\bf j},{\bf i})
\end{equation}
and the process repeated with new set of ambiguities ${\bf z'}$. 
This eliminates the influence of errors and uncertainty of ${\bf z}({\bf j})$ from ambiguities outside the ${\bf j}$ set, hopefully facilitating the resolution of the later ones.  

Alternatively, the `bootst` and lambda methods handle ambiguity correlations by performing a Z-transform based reduction process. 
The substitutes the ambiguities ${\bf z}$ with a less correlated linear combination ${\bf z''}$.
The linear combination matrix is obtained by applying a series of unimodular transformations
\begin{equation}
{\bf z''}=(\coprod {\bf Z_{ij}}) {\bf z}
\end{equation}
where
\begin{equation}
{\bf Z_{ij}} = {\bf I} - n_{ij}{\bf e_i}{\bf e_j^{T}}
\end{equation}
with $n_{ij} \in \Bbb Z$ and ${\bf e_i}$ a column vector with 1 in positin $i$ and 0 elsewere.
The ${\bf Z_{ij}}$ transformation is equivalent to substituting $z(i)$ with $z(i)-n_{ij}z(j)$, and has the effect of changing the covariance matrix as:
\begin{equation}
{\bf L''} = {\bf L} - n_{ij}{\bf L}{\bf e_i}{\bf e_j^{T}}
\end{equation}
where ${\bf L}$ is lower triangular matrix product of the $L^TDL$ decomposition of $Q_{zz}$.
${\bf L''}$ s preseved by the transformation with the exception of:
\begin{equation}
l''_{kj} = l_{kj} - n_{ij}l_{ki} \forall k \ge i
\end{equation}
by chosing $n_{ij}$ such that $|l''_{kj}|<0.5$, the correlation between transformed ambiguities $z''$ are minimized 
The `bootst` method applies the same process as `iter_rnd` on the transformed ambiguities $z''$.

The lambda methods on the other hand use a iterative fix and adjust process to select a set of ambigities with minimum distance to the real-valued estimates.
The $R_{thres}$ and $S_{thres}$ thresholds are used in different ways. First the success rate threshold $S_{thres}$ is used to discard ambiguities that has too high an uncertainty to resolve.
The reduction process used for lambda algorithms use a reordering process alongside the de-correlation whic tends to order the $z''$ ambiguity in descending ordr of variance.
Relying on this, the last $J$ ambiguities are selected for resolutions, with $J$ selected in such a way as to fulfill 
\begin{equation}
\coprod_{n-J}^n erf(sqrt{\frac{1}{8{\bf D''}(j)}}) \ge S_{thres}
\end{equation}
where ${\bf D''}$ is obtained from the $L^TDL$ decomposition of $Q_{z''z''}$.
The potential integer set candidates ${\bf \hat{z''}_k}$ candidates are selected among those that fulfill the criteria:
\begin{equation}
\|{\bf z''}-{\bf \hat{z''}_k}\|<R_{thres}\|{\bf z''}-{\bf \hat{z''}_0}\|
\end{equation}
where ${\bf \hat{z''}_0}$ is the integer set at minimum (weighted) distance from the real-valued estmate ${\bf z''}$.
There is also a maximum number of candidate sets set by the user.

The difference between lambda methods  implemented in Ginan is the ambiguity validatio/selection process. 
- The `lambda` method does not make further selection (beyond the application of $S_{thres}$), it returns the full ${\bf \hat{z''}_0}$ set as resolved ambiguities.
- The `lambda_alt` method perform full ambiguity validation, it return ${\bf \hat{z''}_0}$ if and only if there are no alternative set that passes the $R_{thres}$ test.
- The `lambda_al2` method perform partial ambiguity validation, it returns only integer ambiguities that have common values among all candidates ${\bf \hat{z''}_k}$
- The `lambda_bie` method returns a weighted summ of all candidate sets candidates ${\bf \hat{z''}_k}$:
\begin{equation}
{\bf z''_{bie}}=\sum_k (\frac {exp^{-0.5{\|{\bf z''}-{\bf \hat{z''}_k}\|}^2}}{\sum_k exp^{-0.5{\|{\bf z''}-{\bf \hat{z''}_k}\|}^2}}{\bf \hat{z''}_k})
\end{equation}

