

# Attitude Modelling
A satellite's attitude is its orientation in space. Satellite attitude affects both the position and orientation of antenna arrays, as well as the spacecraft profile with respect to solar radiation or drag.


## Nominal Yaw
GNSS satellite attitudes are mainly dictated by their mission and operational requirements. In order to transmit GNSS signals, the antenna array (located on the satellite -Z axis) must point toward Earth. Secondly, in order to maximise solar power, satellites rotate around their Z axis (yaw) and rotate their solar panels around the Y axis to point toward the Sun.
The optimal yaw $\psi$ which allows solar panels to point directly at the Sun is given by:

\begin{equation}
	\psi = atan2(-tan(\beta), sin(\mu)) + \pi
\end{equation}

where $\beta$ is the Sun elevation angle with respect to the orbital plane and $\mu$ is the angle of the satellite from orbital 'midnight' - i.e. when satellite is at the furthest point from the Sun in its orbit.


## Modelled Yaw
At low $\beta$ angles as the satellite passes through orbital noon or midnight, satellites experience 'gimbal lock' where the rate of yaw change required by the nominal yaw equation approaches $\pm\infty$ . Each constellation block has different modified yaw steering strategies to handle this problem, summarised below.

| Block			| Yaw steering strategy																						|
| -				| -																											|
| GPS-IIA		| Noon: catch up steering at max yaw rate; Midnight: max yaw rate steering during eclipse					|
| GPS-IIF		| Noon: catch up steering at max yaw rate; Midnight: constant yaw rate steering during eclipse				|
| GPS-IIR		| Catch up steering at max yaw rate																			|
| GPS-III		| Smoothed yaw steering																						|
| GAL-IOV		| Smoothed yaw steering via auxiliary Sun vector															|
| GAL-FOC		| Smoothed yaw steering																						|
| GLO			| Noon: centered yaw steering at max yaw rate; Midnight: max yaw rate steering upon eclipse entry then stop	|
| GLO-K			| Unknown																									|
| QZSS-1		| Orbit-normal (yaw = 0) during specified periods															|
| QZSS-2A/2I	| Centered yaw steering at max yaw rate, with orbit-normal during specified periods							|
| QZSS-2G		| Orbit-normal																								|
| BDS-2I/2M		| Orbit-normal at low beta angles																			|
| BDS-2G		| Orbit normal																								|
| BDS-3I/3M		| Smoothed yaw steering																						|
| BDS-3M-SECM	| Smoothed yaw steering using auxiliary beta angle															|


## Precise Attitudes

Ginan is also able to use a-priori satellite attitudes, in place of the models above, in cases where GNSS satellite attitudes have already been precomputed or when a non-GNSS satellite has attitude data.

