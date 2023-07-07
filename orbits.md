

# Orbit Modelling
## Gravitational Accelerations
Satellite propagation involves considering various gravitational accelerations that influence the motion of the satellite.

### Central Force
The central force arises from the gravitational attraction between the satellite and the central body around which it orbits. This force is responsible for keeping the satellite in a stable orbit and determines its trajectory.

### 3rd Body Celestial Accelerations
The third-body celestial accelerations come into play due to the gravitational pull of other celestial bodies. While the central force dominates the motion of the satellite, these additional accelerations, caused by the gravitational interaction with other nearby bodies like the Moon or Sun, contribute to perturbations in the satellite's orbit.

### Spherical Harmonics of the Gravity Field
The spherical harmonics of the gravity field refer to the non-uniform distribution of mass within a celestial body, such as Earth. This non-uniformity causes variations in the gravitational acceleration experienced by the satellite at different points in its orbit.

### Ocean Tide
The ocean tide effect is caused by the gravitational interaction between the satellite and Earth's ocean. As the ocean experiences tidal bulges due to the gravitational pull of the Moon and Sun, the resulting gravitational forces can have a small influence on the satellite's orbit.

### Solid Earth Tide
Similar to the ocean tide, the solid Earth tide results from the gravitational interaction between the satellite and the solid Earth. The gravitational forces induced by the Earth's deformation due to the tidal effects of the Moon and Sun can cause subtle changes in the satellite's orbit.

## Non-Gravitational Accelerations
In addition to gravitational accelerations, non-gravitational accelerations significantly affect satellite propagation.

### Solar Radiation Pressure Acceleration (CannonBall)
The solar radiation pressure acceleration, arises from the pressure exerted by sunlight on the satellite's surface. The momentum transfer from photons to the satellite creates a force that affects the satellite's trajectory. For this release, only the Cannon Ball model is implemented.

### Antenna thrust
A GNSS satellite continuously emitting a RF signal has an acceleration generated due to the emission of the signal. 

### Albedo
The acceleartion due to the albedo occurs when sunlight reflects off the Earth's surface. This reflection imparts a force on the satellite, which can causes a change in its orbit.

## Transformation between Celestial and Terrestrial Reference Systems

The variational equations obtained from the POD need to be transformed into the terrestrial reference frame so that the adjustments can be made in the ECEF frame that the PEA operates in.

\begin{equation}
    [CRS] = Q(t)R(t)W(t)[TRS]
\end{equation}

Where,
$CRS$ is the Celestial Reference System
$TRS$ is the Terrestrial Reference Systems
$Q(t)$ is the Celestial Pole motion (Precession-Nutation) matrix
$R(t)$ is the Earth Rotation matrix
$W(t)$ is the Polar motion matrix

\begin{equation}
Q(t) = 
\begin{bmatrix} 
1-aX^2  & -aXY     & X \\
 -aXY   & 1 - aY^2 & Y \\
 -X     & -Y       & 1-a(X^2+Y^2) 
\end{bmatrix}
\end{equation}

\begin{equation}
R(t) = R_2(-\theta) = 
\begin{bmatrix}
cos \theta & -sin \theta & 0 \\ 
sin \Theta & cos \theta  & 0 \\
0 & 0 1
\end{bmatrix}
\end{equation}
