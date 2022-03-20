SUBROUTINE yaw_pred (mjd, r_sat, v_sat, delta_t, satbf, yaw_angle)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_pred.f03
! ----------------------------------------------------------------------
! Purpose:
!  Computation of the yaw angle (Sun elevation angle w.r.t. orbtial plane) at a previous or future epoch
!  based on the Keplerian elements for the satellite orbit propagation and the DE Ephemeris for the Sun position
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - delta_t:	Time span between current and previous/future epoch (sign defines backward or forward prediciton epoch)
!
! Output arguments:
! - yaw_angle:	Sun elevation angle with respect to the orbital plane at the epoch t0 (degrees) 
! ----------------------------------------------------------------------
! Note 1:
! - kepler:		Kepler elements array (6)
!   a:       	semi-major axis  (m)
!   e:      	eccentricity
!   i:      	inclination (degrees)
!   Omega:  	right ascension of the ascending node (degrees)
!   omega:  	argument of perigee (degrees)
!   E:      	eccentric anomaly (degrees)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	24 April 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_planets
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: mjd, r_sat(3), v_sat(3)
      REAL (KIND = prec_q), INTENT(IN) :: delta_t
      INTEGER (KIND = 4), INTENT(IN) :: satbf 
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: yaw_angle
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: ti, t0, frate
      REAL (KIND = prec_q) :: mjd_t0 , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      INTEGER (KIND = prec_int8) :: Norb, i, sz1, sz2
      REAL (KIND = prec_q) :: GM, kepler_i(9), kepler_0(6), a_semiaxis, ec, n_motion, Ei_rad, Mi_rad, Mo_rad, Mo_deg, Eo_deg
      REAL (KIND = prec_q) :: dt
      REAL (KIND = prec_q), DIMENSION(3) :: rsat_t0, vsat_t0
! ----------------------------------------------------------------------
! Planetary/Lunar Epehemeris data processing (lib3_planets): Variables declaration
      DOUBLE PRECISION  JD_TT, R_eph(6)
      INTEGER  NTARG, NCTR
!      CHARACTER (LEN=100) :: fname_header, fname_data, fname_out
      REAL (KIND = prec_d) :: rsun_t0(3)
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: NPint
      REAL (KIND = prec_d) :: Yangle_nom




! ----------------------------------------------------------------------
! Current epoch in seconds since start of the day (0h)
ti = (mjd - INT(mjd)) * 86400.D0 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Computation of the forward/backward epoch t0
! ----------------------------------------------------------------------
! Orbital angle rate (in degrees/sec)
!frate = sqrt( (v_sat(1)**2+v_sat(2)**2+v_sat(3)**2) / (r_sat(1)**2+r_sat(2)**2+r_sat(3)**2) ) * (180.0D0 / PI_global)

!t0 = ti + (ft0 - fti)/ frate
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Computation of the satellite position and velocity vectors at epoch t0
! through Keplerian orbit consideration
! ----------------------------------------------------------------------
! Gravitational constant
GM = 0.39860044150D+15

! Kepler elements at ti
CALL kepler_z2k (r_sat, v_sat, GM, kepler_i)

a_semiaxis = kepler_i(1)
ec = kepler_i(2)

! Eccentric anomaly at ti (radians)
Ei_rad = kepler_i(7) * (PI_global / 180.D0)

! Mean anomaly  at ti (radians)
Mi_rad = Ei_rad - ec * sin(Ei_rad)

! mean motion (rad/sec)
n_motion = sqrt(GM / a_semiaxis**3)


! ----------------------------------------------------------------------
! Time difference since current epoch
!dt = t0 - ti
dt = delta_t
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Mean anomaly (radians) at epoch t0 
Mo_rad = Mi_rad + n_motion * dt

! Mean anomaly (in degrees) at epoch t0
Mo_deg = Mo_rad * (180.D0/PI_global)

! Angle reduction within the range {0 - 360} degrees
If (abs(Mo_deg) >= 360.D0) then
	Mo_deg = Mo_deg - INT(Mo_deg / 360.D0) * 360.D0
End If

if (Mo_deg < 0.0D0) then
    Mo_deg = Mo_deg + 360.D0
end if
! ----------------------------------------------------------------------


! Kepler Equation 
! Eccentric anomaly (degrees) at epoch t0 obtained by solving Kepler's Equation
CALL kepler_eq ( Mo_deg , ec , Eo_deg)

! Kepler elements at epoch t0
!kepler_0 = (/ a, e, i, Omega_asc, omega_per, Eo_deg /)
kepler_0 = (/ kepler_i(1), kepler_i(2), kepler_i(3), kepler_i(4), kepler_i(5), Eo_deg /)

! State vector (Cartesian coordinates) at epoch t0
CALL kepler_k2z (kepler_0, GM, rsat_t0, vsat_t0) 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Sun position vector at reference epoch t0
! ----------------------------------------------------------------------
! Center celestial body : Earth
      NCTR = 3 
! Target celestial body : Sun
      NTARG = 11 
	  
! Time System transformation: GPS to TT
mjd_t0 = mjd + dt/86400.0D0		
CALL time_GPS (mjd_t0 , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)

! JD at the epoch t0 (in TT)
      JD_TT = mjd_TT + 2400000.5D0

! Sun state vector in ICRF (in KM)
      CALL  PLEPH ( JD_TT, NTARG, NCTR, R_eph )

	  ! Km to meters
	  rsun_t0(1) = R_eph(1) * 1000D0
	  rsun_t0(2) = R_eph(2) * 1000D0
	  rsun_t0(3) = R_eph(3) * 1000D0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Sun elevation angle at epoch t0
! ----------------------------------------------------------------------
! Beta angle (in degrees)
!      CALL beta_angle (rsat_t0, vsat_t0, rsun_t0, beta)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Yaw angle nominal
CALL yaw_gal_foc_nom (rsat_t0, vsat_t0, rsun_t0, satbf, Yangle_nom)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Yaw angle at t0
yaw_angle = Yangle_nom
! ----------------------------------------------------------------------

END
