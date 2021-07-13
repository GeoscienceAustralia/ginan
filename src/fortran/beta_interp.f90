SUBROUTINE beta_interp (mjd, r_sat, v_sat, fti, ft0, NPint, beta)


! ----------------------------------------------------------------------
! SUBROUTINE: beta_interp.f90
! ----------------------------------------------------------------------
! Purpose:
!  Computation of the beta angle (Sun elevation angle w.r.t. orbtial plane) at a previous of future epoch
!  Orbit numerical interpolation is applied for the computing the orbit point at the input epoch 
!  Planetary/Lunar DE Ephemeris data processing is performed for the Sun position computation at the input epoch
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - fti:		Orbit angle (degrees) at the input epoch ti 
! - ft0:		Orbit angle (degrees) at the required epoch t0 (forward or backward)
! - NPint:		Number of data points to be used for numerical interpolation (defines the order of the polynomial)
!
! Output arguments:
! - beta:		Sun elevation angle with respect to the orbital plane at the epoch t0 (degrees) 
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
! Dr. Thomas Papanikolaou
! Cooperative Research Centre for Spatial Information, Australia
! Created: 27 June 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_planets
      USE mdl_eop
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: mjd, r_sat(3), v_sat(3)
      REAL (KIND = prec_q), INTENT(IN) :: fti, ft0
      INTEGER (KIND = prec_int8), INTENT(IN) :: NPint
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: beta
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: ti, t0, frate
      REAL (KIND = prec_q) :: mjd_t0 , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      INTEGER (KIND = prec_int8) :: Norb, i, sz1, sz2
      REAL (KIND = prec_q) :: GM, kepler_i(6), kepler_0(6), a_semiaxis, ec, n_motion, Ei_rad, Mi_rad, Mo_rad, Mo_deg, Eo_deg
      REAL (KIND = prec_q) :: dt
      REAL (KIND = prec_q), DIMENSION(3) :: rsat_t0, vsat_t0
      REAL (KIND = prec_d) :: r_TRS(3), v_TRS(3), r_CRS(3), v_CRS(3), v_CRS_1(3), v_CRS_2(3)
	  DOUBLE PRECISION EOP_cr(7)
      DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)	  
! ----------------------------------------------------------------------
! Planetary/Lunar Epehemeris data processing (lib3_planets): Variables declaration
      DOUBLE PRECISION  JD_TT, R_eph(6)
      INTEGER  NTARG, NCTR
!      CHARACTER (LEN=100) :: fname_header, fname_data, fname_out
      REAL (KIND = prec_d) :: rsun_t0(3)
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Current epoch in seconds since start of the day (0h)
ti = (mjd - INT(mjd)) * 86400.D0 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Computation of the forward/backward epoch t0
! ----------------------------------------------------------------------
! Orbital angle rate (in degrees/sec)
frate = sqrt( (v_sat(1)**2+v_sat(2)**2+v_sat(3)**2) / (r_sat(1)**2+r_sat(2)**2+r_sat(3)**2) ) * (180.0D0 / PI_global)

t0 = ti + (ft0 - fti)/ frate
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Time difference (in seconds) since current epoch
dt = t0 - ti
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Time System transformation: GPS to TT
! ----------------------------------------------------------------------
! MJD at the epoch t0
mjd_t0 = mjd + dt/86400.0D0

! Time transformation at epoch t0		
CALL time_GPS (mjd_t0 , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Sun position vector at reference epoch t0
! ----------------------------------------------------------------------
! Center celestial body : Earth
      NCTR = 3 
! Target celestial body : Sun
      NTARG = 11 

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
! Orbit interpolation for the epoch t0 (ITRF)
Call interp_orb3 (mjd_GPS, NPint, rsat_t0, vsat_t0)
! ----------------------------------------------------------------------
r_TRS = rsat_t0
v_TRS = vsat_t0

! ----------------------------------------------------------------------
! Tranformation: ITRF to ICRF
CALL EOP (mjd_TT, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)	  

! r_CRS = TRS2CRS * r_TRS
CALL matrix_Rr (TRS2CRS, r_TRS , r_CRS)
	  
! v_CRS = TRS2CRS * v_TRS + d_TRS2CRS * r_TRS
CALL matrix_Rr (TRS2CRS, v_TRS , v_CRS_1)
CALL matrix_Rr (d_TRS2CRS, r_TRS , v_CRS_2)
v_CRS = v_CRS_1 + v_CRS_2
! ----------------------------------------------------------------------
rsat_t0 = r_CRS
vsat_t0 = v_CRS


! ----------------------------------------------------------------------
! Beta angle (Sun elevation angle) at epoch t0 (in degrees)
CALL beta_angle (rsat_t0, vsat_t0, rsun_t0, beta)
! ----------------------------------------------------------------------



if (1<0) then
PRINT *, "rsat   ", r_sat
PRINT *, "vsat   ", v_sat

PRINT *, "fti,ft0,frate", fti, ft0, frate
PRINT *, "ti, t0, dt", ti, t0, dt
PRINT *, "frate ", frate

PRINT *, "beta", beta
PRINT *, "rsun_t0", rsun_t0
PRINT *, "rsat_t0", rsat_t0
PRINT *, "vsat_t0", vsat_t0

PRINT *, "dt    ", dt
PRINT *, "t0    ", t0
PRINT *, "ti    ", ti
PRINT *, "mjd    ", mjd
PRINT *, "mjd_t0 ", mjd_t0
PRINT *, "mjd_GPS", mjd_GPS

end if



END
