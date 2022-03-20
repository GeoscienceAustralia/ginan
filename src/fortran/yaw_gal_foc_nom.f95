SUBROUTINE yaw_gal_foc_nom (r_sat, v_sat, r_sun, satbf, Yangle_nom)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_gal_foc_nom.f03
! ----------------------------------------------------------------------
! Purpose:
!  Galileo yaw-attitude based on the yaw-steering law provided by the 
!  European GNSS Service Center  
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number in GPS Time (including the fraction of the day)
! - r_sat: 		Satellite position vector (m) in ITRF
! - v_sat: 		Satellite velocity vector (m/sec) in ITRF
! - r_sun:		Sun position vector (m) in ITRF
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
!
! Output arguments:
! - Yangle_nom: Yaw angle based on nominal attitude law (in degrees) 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	15 April 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
      
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: r_sat(3), v_sat(3), r_sun(3)
      INTEGER (KIND = 4), INTENT(IN) :: satbf 
	  
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Yangle_nom
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), Xsun(3)
      REAL (KIND = prec_d) :: pcross(3), pcross2(3), pdot
      REAL (KIND = prec_d) :: SVBCOS
      INTEGER (KIND = 4) :: satblk
      REAL (KIND = prec_d) :: eta
      REAL (KIND = prec_d) :: So(3), Sh(3)
      REAL (KIND = prec_d) :: betaX, betaY
      REAL (KIND = prec_d) :: Gama, Soy_to, ft0, fti, ftE, beta_to, beta_te
      LOGICAL GALecl
      LOGICAL NOON, NIGHT
      REAL (KIND = prec_d) :: ANOON, ANIGHT
      REAL (KIND = prec_d) :: delta_Yaw, delta_Yaw_rad
      REAL (KIND = prec_d) :: Rz_yaw(3,3)
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: r_norm, v_norm
      REAL (KIND = prec_q) :: rr_dot, vv_dot
      REAL (KIND = prec_q) :: h_angmom(3), h_dot, h_norm
      REAL (KIND = prec_q) :: er(3), et(3), en(3) 
	  REAL (KIND = prec_d) :: sn_dot, rn_cross(3), srn_dot
! ----------------------------------------------------------------------

 
! ----------------------------------------------------------------------
! Unit Vectors
! ----------------------------------------------------------------------
! Xsat:		Satellite position unit vector
      Xsat = (1D0 / sqrt(r_sat(1)**2 + r_sat(2)**2 + r_sat(3)**2) ) * r_sat
	  
! Xsun:		Sun position unit vector
      Xsun = (1D0 / sqrt(r_sun(1)**2 + r_sun(2)**2 + r_sun(3)**2) ) * r_sun
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Orbit normal vector (cross-track)
! ----------------------------------------------------------------------
! cross product of r,v : angular momentum per unit mass
      CALL productcross(r_sat, v_sat, h_angmom)
      CALL productdot(h_angmom, h_angmom, h_dot)
      h_norm = sqrt(h_dot)

! Cross-track or normal component
     en = (1D0 / h_norm) * h_angmom
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Galileo Attitude Law as provided by the European GNSS Service Centre (GSC) web page 
! https://www.gsc-europa.eu/support-to-developers/galileo-iov-satellite-metadata#top
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Galileo yaw steering law for FOC satellites
! ----------------------------------------------------------------------
! Nominal yaw angle (in degrees)

!Yangle_nom = ATAN2( s(t)*n(t) , s(t)*r(t) x n(t) )
CALL productdot(Xsun, en, sn_dot)
CALL productcross(Xsat, en, rn_cross)
CALL productdot(Xsun, rn_cross, srn_dot)
Yangle_nom = ATAN2( sn_dot , srn_dot ) * (180.0D0 / PI_global)

! IGS Conventions
IF (satbf == 1) then
	Yangle_nom = ATAN2( -1.0D0 * sn_dot , -1.0D0 * srn_dot ) * (180.0D0 / PI_global)
END IF
! ----------------------------------------------------------------------

END
