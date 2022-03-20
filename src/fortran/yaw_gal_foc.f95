SUBROUTINE yaw_gal_foc (mjd, r_sat, v_sat, r_sun, beta_angle, eclipsf, eBX_nom, eBX_ecl, Yangle, Mangle, satbf)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_gal_foc.f03
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
! - beta_angle:	Sun angle with respect to the orbital plane (degrees)
! - satbf:		Body-frame definition type
!				satbf=1 : Body-fixed frame according to the IGS Conventions (GPS Block IIA)  
!				satbf=2 : Body-fixed frame X,Y axes reversal; Case of Galileo and GPS Block IIR 
!
! Output arguments:
! - eclipsf:	non-nominal attitude status flag:
!				0 = Nominal
!				1 = Non-nominal, midnight
!				2 = Non-nominal, noon
! - eBX_nom:	Body-X axis unit vector during noninal yaw-attitude
! - eBX_ecl:	Body-X axis unit vector during eclipsng yaw-attitude (Set to eBX_nom when out of eclipse season) 
! - Yangle: 	Yaw angle array (in degrees) with 2 values: Yaw nominal and Yaw modelled
!				During nominal periods, the two values are equal
! - Mangle:		Orbit angle between satellite and orbit midnight (deg)
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
      REAL (KIND = prec_d) :: mjd, r_sat(3), v_sat(3), r_sun(3), beta_angle
      INTEGER (KIND = 4), INTENT(IN) :: satbf 
	  
! OUT
      REAL (KIND = prec_d) :: Yangle(2)
	  REAL (KIND = prec_d) :: eBX_nom(3), eBX_ecl(3)
      REAL (KIND = prec_d) :: Mangle
      INTEGER (KIND = 4) :: eclipsf
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), Xsun(3)
      REAL (KIND = prec_d) :: pcross(3), pcross2(3), pdot
      REAL (KIND = prec_d) :: SVBCOS
      INTEGER (KIND = 4) :: satblk
	  REAL (KIND = prec_d) :: Yangle_nom, Yangle_ecl
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
	  REAL (KIND = prec_d) :: epsilon_angle, epsilon_dot, epsilon_0
	  REAL (KIND = prec_d) :: epsilon_limit, beta_limit 
	  REAL (KIND = prec_d) :: delta_t, yaw_t0, yaw_t0_sign 


! ----------------------------------------------------------------------
! Body-frame definition type
if (satbf == 1) then
! IGS Conventions definition
   satblk = 6 
elseif (satbf == 2) then
! Galileo body-fixed frame has same orientation with the GPS Block IIR satellites
   satblk = 5 
Else
   Print *,"Error in input argument 'satbf': Galileo Body-frame type"
end if
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
! SVBCOS:	Cosine of the angle E
! E:		Angle between the satellite position vector and the sun position vector
! cos(E) is computed as the dot product between the unit vectors of those position vectors 
! ----------------------------------------------------------------------
      CALL productdot (Xsat, Xsun , pdot)
      SVBCOS = pdot
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Body-frame X-axis unit vector
! eBX_nom = - ( (Xsun x Xsat) x Xsat )  							
! ----------------------------------------------------------------------
      CALL productcross (Xsun,Xsat , pcross)
      CALL productcross (pcross,Xsat , pcross2 )
      eBX_nom = -1.0D0 * pcross2	  
  
if (satbf == 2) then
! Galileo body-fixed frame has same orientation with the GPS Block IIR satellites
! Galileo: Body-X unit vector reversal
      eBX_nom = -1.0D0 * eBX_nom
end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Satellite orbital angle (m)
! ----------------------------------------------------------------------
! Arguments to be removed from the yaw_nom.f90 subroutine
ANOON  = 0.0D0
ANIGHT = 0.0D0

! Mangle (in degrees)
Call yaw_nom (eBX_nom, v_sat, beta_angle, satblk, SVBCOS, ANOON, ANIGHT, Mangle, Yangle_nom) 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Galileo Attitude Law as provided by the European GNSS Service Centre (GSC) web page 
! https://www.gsc-europa.eu/support-to-developers/galileo-iov-satellite-metadata#top
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Galileo yaw steering law for FOC satellites
! ----------------------------------------------------------------------
! Nominal yaw angle (in degrees)
CALL yaw_gal_foc_nom (mjd, r_sat, v_sat, r_sun, satbf, Yangle_nom)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Galileo non-nominal yaw attitude
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Conditions limits for modified yaw-steering attitude
! ----------------------------------------------------------------------
! Beta angle (Sun elevation angle) limit
beta_limit = 4.1D0

! Colinearity angle: epsilon limit
epsilon_limit = 10.0D0
! ----------------------------------------------------------------------

! Colinearity angle computation
CALL colinearity_angle (r_sat, v_sat, r_sun, epsilon_angle, epsilon_dot)

! ----------------------------------------------------------------------
! Conditions 
GALecl = .FALSE. 	 
If ( ( beta_angle    < beta_limit ) .and. &
     ( ABS(epsilon_angle) < epsilon_limit ) ) THEN

     GALecl = .TRUE. 	 

END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
IF (GALecl .EQV. .TRUE.) THEN

epsilon_0 = epsilon_limit
!IF ( epsilon_angle < 0.0D0) THEN
!	epsilon_0 = -1.0D0 * epsilon_0
!END IF

! Colinearity angle prediction at epoch t0 (start of non-nominal period)
CALL colinear_pred (mjd, r_sat, v_sat, r_sun, epsilon_0, delta_t)

! Yaw angle prediction at epoch t0
CALL yaw_pred (mjd, r_sat, v_sat, delta_t, satbf, yaw_t0)

yaw_t0_sign = yaw_t0 / ABS(yaw_t0)
Yangle_ecl = 90.0D0 * yaw_t0_sign + (yaw_t0 - 90.0D0 * yaw_t0_sign) * cos(2* PI_global * delta_t / 5656.D0)

! ----------------------------------------------------------------------
! Eclipse status flag 
! Midnight
   if ( abs(Mangle) <= 90.0D0 ) eclipsf = 1
! Noon
   if ( abs(Mangle) >  90.0D0 ) eclipsf = 2     
! ----------------------------------------------------------------------   

ELSE

! Eclipse status flag
eclipsf = 0  

! Return Yangle nominal when the eclipse conditions are not fullfilled
Yangle_ecl = Yangle_nom

END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Yaw angle values: Nominal and Modified (eclipsing)
Yangle(1) = Yangle_nom
Yangle(2) = Yangle_ecl
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Body-fixed X-axis in ICRF based on the eclipsing yaw angle 
! ----------------------------------------------------------------------
If (eclipsf > 0) THEN

! Body-fixed X-axis rotation about body-fixed frame Z-axis

! Rotation matrix: Inertial/Orbital frame to Body-fixed frame
!CALL crf_bff (r_sat, v_sat, Yangle_ecl, Rcrf_bff, Rrtn_bff)

delta_Yaw = Yangle_ecl - Yangle_nom

! Rz(delta-yaw) rotation matrix: 
delta_Yaw_rad = delta_Yaw * (PI_global / 180.0D0)	  
Rz_yaw(1,1:3) = (/  cos(delta_Yaw_rad),  sin(delta_Yaw_rad),  0.0D0 /)
Rz_yaw(2,1:3) = (/ -sin(delta_Yaw_rad),  cos(delta_Yaw_rad),  0.0D0 /)
Rz_yaw(3,1:3) = (/        0.0D0,            0.0D0,  			1.0D0 /)

! Apply rotation matrix to the Body-fixed X-axis unit vector
eBX_ecl = MATMUL(Rz_yaw, eBX_nom)

Else

eBX_ecl = eBX_nom

End If
! ----------------------------------------------------------------------


END
