SUBROUTINE colinearity_angle (r_sat, v_sat, r_sun, epsilon_angle, epsilon_dot)

! ----------------------------------------------------------------------
! SUBROUTINE: colinearity_angle.f90
! ----------------------------------------------------------------------
! Purpose:
!  Computation of the colinearity angle between the orbital plane and the Sun vector 
! ----------------------------------------------------------------------
! Input arguments:
! - r_sat: 			Satellite position vector (m) 
! - v_sat: 			Satellite velocity vector (m/sec)
! - r_sun:			Sun position vector (m)
!
! Output arguments:
! - epsilon_angle:		Colinearity angle in degrees
! - epsilon_angle:		Colinearity angle partial derivative w.r.t. time 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	24 April 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: r_sat(3), v_sat(3), r_sun(3)
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: epsilon_angle, epsilon_dot
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), dXsat(3), Xsun(3)
      REAL (KIND = prec_q) :: r_norm, v_norm
      REAL (KIND = prec_q) :: rr_dot, vv_dot
      REAL (KIND = prec_q) :: h_angmom(3), h_dot, h_norm
      REAL (KIND = prec_q) :: er(3), et(3), en(3) 
	  REAL (KIND = prec_d) :: sn_dot, rn_cross(3), srn_dot, x_cross(3), y_cross(3) 
	  REAL (KIND = prec_d) :: ry_dot, vy_dot
	  REAL (KIND = prec_d) :: acos_ry


! ----------------------------------------------------------------------
! Unit Vectors
! ----------------------------------------------------------------------
! Xsat:		Satellite position unit vector
      Xsat = (1D0 / sqrt(r_sat(1)**2 + r_sat(2)**2 + r_sat(3)**2) ) * r_sat
	  
! dXsat:	Satellite Velocity unit vector
      dXsat = (1D0 / sqrt(v_sat(1)**2 + v_sat(2)**2 + v_sat(3)**2) ) * v_sat
	  
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
! Colinearity angle: epsilon
! ----------------------------------------------------------------------
CALL productcross(en, Xsun,    x_cross)

CALL productcross(en, x_cross, y_cross)

CALL productdot(Xsat, y_cross, ry_dot)

CALL productdot(dXsat, y_cross, vy_dot)

acos_ry = ACOS( ry_dot ) * (180.0D0 / PI_global)

! Epsilon angle and partial derivative of epsilon w.r.t. time
IF ( acos_ry <= 90.0D0 ) THEN
	epsilon_angle = acos_ry	
	epsilon_dot = (-1.0D0 / sin(epsilon_angle) ) * vy_dot 
	
ELSE
	epsilon_angle = 180.0D0 - acos_ry
	epsilon_dot = (1.0D0 / sin(epsilon_angle) ) * vy_dot 
	
END IF 


END
