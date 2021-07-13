SUBROUTINE beta_angle (r_sat, v_sat, r_sun, beta)

! ----------------------------------------------------------------------
! SUBROUTINE: beta_angle.f90
! ----------------------------------------------------------------------
! Purpose:
!  Computation of Sun angle with respect to the orbital plane
! ----------------------------------------------------------------------
! Input arguments:
! - r_sat: 			Satellite position vector (m) 
! - v_sat: 			Satellite velocity vector (m/sec)
! - r_sun:			Sun position vector (m)
!
! Output arguments:
! - beta:			Sun angle with the orbital plane in degrees (see Note 1)
! ----------------------------------------------------------------------
! Note 1:
!  Beta angle is computed based on the input vectors in inertial system
!  Sign conventions for beta angle:
!   Postiive: above orbital plane
!   Negative: below orbital plane  	
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia             June 2016
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
      REAL (KIND = prec_d), INTENT(OUT) :: beta
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Xsat(3), dXsat(3), Xsun(3), dERA
      REAL (KIND = prec_d) :: pcross(3), pdot, beta_rad
      REAL (KIND = prec_d) :: eV_i(3)
      INTEGER (KIND = prec_int4) :: eV_opt 
! ----------------------------------------------------------------------


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
! Inertial geocentric satellite velocity unit vector
! ----------------------------------------------------------------------
      eV_opt = 1 
      !PRINT *,'eV_opt',eV_opt 

	  
      if (eV_opt == 1) then  
! ----------------------------------------------------------------------
! 1. Satellite Velocity unit vector in ICRF (obtained from orbit integration)
      eV_i = (1D0 / sqrt(v_sat(1)**2 + v_sat(2)**2 + v_sat(3)**2) ) * v_sat
! ----------------------------------------------------------------------


      elseif (eV_opt == 2) then
! ----------------------------------------------------------------------
! 2. Approximate value used in Kouba (2009), Eq.(3)
! ----------------------------------------------------------------------
! Earth angular velocity  |  IERS 2010 constants: omega = 7.292115D-05
! Omega as derivative of the Earth Rotation Angle (ERA), dERA/dt in rad/s
      dERA = 2.0D0 * PI_global * 1.00273781191135448D0 * (1.0D0/86400.0D0)
! ----------------------------------------------------------------------
      eV_i(1) = dXsat(1) - dERA * Xsat(2)  														 
      eV_i(2) = dXsat(2) + dERA * Xsat(1)  														 
      eV_i(3) = dXsat(3)  														 
! ----------------------------------------------------------------------
      end if
! ----------------------------------------------------------------------

	  
	  
! ----------------------------------------------------------------------
! BETA : Sun angle with the orbtial plane
! ----------------------------------------------------------------------
      CALL productcross (eV_i , Xsat , pcross)	! -Normal (antiparallel) 
      !CALL productcross (Xsat , eV_i , pcross)	! +Normal  
      CALL productdot (pcross , Xsun , pdot)				
	  
      !beta_rad = acos (pdot) - PI_global						! Kouba (2009), Eq.(2)
      beta_rad = acos(pdot) - PI_global/2.0D0					! correction to Eq.(2): pi has been changed to pi/2 
		  
      ! Degrees
      beta = beta_rad * (180.0D0 / PI_global)  	  
  
	  
END
