SUBROUTINE yaw_angle (e_BX, v_sat, beta, satblk, Yangle)


! ----------------------------------------------------------------------
! SUBROUTINE: yaw_angle.f90
! ----------------------------------------------------------------------
! Purpose:
!  Computation of the Yaw angle based on the Body-X unit vector 
! (as computed by the eclips.f subroutine) and the satellite velocity vector.
!
!  The sign of the Yaw angle follows the convections of the eclips.f 
! ----------------------------------------------------------------------
! Input arguments:
! - eB_X: 			Satellite Body-X unit vector (m) 
! - v_sat: 			Satellite velocity vector (m/sec)
! - r_sun:			Sun position vector (m)
! - beta:			Sun angle with the orbital plane in degrees (see Note 1)
! - satblk:			Satellite Block series number 1=I, 2=II, 3=IIA, IIR=(4,5) IIF=6
!
! Output arguments:
! - Yangle:			Yaw angle (in degrees)
! ----------------------------------------------------------------------
! Note 1:
!  Beta angle is computed by the beta_angle.f90 subroutine 
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
      REAL (KIND = prec_d), INTENT(IN) :: e_BX(3), v_sat(3), beta
      !INTEGER (KIND = prec_int4), INTENT(IN) :: satblk
      INTEGER, INTENT(IN) :: satblk

! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Yangle
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Yaw angle computation (in degrees)
! ----------------------------------------------------------------------

      Yangle = acos( (e_BX(1) * v_sat(1) + e_BX(2) * v_sat(2) + e_BX(3) * v_sat(3)) &
	                  / sqrt(v_sat(1)**2 + v_sat(2)**2 + v_sat(3)**2) ) &
					 * (180D0 / PI_global)


! Yaw angle sign (according to eclips.f convenctions)


! IIR YANGLE has the same sign as beta, II/IIA has the opposite sign
      if (beta .LT. 0.d0 .AND. satblk .GE. 4 .AND. satblk .LE. 5) then
          Yangle = -Yangle
      end if
	 
      if (beta .GT. 0.d0 .AND. satblk .NE. 4 .AND. satblk .NE. 5) then
          Yangle = -Yangle
      end if
! ----------------------------------------------------------------------
 
  
END
