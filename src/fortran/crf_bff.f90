SUBROUTINE crf_bff (r_CRS, v_CRS, Yangle, Rcrf_bff, Rrtn_bff)


! ----------------------------------------------------------------------
! SUBROUTINE: crf_bff.f90
! ----------------------------------------------------------------------
! Purpose:
!  Transformation between inertial frame and body-fixed frame
!  Transformation between orbital frame and body-fixed frame 
! ----------------------------------------------------------------------
! Input arguments:
! - r_sat: 			Satellite position vector (m) 
! - v_sat: 			Satellite velocity vector (m/sec)
! - Yangle:			Yaw angle (degrees)
!
! Output arguments:
! - Rcrf_bff:		Transformation matrix: Inertial frame (GCRF) to Body-fixed frame
! - Rrtn_bff:		Transformation matrix: Orbital frame to Body-fixed frame
! ----------------------------------------------------------------------
! Thomas D. Papanikolaou, Geoscience Australia                 June 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: r_CRS(3), v_CRS(3)
      REAL (KIND = prec_d), INTENT(IN) :: Yangle
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Rcrf_bff(3,3), Rrtn_bff(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Rcrf_rtn(3,3), Drtn_bff(3,3), Rz_yaw(3,3)
      REAL (KIND = prec_d) :: Rtest(3,3)
      REAL (KIND = prec_d) :: Yangle_rad
      INTEGER (KIND = prec_int4) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------
	  

	  
! ----------------------------------------------------------------------
! Transformation matrix: Inertial frame (GCRF) to Body-fixed frame
! ----------------------------------------------------------------------

! Rcrf_bff = Rz_yaw * Drtn_bff * Rcrf_rtn

! Xbff = Rz_yaw * Drtn_bff * Rcrf_rtn * Xcrf = Rz_yaw * Drtn_bff * Xrtn


! ----------------------------------------------------------------------
! Inertial (GCRF) to Orbital Frame
! ----------------------------------------------------------------------
! RTN: Radial, Along (Tangential), Cross (Normal)
! Xrtn = Rcrf_rtn * Xcrf
      CALL orb_frame(r_CRS, v_CRS, Rcrf_rtn)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Orbital frame to body-fixed frame
! ----------------------------------------------------------------------

! Rotation matrix for the axes directions correspondence: RTN to TNR to XYZ axes
! For yaw = 0
! eBF_x =  eT
! eBF_y = -eN
! eBF_z = -eR
      Drtn_bff (1,1:3) = (/  0.0D0,  1.0D0,  0.0D0  /) 
      Drtn_bff (2,1:3) = (/  0.0D0,  0.0D0, -1.0D0  /) 
      Drtn_bff (3,1:3) = (/ -1.0D0,  0.0D0,  0.0D0  /) 

	  
! Rz(yaw) rotation matrix: 
      Yangle_rad = Yangle * (PI_global / 180.0D0)
	  
      Rz_yaw(1,1:3) = (/  cos(Yangle_rad),  sin(Yangle_rad),  0.0D0 /)
      Rz_yaw(2,1:3) = (/ -sin(Yangle_rad),  cos(Yangle_rad),  0.0D0 /)
      Rz_yaw(3,1:3) = (/            0.0D0,            0.0D0,  1.0D0 /)
! ----------------------------------------------------------------------


! Arrays multiplication

! ----------------------------------------------------------------------
! Allocatable arrays
!      ALLOCATE (R1(3,3), STAT = AllocateStatus)
!      ALLOCATE (R2(3,3), STAT = AllocateStatus)
!      ALLOCATE (R3(3,3), STAT = AllocateStatus)
!      IF (AllocateStatus /= 0) THEN
!         PRINT *, "Error: Not enough memory"
!         PRINT *, "Error: SUBROUTINE crf_bff.f90"
!         STOP "*** Not enough memory ***"
!      END IF  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Orbital to body-fixed frame
      !R1 = Rz_yaw
      !R2 = Drtn_bff
      !CALL matrixRxR (R1, R2, R3)								
      !Rrtn_bff = R3
      ! matrix_RxR equivalent to MATMUL 
      Rrtn_bff = MATMUL(Rz_yaw, Drtn_bff)
	  
! Inertial to body-fixed frame
      !R1 = Rrtn_bff
      !R2 = Rcrf_rtn
      !CALL matrix_RxR (R1, R2, R3)
      !Rcrf_bff = R3
      ! matrix_RxR equivalent to MATMUL 
      Rcrf_bff = MATMUL(Rrtn_bff, Rcrf_rtn)
      !Rcrf_bff = MATMUL(Rcrf_rtn,Rrtn_bff)
! ----------------------------------------------------------------------
 

END Subroutine
