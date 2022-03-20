SUBROUTINE orbk_frame (r,v, Rrtn)


! ----------------------------------------------------------------------
! SUBROUTINE: orbk_frame.f90
! ----------------------------------------------------------------------
! Purpose:
!  Rotation matrix from inertial to orbital frame
! ----------------------------------------------------------------------
! Input arguments:
! - r: 			position vector (m) in inertial frame 
! - v: 			velocity vector (m/sec) in inertial frame
!
! Output arguments:
! - Rrtn : 		Rotation matrix from inertial to orbital frame
! ----------------------------------------------------------------------
! Note 1:
!  Orbital frame components:
!  radial, along-track (tangential) and cross-track (normal)  [r t n]
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia           10 May 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: r(3), v(3)
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: Rrtn(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: kepler(9), GM, i_deg, Omega_asc_deg, omega_per_deg
      REAL (KIND = prec_q) :: deg2rad
      REAL (KIND = prec_q) :: Rz_Omega_asc(3,3), Rz_omega_per(3,3), Rx_i(3,3), R12(3,3)
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus, pflag
! ----------------------------------------------------------------------


      deg2rad = PI_global / 180.D0

      GM = GM_gfm

      ! Kepler elements	  
      CALL kepler_z2k (r, v , GM, kepler)	  

      i_deg = kepler(3)
      Omega_asc_deg = kepler(4)
	  omega_per_deg = kepler(5)


! Rotation matrix for Omega (right ascension of the ascending node)
      Rz_Omega_asc(1,1:3) = (/  cos(Omega_asc_deg * deg2rad),  sin(Omega_asc_deg * deg2rad),	0.D0 /)
      Rz_Omega_asc(2,1:3) = (/ -sin(Omega_asc_deg * deg2rad),  cos(Omega_asc_deg * deg2rad),	0.D0 /)
      Rz_Omega_asc(3,1:3) = (/ 				0.D0, 							0.D0, 			1.D0 /)

! Rotation matrix for omega (argument of perigee)
      Rz_omega_per(1,1:3) = (/  cos(omega_per_deg * deg2rad),  sin(omega_per_deg * deg2rad),	0.D0 /)
      Rz_omega_per(2,1:3) = (/ -sin(omega_per_deg * deg2rad),  cos(omega_per_deg * deg2rad),	0.D0 /)
      Rz_omega_per(3,1:3) = (/ 				0.D0, 							0.D0, 				1.D0 /)

! Rotation matrix for orbit plane's inclination (i)
      Rx_i(1,1:3) = (/ 1.D0,			0.D0,					0.D0 		 /)
      Rx_i(2,1:3) = (/ 0.D0,	 cos(i_deg * deg2rad),	sin(i_deg * deg2rad) /)
      Rx_i(3,1:3) = (/ 0.D0,	-sin(i_deg * deg2rad),	cos(i_deg * deg2rad) /)


! ----------------------------------------------------------------------
! Rotation matrix from Inertial to orbital frame			
! Rrtn = Rz_omega_per * Rx_i * Rz_Omega_asc
! ----------------------------------------------------------------------
      ALLOCATE (R1(3,3), STAT = AllocateStatus)
      ALLOCATE (R2(3,3), STAT = AllocateStatus)
      ALLOCATE (R3(3,3), STAT = AllocateStatus)
	  
      R1 = Rz_omega_per
      R2 = Rx_i
      CALL matrix_RxR

      R12 = R3
	  
      DEALLOCATE (R1, STAT = DeAllocateStatus)
      DEALLOCATE (R2, STAT = DeAllocateStatus)
      DEALLOCATE (R3, STAT = DeAllocateStatus)
! ----------------------------------------------------------------------

      ALLOCATE (R1(3,3), STAT = AllocateStatus)
      ALLOCATE (R2(3,3), STAT = AllocateStatus)
      ALLOCATE (R3(3,3), STAT = AllocateStatus)
	  
      R1 = R12
      R2 = Rz_Omega_asc
      CALL matrix_RxR

	  Rrtn = R3

      DEALLOCATE (R1, STAT = DeAllocateStatus)
      DEALLOCATE (R2, STAT = DeAllocateStatus)
      DEALLOCATE (R3, STAT = DeAllocateStatus)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
Rrtn(1,1) =  cos(Omega_asc_deg * deg2rad) * cos(omega_per_deg * deg2rad) &
            -sin(Omega_asc_deg * deg2rad) * cos(i_deg * deg2rad) * sin(omega_per_deg * deg2rad)

Rrtn(1,2) =  sin(Omega_asc_deg * deg2rad) * cos(omega_per_deg * deg2rad) &	   
            +cos(Omega_asc_deg * deg2rad) * cos(i_deg * deg2rad) * sin(omega_per_deg * deg2rad) 

Rrtn(1,3) =  sin(i_deg * deg2rad) * sin(omega_per_deg * deg2rad)
	   

Rrtn(2,1) = -cos(Omega_asc_deg * deg2rad) * sin(omega_per_deg * deg2rad) &
            -sin(Omega_asc_deg * deg2rad) * cos(i_deg * deg2rad) * cos(omega_per_deg * deg2rad)

Rrtn(2,2) = -sin(Omega_asc_deg * deg2rad) * sin(omega_per_deg * deg2rad) &	   
            +cos(Omega_asc_deg * deg2rad) * cos(i_deg * deg2rad) * cos(omega_per_deg * deg2rad) 

Rrtn(2,3) =  sin(i_deg * deg2rad) * cos(omega_per_deg * deg2rad)


Rrtn(3,1) =  sin(Omega_asc_deg * deg2rad) * sin(i_deg * deg2rad)
Rrtn(3,2) = -cos(Omega_asc_deg * deg2rad) * sin(i_deg * deg2rad)
Rrtn(3,3) =  cos(i_deg * deg2rad)
! ----------------------------------------------------------------------

  

end



