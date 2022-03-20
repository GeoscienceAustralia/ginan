MODULE m_attitude_orb


! ----------------------------------------------------------------------
! MODULE: m_attitude_orb.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the attitude_orb subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 December 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
SUBROUTINE attitude_orb (orbits_partials_itrf, orbits_partials_icrf, PRN_array, BLOCK_array, attitude_array)


! ----------------------------------------------------------------------
! SUBROUTINE: attitude_orb
! ----------------------------------------------------------------------
! Purpose:
!  Orbit (position vector only) transformation from terrestrial (ITRF) to inertial (ICRF) reference frame 
! ----------------------------------------------------------------------
! Input arguments:
! - orbits_partials_icrf:	Satellite Orbits and Partial derivatives of the estimated parameters in inertial frame (ICRF) per satellite per epoch:
!   						Matrix Dimensions ixjxk
! 							i: Epochs | j: Orbit elements | k: Satellites
!							i=1:Nepochs, j=1:max, k=1:Nsatellites	
!							k=1	:: Satellite 1
! 							i=1 :: Epoch 1
! 							j=1:max :: Row i Format:
!               			- Modified Julian Day number (including the fraction of the day) 
!							- Seconds since 00h 
!							- Position vector (m)
!							- Velocity vector (m/sec)
! 							- Partial Derivatives
! - orbits_partials_itrf:   Satellite Orbits and Partial derivatives of the estimated parameters in inertial frame (ITRF) per satellite per epoch:
!   						Matrix Dimensions ixjxk :: Format as orbits_partials_icrf matrix
! - PRN_array:				PRN numbers array e.g. G01, .., G32, E01, .., E30
! - BLOCK_array:			Block type array e.g. GPS-IIA 
!
! Output arguments:
! - attitude_array:			Satellite attitude matrix per satellite per epoch:
!   						Matrix Dimensions ixlxk
!               			- Modified Julian Day number in the input time scale (including the fraction of the day) 
!							- Seconds since 00h in the input time scale
!							- Position vector (m)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 December 2019
! ----------------------------------------------------------------------
 

      USE mdl_precision
!      USE mdl_num
!      USE mdl_param
      !USE m_read_satsnx
      !USE m_satmetadata
      use pod_yaml
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(IN) :: PRN_array(:)
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: orbits_partials_icrf  
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: orbits_partials_itrf  
      CHARACTER (LEN=20), ALLOCATABLE, INTENT(IN) :: BLOCK_array(:)
      !CHARACTER (LEN=100), INTENT(IN) :: satsinex_filename
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE, INTENT(OUT) :: attitude_array  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, Nparam, Nsat, Natt 
      INTEGER (KIND = prec_int8) :: i_epoch, i_sat
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3
      INTEGER (KIND = prec_int8) :: i, j
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
	  CHARACTER (LEN=3) :: PRN_GNSS, PRN_isat
      CHARACTER (LEN=20) :: BLOCK_isat
	  REAL (KIND = prec_d) :: mjd, Sec_00, mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      REAL (KIND = prec_d) :: rsat_icrf(3), vsat_icrf(3)
      INTEGER (KIND = 4) :: eclipse_status
      REAL (KIND = prec_d) :: Yangle_array(2)
      REAL (KIND = prec_d) :: Rtrf2bff(3,3)
      REAL (KIND = prec_q) :: Quaternions_trf2bff(4)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Orbit arrays dimensions
sz1 = SIZE (orbits_partials_icrf,DIM=1)
sz2 = SIZE (orbits_partials_icrf,DIM=2)
sz3 = SIZE (orbits_partials_icrf,DIM=3)

Nepochs = sz1
Nparam  = sz2
Nsat    = sz3

! Dynamic allocatable array
Natt = 18
ALLOCATE (attitude_array(Nepochs,Natt,Nsat), STAT = AllocateStatus)

! ----------------------------------------------------------------------
DO i_epoch = 1 , Nepochs
	DO i_sat = 1 , Nsat
        if (.not. yml_satellites(i_sat)) cycle

PRN_isat = PRN_array(i_sat)
BLOCK_isat = BLOCK_array(i_sat)

mjd    = orbits_partials_icrf(i_epoch,1,i_sat)
Sec_00 = orbits_partials_icrf(i_epoch,2,i_sat)
rsat_icrf = orbits_partials_icrf(i_epoch,3:5,i_sat)
vsat_icrf = orbits_partials_icrf(i_epoch,6:8,i_sat)
!print *,"PRN, Sec00 ", PRN_GNSS, Sec_00 

! Computation of satellite attitude and transformation matrix between terrestrial and satellite body-fixed frame
CALL att_matrix (mjd, rsat_icrf, vsat_icrf, PRN_isat, BLOCK_isat,       & 
				& eclipse_status, Yangle_array, Rtrf2bff, Quaternions_trf2bff)
!print *,"Yangle", Yangle_array 
!print *,"  "

attitude_array(i_epoch,1,i_sat) = mjd
attitude_array(i_epoch,2,i_sat) = Sec_00
attitude_array(i_epoch,3,i_sat) = eclipse_status
attitude_array(i_epoch,4,i_sat) = Yangle_array(1)
attitude_array(i_epoch,5,i_sat) = Yangle_array(2)

attitude_array(i_epoch,6,i_sat)   = Rtrf2bff(1,1)
attitude_array(i_epoch,7,i_sat)   = Rtrf2bff(1,2)
attitude_array(i_epoch,8,i_sat)   = Rtrf2bff(1,3)

attitude_array(i_epoch,9,i_sat)  = Rtrf2bff(2,1)
attitude_array(i_epoch,10,i_sat)  = Rtrf2bff(2,2)
attitude_array(i_epoch,11,i_sat)  = Rtrf2bff(2,3)

attitude_array(i_epoch,12,i_sat) = Rtrf2bff(3,1)
attitude_array(i_epoch,13,i_sat) = Rtrf2bff(3,2)
attitude_array(i_epoch,14,i_sat) = Rtrf2bff(3,3)

attitude_array(i_epoch,15,i_sat) = Quaternions_trf2bff(1)
attitude_array(i_epoch,16,i_sat) = Quaternions_trf2bff(2)
attitude_array(i_epoch,17,i_sat) = Quaternions_trf2bff(3)
attitude_array(i_epoch,18,i_sat) = Quaternions_trf2bff(4)

	END DO
END DO
! ----------------------------------------------------------------------

END SUBROUTINE

End

