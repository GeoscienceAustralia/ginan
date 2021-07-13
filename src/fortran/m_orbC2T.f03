MODULE m_orbC2T


! ----------------------------------------------------------------------
! MODULE: m_orbC2T.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the orbC2T subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	6 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
SUBROUTINE orbC2T (orbC, time, orbT)


! ----------------------------------------------------------------------
! SUBROUTINE: orbC2T
! ----------------------------------------------------------------------
! Purpose:
!  Orbit transformation from inertial (ICRF) to terrestrial (ITRF) reference frame 
! ----------------------------------------------------------------------
! Input arguments:
! - orbC: 		Satellite orbit array in ICRF including the following per epoch:
!               - Modified Julian Day number in the input time scale (including the fraction of the day) 
!				- Seconds since 00h in the input time scale
!				- Position vector (m)
!				- Velocity vector (m/sec)
! - time:		Time scale of the input orbit array (2 first collumns); Options
!				TT  (Terrestrial Time)
! 				GPS (GPS Time)
! 				TAI
! 				UTC
!
! Output arguments:
! - orbT: 		Satellite orbit array in ITRF including the following per epoch:
!               - Modified Julian Day number (including the fraction of the day) 
!				- Seconds since 00h 
!				- Position vector (m)
!				- Velocity vector (m/sec)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	13 November 2017
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
!      USE mdl_num
!      USE mdl_param
      use pod_yaml
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: orbC 
      INTEGER*2, INTENT(IN) :: time
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: orbT
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, i, j
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
	  REAL (KIND = prec_d) :: mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      DOUBLE PRECISION EOP_cr(7)
      DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)
      REAL (KIND = prec_d) :: r_TRS(3), v_TRS(3)
      REAL (KIND = prec_d) :: r_CRS(3), v_CRS(3)
      REAL (KIND = prec_d) :: v_TRS_1(3), v_TRS_2(3), v_CRS_1(3), v_CRS_2(3)	  
! ----------------------------------------------------------------------
      INTEGER IY, IM, ID, J_flag
      DOUBLE PRECISION DJM0, sec, FD, Sec0



sz1 = size(orbC, DIM = 1)
sz2 = size(orbC, DIM = 2)

Nepochs = sz1

! Dynamic allocatable array
ALLOCATE (orbT(sz1,sz2), STAT = AllocateStatus)

! ----------------------------------------------------------------------
! Computations per epoch loop
Do i = 1 , Nepochs

!print *, "i", i

! MJD
mjd = orbC(i,1)

! Time scale transformation to TT
If (time == TT_time) Then
	CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
Else If (time == GPS_time) then
	CALL time_GPS (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
Else if (time == UTC_time) then
	CALL time_UTC (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
Else if (time == TAI_time) then
	CALL time_TAI (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
End If	

! Position and Velocity vectors in ICRF
r_CRS(1:3) = orbC(i,3:5)
v_CRS(1:3) = orbC(i,6:8)

! ----------------------------------------------------------------------
! Orbit Tranformation: ICRF to ITRF
! ----------------------------------------------------------------------
! ICRF-ITRF transformation matrix (including derivatives) based on EOP data
CALL EOP (mjd_TT, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)

! r_TRS = CRS2TRS * r_CRS
CALL matrix_Rr (CRS2TRS, r_CRS , r_TRS)

! v_TRS = CRS2TRS * v_CRS + d_CRS2TRS * r_CRS
CALL matrix_Rr (CRS2TRS,   v_CRS , v_TRS_1)
CALL matrix_Rr (d_CRS2TRS, r_CRS , v_TRS_2)
v_TRS = v_TRS_1 + v_TRS_2
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! orbT array per epoch
! ----------------------------------------------------------------------
orbT(i,1)   = orbC(i,1)
orbT(i,2)   = orbC(i,2)
orbT(i,3:5) = r_TRS
orbT(i,6:8) = v_TRS
! ----------------------------------------------------------------------

End Do
! ----------------------------------------------------------------------



END SUBROUTINE


End

