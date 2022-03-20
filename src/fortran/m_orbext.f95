MODULE m_orbext


! ----------------------------------------------------------------------
! MODULE: m_orbext.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for external orbit comparison 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, CRC-SI
! Created:	24 April 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
SUBROUTINE orbext (EQMfname, orb_icrf, orb_itrf, stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC, stat_XYZ_extT, orbdiff)


! ----------------------------------------------------------------------
! SUBROUTINE: orbext.f03
! ----------------------------------------------------------------------
! Purpose:
!  External orbit comparison between estimated orbit and available precise orbits (sp3, RSO) 
! ----------------------------------------------------------------------
! Input arguments:
! - EQMfname: 	Input cofiguration file name for the orbit parameterization 
! - orb_icrf: 	Satellite orbit array in ICRF including the following per epoch:
!               - Modified Julian Day number (including the fraction of the day) 
!				- Seconds since 00h 
!				- Position vector (m)
!				- Velocity vector (m/sec)
! - orb_itrf: 	Satellite orbit array in ITRF including the following per epoch:
!               - Modified Julian Day number (including the fraction of the day) 
!				- Seconds since 00h 
!				- Position vector (m)
!				- Velocity vector (m/sec)
!
! Output arguments:
! - veqSmatrix:	State trasnition matrix obtained from the Variational Equations solution based on numerical integration methods
! - veqPmatrix: Sensitivity matrix obtained from the Variational Equations solution based on numerical integration methods
! - orbdiff:  : [MJD PRN BLOCKTYPE lambda beta(deg) del_u(deg) yaw(deg) ANGX(deg) ANGY(deg) ANGZ(deg) dR(m) dT(m) dN(m) FR(m^2/s) FT(m^2/s) FN(m^2/s)]
! ----------------------------------------------------------------------
! Note 1:
! The time scale of the 2 first collumns of the orbit arrays (MJD and Seoncds since 00h) 
! refer to the time system defined by the global variable TIME_SCALE in the module mdl_param.f03
! according to the input parameterization file 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, CRC-SI
! Created:	24 April 2018
!
! Changes:      20-05-219  Tzupang Tseng: Output the orbital information for data analysis
!               06-08-2019 Tzupang Tseng: Skip bad orbits with zero value in SP3 file
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE m_statdelta
      USE m_statorbit
      USE m_statorbit2
      USE m_writearray
      use pod_yaml
      IMPLICIT NONE
	  
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN)  :: EQMfname				
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: orb_icrf, orb_itrf  
! ----------------------------------------------------------------------
! OUT
	  REAL (KIND = prec_d), DIMENSION(5,6), INTENT(OUT) :: stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC
	  REAL (KIND = prec_d), DIMENSION(5,6), INTENT(OUT) :: stat_XYZ_extT
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: t_sec     
      INTEGER (KIND = prec_int8) :: Nepochs, i, j
      INTEGER (KIND = prec_int8) :: sz1, sz2 
	  REAL (KIND = prec_d) :: mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
	  REAL (KIND = prec_d) :: dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: dorb, dorb_icrf, dorb_itrf, orbdiff, orbang
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: dorb_XYZ, dorb_RTN, dorb_Kepler
!	  REAL (KIND = prec_d), DIMENSION(5,6) :: stat_XYZ, stat_RTN, stat_Kepler
      REAL (KIND = prec_d), DIMENSION(:), ALLOCATABLE :: RMSdsr, Sigmadsr, MEANdsr, MINdsr, MAXdsr 	  
      CHARACTER (LEN=100) :: filename				
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! External Orbit reading: Precise Orbit (sp3)
! External orbit: orbext_ICRF, orbext_ITRF, orbext_kepler
CALL prm_orbext (EQMfname)												
! ----------------------------------------------------------------------
! Orbit comparison statistics
! ICRF

if (.false.) then
! ----------------------------------------------------------------------
! Time Scale transformation in orbext_ICRF matrix
! ----------------------------------------------------------------------
! Time scale change is applied in case that TIME_SCALE .NOT. Terrestrial Time
! TIME_SCALE: global variable in module mdl_param.f03
! ----------------------------------------------------------------------
If (yml_time_scale /= TT_time) Then

sz1 = size(orbext_ICRF, DIM = 1)
sz2 = size(orbext_ICRF, DIM = 2)
Nepochs = sz1

Do i = 1 , Nepochs

! MJD in TT Time
mjd = orbext_ICRF(i,1)
! Seconds since 00h
t_sec = orbext_ICRF(i,2)
!print *, "t_sec ", t_sec

! Time scale: TT to GPS time
CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
Call time_TT_sec (mjd , dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS)
!print *,"dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS", dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS

! Test the TIME_SCALE global variable in mdl_param.f03	
If (yml_time_scale == GPS_time) then
	mjd = mjd_GPS
	t_sec = t_sec - (dt_TT_TAI + dt_TAI_GPS)
Else if (yml_time_scale == UTC_time) then
	mjd = mjd_UTC		
	t_sec = t_sec - (dt_TT_TAI + dt_TAI_UTC)
Else if (yml_time_scale == TAI_time) then
	mjd = mjd_TAI
	t_sec = t_sec - (dt_TT_TAI)	
End If	
!print *, "TIME_SCALE ", TIME_SCALE
!print *, "(dt_TT_TAI + dt_TAI_GPS) ", (dt_TT_TAI + dt_TAI_GPS)
!print *, "t_sec ", t_sec

! Seconds since 00h
!t_sec = (mjd - INT(mjd)) * (24.D0 * 3600.D0)
! Seconds since 00h
If (t_sec >= 86400.D0) Then
	t_sec = t_sec - INT(t_sec / 86400.D0) * 86400.D0
End IF

! Time scale change in orbit matrix and VEQ matrices 
orbext_ICRF(i,1) = mjd
orbext_ICRF(i,2) = t_sec
 
End Do

End If
endif

CALL statorbit2 (orbext_ICRF, orb_icrf, orbdiff)
CALL statorbit (orbext_ICRF, orb_icrf, dorb_icrf, dorb_RTN, dorb_Kepler, stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC)

! ITRF
Call statdelta(orbext_ITRF, orb_itrf, dorb_itrf, RMSdsr, Sigmadsr, MEANdsr, MINdsr, MAXdsr)
stat_XYZ_extT(1,:) = RMSdsr
stat_XYZ_extT(2,:) = Sigmadsr
stat_XYZ_extT(3,:) = MEANdsr
stat_XYZ_extT(4,:) = MINdsr
stat_XYZ_extT(5,:) = MAXdsr
! ----------------------------------------------------------------------

!print *,"Orbit comparison: ICRF"
!print *,"RMS RTN r ", stat_RTN(1, 1:3)
!print *,"RMS RTN v ", stat_RTN(1, 4:6)
!print *,"RMS r     ", stat_XYZ(1, 1:3)
!print *,"RMS v     ", stat_XYZ(1, 4:6)
!print *,"RMS Kepler", stat_Kepler(1, 1:3)
!print *,"RMS Kepler", stat_Kepler(1, 4:6)

!print *,"Orbit comparison: ITRF"
!print *,"RMS r     ", RMSdsr(1:3)
!print *,"RMS v     ", RMSdsr(4:6)
!print *,"MIN r     ", MINdsr(1:3)
!print *,"MIN v     ", MINdsr(4:6)
!print *,"MAX r     ", MAXdsr(1:3)
!print *,"MAX v     ", MAXdsr(4:6)


! ----------------------------------------------------------------------
! Write orbit matrices to ascii files
! ----------------------------------------------------------------------
! External orbit
filename = "orbext_ICRF.out"
!Call writearray (orbext_ICRF, filename)
filename = "orbext_ITRF.out"
!Call writearray (orbext_ITRF, filename)
! ----------------------------------------------------------------------
! Orbit comparison residuals
! ICRF
filename = "dorb_icrf.out"
!Call writearray (dorb_icrf, filename)
filename = "dorb_RTN.out"
!Call writearray (dorb_RTN, filename)
filename = "dorb_Kepler.out"
!Call writearray (dorb_Kepler, filename)
! ITRF
filename = "dorb_itrf.out"
!Call writearray (dorb_itrf, filename)
! ----------------------------------------------------------------------
! With all orbital information
filename = "orbdiff.rtn"
!Call writearray (orbdiff, filename)

!filename = "orbang.rtn"
!Call writearray (orbang, filename)



END SUBROUTINE

End

