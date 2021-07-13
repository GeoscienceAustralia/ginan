SUBROUTINE prm_pseudobs (PRMfname, pseudobs_opt)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_pseudobs.f03
! ----------------------------------------------------------------------
! Purpose:
!  Pseudo-observations based on external precise orbits (sp3) 
!  Pseudo-observations are applied to the dynamic orbit estimation procedure
! 
! ----------------------------------------------------------------------
! Input arguments:
! - PRMfname:		Configuration file name for the orbit parameterization
! 
! Output arguments:
! 	External Orbit is stored in the allocatable arrays orbext_ICRF, orbext_ITRF, orbext_kepler
! 	that are set as global variables in the module mdl_param.f03.
! 
! - pseudobs_ICRF: 	Orbit array (Nx5) in inertial frame (ICRF) including the state vector per epoch
! 					Collumns desciption per epoch:
!               	- Modified Julian Day number (including the fraction of the day) 
!					- Seconds since 00h 
!					- Position vector (m)
! - pseudobs_ITRF:	Orbit array (Nx5) in terrestrial frame (ITRF)
! 					Collumns desciption per epoch: similar to the orbobs_ICRF
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	11 April 2018
! ----------------------------------------------------------------------
! Last modified:
! 25 June 2020,	Dr. Thomas Papanikolaou
!				1. Outliers detection for removing epochs with zero values of the pseudo-observations has been added 
! 				2. Option has been added to the input arguments for applying or not the numerical interpolation to the pseudo-observations (a-priori orbit)
! ----------------------------------------------------------------------	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      !USE mdl_arr
      USE m_sp3
      !USE m_keplerorb
      !USE m_rso
      USE m_interporb
      USE m_orbT2C
      USE m_obsorbT2C
      USE m_orb_outlier
      use pod_yaml
      use mdl_config
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: PRMfname				
      INTEGER (KIND = prec_int2), INTENT(IN) :: pseudobs_opt
! OUT
! 
! ----------------------------------------------------------------------
 

! ----------------------------------------------------------------------
! Local Variables declaration
! ----------------------------------------------------------------------
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbext_ICRF, orbext_ITRF, orbext_kepler
      INTEGER (KIND = prec_int2) :: data_opt
      CHARACTER (LEN=3) :: time_sys
      CHARACTER (LEN=300) :: fname_orb
      CHARACTER (LEN=300) :: fname_orb_0, fname_orb_1, fname_orb_2
      CHARACTER (LEN=300) :: fname_orbint
      CHARACTER (LEN=300) :: fname_write
  
      INTEGER (KIND = prec_int8) :: NPint
      INTEGER (KIND = prec_int8) :: interpstep
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int8) :: Ndays	  
      INTEGER (KIND = prec_int4) :: Zo_el
	  REAL (KIND = prec_q) :: GMearth
	  REAL (KIND = prec_d) :: Zo(6), Sec0, MJDo
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
! ----------------------------------------------------------------------
      CHARACTER (LEN=30) :: fmt_line
      CHARACTER (LEN=1) :: GNSSid
      INTEGER (KIND = prec_int4) :: PRN_no
! ----------------------------------------------------------------------
      CHARACTER (LEN=3) :: time
	  REAL (KIND = prec_d) :: mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: clock_matrix
      INTEGER (KIND = prec_int8) :: Noutliers, Nobs, Nobs_all
	  REAL (KIND = prec_d) :: outlier_value 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3_filt, orb_out
      logical found

  
! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
      found = .false.
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Open .in file
      if (.not. yaml_found) then
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
      end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read input file
i = 0
fname_orb = yml_orbit_filename
interpstep = yml_orbit_steps
NPint = yml_orbit_points

if (.not. yaml_found) then
DO

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1
! PRINT *, "READ Line (i,ios):", i, ios_line

! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
!READ (line_ith, * , IOSTAT=ios_data) word1_ln, charN 
! ----------------------------------------------------------------------
!PRINT *, "word1_ln: ", word1_ln


! ----------------------------------------------------------------------
! Parameters Keywords read 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! External Orbit (sp3) to be used as pseudo-observations
! ----------------------------------------------------------------------
! GNSS orbit data (sp3) file name
IF (word1_ln == "pseudobs_filename" .and. .not. soption_on_command_line) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_orb 
END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Interpolated Orbit
! ----------------------------------------------------------------------
! Numerical Interpolation interval (sec)
IF (word1_ln == "pseudobs_interp_step") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, interpstep 
END IF
!interpstep = integstep ! Module mdl_param.f03
! ----------------------------------------------------------------------
! Number of data points used in Lagrange interpolation   
IF (word1_ln == "pseudobs_interp_points") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, NPint 
END IF
!NPint = 12
! ----------------------------------------------------------------------


END DO
CLOSE (UNIT=UNIT_IN)
end if
! Close of input parameterization file
! ----------------------------------------------------------------------


!data_opt = 1
data_opt = pseudobs_opt

! ----------------------------------------------------------------------
! Orbit (Position vector) obtained from from IGS sp3 data 
! ----------------------------------------------------------------------
if (data_opt == TYPE_SP3) Then

! Read IGS sp3 orbit data file 
!Call sp3 (fname_orb, PRN, pseudobs_ITRF, clock_matrix)
Call sp3 (fname_orb, PRN, orbsp3, clock_matrix)

! ----------------------------------------------------------------------
! Pseudo-observations scanning : Outliers detection
! Remove Epochs that include ccordinates with zero values
! ----------------------------------------------------------------------
Noutliers = 0
outlier_value = 0.0D0
CALL orb_outlier (orbsp3, outlier_value, Noutliers, orbsp3_filt, orb_out)

sz1 = size(orbsp3, DIM = 1)
sz2 = size(orbsp3, DIM = 2)
Nobs_all = sz1
Nobs = Nobs_all - Noutliers
ALLOCATE (pseudobs_ITRF(Nobs,sz2), STAT = AllocateStatus)

IF (Noutliers == 0) THEN
! Case without outliers
pseudobs_ITRF = orbsp3
ELSE
! Case with outliers
pseudobs_ITRF = orbsp3_filt
END IF

! Orbit transformation ITRF to ICRF
!Call orbT2C (pseudobs_ITRF, time_sys, pseudobs_ICRF)
! always use GPS_time here
Call obsorbT2C (pseudobs_ITRF, GPS_time, pseudobs_ICRF)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Case 2: Orbit based on Lagrange interpolation of orbit sp3 data e.g. IGS final/rapid (15 min); MGEX (5 min) IGS rapid orbits; interval 15 min
! ----------------------------------------------------------------------
Else if (data_opt == TYPE_INTERP) then

! Interpolated Orbit: Read sp3 orbit data and apply Lagrange interpolation
CALL interp_orb (fname_orb, PRN, interpstep, NPint, pseudobs_ITRF)

! Orbit transformation ITRF to ICRF
! always use GPS_time here
Call orbT2C (pseudobs_ITRF, GPS_time, pseudobs_ICRF)
! ----------------------------------------------------------------------

End IF


END
