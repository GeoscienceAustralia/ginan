SUBROUTINE prm_orbext (PRMfname)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_orbext.f90
! ----------------------------------------------------------------------
! Purpose:
!  External Orbit settings
!  Precise orbit data (sp3) interpolation or computation of Keplerian orbits
! is set here.
! 
!  The formulated orbit may be used for the following purposes:
!  1. External orbit comparison 
!  2. Pseudo-observations within the dynamic orbit estimation procedure 
! ----------------------------------------------------------------------
! Input arguments:
! - PRMfname:		Configuration file name for the orbit parameterization
! 
! Output arguments:
! 	External Orbit is stored in the allocatable arrays orbext_ICRF, orbext_ITRF, orbext_kepler
! 	that are set as global variables in the module mdl_param.f03.
! 
! - orbext_ICRF: 	Orbit array (Nx8) in inertial frame (ICRF) including the state vector per epoch
! 					  Collumns desciption per epoch:
!               	- Modified Julian Day number (including the fraction of the day) 
!					- Seconds since 00h 
!					- Position vector (m)
!					- Velocity vector (m/sec)
! - orbext_ITRF:	Orbit array (Nx8) in terrestrial frame (ITRF)
! 					  Collumns desciption per epoch: similar to the orbext_ICRF
! - orbext_kepler:	Orbit array (Nx8) in inertial frame including the Keplerian elements per epoch
! 					  Collumns desciption per epoch:
!               	- Modified Julian Day number (including the fraction of the day) 
!					- Seconds since 00h 
!          			- semi-major axis  (m)
!         			- eccentricity
!    	     		- inclination (degrees)
!     				- right ascension of the ascending node (degrees)
!     				- argument of perigee (degrees)
!         			- eccentric anomaly (degrees)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	20 September 2017
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      !USE mdl_arr
      USE m_keplerorb
      USE m_rso
      USE m_interporb
      USE m_orbT2C
      USE m_orbC2T
      USE m_sp3
      use pod_yaml
      use mdl_config
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: PRMfname				
! OUT
! 
! ----------------------------------------------------------------------
 

! ----------------------------------------------------------------------
! Local Variables declaration
! ----------------------------------------------------------------------
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbext_ICRF, orbext_ITRF, orbext_kepler
      INTEGER (KIND = prec_int2) :: data_opt
      CHARACTER (LEN=10) :: FRAME_EXT
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

  
! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read input file
i = 0
fname_orb = yml_ext_orbit_filename
interpstep = yml_ext_orbit_steps
NPint = yml_ext_orbit_points
data_opt = yml_ext_orbit_opt

if (.not. yaml_found) then
! ----------------------------------------------------------------------
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

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
! External Orbit
! ----------------------------------------------------------------------
! 1. Orbit data in sp3 format (including position and velocity vectors) ! Old option: RSO orbit data by GFZ 
! 2. Interpolated orbit based on Lagrange interpolation of daily sp3 data
! 3: Keplerian orbit
! 4. Interpolated orbit (3 days arc) based on Lagrange interpolation of 3 boundary sp3 data files 
! 5. Orbit: Position vector from sp3 data; Velocity vector approximated through sp3 position differences without applying interpolation  
IF (word1_ln == "orbit_external_opt") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, data_opt 
END IF
yml_ext_orbit_opt = data_opt
! ----------------------------------------------------------------------

! Reference Frame of external orbit
! 1. ITRF
! 2. ICRF
IF (word1_ln == "orbit_ext_frame") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, FRAME_EXT 
   if (FRAME_EXT == 'ITRF') yml_ext_orbit_frame = ITRF
   if (FRAME_EXT == 'ICRF') yml_ext_orbit_frame = ICRF
END IF

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GNSS orbit data (sp3) file name
IF (word1_ln == "orbit_filename" .and. .not. ooption_on_command_line) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_orb 
END IF
! Case 4:
!fname_orb_0 = 'wum19283.sp3'  
!fname_orb_1 = 'wum19284.sp3'  
!fname_orb_2 = 'wum19285.sp3'  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Numerical Interpolation or Kepler orbit interval (sec)
IF (word1_ln == "orbit_interp_step") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, interpstep 
END IF
!interpstep = integstep ! Module mdl_param.f03
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Interpolated Orbit:	Cases: 2,4
! ----------------------------------------------------------------------
! Number of data points used in Lagrange interpolation   
IF (word1_ln == "orbit_interp_points") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, NPint 
END IF
!NPint = 12

! Output file name for writing interpolated orbit (Cases: 2, 4)
!IF (word1_ln == "orbit_interp_fname") THEN
!   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_orbint 
!END IF
!fname_orbint = 'orb_interp.out'	  
! ----------------------------------------------------------------------


END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------
end if

data_opt = yml_ext_orbit_opt

! ----------------------------------------------------------------------
! Compute the interpolated orbit arc (based on GNSS sp3 orbits) or the Keplerian orbit arc 
! Form the orbit arrays (satellite state vector per epoch) to be used for external orbit comparison
! ----------------------------------------------------------------------
 
! ----------------------------------------------------------------------
! Case 1: Orbit SP3 data with position and velocity vectors (Old: RSO data by GFZ)
! ----------------------------------------------------------------------
If (data_opt == TYPE_SP3) then 
	  
! PRN: GNSS constellation ID letter + Satellite number
fmt_line = '(A1,I2.2)'
READ (PRN, fmt_line , IOSTAT=ios) GNSSid, PRN_no
! Read RSO data and produce the orbit array (orbext_ITRF) for the input satellite PRN
!CALL rso (fname_orb, PRN_no, orbext_ITRF)	  

! Read sp3 orbit data that include position and velocity vectors 
If (yml_ext_orbit_frame == ICRF) Then
! ICRF
Call sp3 (fname_orb, PRN, orbext_ICRF, clock_matrix)
! Orbit transformation ICRF to ITRF
Call orbC2T (orbext_ICRF, GPS_time, orbext_ITRF)
Else If (yml_ext_orbit_frame == ITRF) Then
! ITRF
Call sp3 (fname_orb, PRN, orbext_ITRF, clock_matrix)
! Orbit transformation ITRF to ICRF
Call orbT2C (orbext_ITRF, GPS_time, orbext_ICRF)
End IF
! ----------------------------------------------------------------------

  
! ----------------------------------------------------------------------
! Case 2: Orbit based on Lagrange interpolation of sp3 data e.g. IGS final/rapid (15 min); MGEX (5 min) IGS rapid orbits; interval 15 min
! ----------------------------------------------------------------------
Else if (data_opt == TYPE_INTERP) then

! Interpolated Orbit: Read sp3 orbit data and apply Lagrange interpolation
If (yml_ext_orbit_frame == ICRF) Then
CALL interp_orb (fname_orb, PRN, interpstep, NPint, orbext_ICRF)
! Orbit transformation ICRF to ITRF
Call orbC2T (orbext_ICRF, GPS_time, orbext_ITRF)

Else If (yml_ext_orbit_frame == ITRF) Then

CALL interp_orb (fname_orb, PRN, interpstep, NPint, orbext_ITRF)
! Orbit transformation ITRF to ICRF
Call orbT2C (orbext_ITRF, GPS_time, orbext_ICRF)

END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Case 3:	Keplerian orbit	
! ----------------------------------------------------------------------
Else if (data_opt == TYPE_KEPLER) then	  

! Keplerian orbit arc length in number of days	  
Ndays = INT(orbarc / (24.0D0 * 3600.0D0)) + 1 ! orbarc: global in Module mdl_param.f03

! ----------------------------------------------------------------------
! Initial Epoch
MJDo = MJD_to ! Module mdl_param.f03
Sec0 = SEC_to ! Module mdl_param.f03

! Time scale transformation
mjd = MJD_to
CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)	

!If (time == 'TT') Then
If (yml_time_scale == GPS_time) then
    MJDo = mjd_GPS
Else if (yml_time_scale == UTC_time) then
    MJDo = mjd_UTC
Else if (yml_time_scale == TAI_time) then
    MJDo = mjd_TAI
End If	
Sec0 = (MJDo - INT(MJDo)) * (86400.D0)
! ----------------------------------------------------------------------

! Initial State Vector in ICRF
Zo = SVEC_Zo ! Module mdl_param.f03 

! Initial state vector type: SVEC_Zo is computed in ICRF >> Set Zo_el to 2
! 1. Keplerian Elements
! 2. State Vector (Position & Velocity) cartesian coordinates (in ICRF)
Zo_el = 2	

! Earth Gravity constant:  Module mdl_num.f90
GMearth = GM_global

! Computation of the Kepler orbit arc
CALL keplerorb (GMearth, MJDo, Sec0, Zo, Zo_el, Ndays, interpstep, orbext_kepler, orbext_ICRF)


! ----------------------------------------------------------------------
! Temporary
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Orbit Transformation to ITRF is not applied
! The orbit comparison based on Kepler orbits is perfromed only in the inertial frame
sz1 = size(orbext_ICRF, DIM = 1)
sz2 = size(orbext_ICRF, DIM = 2)
ALLOCATE (orbext_ITRF(sz1,sz2), STAT = AllocateStatus)
orbext_ITRF = orbext_ICRF
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Deactivated cases
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Case 4: Interpolated orbit 3 days arc based on Lagrange interpolation of 3 daily sp3 data files 
! ----------------------------------------------------------------------
else if (data_opt == TYPE_3LINTERP) then
	! Performing interpolation individually for each sp3 orbit file
	! Extrapolation is applied in order to avoid the gaps at the end of the daily file (sp3 last epoch is 23h 45min 00sec)
	!CALL orb3sp3(fname_orb_0, fname_orb_1, fname_orb_2, PRN, interpstep, NPint, fname_orbint)	
! or	
	! Performing interpolation after forming the orbit array (position vector) based on the mutliple sp3 files
	!CALL orb3sp3_2(fname_orb_0, fname_orb_1, fname_orb_2, PRN, interpstep, NPint, fname_orbint)	
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Case 5: Orbit: Position from sp3 data; Velocity approximated through sp3 position differences without applying interpolation  
! ----------------------------------------------------------------------
Else if (data_opt == TYPE_POSSP3) then
! fname_write = 'orb_sp3.out' 
! CALL orb_sp3 (fname_orb, PRN, fname_write)
! ----------------------------------------------------------------------


End if	  




if (.false.) then
! ----------------------------------------------------------------------
PRINT *,"--------------------- INPUT ----------------------------"
PRINT *, "External orbit case:              ", data_opt
PRINT *, "External orbit file name:         ", TRIM(fname_orb)
PRINT *, "External orbit interval           ", interpstep
PRINT *, "External orbit inerpolation points", NPint
PRINT *, "External orbit output file name   ", TRIM(fname_orbint) 
PRINT *,"--------------------------------------------------------"
PRINT *," "
! ----------------------------------------------------------------------
end if
	  

END
