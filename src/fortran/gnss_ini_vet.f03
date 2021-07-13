SUBROUTINE gnss_ini_vet (PRMfname)


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
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      !USE mdl_arr
      USE m_sp3
      USE m_gnssp3
      !USE m_keplerorb
      !USE m_rso
      USE m_interporb
      USE m_orbT2C
      USE m_obsorbT2C
      USE m_sat_ini_vet
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: PRMfname				
! OUT
!      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:,:), ALLOCATABLE :: ini_vet 
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
!      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: ini_vet
! ----------------------------------------------------------------------

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
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read input file
i = 0
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


! GNSS orbit data (sp3) file name
IF (word1_ln == "pseudobs_filename") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_orb 
END IF
! ----------------------------------------------------------------------

END DO
CLOSE (UNIT=UNIT_IN)


! satellite position and velocity are based on ITRF
! generate the initial state vectors for multi-GNSS satellitesn
CALL sat_ini_vet (fname_orb, ini_vet)


END
