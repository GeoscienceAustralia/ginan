MODULE m_rso


! ----------------------------------------------------------------------
! MODULE: m_rso.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for the subroutine that read the GFZ RSO (Rapid Science Orbit) 
!  data products that provide GPS orbit including position and velocity 
!  vector in sp3 data format  
! 
! Subroutines contained within the module:
! - rso.f03
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	3 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains



SUBROUTINE rso (fname, PRN, orbRSO)

! ----------------------------------------------------------------------
! SUBROUTINE: rso.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read GFZ RSO (Rapid Science Orbit) data product file for GPS satellites
! ----------------------------------------------------------------------
! Input arguments:
! - fname:			RSO data file name
! - PRN:			GPS PRN number only (GPS <= 32) e.g. for G04 set PRN to 4
!
! Output arguments:
! - orbRSO:			Orbit array with the following values per epoch
!					Position and Velocity vector (if available)
!					[MJD Sec_of_day X Y Z Vx Vy Vz]
!
!					MJD: Modified Julian Number (including the fraction of the day)
!					Sec_of_day (Seconds since 00h of the day)
!					Position vector (X,Y,X) in m
!					Velocity vector (Vx,Vy,Vz) in m/sec 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Cooperative Research Centre for Spatial Information, Australia
! Created:	July 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 3 November 2017:
!   The subroutine has been modified in order to use advantages of 
!   Fortran 2003 in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname
      INTEGER (KIND = prec_int4), INTENT(IN) :: PRN

! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbRSO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: GNSSid

      CHARACTER (LEN=2) :: ver
      CHARACTER (LEN=1) :: Zvec
      INTEGER (KIND = prec_int8) :: Nepochs
      INTEGER (KIND = prec_int4) :: Ncol, Nvec

      INTEGER :: year, month, day, hr, minute, J_flag
      DOUBLE PRECISION sec, DJM0, FD, mjd_i
	  REAL (KIND = prec_q) :: sec_day 

      INTEGER (KIND = prec_int4) :: PRN_i
      REAL (KIND = prec_q) :: r_x, r_y, r_z
	  REAL (KIND = prec_q) :: v_x, v_y, v_z
	  REAL (KIND = prec_q) :: clock_r, clock_v 
      REAL (KIND = prec_q) :: r(3), v(3)


      INTEGER (KIND = prec_int8) :: i, orb_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      CHARACTER (LEN=7) :: Format1
      CHARACTER (LEN=100) :: fmt_line
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=1) :: char1st
      CHARACTER (LEN=1) :: char1
      CHARACTER (LEN=1) :: GNSSlet
      CHARACTER (LEN=3) :: char3
      CHARACTER (LEN=200) :: charN 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: space_i, pflag
      INTEGER (KIND = prec_int2) :: AllocateStatus
	  
	  

	  
! ----------------------------------------------------------------------
! PRN: GNSS constellation ID letter + Satellite number
!fmt_line = '(A1,I2.2)'
!READ (PRNid, fmt_line , IOSTAT=ios) GNSSid, PRN
! ----------------------------------------------------------------------
!print *,"GNSSid ", GNSSid
!print *,"PRN_sp3 ", PRN
	  
	  
      UNIT_IN = 9  												

! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (fname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", fname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------
! variable initialisation
Nvec = 3

! ----------------------------------------------------------------------
! Read data file
! ----------------------------------------------------------------------
      i = 0
	  orb_i = 0
      Format1 = '(A)'
	  
      DO
	     READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
	     i = i + 1
		 
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------


! ---------------------------------------------------------------------- 
! 1st character of line		 
         fmt_line = '(A1)'
		 READ (line_ith, fmt_line , IOSTAT=ios_data) char1st
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! 1st line : Read Vector type (Pos or Vel Flag), Number of epochs
         If (i == 1) then
			fmt_line = '(A2,A1,I4)'
            READ (line_ith, fmt_line , IOSTAT=ios_data) ver, Zvec, Year

			READ (line_ith, * , IOSTAT=ios_data) charN, month, day, hr, minute, sec, Nepochs, charN 
			   
            if (Zvec == 'P') then
               Nvec = 3
			else if (Zvec == 'V') then
			   Nvec = 6
			end if
			
! ----------------------------------------------------------------------
! Dyanic Allocatable array (Output argument)
Ncol = 2 + Nvec
ALLOCATE ( orbRSO(Nepochs , Ncol), STAT = AllocateStatus)
! ----------------------------------------------------------------------
		   		   
		 End if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Data (per epoch)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Epoch
         IF (char1st == "*") THEN
			orb_i = orb_i + 1
            !*  2010 12 25  0  0  0.0000 
			READ (line_ith, * , IOSTAT=ios_data) char3, year, month, day, hr, minute, sec  

! MJD of Epoch (including fraction of the day)
            CALL iau_CAL2JD ( year, month, day, DJM0, mjd_i, J_flag )
            mjd_i = mjd_i + (sec/86400D0) + (hr/24D0) + (minute/60D0/24D0)
			
! Sec of Day (since 0h)
            sec_day = sec + minute * 60D0 + hr * 3600D0

! Write MJD, Sec_day
			orbRSO (orb_i , 1) = mjd_i
			orbRSO (orb_i , 2) = sec_day
		 End if	
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Position and Velocity vector per PRN
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Position
         IF (char1st == "P") THEN
			!READ (line_ith, * , IOSTAT=ios_data) char1, PRN_i !, r_x, r_y, r_z, clock_r
			!fmt_line = '(A1,A1,I2.2,4F14.6,A)'
			!READ (line_ith, fmt_line , IOSTAT=ios_data) char1, GNSSlet, PRN_i, r_x, r_y, r_z, clock_r, charN
			READ (line_ith, * , IOSTAT=ios_data) char1, PRN_i ,r_x, r_y, r_z, clock_r, charN
			
! PRN check
			!IF (GNSSid == GNSSlet .AND. PRN == PRN_i) THEN
			IF (PRN == PRN_i) THEN
				! Position vector
				r(1) = r_x
				r(2) = r_y
				r(3) = r_z
                ! Unit conversion: Km to meters				
				r = r * 1.0D3
				! Write r 
				orbRSO (orb_i , 3) = r(1)
				orbRSO (orb_i , 4) = r(2)
				orbRSO (orb_i , 5) = r(3)
			End If		 			
         End If
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Velocity		 
         IF (char1st == "V") THEN
			fmt_line = '(A1,A1,I2.2,4F14.6,A)'
			READ (line_ith, fmt_line , IOSTAT=ios_data) char1, char1, PRN_i, v_x, v_y, v_z, clock_v, charN		
! PRN check
			IF (PRN == PRN_i) THEN
				! Velocity vector
				v(1) = v_x
				v(2) = v_y
				v(3) = v_z
				! Unit conversion: dm/sec to m/sec                
				v = v * 1.0D-1
				! Write v 
				orbRSO (orb_i , 6) = v(1)
				orbRSO (orb_i , 7) = v(2)
				orbRSO (orb_i , 8) = v(3)
			End If
		 End If
! ----------------------------------------------------------------------

      END DO
      CLOSE (UNIT=UNIT_IN)	  


	  
END SUBROUTINE



END
