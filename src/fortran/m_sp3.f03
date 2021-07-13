MODULE m_sp3


! ----------------------------------------------------------------------
! MODULE: m_sp3.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the subroutine 'sp3' for reading GNSS orbits in sp3 format
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	13 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
	  


Contains


SUBROUTINE sp3 (fname, PRNid, orbsp3, clock_matrix)

! ----------------------------------------------------------------------
! SUBROUTINE: sp3
! ----------------------------------------------------------------------
! Purpose:
!  Read sp3 orbit data file format (including multi-GNSS orbits)
! ----------------------------------------------------------------------
! Input arguments:
! - fname:			sp3 file name
! - PRNid:			PRN number (e.g. G21)
!
! Output allocatable arrays:
! - orbsp3:			Orbit array with the following values per epoch
!					Position vector (only); Velocity is not available
!					[MJD Sec_of_day X Y Z]
!
!					Position and Velocity vector (if available)
!					[MJD Sec_of_day X Y Z Vx Vy Vz]
!
!					MJD: Modified Julian Number (including the fraction of the day)
!					Sec_of_day (Seconds since 00h of the day)
!					Position vector (X,Y,X) in m
!					Velocity vector (Vx,Vy,Vz) in m/sec 
! - clock_matrix:	Clocks array with the following values per epoch
!					Position vector (only); Velocity is not available
!					[MJD Sec_of_day X Y Z]
!
!					Position and Velocity vector (if available)
!					[MJD Sec_of_day X Y Z Vx Vy Vz]
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	July 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 14 June 2017:
!	PRN modification for considering the multi-GNSS constellations
!	PRN has been changed from number data type to character (e.g. G21)
! 	The letter before the satellite number refer to the GNSS constellation
! 	according to the following convention					
! 	GNSS constellation id letter
! 	G: GPS
! 	R: GLONASS
! 	E: Galileo
! 	C: BDS (BeiDou)
! 	J: QZSS
! 	S: SBAS
!
! - Dr. Thomas Papanikolaou, 13 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 adavantages in dynamic memory allocation
! 
! - Dr. Thomas Papanikolaou, 17 October 2019:
!	Satellite Clocks matrix has been formed and added as output argument 
! ----------------------------------------------------------------------

      USE mdl_precision
      USE mdl_num
      use mdl_config
      !USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname
      CHARACTER (LEN=3), INTENT(IN) :: PRNid

! OUT
! 	  orb1: Allocatable array via mdl_arr.f90 
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: clock_matrix
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: PRN
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
      CHARACTER (LEN=100) :: mesg 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: space_i, pflag
      INTEGER (KIND = prec_int2) :: AllocateStatus
	  
	  

	  
! ----------------------------------------------------------------------
! PRN: GNSS constellation ID letter + Satellite number
fmt_line = '(A1,I2.2)'
READ (PRNid, fmt_line , IOSTAT=ios) GNSSid, PRN
! ----------------------------------------------------------------------
!print *,"GNSSid ", GNSSid
!print *,"PRN_sp3 ", PRN
	  
	  
      UNIT_IN = 9  												

      !default initial value
      Nvec = 3

! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (fname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", fname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------



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
                   else
                           print *, "ERROR! Zvec = ", Zvec, ", Nvec default vaule set"
                           Nvec = 3
			end if

! Allocatable arrays
               Ncol = 2 + Nvec
               !ALLOCATE (orb1(Nepochs,Ncol), STAT = AllocateStatus)
               ALLOCATE (orbsp3(Nepochs,Ncol), STAT = AllocateStatus)
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate orbsp3, dimension = (", &
                               Nepochs, ",", Ncol, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/m_sp3.f03', 1)
               end if
			   Ncol = 2 + Nvec/3
               ALLOCATE (clock_matrix(Nepochs,Ncol), STAT = AllocateStatus)		  
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate clock_matrix, dimension = (", &
                               Nepochs, ",", Ncol, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/m_sp3.f03', 1)
               end if
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
			orbsp3 (orb_i , 1) = mjd_i
			orbsp3 (orb_i , 2) = sec_day
			clock_matrix (orb_i , 1) = mjd_i
			clock_matrix (orb_i , 2) = sec_day
		 End if	
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Position and Velocity vector per PRN
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Position
         IF (char1st == "P") THEN
			!READ (line_ith, * , IOSTAT=ios_data) char1, PRN_i !, r_x, r_y, r_z, clock_r
			fmt_line = '(A1,A1,I2.2,4F14.6,A)'
			READ (line_ith, fmt_line , IOSTAT=ios_data) char1, GNSSlet, PRN_i, r_x, r_y, r_z, clock_r, charN
			
! PRN check
			IF (GNSSid == GNSSlet .AND. PRN == PRN_i) THEN
				! Position vector
				r(1) = r_x
				r(2) = r_y
				r(3) = r_z
                ! Unit conversion: Km to meters				
				r = r * 1.0D3
				! Write r 
				orbsp3 (orb_i , 3) = r(1)
				orbsp3 (orb_i , 4) = r(2)
				orbsp3 (orb_i , 5) = r(3)
				! Write clock_r 
				clock_matrix (orb_i,3) = clock_r
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
				orbsp3 (orb_i , 6) = v(1)
				orbsp3 (orb_i , 7) = v(2)
				orbsp3 (orb_i , 8) = v(3)
				! Write clock_v 
				clock_matrix (orb_i,4) = clock_v
			End If
		 End If
! ----------------------------------------------------------------------

      END DO
 100  CLOSE (UNIT=UNIT_IN)

	  
END SUBROUTINE



End Module


