MODULE m_sp3


! ----------------------------------------------------------------------
! MODULE: m_sp3.f95
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


SUBROUTINE sp3 (fname, PRNid, orbsp3, interpolate_start, clock_matrix, time_system)

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
!
! - time_system:        output value of the time scale of the sp3 file
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
      use mdl_param
      use pod_yaml
      !USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname
      CHARACTER (LEN=3), INTENT(IN) :: PRNid
      REAL (KIND = prec_q), INTENT (IN) :: interpolate_start

! OUT
! 	  orb1: Allocatable array via mdl_arr.f90 
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: clock_matrix
      INTEGER (KIND=prec_int2), INTENT(OUT) :: time_system
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: PRN_num
      CHARACTER (LEN=1) :: GNSSid

      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3_tmp, clock_matrix_tmp
      INTEGER (KIND = prec_int4), DIMENSION (:), ALLOCATABLE :: bad_rows

      CHARACTER (LEN=2) :: ver
      CHARACTER (LEN=1) :: Zvec
      INTEGER (KIND = prec_int8) :: Nepochs, epochs_read
      INTEGER (KIND = prec_int4) :: Ncol, Nvec

      INTEGER :: year, month, day, hr, minute, J_flag
      DOUBLE PRECISION sec, DJM0, FD, mjd_i, mjd_1, diff
      DOUBLE PRECISION mjd_UTC, mjd_TT, mjd_GPS, mjd_TAI
	  REAL (KIND = prec_q) :: sec_day 

      INTEGER (KIND = prec_int4) :: PRN_i
      REAL (KIND = prec_q) :: r_x, r_y, r_z
	  REAL (KIND = prec_q) :: v_x, v_y, v_z
	  REAL (KIND = prec_q) :: clock_r, clock_v 
      REAL (KIND = prec_q) :: r(3), v(3)
      logical mjd_first

      INTEGER (KIND = prec_int8) :: i, orb_i, orb_j, bad_row_count
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      CHARACTER (LEN=7) :: Format1
      CHARACTER (LEN=100) :: fmt_line
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=1) :: char1st
      CHARACTER (LEN=1) :: char1
      CHARACTER (LEN=1) :: GNSSlet
      CHARACTER (LEN=3) :: char3
      CHARACTER (LEN=200) :: charN, charM
      CHARACTER (LEN=100) :: mesg 
      CHARACTER (LEN=10) :: TZ
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: space_i, pflag
      INTEGER (KIND = prec_int2) :: AllocateStatus
	  
	  

	  
      mjd_first = .false.
      mjd_1 = 0.d0

! ----------------------------------------------------------------------
! PRN: GNSS constellation ID letter + Satellite number
fmt_line = '(A1,I2.2)'
READ (PRNid, fmt_line , IOSTAT=ios) GNSSid, PRN_num
! ----------------------------------------------------------------------
!print *,"GNSSid ", GNSSid
!print *,"PRN_sp3 ", PRN
	  
	  
      UNIT_IN = 9  												

      !default initial value
      Nvec = 3

      ! default time scale selection
      time_system = GPS_time

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
               ALLOCATE (orbsp3_tmp(Nepochs,Ncol), STAT = AllocateStatus)
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate orbsp3, dimension = (", &
                               Nepochs, ",", Ncol, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/fortran/m_sp3.f95', 1)
               end if
			   Ncol = 2 + Nvec/3
               ALLOCATE (clock_matrix_tmp(Nepochs,Ncol), STAT = AllocateStatus)		  
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate clock_matrix, dimension = (", &
                               Nepochs, ",", Ncol, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/fortran/m_sp3.f95', 1)
               end if
		 End if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Data (per epoch)
! ----------------------------------------------------------------------
         IF (char1st == "%") THEN
              READ (line_ith, '(1A1A1X3A3A3A)', IOSTAT=ios_data) char1st, char1, charM, charN, TZ
              IF (char1 == "c") then
                      TZ = trim(TZ)
                      if (TZ == "GPS") then
                              time_system = GPS_time
                      elseif (TZ == "UTC") then
                              time_system = UTC_time
                      elseif (TZ == "TAI") then
                              time_system = TAI_time
                      elseif (TZ == "TT") then
                              time_system = TT_time
                      end if
                      !do nothing for anything else
              end if
              cycle
          end if

! ----------------------------------------------------------------------
! Epoch
         IF (char1st == "*") THEN
            !*  2010 12 25  0  0  0.0000 
			READ (line_ith, * , IOSTAT=ios_data) char3, year, month, day, hr, minute, sec  

! MJD of Epoch (including fraction of the day)
            CALL iau_CAL2JD ( year, month, day, DJM0, mjd_i, J_flag )
            mjd_i = mjd_i + (sec/86400D0) + (hr/24D0) + (minute/60D0/24D0)
            if (.not. yml_determination_arc_corrected .and. .not. mjd_first .and. interpolate_start > 0.d0) then
                    mjd_1 = mjd_i
                    mjd_first = .true.
             end if
			
            orb_i = orb_i + 1
            if (interpolate_start > 0.0d0 .and. mjd_i - interpolate_start < 1.d-6) then
                    orb_i = 1
            end if
            if (orb_i == 2 .and. mjd_1 /= 0.d0) then
                yml_determination_arc_corrected = .true.
                diff = (orbsp3_tmp(1, 1) - mjd_1) * 86400
                orb_est_arc = orb_est_arc - diff
            end if
            
! Sec of Day (since 0h)
            sec_day = sec + minute * 60D0 + hr * 3600D0

! Write MJD, Sec_day
			orbsp3_tmp (orb_i , 1) = mjd_i
			orbsp3_tmp (orb_i , 2) = sec_day
			clock_matrix_tmp (orb_i , 1) = mjd_i
			clock_matrix_tmp (orb_i , 2) = sec_day
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
			IF (GNSSid == GNSSlet .AND. PRN_num == PRN_i) THEN
				! Position vector
				r(1) = r_x
				r(2) = r_y
				r(3) = r_z
                ! Unit conversion: Km to meters				
				r = r * 1.0D3
				! Write r 
				orbsp3_tmp (orb_i , 3) = r(1)
				orbsp3_tmp (orb_i , 4) = r(2)
				orbsp3_tmp (orb_i , 5) = r(3)
				! Write clock_r 
				clock_matrix_tmp (orb_i,3) = clock_r
			End If		 			
         End If
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Velocity		 
         IF (char1st == "V") THEN
			fmt_line = '(A1,A1,I2.2,4F14.6,A)'
			READ (line_ith, fmt_line , IOSTAT=ios_data) char1, char1, PRN_i, v_x, v_y, v_z, clock_v, charN		
! PRN check
			IF (PRN_num == PRN_i) THEN
				! Velocity vector
				v(1) = v_x
				v(2) = v_y
				v(3) = v_z
				! Unit conversion: dm/sec to m/sec                
				v = v * 1.0D-1
				! Write v 
				orbsp3_tmp (orb_i , 6) = v(1)
				orbsp3_tmp (orb_i , 7) = v(2)
				orbsp3_tmp (orb_i , 8) = v(3)
				! Write clock_v 
				clock_matrix_tmp (orb_i,4) = clock_v
			End If
		 End If
! ----------------------------------------------------------------------

      END DO
 100  CLOSE (UNIT=UNIT_IN)

      epochs_read = orb_i

               ALLOCATE (bad_rows(Nepochs), STAT = AllocateStatus)
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate bad_rows, dimension = (", &
                               Nepochs, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/fortran/m_sp3.f95', 1)
               end if
               bad_rows = 0
               bad_row_count = 0
 ! maybe some rows which are all zero - sat has been turned off by NANU. Remove these rows
 do orb_i = 1, epochs_read
     if (orbsp3_tmp(orb_i,3) .eq. 0.d0 .and. &
             orbsp3_tmp(orb_i, 4) .eq. 0.d0 .and. &
             orbsp3_tmp(orb_i, 5) .eq. 0.d0) then
           bad_row_count = bad_row_count + 1
           bad_rows(bad_row_count) = orb_i
      end if
 end do
     
 Nepochs = epochs_read - bad_row_count
 Ncol = 2 + Nvec
               ALLOCATE (orbsp3(Nepochs, Ncol), STAT = AllocateStatus)
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate orbsp3, dimension = (", &
                               Nepochs, ",", Ncol, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/fortran/m_sp3.f95', 1)
               end if
 Ncol = 2 + Nvec/3
               ALLOCATE (clock_matrix(Nepochs, Ncol), STAT = AllocateStatus)
               if (AllocateStatus .ne. 0) then
                   write(mesg, *) "Not enough memory - failed to allocate orbsp3, dimension = (", &
                               Nepochs, ",", Ncol, ")"
                   call report('FATAL', pgrm_name, 'sp3', mesg, 'src/fortran/m_sp3.f95', 1)
               end if

  Nepochs = epochs_read + bad_row_count
  bad_row_count = 1
  orb_j = 0
  do orb_i = 1, Nepochs
     if (orb_i .eq. bad_rows(bad_row_count)) then
           bad_row_count = bad_row_count + 1
      else
           orb_j = orb_j + 1
           Ncol = 2 + Nvec
           orbsp3(orb_j, 1:Ncol) = orbsp3_tmp(orb_i, 1:Ncol)
           Ncol = 2 + Nvec/3
           clock_matrix(orb_j, 1:Ncol) = clock_matrix_tmp(orb_i, 1:Ncol)
      end if
  end do

  deallocate (orbsp3_tmp)
  deallocate (clock_matrix_tmp)
  deallocate (bad_rows)
	  
END SUBROUTINE



End Module


