MODULE m_sp3_PRN


! ----------------------------------------------------------------------
! MODULE: m_sp3_PRN.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the subroutine 'sp3_PRN'
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	13 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
	  


Contains


SUBROUTINE sp3_PRN (fname, PRNmatrix, year, month, day, Sec_00)

! ----------------------------------------------------------------------
! SUBROUTINE: sp3
! ----------------------------------------------------------------------
! Purpose:
!  Read the input sp3 orbit data file format (including multi-GNSS orbits) 
!  for outputting the satellites PRN numbers
! ----------------------------------------------------------------------
! Input arguments:
! - fname:			sp3 file name
!
! Output allocatable arrays:
! - PRNmatrix: 		PRN array of all the satellites included in the sp3 file
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia, Frontier-SI 
! Created:	28 March 2019
! ----------------------------------------------------------------------

      USE mdl_precision
      USE mdl_num
      !USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname

! OUT
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(OUT) :: PRNmatrix(:)
!year, month, day, sec_day	  
      INTEGER :: year, month, day
	  REAL (KIND = prec_q), INTENT(OUT) :: Sec_00 
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

      INTEGER :: hr, minute, J_flag
      DOUBLE PRECISION sec, DJM0, FD, mjd_i

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
      CHARACTER (LEN=2) :: char2
      CHARACTER (LEN=3) :: char3
      CHARACTER (LEN=200) :: charN 
      CHARACTER (LEN=1) :: GNSSlet
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: space_i, pflag
      INTEGER (KIND = prec_int2) :: AllocateStatus
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nsat
      INTEGER (KIND = prec_int8) :: isat
      CHARACTER (LEN=3) :: PRN_isat
! ----------------------------------------------------------------------
  

UNIT_IN = 9  												

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
isat = 0
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
! 1st line : Read Initial Epoch (year, month, day, sec_day)
! ----------------------------------------------------------------------
         If (i == 1) then
			fmt_line = '(A2,A1,I4)'
            READ (line_ith, fmt_line , IOSTAT=ios_data) ver, Zvec, year
			READ (line_ith, * , IOSTAT=ios_data) charN, month, day, hr, minute, sec, Nepochs, charN 
			   
            if (Zvec == 'P') then
               Nvec = 3
			else if (Zvec == 'V') then
			   Nvec = 6
			end if

! MJD of Epoch (including fraction of the day)
!            CALL iau_CAL2JD ( year, month, day, DJM0, mjd_i, J_flag )
!            mjd_i = mjd_i + (sec/86400D0) + (hr/24D0) + (minute/60D0/24D0)
	
! Sec of Day (since 0h)
            Sec_00 = sec + minute * 60D0 + hr * 3600D0
		 End if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! 3rd line: Satellites Number ! and PRN numbers
! ----------------------------------------------------------------------
         If (i == 3) then
			fmt_line = '(A2,A1, I3 ,A)'
            READ (line_ith, fmt_line , IOSTAT=ios_data) char2, char1, Nsat, charN
			!print *,"Nsat ", Nsat
			ALLOCATE (PRNmatrix(Nsat), STAT = AllocateStatus)		   
		 End if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Satellites PRNs :: Check first epoch
! ----------------------------------------------------------------------
IF (char1st == "*") THEN

! ----------------------------------------------------------------------
! Satellites loop
! ----------------------------------------------------------------------
    DO
	  READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
	  i = i + 1
	  ! 1st character of line		 
      fmt_line = '(A1)'
	  READ (line_ith, fmt_line , IOSTAT=ios_data) char1st
! ----------------------------------------------------------------------
! End of Loop
If (isat .GE. Nsat) Then
    EXIT		
END IF
! ----------------------------------------------------------------------
		 ! Position PRN
         IF (char1st == "P") THEN
			!READ (line_ith, * , IOSTAT=ios_data) char1, PRN_i !, r_x, r_y, r_z, clock_r
			
			!fmt_line = '(A1,A1,I2.2,4F14.6,A)'
			!READ (line_ith, fmt_line , IOSTAT=ios_data) char1, GNSSlet, PRN_i, r_x, r_y, r_z, clock_r, charN

			!WRITE(PRN_isat, FMT='(A1,I2.2)', IOSTAT=ios_data) GNSSlet, PRN_i
			!print *,"GNSSlet ", GNSSlet
			!print *,"PRN_i ", PRN_i
			!print *,"PRN_isat ", PRN_isat
			
			fmt_line = '(A1,A3,A)'
			READ (line_ith, fmt_line , IOSTAT=ios_data) char1, PRN_isat, charN
			isat = isat + 1			
			PRNmatrix(isat) = PRN_isat
			!print *,"PRN_isat ", PRN_isat
			!print *,"isat ", isat
			!CALL SLEEP(1)
		 END IF
    END DO  
! ----------------------------------------------------------------------
END IF
! ----------------------------------------------------------------------

END DO
CLOSE (UNIT=UNIT_IN)
! ----------------------------------------------------------------------


END SUBROUTINE


End Module


