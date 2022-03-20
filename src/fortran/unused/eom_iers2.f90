SUBROUTINE eom_iers2 (filename)


! ----------------------------------------------------------------------
! Subroutine:  eom_iers2.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read elements of the Earth Orientation Matrix (EOM) computed by the 
!  web service of the IERS (International Earth Rotation Service and 
!  Reference Systems) Rapid Service/Prediction Center (EOC)    
! ----------------------------------------------------------------------
! Input arguments:
! - filename:		Data file name  e.g. 'eom_iers.dat'
!
! Output arguments:
! - wrtArray:		Allocatable array for storing the Earth Orientation 
!					Matrix (EOM) elements per epoch 
!   				eom = [MJD R11 R12 R13 R14 R21 R22 R23 R31 R32 R33]  Nx10 matrix
!   				MJD	:	MJD referred in TAI time scale 
!   				R	:	EOM 3x3
! - mdl_wrte:		Module used for the 'wrtArray' allocatable array 
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia               April 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_write
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=512), INTENT(IN) :: filename
! OUT
!      REAL (KIND = prec_d), INTENT(OUT) :: eom(10)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_line, ios_data, AllocateStatus
      INTEGER (KIND = prec_int8) :: i, read_i, len_line
      CHARACTER (LEN=100) :: Format_eop, Format_eom
      CHARACTER (LEN=700) :: line_ith 
      INTEGER (KIND = prec_int8) :: year, month, day, hour, minutes, mjd_d
      REAL (KIND = prec_d) :: mjd_f, sec, R11, R12, R13, R21, R22, R23, R31, R32, R33 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      UNIT_IN = 9
      !Format_eop = '(3(I4),I7,2(F11.6),2(F12.7),2(F11.6),2(F11.6),2(F11.7),2F12.6)'
      Format_eom = '(I,F,5I,10F)'
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open data file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (filename), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", filename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read file
! ----------------------------------------------------------------------
      i = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT
         ELSE
       	    i = i + 1
         END IF
      END DO
! ----------------------------------------------------------------------
! Allocate arrays
      ALLOCATE ( wrtArray (i - 3 , 10) , STAT = AllocateStatus)
! ----------------------------------------------------------------------

      CLOSE (UNIT=UNIT_IN)


	  
! ----------------------------------------------------------------------
! Open data file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (filename), IOSTAT = ios)

! ----------------------------------------------------------------------
! Read data file
! ----------------------------------------------------------------------
! 1st line :	finalsfile = dailyfiles/57225/finals2000A.daily  
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
! 2nd line :	 (integer)     (fractional)
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
! 3rd line :	 < MJD_UTC >|<---- MJD_UTC ---->|< yr >|< mo >|< day >|< hrs >|< min >|<------seconds ---->|<------------------------------------------------Terrestrial to Celestial (T2C) Direction Cosine Matrix (R11 R12 R13 R21 R22 R23 R31 R32 R33) ------------------------------------------------------------------------------------------->
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
! ----------------------------------------------------------------------
! Data loop
      i = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
      !PRINT *, "line_ith", line_ith 
	     i = i + 1
		 

! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Data
! < MJD_UTC >|<---- MJD_UTC ---->|< yr >|< mo >|< day >|< hrs >|< min >|<------seconds ---->|<------------------------------------------------Terrestrial to Celestial (T2C) Direction Cosine Matrix (R11 R12 R13 R21 R22 R23 R31 R32 R33) ------------------------------------------------------------------------------------------->
!   57225        0.00000000E+00    2015     7     22       0       0      0.000000000E+00      4.88354143505670E-01      8.72644217371432E-01      1.51670994068086E-03     -8.72645185344796E-01      4.88354766833499E-01     -4.69632465251865E-05     -7.81674734944402E-04     -1.30061493126671E-03      9.99998848692042E-01
	     READ (line_ith, FMT =*, IOSTAT=ios_data) mjd_d,mjd_f, year, month, day, hour, minutes, sec, &
                                                  R11, R12, R13, R21, R22, R23, R31, R32, R33 
	     !READ (line_ith, FMT =Format_eom, IOSTAT=ios_data) mjd_d,mjd_f, year, month, day, hour, minutes, &
         !                                                  sec, R11, R12, R13, R21, R22, R23, R31, R32, R33 
         wrtArray(i,1) = mjd_d + mjd_f
         wrtArray(i,2) = R11
         wrtArray(i,3) = R12
         wrtArray(i,4) = R13
         wrtArray(i,5) = R21
         wrtArray(i,6) = R22
         wrtArray(i,7) = R23
         wrtArray(i,8) = R31
         wrtArray(i,9) = R32
         wrtArray(i,10) = R33
         !PRINT *, "R21", R21		 
! ----------------------------------------------------------------------
      END DO
! ----------------------------------------------------------------------

      CLOSE (UNIT=UNIT_IN)

	  
	  
END
