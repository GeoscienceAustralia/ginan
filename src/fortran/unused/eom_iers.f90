SUBROUTINE eom_iers (filename)


! ----------------------------------------------------------------------
! Subroutine:  eom_iers.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read elements of the Earth Orientation Matrix (EOM) computed by the 
!  web service of the IERS (International Earth Rotation Service and 
!  Reference Systems) Earth Orientation Center (EOC)    
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
      CHARACTER (LEN=100) :: Format_eop
      CHARACTER (LEN=170) :: line_ith 
      INTEGER (KIND = prec_int8) :: year, month, day, hour, minutes
      REAL (KIND = prec_d) :: mjd, sec, R11, R12, R13, R21, R22, R23, R31, R32, R33 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      UNIT_IN = 9
      !Format_eop = '(3(I4),I7,2(F11.6),2(F12.7),2(F11.6),2(F11.6),2(F11.7),2F12.6)'
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
      ALLOCATE ( wrtArray (i - 2,10) , STAT = AllocateStatus)
! ----------------------------------------------------------------------

      CLOSE (UNIT=UNIT_IN)


	  
! ----------------------------------------------------------------------
! Open data file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (filename), IOSTAT = ios)

! ----------------------------------------------------------------------
! Read data file
! ----------------------------------------------------------------------
! 1st line :	        TAI          //          Civil date UTC               // Matrix  
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
! 2nd line :	Modified Julian Date //  an  / mois / jour / hour / min / sec / R11 / R12 / R13 / R21 / R22 / R23 / R31 / R32 / R33 
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
! ----------------------------------------------------------------------
! Data loop
      i = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
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
!57225.000428240739 2015  7 22  0    0 -0.0000002  0.488354145519  0.872644216245  0.001516709729 -0.872645184218  0.488354768847 -0.000046963944 -0.000781675243 -0.001300614404  0.999998848692
	     READ (line_ith, FMT =*, IOSTAT=ios_data) mjd, year, month, day, hour, minutes, sec, R11, R12, R13, R21, R22, R23, R31, R32, R33 
         wrtArray(i,1) = mjd
         wrtArray(i,2) = R11
         wrtArray(i,3) = R12
         wrtArray(i,4) = R13
         wrtArray(i,5) = R21
         wrtArray(i,6) = R22
         wrtArray(i,7) = R23
         wrtArray(i,8) = R31
         wrtArray(i,9) = R32
         wrtArray(i,10) = R33
! ----------------------------------------------------------------------
      END DO
! ----------------------------------------------------------------------

      CLOSE (UNIT=UNIT_IN)

	  
	  
END
