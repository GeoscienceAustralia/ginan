SUBROUTINE write_array (filename)

! ----------------------------------------------------------------------
! SUBROUTINE: write_array.f90
! ----------------------------------------------------------------------
! Purpose:
!  Write array data to an ascii file
! ----------------------------------------------------------------------
! Input arguments:
! - filename:       file name to be used for writing array data
! - wrtArray:       Input allocatable array
! - mdl_write:		Module for the input allocatable array    
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia           September 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_write
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: filename
! OUT
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, i_write
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_ith
      INTEGER (KIND = prec_int8) :: sz1, sz2
      INTEGER (KIND = prec_int2) :: wrt_opt
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: RealT
      INTEGER (KIND = prec_int2) :: RealW, RealD
      CHARACTER (LEN=70) :: fmt_wrt,fmt_wrt0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Array dimensions
      sz1 = SIZE (wrtArray,DIM=1)
      sz2 = SIZE (wrtArray,DIM=2)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      UNIT_IN = 7  												
      wrt_opt = 2
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Format definition
100   FORMAT (<sz2>E27.17)                                      			!777
! ----------------------------------------------------------------------
! Alternative approach
      RealT = 'E'
      RealW = 27
      RealD = 17
      WRITE (fmt_wrt,FMT='(A1,I3,A1, A1,I2,A1,I2)') "<",sz2,">", RealT, RealW, ".", RealD 
      !PRINT *, "fmt_wrt: ", fmt_wrt
      fmt_wrt0 = '(fmt_wrt)'
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT=UNIT_IN,FILE=filename,ACTION="WRITE",POSITION="REWIND", IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", filename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------
	 
	  
      IF (wrt_opt == 1)	THEN
! ----------------------------------------------------------------------
! Write data to file 
      WRITE (UNIT=UNIT_IN, FMT=100, IOSTAT=ios_ith) TRANSPOSE(wrtArray)
      IF (ios_ith /= 0) THEN
         PRINT *, "Error in writing to file: ", TRIM(filename)
         PRINT *, "WRITE IOSTAT=", ios_ith
      END IF
      ENDFILE (UNIT = UNIT_IN) 
      CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------

      ELSE  
! ----------------------------------------------------------------------
! Write data to file | Line by line	  
      DO i_write = 1 , sz1
	     WRITE (UNIT=UNIT_IN,FMT=100,IOSTAT=ios_ith) wrtArray(i_write,:)
         IF (ios_ith /= 0) THEN
            PRINT *, "Error in writing to file: ", TRIM (filename)
            PRINT *, "WRITE IOSTAT=", ios_ith
         END IF
      END DO
      ENDFILE (UNIT = UNIT_IN) 
      CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------
      END IF

	  


END
