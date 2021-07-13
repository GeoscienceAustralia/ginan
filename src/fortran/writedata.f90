SUBROUTINE writedata (filename)

! ----------------------------------------------------------------------
! SUBROUTINE: writedata.f90
! ----------------------------------------------------------------------
! Purpose:
!  Write data to an ascii file
! ----------------------------------------------------------------------
! Input arguments:
! - filename:       File name to be used for writing the data
! - wrtline:        Input allocatable array
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
      CHARACTER (LEN=100), INTENT(IN) :: filename
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


      UNIT_IN = 7  												

  
! ----------------------------------------------------------------------
! Array dimensions
!      sz1 = SIZE (wrtline,DIM=1)
      sz2 = SIZE (wrtline,DIM=1)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Format definition
100   FORMAT (<sz2>E27.17)                                      	
! ----------------------------------------------------------------------
! Alternative approach to the format definition
      RealT = 'E'
      RealW = 27
      RealD = 17
      WRITE (fmt_wrt,FMT='(A1,I3,A1, A1,I2,A1,I2)') "<",sz2,">", RealT, RealW, ".", RealD 
      !PRINT *, "fmt_wrt: ", fmt_wrt
      fmt_wrt0 = '(fmt_wrt)'
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open file
!OPEN (UNIT=UNIT_IN, FILE=filename, ACTION="WRITE", POSITION="REWIND", IOSTAT=ios)
! STATUS='OLD'
! STATUS='UNKNOWN' ! Default 
OPEN (UNIT=UNIT_IN, FILE=filename, ACTION="WRITE", POSITION="APPEND", IOSTAT=ios)

IF (ios /= 0) THEN
   PRINT *, "Error in opening file:", filename
   PRINT *, "OPEN IOSTAT=", ios
END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Write data to file | Write data line 
WRITE (UNIT=UNIT_IN, FMT=100, IOSTAT=ios_ith) wrtline

IF (ios_ith /= 0) THEN
   PRINT *, "Error in writing to file: ", TRIM (filename)
   PRINT *, "WRITE IOSTAT=", ios_ith
END IF
! ----------------------------------------------------------------------


!ENDFILE (UNIT = UNIT_IN) 

CLOSE (UNIT = UNIT_IN)


 
END
