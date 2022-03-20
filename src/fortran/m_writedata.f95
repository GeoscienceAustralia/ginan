MODULE m_writedata


! ----------------------------------------------------------------------
! MODULE: m_writedata.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for write data to output (ascii) files in append option
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia
! Created:	30 January 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE write_data (filename, writeline)

! ----------------------------------------------------------------------
! SUBROUTINE: writedata.f90
! ----------------------------------------------------------------------
! Purpose:
!  Write data to an ascii file using append option
! ----------------------------------------------------------------------
! Input arguments:
! - filename:       File name to be used for writing the data
! - writeline:        Data line to be written
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia           September 2015
! ----------------------------------------------------------------------
! Last modified:
! - Thomas Papanikolaou, 30 January 2019:
!   writedata.f90 subroutine has been upgraded to Fortran 2003 and 
!   has been modified to avoid compiling errors of the gfortran compiler 
!   (variable format expressions are not recognized in gfortran)   
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_write
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: filename
      !REAL (KIND = prec_q), INTENT(IN), DIMENSION(:), ALLOCATABLE :: writeline 
      CHARACTER (LEN=1000), INTENT(IN) :: writeline
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
!      sz1 = SIZE (writeline,DIM=1)
!      sz2 = SIZE (writeline,DIM=1)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Format definition
!100   FORMAT (<sz2>E27.17)                                      	
! ----------------------------------------------------------------------
! Alternative approach to the format definition
      RealT = 'E'
      RealW = 27
      RealD = 17
!      WRITE (fmt_wrt,FMT='(A1,I3,A1, A1,I2,A1,I2)') "<",sz2,">", RealT, RealW, ".", RealD 
      !PRINT *, "fmt_wrt: ", fmt_wrt
!      fmt_wrt0 = '(fmt_wrt)'
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
!WRITE (UNIT=UNIT_IN, FMT=100, IOSTAT=ios_ith) writeline
WRITE (UNIT=UNIT_IN, FMT=*, IOSTAT=ios_ith) writeline

IF (ios_ith /= 0) THEN
   PRINT *, "Error in writing to file: ", TRIM (filename)
   PRINT *, "WRITE IOSTAT=", ios_ith
END IF
! ----------------------------------------------------------------------


!ENDFILE (UNIT = UNIT_IN) 

CLOSE (UNIT = UNIT_IN)


 
END SUBROUTINE



End Module
