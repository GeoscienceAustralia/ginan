      subroutine CATfile (fname_header,fname_data,fname_out)

	  
! ----------------------------------------------------------------------
! SUBROUTINE: CATfile.f
! ----------------------------------------------------------------------
! Purpose:
!  Write 2 input ascii files to one output file
!  Equivalent with the Unix command CAT
! ----------------------------------------------------------------------
! Input arguments:
! - fname_header:	1st file name
! - fname_data:		2nd file name
!
! Output arguments:
! - fname_out:		Output file name
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou                                October 2015
! ----------------------------------------------------------------------


      IMPLICIT NONE

! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: fname_header, fname_data, fname_out
      INTEGER  UNIT_hd, UNIT_dat, UNIT_out
      INTEGER  ios, ios_line, ios_data, i, ios_ith
      CHARACTER (LEN=1000) :: line_ith	 
! ----------------------------------------------------------------------

      UNIT_out  = 7
      UNIT_hd   = 91
      UNIT_dat  = 92

! ----------------------------------------------------------------------
! Open Output file
!      OPEN (UNIT=UNIT_out,FILE=fname_out,IOSTAT=ios)
      OPEN (UNIT=UNIT_out,FILE=fname_out,ACTION="WRITE",POSITION="REWIND",IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", fname_out
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------	   

! ----------------------------------------------------------------------
! Open HEADER file
      OPEN (UNIT=UNIT_hd,FILE=fname_header,IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", fname_header
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------	   

! ----------------------------------------------------------------------
! Open Data file
      OPEN (UNIT=UNIT_dat,FILE=fname_data,IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", fname_data
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------	   


! ----------------------------------------------------------------------	  
! Write Header file
! ----------------------------------------------------------------------	  
      i = 0
      DO
	     READ (UNIT=UNIT_hd,FMT=700,IOSTAT=ios_line) line_ith
	     i = i + 1
!  	     PRINT *, "READ Line (i,ios):", i, ios_line
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------
! Write to OUT file 
	     WRITE (UNIT=UNIT_out,FMT=700,IOSTAT=ios_ith) line_ith
         IF (ios_ith /= 0) THEN
            PRINT *, "Error in writing to file: ", TRIM (fname_out)
            PRINT *, "WRITE IOSTAT=", ios_ith
         END IF
! ----------------------------------------------------------------------
      END DO
      CLOSE (UNIT=UNIT_hd)

	  
! ----------------------------------------------------------------------	  
! Write Data file
! ----------------------------------------------------------------------	  
      i = 0
      DO
	     READ (UNIT=UNIT_dat,FMT=700,IOSTAT=ios_line) line_ith
	     i = i + 1
!  	     PRINT *, "READ Line (i,ios):", i, ios_line
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------
! Write to OUT file 
	     WRITE (UNIT=UNIT_out,FMT=700,IOSTAT=ios_ith) line_ith
         IF (ios_ith /= 0) THEN
            PRINT *, "Error in writing to file: ", TRIM (fname_out)
            PRINT *, "WRITE IOSTAT=", ios_ith
         END IF
! ----------------------------------------------------------------------
      END DO
      CLOSE (UNIT=UNIT_dat)

!      ENDFILE (UNIT = UNIT_out) 
      CLOSE (UNIT = UNIT_out)
	  
! ----------------------------------------------------------------------
! Format definition
700   FORMAT (A80)                                 
! ----------------------------------------------------------------------
  
      END
