MODULE m_read_leapsec

! ----------------------------------------------------------------------
! MODULE: m_read_leapsec.f03
! ----------------------------------------------------------------------
! Purpose:
!
!  Module for reading leap.second file and stroing info in arrays for
!  later use
! 
! ----------------------------------------------------------------------
! Author :	Simon McClusky, Geoscience Australia, Australia
! Created:	09 July 2019
! ----------------------------------------------------------------------

      IMPLICIT NONE
!      SAVE		
  	  
Contains
  
SUBROUTINE read_leapsec (leapsec_filename)

! ----------------------------------------------------------------------
! SUBROUTINE: read_leapsec.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read and store leap second information
! ----------------------------------------------------------------------
! Input arguments:
! - leapsec_filename:	Name of leap.second file containing UTC-TAI increments and dates
!
! Ouptut arguments: (None)
! ----------------------------------------------------------------------
! Remarks:
!  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy argument declarations
! ----------------------------------------------------------------------
      CHARACTER (LEN=*), INTENT(IN)                           :: leapsec_filename

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: AllocateStatus
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=100) :: line_ith
      CHARACTER (LEN=50) :: word1_ln, word_i

! ----------------------------------------------------------------------
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'

! ----------------------------------------------------------------------
! Open leap.second file
      OPEN (UNIT = UNIT_IN, FILE = TRIM(leapsec_filename), status='old', IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening leap second file:", leapsec_filename
         PRINT *, "OPEN IOSTAT=", ios
         STOP
      END IF
      
! ----------------------------------------------------------------------
! Read the expected number of leap second entries in file
! ----------------------------------------------------------------------
      i = 0
      DO
         READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
	 i = i + 1
!  	 PRINT *, "READ Line (i,ios_line):", i, ios_line
         IF (ios_line /= 0) THEN
            PRINT *, "Error reading NDAT: line from file:", leapsec_filename
            PRINT *, "OPEN IOSTAT=", ios_line
         END IF
         
! ----------------------------------------------------------------------
! 1st Word of Line ith
  	 READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
	 IF (ios_data /= 0) THEN
            PRINT *, "Error reading NDAT: keyword from file:", line_ith, word1_ln, leapsec_filename
            PRINT *, "READ IOSTAT=", ios_data
         ENDIF
                     
! ----------------------------------------------------------------------
! Look for the expected number of entries ginve by "NDAT:" keyword
         IF (word1_ln == "NDAT:") THEN
	    READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, NDAT     
!           PRINT *, "NDAT =", NDAT

	    IF (ios_key /= 0) THEN
               PRINT *, "Error reading NDAT: value from file:", line_ith, word_i, leapsec_filename
               PRINT *, "OPEN IOSTAT=", ios_key 
            ENDIF
            EXIT        
         END IF
         
! ----------------------------------------------------------------------
      END DO
        	 
! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------
      ALLOCATE (IDAT(2,NDAT), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE read_leapsec in m_read_leapsec.f03"
         PRINT *, "Error: Allocatable Array: IDAT, NDAT =", NDAT
!         STOP "*** Not enough memory ***"
      END IF  
	  
      ALLOCATE (DATS(NDAT), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE read_leapsec in m_read_leapsec.f03"
         PRINT *, "Error: Allocatable Array: DATS, NDAT =", NDAT
!         STOP "*** Not enough memory ***"
      END IF  
	  
! ----------------------------------------------------------------------
! Read the leap second increment vaules into a allocates arrays
! ----------------------------------------------------------------------
      i = 0
      DO
         READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
         
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
            PRINT *,'Success: End of ',trim(leapsec_filename), ' found, ',i,'values read'
            PRINT *,'Final entry in ',trim(leapsec_filename), ' is, ',IDAT(:,i),DATS(i)
            EXIT		
         END IF

!  	 PRINT *, 'READ Line (i,line_ith,ios_line):',i,line_ith,ios_line         
	 i = i + 1         
! ----------------------------------------------------------------------
! Read leap second data records
	 READ (line_ith, * , IOSTAT=ios_data) IDAT(1,i),IDAT(2,i),DATS(i)
!         PRINT*,'i, IDAT(:,i), DATS(i): ',i, IDAT(:,i),DATS(i)  
! ----------------------------------------------------------------------
      END DO
 
! ----------------------------------------------------------------------
      CLOSE (UNIT=UNIT_IN)
      	  
END SUBROUTINE

END
