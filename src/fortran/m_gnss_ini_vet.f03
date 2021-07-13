MODULE m_gnss_ini_vet


      IMPLICIT NONE
      !SAVE 			
  
  
Contains
  
  
SUBROUTINE gnss_ini_vet (EQMfname, ini_vet)
! ----------------------------------------------------------------------
! SUBROUTINE:  gnss_ini_vet.f03
! ----------------------------------------------------------------------
! Purpose:
!  This program is used to read the SP3 file and output the initial state vector
!
!
! ----------------------------------------------------------------------
! Input arguments:
! - EQMfname:           Configuration file name for the orbit parameterization
!
! Output arguments:
!
! - ini_vet:            An array is used to store the initial state vectors of
!                       GNSS satellite.
!                       ini_vet (PRN:1:vectors)
! ----------------------------------------------------------------------
! Author :      Tzupang tseng, Geoscience Australia, Australia
!
! Crreate:      06-09-2018
!
! Changes:
! ----------------------------------------------------------------------



      USE mdl_precision
      USE mdl_param
      USE m_sat_ini_vet
      IMPLICIT NONE
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: EQMfname
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: ini_vet
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local Variables declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=300) :: fname_orb
      CHARACTER (LEN=300) :: fname_write

      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith
      CHARACTER (LEN=150) :: word1_ln, word_i, t0
      CHARACTER (LEN=30) :: fmt_line


! ----------------------------------------------------------------------
      UNIT_IN = 9
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (EQMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", EQMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------
  
! ----------------------------------------------------------------------
! Read input file
i = 0
DO

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1
! PRINT *, "READ Line (i,ios):", i, ios_line

! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
!READ (line_ith, * , IOSTAT=ios_data) word1_ln, charN
! ----------------------------------------------------------------------
!PRINT *, "word1_ln: ", word1_ln


! GNSS orbit data (sp3) file name
IF (word1_ln == "pseudobs_filename") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, fname_orb
END IF
! ----------------------------------------------------------------------

END DO
CLOSE (UNIT=UNIT_IN)


! Satellite position and velocity are based on ITRF
! Generate the initial state vectors for multi-GNSS satellitesn
CALL sat_ini_vet (fname_orb, ini_vet)


END SUBROUTINE

End

