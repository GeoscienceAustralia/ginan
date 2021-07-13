SUBROUTINE writeparam (fname1, fname2, param_id, param_value)


! ----------------------------------------------------------------------
! SUBROUTINE: writeparam.f03
! ----------------------------------------------------------------------
! Purpose:
!  Update the input parameters of the configuration files with the estimated parameters values
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, CRC-SI
! Created:	18 October 2018
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      use mdl_config
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: fname1, fname2				
      CHARACTER (LEN=100), INTENT(IN) :: param_id				
      CHARACTER (LEN=500), INTENT(IN) :: param_value				
! OUT

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: ForceMod  
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: filename				
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, UNIT_IN2, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: bias_r, bias_t, bias_n
      INTEGER (KIND = prec_int2) :: cpr_r, cpr_t, cpr_n, cpr_freq
      REAL (KIND = prec_q) :: Bias_radial, Bias_along, Bias_cross
      REAL (KIND = prec_q) :: Cterm, Sterm
! ----------------------------------------------------------------------
      CHARACTER (LEN=500) :: param_apriori, param_aposteriori 	  


      if (yaml_found) return

param_aposteriori = param_value

! ----------------------------------------------------------------------
      UNIT_IN  = 9  												
      UNIT_IN2 = 99  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Open files
OPEN (UNIT=UNIT_IN , FILE=fname1, ACTION="READ", IOSTAT = ios)
OPEN (UNIT=UNIT_IN2, FILE=fname2, ACTION="READWRITE", POSITION="REWIND", IOSTAT = ios)

!OPEN (UNIT=UNIT_IN , FILE=TRIM(fname1), ACTION="READ", IOSTAT = ios)
!OPEN (UNIT=UNIT_IN2, FILE=TRIM(fname2), ACTION="READWRITE", POSITION="REWIND", IOSTAT = ios)
!OPEN (UNIT=UNIT_IN2, FILE=TRIM(fname2), ACTION="WRITE", IOSTAT = ios)
!OPEN (UNIT=UNIT_IN2, FILE=TRIM(fname2), ACTION="WRITE", POSITION="REWIND", IOSTAT = ios)
!OPEN (UNIT=UNIT_IN , FILE=TRIM(fname1), IOSTAT = ios)
!OPEN (UNIT=UNIT_IN2, FILE=TRIM(fname2), IOSTAT = ios)

!IF (ios /= 0) THEN
!   PRINT *, "Error in opening file:", filename
!    PRINT *, "OPEN IOSTAT=", ios
!END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Read input file
i = 0
DO

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1
!PRINT *, "READ Line (i,ios):", i, ios_line

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
!PRINT *, "line_ith: ", TRIM(ADJUSTL(line_ith))
!print *,"param_id ", param_id

! ----------------------------------------------------------------------
! Parameters Keyword read 
IF (word1_ln == param_id) THEN

! Write data to file | Write data line 
WRITE (UNIT=UNIT_IN2, FMT=*, IOSTAT=ios) TRIM(ADJUSTL(param_id)), '			', TRIM(param_aposteriori)
!PRINT *, "WRITE Line: param_aposteriori: ", param_aposteriori

Else

! Write data to file | Write data line 
WRITE (UNIT=UNIT_IN2, FMT=*, IOSTAT=ios) TRIM(ADJUSTL(line_ith))
!PRINT *, "WRITE Line: ", line_ith
 
END IF
! ----------------------------------------------------------------------
word1_ln = ''

END DO

ENDFILE (UNIT=UNIT_IN2) 

CLOSE (UNIT=UNIT_IN)
CLOSE (UNIT=UNIT_IN2)
! Close of files
! ----------------------------------------------------------------------



END
