SUBROUTINE writeparam1 (fname1, param_id, param_value)


! ----------------------------------------------------------------------
! SUBROUTINE: writeparam1.f03
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
      CHARACTER (LEN=100), INTENT(IN) :: fname1				
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
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
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
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Open .in file
!OPEN (UNIT=UNIT_IN, FILE=TRIM(fname1), ACTION="READWRITE", IOSTAT = ios)
OPEN (UNIT=UNIT_IN, FILE=fname1, IOSTAT = ios)
IF (ios /= 0) THEN
	PRINT *, "Error in opening file:", fname1
	PRINT *, "OPEN IOSTAT=", ios
END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read input file
i = 0
DO

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1
PRINT *, "READ Line (i,ios):", i, ios_line, line_ith

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
PRINT *, "word1_ln: ", word1_ln


! ----------------------------------------------------------------------
! Parameters Keyword read 
IF (word1_ln == param_id) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, param_apriori 
	!PRINT *, "writeparam.f03 :: word_i ", word_i
	!PRINT *, "writeparam.f03 :: param_apriori ", param_apriori
   
! Replacement with the estimated value
Backspace (UNIT=UNIT_IN,IOSTAT=ios)  

! Write data to file | Write data line 
WRITE (UNIT=UNIT_IN, FMT=*, IOSTAT=ios) TRIM(param_id), TRIM(param_aposteriori)
!PRINT *, "param_aposteriori: ", param_aposteriori

IF (ios /= 0) THEN
   PRINT *, "Error in writing to file: ", TRIM (fname1)
   PRINT *, "WRITE IOSTAT=", ios
END IF

!EXIT

END IF
! ----------------------------------------------------------------------


END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------



END
