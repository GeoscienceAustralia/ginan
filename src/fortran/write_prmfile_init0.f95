SUBROUTINE write_prmfile_init0 (fname, param_id, param_value)


! ----------------------------------------------------------------------
! SUBROUTINE: write_prmfile_init.f03
! ----------------------------------------------------------------------
! Purpose:
!  Create parameters initialization file 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Geoscience Australia, CRC-SI
! Created:  28 May 2019
! ----------------------------------------------------------------------
        
        
      USE mdl_precision
      USE mdl_num
      use mdl_config
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: fname                    
      !CHARACTER (LEN=100), ALLOCATABLE, INTENT(IN) :: param_id_array                     
      !CHARACTER (LEN=500), ALLOCATABLE, INTENT(IN) :: param_value_array                        
      CHARACTER (LEN=100), INTENT(IN) :: param_id                       
      CHARACTER (LEN=500), INTENT(IN) :: param_value                    

! OUT

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: UNIT_IN, UNIT_IN2, ios
        LOGICAL :: file_existence
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith       
      CHARACTER (LEN=150) :: word1_ln, word_i, t0       
! ----------------------------------------------------------------------
      !INTEGER (KIND = prec_int8) :: Nparam, Nparam2
      !CHARACTER (LEN=100) :: param_id                      
      !CHARACTER (LEN=500) :: param_value                   
! ----------------------------------------------------------------------
! Parameters arrays size
! ----------------------------------------------------------------------
!Nparam = size(param_id_array, DIM = 1)
!Nparam2 = size(param_value_array, DIM = 1)
!IF (Nparam /= Nparam2) THEN
!   PRINT *, "Error:: Number of parameters written do not match in the initialization file:", fname
!   PRINT *, "OPEN IOSTAT=", ios
!END IF
! ----------------------------------------------------------------------

if (yaml_found) return

! Check file existence
INQUIRE(FILE=fname, EXIST=file_existence)
!print *, "file_existence", file_existence
  
! ----------------------------------------------------------------------
! Create/Open file
UNIT_IN  = 9                                                                        
!IF (file_existence .EQ. .TRUE.) THEN
IF (file_existence) THEN

OPEN (UNIT=UNIT_IN, FILE=TRIM(fname), STATUS="OLD", ACTION="WRITE", IOSTAT = ios)
IF (ios /= 0) THEN
   PRINT *, "Error in opening file:", fname
   PRINT *, "OPEN IOSTAT=", ios
END IF

!ELSE IF (file_existence .EQ. .FALSE.) THEN
ELSE 

OPEN (UNIT=UNIT_IN, FILE=TRIM(fname), STATUS="NEW", ACTION="WRITE", IOSTAT = ios)
IF (ios /= 0) THEN
   PRINT *, "Error in opening file:", fname
   PRINT *, "OPEN IOSTAT=", ios
END IF

END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Parameters Keyword read 
!param_id = param_id_array(i)
!param_value = param_value_array_array(i)

! Write parameters to file 
WRITE (UNIT=UNIT_IN, FMT=*, IOSTAT=ios) TRIM(ADJUSTL(param_id)), '                  ', TRIM(param_value)
! ----------------------------------------------------------------------
!PRINT *, "WRITE param_id, param_value: ", param_id, param_value


ENDFILE (UNIT=UNIT_IN) 
CLOSE (UNIT=UNIT_IN)
! Close of files
! ----------------------------------------------------------------------


END SUBROUTINE
