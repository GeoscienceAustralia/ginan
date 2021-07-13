SUBROUTINE prm_emp (PRMfname, isVeq)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_emp.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read the configaration of the EMP parameters, dynamically 
! ----------------------------------------------------------------------
! Author :	Tzupang Tseng
!
! Copyright:	GEOSCIENCE AUSTRALIA 
!
! Created:	09-02-2021
! Changes:      
!
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config
      use pod_yaml
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (*), INTENT(IN) :: PRMfname				
      logical isVeq
! OUT

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: ForceMod  
      INTEGER (KIND = prec_int2) :: PD_Param_ID
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: filename				
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i, prn_index
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
      logical found
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: bias_r, bias_t, bias_n
      INTEGER (KIND = prec_int2) :: cpr_r, cpr_t, cpr_n, cpr_freq
      REAL (KIND = prec_q) :: Bias_radial, Bias_along, Bias_cross
      REAL (KIND = prec_q) :: Cterm, Sterm
      REAL(KIND = prec_q), DIMENSION(:), ALLOCATABLE :: EMP_PARA

      bias_r = -1
      bias_t = -1
      bias_n = -1
      cpr_r = -1
      cpr_t = -1
      cpr_n = -1
      cpr_freq = -1

! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      UNIT_IN = 9  												
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------
if (.not. yaml_found) then
! ----------------------------------------------------------------------
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------
! Read EMP parameterizations
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


! ----------------------------------------------------------------------
! Parameters Keywords read 
! ----------------------------------------------------------------------
! TODO: wrapper for yml_EMP_mode?
! ----------------------------------------------------------------------
! EMP parameters related to orbit force modelling
! ----------------------------------------------------------------------
!
! Radial direction
IF (word1_ln == "bias_r" .and. bias_r == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, bias_r 
!	EMP_Bias_glb(1) = bias_r
        if (bias_r /= 0) yml_srp_parameters = yml_srp_parameters + pow (2, EMP_R_Bias - one)
END IF
! Tangential direction
IF (word1_ln == "bias_t" .and. bias_t == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, bias_t 
!	EMP_Bias_glb(2) = bias_t
        if (bias_t /= 0) yml_srp_parameters = yml_srp_parameters + pow (2, EMP_T_Bias - one)
END IF
! Normal direction
IF (word1_ln == "bias_n" .and. bias_n == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, bias_n 
!	EMP_Bias_glb(3) = bias_n
        if (bias_n /= 0) yml_srp_parameters = yml_srp_parameters + pow (2, EMP_N_Bias - one)
END IF
! ----------------------------------------------------------------------
! Empirical parameters: Cycle per revolution accelerations
! Number of cycles per revolution
IF (word1_ln == "cpr_no" .and. cpr_freq == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_freq 
	yml_emp_cpr_count = cpr_freq
END IF
! 
! EMP_CPR_glb(1) : Bias acceleration in Radial direction
! EMP_CPR_glb(2) : Bias acceleration in Tangential direction
! EMP_CPR_glb(3) : Bias acceleration in Normal direction
! 
! 1. EMP_CPR_glb(i)=1 : Effect is considered
! 2. EMP_CPR_glb(i)=0 : Effect is neglected 
!
! Radial direction
IF (word1_ln == "cpr_r" .and. cpr_r == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_r 
        if (cpr_r /= 0) yml_srp_parameters = yml_srp_parameters + pow (2, EMP_R_CPR - one)
!	EMP_CPR_glb(1) = cpr_r
END IF
! Tangential direction
IF (word1_ln == "cpr_t" .and. cpr_t == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_t 
!	EMP_CPR_glb(2) = cpr_t
        if (cpr_t /= 0) yml_srp_parameters = yml_srp_parameters + pow (2, EMP_T_CPR - one)
END IF
! Normal direction
IF (word1_ln == "cpr_n" .and. cpr_n == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_n 
!	EMP_CPR_glb(3) = cpr_n
        if (cpr_n /= 0) yml_srp_parameters = yml_srp_parameters + pow (2, EMP_N_CPR - one)
END IF

END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------
else if (isVeq) then
yml_srp_parameters = yml_veq_srp_parameters
yml_emp_cpr_count = yml_veq_emp_cpr_count
EMPNUM = yml_veq_empnum
else
yml_srp_parameters = yml_eqm_srp_parameters
yml_emp_cpr_count = yml_eqm_emp_cpr_count
EMPNUM = yml_eqm_empnum
end if

! ----------------------------------------------------------------------
! Read the estimation values of the EMP model 
! ----------------------------------------------------------------------

IF (yml_EMP_mode) THEN
    filename = 'emp_est.in'
    PD_Param_ID = 0
    If (BTEST(yml_srp_parameters, EMP_R_Bias - one))  PD_Param_ID = PD_Param_ID + 1
    If (BTEST(yml_srp_parameters, EMP_T_Bias - one))  PD_Param_ID = PD_Param_ID + 1
    If (BTEST(yml_srp_parameters, EMP_N_Bias - one))  PD_Param_ID = PD_Param_ID + 1
    If (BTEST(yml_srp_parameters, EMP_R_CPR - one))   PD_Param_ID = PD_Param_ID + 2
    If (BTEST(yml_srp_parameters, EMP_T_CPR - one))   PD_Param_ID = PD_Param_ID + 2
    If (BTEST(yml_srp_parameters, EMP_N_CPR - one))   PD_Param_ID = PD_Param_ID + 2

!PRINT*,'Reading config,  PD_Param_ID =', PD_Param_ID

ALLOCATE (EMP_accel_glb(PD_Param_ID), STAT = AllocateStatus)
EMP_accel_glb = 0.0d0

ALLOCATE (EMP_PARA(PD_Param_ID), STAT = AllocateStatus)
EMP_PARA = 0.d0

END IF

! ----------------------------------------------------------------------
! Open .in file if EMP activated
IF (yml_EMP_mode) THEN

  if (.not. yaml_found) then
  OPEN (UNIT = UNIT_IN, FILE = TRIM (filename), IOSTAT = ios)

  IF (ios /= 0) THEN
    PRINT *, "Error in opening file:", filename
    PRINT *, "OPEN IOSTAT=", ios
  END IF

! Read input file
  i = 0
  DO
    READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
    i = i + 1
!PRINT *, "READ Line (i,ios):", i, ios_line

! End of file
    IF (ios_line < 0) THEN
	! PRINT *, "End of file, i=", i
      EXIT		
    END IF


! 1st Word of Line ith
    READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word

! ECOM parameters :: Bias accelerations
    IF (word1_ln=="EMP") THEN

      READ (line_ith, FMT = * , IOSTAT=ios_key) word_i, EMP_PARA(:)  
!print*,"SRP_PARA=",SRP_PARA(:)
    END IF

  END DO
!print*,'EMP_PARA =', EMP_PARA
CLOSE (UNIT=UNIT_IN)
! Close of input file
! ----------------------------------------------------------------------
else
  found = .false.
  do i = 1, prn_override_count
      if(yml_prn_overrides(i)%name .eq. trim(PRN)) then
          prn_index = i
          found = .true.
      end if
  end do
  ! possible we have not set up the overrides yet (first pass through)
  if (found) then
    EMP_PARA(1:EMPNUM) = yml_prn_overrides(prn_index)%integ%emp_init_values(1:EMPNUM)
  else
    EMP_PARA(1:EMPNUM) = 0.d0
  end if
end if

      PD_Param_ID = 0

      If (BTEST(yml_srp_parameters, EMP_R_Bias - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
      End IF

      If (BTEST(yml_srp_parameters, EMP_T_Bias - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
      End IF

      If (BTEST(yml_srp_parameters, EMP_N_Bias - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
      End IF

      If (BTEST(yml_srp_parameters, EMP_R_CPR - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
      End IF

      If (BTEST(yml_srp_parameters, EMP_T_CPR - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
      End IF

      If (BTEST(yml_srp_parameters, EMP_N_CPR - one)) Then
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
        PD_Param_ID = PD_Param_ID + 1
        EMP_accel_glb(PD_Param_ID) = EMP_PARA(PD_Param_ID)
      End If


Deallocate(EMP_Para, stat=DeAllocateStatus)

ENDIF

END
