SUBROUTINE prm_srp (PRMfname, isVeq)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_srp.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read the configaration of the SRP parameters 
! ----------------------------------------------------------------------
! Author :	Tzupang Tseng,  Thomas Papanikolaou
!
! Copyright:	GEOSCIENCE AUSTRALIA 
!
! Created:	13 DEC 2018
!
! Changes:      13-12-2018 Tzupang Tseng : added ECOM-based options in the configuration file
!                                              for ACS POD
!               21-02-2019 Tzupang Tseng : added a function for switching on and
!                                          off some coefficients in ECOM models   
!               03-12-2019 Tzupang Tseng : added a function of estimating
!                                          parameters in simple box wing model
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
      INTEGER (KIND = prec_int8) :: i, read_i, prn_index
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i
      INTEGER (KIND = prec_int2) :: ECOMNUM_orig
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: bias_D, bias_Y, bias_B
      INTEGER (KIND = prec_int2) :: cpr_D, cpr_Y, cpr_B, cpr_freq
      INTEGER (KIND = prec_int2) :: cpr_D2, cpr_D4
      REAL(KIND = prec_q), DIMENSION(:), ALLOCATABLE :: SRP_PARA
      logical found

! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

        bias_D = -1
        bias_Y = -1
        bias_B = -1
        cpr_D = -1
        cpr_Y = -1
        cpr_B = -1
        cpr_D2 = -1
        cpr_D4 = -1
        ECOMNUM_orig = ECOMNUM

        
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
! Read SRP parameterizations
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

! ----------------------------------------------------------------------
! ECOM parameters related to orbit force modelling
! ----------------------------------------------------------------------
!
IF(yml_ECOM_mode/=ECOM_NONE .and.yml_ECOM_mode/=SBOXW) THEN

! D direction
   IF (word1_ln == "bias_D" .and. bias_D == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, bias_D 
!	ECOM_Bias_glb(1) = bias_D
        if (bias_D > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_D_Bias - one)
        !print *, "isVeq = ", isVeq, ", bias_D = ", bias_D
   END IF
! Y direction
   IF (word1_ln == "bias_Y" .and. bias_Y == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, bias_Y 
	!ECOM_Bias_glb(2) = bias_Y
        if (bias_Y > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_Y_Bias - one)
        !print *, "isVeq = ", isVeq, ", bias_Y = ", bias_Y
   END IF
! B direction
   IF (word1_ln == "bias_B" .and. bias_B == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, bias_B 
	!ECOM_Bias_glb(3) = bias_B
        if (bias_B > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_B_Bias - one)
        !print *, "isVeq = ", isVeq, ", bias_B = ", bias_B
   END IF
! D direction
   IF (word1_ln == "cpr_D" .and. cpr_D == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_D 
	!ECOM_CPR_glb(1) = cpr_D
        if (cpr_D > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_D_CPR - one)
        !print *, "isVeq = ", isVeq, ", cpr_D = ", cpr_D 
   END IF
! Y direction
   IF (word1_ln == "cpr_Y" .and. cpr_Y == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_Y 
	!ECOM_CPR_glb(2) = cpr_Y
        if (cpr_Y > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_Y_CPR - one)
        !print *, "isVeq = ", isVeq, ", cpr_Y = ", cpr_Y 
   END IF
! B direction
   IF (word1_ln == "cpr_B" .and. cpr_B == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_B 
	!ECOM_CPR_glb(3) = cpr_B
        if (cpr_B > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_B_CPR - one)
        !print *, "isVeq = ", isVeq, ", cpr_B = ", cpr_B 
   END IF

   IF (word1_ln == "cpr_D2" .and. cpr_D2 == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_D2
        !ECOM_CPR_glb(4) = cpr_D2
        if (cpr_D2 > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_D_2_CPR - one)
        !print *, "isVeq = ", isVeq, ", cpr_D2 = ", cpr_D2 
   END IF

   IF (word1_ln == "cpr_D4" .and. cpr_D4 == -1) THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, cpr_D4
        !ECOM_CPR_glb(5) = cpr_D4
        if (cpr_D4 > 0) yml_srp_parameters = yml_srp_parameters + pow (2, ECOM_D_4_CPR - one)
        !print *, "isVeq = ", isVeq, ", cpr_D4 = ", cpr_D4
   END IF

! ----------------------------------------------------------------------
! SBOXW is applied
! **********************************************************************
ELSE IF (yml_ECOM_mode == SBOXW) THEN

   !ECOM_Bias_glb = 0.d0
   !ECOM_CPR_glb  = 0.d0

ELSE IF(yml_ECOM_mode == ECOM_NONE) THEN

!PRINT*,'ECOM NOT ACTIVATED'

END IF

END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------
else if (isVeq) then
yml_srp_parameters = yml_veq_srp_parameters
ECOMNUM = yml_veq_ecomnum
else
yml_srp_parameters = yml_eqm_srp_parameters
ECOMNUM = yml_eqm_ecomnum
end if

! ----------------------------------------------------------------------
! Read the estimation values of the SRP model 
! ----------------------------------------------------------------------
IF (yml_ECOM_mode == ECOM1) THEN

! Read the input file
filename = 'ECOM1_srp.in'

ELSE IF (yml_ECOM_mode == ECOM2) THEN

filename = 'ECOM2_srp.in'

ELSE IF (yml_ECOM_mode == SBOXW) THEN

filename = 'SBOXW_srp.in'
ECOMNUM = 7

ELSE IF (yml_ECOM_mode == ECOM_HYBRID) THEN

filename = 'ECOM12_srp.in'

END IF

PD_param_ID = 1 ! yml_ECOM_mode == ECOM_NONE

IF (yml_ECOM_mode/=ECOM_NONE .and.yml_ECOM_mode/=SBOXW) THEN
    PD_Param_ID = 0
    If (BTEST(yml_srp_parameters, ECOM_D_Bias - one)) then
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_Y_Bias - one)) then
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_B_Bias - one)) then
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_D_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
! S term
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_Y_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
! S term
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_B_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
! S term
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
! S term
        PD_Param_ID = PD_Param_ID + 1
    End IF
    If (BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
! S term
        PD_Param_ID = PD_Param_ID + 1
    End IF

END IF

IF (yml_ECOM_mode == SBOXW) PD_Param_ID = 7

!PRINT*,'Reading config,  PD_Param_ID =', PD_Param_ID

ALLOCATE (ECOM_accel_glb(PD_Param_ID), STAT = AllocateStatus)

IF(yml_ECOM_mode==ECOM_NONE)THEN

RETURN

ELSE

ALLOCATE (SRP_PARA(PD_Param_ID), STAT = AllocateStatus)
SRP_PARA = 0.d0

END IF

! ----------------------------------------------------------------------
! Open .in file if ECOM activated
IF (yml_ECOM_mode /= ECOM_NONE .and. ECOMNUM_orig > 0) THEN

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

    READ (line_ith, FMT = * , IOSTAT=ios_key) word_i, SRP_PARA(:)
!print*,"SRP_PARA=",SRP_PARA(:)

  END DO

! Close of input file
! ----------------------------------------------------------------------
  CLOSE (UNIT=UNIT_IN)
  else
  !print *, "Sat PRN = ", PRN
  found = .false.
  do i = 1, prn_override_count
      if(yml_prn_overrides(i)%name .eq. trim(PRN)) then
          prn_index = i
          found = .true.
      end if
  end do
  if (found) then
      SRP_PARA(1:PD_Param_ID) = yml_prn_overrides(prn_index)%integ%ecom_init_values(1:PD_Param_ID)
  else
      SRP_PARA(1:PD_Param_ID) = 0.d0
  end if
  end if
ENDIF

if (yml_ECOM_mode /= SBOXW) then
      PD_Param_ID = 0
!print*,"SRP_PARA=",SRP_PARA(:)
      If (BTEST(yml_srp_parameters, ECOM_D_Bias - one)) then
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'D0=',PD_Param_ID
      End IF

      If (BTEST(yml_srp_parameters, ECOM_Y_Bias - one)) then
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'Y0=',PD_Param_ID
      End IF

      If (BTEST(yml_srp_parameters, ECOM_B_Bias - one)) then
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'B0=',PD_Param_ID
      End IF

      If (BTEST(yml_srp_parameters, ECOM_D_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'DC=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'DS=',PD_Param_ID
      End IF

      If (BTEST(yml_srp_parameters, ECOM_Y_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'YC=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'YS=',PD_Param_ID
      End IF

      If (BTEST(yml_srp_parameters, ECOM_B_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'BC=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'BS=',PD_Param_ID
      End If

      If (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'D2C=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'D2S=',PD_Param_ID
      End If

      If (BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'D4C=',PD_Param_ID
! S term
        PD_Param_ID = PD_Param_ID + 1
        ECOM_accel_glb(PD_Param_ID) = SRP_PARA(PD_Param_ID)
!print*,'D4S=',PD_Param_ID
      End If
else
      DO PD_Param_ID =1, 7 
        ECOM_accel_glb(PD_Param_ID)= SRP_PARA(PD_Param_ID)
      END DO
end if

DEALLOCATE(SRP_PARA, stat=DeallocateStatus)
END
