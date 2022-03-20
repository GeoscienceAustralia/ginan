MODULE m_ecom_init

contains

SUBROUTINE ecom_init (ECOM_0_coef)


! ----------------------------------------------------------------------
! SUBROUTINE: ecom_init
! ----------------------------------------------------------------------
! Purpose: Initialize the ECOM model 
! ----------------------------------------------------------------------
! Input arguments:
! - ECOM_0_coef: 	Array for the initialization of the ECOM model  
! ----------------------------------------------------------------------
! Author :	Tzupang Tseng,  Geoscience Australia
!
! Created:	28-05-2019 
!
! Changes:      03-12-2019 Tzupang Tseng: a option for the simple box wing  
!                                         is created
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config
      USE mdl_param
      use pod_yaml
      IMPLICIT NONE
	  
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
       REAL (KIND = prec_q), DIMENSION(:), INTENT(OUT), ALLOCATABLE :: ECOM_0_coef
! ----------------------------------------------------------------------
!      INTEGER (KIND = prec_int2) :: Niteration,srp_i 
      INTEGER (KIND = prec_int2) :: srp_i 
      INTEGER (KIND = prec_int8) :: i, ii, PD_Param_ID 
      INTEGER (KIND = prec_int2) :: AllocateStatus
      INTEGER (KIND = prec_int4) :: prn_index
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: fname 				
      CHARACTER (LEN=50) :: fname_id				
      CHARACTER (LEN=100) :: param_id				
      CHARACTER (LEN=500) :: param_value				
      logical found

! ----------------------------------------------------------------------
! Initial conditions for solar radiation pressure
! ----------------------------------------------------------------------
!ALLOCATE (ECOM_0_coef(NPARAM_glb))
ALLOCATE (ECOM_0_coef(ECOMNUM))
srp_i = yml_ECOM_mode
!i = Niteration
!write (fname_id, *) i
ECOM_0_coef = 0.d0
IF (yml_ic_input_format == IC_FILE) THEN
!	ECOM_0_coef = IC_sat_glb (9:8+NPARAM_glb)
        ECOM_0_coef = IC_sat_glb (9:8+ECOMNUM)
END IF
!print*,'ECOM_0_coef=',ECOM_0_coef

if (.not. yaml_found) then

IF (srp_i == ECOM1) THEN
fname = 'ECOM1_srp.in'
param_id = 'ECOM1'
write (param_value, *) ECOM_0_coef
!Call write_prmfile (fname, fname_id, param_id, param_value)
CALL write_prmfile_init0 (fname, param_id, param_value)
END IF


IF (srp_i == ECOM2) THEN
fname = 'ECOM2_srp.in'
param_id = 'ECOM2'
write (param_value, *) ECOM_0_coef
!Call write_prmfile (fname, fname_id, param_id, param_value)
CALL write_prmfile_init0 (fname, param_id, param_value)
END IF


IF (srp_i == ECOM_HYBRID) THEN
fname = 'ECOM12_srp.in'
param_id = 'ECOM12'
write (param_value, *) ECOM_0_coef
!Call write_prmfile (fname, fname_id, param_id, param_value)
CALL write_prmfile_init0 (fname, param_id, param_value)
END IF


IF (srp_i == SBOXW) THEN
fname = 'SBOXW_srp.in'
param_id = 'SBOXW'
write (param_value, *) ECOM_0_coef
!Call write_prmfile (fname, fname_id, param_id, param_value)
CALL write_prmfile_init0 (fname, param_id, param_value)
END IF

else

    found = .false.
    do i=1, prn_override_count
    if (.not. found .and. yml_prn_overrides(i)%name .eq. trim(PRN)) then
        yml_prn_overrides(i)%integ%ecom_init_values(1:ECOMNUM) = ECOM_0_coef(1:ECOMNUM)
        yml_prn_overrides(i)%integ%ecom_parameters_used = ECOMNUM
        found = .true.
    end if
    end do
    if (.not. found) then
        i = new_prn_override(PRN)
        yml_prn_overrides(i)%integ%ecom_init_values(1:ECOMNUM) = ECOM_0_coef(1:ECOMNUM)
        yml_prn_overrides(i)%integ%ecom_parameters_used = ECOMNUM
    end if

end if

END SUBROUTINE

END MODULE
