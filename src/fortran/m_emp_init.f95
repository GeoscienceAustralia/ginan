MODULE m_emp_init

contains

SUBROUTINE emp_init (EMP_0_coef)


! ----------------------------------------------------------------------
! SUBROUTINE: emp_init
! ----------------------------------------------------------------------
! Purpose: Initialize the EMP model 
! ----------------------------------------------------------------------
! Input arguments:
! - EMP_0_coef: 	Array for the initialization of the EMP model  
! ----------------------------------------------------------------------
! Author :	Tzupang Tseng,  Geoscience Australia
!
! Created:	10-02-2021 
!
! Changes:      
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
       REAL (KIND = prec_q), DIMENSION(:), INTENT(OUT), ALLOCATABLE :: EMP_0_coef
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, ii, PD_Param_ID 
      INTEGER (KIND = prec_int2) :: AllocateStatus
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: fname 				
      CHARACTER (LEN=50) :: fname_id				
      CHARACTER (LEN=100) :: param_id				
      CHARACTER (LEN=500) :: param_value				
      logical found
! ----------------------------------------------------------------------
! Initial conditions for empirical model 
! ----------------------------------------------------------------------
ALLOCATE (EMP_0_coef(EMPNUM))
EMP_0_coef = 0.d0
IF (yml_ic_input_format == IC_FILE) THEN
        EMP_0_coef = IC_sat_glb (9:8+EMPNUM)
END IF
!print*,'EMP_0_coef=',EMP_0_coef

if (.not. yaml_found) then
fname = 'emp_est.in'
param_id = 'EMP'
write (param_value, *) EMP_0_coef
!Call write_prmfile (fname, fname_id, param_id, param_value)
CALL write_prmfile_init0 (fname, param_id, param_value)
else
    found = .false.
    do i=1, prn_override_count
    if (.not. found .and. yml_prn_overrides(i)%name .eq. trim(PRN)) then
        yml_prn_overrides(i)%integ%emp_init_values(1:EMPNUM) = EMP_0_coef(1:EMPNUM)
        yml_prn_overrides(i)%integ%emp_parameters_used = EMPNUM
        found = .true.
    end if
    end do
    if (.not. found) then
        i = new_prn_override(PRN)
        yml_prn_overrides(i)%integ%emp_init_values(1:EMPNUM) = EMP_0_coef(1:EMPNUM)
        yml_prn_overrides(i)%integ%emp_parameters_used = EMPNUM
    end if
end if


END SUBROUTINE

END MODULE
