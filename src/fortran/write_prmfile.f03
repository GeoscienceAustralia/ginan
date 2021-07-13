SUBROUTINE write_prmfile (fname, fname_id, param_id, param_value)


! ----------------------------------------------------------------------
! SUBROUTINE: write_prmfile.f03
! ----------------------------------------------------------------------
! Purpose:
!  Update the parameters of the input/configuration files with the estimated parameters values
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
      CHARACTER (LEN=100), INTENT(IN) :: fname				
      CHARACTER (LEN=50), INTENT(IN) :: fname_id				
      CHARACTER (LEN=100), INTENT(IN) :: param_id				
      CHARACTER (LEN=500), INTENT(IN) :: param_value				
! OUT

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Variables declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: fname1, fname2, fname3, fname4				
      CHARACTER (LEN=100) :: param_id0				
      CHARACTER (LEN=500) :: param_value0				
      INTEGER (KIND = prec_int8) :: fname_len
! ----------------------------------------------------------------------

if (yaml_found) return
  
fname_len = LEN(TRIM(ADJUSTL(fname)))
!print *, "fname_len", fname_len
	  
! Copy input file
!fname1 = 'emp_bias.in'
!fname2 = 'emp_bias0.in'
fname1 = fname
write (fname2, FMT='(a)') TRIM(ADJUSTL(fname(1:fname_len-3)))//TRIM(ADJUSTL(fname_id))//".in"
!print *, "fname2", fname2
param_id0 =    '999'
param_value0 = '999'
Call writeparam (fname1, fname2, param_id0, param_value0)

! Write the estimated parameters in the input files
fname3 = fname2
fname4 = fname1
Call writeparam (fname3, fname4, param_id, param_value)


END
