SUBROUTINE write_prmfile2 (fname, fname_id, fname_out)


! ----------------------------------------------------------------------
! SUBROUTINE: write_prmfile2.f03
! ----------------------------------------------------------------------
! Purpose:
!  Copy the input/configuration file to new configuration file
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	14 May 2019
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
      CHARACTER (LEN=50) , INTENT(IN) :: fname_id				
      !CHARACTER (LEN=100), INTENT(IN) :: param_id				
      !CHARACTER (LEN=500), INTENT(IN) :: param_value				
! OUT
      CHARACTER (LEN=100), INTENT(OUT) :: fname_out				
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
fname1 = fname
!fname2 = fname_out

! fname2
write (fname2, FMT='(a)') TRIM(ADJUSTL(fname(1:fname_len-3)))//TRIM(ADJUSTL(fname_id))//".in"
!print *, "fname2", fname2

! Copy input file data to fname2
param_id0 =    '999'
param_value0 = '999'
Call writeparam (fname1, fname2, param_id0, param_value0)

fname_out = fname2

!print *,"fname2    ", fname2
!print *,"fname_out ", fname_out

!! Write the estimated parameters in the input files
!fname3 = fname2
!fname4 = fname1
!Call writeparam (fname3, fname4, param_id, param_value)


END
