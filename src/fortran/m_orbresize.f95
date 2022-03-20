MODULE m_orbresize


! ----------------------------------------------------------------------
! MODULE: m_orbresize.f95
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the orbresize subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia, Frontier-SI
! Created:	12 May 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
SUBROUTINE orbresize (orbmatrix0, steprate, orbmatrix1) 


! ----------------------------------------------------------------------
! SUBROUTINE: orbresize
! ----------------------------------------------------------------------
! Purpose:
!  Modify the dimensions of the Orbit and Partials matrices based on the 
!  input epochs rate 
! ----------------------------------------------------------------------
! Input arguments:
! - orbmatrix0: 	Orbit matrix including the overall number of epochs 
!
! Output arguments:
! - orbmatrix1: 	Orbit matrix with reduced number of epochs 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia, Frontier-SI
! Created:	12 June 2019
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      use mdl_config
!      USE mdl_num
!      USE mdl_param
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: orbmatrix0 
      INTEGER (KIND = prec_int8), INTENT(IN) :: steprate
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: orbmatrix1
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, j
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int8) :: Nepochs, Nepochs_0, Nepochs_1, N2_0
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
      CHARACTER (LEN=100) :: mesg
! ----------------------------------------------------------------------


! Input matrix
sz1 = size(orbmatrix0, DIM = 1)
sz2 = size(orbmatrix0, DIM = 2)
Nepochs_0 = sz1
N2_0 = sz2
!PRINT *,"sz1,sz2",sz1,sz2

IF (steprate > 0) THEN
! Allocate output matrix
Nepochs_1 = (Nepochs_0 - 1) / steprate +1
!PRINT *,"Nepochs_0, Nepochs_1", Nepochs_0, Nepochs_1

ALLOCATE (orbmatrix1(Nepochs_1, N2_0), STAT = AllocateStatus)
IF (AllocateStatus /= 0) then
        write(mesg, *) "Not enough memory - Array orbmatrix1, dimension = (", &
                Nepochs_1, ",", N2_0, ")"
        call report ('FATAL', pgrm_name, 'orbresize', mesg, 'src/fortran/m_orbresize.f95', 1)
end if
orbmatrix1 = 0.0D0

j = 0 
Do i = 1 , Nepochs_0 , steprate
   j = j + 1
   orbmatrix1(j, 1:N2_0) = orbmatrix0(i,1:N2_0)  
End Do

ELSE IF (steprate <= 0) THEN

! Output matrix is set equal to the input matrix
ALLOCATE (orbmatrix1(Nepochs_0, N2_0), STAT = AllocateStatus)
IF (AllocateStatus /= 0) then
        write(mesg, *) "Not enough memory - Array orbmatrix1, dimension = (", &
                Nepochs_0, ",", N2_0, ")"
        call report ('FATAL', pgrm_name, 'orbresize', mesg, 'src/fortran/m_orbresize.f95', 1)
end if
orbmatrix1 = orbmatrix0

END IF


 100 END SUBROUTINE


End MODULE

