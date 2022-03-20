MODULE m_matrixreverse


! ----------------------------------------------------------------------
! MODULE: m_matrixreverse.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the matrixreverse subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia, Frontier-SI
! Created:	12 September 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
SUBROUTINE matrixreverse (matrix0) 


! ----------------------------------------------------------------------
! SUBROUTINE: matrixreverse
! ----------------------------------------------------------------------
! Purpose:
!  Reverse the input matrix through reversing the rows series
! ----------------------------------------------------------------------
! Input arguments:
! - matrix0: 	Input matrix 
!
! Output arguments:
! - matrix1: 	Output matrix with reversed series of the rows 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia, Frontier-SI
! Created:	12 September 2019
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
!      USE mdl_num
!      USE mdl_param
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: matrix0 
! ----------------------------------------------------------------------
! OUT
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: matrix1
! ----------------------------------------------------------------------
! INOUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: matrix0 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i
!       , j
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int8) :: Nepochs_0, N2_0
!       , Nepochs, , Nepochs_1, 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
! ----------------------------------------------------------------------
      !REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: matrix0 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: matrix1


! Input matrix
sz1 = size(matrix0, DIM = 1)
sz2 = size(matrix0, DIM = 2)
Nepochs_0 = sz1
N2_0 = sz2
!PRINT *,"sz1, sz2 ",sz1,sz2

ALLOCATE (matrix1(Nepochs_0, N2_0), STAT = AllocateStatus)
IF (AllocateStatus /= 0) PRINT *, "ALLOCATE Error: Module m_matrixreverse.f03, Array matrix1"
matrix1 = 0.0D0

Do i = 1 , Nepochs_0
   matrix1(Nepochs_0+1-i, 1:N2_0) = matrix0(i,1:N2_0)  
End Do

matrix0 = matrix1

DEALLOCATE(matrix1, STAT = DeAllocateStatus)

END SUBROUTINE


End MODULE

