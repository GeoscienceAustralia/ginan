MODULE m_matrixmerge


! ----------------------------------------------------------------------
! MODULE: m_matrixmerge.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the matrixmerge subroutine 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Geoscience Australia, Frontier-SI
! Created:  12 September 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE                   
  
        
Contains
        
        
SUBROUTINE matrixmerge (matrix0, matrix1) 


! ----------------------------------------------------------------------
! SUBROUTINE: matrixmerge
! ----------------------------------------------------------------------
! Purpose:
!  Merge input matrices 
! ----------------------------------------------------------------------
! Input arguments:
! - matrix0:      Input Matrix 0  
! - matrix1:      Input Matrix 1  
!
! Output arguments:
! - matrix2:      Merged matrix 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Geoscience Australia, Frontier-SI
! Created:  12 September 2019
! ----------------------------------------------------------------------
        
        
      USE mdl_precision
!      USE mdl_num
!      USE mdl_param
      IMPLICIT NONE

        
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: matrix0
! ----------------------------------------------------------------------
! OUT
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: matrix2
! ----------------------------------------------------------------------
! INOUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(INOUT) :: matrix1

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, j
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int8) :: Nepochs, Nepochs_0, Nepochs_1, N2_0, N2_1
      INTEGER (KIND = prec_int8) :: Nepoch_common, Nepochs_merge
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: matrix2
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Matrices common epoch:
! ----------------------------------------------------------------------
! 0. Non-common epoch
! 1. Common epoch: Last epoch of 1st matrix and First epoch of 2nd matrix
Nepoch_common = 1
! ----------------------------------------------------------------------

! Input matrix
sz1 = size(matrix0, DIM = 1)
sz2 = size(matrix0, DIM = 2)
Nepochs_0 = sz1
N2_0 = sz2
!PRINT *,"sz1,sz2",sz1,sz2

! Input matrix
sz1 = size(matrix1, DIM = 1)
sz2 = size(matrix1, DIM = 2)
Nepochs_1 = sz1
N2_1 = sz2

IF (N2_0 /= N2_1) THEN 
      PRINT *, "m_matrixmerge.f03 :: Matrices columns dimensions do not match. Merging matrices failed"
      STOP 
END IF

! Number of epochs (rows) of merged matrix
Nepochs_merge = Nepochs_0 - Nepoch_common + Nepochs_1

ALLOCATE (matrix2(Nepochs_merge, N2_0), STAT = AllocateStatus)
IF (AllocateStatus /= 0) PRINT *, "ALLOCATE Error: Module m_matrixmerge.f03, Array matrix2"
matrix2 = 0.0D0

j = 0 
Do i = 1 , Nepochs_0 - Nepoch_common
   j = j + 1
   matrix2(j, 1:N2_0) = matrix0(i,1:N2_0)  
End Do

Do i = 1 , Nepochs_1
   j = j + 1
   matrix2(j, 1:N2_1) = matrix1(i,1:N2_1)  
End Do

DEALLOCATE (matrix1, STAT = DeAllocateStatus)
ALLOCATE (matrix1(Nepochs_merge, N2_0), STAT = AllocateStatus)
IF (AllocateStatus /= 0) PRINT *, "ALLOCATE Error: Module m_matrixmerge.f03, Array matrix1"
matrix1 = matrix2

END SUBROUTINE

End MODULE

