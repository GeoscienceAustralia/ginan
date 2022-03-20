MODULE m_matrixRxR


! ----------------------------------------------------------------------
! MODULE: m_matrixRxR
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the following subroutines 
! 
! Subroutines contained within the module:
! - matrixRxR
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Cooperative Research Centre for Spatial Information, Australia
! Created:  5 March 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE                   
  
        
Contains


SUBROUTINE matrixRxR (R1, R2, R3)


! ----------------------------------------------------------------------
! SUBROUTINE: matrixRxR (matrix_RxR.f90)
! ----------------------------------------------------------------------
! Purpose:
!  Multiply 2 arrays 
! ----------------------------------------------------------------------
! Input arguments:
! - R1 : Array 1 with dimensions ixj
! - R1 : Array 2 with dimensions nxm
!
! Output arguments:
! - R3 : Array ixm obtained from multiplication R1*R2
! ----------------------------------------------------------------------
! Note:
! Input and Output arguments are allocatable arrays and are declared
! through module mdl_arr.f90 
! ----------------------------------------------------------------------
! Thomas D. Papanikolaou, Geoscience Australia              29 June 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      !USE mdl_arr
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: R1, R2
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: R3
! ---------------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: R1_i, R1_j, R2_i, R2_j
      INTEGER (KIND = prec_int8) :: i, j, n, m
      REAL (KIND = prec_q) :: R3_ij
      INTEGER (KIND = prec_int2) :: AllocateStatus
!       , DeAllocateStatus      
! ----------------------------------------------------------------------


      R1_i = size(R1, dim = 1)
      R1_j = size(R1, dim = 2)
        
      R2_i = size(R2, dim = 1)
      R2_j = size(R2, dim = 2)

! Allocatable arrays
ALLOCATE (R3(R1_i,R2_j), STAT = AllocateStatus) 
        
      DO i = 1 , R1_i     
        DO m = 1 , R2_j
        ! R3(i,m)        
          R3_ij = 0.0D0
          DO j = 1 , R1_j
            n = j
                  R3_ij = R1(i,j) * R2(n,m) + R3_ij
          End Do
          R3(i,m) = R3_ij    
        End Do
      End Do
      



END SUBROUTINE



END Module

