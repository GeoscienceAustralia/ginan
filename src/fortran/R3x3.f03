SUBROUTINE R3x3 (R1, R2, R3)


! ----------------------------------------------------------------------
! SUBROUTINE: matrixRxR (matrix_RxR.f90)
! ----------------------------------------------------------------------
! Purpose:
!  Multiply 2 matrices 3x3 
! ----------------------------------------------------------------------
! Input arguments:
! - R1 : Matrix 1 with dimensions 3x3
! - R1 : Matrix 2 with dimensions 3x3
!
! Output arguments:
! - R3 : Matrix 3x3 obtained from multiplication R1*R2
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	7 March 2018
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3,3) :: R1, R2
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(3,3) :: R3
! ---------------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: R1_i, R1_j, R2_i, R2_j
      INTEGER (KIND = prec_int8) :: i, j, n, m
      REAL (KIND = prec_q) :: R3_ij
! ----------------------------------------------------------------------


      R1_i = size(R1, dim = 1)
      R1_j = size(R1, dim = 2)
	  
      R2_i = size(R2, dim = 1)
      R2_j = size(R2, dim = 2)
	  
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
