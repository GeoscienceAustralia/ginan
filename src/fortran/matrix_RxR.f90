SUBROUTINE matrix_RxR


! ----------------------------------------------------------------------
! SUBROUTINE: matrix_RxR.f90
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
      USE mdl_arr
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
!      R1(i,j), R2(n,m) : Allocatable arrays via module mdl_arr.f90
! OUT
!      R3(i,m) : : Allocatable array via module mdl_arr.f90
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
	



END

