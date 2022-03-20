MODULE m_matrixinv


! ----------------------------------------------------------------------
! MODULE: m_matrixinv
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the following subroutines 
! 
! Subroutines contained within the module:
! - matrixinv
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	22 March 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE matrixinv (Amatrix, AmatrixInv, An)


! ----------------------------------------------------------------------
! SUBROUTINE: matrixinv
! ----------------------------------------------------------------------
! Purpose:
!  Matrix inversion based on LAPACK library 
! ----------------------------------------------------------------------
! Input arguments:
! - Amatrix : 		Matrix with dimensions ixj
!
! Output arguments:
! - AmatrixInv :	Inverse of input matrix Amatrix
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
! 			Geoscience Australia
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	22 March 2018
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int8), INTENT(IN) :: An
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: Amatrix
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: AmatrixInv
! ---------------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      !INTEGER (KIND = prec_int8) :: n, m, info
      INTEGER :: n, m, info
! ----------------------------------------------------------------------
      !REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: work_matrix
      !DOUBLE PRECISION   WORK( * )
	  !Integer, Dimension(:), ALLOCATABLE :: ipiv   
	  Integer (KIND = prec_int8) :: ipiv(An)   
      REAL (KIND = prec_d) :: WORK(An) 
      REAL (KIND = prec_q) :: Nmatrix(An,An)
      REAL (KIND = prec_q) :: NmatrixInv(An,An)
      INTEGER (KIND = prec_int2) :: AllocateStatus
!       , DeAllocateStatus	  


! External procedures in LAPACK library
external DGETRF
external DGETRI


n = size(Amatrix, DIM = 1)
m = size(Amatrix, DIM = 2)

ALLOCATE (AmatrixInv(n,n), STAT = AllocateStatus)
!ALLOCATE (work_matrix(n), STAT = AllocateStatus)
!ALLOCATE (ipiv(n), STAT = AllocateStatus)

Nmatrix = Amatrix
NmatrixInv = Nmatrix

! ----------------------------------------------------------------------
! LU factorization using partial pivoting with row interchanges
!SUBROUTINE DGETRF( M, N, A, LDA, IPIV, INFO )
!call DGETRF(n, n, AmatrixInv, n, ipiv, info)
call DGETRF(n, n, NmatrixInv, n, ipiv, info)
!print *,"info", info
if (info /= 0) then
   !stop 'Matrix is numerically singular!'
   print *,"Matrix is numerically singular"
end if
! ----------------------------------------------------------------------
! DGETRI computes the inverse of an LU-factored general matrix computed by DGETRF.
!SUBROUTINE DGETRI( N, A, LDA, IPIV, WORK, LWORK, INFO )
!call DGETRI(n, AmatrixInv, n, ipiv, WORK, n, info)
call DGETRI(n, NmatrixInv, n, ipiv, WORK, n, info)

if (info /= 0) then
   !stop 'Matrix inversion failed!'
   print *,"Matrix inversion failed"
end if
! ----------------------------------------------------------------------

AmatrixInv = NmatrixInv


END SUBROUTINE


END Module

