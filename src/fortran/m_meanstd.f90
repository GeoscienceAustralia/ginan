MODULE m_meanstd

      IMPLICIT NONE
      !SAVE
Contains


SUBROUTINE meanstd(A, A_SumSQR, NUM, AVE_A, STD_A)

! ----------------------------------------------------------------------
! MODULE: m_meanstd.f90
! ----------------------------------------------------------------------
! Purpose:
! Compute the mean and std
!
! Input:
!       A  : Sum of a specific parameters
!  A_SumSQR: Sum of squares of  a specific parameters
!      NUM : number of the parameter measured
!
! Output:
!    AVE_A : the mean value
!    STD_A : the STD value
!
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng, Geoscience  Australia
!
! Created:      21-08-2019
! ----------------------------------------------------------------------

USE mdl_precision
IMPLICIT  NONE

INTEGER(KIND=4)      :: NUM
REAL(KIND = prec_q)  :: A, A_SumSQR
REAL(KIND = prec_q)  :: AVE_A, Var, STD_A

AVE_A = A / NUM
Var   = (A_SumSQR - A*A/NUM)/(NUM-1)
STD_A = SQRT(Var)

END SUBROUTINE
END
