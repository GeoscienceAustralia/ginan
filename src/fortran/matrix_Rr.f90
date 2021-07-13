SUBROUTINE matrix_Rr (r1,r2 , r3)


! ----------------------------------------------------------------------
! SUBROUTINE: matrix_Rr.f90
! ----------------------------------------------------------------------
! Purpose:
!  Multiply 2 arrays : 3x3 multiply 3x1
! ----------------------------------------------------------------------
! Input arguments:
! - r1 : Array 1 with dimensions 3x3
! - r2 : Array 2 with dimensions 3x1
!
! Output arguments:
! - r3 : Array obtained from multiplying r1 * r2
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            26 April 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: r1(3,3), r2(3)
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: r3(3)
! ---------------------------------------------------------------------------

  
      r3(1) = r1(1,1) * r2(1) + r1(1,2) * r2(2) + r1(1,3) * r2(3)
      r3(2) = r1(2,1) * r2(1) + r1(2,2) * r2(2) + r1(2,3) * r2(3)
      r3(3) = r1(3,1) * r2(1) + r1(3,2) * r2(2) + r1(3,3) * r2(3)


END

