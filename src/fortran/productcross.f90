SUBROUTINE productcross(r1,r2 , cp)


! ----------------------------------------------------------------------
! SUBROUTINE: productcross.f90
! ----------------------------------------------------------------------
! Purpose:
!  Cross product between two vectors
! ----------------------------------------------------------------------
! Input arguments:
! - r1 : first vector   [x1 y1 z1]'  (meters)
! - r2 : second vector  [x2 y2 z2]'  (meters)
!
! Output arguments:
! - cp : Cross product of r1,r2 vectors  [x3 y3 z3]'  (meters)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia         24 November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: r1, r2
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3) :: cp
! ---------------------------------------------------------------------------


      cp(1) = r1(2) * r2(3) - r1(3) * r2(2)

      cp(2) = r1(3) * r2(1) - r1(1) * r2(3)

      cp(3) = r1(1) * r2(2) - r1(2) * r2(1)


END

