SUBROUTINE productdot(r1,r2 , dp)


! ----------------------------------------------------------------------
! SUBROUTINE: productdot.f90
! ----------------------------------------------------------------------
! Purpose:
!  Dot product between two vectors (scalar product)
! ----------------------------------------------------------------------
! Input arguments:
! - r1 : first vector   [x1 y1 z1]'
! - r2 : second vector  [x2 y2 z2]'
!
! Output arguments:
! - dp : Dot product of r1,r2 vectors (scalar)
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
      REAL (KIND = prec_q), INTENT(OUT) :: dp
! ---------------------------------------------------------------------------



      dp = r1(1) * r2(1) + r1(2) * r2(2) + r1(3) * r2(3)
	  

END

