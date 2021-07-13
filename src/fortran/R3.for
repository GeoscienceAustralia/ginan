! ----------------------------------------------------------------------
! SUBROUTINE: R3
! ----------------------------------------------------------------------
! Purpose:
!    Calculate rotation matrix about the z-axis (towards earth centre)
!    for given radians PSI
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------
*
*         (  + cos(PSI)   + sin(PSI)     0  )
*         (                                 )
*         (  - sin(PSI)   + cos(PSI)     0  )
*         (                                 )
*         (       0            0         1  )
*

      SUBROUTINE R3 ( PSI, R )
      IMPLICIT NONE

      DOUBLE PRECISION PSI, R(3,3)

      DOUBLE PRECISION S, C, A11, A12, A13, A21, A22, A23

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      S = SIN(PSI)
      C = COS(PSI)


      A11 =   C
      A12 =   S
      A13 =   0.0d0
      A21 = - S
      A22 =   C
      A23 =   0.0d0


      R(1,1) = A11
      R(1,2) = A12
      R(1,3) = A13
      R(2,1) = A21
      R(2,2) = A22
      R(2,3) = A23

      END
