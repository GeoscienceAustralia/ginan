! ----------------------------------------------------------------------
! SUBROUTINE: R1
! ----------------------------------------------------------------------
! Purpose:
!    calculate rotation matrix about the x-axis (along the track) for
!    given radians PHI
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------
*
*         (  1        0            0      )
*         (                               )
*         (  0   + cos(PHI)   + sin(PHI)  )
*         (                               )
*         (  0   - sin(PHI)   + cos(PHI)  )
*
*
*-----------------------------------------------------------------------

      SUBROUTINE R1 ( PHI, R )
      IMPLICIT NONE

      DOUBLE PRECISION PHI, R(3,3)

      DOUBLE PRECISION S, C, A21, A22, A23, A31, A32, A33

* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      S = SIN(PHI)
      C = COS(PHI)

      A21 =   0.0d0
      A22 =   C
      A23 =   S
      A31 =   0.0d0
      A32 = - S
      A33 =   C

      R(2,1) = A21
      R(2,2) = A22
      R(2,3) = A23
      R(3,1) = A31
      R(3,2) = A32
      R(3,3) = A33


      END
