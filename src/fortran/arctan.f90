SUBROUTINE arctan ( y,x , angle )


! ----------------------------------------------------------------------
! SUBROUTINE: arctan.f90
! ----------------------------------------------------------------------
! Purpose:
!  Angle computation based on the x,y Cartesian coordinates.
!  Test for identifying the angle quadrant.
! ----------------------------------------------------------------------
! Remarks:
!  Angles are considered counter-clockwise
!  The result is computed in radians. 
! ----------------------------------------------------------------------
! Input arguments:
! - x,y:			2D Cartesian coordinates
! Output arguments:
! - angle:			Orientation angle (counter-clockwise) starting from x axis (radians)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia              August 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), INTENT(IN) :: x,y
      REAL (KIND = prec_q), INTENT(OUT) :: angle
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: pi
      REAL (KIND = prec_q) :: a
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Numerical Constants
      pi = PI_global
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      if (x .ne. 0.d0) then
          a = atan( abs( y/x ) )
          if (x > 0.0D0) THEN
              if (y > 0.0D0) THEN
                  angle = a
              else if (y < 0.0D0) THEN
                  angle = 2.0D0 * pi - a
              else
                  angle = 0.0D0
              end IF
          else if (x < 0.0D0) THEN
              if (y > 0.0D0) THEN
                  angle = pi - a
              else if (y < 0.0D0) THEN
                  angle = pi + a
              else
                  angle = pi
              end IF
          end if
      else
          if (y > 0.0D0) THEN
              angle = pi / 2.0D0
          else if (y < 0.0D0) THEN
              angle = 3.0D0 * pi / 2.0D0
          else
              angle = 0.d0
          end IF
      end IF
! ----------------------------------------------------------------------

END
	  
