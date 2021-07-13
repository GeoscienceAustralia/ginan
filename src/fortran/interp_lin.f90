SUBROUTINE interp_lin (x, y, x_int, y_int)


! ----------------------------------------------------------------------
! Subroutine:  interp_lin.f90
! ----------------------------------------------------------------------
! Purpose:
!  Linear interpolation based on two data points
! ----------------------------------------------------------------------
! Input arguments:
! - x:			Array with two values of the x variable
! - y:			Array with two values of the y variable given as a 
!               function of x. y = f(x)
! - x_int:		Value of x variable that the interpolation of y is required 
!
! Output arguments:
! - y_int:		Interpolated value of y variable at the value x_int
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia               March 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: x(2), y(2), x_int
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: y_int
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      y_int = y(1) + ( x_int - x(1) ) * ( (y(2) - y(1)) / (x(2) - x(1)) )
! ----------------------------------------------------------------------


END
