MODULE m_lagrange


! ----------------------------------------------------------------------
! MODULE: m_lagrange.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the subroutine 'interp_lag' for Lagrange interpolation
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	13 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
	  


Contains

SUBROUTINE interp_lag (Xint, X_interp, Y_interp, Yint)


! ----------------------------------------------------------------------
! SUBROUTINE: interp_lag
! ----------------------------------------------------------------------
! Purpose:
!  Interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - Xint:  			X value where the interpolated value of Y is required
! - X_interp:   	array of values of the independent variable X
! - Y_interp:   	array of the Y function values corresponding to X i.e. y=f(x)
!
! Output arguments:
! - Yint:			Y interpolated value at Xint
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	5 August 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 14 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 adavantages in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: Xint
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:), ALLOCATABLE :: Y_interp 
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: Yint
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: sz1, n, i, j
      REAL (KIND = prec_q) :: L_numerator, L_denominator, Li
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: L 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------


  
! Number of Data points
n = size(X_interp, DIM = 1)

!sz1 = size(Y_interp, DIM = 1)
!print *,'n,sz1', n, sz1


Allocate( L(n), STAT = AllocateStatus)
!print *, "AllocateStatus=", AllocateStatus
if (AllocateStatus == 0) then

! Computation of coefficients Li (xint)
DO i = 1 , n
   L_numerator   = 1.0D0 
   L_denominator = 1.0D0
   
   DO j = 1 , n
	  If (j == i) then
	     ! not
	  Else
		L_numerator   = ( xint - X_interp(j) ) * L_numerator 
        L_denominator = ( X_interp(i) - X_interp(j) ) * L_denominator
	  End If
	End Do

    L(i) = L_numerator / L_denominator
	!Li = L_numerator / L_denominator
    !L(i) = Li
End Do

!print *,"X_interp", X_interp
!print *,"L", L


! Value of function Y(X) at the point xint
! Value is approximated by interpolant Pn(xint) based on Lagrange Polynomial
yint = 0.D0

Do i = 1 , n
    yint = Y_interp(i) * L(i) + yint
End Do


Deallocate(L, STAT = DeAllocateStatus)
else
        ! only hit if the allocation fails
        yint = 1.d0
end if


!print *,"yint",yint


End subroutine



End Module


