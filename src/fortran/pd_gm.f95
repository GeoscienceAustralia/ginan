SUBROUTINE pd_gm (GM, r, U )


! ----------------------------------------------------------------------
! SUBROUTINE: pd_gm
! ----------------------------------------------------------------------
! Purpose:
! Partial derivatives of the acceleration due to the central Earth Gravity
! field (based on Newton's law of gravity considering Earth as a point mass)
! ----------------------------------------------------------------------
! Input arguments:
! - r:				position vector (m)
! 
! Output arguments:
! - U: 				Matrix of the partial derivatives of the acceleration
!   				given in the form of second partial derivatives of 
!					geopotential Uxx, Uyy, Uzz, Uxy, Uxz, Uyz 
! ----------------------------------------------------------------------
! Note:
!  Matrix of second partial derivatives:
!   U = [ Uxx   Uxy   Uxz 
!         Uxy   Uyy   Uyz
!         Uxz   Uyz   Uzz ]  
! 
!  Partial derivatives are noted as:
!   dfx / dy = Uxy,     dfx / dx = Uxx
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	11 December 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: r
      REAL (KIND = prec_q), INTENT(IN) :: GM	
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: U(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: phi, lamda, radius
      REAL (KIND = prec_q) :: x, y, z
      REAL (KIND = prec_q) :: Uxx, Uyy, Uzz, Uxy, Uxz, Uyz 
      REAL (KIND = prec_q) :: Uo(3,3) 
! ----------------------------------------------------------------------


! computation of spherical coordinates
      CALL coord_r2sph (r , phi,lamda,radius)
	  
! r coordinates x,y,z
x = r(1)
y = r(2)
z = r(3)

! Computation of second partial derivatives Uxx, Uyy, Uzz, Uxy, Uxz, Uyz
Uxx = 0.D0
Uyy = 0.D0
Uzz = 0.D0
Uxy = 0.D0
Uxz = 0.D0
Uyz = 0.D0

Uxx = 3 * x**2 - radius**2
Uyy = 3 * y**2 - radius**2
Uzz = 3 * z**2 - radius**2
Uxy = 3 * x * y
Uxz = 3 * x * z
Uyz = 3 * y * z


! Matrix of second partial derivatives of geopotential V:
Uo(1,1:3) = (/ Uxx,   Uxy,   Uxz /)  
Uo(2,1:3) = (/ Uxy,   Uyy,   Uyz /)          
Uo(3,1:3) = (/ Uxz,   Uyz,   Uzz /)    

U = ( GM / radius**5 ) *  Uo 	  



END

