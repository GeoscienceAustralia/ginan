SUBROUTINE pd_gm3rd (rbody, rsat, GMbody , U)


! ----------------------------------------------------------------------
! SUBROUTINE: pd_gm3rd
! ----------------------------------------------------------------------
! Purpose:
! Partial derivatives of the acceleration due to solar system body Gravity
! field (based on Newton's law of gravity considering the third body as a point mass)
! ----------------------------------------------------------------------
! Input arguments:
! - rbody  :		Celestial body's position vector in GCRF (m)
! - rsat   :		Satellite's position vector in GCRF (m)
! - GMbody :		Celestial body's Gravity constant GM (m^3/sec^2)
! 
! Output arguments:
! - U: 			Matrix of the partial derivatives of the acceleration
!   			given in the form of second partial derivatives of 
!				geopotential Uxx, Uyy, Uzz, Uxy, Uxz, Uyz 
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
! Author :	Dr. Yujun Du, Mark Yeo, Industrial Sciences Group, Australia
! Created:	03 June 2022
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: rbody, rsat
      REAL (KIND = prec_q), INTENT(IN) :: GMbody
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: U(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: rsat_body
      REAL (KIND = prec_q) :: xx, yy, zz, l_sat_body
      REAL (KIND = prec_q) :: Uxx, Uyy, Uzz, Uxy, Uxz, Uyz 
      REAL (KIND = prec_q) :: Uo(3,3) 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Partial derivatives of the acceleration due to the Celestial Body (point mass)
! U = -GMbody * { I / |rsat-rbody|^3 - 3 (rsat-rbody) transpose(rsat-rbody) / |rsat-rbody|^5 }
! ----------------------------------------------------------------------

! Vector pointing from the Celestial Body to the Satellite
rsat_body = rsat - rbody

! Cartesian components
xx = rsat_body(1)
yy = rsat_body(2)
zz = rsat_body(3)

! Distance from the Celestial Body to the Satellite
l_sat_body = sqrt(xx**2 + yy**2 + zz**2)


! Computation of second partial derivatives Uxx, Uyy, Uzz, Uxy, Uxz, Uyz
Uxx = 0.D0
Uyy = 0.D0
Uzz = 0.D0
Uxy = 0.D0
Uxz = 0.D0
Uyz = 0.D0

Uxx = 3 * xx**2 - l_sat_body**2
Uyy = 3 * yy**2 - l_sat_body**2
Uzz = 3 * zz**2 - l_sat_body**2
Uxy = 3 * xx * yy
Uxz = 3 * xx * zz
Uyz = 3 * yy * zz


! Matrix of second partial derivatives of geopotential V:
Uo(1,1:3) = (/ Uxx,   Uxy,   Uxz /)
Uo(2,1:3) = (/ Uxy,   Uyy,   Uyz /)
Uo(3,1:3) = (/ Uxz,   Uyz,   Uzz /)

U = ( GMbody / l_sat_body**5 ) *  Uo



END
