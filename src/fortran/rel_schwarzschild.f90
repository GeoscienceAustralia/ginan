SUBROUTINE rel_schwarzschild (Zsat, Zearth, GMearth, beta, gama, clight, a_Schwarzschild)


! ----------------------------------------------------------------------
! SUBROUTINE: rel_schwarzschild.f90
! ----------------------------------------------------------------------
! Purpose:
! Relativistic effects : Schwarzschild field
!  Relativistic corrections (Schwarzschild term) to the acceleration of an  
!  Earth artificial satellite according to the formula adopted by the IERS 
!  Conventions 2010.
! ----------------------------------------------------------------------
! Input arguments:
! - Zsat     :		Satellite's state vector with respect to the Earth
! - Zearth   : 		Earth's state vector with respect to the Sun 
!   				z = [x y z Vx Vy Vz]'
! - GMearth	: 		Gravitational coefficient of Earth 
! - beta	: 		PPN (parameterized post-Newtonian) parameters
!   gama
! - clight	: 		Speed of light
!
! Output arguments:
! - a_Schwarzschild:	Satellite acceleration vector of relativistic corrections
!               		a_relativ = [ax; ay; az]
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	24 November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      !USE mdl_num
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(6) :: Zsat, Zearth
      REAL (KIND = prec_q), INTENT(IN) :: GMearth
      REAL (KIND = prec_q), INTENT(IN) :: beta, gama
      REAL (KIND = prec_q), INTENT(IN) :: clight

! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3) :: a_Schwarzschild
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: rsat,vsat , rearth,vearth , Jearth
      REAL (KIND = prec_q) :: lsat, learth
      REAL (KIND = prec_q) :: prod_rv , prod_vv  
! ---------------------------------------------------------------------------



! ---------------------------------------------------------------------------
! Satellite vectors
rsat = (/Zsat(1), Zsat(2), Zsat(3) /) 
vsat = (/ Zsat(4), Zsat(5), Zsat(6) /)

! Earth-Satellite distance
lsat = sqrt( rsat(1)**2 + rsat(2)**2 + rsat(3)**2 )
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Earth vectors
rearth = (/ Zearth(1), Zearth(2), Zearth(3) /) 
vearth = (/ Zearth(4), Zearth(5), Zearth(6) /) 

! Earth's angular momentum per unit mass
      CALL productcross (rearth,vearth , Jearth) 

! Sun-Earth distance
learth = sqrt(rearth(1)**2 + rearth(2)**2 + rearth(3)**2)
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Relativistic effects: Schwarzschild term
! ---------------------------------------------------------------------------
      CALL productdot(vsat,vsat , prod_vv)
      CALL productdot(rsat,vsat , prod_rv)

! Schwarzschild terms
a_Schwarzschild = (GMearth / (clight**2 * lsat**3)) *									&
                  ( 																	&
                  ( 2.0D0 * (beta+gama) * (GMearth/lsat) - gama * prod_vv ) * rsat 		&
                  + 2.0D0 * (1.0D0 + gama) * prod_rv * vsat 							&
                  )
! ---------------------------------------------------------------------------


END
