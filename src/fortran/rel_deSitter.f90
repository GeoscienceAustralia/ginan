SUBROUTINE rel_deSitter (Zsat, Zearth, GMearth, GMsun, gama, clight , a_deSitter)


! ----------------------------------------------------------------------
! SUBROUTINE: rel_deSitter.f90
! ----------------------------------------------------------------------
! Purpose:
! Relativistic effects due to deSitter term
!  Relativistic corrections (deSitter term) to the acceleration of an Earth 
!  artificial satellite according to the formula adopted by the IERS 
!  Conventions 2010.
! ----------------------------------------------------------------------
! Input arguments:
! - Zsat     : 		Satellite's state vector with respect to the Earth
! - Zearth   :		Earth's state vector with respect to the Sun 
!   				z = [x y z Vx Vy Vz]'
! - GMearth :		Gravitational coefficient of Earth 
! - GMsun   :		Gravitational coefficient of Sun
! - clight	:		Speed of light
! - gama	:		PPN (parameterized post-Newtonian) parameters
!
! Output arguments:
! - a_deSitter :	Satellite acceleration vector of relativistic corrections
!               	a_relativ = [ax; ay; az]
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
      REAL (KIND = prec_q), INTENT(IN) :: GMearth, GMsun
      REAL (KIND = prec_q), INTENT(IN) :: gama
      REAL (KIND = prec_q), INTENT(IN) :: clight

! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3) :: a_deSitter
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: rsat,vsat , rearth,vearth , Jearth
      REAL (KIND = prec_q), DIMENSION(3) :: vec1, cp1 , cp2
!      REAL (KIND = prec_q) :: GMearth, GMsun
      REAL (KIND = prec_q) :: lsat, learth
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
! Relativistic effects: deSitter term
! ---------------------------------------------------------------------------
      vec1 = -1.0D0 * ( GMsun / (clight**2 * learth**3) ) * rearth
      CALL productcross(vearth,vec1 , cp1)
      CALL productcross(cp1,vsat , cp2)
! geodesic (de Sitter) precession terms
      a_deSitter = (1.0D0 + 2.0D0 * gama) * cp2
! ---------------------------------------------------------------------------


END
