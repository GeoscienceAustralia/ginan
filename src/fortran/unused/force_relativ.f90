SUBROUTINE force_rel (Zsat,Zearth,relativ , a_rel)


! ----------------------------------------------------------------------
! SUBROUTINE: force_rel.f90
! ----------------------------------------------------------------------
! Purpose:
!  Relativistic corrections to the acceleration of an Earth artificial 
!  satellite according to the formula adopted in IERS Conventions 2010.
! ----------------------------------------------------------------------
! Input arguments:
! - Zsat  :			Satellite's state vector with respect to the Earth
! - Zearth:			Earth's state vector with respect to the Sun 
!					z = [x y z Vx Vy Vz]
! - relativ:		Relativistic effects to be or not to be computed
!					Schwarzschild, Lense-Thirring, geodesic (de Sitter) precession
!   rlv 3x1 matrix  e.g. rlv = [1 1 1]
!   value 1 for computation and value 0 for neglection   

! - GMearth  : Gravitational coefficient of Earth 
! - GMsun    : Gravitational coefficient of Sun
! - cslight  : Speed of light
! - ppn_beta : PPN (parameterized post-Newtonian) parameters
!   ppn_gama   PPN equal to 1 in General Relativity

!
! Output arguments:
! - a_relativ : Cartesian components of relativistic corrections to
!               satellite acceleration     a_relativ = [ax; ay; az]
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(6) :: Zsat,Zearth
      INTEGER (KIND = prec_int2), INTENT(IN), DIMENSION(3) :: relativ
! mdl_num.f90: GMearth,GMsun , cslight,ppn_beta,ppn_gama
!
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3) :: a_rel
!      REAL (KIND = prec_q), INTENT(OUT) :: ax,ay,az
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q) :: xMoon,yMoon,zMoon,l_Moon , xSun,ySun,zSun,l_Sun
      REAL (KIND = prec_q) :: termMoon,termMoon_x,termMoon_y,termMoon_z , termSun,termSun_x,termSun_y,termSun_z
      REAL (KIND = prec_q) :: indirectJ2_coef
      REAL (KIND = prec_q) :: ax,ay,az
!      REAL (KIND = prec_q), DIMENSION(3) :: a_iJ2
! ---------------------------------------------------------------------------


! ----------------------------------------------------------------------
! Satellite vectors
rsat = (/Zsat(1), Zsat(2), Zsat(3) /) 
vsat = (/ Zsat(4), Zsat(5), Zsat(6) /)

! Earth-Satellite distance
lsat = sqrt( rsat(1)**2 + rsat(2)**2 + rsat(3)**2 )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth vectors
rearth = (/ Zearth(1), Zearth(2), Zearth(3) /) 
vearth = (/ Zearth(4), Zearth(5), Zearth(6) /) 

! Earth's angular momentum per unit mass
      CALL productcross (rearth,vearth , Jearth) 

! Sun-Earth distance
learth = sqrt(rearth(1)**2 + rearth(2)**2 + rearth(3)**2)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Relativistic effects terms
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
!if relativ(1,1) == 1
!% Schwarzschild terms
!a_Schwarzschild = (GMearth / (cslight^2 * lsat^3)) * ...
!                  ( ...
!                  ( 2*(ppn_beta+ppn_gama) * (GMearth/lsat) - ppn_gama * productdot(vsat,vsat) ) * rsat ...
!                  + 2 * (1 + ppn_gama) * productdot(rsat,vsat) * vsat ...
!                  );
!elseif relativ(1,1) == 0
!    a_Schwarzschild = [0; 0; 0];
!end
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Schwarzschild terms
productdot(vsat,vsat)
productdot(rsat,vsat)

a_Schwarzschild = (GMearth / (cslight**2 * lsat**3)) *															&
                  ( 																							&
                  ( 2.0D0 * (ppn_beta+ppn_gama) * (GMearth/lsat) - ppn_gama * productdot(vsat,vsat) ) * rsat 	&
                  + 2.0D0 * (1.0D0 + ppn_gama) * productdot(rsat,vsat) * vsat 									&
                  )
				  

! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!if relativ(1,2) == 1
!% Lense-Thirring (precession,frame-dragging) terms
!a_LenseThirring = (1 + ppn_gama) * (GMearth / (cslight^2 * lsat^3)) * ...
!                  ( (3/lsat^2) * productcross(rsat,vsat) * productdot(rsat,Jearth) + productcross(vsat,Jearth) );
!elseif relativ(1,2) == 0
!    a_LenseThirring = [0; 0; 0];
!end
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Lense-Thirring (precession,frame-dragging) terms
a_LenseThirring = (1.0D0 + ppn_gama) * (GMearth / (cslight**2 * lsat**3)) * 	&
                  ( (3.0D0/lsat**2) * productcross(rsat,vsat) * productdot(rsat,Jearth) + productcross(vsat,Jearth) )



! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!if relativ(1,3) == 1
!% geodesic (de Sitter) precession terms
!vec1 = - ( GMsun / (cslight^2 * learth^3) ) * rearth;
!cp1 = productcross(vearth,vec1);
!cp2 = productcross(cp1,vsat);
!a_deSitter = (1 + 2 * ppn_gama) * cp2;
!clear vec1 cp1 cp2
!elseif relativ(1,3) == 0
!    a_deSitter = [0; 0; 0];
!end
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! geodesic (de Sitter) precession terms
vec1 = -1.0D0 * ( GMsun / (cslight**2 * learth**3) ) * rearth
cp1 = productcross(vearth,vec1)
cp2 = productcross(cp1,vsat)
a_deSitter = (1.0D0 + 2.0D0 * ppn_gama) * cp2


% % Vectors magnitude
% a_Schwarzschild_magn = sqrt(a_Schwarzschild(1,1)^2 + a_Schwarzschild(2,1)^2 + a_Schwarzschild(3,1)^2)
% a_LenseThirring_magn = sqrt(a_LenseThirring(1,1)^2 + a_LenseThirring(2,1)^2 + a_LenseThirring(3,1)^2)
% a_deSitter_magn      = sqrt(a_deSitter(1,1)^2 + a_deSitter(2,1)^2 + a_deSitter(3,1)^2)

% Overall vector
a_relativ = a_Schwarzschild + a_LenseThirring + a_deSitter;
END 
