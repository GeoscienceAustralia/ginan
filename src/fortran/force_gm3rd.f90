SUBROUTINE force_gm3rd (rbody,rsat,GMbody , a_perturb)


! ----------------------------------------------------------------------
! SUBROUTINE: force_gm3rd.f90
! ----------------------------------------------------------------------
! Purpose:
!  Orbital perturbation due to a solar system body 
!  Satellite's perturbing acceleration due to celestial bodies (i.e. Sun,
!  Moon, Planets) is computed according to Newton's law of gravity
!  considering the third body as a point mass.
! ----------------------------------------------------------------------
! Input arguments:
! - rbody  :		Celestial body's position vector in GCRF (m)
! - rsat   :		Satellite's position vector in GCRF (m)
! - GMbody :		Celestial body's Gravity constant GM (m^3/sec^2)
!
! Output arguments:
! - ax, ay, az :	Acceleration's cartesian components in GCRF (m/s/s)
!
! GCRF: Geocentric Celestial Reference Frame 
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
	  
! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: rbody, rsat
      REAL (KIND = prec_q), INTENT(IN) :: GMbody
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3) :: a_perturb
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: a_earth, a_sat, rsat_body
      REAL (KIND = prec_q) :: l_body,ax_earth,ay_earth,az_earth, l_sat_body,ax_sat,ay_sat,az_sat
      REAL (KIND = prec_q) :: ax,ay,az
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Acceleration of the Earth caused by the Celestial Body (point mass)
! a_earth = -GMbody * { (-rbody) / |-rbody|^3 } = GMbody*{rbody/|rbody|^3}
l_body = sqrt(rbody(1)**2 + rbody(2)**2 + rbody(3)**2)
ax_earth = GMbody * ( rbody(1) / (l_body**3) )
ay_earth = GMbody * ( rbody(2) / (l_body**3) )
az_earth = GMbody * ( rbody(3) / (l_body**3) )
a_earth(1) = ax_earth
a_earth(2) = ay_earth
a_earth(3) = az_earth
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! rsat_body : Vector pointing from the Celestial Body to the Satellite 
rsat_body = rsat - rbody
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Acceleration of the Satellite caused by the Celestial Body (point mass)
! a_sat = -GMbody * { (rsat-rbody) / |rsat-rbody|^3 } 
l_sat_body = sqrt(rsat_body(1)**2 + rsat_body(2)**2 + rsat_body(3)**2)
! Cartesian components
ax_sat = -GMbody * ( rsat_body(1) / (l_sat_body**3) )
ay_sat = -GMbody * ( rsat_body(2) / (l_sat_body**3) )
az_sat = -GMbody * ( rsat_body(3) / (l_sat_body**3) )
a_sat(1) = ax_sat
a_sat(2) = ay_sat
a_sat(3) = az_sat
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Perturbing acceleration of the Satellite caused by the Celestial Body :
! Relative acceleration of the Satellite in Earth-centered reference system 
a_perturb = a_sat - a_earth
ax = a_perturb(1)
ay = a_perturb(2)
az = a_perturb(3)
! ---------------------------------------------------------------------------


END
 