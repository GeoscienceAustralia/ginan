SUBROUTINE kepler_k2z (kepler, GM , r,v)


! ----------------------------------------------------------------------
! SUBROUTINE: kepler_k2z.f90
! ----------------------------------------------------------------------
! Purpose:
!  Conversion of Keplerian elements (a,e,i,Omega,omega,E) to Position and 
!  Velocity vectors (Cartesian coordinates) 
! ----------------------------------------------------------------------
! Input arguments:
! - kepler:		Kepler elements | Array with rank 6
!   a:       	semi-major axis  (m)
!   e:      	eccentricity
!   i:      	inclination (degrees)
!   Omega:  	right ascension of the ascending node (degrees)
!   omega:  	argument of perigee (degrees)
!   E:      	eccentric anomaly (degrees)
! - GM: 		Earth gravity constant,  m^3/sec^2
!
! Output arguments:
! - r:  		position (m)
! - v:  		velocity (m/sec)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            26 April 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: kepler(6), GM
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: r(3), v(3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: a, e, i_rad, Omega_asc_rad, omega_per_rad, E_rad
      REAL (KIND = prec_q) :: n
      REAL (KIND = prec_q) :: P(3), Q(3), W(3), Rmatrix(3,3), r_orbplane(3), v_orbplane(3) 
! ----------------------------------------------------------------------
	  
	  
! ----------------------------------------------------------------------
! Keplerian elements
! ----------------------------------------------------------------------
! semi-major axis
      a = kepler(1)
	  
! Eccentricity
      e = kepler(2)
	  
! Inclination
      !i_deg = kepler(3)
      i_rad = kepler(3) * (PI_global / 180D0)

! Omega : right ascension of the ascending node (degrees)
      !Omega_asc_deg = kepler(4)
      Omega_asc_rad = kepler(4) * (PI_global / 180D0)

! omega : argument of perigee (degrees)
      !omega_per_deg = kepler(5)
      omega_per_rad = kepler(5) * (PI_global / 180D0)

! E : eccentric anomaly (degrees)
      !E_deg = kepler(6)
      E_rad = kepler(6) * (PI_global / 180D0)
! ----------------------------------------------------------------------
 
! ----------------------------------------------------------------------
! Mean motion
      n = sqrt( GM / a**3 )
! ----------------------------------------------------------------------
 

! ----------------------------------------------------------------------
! Rotation matrix : Rmatrix = [P Q W]

!P = [cos(omega)*cos(Omega)-sin(omega)*cos(i)*sin(Omega)
!     cos(omega)*sin(Omega)+sin(omega)*cos(i)*cos(Omega)
!     sin(omega)*sin(i)];
      P(1) = cos(omega_per_rad) * cos(Omega_asc_rad) - sin(omega_per_rad) * cos(i_rad) * sin(Omega_asc_rad)
      P(2) = cos(omega_per_rad) * sin(Omega_asc_rad) + sin(omega_per_rad) * cos(i_rad) * cos(Omega_asc_rad)
      P(3) = sin(omega_per_rad) * sin(i_rad)
	 
!Q = [-sin(omega)*cos(Omega)-cos(omega)*cos(i)*sin(Omega)
!     -sin(omega)*sin(Omega)+cos(omega)*cos(i)*cos(Omega)
!      cos(omega)*sin(i)];
      Q(1) = -1.0D0 * sin(omega_per_rad) * cos(Omega_asc_rad) - cos(omega_per_rad) * cos(i_rad) * sin(Omega_asc_rad)
      Q(2) = -1.0D0 * sin(omega_per_rad) * sin(Omega_asc_rad) + cos(omega_per_rad) * cos(i_rad) * cos(Omega_asc_rad)
      Q(3) = cos(omega_per_rad) * sin(i_rad)
	  
!W = [sin(i)*sin(Omega) 
!    -sin(i)*cos(Omega)
!    cos(i)];
      W(1) = sin(i_rad) * sin(Omega_asc_rad)
      W(2) = -1.0D0 * sin(i_rad) * cos(Omega_asc_rad)
      W(3) = cos(i_rad)
	 
! Rmatrix = [P Q W]
      Rmatrix(1,1:3) = (/ P(1), Q(1), W(1) /)
      Rmatrix(2,1:3) = (/ P(2), Q(2), W(2) /)
      Rmatrix(3,1:3) = (/ P(3), Q(3), W(3) /)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Coordinates in the orbital plane
      r_orbplane(1) = a * (cos(E_rad) - e)
	  r_orbplane(2) = a * sqrt(1.0D0 - e**2) * sin(E_rad)
      r_orbplane(3) = 0.0D0
	  
! Derivatives of the coordinates in the orbital plane
      !dX = ( (n*a) / ( 1-e*cos(E) ) ) * [-sin(E); sqrt(1-e^2)*cos(E); 0;] ;
      v_orbplane(1) = ( (n * a) / (1.D0 - e * cos(E_rad)) ) * (-1.0D0 * sin(E_rad) )   
	  v_orbplane(2) = ( (n * a) / (1.D0 - e * cos(E_rad)) ) * sqrt(1.0D0 - e**2) * cos(E_rad)
      v_orbplane(3) = 0.0D0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Position vector  
      !r = R * r_orbplane
      CALL matrix_Rr (Rmatrix, r_orbplane , r)	  
	  
! Velocity vector
      !v = R * v_orbplane
      CALL matrix_Rr (Rmatrix, v_orbplane , v)	  
! ----------------------------------------------------------------------



end
