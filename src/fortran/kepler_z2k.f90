SUBROUTINE kepler_z2k (r,v,GM , kepler)


! ----------------------------------------------------------------------
! SUBROUTINE: kepler_z2k.f90
! ----------------------------------------------------------------------
! Purpose:
!  Conversion of Position and Velocity vectors cartesian coordinates to 
!  Keplerian elements  [a,e,i,Omega,omega,f,M,E,u]
! ----------------------------------------------------------------------
! Input arguments:
! - r:  		position (m)
! - v:  		velocity (m/sec)
! - GM: 		Earth gravity constant,  m^3/sec^2
!
! Output arguments:
!  - kepler:	Kepler elements
!    a:       	semi-major axis  (m)
!    e:      	eccentricity
!    i:      	inclination (degrees)
!    Omega:  	right ascension of the ascending node (degrees)
!    omega:  	argument of perigee (degrees)
!    f:      	true anomaly (degrees)
!    E:      	eccentric anomaly (degrees)
!    M:      	Mean anomaly (degrees)
!    u:      	argument of latitude (degrees)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            21 April 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: r(3), v(3), GM
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: kepler(9)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: X,Y,Z, Vx,Vy,Vz
      REAL (KIND = prec_q) :: r_norm, v_norm
      REAL (KIND = prec_q) :: rr_dot, vv_dot, rv_dot, rv_norm
      REAL (KIND = prec_q) :: h(3), hh_dot, h_norm
      REAL (KIND = prec_q) :: W(3), Wx, Wy, Wz, p
      REAL (KIND = prec_q) :: i_y, i_x, i_rad
      REAL (KIND = prec_q) :: Omega_asc_rad, a, n, e, M_rad 
      REAL (KIND = prec_q) :: E_y, E_x, E_rad
      REAL (KIND = prec_q) :: u_y, u_x, u_rad
      REAL (KIND = prec_q) :: f_y, f_x, f_rad
      REAL (KIND = prec_q) :: i_deg, Omega_asc_deg, E_deg, M_deg, u_deg, f_deg, omega_per_deg  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Position vector r = [ X Y Z ]'
X = r(1)
Y = r(2)
Z = r(3)

! Velocity vector v = [ Vx Vy Vz ]'
Vx = v(1)
Vy = v(2)
Vz = v(3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! r vector magnitude (norm)
      CALL productdot(r, r, rr_dot)
      r_norm = sqrt(rr_dot)
! v vector magnitude (norm)
      CALL productdot(v, v, vv_dot)
      v_norm = sqrt(vv_dot)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! h: angular momentum (areal velocity vector)
!h = [ Y*Vz-Z*Vy
!      Z*Vx-X*Vz
!      X*Vy-Y*Vx ];
      h(1) = Y * Vz - Z * Vy
      h(2) = Z * Vx - X * Vz
      h(3) = X * Vy - Y * Vx
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! W (unit vector of h)
!W = h/sqrt(h'*h);
!Wx = W([1]);
!Wy = W([2]);
!Wz = W([3]);

! h length (norm)
      !h_norm = sqrt(h(1)**2 + h(2)**2 + h(3)**2)
      CALL productdot(h, h, hh_dot)
      h_norm = sqrt(hh_dot)

      W(1) = h(1) * (1.0D0 / h_norm)
      W(2) = h(2) * (1.0D0 / h_norm)
      W(3) = h(3) * (1.0D0 / h_norm)
      Wx = W(1)
      Wy = W(2)
      Wz = W(3)
! ----------------------------------------------------------------------
	  
! ----------------------------------------------------------------------
! parameter p: semi-latus rectum
!p = (h'*h)/GM;
      p = hh_dot / GM
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! i: inclination (in radians)
!i = arctan( sqrt(Wx^2+Wy^2) , Wz );
!i_deg = i * (180/pi);
      i_y = sqrt(Wx**2 + Wy**2) 
      i_x = Wz
      CALL arctan ( i_y, i_x , i_rad )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Omega: the right ascension of the ascending node (in radians)
!Omega = arctan( Wx , -Wy );
!Omega_deg = Omega * (180/pi);
      CALL arctan ( Wx, -1.0D0 * Wy , Omega_asc_rad )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! a: semi-major axis
!a = 1/( (2/sqrt(r'*r))-(v'*v)/ GM );
      a = 1.0D0 / ( 2.0D0 / r_norm - v_norm**2 / GM )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! n: the mean motion
!n = sqrt( GM /a^3);
      n = sqrt(GM / a**3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! e: the eccentricity
!e = sqrt(1- (p/a) );
      e = sqrt(1.0D0 - (p/a) )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! E: the eccentric anomaly (in radians)
!E = arctan( (r'*v)/(a^2*n) , 1-sqrt(r'*r)/a );
!E_deg = E * (180/pi);
      CALL productdot(r, v, rv_dot)
      rv_norm = sqrt(rv_dot)
      E_y = rv_dot / (a**2 * n)
      E_x = 1.0D0 - r_norm / a
      CALL arctan ( E_y, E_x , E_rad )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! M: Mean anomaly (in radians)
!M = E - e*sin(E);
!% converse in degrees
!M_deg = M * (180/pi);
      M_rad = E_rad - e * sin(E_rad)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! u: Argument of latitude (in radians)
!u = arctan( Z , -X*Wy+Y*Wx );
!% converse in degrees
!u_deg = u * (180/pi);
      u_y = Z
      u_x = -1.0D0 * X * Wy + Y * Wx
      CALL arctan ( u_y, u_x , u_rad )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! f : True anomaly (in radians)
!f = arctan( sqrt(1-e^2)*sin(E) , cos(E)-e );
!% converse in degrees
!f_deg = f * (180/pi);
      f_y = sqrt(1.0D0 - e**2) * sin(E_rad)
      f_x = cos(E_rad) - e
      CALL arctan ( f_y, f_x , f_rad )
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Angular elements: radians to degrees
      i_deg = i_rad * (180D0 / PI_global)
      Omega_asc_deg = Omega_asc_rad * (180D0 / PI_global)
      E_deg = E_rad * (180D0 / PI_global)
      M_deg = M_rad * (180D0 / PI_global)
      u_deg = u_rad * (180D0 / PI_global)
      f_deg = f_rad * (180D0 / PI_global)
! ----------------------------------------------------------------------
	  
! ----------------------------------------------------------------------
! omega : Argument of perigee (degrees)
!omega_deg = u_deg - f_deg;
!if omega_deg < 0
!    omega_deg = omega_deg + 360;
!end

      omega_per_deg = u_deg - f_deg
      if (omega_per_deg < 0.0D0) then
          omega_per_deg = omega_per_deg + 360D0
      end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Keplerian elements (angular elements in degrees)
!  - kepler:	Kepler elements
!    a:       	semi-major axis  (m)
!    e:      	eccentricity
!    i:      	inclination
!    Omega:  	right ascension of the ascending node
!    omega:  	argument of perigee
!    f:      	true anomaly
!    E:      	eccentric anomaly
!    M:      	Mean anomaly
!    u:      	argument of latitude

! kepler = [a,e,i,Omega,omega,f,M,E,u] 
      kepler(1) = a
      kepler(2) = e
      kepler(3) = i_deg
      kepler(4) = Omega_asc_deg
      kepler(5) = omega_per_deg
      kepler(6) = f_deg
      kepler(7) = E_deg
      kepler(8) = M_deg
      kepler(9) = u_deg
! ----------------------------------------------------------------------


end
