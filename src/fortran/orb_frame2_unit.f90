SUBROUTINE orb_frame2_unit(r,v, Rrtn, er, et, en)


! ----------------------------------------------------------------------
! SUBROUTINE: orb_frame.f90
! ----------------------------------------------------------------------
! Purpose:
!  Transformation matrix from inertial to orbital frame
! ----------------------------------------------------------------------
! Input arguments:
! - r: 			position (m) 
! - v: 			velocity (m/sec)
!
! Output arguments:
! - Rrtn : 		Transformation matrix between orbital and inertial frames
! ----------------------------------------------------------------------
! Note 1:
!  Orbital frame components:
!  radial, along-track (tangential) and cross-track (normal)  [r t n]
!
! Note 2:
!  Transformation may be applied as follows:
!  [ar at an]^T = [Rrtn]  * [ax ay az]^T
!      (3x1)       (3x3)  *     (3x1) 
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia           10 May 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: r(3), v(3)
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: Rrtn(3,3)
      REAL (KIND = prec_q), INTENT(OUT) :: er(3), et(3), en(3) 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: X,Y,Z, Vx,Vy,Vz
      REAL (KIND = prec_q) :: r_norm, v_norm
      REAL (KIND = prec_q) :: rr_dot, vv_dot
      REAL (KIND = prec_q) :: h(3), h_dot, h_norm
      !REAL (KIND = prec_q) :: er(3), et(3), en(3) 
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
! Orbital Frame unit vectors
! ----------------------------------------------------------------------
! Radial component
      er = r / r_norm
	  
! Along-track or tangential component
      et = v / v_norm
	 
! cross product of r,v : angular momentum per unit mass
      CALL productcross(r,v , h)
      CALL productdot(h, h, h_dot)
      h_norm = sqrt(h_dot)

! Cross-track or normal component
     en = h / h_norm
! ----------------------------------------------------------------------

! Along-track or tangential component
!      CALL productcross(en, er , et)
!print *,"eT", et
   
! ----------------------------------------------------------------------
! Tranformation matrix from inertial to orbital frame
      Rrtn(1,1:3) = (/ er(1), er(2), er(3) /)
      Rrtn(2,1:3) = (/ et(1), et(2), et(3) /)
      Rrtn(3,1:3) = (/ en(1), en(2), en(3) /)
! ----------------------------------------------------------------------



end



