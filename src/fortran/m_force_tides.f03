MODULE m_force_tides


! ----------------------------------------------------------------------
! MODULE: m_force_tides.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the force_tides subroutine 
! 
! Subroutines contained within the module:
! - force_tides
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	16 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE force_tides(r, GM, ae, n_max, m_max, dCnm, dSnm, fx,fy,fz)


! ----------------------------------------------------------------------
! SUBROUTINE: force_tides
! ----------------------------------------------------------------------
! Purpose:
! Tides acceleration
!   Tides acceleration components are computed as partial derivatives of
!   spherical harmonics expansion of the tides harmonics coefficients.
! ----------------------------------------------------------------------
! Input arguments:
! - r:				position vector (m) in Terrestrial Reference System (ITRS)
!   				r = [x y z]
!
! Output arguments:
! - fx,fy,fz:		Tides acceleration's Cartesian components (m)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	November 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 2 November 2017:
!   Input arguments modifications 
! - Dr. Thomas Papanikolaou, 16 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 advantages in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      !USE mdl_num
      !USE mdl_param
      USE m_legendre
      USE m_legendre1
      !USE mdl_tides
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), DIMENSION(3) :: r
      REAL (KIND = prec_q), INTENT(IN) :: GM, ae
      INTEGER (KIND = prec_int8), INTENT(IN) :: n_max, m_max
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: dCnm, dSnm

! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: fx,fy,fz
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm, dPnm_norm
      INTEGER (KIND = prec_int8) :: Nmax_tide, n1, n, m, m_limit
      REAL (KIND = prec_q) :: phi,lamda,l, dV_r,dV_phi,dV_lamda
      REAL (KIND = prec_q) :: PDVrx(3,3), PDVrx_transp(3,3) , dV_rpl(3,1), fxyz(3,1)
      INTEGER (KIND = prec_int2) :: DeAllocateStatus
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Numerical Constants
! ----------------------------------------------------------------------
! Module mdl_param.f03
!GM = GFM_GM 
!ae = GFM_ae

! Module mdl_num.f90
! GM = GM_gfm
! ae = Earth_radius
! ----------------------------------------------------------------------

	  
! ----------------------------------------------------------------------
! Tides corrections Nmax
n1 = SIZE (dCnm, DIM=1)
Nmax_tide = n1 - 1
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! computation of spherical coordinates in radians
      CALL coord_r2sph (r , phi,lamda,l)  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! computation of normalized associated Legendre functions
      CALL legendre (phi, n_max, Pnm_norm)
! First-order derivatives of normalized associated Legendre functions
      CALL legendre_drv1 (phi, n_max, dPnm_norm)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Partial derivatives of potential w.r.t. the spherical coordinates (2nd approach):
! - dV_r     : partial derivative w.r.t. radius
! - dV_phi   : partial derivative w.r.t. latitude
! - dV_lamda : partial derivative w.r.t. longtitude
dV_r = 0.0D0
dV_phi = 0.0D0
dV_lamda = 0.0D0
DO n = 2 , n_max
    if (n > m_max) THEN
        m_limit = m_max
    else
        m_limit = n
    end if
    DO m = 0 , m_limit    
        dV_r = dV_r         + (-1.0D0 * (GM/l**2)) * (n+1)*1.0D0*((ae/l)**n) * Pnm_norm(n+1,m+1) &
		                      * (dCnm(n+1,m+1) * cos(m*lamda) + dSnm(n+1,m+1) * sin(m*lamda)) 
        dV_phi = dV_phi     + (GM / l) * ((ae/l)**n) * dPnm_norm(n+1,m+1) &
		                      * (dCnm(n+1,m+1)*cos(m*lamda) + dSnm(n+1,m+1) * sin(m*lamda)) 
        dV_lamda = dV_lamda + (GM / l) * m * ((ae/l)**n) * Pnm_norm(n+1,m+1) & 
		                      * (dSnm(n+1,m+1) * cos(m*lamda) - dCnm(n+1,m+1) * sin(m*lamda)) 
    END DO
END DO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Partial derivatives of (r,phi,lamda) with respect to (x,y,z)
! ----------------------------------------------------------------------
      PDVrx(1,1:3) = (/ cos(phi)*cos(lamda) , cos(phi)*sin(lamda) , sin(phi) /) 
      PDVrx(2,1:3) = (/ ( 1.0D0/l)*sin(phi)*cos(lamda) , ( 1.0D0/l)*sin(phi)*sin(lamda) , (-1.0D0/l)*cos(phi) /)
      PDVrx(3,1:3) = (/ ( -1.0D0/(l*cos(phi)) )*sin(lamda) , ( 1.0D0/(l*cos(phi)) )*cos(lamda) , 0.0D0 /)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Computation of Cartesian counterparts of the acceleration
! ----------------------------------------------------------------------
      dV_rpl = RESHAPE ( (/ dV_r, dV_phi, dV_lamda /) , (/ 3 , 1 /) )
       PDVrx_transp = TRANSPOSE(PDVrx)
       fx = PDVrx_transp(1,1) * dV_r + PDVrx_transp(1,2) * dV_phi + PDVrx_transp(1,3) * dV_lamda
       fy = PDVrx_transp(2,1) * dV_r + PDVrx_transp(2,2) * dV_phi + PDVrx_transp(2,3) * dV_lamda
       fz = PDVrx_transp(3,1) * dV_r + PDVrx_transp(3,2) * dV_phi + PDVrx_transp(3,3) * dV_lamda
! ----------------------------------------------------------------------

if (1<0) then
! ----------------------------------------------------------------------
! Arrays DeALLOCATION
! ----------------------------------------------------------------------
      DEALLOCATE (Pnm_norm, STAT = DeAllocateStatus)
!      print *,"Pnm_norm DeAllocateStatus=", DeAllocateStatus
      IF (DeAllocateStatus /= 0) THEN
         PRINT *, "Error: Memory deallocation error"
         PRINT *, "Error: SUBROUTINE force_tides.f90"
         PRINT *, "Error: Allocatable Array: Pnm_norm"
!         STOP "*** Not enough memory ***"
      END IF
! ----------------------------------------------------------------------
      DEALLOCATE (dPnm_norm, STAT = DeAllocateStatus)
!      print *,"dPnm_norm DeAllocateStatus=", DeAllocateStatus
      IF (DeAllocateStatus /= 0) THEN
         PRINT *, "Error: Memory deallocation error"
         PRINT *, "Error: SUBROUTINE force_tides.f"
         PRINT *, "Error: Allocatable Array: dPnm_norm"
!         STOP "*** Not enough memory ***"
      END IF
! ----------------------------------------------------------------------
end if

end Subroutine



END Module
