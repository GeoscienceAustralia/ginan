MODULE m_pd_geopotential


! ----------------------------------------------------------------------
! MODULE: m_pd_geopotential
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the following subroutines 
! 
! Subroutines contained within the module:
! - pd_geopotential
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	11 December 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE pd_geopotential (GM, ae, r, nmax, mmax, Cnm, Snm, Fvec, Umatrix)


! ----------------------------------------------------------------------
! SUBROUTINE: pd_geopotential
! ----------------------------------------------------------------------
! Purpose:
! Partial derivatives of the acceleration due to the Earth Gravity field 
! (based on a gravity model expressed by a set of spherical harmonic coefficients)
! and the Tides effects (expressed as corrections to the spherical harmonic coefficients)
! ----------------------------------------------------------------------
! Input arguments:
! - GM:				Earth gravity constant  (m^3/sec^2)
! - ae:				Earth radius (meters)
! - r:				Position vector (m) in Terrestrial Reference System (ITRS)
!   				r = [x y z]
! - nmax:           maximum degree expansion
! - mmax:           maximum order expansion (mmax <= nmax)
! - Cnm, Snm:		Spherical Harmonic Coefficients (degree n, order m); dynamic allocatable arrays
! 
! Output arguments:
! - fxyz:			Acceleration vector cartesian components in ITRS
! - U: 				Matrix of the partial derivatives of the acceleration in ITRS
!   				given in the form of second partial derivatives of 
!					geopotential Uxx, Uyy, Uzz, Uxy, Uxz, Uyz 
! ----------------------------------------------------------------------
! Remark 1:
!  Cnm, Snm are dynamic allocatable arrays
!  Cnm and Snm arrays are formed into lower triangular matrices.
!  Coefficient Cnm corresponds to the matrix element Cnm(n+1,m+1)
!
! Remark 2:
!  Matrix of second partial derivatives:
!   U = [ Uxx   Uxy   Uxz 
!         Uxy   Uyy   Uyz
!         Uxz   Uyz   Uzz ]  
! 
!  Partial derivatives are noted as:
!   dfx / dy = Uxy,     dfx / dx = Uxx
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	11 December 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE m_legendre
      USE m_legendre1
      USE m_legendre2
      USE m_matrixRxR
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: GM, ae
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: r
      INTEGER (KIND = prec_int8), INTENT(IN) :: nmax, mmax
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: Cnm, Snm
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: Fvec(3)
      REAL (KIND = prec_q), INTENT(OUT) :: Umatrix(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm, dPnm_norm, d2Pnm_norm
      REAL (KIND = prec_q) :: phi, lamda, radius, fgrav_3d
      REAL (KIND = prec_q) :: l, rdist
      REAL (KIND = prec_q) :: fr, ftheta, flamda
      REAL (KIND = prec_q) :: dV_r, dV_phi, dV_lamda
      INTEGER (KIND = prec_int8) :: n, m, m_limit
      INTEGER (KIND = prec_int8) :: sz_tides, Nmax_tide
      INTEGER (KIND = prec_int2) :: comp_option
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      REAL (KIND = prec_q) :: fx, fy, fz
      REAL (KIND = prec_q), DIMENSION(3) :: fxyz, dV_sph
      REAL (KIND = prec_q), DIMENSION(3,3) :: PDVrx, PDVrx_T
      REAL (KIND = prec_q), DIMENSION(3,3) :: Vrtl, pdv2_r, pdv2_theta, pdv2_lamda, pdvFxyz_rtl
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: U1, U2, U3
      REAL (KIND = prec_q) :: theta, pi  
      REAL (KIND = prec_q) :: Vrr, Vrt, VrL, Vtt, VtL, VLL
      REAL (KIND = prec_q) :: Vrr_f, Vrt_f, VrL_f, Vtt_f, VtL_f, VLL_f  
      REAL (KIND = prec_q) :: pdvfx_r, pdvfy_r, pdvfz_r
      REAL (KIND = prec_q) :: pdvfx_theta, pdvfy_theta, pdvfz_theta
      REAL (KIND = prec_q) :: pdvfx_lamda, pdvfy_lamda, pdvfz_lamda  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Numerical Constants
      pi = PI_global
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! computation of spherical coordinates in radians
CALL coord_r2sph (r , phi,lamda,radius)
l = radius
rdist = radius
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Normalized associated Legendre functions
CALL legendre (phi, nmax, Pnm_norm)
! First-order derivatives of normalized associated Legendre functions
CALL legendre_drv1 (phi, nmax, dPnm_norm)
! Second-order derivatives of the Normalized Associated Legendre functions
CALL legendre2 (phi, nmax, d2Pnm_norm)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! 2nd approach:
! Partial derivatives of potential with respect to the spherical coordinates:
! - dV_r     : partial derivative of geopotential to radius
! - dV_phi   : partial derivative of geopotential to latitude
! - dV_lamda : partial derivative of geopotential to longtitude
dV_r = 0.D0
dV_phi = 0.D0
dV_lamda = 0.D0
Do n = 2 , nmax
    If (n > mmax) Then
        m_limit = mmax
    else
        m_limit = n
    End IF
        Do m = 0 , m_limit    
            dV_r = dV_r + 1.0D0*(n+1)*((ae/l)**n) * Pnm_norm(n+1,m+1) * (Cnm(n+1,m+1) * cos(m*lamda) + Snm(n+1,m+1) * sin(m*lamda)) 
            dV_phi = dV_phi     + ((ae/l)**n) * dPnm_norm(n+1,m+1) * (Cnm(n+1,m+1)*cos(m*lamda)+Snm(n+1,m+1)*sin(m*lamda))
            dV_lamda = dV_lamda + m * ((ae/l)**n) * Pnm_norm(n+1,m+1) * (Snm(n+1,m+1)*cos(m*lamda)-Cnm(n+1,m+1)*sin(m*lamda))
        end Do
end Do
dV_r = - GM / (l**2) - (GM/l**2) * dV_r
dV_phi = (GM / l) * dV_phi
dV_lamda = (GM / l) * dV_lamda
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Partial derivatives of (r,phi,lamda) with respect to (x,y,z)
! ----------------------------------------------------------------------
PDVrx (1,1:3) = (/ cos(phi)*cos(lamda) ,              cos(phi)*sin(lamda) ,             sin(phi) /) 
PDVrx (2,1:3) = (/ (1.0D0/l)*sin(phi)*cos(lamda) ,    (1.0D0/l)*sin(phi)*sin(lamda),    (-1.0D0/l)*cos(phi) /)
PDVrx (3,1:3) = (/ ( -1.D0/(l*cos(phi)) )*sin(lamda), ( 1.D0/(l*cos(phi)) )*cos(lamda),         0.0D0 /)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Computation of Cartesian counterparts of the acceleration
!fxyz = PDVrx' * [dV_r; dV_phi; dV_lamda];
PDVrx_T = TRANSPOSE (PDVrx)
dV_sph = (/ dV_r, dV_phi, dV_lamda /)
CALL matrix_Rr (PDVrx_T, dV_sph , fxyz)
fx = fxyz(1)
fy = fxyz(2)
fz = fxyz(3)
Fvec = fxyz
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Geopotential 2nd-order partial derivatives in r,theta,lamda components
! ----------------------------------------------------------------------
! Vrr, VrTheta (Vrt), VrLamda(VrL), V_ThetaTheta (Vtt), V_ThetaLamda (VtL)
! V_LamdaLamda (VLL)
! ----------------------------------------------------------------------
Vrr = 0.D0
Vrt = 0.D0
VrL = 0.D0
Vtt = 0.D0 
VtL = 0.D0 
VLL = 0.D0
Do n = 0 , nmax
        Do m = 0 , n        
            Vrr_f = (GM/ae**3) * 1.D0*(n+1)*(n+2) * (ae/l)**(n+3)
            Vrr = Vrr + Vrr_f * ( Cnm(n+1,m+1) * cos(m*lamda) + Snm(n+1,m+1) * sin(m*lamda) ) * Pnm_norm(n+1,m+1)

            Vrt_f = - (GM/ae**2) * 1.D0*(n+1) * (ae/l)**(n+2) 					!% Eq.6.24 Revised 2012
            Vrt = Vrt + Vrt_f * ( Cnm(n+1,m+1) * cos(m*lamda) + Snm(n+1,m+1) * sin(m*lamda) ) * dPnm_norm(n+1,m+1)

            VrL_f = (GM/ae**2) * 1.D0*(n+1) * (ae/l)**(n+2)
            VrL = VrL + VrL_f * m * ( Cnm(n+1,m+1) * sin(m*lamda) - Snm(n+1,m+1) * cos(m*lamda) ) * Pnm_norm(n+1,m+1)

            Vtt_f = (GM / ae) * (ae / l)**(n+1)
            Vtt = Vtt + Vtt_f * ( Cnm(n+1,m+1) * cos(m*lamda) + Snm(n+1,m+1) * sin(m*lamda) ) * d2Pnm_norm(n+1,m+1)

            VtL_f = (GM / ae) * (ae / l)**(n+1)
            !% Corrected : May 2012
            VtL = VtL + VtL_f * m * ( - Cnm(n+1,m+1) * sin(m*lamda) + Snm(n+1,m+1) * cos(m*lamda) ) * dPnm_norm(n+1,m+1)

            VLL_f = (GM / ae) * (ae / l)**(n+1) 
            VLL = VLL - VLL_f * m**2 * ( Cnm(n+1,m+1) * cos(m*lamda) + Snm(n+1,m+1) * sin(m*lamda) ) * Pnm_norm(n+1,m+1) 
        end Do
end Do

! Geopotential 2nd-order partial derivatives in local tangent frame (er, etheta, elamda)
Vrtl(1,:) = (/ Vrr , Vrt , VrL /)
Vrtl(2,:) = (/ Vrt , Vtt , VtL /)   
Vrtl(3,:) = (/ VrL , VtL , VLL /) 	 
! ----------------------------------------------------------------------

theta = pi/2 - phi

! ----------------------------------------------------------------------
! Partial derivatives of PDVrx with respect to (r,theta,lamda)
! ----------------------------------------------------------------------
pdv2_r (1,1:3) = (/ 0.0D0 , 0.0D0 , 0.0D0 /) 
pdv2_r (2,1:3) = (/ (-1.0D0/rdist**2)*(cos(theta)*cos(lamda)), (-1.0D0/rdist**2)*(cos(theta)*sin(lamda)),  & 
                    (1.0D0/rdist**2)*sin(theta) /)
pdv2_r (3,1:3) = (/  (1.0D0/rdist**2)*(sin(lamda)/sin(theta)), (-1.0D0/rdist**2)*(cos(lamda)/sin(theta)),  0.0D0 /)
   
pdv2_theta (1,1:3) = (/ cos(theta)*cos(lamda), cos(theta)*sin(lamda), -sin(theta) /)
pdv2_theta (2,1:3) = (/ (-1.0D0/rdist)*(sin(theta)*cos(lamda)), (-1.0D0/rdist)*(sin(theta)*sin(lamda)),  & 
                        (-1.0D0/rdist)*cos(theta) /)
pdv2_theta (3,1:3) = (/ (1.0D0/rdist)*(sin(lamda)*cos(theta)/sin(theta)**2),  & 
                        (-1.0D0/rdist)*(cos(lamda)*cos(theta)/sin(theta)**2), 0.0D0 /)

pdv2_lamda (1,1:3) = (/ -sin(theta)*sin(lamda), sin(theta)*cos(lamda), 0.0D0 /)
pdv2_lamda (2,1:3) = (/ (-1.0D0/rdist)*(cos(theta)*sin(lamda)),  (1.0D0/rdist)*(cos(theta)*cos(lamda)), 0.0D0 /)
pdv2_lamda (3,1:3) = (/ (-1.0D0/rdist)*(cos(lamda)/sin(theta)), (-1.0D0/rdist)*(sin(lamda)/sin(theta)), 0.0D0 /)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Partial derivatives of (Fx,Fy,Fz) with respect to (r,theta,lamda)
! ----------------------------------------------------------------------
! pdv(fxyz/r)
pdvfx_r = Vrr * PDVrx(1,1) + Vrt * PDVrx(2,1) + VrL * PDVrx(3,1) +           &
          dV_r * pdv2_r(1,1) + dV_phi * pdv2_r(2,1) + dV_lamda * pdv2_r(3,1)
      
pdvfy_r = Vrr * PDVrx(1,2) + Vrt * PDVrx(2,2) + VrL * PDVrx(3,2) +           &
          dV_r * pdv2_r(1,2) + dV_phi * pdv2_r(2,2) + dV_lamda * pdv2_r(3,2)
      
pdvfz_r = Vrr * PDVrx(1,3) + Vrt * PDVrx(2,3) + VrL * PDVrx(3,3) +           &
          dV_r * pdv2_r(1,3) + dV_phi * pdv2_r(2,3) + dV_lamda * pdv2_r(3,3)

! pdv(fxyz/theta)
pdvfx_theta = Vrt * PDVrx(1,1) + Vtt * PDVrx(2,1) + VtL * PDVrx(3,1) +                      &
              dV_r * pdv2_theta(1,1) + dV_phi * pdv2_theta(2,1) + dV_lamda * pdv2_theta(3,1)
      
pdvfy_theta = Vrt * PDVrx(1,2) + Vtt * PDVrx(2,2) + VtL * PDVrx(3,2) +                      &   
              dV_r * pdv2_theta(1,2) + dV_phi * pdv2_theta(2,2) + dV_lamda * pdv2_theta(3,2)

pdvfz_theta = Vrt * PDVrx(1,3) + Vtt * PDVrx(2,3) + VtL * PDVrx(3,3) +                      &
              dV_r * pdv2_theta(1,3) + dV_phi * pdv2_theta(2,3) + dV_lamda * pdv2_theta(3,3)

! pdv(fxyz/lamda)
pdvfx_lamda = VrL * PDVrx(1,1) + VtL * PDVrx(2,1) + VLL * PDVrx(3,1) +                      &  
              dV_r * pdv2_lamda(1,1) + dV_phi * pdv2_lamda(2,1) + dV_lamda * pdv2_lamda(3,1)

pdvfy_lamda = VrL * PDVrx(1,2) + VtL * PDVrx(2,2) + VLL * PDVrx(3,2) +                      &
              dV_r * pdv2_lamda(1,2) + dV_phi * pdv2_lamda(2,2) + dV_lamda * pdv2_lamda(3,2)
          
pdvfz_lamda = VrL * PDVrx(1,3) + VtL * PDVrx(2,3) + VLL * PDVrx(3,3) +                      & 
              dV_r * pdv2_lamda(1,3) + dV_phi * pdv2_lamda(2,3) + dV_lamda * pdv2_lamda(3,3)
          
! matrix : pdvFxyz_rtl
pdvFxyz_rtl (1,1:3) = (/ pdvfx_r,   pdvfx_theta,   pdvfx_lamda /)
pdvFxyz_rtl (2,1:3) = (/ pdvfy_r,   pdvfy_theta,   pdvfy_lamda /)
pdvFxyz_rtl (3,1:3) = (/ pdvfz_r,   pdvfz_theta,   pdvfz_lamda /)
! ----------------------------------------------------------------------
          

! ----------------------------------------------------------------------
! Geopotential 2nd-order partial derivatives in ITRS X,Y,Z components
! ----------------------------------------------------------------------
!Umatrix = PDVrx' * pdvFxyz_rtl'

! Allocatable arrays
ALLOCATE (U1(3,3), STAT = AllocateStatus)
ALLOCATE (U2(3,3), STAT = AllocateStatus)
ALLOCATE (U3(3,3), STAT = AllocateStatus)

U1 = TRANSPOSE(PDVrx)
U2 = TRANSPOSE(pdvFxyz_rtl)
Call matrixRxR (U1, U2, U3)								
Umatrix = U3


END SUBROUTINE



END Module
