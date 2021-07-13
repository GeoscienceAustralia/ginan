SUBROUTINE tides_solid1 (rmoon, rsun, GMearth, Re, GMmoon, GMsun, dCnm, dSnm)


! ----------------------------------------------------------------------
! SUBROUTINE: tides_solid1.f90
! ----------------------------------------------------------------------
! Purpose:
! Solid Earth Tides (frequency independent)
!  Solid Earth Tides (Step 1) based on the IERS Conventions 2010.
!  tides_solid1 implements the Step1 that refers to the computation of the
!  frequency independent corrections to the geopotential's  spherical harmonic 
!  coefficients. 
! ----------------------------------------------------------------------
! Input arguments
! - GMearth : Earth gravity constant  (m^3/sec^2)
! - Re      : Earth radius  (meters)
! - GMmoon  : Moon gravity constant  (m^3/sec^2)
! - rmoon   : Moon body-fixed geocentric position vector (meters)
! - GMsun   : Sun gravity constant  (m^3/sec^2)
! - rsun    : Sun body-fixed geocentric position vector (meters)
!
! Output arguments:
! - dCnm    : Cnm corrections matrix
! - dSnm    : Snm corrections matrix
! ----------------------------------------------------------------------
! Remark:
!  Computed dCnm and dSnm are formed into lower triangular matrices.
!  Coefficient dCnm corresponds to the matrix element dCnm(n+1,m+1).
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia          6 November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_legendre
      USE m_legendre
      USE mdl_planets
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: rmoon, rsun 
      REAL (KIND = prec_q), INTENT(IN) :: GMearth, Re, GMmoon, GMsun
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(5,5) :: dCnm, dSnm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------  
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm
      REAL (KIND = prec_q) :: phi_sun,lamda_sun,lsun , phi_moon,lamda_moon,lmoon
      REAL (KIND = prec_q) :: dCnm_moon,dCnm_sun,dSnm_moon,dSnm_sun
      REAL (KIND = prec_q), DIMENSION(4,4) :: knm, Pnm_norm_moon, Pnm_norm_sun
      REAL (KIND = prec_q), DIMENSION(3,3) :: knm_plus
      INTEGER (KIND = prec_int8) :: n, m, Nmax
      INTEGER (KIND = prec_int2) :: DeAllocateStatus
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Love numbers (Knm): Table 6.3 
! ----------------------------------------------------------------------
! Elastic Earth
knm(2+1,0+1) = 0.29525D0
knm(2+1,1+1) = 0.29470D0
knm(2+1,2+1) = 0.29801D0
knm(3+1,0+1) = 0.093D0
knm(3+1,1+1) = 0.093D0
knm(3+1,2+1) = 0.093D0
knm(3+1,3+1) = 0.094D0
! Knm(+) matrix
knm_plus(2+1,0+1) = -0.00087D0
knm_plus(2+1,1+1) = -0.00079D0
knm_plus(2+1,2+1) = -0.00057D0
! ----------------------------------------------------------------------
! Anelastic Earth
!knm(2+1,0+1) = 0.30190D0
!knm(2+1,1+1) = 0.29830D0
!knm(2+1,2+1) = 0.30102D0
!! Knm(+) matrix
!knm_plus(2+1,0+1) = -0.00089D0
!knm_plus(2+1,1+1) = -0.00080D0
!knm_plus(2+1,2+1) = -0.00057D0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Earth
!GMearth = GM_gfm
!Re = Earth_radius
! ----------------------------------------------------------------------
! GM constants
! Spherical coordinates (radians)
! Normalized associated Legendre functions
! Moon
      !GMmoon = GMconst(10)
      CALL coord_r2sph (rmoon , phi_moon,lamda_moon,lmoon)
	  Nmax = 3
      CALL legendre (phi_moon, Nmax, Pnm_norm)
      Pnm_norm_moon = Pnm_norm
      DEALLOCATE (Pnm_norm, STAT = DeAllocateStatus)	  
! Sun
      !GMsun = GMconst(11)
      CALL coord_r2sph (rsun , phi_sun,lamda_sun,lsun)
	  Nmax = 3
      CALL legendre (phi_sun, Nmax, Pnm_norm)
      Pnm_norm_sun = Pnm_norm
      DEALLOCATE (Pnm_norm, STAT = DeAllocateStatus)	  
! ----------------------------------------------------------------------

dCnm      = 0.0D0
dCnm      = 0.0D0
dCnm_moon = 0.0D0
dSnm_moon = 0.0D0
dCnm_sun  = 0.0D0
dSnm_sun  = 0.0D0

! ----------------------------------------------------------------------
! Computation of dCnm and dSnm terms for degree and order 3
DO n = 2 , 3
    DO m = 0 , n
        ! dCnm term
        dCnm_moon = (GMmoon / GMearth) * (Re / lmoon)**(n+1D0) * Pnm_norm_moon(n+1,m+1) * cos(m*lamda_moon)
        dCnm_sun  = (GMsun / GMearth) * (Re / lsun)**(n+1D0) * Pnm_norm_sun(n+1,m+1) * cos(m*lamda_sun)
        dCnm(n+1,m+1) = ( knm(n+1,m+1) / (2D0 * n + 1D0) ) * (dCnm_moon + dCnm_sun)
        ! dSnm term
        dSnm_moon = (GMmoon / GMearth) * (Re / lmoon)**(n+1D0) * Pnm_norm_moon(n+1,m+1) * sin(m*lamda_moon)
        dSnm_sun  = (GMsun / GMearth) * (Re / lsun)**(n+1D0) * Pnm_norm_sun(n+1,m+1) * sin(m*lamda_sun)
        dSnm(n+1,m+1) = ( knm(n+1,m+1) / (2D0*n + 1D0) ) * (dSnm_moon + dSnm_sun)
    END DO
END DO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Computation of dCnm and dSnm terms for degree n=4 and order m=0,1,2
DO m = 0 , 2
    ! dCnm terms
    dCnm_moon = (GMmoon / GMearth) * (Re / lmoon)**3 * Pnm_norm_moon(2+1,m+1) * cos(m*lamda_moon)
    dCnm_sun  = (GMsun / GMearth) * (Re / lsun)**3 * Pnm_norm_sun(2+1,m+1) * cos(m*lamda_sun)
    dCnm(4+1,m+1) = ( knm_plus(2+1,m+1) / 5D0 ) * (dCnm_moon + dCnm_sun)

    ! dSnm terms
    dSnm_moon = (GMmoon / GMearth) * (Re / lmoon)**3 * Pnm_norm_moon(2+1,m+1) * sin(m*lamda_moon)
    dSnm_sun  = (GMsun / GMearth) * (Re / lsun)**3 * Pnm_norm_sun(2+1,m+1) * sin(m*lamda_sun)
    dSnm(4+1,m+1) = ( knm_plus(2+1,m+1) / 5D0 ) * (dSnm_moon + dSnm_sun)
END DO
! ----------------------------------------------------------------------


END
