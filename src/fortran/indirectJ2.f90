SUBROUTINE indirectJ2(C20,Re,GMmoon,rMoon,GMsun,rSun , a_iJ2)


! ----------------------------------------------------------------------
! SUBROUTINE: indirectJ2.f90
! ----------------------------------------------------------------------
! Purpose:
!  Computation of the so-called indirect J2 effect of Sun and Moon.
! ----------------------------------------------------------------------
! Input arguments
! - C20     : fully normalized second zonal harmonic coefficient of Earth gravity field
! - Re      : Earth radius (meters)
! - GMmoon  : Moon gravity constant  (m^3/sec^2)
! - rMoon   : Moon body-fixed geocentric position vector (meters)
! - GMsun   : Sun gravity constant  (m^3/sec^2)
! - rSun    : Sun body-fixed geocentric position vector (meters)
!
! Output arguments:
! - a_iJ2   : Acceleration in Body-fixed system (m/s/s)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia         23 November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: rMoon, rSun
      REAL (KIND = prec_q), INTENT(IN) :: GMmoon, GMsun, C20, Re
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(3) :: a_iJ2
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q) :: xMoon,yMoon,zMoon,l_Moon , xSun,ySun,zSun,l_Sun
      REAL (KIND = prec_q) :: termMoon,termMoon_x,termMoon_y,termMoon_z , termSun,termSun_x,termSun_y,termSun_z
      REAL (KIND = prec_q) :: indirectJ2_coef
      REAL (KIND = prec_q) :: ax,ay,az
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Celestial bodies Earth-centered coordinates and distance from Earth 
! ---------------------------------------------------------------------------
! Moon
xMoon = rMoon(1)
yMoon = rMoon(2)
zMoon = rMoon(3)
l_Moon = sqrt(rMoon(1)**2 + rMoon(2)**2 + rMoon(3)**2)

! Sun
xSun = rSun(1)
ySun = rSun(2)
zSun = rSun(3)
l_Sun = sqrt(rSun(1)**2 + rSun(2)**2 + rSun(3)**2)
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Moon terms
termMoon = (GMmoon / l_Moon**3) * (Re / l_Moon)**2
termMoon_x = termMoon * (5.0D0 * (zMoon/l_Moon)**2 - 1.0D0) * xMoon
termMoon_y = termMoon * (5.0D0 * (zMoon/l_Moon)**2 - 1.0D0) * yMoon
termMoon_z = termMoon * (5.0D0 * (zMoon/l_Moon)**2 - 3.0D0) * zMoon
! ---------------------------------------------------------------------------
! Sun terms
termSun = (GMsun / l_Sun**3) * (Re / l_Sun)**2
termSun_x = termSun * (5.0D0 * (zSun/l_Sun)**2 - 1.0D0) * xSun
termSun_y = termSun * (5.0D0 * (zSun/l_Sun)**2 - 1.0D0) * ySun
termSun_z = termSun * (5.0D0 * (zSun/l_Sun)**2 - 3.0D0) * zSun
! ---------------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Indirect J2 effect
! ---------------------------------------------------------------------------
indirectJ2_coef = -1.0D0 * ( ( 3.0D0 * sqrt(5.0D0) ) / 2.0D0 ) * C20
! Acceleration cartesian components
ax = indirectJ2_coef * (termMoon_x + termSun_x)
ay = indirectJ2_coef * (termMoon_y + termSun_y)
az = indirectJ2_coef * (termMoon_z + termSun_z)
! ---------------------------------------------------------------------------
      a_iJ2(1) = ax
      a_iJ2(2) = ay
      a_iJ2(3) = az
! ---------------------------------------------------------------------------


END