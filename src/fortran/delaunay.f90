SUBROUTINE delaunay (MJD , F1,F2,F3,F4,F5)


! ----------------------------------------------------------------------
! SUBROUTINE: delaunay.f90
! ----------------------------------------------------------------------
! Purposes
!  Computation of the fundamental arguments of the lunisolar nutation i.e. 
!  the Delaunay variables, based on the formula of the IERS Conventions 2010.
! ----------------------------------------------------------------------
! Input arguments:
! - mjd             : Modified Julian Day including the fraction of the day
!
! Output arguments:
! - F1,F2,F3,F4,F5  : Delaunay variables (l,l',F,D,Omega) in radians
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            November 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: MJD 
! OUT
      !REAL (KIND = prec_q), INTENT(OUT), DIMENSION(5) :: delaunay_var
      REAL (KIND = prec_q), INTENT(OUT) :: F1,F2,F3,F4,F5
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      !REAL (KIND = prec_q) :: F1,F2,F3,F4,F5
      REAL (KIND = prec_q) :: pi, arcsec2rad, JD_TT, taph
! ----------------------------------------------------------------------


pi = PI_global

! ----------------------------------------------------------------------
! Coefficient for Conversion from arcsec to radians
arcsec2rad = pi / (180.0D0 * 3600.0D0)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! parameter t
JD_TT = MJD + 2400000.5D0
taph = ( JD_TT - 2451545.0D0 ) / 36525.0D0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Delaunay variables for Sun and Moon
! ----------------------------------------------------------------------
! F1 = l = Mean anonaly of the Moon
F1 = 134.96340251D0 * (pi/180.0D0) + 1717915923.2178D0 * arcsec2rad * taph + 31.8792D0 * arcsec2rad * taph**2  &
     + 0.051635D0 * arcsec2rad * taph**3 - 0.00024470D0 * arcsec2rad * taph**4

! F2 = l' = Mean anonaly of the Sun
F2 = 357.52910918D0 * (pi/180.0D0) + 129596581.0481D0 * arcsec2rad * taph  & 
     - 0.5532D0 * arcsec2rad * taph**2 + 0.000136D0 * arcsec2rad * taph**3 - 0.00001149D0 * arcsec2rad * taph**4

! F3 = F = L - Omega
F3 = 93.27209062D0 * (pi/180.0D0) + 1739527262.8478D0 * arcsec2rad * taph - 12.7512D0 * arcsec2rad * taph**2  &  
     - 0.001037D0 * arcsec2rad * taph**3 + 0.00000417D0 * arcsec2rad * taph**4

! F4 = D = Mean Elongation of the Moon from the Sun
F4 = 297.85019547D0 * (pi/180.0D0) + 1602961601.2090D0 * arcsec2rad * taph - 6.3706D0 * arcsec2rad * taph**2  & 
     + 0.006593D0 * arcsec2rad * taph**3 - 0.00003169D0 * arcsec2rad * taph**4

! F5 = Omega = Mean Longitude of the Ascending Node of the Moon
F5 = 125.04455501D0 * (pi/180.0D0) - 6962890.5431D0 * arcsec2rad * taph + 7.4722D0 * arcsec2rad * taph**2 & 
     + 0.007702D0 * arcsec2rad * taph**3 - 0.00005939D0 * arcsec2rad * taph**4
! ----------------------------------------------------------------------

END
