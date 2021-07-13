SUBROUTINE coord_r2sph (r , phi,lamda,radius)


! ----------------------------------------------------------------------
! SUBROUTINE: coord_r2sph.f90
! ----------------------------------------------------------------------
! Purpose:
! Geocentric spherical coordinates
!  Computation of the (geocentric) spherical coordinates i.e. longitude
!  and latitude, from position vector components (Cartesian coordinates)
! ----------------------------------------------------------------------
! Input arguments:
! - r:				Position vector  r = [x y z]
!
! Output arguments:
! - phi:			Latitude  (radians)
! - lamda:			Longitude (radians)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia                July 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ---------------------------------------------------------------------------
! Dummy arguments declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: r
      REAL (KIND = prec_q), INTENT(OUT) :: phi,lamda,radius
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Local variables declaration
! ---------------------------------------------------------------------------
      REAL (KIND = prec_q) :: x,y,z
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
      x = r(1)
      y = r(2)
      z = r(3)

      radius = sqrt(x**2 + y**2 + z**2)

! Phi computation: analytical
      phi = atan( z / sqrt(x**2 + y**2) )

! Lamda computation
      CALL arctan (y,x, lamda)
! ---------------------------------------------------------------------------

END
