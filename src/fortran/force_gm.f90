SUBROUTINE force_gm (GM, r, fx,fy,fz )


! ----------------------------------------------------------------------
! SUBROUTINE: force_gm.f90
! ----------------------------------------------------------------------
! Purpose:
! Acceleration due to the central Earth Gravity Field
!  Computation of satellite's acceleration based on Newton's law of gravity
!  considering Earth as a point mass
! ----------------------------------------------------------------------
! Input arguments:
! - r:				position vector (m)
! 
! Output arguments:
! - fx,fy,fz:		Acceleration's cartesian components (m)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	July 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: r
      REAL (KIND = prec_q), INTENT(IN) :: GM	
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: fx,fy,fz
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: phi, lamda, radius
      REAL (KIND = prec_q) :: fr
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Numerical Constants
!      GM = GM_global 
! ----------------------------------------------------------------------


! computation of spherical coordinates
      CALL coord_r2sph (r , phi,lamda,radius)

! Gradient of geopotential V
      fr = - GM / (radius ** 2) 

! Cartesian counterparts (fx,fy,fz) of acceleration fr
      fx = fr * cos(phi) * cos(lamda)
      fy = fr * cos(phi) * sin(lamda)
      fz = fr * sin(phi)	  
	  
END
