
SUBROUTINE force_ant (r,fx,fy,fz)


! ----------------------------------------------------------------------
! SUBROUTINE: force_ant.f90
! ----------------------------------------------------------------------
! Purpose:
! Acceleration due to the antenna thrust effect  
! ----------------------------------------------------------------------
! Input arguments:
! - r            : satellite position vector (m)
! 
! Output arguments:
!
! - fx,fy,fz:	 : Accelerations in the inertial frame (m/s^2)
! 
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
!
! Created:	03-12-2018
!
! Changes:       
!
! Copyright:  GEOSCIENCE AUSTRALIA, AUSTRALIA
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: r
      REAL (KIND = prec_q)               :: fx,fy,fz
!
!-----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: rsat
      REAL (KIND = prec_q), DIMENSION(3) :: er

      REAL (KIND = prec_q) :: f_ant
! ----------------------------------------------------------------------

! Radial vector of satellite
! --------------------------
      rsat = sqrt(r(1)**2+r(2)**2+r(3)**2)
      er(1)=r(1)/sqrt(r(1)**2+r(2)**2+r(3)**2)
      er(2)=r(2)/sqrt(r(1)**2+r(2)**2+r(3)**2)
      er(3)=r(3)/sqrt(r(1)**2+r(2)**2+r(3)**2)
!print *, 'r=', r(1),r(2),r(3)


! The acceleration caused by the satellite antenna thrust
! --------------------------------------------------------
      f_ant = POWER/(MASS*cslight)

! --------------------------------------------------------

! forces in the inertial frame
!-------------------------------
   fx = (f_ant)*er(1)
   fy = (f_ant)*er(2)
   fz = (f_ant)*er(3)
!print*,fx, fy, fz
   

END SUBROUTINE
