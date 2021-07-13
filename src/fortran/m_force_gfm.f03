MODULE m_force_gfm


! ----------------------------------------------------------------------
! MODULE: m_force_gfm.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the force_gfm subroutine 
! 
! Subroutines contained within the module:
! - force_gfm
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	15 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE force_gfm (GM, ae, r, n_max, m_max, Cnm, Snm , fx,fy,fz)


! ----------------------------------------------------------------------
! SUBROUTINE: force_gfm.f90
! ----------------------------------------------------------------------
! Purpose:
! Earth Gravity field effects
!  Computation of the acceleration based on a global Earth Gravity Field  
!  Model expressed by a set of spherical harmonic coefficients
! ----------------------------------------------------------------------
! Input arguments:
! - GM:				Earth gravity constant  (m^3/sec^2)
! - ae:				Earth radius (meters)
! - r:				Position vector (m) in Terrestrial Reference System (ITRS)
!   				r = [x y z]
! - n_max:          maximum degree expansion
! - m_max:          maximum order expansion (m_max <= n_max)
! - Cnm, Snm:		Spherical Harmonic Coefficients (degree n, order m); dynamic allocatable arrays
! 
! Output arguments:
! - fx,fy,fz:		Acceleration's cartesian components in ITRS (m)
! ----------------------------------------------------------------------
! Remark 1:
!  Cnm, Snm are dynamic allocatable arrays
!  Cnm and Snm arrays are formed into lower triangular matrices.
!  Coefficient Cnm corresponds to the matrix element Cnm(n+1,m+1)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	August 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 15 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 advantages in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_legendre
      USE m_legendre1
      USE mdl_param
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: GM, ae
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(3) :: r
      INTEGER (KIND = prec_int8), INTENT(IN) :: n_max, m_max
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: Cnm, Snm
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: fx,fy,fz
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm, dPnm_norm
      REAL (KIND = prec_q) :: phi, lamda, radius, fgrav_3d
      REAL (KIND = prec_q) :: fr, ftheta, flamda
      REAL (KIND = prec_q) :: dV_r, dV_phi, dV_lamda
      INTEGER (KIND = prec_int8) :: n, m, m_limit
      INTEGER (KIND = prec_int2) :: comp_option
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! computation of spherical coordinates in radians
      CALL coord_r2sph (r , phi,lamda,radius)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! computation of normalized associated Legendre functions
      CALL legendre (phi, n_max, Pnm_norm)
! ----------------------------------------------------------------------
! First-order derivatives of normalized associated Legendre functions
      CALL legendre_drv1 (phi, n_max, dPnm_norm)
! ----------------------------------------------------------------------
!      PRINT *,"Pnm_norm", Pnm_norm
!      PRINT *,"dPnm_norm", dPnm_norm


! ----------------------------------------------------------------------
! Computation of acceleration vector components in a local tangent frame:
! ----------------------------------------------------------------------
      comp_option = 2
! ----------------------------------------------------------------------
! 1st approach:
      IF (comp_option == 1) THEN
! ----------------------------------------------------------------------
! - fr     : radius component
! - flamda : geocentric longitude component 
! - ftheta : geocentric latitude component (theta = 90-latitude)
! ----------------------------------------------------------------------
       fr = 0.0D0
       ftheta = 0.0D0
       flamda = 0.0D0
! ----------------------------------------------------------------------
       DO n = 2 , n_max
           IF (n > m_max) THEN
               m_limit = m_max
           ELSE
               m_limit = n
           END IF
           DO m = 0 , m_limit
             fr = fr+ ( -1.0D0*(n+1) * ((ae/radius)**n) * Pnm_norm(n+1,m+1) &
			 * (Cnm(n+1,m+1)*cos(m*lamda) + Snm(n+1,m+1)*sin(m*lamda)) )
	 
             ftheta = ftheta + ((ae/radius)**n) * dPnm_norm(n+1,m+1)    &
			 * (Cnm(n+1,m+1)*cos(m*lamda)+Snm(n+1,m+1)*sin(m*lamda))

             flamda = flamda + ((ae/radius)**n) * (1.0D0/cos(phi))        &
     		   * Pnm_norm(n+1,m+1) * m * (Snm(n+1,m+1)*cos(m*lamda)-Cnm(n+1,m+1)*sin(m*lamda))
           END DO
       END DO
! ----------------------------------------------------------------------
      fr = fr * (GM/radius**2) - GM / (radius**2)
      ftheta = ftheta * (GM/radius**2) 
      flamda = flamda * (GM/radius**2)
! ----------------------------------------------------------------------
! Cartesian counterparts in the Earth fixed system (ITRF)
      fx = fr * cos(phi)*cos(lamda) + ftheta * sin(phi)*cos(lamda) - flamda * sin(lamda)
      fy = fr * cos(phi)*sin(lamda) + ftheta * sin(phi)*sin(lamda) + flamda * cos(lamda)
	  fz = fr * sin(phi) - ftheta * cos(phi)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 2nd approach:
      ELSE
! ----------------------------------------------------------------------
! Partial derivatives of potential with respect to spherical coordinates :
! - dV_r     : partial derivative of geopotential to radius
! - dV_phi   : partial derivative of geopotential to latitude
! - dV_lamda : partial derivative of geopotential to longtitude
! ----------------------------------------------------------------------
      dV_r = 0.0D0
      dV_phi = 0.0D0
      dV_lamda = 0.0D0

      DO n = 2 , n_max
          IF (n > m_max) THEN
              m_limit = m_max
          ELSE
              m_limit = n
          END IF
          DO m = 0 , m_limit    
            dV_r = dV_r + ( -1.0D0*(n+1) * ((ae/radius)**n)                 &
                   * Pnm_norm(n+1,m+1)                                  &
                   * ( Cnm(n+1,m+1) * cos(m*lamda)                      &
                   + Snm(n+1,m+1) * sin(m*lamda)) )

            dV_phi = dV_phi + ((ae/radius)**n) * dPnm_norm(n+1,m+1) *   &
                     ( Cnm(n+1,m+1)*cos(m*lamda) + Snm(n+1,m+1)*sin(m*lamda) )
	 
            dV_lamda = dV_lamda + m * ((ae/radius)**n) * Pnm_norm(n+1,m+1) &
                      * ( Snm(n+1,m+1)*cos(m*lamda) - Cnm(n+1,m+1)*sin(m*lamda) )
          END DO
      END DO
      dV_r = - GM / (radius**2) + (GM/radius**2) * dV_r
      dV_phi = (GM / radius) * dV_phi
      dV_lamda = (GM / radius) * dV_lamda

! Computation of Cartesian counterparts of the acceleration
      fx = dV_r * cos(phi)*cos(lamda) + dV_phi * (1.0D0/radius)*sin(phi)*cos(lamda) & 
           + dV_lamda * (-1.0D0/(radius*cos(phi)))*sin(lamda)
      fy = dV_r * cos(phi)*sin(lamda) + dV_phi * (1.0D0/radius)*sin(phi)*sin(lamda) & 
           + dV_lamda * (1.0D0/(radius*cos(phi)))*cos(lamda)
      fz = dV_r * sin(phi) + dV_phi * (-1.0D0/radius)*cos(phi)
! ----------------------------------------------------------------------
      END IF

	  
if (1<0) then	  
! ----------------------------------------------------------------------
! Arrays DeALLOCATION
! ----------------------------------------------------------------------
      DEALLOCATE (Pnm_norm, STAT = DeAllocateStatus)
!      print *,"Pnm_norm DeAllocateStatus=", DeAllocateStatus
      IF (DeAllocateStatus /= 0) THEN
         PRINT *, "Error: Memory deallocation error"
         PRINT *, "Error: SUBROUTINE force_gfm.f"
         PRINT *, "Error: Allocatable Array: Pnm_norm"
!         STOP "*** Not enough memory ***"
      END IF
! ----------------------------------------------------------------------
      DEALLOCATE (dPnm_norm, STAT = DeAllocateStatus)
!      print *,"dPnm_norm DeAllocateStatus=", DeAllocateStatus
      IF (DeAllocateStatus /= 0) THEN
         PRINT *, "Error: Memory deallocation error"
         PRINT *, "Error: SUBROUTINE force_gfm.f"
         PRINT *, "Error: Allocatable Array: dPnm_norm"
!         STOP "*** Not enough memory ***"
      END IF
! ----------------------------------------------------------------------
end if

	  
END SUBROUTINE



END Module
