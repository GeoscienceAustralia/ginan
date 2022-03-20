MODULE m_legendre1


! ----------------------------------------------------------------------
! MODULE: m_legendre1.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the legendre_drv1 subroutine 
! 
! Subroutines contained within the module:
! - legendre_drv1
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	16 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains

SUBROUTINE legendre_drv1 (phi, nmax, dPnm_norm)


! ---------------------------------------------------------------------------
! Subroutine:  legendre_drv1.f90
! ---------------------------------------------------------------------------
! Purpose:
! Computation of the first order derivatives of the Normalized Associated
! Legendre functions
! 
! Evaluation of Legendre functions is based on Recurrence relations
! ---------------------------------------------------------------------------
! Input arguments:
! - phi:			Spherical latitude (radians)
! - nmax:			Maximum degree expansion
! 
! Output arguments:
! - dPnm_norm: 		First order derivatives of Normalized Associated Legendre
!   				functions up to degree n
! ---------------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	September 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 16 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 advantages in dynamic memory allocation
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_legendre
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int8), INTENT(IN) :: nmax
      REAL (KIND = prec_q), INTENT(IN) :: phi 
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: dPnm_norm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm
      INTEGER (KIND = prec_int8) :: n, m	  
      INTEGER (KIND = prec_int2) :: AllocateStatus
      REAL (KIND = prec_q) :: pi
      REAL (KIND = prec_q) :: theta, c, s
      REAL (KIND = prec_q) :: dPoo, dP10,dP11, dP_f1,dP_f2,dP_f3, SQRT_nm_prod
! ----------------------------------------------------------------------

	  
! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------
      ALLOCATE (dPnm_norm(nmax+1,nmax+1), STAT = AllocateStatus)
!      print *,"dPnm_norm AllocateStatus=", AllocateStatus
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE legendre_drv1.f"
         PRINT *, "Error: Allocatable Array: Pnm_norm, Nmax =", nmax
!         STOP "*** Not enough memory ***"
      END IF
! ----------------------------------------------------------------------
dPnm_norm = 0.0D0

! ----------------------------------------------------------------------
! Numerical Constants
      pi = PI_global
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Normalized associated Legendre functions
      CALL legendre (phi, nmax, Pnm_norm)
! ----------------------------------------------------------------------

      theta = pi/2D0 - phi
      c = cos(theta)
      s = sin(theta)

! Initial values
      dPoo = 0D0
      dP10 = - sqrt(3D0) * s
      dP11 = sqrt(3D0) * c

	  
! ----------------------------------------------------------------------
! Derivatives for order m=0 and variable degree n
! ----------------------------------------------------------------------
      DO n = 0 , nmax
          IF (n==0) THEN
              dPnm_norm(n+1,0+1) = dPoo
          ELSE IF (n==1) THEN
              dPnm_norm(n+1,0+1) = dP10
          ELSE 
              dP_f1 = sqrt(2D0*n-1) *                                   &
     		  ( c * dPnm_norm(n-1+1,0+1) - s * Pnm_norm(n-1+1,0+1) )

              dPnm_norm(n+1,0+1) = (sqrt(2D0*n+1) / n) *                &
     		  (dP_f1 - ((n-1) / sqrt(2D0*n-3)) * dPnm_norm(n-2+1,0+1))
          END IF
      END DO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Derivatives for equal degree n and order m    
! ----------------------------------------------------------------------
      DO n = 1 , nmax
          IF (n == 1) THEN
              dPnm_norm(n+1,n+1) = dP11
          ELSE
              dPnm_norm(n+1,n+1) = ( sqrt(2D0*n+1) / sqrt(2D0*n) ) *    &
     		  (s * dPnm_norm(n-1+1,n-1+1) + c * Pnm_norm(n-1+1,n-1+1))
          END IF
      END DO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Derivatives for variable degree n and order m
! ----------------------------------------------------------------------
      DO n = 2 , nmax
          DO m = 1 , n
! ----------------------------------------------------------------------
			  ! Case : m = n - 1
              IF ( n == (m+1) ) THEN
                  dP_f2 = c * dPnm_norm(n-1+1,m+1) - s * Pnm_norm(n-1+1,m+1)
                  dPnm_norm(n+1,m+1) = sqrt(1D0*(2*n+1)) * dP_f2
! ----------------------------------------------------------------------
			  ! Case : n > m + 1
              ELSE IF ( n > (m+1) ) THEN
                  dP_f2 = c * dPnm_norm(n-1+1,m+1) - s * Pnm_norm(n-1+1,m+1)
                  dP_f3 = sqrt( (1D0*(n-1+m)*(n-1-m)) / (1D0*(2*n-3)) ) * dPnm_norm(n-2+1,m+1)
                  SQRT_nm_prod = sqrt( (n+m)*(n-m) *1D0 )			  
                  dPnm_norm(n+1,m+1) = ( sqrt(2D0*n+1) / SQRT_nm_prod ) * ( sqrt(2D0*n-1) * dP_f2 - dP_f3 )
              END IF
! ----------------------------------------------------------------------
          END DO
      END DO
! ----------------------------------------------------------------------

END SUBROUTINE



END Module
