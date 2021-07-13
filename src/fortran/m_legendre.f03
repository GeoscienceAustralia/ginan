MODULE m_legendre


! ----------------------------------------------------------------------
! MODULE: m_legendre.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the legendre subroutine 
! 
! Subroutines contained within the module:
! - legendre
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	16 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE legendre (phi, nmax, Pnm_norm)


! ---------------------------------------------------------------------------
! Subroutine:  legendre
! ---------------------------------------------------------------------------
! Purpose:
!  Computation of the Normalized Associated Legendre functions
!  Evaluation of Legendre functions is based on recurrence relations
! ---------------------------------------------------------------------------
! Input arguments:
! - phi:				Spherical latitude (radians)
! - nmax:				Maximum degree expansion
! 
! Output declaration to module:
! - mdl_legendre.f90:	Module used for the definition of the output arguments
!						as dynamic allocatable arrays
! Output arguments:
! - Pnm_norm:			Normalized Associated Legendre functions up to degree n
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
!     USE of Allocatable Arrays
!      USE mdl_legendre
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int8), INTENT(IN) :: nmax 
      REAL (KIND = prec_q), INTENT(IN) :: phi 
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: AllocateStatus
      INTEGER (KIND = prec_int8) :: n, m	  
      REAL (KIND = prec_q) :: pi
      REAL (KIND = prec_q) :: theta, c, s
      REAL (KIND = prec_q) :: Poo_norm, P10_norm, P11_norm, P_f1, P_f2, P_f3
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------
      ALLOCATE (Pnm_norm(nmax+1,nmax+1), STAT = AllocateStatus)
!      print *,"Pnm_norm AllocateStatus=", AllocateStatus
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE legendre.f"
         PRINT *, "Error: Allocatable Array: Pnm_norm, Nmax =", nmax
!         STOP "*** Not enough memory ***"
      END IF  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Set Pnm to zero matrice as initialization
      DO n = 0 , nmax
      	DO m = 0 , nmax 
      		Pnm_norm (n+1 , m+1) = 0D0
      	END DO
      END DO	  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Numerical Constants
      pi = PI_global
! ----------------------------------------------------------------------

	  
! %u = sin(phi);
      theta = pi/2D0 - phi
      c = cos(theta)
      s = sin(theta)

      Poo_norm = 1D0
      P10_norm = sqrt(3D0) * c
      P11_norm = sqrt(3D0) * s

	  
! -------------------------------------------------------------------------
! Normalized Associated Legendre functions (Pnm_Norm) for order m=0 and
! variable degree n
! -------------------------------------------------------------------------
      DO n = 0 , nmax
          if (n==0) THEN
              Pnm_norm(n+1,0+1) = Poo_norm
          else if (n==1) THEN
              Pnm_norm(n+1,0+1) = P10_norm
          else 
              P_f1 = sqrt(1D0*(2*n-1)) * c * Pnm_norm(n-1+1,0+1)
              Pnm_norm(n+1,0+1) = (sqrt(1D0*(2*n+1)) / n) *                 &
     		  (P_f1 - ((n-1) / sqrt(1D0*(2*n-3)) ) * Pnm_norm(n-2+1,0+1))
          end IF
      end DO
! -------------------------------------------------------------------------


! -------------------------------------------------------------------------
! Normalized functions for equal degree n and order m    
! -------------------------------------------------------------------------
      DO n = 1 , nmax
          if (n == 1) THEN
              Pnm_norm(n+1,n+1) = P11_norm
          else   
              Pnm_norm(n+1,n+1) = ( sqrt(1D0*(2*n+1)) / sqrt(2D0*n) )       &
     		  * s * Pnm_norm(n-1+1,n-1+1)
          end IF
      end DO
! -------------------------------------------------------------------------


! -------------------------------------------------------------------------
! Normalized functions for variable degree n and order m
! -------------------------------------------------------------------------
      DO n = 2 , nmax
          DO m = 1 , n
! -------------------------------------------------------------------------
			  ! Case:  m = n - 1
              if (n == (m+1)) THEN 
                      Pnm_norm(n+1,m+1) = sqrt(1D0*(2*n+1)) * c * Pnm_norm(n-1+1,m+1)
! -------------------------------------------------------------------------
			  ! Case:  n > m+1
              else if (n > (m+1)) THEN
				  ! General case : n, m
                  P_f2 = sqrt( 1D0*(2*n-1) ) * c * Pnm_norm(n-1+1,m+1)
                  P_f3 = sqrt( (1D0* (n-1+m)*(n-1-m) ) / (1D0*(2*n-3) ) ) * Pnm_norm(n-2+1,m+1)
                  !Pnm_norm(n+1,m+1) = ( sqrt(1D0*(2*n+1)) / sqrt(1D0* (n+m)*(n-m)) ) * (P_f2 - P_f3)
                  Pnm_norm(n+1,m+1) = sqrt( (1D0*(2*n+1)) / (1D0*(n+m)*(n-m)) ) * (P_f2 - P_f3)
              end IF
! -------------------------------------------------------------------------
!      		  Print *, "Pnm ij=", Pnm_norm(n+1,m+1)
          end DO
      end DO
! -------------------------------------------------------------------------
  
END Subroutine



END Module
