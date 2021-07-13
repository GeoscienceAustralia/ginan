MODULE m_legendre2


! ----------------------------------------------------------------------
! MODULE: m_legendre2.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the legendre_drv2 subroutine 
! 
! Subroutines contained within the module:
! - legendre_drv2
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	18 January 2018
! ----------------------------------------------------------------------

      IMPLICIT NONE
      !SAVE 			
  
	  
Contains

SUBROUTINE legendre2 (phi, nmax, d2Pnm_norm)


! ---------------------------------------------------------------------------
! Subroutine:  legendre2
! ---------------------------------------------------------------------------
! Purpose:
! Computation of the second order derivatives of the Normalized Associated
! Legendre functions
! 
! Evaluation of Legendre functions is based on Recurrence relations
! ---------------------------------------------------------------------------
! Input arguments:
! - phi:			Spherical latitude (radians)
! - nmax:			Maximum degree expansion
! 
! Output arguments:
! - d2Pnm_norm: 	Second order derivatives of Normalized Associated Legendre
!   				functions up to degree n
! ---------------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	18 January 2018
! ----------------------------------------------------------------------


! Modules used
      USE mdl_precision
      USE mdl_num
      USE m_legendre
      USE m_legendre1
	  	  
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int8), INTENT(IN) :: nmax
      REAL (KIND = prec_q), INTENT(IN) :: phi 
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: d2Pnm_norm
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Pnm_norm, dPnm_norm
      INTEGER (KIND = prec_int8) :: n, m	  
      INTEGER (KIND = prec_int2) :: AllocateStatus
      REAL (KIND = prec_q) :: pi
      REAL (KIND = prec_q) :: theta, c, s
      REAL (KIND = prec_q) :: dPoo, dP10,dP11, dP_f1,dP_f2,dP_f3, SQRT_nm_prod
      REAL (KIND = prec_q) :: d2Poo, d2P10, d2P11, d2P_f1, d2P_sum, d2P_f2, d2P_f3
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Allocatable arrays
! ----------------------------------------------------------------------
      ALLOCATE (d2Pnm_norm(nmax+1,nmax+1), STAT = AllocateStatus)
!      print *,"dPnm_norm AllocateStatus=", AllocateStatus
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE legendre_drv1.f"
         PRINT *, "Error: Allocatable Array: Pnm_norm, Nmax =", nmax
!         STOP "*** Not enough memory ***"
      END IF
! ----------------------------------------------------------------------
d2Pnm_norm = 0.0D0

! Numerical Constants
pi = PI_global

! Normalized associated Legendre functions
CALL legendre (phi, nmax, Pnm_norm)

! First-order derivatives of the normalized associated Legendre functions
CALL legendre_drv1 (phi, nmax, dPnm_norm)


theta = pi/2D0 - phi
c = cos(theta)
s = sin(theta)


! Initial values
dPoo  = 0.D0
d2Poo = 0.D0

dP10  = - sqrt(3.D0) * s
d2P10 = - sqrt(3.D0) * c 

dP11  =  sqrt(3.D0) * c
d2P11 = -sqrt(3.D0) * s


!%-------------------------------------------------------------------------
!% Second order Derivatives for order m=0 and variable degree n
Do n = 0 , nmax
    If (n==0) Then
        d2Pnm_norm(n+1,0+1) = d2Poo
    else if (n==1) Then        
        d2Pnm_norm(n+1,0+1) = d2P10
    else      
        d2P_f1 = sqrt(2.D0*n-1.D0) * ( c * d2Pnm_norm(n-1+1,0+1) - 2.D0 * s * dPnm_norm(n-1+1,0+1) - c * Pnm_norm(n-1+1,0+1) )
        d2Pnm_norm(n+1,0+1) = (sqrt(2.D0*n+1.D0) / n) * ( d2P_f1 - ( (n-1) / sqrt(2.D0*n-3.D0) ) * d2Pnm_norm(n-2+1,0+1) )
    end If
end Do


!% Second order Derivatives for equal degree n and order m    
Do n = 1 , nmax
    if (n == 1) Then
        d2Pnm_norm(n+1,n+1) = d2P11      !%7777777777777777777777777777777777777777777777777777777777777777777
    else        
        !% ATTENTION!  correction: sqrt(2*n+1) / sqrt(2*n)      %7777777777777777777777777777777777777777777777777777777777777777777
        d2P_sum = s * d2Pnm_norm(n-1+1,n-1+1) + 2.D0 * c * dPnm_norm(n-1+1,n-1+1) - s * Pnm_norm(n-1+1,n-1+1)
        d2Pnm_norm(n+1,n+1) = ( sqrt(2.D0*n+1.D0) / sqrt(2.D0*n) ) * d2P_sum 
    end If
end Do


!% Second order erivatives for variable degree n and order m
Do n = 1 , nmax
    Do m = 1 , n
        if (n >= (m+1) ) Then
            d2P_f2 = c * d2Pnm_norm(n-1+1,m+1) - 2.D0 * s * dPnm_norm(n-1+1,m+1) - c * Pnm_norm(n-1+1,m+1) 
            d2P_f3 = sqrt(  ( (n-1+m)*(n-1-m) ) / (2.D0*n-3.D0)  ) * d2Pnm_norm(n-2+1,m+1) 
  !% ATTENTION! correction: sqrt(2*n+1) / sqrt( (n+m)*(n-m) )       %7777777777777777777777777777777777777777777777777777777777777777777         
            d2Pnm_norm(n+1,m+1) = ( sqrt(2.D0*n+1) / sqrt(1.D0*(n+m)*(n-m)) ) * ( sqrt(2.D0*n-1) * d2P_f2 - d2P_f3 )
        end If
    end Do
end Do


END Subroutine


END MODULE
