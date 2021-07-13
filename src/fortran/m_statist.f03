MODULE m_statist


! ----------------------------------------------------------------------
! MODULE: m_statist.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for computing of representative statistical quantities of a data series 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	13 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			

	  
Contains



SUBROUTINE statist (dx, RMSdx, Sigmadx, MEANdx, MINdx, MAXdx)


! ----------------------------------------------------------------------
! SUBROUTINE: statist.f03
! ----------------------------------------------------------------------
! Purpose:
!  Compute the statistical quantities (RMS, Min, Max) of a series matrix of numerical differences 
! ----------------------------------------------------------------------
! Input arguments:
! - dx:			Matrix with the x series differences (dynamic allocatable array) 
!
! Output arguments:
! - RMSdx: 		Root Mean Square of dx 
! - Sigmadx: 	Standard Deviation of dx
! - MEANdx: 	Mean value of dx
! - MINdx: 		Minimum value of dx
! - MAXdx: 		Maximum value of dx
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	13 October 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), ALLOCATABLE, DIMENSION(:) :: dx
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: RMSdx, Sigmadx, MEANdx, MINdx, MAXdx  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: sum_dx, sum_dx2, sum_dxx 
      INTEGER (KIND = prec_int8) :: sz1, sz2, size_dx, i 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------	


sz1 = size(dx, DIM = 1)
!sz2 = size(dx, DIM = 2)


size_dx = sz1
sum_dx = 0.0D0
sum_dx2 = 0.0D0
MINdx = dx(1)
MAXdx = dx(1)
Do i = 1 , size_dx
   sum_dx2 = sum_dx2 + dx(i)**2
   sum_dx = sum_dx + dx(i)
   If (dx(i) < MINdx) Then
      MINdx = dx(i)
   End IF
   If (dx(i) > MAXdx) Then
      MAXdx = dx(i)
   End IF
   
End Do


! RMS
RMSdx = sqrt(sum_dx2 / size_dx)

! Mean
MEANdx = sum_dx / size_dx

! Standard Deviation
sum_dxx = 0.0D0
DO i = 1 , size_dx	  
   sum_dxx = sum_dxx + (dx(i) - MEANdx)**2
END DO
Sigmadx = sqrt(sum_dxx / size_dx)	  
Sigmadx = sqrt(sum_dxx / (size_dx - 1) )	  



END SUBROUTINE




End Module

