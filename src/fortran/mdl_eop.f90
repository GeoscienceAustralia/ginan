MODULE mdl_eop

! ---------------------------------------------------------------------------
! Purpose:
!  Module used in the subroutines library for the Earth Orientation modelling
! ---------------------------------------------------------------------------
! Dr. Thomas Papanikolaou
! Cooperative Research Centre for Spatial Information, Australia
! December 2015
! ---------------------------------------------------------------------------
! Last modified:
! - 12/07/2017: 	Dr. Thomas Papanikolaou
!   				Cooperative Research Centre for Spatial Information, Australia
!
!   The following global variables have been added:
!   EOP_fname:	Earth Orientation Parameters (EOP) data file name
!   ERP_fname:	Earth Rotation Parameters (ERP) data file name
!   EOP_sol:	EOP data solution type
!   EOP_Nint:	Number of data points used in Lagrange interpolation of EOP data
!   iau_model:	IAU Precession-Nutation model  
!
! - 19/06/2018: 	Dr. Thomas Papanikolaou
!   				Geoscience Australia, CRC-SI
!   EOP_day_glb global variable has been added
! ---------------------------------------------------------------------------

      USE mdl_precision
      IMPLICIT NONE
      SAVE 			

! Allocatable Arrays	  
! ---------------------------------------------------------------------------
! EOP arrays used for EOP data interpolation 
      REAL (KIND = prec_d), DIMENSION(:), ALLOCATABLE :: MJDint_ar, xint_ar, yint_ar, UT1int_ar		
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: EOPint_ar
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Global variables
      CHARACTER (LEN=512) :: EOP_fname
      CHARACTER (LEN=512) :: ERP_fname
      INTEGER (KIND = prec_int4) :: EOP_Nint
      INTEGER (KIND = prec_int1) :: EOP_sol
      INTEGER (KIND = prec_int2) :: iau_model
      !REAL (KIND = prec_d) :: EOP_day_glb(7)	
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: EOP_day_glb
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: ERP_day_glb 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: ERP_day_IC
      REAL (KIND = prec_d) :: EOP_MJD0_glb	  
! ---------------------------------------------------------------------------


END
