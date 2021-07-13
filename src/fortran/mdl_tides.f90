MODULE mdl_tides

! ---------------------------------------------------------------------------
! Purpose:
!  Module used in the subroutines for computing the Tidal effects to orbit
! ---------------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia              November 2015
! ---------------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 30 January 2019:
!   The Doodson arguments multipliers have now been applied and stored 
!   as global dynamic allocatable array through this module.
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			
	  

! Allocatable Arrays	  
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
! Allocated in "tides_ocean.f90"		
!      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCnm_ocean, dSnm_ocean		
! ---------------------------------------------------------------------------
! Allocated in "tides_fes2004.f90" 
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: DelaunayNf
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: Delaunay_FES, Doodson_mult_glb
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: dCnm_p,dSnm_p, dCnm_m,dSnm_m
! ---------------------------------------------------------------------------
! Allocated in "lib4_tides.f90"		
!      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: dCnm_tides, dSnm_tides		
! ---------------------------------------------------------------------------


END
