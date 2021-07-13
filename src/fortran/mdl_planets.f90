MODULE mdl_planets

! ---------------------------------------------------------------------------
! Purpose:
!  Module used in the modified version of the JPL source code for the
!  DE (Development Ephemeris) data processing:
! ---------------------------------------------------------------------------
!  Variables:
!  TTL_glb,CNAM__glb,SS_glb,NCON_glb,AU_glb,EMRAT_glb, IPT_glb,NUMDE_glb,
!  LPT_glb,RPT_glb,TPT_glb
!  CVAL_glb, DB_glb
! ----------------------------------------------------------------------
! Allocatable arrays: CVAL_2, DB_array
! Related subroutine: asc2eph.f90 (reading DE data format)
! ---------------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia               October 2015
! ---------------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			
	  
! ---------------------------------------------------------------------------
      CHARACTER (LEN=50) :: TTL_2(14,3) 
      CHARACTER (LEN=50) :: CNAM_2(1000)                                    
      REAL (KIND = prec_q) :: SS_2(3)
      REAL (KIND = prec_q) :: AU_2, EMRAT_2
      INTEGER (KIND = prec_int2) :: NCON_2, NUMDE_2
      INTEGER (KIND = prec_int2) :: IPT_2(3,12)
      INTEGER (KIND = prec_int2) :: LPT_2(3), RPT_2(3), TPT_2(3)
! ---------------------------------------------------------------------------
! Gravity Constants of Sun, Moon and Planets
      REAL (KIND = prec_q) :: GMconst_au(11), GMconst(11)


! Allocatable Arrays	  
! ---------------------------------------------------------------------------
! Chebyshev coefficients
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: CVAL_2 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: DB_array 
! ---------------------------------------------------------------------------


END
