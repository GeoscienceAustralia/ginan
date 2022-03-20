MODULE m_veqC2T


! ----------------------------------------------------------------------
! MODULE: m_veqC2T.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the subroutine 'veqC2T' 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia & Frontier-SI
! Created:	5 August 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			


Contains


SUBROUTINE veqC2T (mjd, sec00, veqZ_C, veqP_C, veqZ_T, veqP_T)


! ----------------------------------------------------------------------
! SUBROUTINE: veqC2T
! ----------------------------------------------------------------------
! Purpose:
!  Variational Equations solution (matrices per epoch) transformation 
!  from inertial (ICRF) to terrestrial (ITRF) reference frame 
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:		Modified Julian Day number (including the fraction of the day) in Terrestrial Time (TT)
! - sec00:		Seconds since the start of the day 00h 
! - veqZ_C:		State transition matrix (6x6) at the input epoch in inertial reference frame (ICRF)
! - veqP_C:		Sensitivity matrix (6 x Np) at the input epoch in inertial reference frame (ICRF)
!
! Output arguments:
! - veqZ_T:		State transition matrix (6x6) at the input epoch in terrestrial inertial reference frame (ITRF)
! - veqP_T:		Sensitivity matrix (6 x Np) at the input epoch in terrestrial reference frame (ITRF)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia & Frontier-SI
! Created:	5 August 2019
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_eop
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d) :: mjd, sec00  
      REAL (KIND = prec_d), DIMENSION(6,6), INTENT(IN) :: veqZ_C  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(IN) :: veqP_C 
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(6,6), INTENT(OUT) :: veqZ_T  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: veqP_T
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: veqZ_r_crs(3,6), veqZ_v_crs(3,6)
      REAL (KIND = prec_d) :: veqZ_r_trs(3,6), veqZ_v_trs(3,6)

      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: veqP_r_crs, veqP_v_crs
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: veqP_r_trs, veqP_v_trs
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
      INTEGER (KIND = prec_int8) :: Nparam, N1
	  
	  REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      DOUBLE PRECISION EOP_cr(7)
      DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)
! ----------------------------------------------------------------------

mjd_TT = mjd 
 
! ----------------------------------------------------------------------
! ICRF to ITRF Transformation matrices (including derivatives) based on EOP data
CALL EOP (mjd_TT, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! VEQ matrices in celestial reference system (ICRS)
! ----------------------------------------------------------------------
veqZ_r_crs = veqZ_C(1:3,:)
veqZ_v_crs = veqZ_C(4:6,:)

sz1 = size(veqP_C, DIM = 1)
sz2 = size(veqP_C, DIM = 2)
N1 = 3
Nparam = sz2
! Dynamic allocatable arrays
ALLOCATE (veqP_r_crs(3,Nparam), STAT = AllocateStatus)
ALLOCATE (veqP_v_crs(3,Nparam), STAT = AllocateStatus)
veqP_r_crs = veqP_C(1:3,:)
veqP_v_crs = veqP_C(4:6,:)

ALLOCATE (veqP_r_trs(3,Nparam), STAT = AllocateStatus)
ALLOCATE (veqP_v_trs(3,Nparam), STAT = AllocateStatus)
ALLOCATE (veqP_T(6,Nparam), STAT = AllocateStatus)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Transformation of state transition matrix
! ----------------------------------------------------------------------
!veqZ_trs = [veqZ_r_trs; veqZ_v_trs]
!veqZ_r_trs = CRS2TRS * veqZ_r
!veqZ_v_trs = CRS2TRS * veqZ_v + d_CRS2TRS * veqZ_r

veqZ_r_trs = MATMUL(CRS2TRS, veqZ_r_crs) 
veqZ_v_trs = MATMUL(CRS2TRS, veqZ_v_crs) + MATMUL(d_CRS2TRS, veqZ_r_crs) 

veqZ_T(1:3,:) = veqZ_r_trs
veqZ_T(4:6,:) = veqZ_v_trs
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Transformation of sensitivity matrix
! ----------------------------------------------------------------------
!veqP_trs = [veqP_r_trs; veqP_v_trs]
!veqP_r_trs = CRS2TRS * veqP_r
!veqP_v_trs = CRS2TRS * veqP_v + d_CRS2TRS * veqP_r
 
veqP_r_trs = MATMUL(CRS2TRS, veqP_r_crs)
veqP_v_trs = MATMUL(CRS2TRS, veqP_v_crs) + MATMUL(d_CRS2TRS, veqP_r_crs) 

veqP_T(1:3,:) = veqP_r_trs
veqP_T(4:6,:) = veqP_v_trs
! ----------------------------------------------------------------------
 

END SUBROUTINE


END MODULE
