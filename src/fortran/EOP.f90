SUBROUTINE EOP (mjd, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)


! ----------------------------------------------------------------------
! Subroutine:  EOP.f90
! ----------------------------------------------------------------------
! Purpose:
!  IERS Earth Orientation Parameters (EOP) data reading and processing.
!  Corrections computing due to tidal variations
!  Computation of the ICRF-ITRF transformation matrix (direct/inverse) 
!  and its derivatives.
!  Earth Orientation Matrix computation is based on the EOP data corrected 
!  due to tidal variations.
! ---------------------------------------------------------------------------
! Input arguments:
! - mjd:                Modified Julian Day number of the epoch (in TT scale)
!
! Output arguments:
! - EOP_cr:       Array of the EOP data after applying the tidal corrections
!   EOP_cr = [MJD xp_cor yp_cor UT1-UTC_cor LOD_cor dX dY]  1x6 matrix
!   MJD:                MJD of the input epoch (in TT)
!   xp_cor,yp_cor:      Polar motion coordinates (arcsec) at input epoch considering 
!                             corrections due to tidal variations (ocean tidal and libration effects)       
!   UT1UTC_cor:         UT1-UTC difference (sec) at input epoch considering corrections
!                             due to tidal variations (ocean tidal and libration effects)  
!   LOD:                Length of Day (LOD)
!   dX, dY:             Corrections to the Precession-Nutation model (arcsec)
! - CRS2TRS:            GCRS to ITRS transformation matrix
! - TRS2CRS:            ITRS to GCRS transformation matrix 
! - d_CRS2TRS:          Derivative of GCRS to ITRS transformation matrix
! - d_TRS2CRS:          Derivative of ITRS to GCRS transformation matrix
! ----------------------------------------------------------------------
! Note 1:
!  Values of the EOP_sol input argument:
!  1. EOP_sol = 1 : refers to EOP data format of the C04 solution
!                     provided by the IERS Earth Orientation Center 
!  2. EOP_sol = 2 : refers to EOP data format of the daily solution (finals2000A.daily)
!                     provided by the IERS Rapid Service/Prediction Center
!  3. EOP_sol = 3 : refers to EOP data format of the ultra-rapid products
!                     provided by the IGS (per every 6 hours)
! ---------------------------------------------------------------------------
! ---------------------------------------------------------------------------
! Dr. Thomas Papanikolaou
! Cooperative Research Centre for Spatial Information, Australia
! 12 July 2017
! ---------------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      USE mdl_eop
      USE m_eop_cor
      USE m_eop_igu
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd

! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: EOP_cr(EOP_MAX_ARRAY)
      REAL (KIND = prec_d), INTENT(OUT) :: CRS2TRS(3,3), TRS2CRS(3,3)
      REAL (KIND = prec_d), INTENT(OUT) :: d_CRS2TRS(3,3), d_TRS2CRS(3,3)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER i, arc_length
      REAL (KIND = prec_d) :: mjd_TT
! ----------------------------------------------------------------------




mjd_TT = mjd
EOP_cr = 0.d0


! ----------------------------------------------------------------------
! EOP data
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Case 1. EOP by IERS data: EOP reading, interpolation and corrections
! ----------------------------------------------------------------------
! - IERS Earth Orientation Center:              C04 solution
! - IERS Rapid Service/Prediction Center: finals2000A.daily solution
! ----------------------------------------------------------------------
      IF (EOP_sol == EOP_C04T .OR. EOP_sol == EOP_FAST) THEN  
            CALL eop_cor (mjd_TT, EOP_day_glb, EOP_sol, EOP_Nint, EOP_cr)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Case 2. ERP by IGS ultra-rapid data (ERP reading and interpolation) +
!             dX,dY w.r.t. IAU2000A by IERS RS/PC (finals2000A.daily) 
! ----------------------------------------------------------------------
      ELSEIF (EOP_sol == EOP_SUPER_FAST)  THEN 
            arc_length = yml_orbit_arc_determination + yml_orbit_arc_prediction +&
                    yml_orbit_arc_backwards
            CALL eop_igu (mjd_TT, ERP_fname, ERP_day_glb, EOP_day_glb, EOP_Nint, EOP_cr)
      END IF
! ----------------------------------------------------------------------
      ! debug
      if (.false.) then
              print *, EOP_cr
      end if
! ----------------------------------------------------------------------
!SCM 20190606 Removed the nutation model correction terms from EOP array,
!             These terms were adding noise to the orbit fits to IGS products.
!             Add flag to turn on/off in the future!
      EOP_cr(EOP_DX) = 0.D0
      EOP_cr(EOP_DY) = 0.D0
! ---------------------------------------------------------------------- 
 
! ----------------------------------------------------------------------
! Tranformation: ITRF to ICRF
! ----------------------------------------------------------------------
! ICRF-ITRF transformation matrix (including derivatives)
      CALL crs_trs (mjd_TT, EOP_cr, iau_model, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)
! ----------------------------------------------------------------------
        
      ! just some debug
      if (.false.) then
      print *, "mjd=", mjd_TT

      print *, "ECI-ECEF matrix"
      do i = 1, 3
          print *, crs2trs(i, 1), crs2trs (i, 2), crs2trs (i,3)
      end do  
        
      print *, "derivative of ECI-ECEF matrix"
      do i = 1, 3
          print *, d_crs2trs(i, 1), d_crs2trs(i, 2), d_crs2trs(i, 3)
      end do  
      end if

END
