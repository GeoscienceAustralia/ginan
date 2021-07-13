SUBROUTINE eop_cor2 (mjd , delta_EOP )


! ----------------------------------------------------------------------
! Subroutine:  eop_cor2.f90
! ----------------------------------------------------------------------
! Purpose:
!  Earth Orientation Parameters (EOP) corrections due to tidal variations:
!  - diurnal and semi-diurnal variations in EOP (polar motion, UT1) due to ocean tides
!  - Sub-diurnal libration effects in UT1, LOD and polar motion
!  - Zonal tides effects
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number of the epoch (in TT)
!
! Output arguments:
! - delta_EOP:		Array of the tidal variations in the Earth Rotation (see Note 1)
! ----------------------------------------------------------------------
! Note1:
! delta_EOP array elements: 
! - dxp_ot:			diurnal and semi-diurnal variations in polar motion (xp) due to ocean tides (arcsec) 
! - dyp_ot:			diurnal and semi-diurnal variations in polar motion (yp) due to ocean tides (arcsec)
! - dUT1_ot:        diurnal and semi-diurnal variations in UT1 due to ocean tides (sec)
! - dUT1_libr:		Sub-diurnal libration effects on UT1 (sec) 
! - dLOD_libr:		Sub-diurnal libration effects on LOD (sec/day)
! - dxp_libr:		Sub-diurnal libration effects on polar motion (arcsec)
! - dyp_libr:		Sub-diurnal libration effects on polar motion (arcsec)
! - DUT_zont2		Zonal tides effect on UT1 (sec)
! - DLOD_zont2		Zonal tides effect on LOD (sec)
! - DOMEGA_zont2	Zonal tides effect on Omega (radians/sec)	  
! ----------------------------------------------------------------------
! IERS called subroutines for the tidal variations (see IERS Conv.2010 Ch.8)
! - ORTHO_EOP.f:	Diurnal and semi-diurnal variations in polar motion and UT1 due to ocean tides
! - UTLIBR.f:		Sub-diurnal libration effects in UT1 and LOD	
! - PMSDNUT2.f:		Sub-diurnal libration effects in polar motion	
! - RG_ZONT2.f:		Tidal deformation (zonal tides) corrections
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia               March 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: delta_EOP (10)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
      DOUBLE PRECISION TT1, TT2, TT1_UT1, TT2_UT1, TAI1, TAI2
      INTEGER (KIND = prec_int8) :: mjd_UTC_day
      REAL (KIND = prec_d) :: d_otides(3), PM_libr(2)
      DOUBLE PRECISION dxp_ot, dyp_ot, dUT1_ot, dUT1_libr, dLOD_libr, dxp_libr, dyp_libr
      DOUBLE PRECISION JD_TT, TT_cent, DUT_zont2, DLOD_zont2, DOMEGA_zont2
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Time Systems transformation											 
! ----------------------------------------------------------------------
      CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
! ----------------------------------------------------------------------
! TT
      TT1 = 2400000.5D0
      TT2 = mjd_TT
! ----------------------------------------------------------------------
! TAI
      TAI1 = 2400000.5D0
      TAI2 = mjd_TAI
! ----------------------------------------------------------------------
! UTC
      mjd_UTC_day = INT (mjd_UTC)
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Diurnal and semi-diurnal variations in polar motion and UT1 due to ocean tides.
! - Subroutine ORTHO_EOP.f
      CALL ORTHO_EOP ( mjd_UTC, d_otides )
      dxp_ot  = d_otides (1)  ! delta_x, in microarcseconds
      dyp_ot  = d_otides (2)  ! delta_y, in microarcseconds
      dUT1_ot = d_otides (3)  ! delta_UT1, in microseconds 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Sub-diurnal libration effects in UT1 and LOD
! - UTLIBR.f  :  DUT1 (microseconds), DLOD (microseconds/day)
      CALL UTLIBR (mjd_UTC, dUT1_libr, dLOD_libr)    
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Sub-diurnal libration effects in polar motion
! - PMSDNUT2.f  :  (dxp,dyp) in microarcseconds
      CALL PMSDNUT2 (mjd_UTC, PM_libr)
      dxp_libr = PM_libr (1)
      dyp_libr = PM_libr (2)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Tidal deformation (zonal tides) corrections
! ----------------------------------------------------------------------
      JD_TT = mjd_TT + 2400000.5D0
      TT_cent = (JD_TT - 2451545.0D0) / 36525D0
      CALL RG_ZONT2 ( TT_cent, DUT_zont2, DLOD_zont2, DOMEGA_zont2 )
! ----------------------------------------------------------------------
      !PRINT *,"-------------------- Zonal tides ----------------------"
      !PRINT *,"DUT_zont2"   , DUT_zont2
      !PRINT *,"DLOD_zont2"  , DLOD_zont2
      !PRINT *,"DOMEGA_zont2", DOMEGA_zont2
      !PRINT *,"------------------------------------------------------"
      !PRINT *," "


! ----------------------------------------------------------------------
! - dxp_ot:			diurnal and semi-diurnal variations in polar motion (xp) due to ocean tides (arcsec) 
! - dyp_ot:			diurnal and semi-diurnal variations in polar motion (yp) due to ocean tides (arcsec)
! - dUT1_ot:        diurnal and semi-diurnal variations in UT1 due to ocean tides (sec)
! - dUT1_libr:		Sub-diurnal libration effects on UT1 (sec) 
! - dLOD_libr:		Sub-diurnal libration effects on LOD (sec/day)
! - dxp_libr:		Sub-diurnal libration effects on polar motion (arcsec)
! - dyp_libr:		Sub-diurnal libration effects on polar motion (arcsec)
! - DUT_zont2		Zonal tides effect on UT1 (sec)
! - DLOD_zont2		Zonal tides effect on LOD (sec)
! - DOMEGA_zont2	Zonal tides effect on rotation velocity (radians/sec)	  
      delta_EOP (1) = dxp_ot * 1.0D-6
      delta_EOP (2) = dyp_ot * 1.0D-6
      delta_EOP (3) = dUT1_ot * 1.0D-6
      delta_EOP (4) = dUT1_libr * 1.0D-6
      delta_EOP (5) = dLOD_libr * 1.0D-6
      delta_EOP (6) = dxp_libr * 1.0D-6
      delta_EOP (7) = dyp_libr * 1.0D-6
      delta_EOP (8) = DUT_zont2
      delta_EOP (9) = DLOD_zont2
      delta_EOP (10) = DOMEGA_zont2
! ----------------------------------------------------------------------


END
