SUBROUTINE crs_trs (mjd, EOP_ar, iau_model, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Subroutine:  crs_trs.f90
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Purpose:
!  ICRF-ITRF transformation matrix (direct/inverse) for position/velocity vectors
!  Earth Orientation Matrix based on the EOP data corrected due to tidal variations
!
! Remark:
!  The matrices required for the ICRF-ITRF direct and inverse transformation
!  are computed for the position and velocity vectors individually.
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Input arguments:
! - mjd:			Modified Julian Day number of the epoch (in TT scale)
! - iau_model:		Precession-Nutation model by International Astronomical Union (IAU)
!					IAU = 2 refers to the IAU 2000A model
!					IAU = 3 refers to the IAU 2006/2000A model
! - EOP_ar:			Array of the EOP values for the input epoch
!   				eop = [MJD xp yp UT1_UTC LOD dX dY]  1x6 matrix
!   				MJD:     MJD of the input epoch (in TT) 
!   				x,y:     Polar motion coordinates (arcsec) 
!   				UT1_UTC: Difference between UT1 and UTC (sec)
!   				LOD_cor: Length of Day (LOD)
!					dX,dY:   Corrections to Precession-Nutation model (arcsec)
!
! Output arguments:
! - CRS2TRS:		GCRS to ITRS transformation matrix
! - TRS2CRS:		ITRS to GCRS transformation matrix 
! - d_CRS2TRS:		Derivative of GCRS to ITRS transformation matrix
! - d_TRS2CRS:		Derivative of ITRS to GCRS transformation matrix
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Dr. Thomas Papanikolaou, Geoscience Australia            December 2015
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd, EOP_ar(7)
      INTEGER (KIND = prec_int2), INTENT(IN) :: iau_model
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: CRS2TRS(3,3), TRS2CRS(3,3)
      REAL (KIND = prec_d), INTENT(OUT) :: d_CRS2TRS(3,3), d_TRS2CRS(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Called Functions (F77)
      DOUBLE PRECISION iau_S06, iau_ERA00, iau_ANP, iau_SP00 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
      DOUBLE PRECISION TT1, TT2, TT1_UT1, TT2_UT1, TAI1, TAI2
      INTEGER (KIND = prec_int8) :: mjd_UTC_day

      DOUBLE PRECISION xp, yp, UT1_UTC, dX_eop, dY_eop
      DOUBLE PRECISION xp_rad, yp_rad, arcsec2rad, sp_iau00
      REAL (KIND = prec_d) :: X_iau00A,Y_iau00A,s_iau00A , X_iau06,Y_iau06,s_iau06
      REAL (KIND = prec_d) :: X_iau,Y_iau,s_iau , X_pn,Y_pn

      DOUBLE PRECISION RPOM(3,3), RPOM_T(3,3)
      DOUBLE PRECISION RC2I(3,3), RC2I_T(3,3)
      DOUBLE PRECISION R_era(3,3), era, R_era_n(3,3), era_n
      DOUBLE PRECISION Qt(3,3), Qt_inv(3,3), Rt(3,3), Rt_inv(3,3), Wt(3,3), Wt_inv(3,3)
      DOUBLE PRECISION GCRS2TIRS(3,3)
      DOUBLE PRECISION QR(3,3), Wi_Ri(3,3)
      DOUBLE PRECISION dtheta, P_dR (3,3), P_dR_T(3,3)
      DOUBLE PRECISION QP(3,3), QPR(3,3), QPRW(3,3), Ri_Qi(3,3), PT_Ri_Qi(3,3), Wi_PT_RiQi(3,3)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      arcsec2rad = PI_global / (3600.0D0 * 180.0D0)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! EOP values
      xp = EOP_ar(2)
      yp = EOP_ar(3)
      UT1_UTC = EOP_ar(4)
      dX_eop = EOP_ar(6)
      dY_eop = EOP_ar(7)
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
      mjd_UTC_day = IDINT(mjd_UTC)
! ----------------------------------------------------------------------
! UT1
      mjd_UT1 = mjd_UTC + UT1_UTC / 86400D0
      TT1_UT1 = 2400000.5D0
      TT2_UT1 = mjd_UT1  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! CIO based transformation
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Q(t)
! ----------------------------------------------------------------------
!-----------------------------------------------------------------------
! variable initialisation (X_iau, Y_iau, s_iau)
X_iau = 0.d0
Y_iau = 0.d0
s_iau = 0.d0

! ----------------------------------------------------------------------
! Precession-Nutation model:  X, Y (in radians)
! ----------------------------------------------------------------------
! 1. IAU 2000A
      if (iau_model == IAU2000) then
      CALL iau_XYS00A ( TT1, TT2, X_iau00A, Y_iau00A, s_iau00A )
      X_iau = X_iau00A
      Y_iau = Y_iau00A
      s_iau = s_iau00A
! ----------------------------------------------------------------------
! 2. IAU 2006/2000A
      else if (iau_model == IAU2006) then
      CALL iau_XY06 ( TT1, TT2, X_iau06, Y_iau06 )
! CIO locator s (in radians) | Function iau_S06
      s_iau06 = iau_S06 ( TT1, TT2, X_iau06, Y_iau06 )
!or   CALL XYS06A
      !PRINT *,"X,Y,s", X_iau06,Y_iau06,s_iau06
      X_iau = X_iau06
      Y_iau = Y_iau06
      s_iau = s_iau06
      end if 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! EOP: dX,dY (arcsec to radians)
      !dX_eop_rad =  dX_eop * arcsec2rad
      !dY_eop_rad =  dY_eop * arcsec2rad
      X_pn = X_iau + dX_eop * arcsec2rad
      Y_pn = Y_iau + dY_eop * arcsec2rad
! ----------------------------------------------------------------------
! Q(t)^-1
      CALL iau_C2IXYS ( X_pn, Y_pn, s_iau, RC2I )
      CALL iau_TR ( RC2I, RC2I_T )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! R(t)
! ----------------------------------------------------------------------
!- ERA00: era (in radians)
      era = iau_ERA00 ( TT1_UT1, TT2_UT1 )
! ----------------------------------------------------------------------
! Rotation Matrices initialization: set them to singular I3x3
! R3(ERA)
      R_era(1,1) = 1D0
      R_era(1,2) = 0D0
      R_era(1,3) = 0D0
      R_era(2,1) = 0D0
      R_era(2,2) = 1D0
      R_era(2,3) = 0D0
      R_era(3,1) = 0D0
      R_era(3,2) = 0D0
      R_era(3,3) = 1D0
! R3(-ERA)
      R_era_n = R_era
! ----------------------------------------------------------------------
      !CALL iau_RZ ( PSI, R )
! R(t)^T = R3(ERA)	  
      CALL iau_RZ ( era, R_era )
! R(t) = R3(-ERA)
      era_n = -1.0D0 * era
      CALL iau_RZ ( era_n, R_era_n )
! ----------------------------------------------------------------------
      !PRINT *,"ERA", era
      !PRINT *,"R_era", R_era
      !PRINT *,"R_era_n", R_era_n


! ----------------------------------------------------------------------
! W(t)
! ----------------------------------------------------------------------
! xp, yp arcsec to radians
      xp_rad = xp * arcsec2rad
      yp_rad = yp * arcsec2rad
! ----------------------------------------------------------------------
! s': TIO locator (radians)
      sp_iau00 = iau_SP00 ( TT1, TT2 )
! ----------------------------------------------------------------------
! W(t)^-1
      call iau_POM00 ( xp_rad, yp_rad, sp_iau00, RPOM )
! W(t)
      CALL iau_TR ( RPOM, RPOM_T )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth Orientation Matrix (EOM)
! ----------------------------------------------------------------------
! [GCRS] = Q(t) * R(t) * W(t) * [ITRS] = EOM * {ITRS}
! ----------------------------------------------------------------------
      Qt = RC2I_T
      Qt_inv = RC2I
! ----------------------------------------------------------------------
      Rt = R_era_n
      Rt_inv = R_era
! ----------------------------------------------------------------------
      Wt = RPOM_T
      Wt_inv = RPOM
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! GCRS to ITRS transformation matrix
! ----------------------------------------------------------------------
!SUBROUTINE iau_RXR ( A, B, ATB )
      CALL iau_RXR ( R_era, RC2I, GCRS2TIRS )
      CALL iau_RXR ( RPOM, GCRS2TIRS, CRS2TRS )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! ITRS to GCRS transformation matrix (Inverse transformation)
      CALL iau_TR ( CRS2TRS, TRS2CRS )
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Alternative approach
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! ITRS to GCRS transformation matrix
! ----------------------------------------------------------------------
! [GCRS] = Q(t) * R(t) * W(t) * [ITRS] = EOM * {ITRS}
! ----------------------------------------------------------------------
! TRS2CRS = Qt * Rt * Wt
      CALL iau_RXR ( Qt, Rt, QR )
      CALL iau_RXR ( QR, Wt, TRS2CRS )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GCRS to ITRS transformation matrix
! ----------------------------------------------------------------------
! [ITRS] = W(t)^T * R(t)^T * Q(t)^T * [GCRS] = W(t)^T * R3(ERA) * Q(t)^T * [GCRS]
!*        [TRS]  =  RPOM * R_3(ERA) * RC2I * [CRS]
! ----------------------------------------------------------------------
! Inverse transformation matrix as transpose matrix (orthogonality)
      !CALL iau_TR ( TRS2CRS, CRS2TRS )

! or (analytical form)

! CRS2TRS = Wt_inv * Rt_inv * Qt_inv
      ! CRS2TRS = RPOM * R_era * RC2I
      CALL iau_RXR ( Wt_inv, Rt_inv, Wi_Ri )
      CALL iau_RXR ( Wi_Ri, Qt_inv, CRS2TRS )
! ----------------------------------------------------------------------




! ----------------------------------------------------------------------
! Derivatives of CRS-TRS transformation matrix (direct/inverse)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth angular velocity
! ----------------------------------------------------------------------
! IERS 2010;
!      dtheta = 0.7292115D0 * 10.0D-04
! ----------------------------------------------------------------------
! Omega as derivative of Earth Rotation Angle (ERA), dERA/dt in rad/s
      dtheta = 2.0D0 * PI_global * 1.00273781191135448D0 * (1.0D0 / 86400D0)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! d_TRS2CRS
! ----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%% Computation of derivative of TRS2CRS matrix
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! d_TRS2CRS = d(TRS2CRS)/dt = Qt * (dtheta * P * Rt) * Wt
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! ----------------------------------------------------------------------
!P = [ 0  -1   0
!      1   0   0
!      0   0   0 ] ;
! ----------------------------------------------------------------------
      P_dR (1,1) =  0.0D0
      P_dR (1,2) = -1.0D0
      P_dR (1,3) =  0.0D0
      P_dR (2,1) =  1.0D0
      P_dR (2,2) =  0.0D0
      P_dR (2,3) =  0.0D0
      P_dR (3,1) =  0.0D0
      P_dR (3,2) =  0.0D0
      P_dR (3,3) =  0.0D0
! ----------------------------------------------------------------------
! d_TRS2CRS = dtheta * Qt * P * Rt * Wt
      CALL iau_RXR ( Qt, P_dR, QP )  
      CALL iau_RXR ( QP, Rt, QPR )  
      CALL iau_RXR ( QPR, Wt, QPRW )  
      d_TRS2CRS = dtheta * QPRW
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! d_CRS2TRS 
! ----------------------------------------------------------------------
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%% Computation of derivative of CRS2TRS matrix
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! d_CRS2TRS = Wt_inv * (omega * P^T * Rt_inv) * Qt_inv
! d_CRS2TRS = omega * W(-t) * P^T * R(-t) * Q(-t) ;
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      CALL iau_TR ( P_dR, P_dR_T )
      CALL iau_RXR ( Rt_inv, Qt_inv, Ri_Qi ) 
      CALL iau_RXR ( P_dR_T, Ri_Qi, PT_Ri_Qi ) 
      CALL iau_RXR ( Wt_inv, PT_Ri_Qi, Wi_PT_RiQi )
      d_CRS2TRS = dtheta * Wi_PT_RiQi
! ----------------------------------------------------------------------



END
