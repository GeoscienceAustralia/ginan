SUBROUTINE era_matrix (mjd, ERM, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)


! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Subroutine:  era_matrix.f90
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Purpose:
!  Earth Rotation matrix (approximate) based only on the Earth Rotation angle 
!  without considering any corrections (see Note 1) 
!  Simplified approach to the transformation matrix between Inertial and Earth-fixed systems 
!  based only to the Earth Rotation matrix.
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Note 1:
! The Earth Rotation Angle is computed based on the IERS Conventions 2010 formula.
! The UT1-UTC difference is considered as zero (approximate approach)
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Input arguments:
! - mjd:			Modified Julian Day number of the epoch (in TT scale)
!
! Output arguments:
! - ERM:			Earth Rotation matrix
! - CRS2TRS:		Inertial to Terrestrial reference system transformation matrix
! - TRS2CRS:		Terrestrial to Inertial reference system transformation matrix 
! - d_CRS2TRS:		Derivative of Inertial to Terrestrial reference system transformation matrix
! - d_TRS2CRS:		Derivative of Terrestrial to Inertial reference system transformation matrix
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! Dr. Thomas Papanikolaou, Geoscience Australia            September 2016
! %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: ERM(3,3)
      REAL (KIND = prec_d), INTENT(OUT) :: CRS2TRS(3,3), TRS2CRS(3,3)
      REAL (KIND = prec_d), INTENT(OUT) :: d_CRS2TRS(3,3), d_TRS2CRS(3,3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Called Functions (F77)
      DOUBLE PRECISION iau_ERA00 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
      DOUBLE PRECISION TT1, TT2, TT1_UT1, TT2_UT1, TAI1, TAI2
      INTEGER (KIND = prec_int8) :: mjd_UTC_day
      DOUBLE PRECISION PR(3,3)
      DOUBLE PRECISION JD_ut1, Tu

	  
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


      arcsec2rad = PI_global / (3600.0D0 * 180.0D0)


! ----------------------------------------------------------------------
! The UT1-UTC difference is considered as zero (approximate approach)
UT1_UTC = 0.D0
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
! UT1
      mjd_UT1 = mjd_UTC + UT1_UTC / 86400.D0
      TT1_UT1 = 2400000.5D0
      TT2_UT1 = mjd_UT1  
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Earth Rotation Matrix (ERM) | R(t)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth Rotation Angle: era (in radians)
! ----------------------------------------------------------------------

era = iau_ERA00 ( TT1_UT1, TT2_UT1 )

! ----------------------------------------------------------------------
if (1 < 0) then

JD_ut1 = TT1_UT1 + TT2_UT1
Tu = JD_ut1 - 2451545.0D0

era = (2.D0*PI_global) * ( 0.7790572732640D0 + 1.00273781191135448D0 * Tu ) 

if (era > 2.D0*PI_global) then
	era = era - INT(era / (2.D0*PI_global) ) * (2.D0*PI_global)
end if

end if
! ----------------------------------------------------------------------


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

! R(t)^T = R3(ERA)	  
      !CALL iau_RZ ( era, R_era )
! Rotation matrix
      R_era(1,1:3) = (/  cos(era),  sin(era),	0.D0 /)
      R_era(2,1:3) = (/ -sin(era),  cos(era),	0.D0 /)
      R_era(3,1:3) = (/		 0.D0,		0.D0, 	1.D0 /)

	  
! R(t) = R3(-ERA)
      era_n = -1.0D0 * era
!      CALL iau_RZ ( era_n, R_era_n )
      R_era_n(1,1:3) = (/  cos(era_n),  sin(era_n),	0.D0 /)
      R_era_n(2,1:3) = (/ -sin(era_n),  cos(era_n),	0.D0 /)
      R_era_n(3,1:3) = (/		 0.D0,		  0.D0, 1.D0 /)   
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth Rotation matrix
ERM = R_era_n  
! ----------------------------------------------------------------------

Rt = R_era_n
Rt_inv = R_era



! ----------------------------------------------------------------------
! CRS-TRS transformation matrix (direct/inverse)
! ----------------------------------------------------------------------
! ITRS to GCRS transformation matrix
TRS2CRS = Rt

! GCRS to ITRS transformation matrix
CRS2TRS = Rt_inv
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


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%% Computation of derivative of TRS2CRS matrix
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! d_TRS2CRS = d(TRS2CRS)/dt = (dtheta * P * Rt) 
! ----------------------------------------------------------------------
!P = [ 0  -1   0
!      1   0   0
!      0   0   0 ] ;

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
CALL iau_RXR ( P_dR, Rt, PR )
d_TRS2CRS = dtheta * PR
! ----------------------------------------------------------------------


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
!%% Computation of derivative of CRS2TRS matrix
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
! d_CRS2TRS = omega * P^T * R(-t) ;
! ----------------------------------------------------------------------
CALL iau_TR ( P_dR, P_dR_T )
CALL iau_RXR ( P_dR_T, Rt_inv, PR ) 
d_CRS2TRS = dtheta * PR
! ----------------------------------------------------------------------




END
