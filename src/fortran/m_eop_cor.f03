MODULE m_eop_cor


! ----------------------------------------------------------------------
! MODULE: m_eop_cor.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the modified eop_cor subroutine 
! 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia 
! Created:	23 July 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
 
Contains


SUBROUTINE eop_cor (mjd, EOP_days, EOP_sol, n_interp , EOP_cr)


! ----------------------------------------------------------------------
! Subroutine:  eop_cor.f90
! ----------------------------------------------------------------------
! Purpose:
!  IERS Earth Orientation Parameters (EOP) data reading and processing.
!  Corrections computing due to tidal variations
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number of the epoch (in TT)
! - EOP_fname:		EOP data file name  e.g. 'eopc04_IAU2000.62-now'
! - EOP_days:		EOP data array of the days (data points aplied for interpolation)
! - EOP_sol:		EOP solution type (see Note 1)
! - n_interp:		number of points to be used for EOP data interpolation 
!
! Output arguments:
! - EOP_cr:		Array of the EOP data after applying the tidal corrections
!   EOP_cr = [MJD xp_cor yp_cor UT1-UTC_cor LOD_cor dX dY]  1x6 matrix
!   MJD:     		MJD of the input epoch (in TT)
!   xp_cor,yp_cor:	Polar motion coordinates (arcsec) at input epoch considering 
!					corrections due to tidal variations (ocean tidal and libration effects)  	
!   UT1UTC_cor:		UT1-UTC difference (sec) at input epoch considering corrections
!					due to tidal variations	(ocean tidal and libration effects)  
!   LOD:			Length of Day (LOD)
!   dX, dY:			Corrections to the Precession-Nutation model (arcsec)
! ----------------------------------------------------------------------
! Note 1:
! Values of the EOP_sol input argument:
! 1. EOP_sol = 1 : refers to EOP data format of the C04 solution
!    			   provided by the IERS Earth Orientation Center 
! 2. EOP_sol = 2 : refers to EOP data format of the daily solution (finals2000A.daily)
!    			   provided by the IERS Rapid Service/Prediction Center
! 3. EOP_sol = 3 : refers to EOP data format of the ultra-rapid products
!    			   provided by the IGS (per every 6 hours)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            December 2015
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 16 August 2018
!	Replacement of reading here the EOP data (input variable "EOP_fname") 
!   with the EOP data array (input variable "EOP_days") that is povided 
!   by the global variable "EOP_day_glb" through the module mdl_eop.f90
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
      !CHARACTER (LEN=50), INTENT(IN) :: EOP_fname
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: EOP_days
      INTEGER (KIND = prec_int4), INTENT(IN) :: n_interp
      INTEGER (KIND = prec_int1), INTENT(IN) :: EOP_sol
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: EOP_cr(7)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
      DOUBLE PRECISION TT1, TT2, TT1_UT1, TT2_UT1, TAI1, TAI2
      INTEGER (KIND = prec_int8) :: mjd_UTC_day
      REAL (KIND = prec_d) :: EOP_day(7)
      DOUBLE PRECISION xp, yp, xp_int, yp_int, xp_cor, yp_cor
      DOUBLE PRECISION dUT1_UTC, ut1utc_int, UT1UTC_cor
      DOUBLE PRECISION dX_eop, dY_eop, LOD 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: i, sz1_EOP, sz2_EOP
      DOUBLE PRECISION x_int, y_int, ut1_int
      REAL (KIND = prec_d) :: MJDint_ar(n_interp), xint_ar(n_interp), yint_ar(n_interp), UT1int_ar(n_interp)

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
! EOP data array
sz1_EOP = SIZE (EOP_days,DIM=1)
sz2_EOP = SIZE (EOP_days,DIM=2)
! ----------------------------------------------------------------------
! Ensure LOD, dX_eop, dY_eop are set to something sensible
LOD = 0.d0
dX_eop = 0.d0
dY_eop = 0.d0

      DO i = 1 , sz1_EOP
         MJDint_ar(i) = EOP_days(i,1)
! xp,yp (arcsec)
         xint_ar(i)   = EOP_days(i,2)
         yint_ar(i)   = EOP_days(i,3)
! UT1-UTC (sec)
         UT1int_ar(i) = EOP_days(i,4)

! LOD (sec)
!		 LOD_ar = EOP_days(i,5)

! dX,dY (arcsec)													
!		 dX_ar = EOP_days(i,6)
!		 dY_ar = EOP_days(i,7)

If (mjd_UTC_day == EOP_days(i,1) ) then
      LOD =    EOP_days(i,5)
      dX_eop = EOP_days(i,6)
      dY_eop = EOP_days(i,7)
End If
      END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Diurnal and semi-diurnal tidal corrections to EOP data by IERS
! "Polar motion" and "UT1" corrections due to ocean tidal and libration effects at interpolation epoch
      CALL INTERP_iers (MJDint_ar, xint_ar, yint_ar, UT1int_ar, n_interp, mjd_UTC, x_int,y_int,ut1_int)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! xp,yp corrected (arcsec) 											
      xp_cor = x_int
      yp_cor = y_int  
! ----------------------------------------------------------------------
! UT1-UTC corrected (sec) 													
      UT1UTC_cor = ut1_int
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      EOP_cr(1) = mjd
      EOP_cr(2) = xp_cor
      EOP_cr(3) = yp_cor
      EOP_cr(4) = UT1UTC_cor
      EOP_cr(5) = LOD
      EOP_cr(6) = dX_eop
      EOP_cr(7) = dY_eop 
! ----------------------------------------------------------------------

END SUBROUTINE


END
