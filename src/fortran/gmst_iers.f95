SUBROUTINE gmst_iers(mjd, ut1_utc , GMST)


! ----------------------------------------------------------------------
! SUBROUTINE: gmst_iers.f03
! ----------------------------------------------------------------------
! Purpose:
!  Greenwich Mean Sidereal Time (GMST) computation based on the IERS 
!  Convetions 2010 and IAU 2006 resolutions
! ----------------------------------------------------------------------
! Input arguments
! - mjd:			Modified Julain Day (MJD) number in Terrestrial Time (TT) 
! 					scale including the fraction of the day 
! - ut1_utc:   		Time difference between UT1 and UTC (seconds)
!
! Output arguments:
! - GMST :			Greenwich mean sidereal time (radians)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	27 October 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN) :: mjd, ut1_utc

! OUT
      REAL (KIND = prec_q), INTENT(OUT) :: GMST
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------  
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC, mjd_UT1
      INTEGER (KIND = prec_int8) :: mjd_UTC_day  
      DOUBLE PRECISION TT1_UT1, TT2_UT1, TT1, TT2
      DOUBLE PRECISION GMST00, GMST06
! ----------------------------------------------------------------------  

! ----------------------------------------------------------------------
! Called Functions (F77)
      DOUBLE PRECISION iau_GMST00, iau_GMST06, iau_ANP 
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Time Systems transformation											 
CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! TT
      TT1 = 2400000.5D0
      TT2 = mjd_TT
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
! IAU SOFA subroutines
! Model consistent with IAU 2000 resolutions	  
GMST00 = iau_GMST00 ( TT1_UT1, TT2_UT1, TT1, TT2 )

! IERS Conventions 2010 Equation 5.32
! Model consistent with IAU 2006 resolutions	  
GMST06 = iau_GMST06 ( TT1_UT1, TT2_UT1, TT1, TT2 )
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Greenwich Mean Sidereal Time (GMST) in radians
!GMST = GMST00
GMST = GMST06
! ----------------------------------------------------------------------



END
