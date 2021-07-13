SUBROUTINE time_UTC (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)


! ----------------------------------------------------------------------
! Subroutine:	time_UTC.f90
! ----------------------------------------------------------------------
! Purpose:
!  Time Systems transformation
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number in UTC (including fraction of the day)
!
! Output arguments:
! - mjd_TT:			MJD number in TT (including the fraction of the day)
! - mjd_GPS:		MJD number in GPS (including the fraction of the day)
! - mjd_TAI:		MJD number in TAI (including the fraction of the day)
! - mjd_UTC:		MJD number in UTC (including the fraction of the day)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            December 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: jd0
      INTEGER IY, IM, ID, J_flag
      DOUBLE PRECISION FD  
      DOUBLE PRECISION TAI_UTC, TT_TAI, TAI_GPS
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      jd0 = 2400000.5D0
      CALL iau_JD2CAL ( jd0, mjd, IY, IM, ID, FD, J_flag )
! ----------------------------------------------------------------------
      !PRINT *,"J_flag time_utc.f90", J_flag
      !PRINT *,"time_utc.f90 mjd", mjd
      !PRINT *,"time_utc.f90 FD", FD

! ----------------------------------------------------------------------
! Leap Seconds: TAI-UTC difference
! ----------------------------------------------------------------------
!SUBROUTINE iau_DAT ( IY, IM, ID, FD, DELTAT, J )
!*     DELTAT   d     TAI minus UTC, seconds
!      CALL iau_DAT ( IY, IM, ID, FD, TAI_UTC, J_flag )
! SCM replaced SOFA hardwire with tai-utc (leap second, read from file).
      CALL taiutc ( IY, IM, ID, FD, TAI_UTC, J_flag )
! ----------------------------------------------------------------------
      !PRINT *,"TAI-UTC", TAI_UTC
      !PRINT *,"J_flag time_utc.f90", J_flag


! ----------------------------------------------------------------------
! UTC
      mjd_UTC = mjd
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! TAI
      mjd_TAI = mjd_UTC + TAI_UTC / 86400D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! TAI
      TAI_GPS = 19D0
      mjd_GPS = mjd_TAI - TAI_GPS / 86400D0
! ----------------------------------------------------------------------
	  
! ----------------------------------------------------------------------
! TT
      TT_TAI = 32.184D0
      mjd_TT = mjd_TAI + TT_TAI / 86400D0
! ----------------------------------------------------------------------


END
