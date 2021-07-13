SUBROUTINE time_TT_sec (mjd , dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS)


! ----------------------------------------------------------------------
! Subroutine:	time_TT.f90
! ----------------------------------------------------------------------
! Purpose:
!  Time Systems transformation
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number in TT (including fraction of the day)
!
! Output arguments:
! - dt_TT_TAI:		TT-TAI difference in seconds 
! - dt_TAI_UTC:		TAI-UTC difference in seconds 
! - dt_TAI_GPS:		TAI-GPS difference in seconds 
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
      !REAL (KIND = prec_d), INTENT(OUT) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      REAL (KIND = prec_d), INTENT(OUT) :: dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS
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


! ----------------------------------------------------------------------
! Leap Seconds: TAI-UTC difference
! ----------------------------------------------------------------------
!SUBROUTINE iau_DAT ( IY, IM, ID, FD, DELTAT, J )
!*     DELTAT   d     TAI minus UTC, seconds
!      CALL iau_DAT ( IY, IM, ID, FD, TAI_UTC, J_flag )
! SCM replaced SOFA hardwire with tai-utc (leap second, read from file).
      CALL taiutc ( IY, IM, ID, FD, TAI_UTC, J_flag )
! ----------------------------------------------------------------------
      !PRINT *,"TAI_UTC", TAI_UTC

! ----------------------------------------------------------------------
! TT
!      mjd_TT = mjd
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! TAI time scale
TT_TAI = 32.184D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GPS Time scale
TAI_GPS = 19D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Time Scales differences in seconds
! ----------------------------------------------------------------------
dt_TT_TAI  = TT_TAI
dt_TAI_UTC = TAI_UTC
dt_TAI_GPS = TAI_GPS
! ----------------------------------------------------------------------


END
