PROGRAM main_time

! ----------------------------------------------------------------------
! Program:	main_time.f03
! ----------------------------------------------------------------------
! Purpose: This function is used to convert different time systems, including
!          MJD, GPS week, calendar year and day of year
! 
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng, Geoscience Australia, Frontier-SI
! Created:	03-Apr-2020 
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_timeconfig
	  
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: jd0
      REAL (KIND = prec_d) :: GPS_wsec
      DOUBLE PRECISION FD, mjd_1980, delta_days
      DOUBLE PRECISION MJDD0, MJDref  
      INTEGER (KIND = prec_int8) :: GPSweek_mod1024
      INTEGER (KIND = prec_int4) :: J
      INTEGER J_flag

! Initialize variables
MJD_IN    = 0.D0
WEEK_IN   = 0 
WEEKDAY_IN= 0
YEAR_IN   = 0 
MONTH_IN  = 0
DAY_IN    = 0  
DOY_IN    = 0

CALL time_cmdline

IF (MJD_IN /= 0.D0) THEN
! Print MJD
        PRINT*,'MJD  ', MJD_IN
        PRINT*,''

! Compute calendar year
        jd0 = 2400000.5D0
        CALL iau_JD2CAL ( jd0, MJD_IN, YEAR_IN, MONTH_IN, DAY_IN, FD, J_flag )
        PRINT*,'YEAR-MONTH-DAY  ', YEAR_IN, MONTH_IN, DAY_IN
        PRINT*,''

! Compute day of year
        CALL iau_CAL2JD ( YEAR_IN, 1, 1, MJDD0, MJDref, J )
        DOY_IN = IDNINT(MJD_IN-MJDref) + 1
        PRINT*,'DOY  ', YEAR_IN, DOY_IN
        PRINT*,''

! Compute GPS week
        CALL time_GPSweek (MJD_IN , WEEK_IN, GPS_wsec, GPSweek_mod1024)
        WEEKDAY_IN = IDNINT(GPS_wsec/86400.D0)
        PRINT*,'GPSWEEKDAY  ', WEEK_IN, WEEKDAY_IN
        PRINT*,''

ELSE IF (DOY_IN /= 0) THEN
! Compte MJD
        CALL iau_CAL2JD ( YEAR_IN, 1, 1, MJDD0, MJDref, J )
        MJD_IN = DOY_IN -1 + MJDref
        PRINT*,'MJD  ', MJD_IN
        PRINT*,''

! Compute calendar year
        jd0 = 2400000.5D0
        CALL iau_JD2CAL ( jd0, MJD_IN, YEAR_IN, MONTH_IN, DAY_IN, FD, J_flag )
        PRINT*,'YEAR-MONTH-DAY  ', YEAR_IN, MONTH_IN, DAY_IN
        PRINT*,''

! Print DOY
        PRINT*,'DOY  ', YEAR_IN, DOY_IN
        PRINT*,''

! Compute GPS week
        CALL time_GPSweek (MJD_IN , WEEK_IN, GPS_wsec, GPSweek_mod1024)
        WEEKDAY_IN = IDNINT(GPS_wsec/86400.D0)
        PRINT*,'GPSWEEKDAY  ', WEEK_IN, WEEKDAY_IN
        PRINT*,''

ELSE IF (MONTH_IN /= 0) THEN
! Compute MJD
        CALL iau_CAL2JD ( YEAR_IN, MONTH_IN, DAY_IN, MJDD0, MJD_IN, J )
        PRINT*,'MJD  ', MJD_IN
        PRINT*,''

! Print canlendar year
        PRINT*,'YEAR-MONTH-DAY  ', YEAR_IN, MONTH_IN, DAY_IN
        PRINT*,''
 
! Compute day of year
        CALL iau_CAL2JD ( YEAR_IN, 1, 1, MJDD0, MJDref, J )
        DOY_IN = IDNINT(MJD_IN-MJDref) + 1
        PRINT*,'DOY  ', YEAR_IN, DOY_IN
        PRINT*,''
        
! Compute GPS week
        CALL time_GPSweek (MJD_IN , WEEK_IN, GPS_wsec, GPSweek_mod1024)
        WEEKDAY_IN = IDNINT(GPS_wsec/86400.D0)
        PRINT*,'GPSWEEKDAY  ', WEEK_IN, WEEKDAY_IN
        PRINT*,''

ELSE IF (WEEK_IN /= 0) THEN
        delta_days = WEEKDAY_IN + WEEK_IN*7 
! 06 Jan. 1980 : GPS_week = 0
        CALL iau_CAL2JD ( 1980, 01, 06, MJDD0, mjd_1980, J_flag )
! Compute MJD
        MJD_IN = delta_days + mjd_1980
        PRINT*,'MJD  ', MJD_IN
        PRINT*,''

! Compute calendar year
        jd0 = 2400000.5D0
        CALL iau_JD2CAL ( jd0, MJD_IN, YEAR_IN, MONTH_IN, DAY_IN, FD, J_flag )
        PRINT*,'YEAR-MONTH-DAY  ', YEAR_IN, MONTH_IN, DAY_IN
        PRINT*,''

! Compute day of year
        CALL iau_CAL2JD ( YEAR_IN, 1, 1, MJDD0, MJDref, J )
        DOY_IN = IDNINT(MJD_IN-MJDref) + 1
        PRINT*,'DOY  ', YEAR_IN, DOY_IN
        PRINT*,''

! Print GPSWEEK DAY
        PRINT*,'GPSWEEKDAY  ', WEEK_IN, WEEKDAY_IN
        PRINT*,''

END IF



END PROGRAM

