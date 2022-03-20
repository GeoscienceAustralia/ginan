SUBROUTINE time_GPSweek (mjd , GPS_week, GPS_wsec, GPSweek_mod1024)


! ----------------------------------------------------------------------
! Subroutine:     time_GPSweek.f90
! ----------------------------------------------------------------------
! Purpose:
!  Convert GPS time to GPS Week number and seconds
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:                Modified Julian Day number in GPS time (including fraction of the day)
!
! Output arguments:
! - GPS_week:           GPS Week number 
! - GPS_day :           Day since start of GPS Week
! - GPS_wsec:           Seconds since start of GPS Week 
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
      INTEGER (KIND = prec_int8) , INTENT(OUT) :: GPS_week, GPSweek_mod1024
      REAL (KIND = prec_d), INTENT(OUT) :: GPS_wsec
      !REAL (KIND = prec_d), INTENT(OUT) :: GPS_day
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: GPS_day
      INTEGER  J_flag
!      IY, IM, , ID
      DOUBLE PRECISION DJM0  
      DOUBLE PRECISION mjd_1980, mjd_1999, delta_days, GPS_week_0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! The GPS Week Number field is modulo 1024 : 
! ----------------------------------------------------------------------
! GPS_wn = 0    | 06 Jan. 1980
! GPS_wn = 1023 | 15 Aug. 1999
! GPS_wn = 0    | 22 Aug. 1999      | modulo 1024
! GPS_wn = 1    | 29 Aug. 1999
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! 06 Jan. 1980 : GPS_week = 0
      CALL iau_CAL2JD ( 1980, 01, 06, DJM0, mjd_1980, J_flag )
! ----------------------------------------------------------------------
! Rollover to 0
! 22 Aug. 1999 : GPS_week = 0
      CALL iau_CAL2JD ( 1999, 08, 22, DJM0, mjd_1999, J_flag )
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Days since GPS weeks start epoch i.e. 06 Jan. 1980 
      delta_days = mjd - mjd_1980
      !delta_days = mjd - mjd_1999
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GPS Week since 6 Jan. 1980
      GPS_week_0 = delta_days / 7.0D0   

! GPS Week number | Modulo 1024
      GPSweek_mod1024 = MOD ( INT(GPS_week_0) , 1024) 

! GPS Week (according to IGS numbering)
      GPS_week = INT(GPS_week_0)
! ----------------------------------------------------------------------
        
        
! ----------------------------------------------------------------------
! Day since start of GPS week (including fraction of the day)
      !GPS_day =  (GPS_week_0 - INT(GPS_week_0)) * 7D0 
      GPS_day = delta_days - GPS_week * 7.D0 
! ----------------------------------------------------------------------
        
! ----------------------------------------------------------------------
! Seconds since start of GPS week     
      !GPS_wsec = (GPS_week_0 - INT(GPS_week_0)) * 7.0D0 * 86400.0D0      
      GPS_wsec = GPS_day *    86400.0D0           
! ----------------------------------------------------------------------
        !print *,"mjd       ", mjd    
      !print *,"mjd_1980  ", mjd_1980       
      !print *,"delta_days", delta_days     
      !print *,"GPS_week_0", GPS_week_0     
      !print *,"GPS_week  ", GPS_week       
      !print *,"GPS_wsec  ", GPS_wsec       
      !print *,"GPS_day   ", GPS_day        
      !print *,"          "     


END


