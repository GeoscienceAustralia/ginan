SUBROUTINE  readbrdcmessg(UNIT_IN,VERSION,MAXNSAT,MAXNPAR,MAXEPO, &
                          IYEAR4,MONTH,IDAY,ISAT,EPH,CLK)


! ----------------------------------------------------------------------
! MODULE: readbrdcmessg.f90 
! ----------------------------------------------------------------------
! Purpose: Read the orbit and clock parameters from broadcast ephemeris.
!
! INPUT:
!       UNIT_IN : File number
!       VERSION : RINEX version
!       MAXNSAT : Maximum number of satellites
!       MAXNPAR : Maximum number of parameters from broadcast file
!       MAXEPO  : Maximum number of epochs recorded in broadcast file
!       IYEAR4  : 4-digit year
!       MONTH   : month
!       IDAY    : day
!
! OUTPUT:
!       ISAT    : Satellite PRN number
!       EPH     : Array containing the broadcast orbital message
!       CLK     : Array containing the broadcast clock message 
!           
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng
!
! Copyright: GEOSCIENCE AUSTRALIA, AUSTRALIA
!
! Created:      31-05-2019
! ----------------------------------------------------------------------
      USE mdl_precision
      USE mdl_num
      USE m_shadow
      USE mdl_param
      IMPLICIT NONE


!-------------------------------------------------------------------
      INTEGER (KIND = prec_int4),INTENT(IN) :: UNIT_IN
      INTEGER (KIND = prec_int4),INTENT(IN) :: VERSION
      INTEGER (KIND = prec_int4),INTENT(IN) :: MAXNSAT, MAXNPAR,MAXEPO
      INTEGER (KIND = prec_int4),INTENT(OUT) :: IYEAR4,MONTH,IDAY
      INTEGER (KIND = prec_int4),INTENT(OUT) :: ISAT
      !REAL (KIND = prec_q),INTENT(OUT) ::EPH(MAXNPAR,MAXEPO,MAXNSAT),CLK(MAXNPAR,MAXEPO,MAXNSAT)
      REAL (KIND = prec_q),INTENT(OUT)::EPH(MAXNPAR,MAXEPO,MAXNSAT),CLK(3,MAXEPO,MAXNSAT)
      REAL (KIND = prec_q) :: EPHDAT(MAXNPAR)
      !REAL (KIND = prec_q) :: EPHV3(MAXNPAR),CLKV3(MAXNPAR)
      REAL (KIND = prec_q) :: EPHV3(MAXNPAR),CLKV3(3)
      INTEGER (KIND = prec_int4) :: ISVN
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: I,J,K,IEPO
      INTEGER (KIND = prec_int4) :: N2,IOS_LINE, NG
      INTEGER (KIND = prec_int4) :: iSec
      INTEGER (KIND = prec_int4) :: iSvn2
      INTEGER (KIND = prec_int2) :: LIMYR
      INTEGER (KIND = prec_int4) :: IYEAR, IHOUR,MINUTE
      REAL (KIND = prec_q)       :: SEC
      REAL (KIND = prec_q)       :: LIMYR2, LIMYR0
      REAL (KIND = prec_q)       :: DAY, TOC, T1, DTT
      CHARACTER (LEN=7) :: FORMAT1
      CHARACTER (LEN=7) :: cSvn
      CHARACTER (LEN=500) :: line_ith
! ----------------------------------------------------------------------
!      INTEGER IY, IM, ID
      DOUBLE PRECISION MJD0, MJD
      REAL (KIND = prec_q)       :: Z, T
      REAL (KIND = prec_d)       :: GPS_wsec, GPS_day
      INTEGER (KIND = prec_int8) :: GPS_week, GPSweek_mod1024
      INTEGER (KIND = prec_int2) :: AllocateStatus
      DATA LIMYR/1960/

EPHDAT = 0.d0
EPH    = 0.d0
CLK    = 0.d0
EPHV3  = 0.d0
CLKV3  = 0.d0

! Read broadcast message 
!-------------------------------

!stop compiler warning
N2 = 25

IF(VERSION.EQ.1) THEN
   N2 = 25
ELSE IF(VERSION.EQ.2) THEN
   N2 = MAXNPAR
ELSE IF(VERSION.EQ.3) THEN
   NG = 16
   N2 = MAXNPAR
ENDIF
T1 = 0.d0
IEPO = 0
DO WHILE(.TRUE.)
! Read input file
FORMAT1 = '(A80)'
READ (UNIT=UNIT_IN,FMT=FORMAT1,IOSTAT=IOS_LINE) line_ith

   ! ERROR MESSAGE HANDLING
   IF (IOS_LINE > 0)  THEN
       print*,'SOMETHING WRONG! PLEASE CHECK FILE OR VARIABLES USED IN PROGRAM '
   ELSE IF (IOS_LINE < 0) THEN
       print*,'END OF BROADCAST ORBIT FILE'
       EXIT
   ELSE
     ! print*,line_ith
   END IF
     ! -----------------------

! Read the orbital parameters
IF(VERSION.LT.3) THEN
READ(line_ith,'(I2,5I3,F5.1,3D19.12)') ISVN,IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,&
                                     (EPHDAT(K),K=2,4)

IEPO = IEPO + 1
ISAT = ISVN
       ! Convert 2-digit year to 4-digit year
         LIMYR2 = MOD(LIMYR,100)
         LIMYR0 = (LIMYR/100)*100
         IYEAR4 = IYEAR
         IF (IYEAR4.LT.LIMYR2) IYEAR4=IYEAR4+LIMYR0+100
         IF (IYEAR4.LT.100)    IYEAR4=IYEAR4+LIMYR0

       ! Convert the calender time to MJD time system
         CALL iau_CAL2JD ( IYEAR4, MONTH, IDAY, MJD0, MJD, J )
         ! print*,'MJD =, J =',MJD, J
          TOC = MJD + (IHOUR/24.D0+MINUTE/1440.D0+SEC/86400.D0)
          !print*,'TOC =', TOC


ELSE IF(VERSION.EQ.3) THEN
READ(line_ith,'(A1,I2,I5,5I3,3D19.12)') cSvn,iSvn2,IYEAR,MONTH,IDAY,IHOUR,&
                                     MINUTE,iSec,(EPHDAT(K),K=2,4)
IEPO = IEPO + 1
IYEAR4 = IYEAR

IF(IEPO > MAXEPO) THEN
PRINT*,'THE NUMBER OF EPOCHS RECORDED IN A FILE EXCEEDS THE THRESHOLD.'
PRINT*,'THE NUMBER OF EPOCHS RECORDED:',IEPO
PRINT*,'THE THRESHOLD:', MAXEPO
PRINT*,'PLEASE ADJUST MAXEPO IN MAIN PROGRAM.'
END IF
        ! Convert the calender time to MJD time system
          CALL iau_CAL2JD ( IYEAR4, MONTH, IDAY, MJD0, MJD, J )
!          print*,'MJD =, J =',MJD, J
         
           TOC = MJD + (IHOUR/24.D0+MINUTE/1440.D0+iSEC/86400.D0)
        ! The following critierion only works for GALILEO broadcast records.
        ! -----------------------------------------------------------------
           DTT = TOC - T1
           IF (DTT == 0.d0) THEN
           IEPO = IEPO - 1
           !PRINT*, 'SATELLITE PRN =',TRIM(cSvn),iSvn2 
           !PRINT*, 'THE EPOCH NUMBER IS ASSIGNED TO THE PREVIOUS ONE : IEPO =', IEPO
           END IF
        ! -----------------------------------------------------------------

!IF (cSvn == 'R') EXIT
IF (cSvn == 'S') EXIT ! exclude the SBAS satellites
! Convert the cSvn to the ACS internal prn
         CALL prn_shift_brdc (cSvn, iSvn2, ISAT)
!PRINT*,'SAT PRN =',cSvn,iSvn2,'==>','ACS/PRN',ISAT, 'IEPO =', IEPO

ELSE
PRINT*,'THE RINEX VERSION OF BROADCAST EPHEMERIS CANNOT BE RECOGNIZED!'
END IF
T1 = TOC


IF (ISAT < 50 .OR.  ISAT > 100 .AND. ISAT < 150 .OR. & 
    ISAT > 150 .AND. ISAT < 200 .OR.  ISAT > 200) THEN
! Convert MJD to the GPS week
    CALL time_GPSweek2 (TOC , GPS_week, EPHDAT(1), GPSweek_mod1024, GPS_day)
!print*,'GPS_week=, GPS_day=',GPS_week,GPS_day
!print*,'EPHDAT(1-4) =',EPHDAT(1),EPHDAT(2),EPHDAT(3),EPHDAT(4)

     DO  I=5,N2,4
     READ (UNIT=UNIT_IN,FMT=FORMAT1,IOSTAT=IOS_LINE) line_ith
     !print*,line_ith

     IF (VERSION == 2) READ(line_ith,'(3X,4D19.12)') (EPHDAT(K),K=I,I+3)
     IF (VERSION == 3) READ(line_ith,'(4X,4D19.12)') (EPHDAT(K),K=I,I+3)
!     PRINT*,'EPHDAT=',  EPHDAT(I),EPHDAT(I+1),EPHDAT(I+2),EPHDAT(I+3)

     END DO

     CALL reformbrdc (MAXNPAR,EPHDAT,EPHV3,CLKV3)
     CLK(1:3,IEPO,ISAT)=CLKV3(1:3)
     DO I=1,20
     EPH(I,IEPO,ISAT)=EPHV3(I)
     !print*,'EPH(1:2,:,:), GPSWEEK, TOE', EPH(1,IEPO,ISAT), EPH(2,IEPO,ISAT)
     END DO

ELSE IF (ISAT > 50 .AND. ISAT < 100 )THEN
     EPHDAT(1) = TOC ! GLONASS time system is UTC time
                     ! Here the first element is setup to the MJD in convenience
                     ! to convert to GPS time later 
!     print*,'ISAT=',ISAT,'GPS_week=',GPS_week,'GPS_day=',GPS_day
!     print*,'EPHDAT(1-4) =',EPHDAT(1),EPHDAT(2),EPHDAT(3),EPHDAT(4)

     DO  I=5,NG,4
     READ (UNIT=UNIT_IN,FMT=FORMAT1,IOSTAT=IOS_LINE) line_ith
!     print*,line_ith
     IF (VERSION == 2) READ(line_ith,'(3X,4D19.12)') (EPHDAT(K),K=I,I+3)
     IF (VERSION == 3) READ(line_ith,'(4X,4D19.12)') (EPHDAT(K),K=I,I+3)
!     print*,'EPHDAT(5-16)=', EPHDAT(I), EPHDAT(I+1),EPHDAT(I+2),EPHDAT(I+3)
     END DO


     DO I=1,16
     EPH(I,IEPO,ISAT)=EPHDAT(I)
!     print*,'ISAT =',ISAT,'UTC, SECONDS', EPH(I,IEPO,ISAT), EPHDAT(I)
     END DO



END IF

END DO
END SUBROUTINE  
