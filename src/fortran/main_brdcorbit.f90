PROGRAM  main_brdcorbit 

! ----------------------------------------------------------------------
! PROGRAM: brdcorbit.f90
! ----------------------------------------------------------------------
! Purpose: Create a sp3 file from the multi-GNSS broadcast ephemeris *.yyn file, 
!          where yy denotes the year or *.rnx file
! ----------------------------------------------------------------------
! Author :      Dr. Tzupang Tseng
!
! Copyright: GEOSCIENCE AUSTRALIA, AUSTRALIA
!
! Created:   31-05-2019
!
! Changes:   23-07-2019 Tzupang Tseng: The multi-GNSS option has been implemented 
!                                      in the program, except for GLONASS   
!
! Remarks:   08-07-2019 Tzupang Tseng: CUrrently, the program only works for
!                                      GPS-only.
! ----------------------------------------------------------------------
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_brdconfig
      USE m_write_brd2sp3
      USE m_write_orb2sp3
      USE m_read_leapsec
      USE m_antoffset
      USE m_eop_data
      USE m_eop_cor
      USE mdl_config
      use pod_yaml
      IMPLICIT NONE

!--------------------------------------------------------------------
      INTEGER (KIND = prec_int4) :: I, J, K, KK, II, JJ, INEW
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
       
      !CHARACTER (LEN=100) :: INFILENAME, OUTFILENAME
      CHARACTER (LEN=100) :: NEW,INPUT,OUTPUT
      CHARACTER (LEN=3)   :: SATTYPE
      INTEGER (KIND = prec_int4) :: UNIT_IN1,UNIT_IN2,ios
      INTEGER (KIND = prec_int4) :: VERSION
      INTEGER (KIND = prec_int4) :: LEAP, NWKUTC
      INTEGER (KIND = prec_int4) :: ISAT
      INTEGER (KIND = prec_int4) :: IYEAR4,MONTH,IDAY
      REAL (KIND = prec_d),DIMENSION(4)::IONALPHA, IONBETA
      REAL (KIND = prec_q) :: AVE(8),STD(8)
!      REAL (KIND = prec_d),DIMENSION(:,:,:),ALLOCATABLE ::EPH, CLK
!      REAL (KIND = prec_q) ::EPH(32,4000,220),CLK(32,4000,220)
      REAL (KIND = prec_q) ::EPH(32,5000,220),CLK(3,5000,220)
      REAL (KIND = prec_q) ::CLKNEW(3,5000,220),CLKNEW2(3,5000,220)
      REAL (KIND = prec_d),DIMENSION(:,:,:),ALLOCATABLE ::EPHNEW
      REAL (KIND = prec_d),DIMENSION(:,:,:),ALLOCATABLE ::EPHNEW2
      REAL (KIND = prec_d),DIMENSION(:,:,:),ALLOCATABLE ::ECEFPOS
      REAL (KIND = prec_d),DIMENSION(:,:,:),ALLOCATABLE ::SATCLK
!      REAL (KIND = prec_d),DIMENSION(:,:),ALLOCATABLE ::NEWEPOCH
      REAL (KIND = prec_d),DIMENSION(:,:),ALLOCATABLE ::NEWEPOCH2
      REAL (KIND = prec_d) :: A0UTC,A1UTC,ITUTC
      REAL (KIND = prec_d) :: CPU_t0, CPU_t1
      REAL (KIND = prec_d) :: DT, DA, DE, DI
      REAL (KIND = prec_d) :: DTGAL,XT, KKK
      
      INTEGER (KIND = prec_int4) :: MAXNSAT, MAXNPAR, MAXEPO
      INTEGER (KIND = prec_int4) :: sz1, sz2, sz3, IREC, NDT
      INTEGER (KIND = prec_int4) :: IG, IR, IE, IC, IJ, TOTG
      INTEGER (KIND = prec_int4) :: IGAL, IBDS, IQZSS, IGLN
!------------------------------------------------------------------
      REAL (KIND = prec_d) :: DELT, DELT2, DWEEK
      REAL (KIND = prec_d) :: XM021, DM0
      REAL (KIND = prec_d) :: M01, M02
      REAL (KIND = prec_q) :: MJD0, MJD
      REAL (KIND = prec_q) :: SEC00
      REAL (KIND = prec_q) :: MJD_ti
      REAL (KIND = prec_d) :: mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      REAL (KIND = prec_d) :: LEAPSEC
      REAL (KIND = prec_d)       :: GPS_day, GPS_wsec, GPS_wsec1
      INTEGER (KIND = prec_int8) :: GPS_week, GPSweek_mod1024
      INTEGER (KIND = prec_int4) :: PRINTID
      INTEGER (KIND = prec_int4) :: ANTOPT
      INTEGER (KIND = prec_int4) :: SAMPLE
      INTEGER (KIND = prec_d) :: IPRN(220),ISTR(220)
      INTEGER (KIND = prec_d),DIMENSION(:,:),ALLOCATABLE :: IBAD
!-----------------------------------------------------------------
      REAL (KIND = prec_q) :: CRS2TRS(3,3), TRS2CRS(3,3)
      REAL (KIND = prec_q) :: d_CRS2TRS(3,3), d_TRS2CRS(3,3)
      REAL (KIND = prec_d) :: EOP_cr(7)
      REAL (KIND = prec_q) :: r_TRS(3), r_CRS(3)
      CHARACTER (LEN=512)   :: EOP_fname
      INTEGER (KIND = prec_int4) :: EOP_Nint 
      INTEGER (KIND = prec_int1) :: EOP_sol
      REAL (KIND = prec_d) :: eop(7)
      INTEGER (KIND = prec_int2) :: iau_model
!----------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(3) :: rbody
      REAL (KIND = prec_q), DIMENSION(3) :: rSun
      DOUBLE PRECISION  JD, Zbody(6)
      INTEGER NTARG_SUN, NCTR
      CHARACTER (LEN=100) :: fname_header, fname_data, fname_out
!----------------------------------------------------------------
      CHARACTER (LEN=3), ALLOCATABLE :: PRNmatrix(:)
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE ::ORBmatrix, CLKmatrix
      INTEGER (KIND = prec_int4) :: Nepochs, Nsat
      INTEGER (KIND = prec_int4) :: i_sat, k_sat, i_write
      CHARACTER (LEN=3) :: PRN_ti
      REAL (KIND = prec_q) :: Sec_00


! ----------------------------------------------------------------

! If PRINTID = 1, print out the debugging.
PRINTID = 0

! Apply the anntenna offset w.r.t COM (Only for GLONASS whose broadcast position
! is in the phase center of transmitting antenna in a PZ-90 ECEF frame)
! ANTOPT = 1 : correct the antenna offset
! ANTOPT = 0 : no correction 
ANTOPT = 1
! START CPU COUNTER
! -----------------
CALL cpu_time (CPU_t0)

! LOCAL VARIABLES
! ---------------
MAXNSAT = 220 ! MAXIMUM NUMBER OF SATELLITE RECORD IN BRDC ORBIT
MAXNPAR = 32  ! MAXIMUM NUMBER OF BRDC ORBI PARAMETERS
IREC = 2      ! SAMPLING RATE OF GPS BROADCAST ORBIT
MAXEPO = 5000 ! MAXIMUM NUMBER OF EPOCHS WITHIN A FILE
SAMPLE = 900    ! SAMPLING RATE FOR SP3 OUTPUT 
ALLOCATE(EPHNEW(MAXNPAR,MAXEPO,MAXNSAT), STAT = AllocateStatus)
ALLOCATE(EPHNEW2(MAXNPAR,MAXEPO,MAXNSAT), STAT = AllocateStatus)
ALLOCATE(IBAD(MAXEPO,MAXNSAT), STAT = AllocateStatus)
ALLOCATE(ECEFPOS(86400/SAMPLE,3,MAXNSAT), STAT = AllocateStatus)
ALLOCATE(SATCLK(86400/SAMPLE,1,MAXNSAT), STAT = AllocateStatus)
!ALLOCATE(NEWEPOCH(86400/SAMPLE,MAXNSAT), STAT = AllocateStatus)
ALLOCATE(NEWEPOCH2(86400/SAMPLE,MAXNSAT), STAT = AllocateStatus)
SATCLK = 999999.999999d0
EPHNEW2 = 0.d0
EPHNEW  = 0.d0
ECEFPOS = 0.d0
NEWEPOCH2= 0.d0
IPRN= 0
ISTR= 0
CALL brdc_cmdline
INPUT = TRIM(INFILENAME)
UNIT_IN1 = 66
! ----------------------------------------------------------------------
! Open *.yyn broadcast ephemeris file
OPEN (UNIT = UNIT_IN1, FILE = INPUT, IOSTAT = ios)
IF (ios /= 0) THEN
! PRINT *, "Error in opening file:", PRMfname
PRINT *, "Error in opening file:", INFILENAME
PRINT *, "OPEN IOSTAT=", ios
END IF

! Create an output file
OUTPUT = TRIM(OUTFILENAME)

! Select the GNSS constellation to be processed
SATTYPE = TRIM(GNSSTYPE)

! Read broadcast ephemeris file 
!-----------------------------------------
CALL readbrdcheader (UNIT_IN1, VERSION, IONALPHA, IONBETA, A0UTC, A1UTC, &
                     ITUTC, NWKUTC, LEAP)
!PRINT*,'RINEX VERSION =',VERSION

CALL readbrdcmessg (UNIT_IN1,VERSION,MAXNSAT,MAXNPAR,MAXEPO,IYEAR4,MONTH,IDAY,ISAT,EPH,CLK)

CLOSE (UNIT=UNIT_IN1)
PRINT*,'ALL SATELLITES HAVE BEEN RECORD !'
PRINT*,''


! Create the reference epoch of the broadcast file
! ------------------------------------------------
CALL iau_CAL2JD ( IYEAR4, MONTH, IDAY, MJD0, MJD_ti, J )

! The seconds of the GPS week
! ----------------------------
CALL time_GPSweek (MJD_ti , GPS_week, GPS_wsec, GPSweek_mod1024)

! Read the leap second
! --------------------
CALL read_leapsec(leapsec_filename_cfg)

! Get EOP data
! EOP option setting
! EOP_sol = 1 : refers to EOP data format of the C04 solution
! EOP_sol = 2 : refers to EOP data format of the daily solution
! EOP_sol = 3 : refers to EOP data format of the ultra-rapid

EOP_sol = 1
EOP_Nint = 4
EOP_fname = EOP_fname_cfg
CALL eop_rd (EOP_fname, EOP_sol, MJD_ti , eop)

!Precession-Nutation model by International Astronomical Union (IAU)
!IAU = 2000 refers to the IAU 2000A model
!IAU = 2006 refers to the IAU 2006/2000A model
iau_model= IAU2006

! Planetary/Lunar Ephemeris data
! -------------------------------
fname_out = 'DE.430'
fname_header = 'header.430_229'
fname_data = 'ascp1950.430'
CALL CATfile (fname_header,fname_data,fname_out)
CALL asc2eph (fname_out)

! Assign a new array for each satellite with the epoch recounting (K value)
! and Detect the bad records in TOE by using an index of IBAD
! -------------------------------------------------------------------------

! Check all records (Here the daily brdc file is used, so the first epoch should
! be the UTC 0h.)
! ------------------------------------------------------------------------------
JJ = 0
KK = 0
K = 0
DO I=1, MAXNSAT

IF (I < 50 .OR. I < 150 .AND. I > 100 .OR. &
    I < 200 .AND. I > 150 .OR. I > 200) THEN

    DO  J =1,MAXEPO
        IF (EPH(3,J,I) .GT. 0.d0) THEN 
            K= K+1
            EPHNEW(1:20,K,I) = EPH(1:20,J,I)
            CLKNEW(1:3, K,I) = CLK(1:3, J,I)
            IF (K .EQ. 1) THEN 
               DT = 0.d0
               DO II=1, 7
               JJ = JJ + 1
               DELT = SAMPLE*(II-1) + EPHNEW(2,K,I)
!               NEWEPOCH(JJ,I) = DELT 
!print*,'PRN SAT', I, 'DELT =', DELT,  'JJ =', JJ
               END DO
            ELSE
               DT =  EPHNEW(2,K,I)-EPHNEW(2,K-1,I)
!print*,'PRN SAT = ', I
               IF (DT .LT. 0.d0) EXIT
               ! For GPS and BDS case
               IF (I < 50 .OR.  I < 200 .AND. I > 150) THEN
                  IF (DT .LT. 1800.d0) THEN
!                  PRINT*,'PREVIOUS EPOCH =',EPHNEW(2,K-1,I)
!                  PRINT*,'CURRENT  EPOCH =',EPHNEW(2,K,I)
!                  PRINT*,'BAD RECORD IN TIME TOE',DT, 'PRN SAT =', I , &
!                         'EPOCH NUMBER =', K
                  IBAD(K,I) = 1
                  ELSE 
                  IBAD(K,I) = 0
                  END IF
               ! For Galileo case
               ELSE IF (I < 150 .AND. I > 100 ) THEN
                  IF (DT .LT. 600.d0) THEN
!                  PRINT*,'PREVIOUS EPOCH =',EPHNEW(2,K-1,I)
!                  PRINT*,'CURRENT  EPOCH =',EPHNEW(2,K,I)
!                  PRINT*,'BAD RECORD IN TIME TOE',DT, 'PRN SAT =', I , &
!                         'EPOCH NUMBER =', K
                  IBAD(K,I) = 1
                  ELSE
                  IBAD(K,I) = 0
                  END IF
              ! For QZSS case
               ELSE IF (I > 200 ) THEN
                  IF (DT .LT. 800.d0) THEN
!                  PRINT*,'PREVIOUS EPOCH =',EPHNEW(2,K-1,I)
!                  PRINT*,'CURRENT  EPOCH =',EPHNEW(2,K,I)
!                  PRINT*,'BAD RECORD IN TIME TOE',DT, 'PRN SAT =', I , &
!                         'EPOCH NUMBER =', K
                  IBAD(K,I) = 1
                  ELSE
                  IBAD(K,I) = 0
                  END IF
               END IF
               
               DO II=1, NINT(DT/SAMPLE+1)-1
               JJ = JJ + 1
               DELT = SAMPLE*(II-1) + EPHNEW(2,K,I)
!               NEWEPOCH(JJ,I) = DELT 
!print*,'PRN SAT', I, 'DELT =', DELT,  'JJ =', JJ, 'II =',II, 'DT =', DT
               END DO
            END IF
              
        END IF
     
    END DO

END IF
K=0 
JJ = 0
KK = 0            
END DO

! GLONASS process
!----------------

DO I=1, MAXNSAT
K = 0
   IF(I > 50 .AND. I < 100)THEN
   DO J =1,MAXEPO
      IF (ABS(EPH(5,J,I)) >  0.d0) THEN
            K= K+1
            EPHNEW(1:16,K,I) = EPH(1:16,J,I)
!      PRINT*,'ISAT =',I, 'K =', K, 'X0 =', EPH(1,J,I)
      END IF
   END DO
   END IF
END DO



IG = 0 ! GPS
IR = 0 ! GLONASS
IE = 0 ! GALILEO
IC = 0 ! BDS
IJ = 0 ! QZSS
! Count how many satellites are recorded
! --------------------------------------
DO I = 1,MAXNSAT 
   IF (I < 50 .AND. EPHNEW(3,1,I) > 0.d0) THEN
        IG = IG + 1 
        IPRN(I) = I ! used for SP3 orbital information
   ELSE IF (I < 100 .AND. I > 50 .AND. ABS(EPHNEW(5,1,I)) > 0.d0) THEN
       IR = IR + 1
       IPRN(I) = I
   ELSE IF (I < 150 .AND. I > 100 .AND. EPHNEW(3,1,I) > 0.d0) THEN
       IE = IE + 1
       IPRN(I) = I
   ELSE IF (I < 200 .AND. I > 150 .AND. EPHNEW(3,1,I) > 0.d0) THEN
       IC = IC + 1
       IPRN(I) = I
   ELSE IF (I < 250 .AND. I > 200 .AND. EPHNEW(3,1,I) > 0.d0) THEN
       IJ = IJ + 1
       IPRN(I) = I
   END IF
END DO

TOTG = 1 ! dummy initialise

! Process the desire GNSS constellation type
! ------------------------------------------
IF (SATTYPE == 'G')THEN 
TOTG = IG
DO I =1, MAXNSAT
   IF (I > 50) THEN
       EPHNEW(:,:,I) = 0.d0
       IPRN(I) = 0
   END IF
END DO

ELSE IF (SATTYPE == 'R') THEN 
TOTG = IR
DO I =1, MAXNSAT
   IF (I <= 50 .OR. I > 100) THEN
       EPHNEW(:,:,I) = 0.d0
       IPRN(I) = 0
   END IF
END DO

ELSE IF (SATTYPE == 'E') THEN 
TOTG = IE
DO I =1, MAXNSAT
   IF (I <= 100 .OR. I > 150) THEN
       EPHNEW(:,:,I) = 0.d0
       IPRN(I) = 0
   END IF
END DO

ELSE IF (SATTYPE == 'C') THEN 
TOTG = IC
DO I =1, MAXNSAT
   IF (I <= 150 .OR. I > 200) THEN
       EPHNEW(:,:,I) = 0.d0
       IPRN(I) = 0
   END IF
END DO

ELSE IF (SATTYPE == 'J') THEN 
TOTG = IJ
DO I =1, MAXNSAT
   IF (I <= 200 ) THEN 
       EPHNEW(:,:,I) = 0.d0
       IPRN(I) = 0
   END IF
END DO

ELSE IF (SATTYPE == 'A') THEN
TOTG = IG + IR + IE + IC + IJ
END IF

! Remove the bad records and convert the broadcast elements to ECEF coordinate
! system 
! -----------------------------------------------------------------------------
JJ=0

DO I = 1, MAXNSAT
INEW = 0
IGLN = 0
IGAL = 0
IBDS = 0
IQZSS = 0
KKK = 0
IF (I < 50 .AND. EPHNEW(3,1,I) > 0.d0 ) THEN ! GPS/GNSS sampling rate: 2 hour
!IF(I == 4) CYCLE ! The PRN04 has been decommissioned. Once the new generation
                 ! statellite is activiated with this PRN number, then this
                 ! statement shall be switched off.
!CALL brdc_qc_gps(EPHNEW(3,1:15,I),  AVE(1), STD(1))
!CALL brdc_qc_gps(EPHNEW(4,1:15,I),  AVE(2), STD(2))
!CALL brdc_qc_gps(EPHNEW(5,1:15,I),  AVE(3), STD(3))
!CALL brdc_qc_gps(EPHNEW(6,1:15,I),  AVE(4), STD(4))
!CALL brdc_qc_gps(EPHNEW(7,1:15,I),  AVE(5), STD(5))
!CALL brdc_qc_gps(EPHNEW(8,1:15,I),  AVE(6), STD(6))
!CALL brdc_qc_gps(EPHNEW(9,1:15,I),  AVE(7), STD(7))
!CALL brdc_qc_gps(EPHNEW(10,1:15,I), AVE(8), STD(8))

   DO K = 1, 15
         IF (IBAD(K,I) /= 1 )THEN
            INEW = INEW + 1
            EPHNEW2(1:20,INEW,I) = EPHNEW(1:20,K,I)
            CLKNEW2(1:3,INEW,I)  = CLKNEW(1:3,K,I)

!            CALL chkbrdc (I,EPHNEW2(1:20,INEW,I),AVE,STD)

            IF (INEW == 1 .AND. EPHNEW2(2,1,I) >= GPS_wsec) THEN
            DO II=1,8 ! 15-minute resolution 
               JJ = JJ + 1
               DELT2 = SAMPLE*(II-1) + GPS_wsec
               NEWEPOCH2(JJ,I) = DELT2
               CALL brdc2ecef(DELT2,EPHNEW2(1:20,INEW,I),CLKNEW2(:,INEW,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,INEW,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

            END DO
           
            ELSE IF (EPHNEW2(2,INEW,I) == GPS_wsec+7200*(INEW-1)) THEN

                 DT = EPHNEW2(2,INEW,I) - GPS_wsec 
                 IF (DT .LT. 0.d0) EXIT
                 IF (NINT(DT) - NINT(KKK) > 7200.d0) THEN
                 NDT =  (NINT(DT) - NINT(KKK))/SAMPLE
                 IF (PRINTID == 1) &
                 PRINT*,'MISSING EPOCHS',NDT,' AND INVALID EPOCHS',NDT-16 
                 DO II=1, NDT-16 ! Identify the invalid zone by remvoing valid
                                 ! zones from the previous (8 poiints forward) 
                                 ! and current (8 points backward)reference epoch
                 JJ = JJ + 1
                 DELT2 = SAMPLE*(II-1) + NINT(KKK)+ GPS_wsec + 7200
                 NEWEPOCH2(JJ,I) = DELT2
                 ECEFPOS(JJ,1:3,I) = 0.d0
                 SATCLK(JJ,1,I)    = 999999.999999d0 
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE= NO REFERENCE EPOCH        ', &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)
                 END DO      
           
                 DO II=1,8
                 JJ = JJ + 1
                 DELT2 = SAMPLE*(II-1) + NINT(KKK)+ GPS_wsec + 7200*(NDT/8-1) 
                 NEWEPOCH2(JJ,I) = DELT2
                 CALL brdc2ecef(DELT2,EPHNEW2(1:20,INEW,I),CLKNEW2(:,INEW,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,INEW,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

                 END DO
                 END IF
                 KKK = DT
 
            DO II = 1, 8
               JJ = JJ + 1
               DELT2 = SAMPLE*(II-1) + NINT(DT)+ GPS_wsec
               IF (DELT2 > EPHNEW2(2,1,I)+86400-900 ) EXIT
               NEWEPOCH2(JJ,I) = DELT2
               CALL brdc2ecef(DELT2,EPHNEW2(1:20,INEW,I),CLKNEW2(:,INEW,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,INEW,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

            END DO
         
            END IF
         END IF
   END DO

ELSE IF(I > 50 .AND. I < 100 .AND. ABS(EPHNEW(5,1,I)) > 0.d0) THEN ! GLONASS sampling rate: 30 minutes

   DO K = 1, 48
   IGLN = IGLN +1
   EPHNEW2(1:16,IGLN,I) = EPHNEW(1:16,K,I)
!print*,'EPHNEW2(1,IGLN,I) =', EPHNEW2(1,IGLN,I)
   CALL time_UTC(EPHNEW2(1,IGLN,I), mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
   LEAPSEC = (mjd_GPS - EPHNEW2(1,IGLN,I) )*86400.d0
!PRINT*,'LEAPSEC =', LEAPSEC
   EPHNEW2(1,IGLN,I) = mjd_GPS ! convert UTC time to GPS time
!PRINT*,'GPS TIME =', EPHNEW2(1,IGLN,I)
   CALL time_GPSweek (mjd_GPS , GPS_week, GPS_wsec1, GPSweek_mod1024)
!PRINT*,'t0 =',GPS_wsec1,'t =', GPS_wsec, 't - t0 =',GPS_wsec-GPS_wsec1
!PRINT*,'A0, A1 =',EPHNEW2(2:3,IGLN,I)
! ICRF-ITRF transformation matrix (including derivatives) based on EOP data
CALL crs_trs (mjd_TT, eop, iau_model, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)

      IF (IGLN == 1) THEN
      DO II = 1,2
      JJ = JJ + 1
      DELT2 = SAMPLE*(II-1) + GPS_wsec ! satellite positions at the specific epoch
      NEWEPOCH2(JJ,I) = DELT2
!PRINT*,'IGLN=, TIME DIFF=', IGLN, DELT2-GPS_wsec1,'A0, A1 =',EPHNEW2(2:3,IGLN,I)
!PRINT*,'SATCLK=', 1.d6*(EPHNEW2(2,IGLN,I)+EPHNEW2(3,IGLN,I)*(DELT2-GPS_wsec1))
      CALL glnorbint(EPHNEW2(1:16,IGLN,I),DELT2,GPS_wsec1,ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))

      IF (ANTOPT == 1) THEN
      r_TRS(1:3) = ECEFPOS(JJ,1:3,I)
      ! ITRF to ICRF
      ! r_CRS = TRS2CRS * r_TRS
      CALL matrix_Rr (TRS2CRS, r_TRS, r_CRS)
      CALL antoffset (mjd_TT, r_CRS, ECEFPOS(JJ,1:3,I))
      CALL matrix_Rr (CRS2TRS, ECEFPOS(JJ,1:3,I), r_TRS)
      ECEFPOS(JJ,1:3,I) = r_TRS
      END IF
IF (PRINTID == 1) &
PRINT*,'NEW EPOCH =',JJ,'PRN SAT =',I,'DELT2 =',DELT2,'REF EPOCH =',GPS_wsec1,&
       'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

      END DO

      ELSE IF (IGLN >= 2) THEN
      DO II = 1,2
      JJ = JJ + 1
      DELT2 = SAMPLE*(II-1) + GPS_wsec + 1800*(K-1)
      NEWEPOCH2(JJ,I) = DELT2
!PRINT*,'IGLN, TIME DIFF=', IGLN, DELT2-GPS_wsec1,'A0, A1 =',EPHNEW2(2:3,IGLN,I)
!PRINT*,'SATCLK=', 1.d6*(EPHNEW2(2,IGLN,I)+EPHNEW2(3,IGLN,I)*(DELT2-GPS_wsec1))
      CALL glnorbint(EPHNEW2(1:16,IGLN,I),DELT2,GPS_wsec1,ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
      IF (ANTOPT == 1) THEN
      r_TRS(1:3) = ECEFPOS(JJ,1:3,I)
      ! ITRF to ICRF
      ! r_CRS = TRS2CRS * r_TRS
      CALL matrix_Rr (TRS2CRS, r_TRS, r_CRS)
      CALL antoffset (mjd_TT, r_CRS, ECEFPOS(JJ,1:3,I))
      CALL matrix_Rr (CRS2TRS, ECEFPOS(JJ,1:3,I), r_TRS)
      ECEFPOS(JJ,1:3,I) = r_TRS
      END IF
IF (PRINTID == 1) &
PRINT*,'NEW EPOCH =',JJ,'PRN SAT =',I,'DELT2 =',DELT2,'REF EPOCH =',GPS_wsec1,&
       'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)
      
      END DO

      ELSE IF (K == 48) THEN
      JJ = JJ + 1
      DELT2 = SAMPLE*(II-1) + GPS_wsec + 1800*(K-1)
      NEWEPOCH2(JJ,I) = DELT2
!PRINT*,'IGLN, TIME DIFF=', DELT2-GPS_wsec1,'A0, A1 =',EPHNEW2(2:3,IGLN,I)
!PRINT*,'SATCLK=', 1.d6*(EPHNEW2(2,IGLN,I)+EPHNEW2(3,IGLN,I)*(DELT2-GPS_wsec1))
      CALL glnorbint(EPHNEW2(1:16,IGLN,I),DELT2,GPS_wsec1,ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
      IF (ANTOPT == 1) THEN
      r_TRS(1:3) = ECEFPOS(JJ,1:3,I)
      ! ITRF to ICRF
      ! r_CRS = TRS2CRS * r_TRS
      CALL matrix_Rr (TRS2CRS, r_TRS, r_CRS)
      CALL antoffset (mjd_TT, r_CRS, ECEFPOS(JJ,1:3,I))
      CALL matrix_Rr (CRS2TRS, ECEFPOS(JJ,1:3,I), r_TRS)
      ECEFPOS(JJ,1:3,I) = r_TRS
      END IF
IF (PRINTID == 1) &
PRINT*,'NEW EPOCH =',JJ,'PRN SAT =',I,'DELT2 =',DELT2,'REF EPOCH =',GPS_wsec1,&
       'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

      END IF
   END DO

ELSE IF (I < 150 .AND. I > 100 .AND. EPHNEW(3,1,I) > 0.d0) THEN ! GALILEO sampling rate: 10 minutes
!CALL brdc_qc_gal(EPHNEW(3,1:150,I),  AVE(1), STD(1))
!CALL brdc_qc_gal(EPHNEW(4,1:150,I),  AVE(2), STD(2))
!CALL brdc_qc_gal(EPHNEW(5,1:150,I),  AVE(3), STD(3))
!CALL brdc_qc_gal(EPHNEW(6,1:150,I),  AVE(4), STD(4))
!CALL brdc_qc_gal(EPHNEW(7,1:150,I),  AVE(5), STD(5))
!CALL brdc_qc_gal(EPHNEW(8,1:150,I),  AVE(6), STD(6))
!CALL brdc_qc_gal(EPHNEW(9,1:150,I),  AVE(7), STD(7))
!CALL brdc_qc_gal(EPHNEW(10,1:150,I), AVE(8), STD(8))
!print*,'AVE =', AVE
!print*,'STD =', STD
   DO K = 1, 150
         IF (IBAD(K,I) /= 1 )THEN
            IGAL = IGAL +1
            EPHNEW2(1:20,IGAL,I) = EPHNEW(1:20,K,I)
            CLKNEW2(1:3, IGAL,I) = CLKNEW(1:3, K,I)
!            CALL chkbrdc (I,EPHNEW2(1:20,IGAL,I),AVE,STD)
            IF (IGAL == 1 .AND. EPHNEW2(2,1,I) >= GPS_wsec) THEN
               DO II=1,2
                  JJ = JJ + 1
                  DELT2 = SAMPLE*(II-1) + GPS_wsec
                  NEWEPOCH2(JJ,I) = DELT2
                  CALL brdc2ecef(DELT2,EPHNEW2(1:20,IGAL,I),CLKNEW2(:,IGAL,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IGAL,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

               END DO
            ELSE IF (IGAL > 1 .AND. EPHNEW2(2,IGAL,I)-EPHNEW2(2,IGAL-1,I) == 600.d0 &
                     .AND.  MOD(EPHNEW2(2,IGAL,I),1800.d0) == 0) THEN 
               DT = EPHNEW2(2,IGAL,I) - GPS_wsec 
!PRINT*,'DT =', NINT(DT), NINT(KKK), IGAL

               IF (NINT(DT)-NINT(KKK) > 1800.d0)THEN
               NDT =  (NINT(DT) - NINT(KKK))/SAMPLE
               IF (PRINTID == 1) &
               PRINT*,'MISSING EPOCHS',NDT,' AND INVALID EPOCHS',NDT-2
                       DO II = 1, NDT-2 
                          JJ = JJ + 1
                       DELT2 = SAMPLE*(II-1) + NINT(KKK) + GPS_wsec + 1800 !7200  
                       NEWEPOCH2(JJ,I) = DELT2
                       ECEFPOS(JJ,1:3,I) = 0.d0
                       SATCLK(JJ,1,I)    = 999999.999999d0
! Because the sampling rate of galileo reference epoch is only 10 minutes, this can be switched on to compensate the gap of the reference epoch.
                       CALL brdc2ecef(DELT2,EPHNEW2(1:20,IGAL,I),CLKNEW2(:,IGAL,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IGAL,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

                      END DO
               END IF
               KKK = DT
               DO II = 1, 2 
                  JJ = JJ + 1
                  DELT2 = SAMPLE*(II-1) +  NINT(DT) + GPS_wsec 
                  NEWEPOCH2(JJ,I) = DELT2
                  CALL brdc2ecef(DELT2,EPHNEW2(1:20,IGAL,I),CLKNEW2(:,IGAL,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IGAL,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

               END DO

            END IF 
         END IF
   END DO

ELSE IF (I < 200 .AND. I > 150 .AND. EPHNEW(3,1,I) > 0.d0) THEN ! BDS sampling rate: 1 hour

   DO K = 1, 24
         IF (IBAD(K,I) /= 1 )THEN
            IBDS = IBDS +1
            EPHNEW2(1:20,IBDS,I) = EPHNEW(1:20,K,I)
            CLKNEW2(1:3, IBDS,I) = CLKNEW(1:3, K,I)
            IF (IBDS == 1 .AND. EPHNEW2(2,1,I) >= GPS_wsec) THEN
               DO II=1,8
                  JJ = JJ + 1
                  DELT2 = SAMPLE*(II-1) + GPS_wsec
                  NEWEPOCH2(JJ,I) = DELT2
                  CALL brdc2ecef(DELT2,EPHNEW2(1:20,IBDS,I),CLKNEW2(:,IBDS,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IBDS,I), &
       'DELT2 =', DELT2, 'POSITIONS=',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

               END DO
            ELSE IF (IBDS > 1 .AND. EPHNEW2(2,IBDS,I)-EPHNEW2(2,IBDS-1,I) == 3600.d0 &
                     .AND.  MOD(EPHNEW2(2,IBDS,I),7200.d0) == 0) THEN
               DT = EPHNEW2(2,IBDS,I) - GPS_wsec
!PRINT*,'DT =', NINT(DT), NINT(KKK)
               IF(NINT(DT)-NINT(KKK) > 7200.d0) THEN
!               print*,'ADD POINTS FOR INTERPOLATION'
               NDT =  (NINT(DT) - NINT(KKK))/SAMPLE
                       DO II = 1, NDT-8
                          JJ = JJ + 1
                       DELT2 = SAMPLE*(II-1) + NINT(KKK) + GPS_wsec + 7200
                       NEWEPOCH2(JJ,I) = DELT2
                       CALL brdc2ecef(DELT2,EPHNEW2(1:20,IBDS,I),CLKNEW2(:,IBDS,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IBDS,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)
                       END DO

               END IF
               KKK = DT
               DO II = 1, 8
                  JJ = JJ + 1
                  DELT2 = SAMPLE*(II-1) + NINT(DT) + GPS_wsec 
                  NEWEPOCH2(JJ,I) = DELT2
                  CALL brdc2ecef(DELT2,EPHNEW2(1:20,IBDS,I),CLKNEW2(:,IBDS,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IBDS,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

               END DO
            END IF
         END IF
   END DO

ELSE IF (I > 200 .AND. EPHNEW(3,1,I) > 0.d0) THEN ! QZSS sampling rate: 15 minutes

   DO K = 1, 100
         IF (IBAD(K,I) /= 1 )THEN
            IQZSS = IQZSS +1
            EPHNEW2(1:20,IQZSS,I) = EPHNEW(1:20,K,I)
            CLKNEW2(1:3, IQZSS,I) = CLKNEW(1:3, K,I)
            IF (IQZSS == 1 .AND. EPHNEW2(2,1,I) >= GPS_wsec)  THEN
               DO II=1,8
                  JJ = JJ + 1
                  DELT2 = SAMPLE*(II-1) + GPS_wsec 
                  NEWEPOCH2(JJ,I) = DELT2
                  CALL brdc2ecef(DELT2,EPHNEW2(1:20,IQZSS,I),CLKNEW2(:,IQZSS,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IQZSS,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

               END DO
             
            ELSE IF (IQZSS > 1 .AND. EPHNEW2(2,IQZSS,I)-EPHNEW2(2,IQZSS-1,I) >=500.d0 &
                     .AND.  MOD(EPHNEW2(2,IQZSS,I),7200.d0) == 0) THEN
               DT = EPHNEW2(2,IQZSS,I) - GPS_wsec
!PRINT*,'DT =', NINT(DT), NINT(KKK)
               IF(NINT(DT)-NINT(KKK) > 7200.d0) THEN
!               print*,'ADD POINTS FOR INTERPOLATION'
               NDT = (NINT(DT) - NINT(KKK))/SAMPLE
                       DO II = 1, NDT-8
                          JJ = JJ + 1
                       DELT2 = SAMPLE*(II-1) +  NINT(KKK) + GPS_wsec + 7200
                       NEWEPOCH2(JJ,I) = DELT2
                       CALL brdc2ecef(DELT2,EPHNEW2(1:20,IQZSS,I),CLKNEW2(:,IQZSS,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IQZSS,I), &
       'DELT2 =', DELT2,'POSITIONS =',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

                       END DO
               END IF
               KKK = DT
               
               DO II = 1, 8 
                  JJ = JJ + 1
                  DELT2 = SAMPLE*(II-1) +  NINT(DT) + GPS_wsec 
                  NEWEPOCH2(JJ,I) = DELT2
                  CALL brdc2ecef(DELT2,EPHNEW2(1:20,IQZSS,I),CLKNEW2(:,IQZSS,I),ECEFPOS(JJ,1:3,I),SATCLK(JJ,1,I))
IF (PRINTID == 1) &
print*,'NEW EPOCH =', JJ, 'PRN SAT =', I,'TOE=',EPHNEW2(2,IQZSS,I), &
       'DELT2 =', DELT2, 'POSITIONS=',ECEFPOS(JJ,1:3,I),'SATCLK=',SATCLK(JJ,1,I)

               END DO
            END IF
         END IF
   END DO

END IF ! GPS/GNSS
  
INEW = 0
JJ = 0
END DO



! Re-arrange the matrixes in preparation for the generation of SP3 file
!----------------------------------------------------------------------
Nepochs = SIZE(ECEFPOS,DIM=1)
Nsat    = SIZE(ECEFPOS,DIM=3)
ALLOCATE(CLKmatrix(Nepochs,3,TOTG), STAT = AllocateStatus)
ALLOCATE(ORBmatrix(Nepochs,8,TOTG), STAT = AllocateStatus)
ALLOCATE(PRNmatrix(TOTG), STAT = AllocateStatus)
CLKmatrix = 999999.999999d0 
ORBmatrix = 0.d0
k_sat = 0
DO  i_sat = 1 , Nsat
IF(i_sat == IPRN(i_sat))THEN
k_sat = k_sat +1
CALL prn2str (i_sat, PRN_ti)
PRNmatrix(k_sat) = PRN_ti

    DO  i_write = 1 , Nepochs
    Sec_00 = 900*(i_write-1)
    IF(i_write == 1) THEN
    ORBmatrix(i_write,1, k_sat) = MJD_ti
    ORBmatrix(i_write,2, k_sat) = Sec_00
    CLKmatrix(i_write,1, k_sat) = MJD_ti
    CLKmatrix(i_write,2, k_sat) = Sec_00
    ELSE
    ORBmatrix(i_write,1, k_sat) = MJD_ti + Sec_00/86400.d0
    ORBmatrix(i_write,2, k_sat) = Sec_00
    CLKmatrix(i_write,1, k_sat) = MJD_ti + Sec_00/86400.d0
    CLKmatrix(i_write,2, k_sat) = Sec_00
    END IF
    ORBmatrix(i_write,3:5, k_sat) = ECEFPOS(i_write,1:3,i_sat)
    ORBmatrix(i_write,6:8, k_sat) = 0.d0
    CLKmatrix(i_write,3,   k_sat) = SATCLK(i_write,1,i_sat)
    END DO
END IF
END DO
yml_pod_mode = MODE_FIT

! WRITE THE ECEF POSITIONS IN A SP3 FORMAT
! ----------------------------------------
!CALL write_brd2sp3 (ISTR,IPRN,TOTG,IG,IR,IE,IC,IJ,SATTYPE, SAMPLE,IYEAR4,MONTH,IDAY,NEWEPOCH2, ECEFPOS, OUTPUT, 0)
CALL write_orb2sp3(ORBmatrix, PRNmatrix, OUTPUT, .false., CLKmatrix)


CALL cpu_time (CPU_t1)
PRINT *,"CPU Time (sec)", CPU_t1-CPU_t0


END PROGRAM 
