C*************************************************************** ECLIPS
C
      SUBROUTINE ECLIPS201707 ( IDIR, IPRN, TTAG, SVBCOS, ANOON, ANIGHT,
     &                    NECLIPS, ECLSTM, ECLETM, IECLIPS, PI ,
C Jan 10, 2017
C    &                    xsv, santxyz, vsvc, beta, iblk)
     &                    xsv, santxyz, vsvc, beta, iblk, BETAINI, 
C July 2017
     &                    yaw_angle) 

C Jan 16, 2015
C *******************WARNING************************************
C the IIA body-x orientation (+x=> Sun) is used for all satellites, including
C IIR/IIRM. For the hardware IIR/IIRM orientation (+x=> deep space), the
C eclipsIIR.f version should be used or implemented
C
C NOTE: the IIA body-x orientation (+x=> Sun) IS NOW THE IGS STANDARD
C FOR ALL SATELLITES AND GNSS' !!!
C ************************************************************* 
C
C       NAME            ECLIPS (version Feb  2017)
C
C     PURPOSE   DETECT ECLIPSING & YAW ROTATE ECLIP. SATELLITES
C                       (THE INPUT BODY-X UNIT VECTOR - SANTXYZ IS YAW-ROTATED
C                        BY PHI-YANGLE (THE ECL-NOMINAL) YAW ANGLE DIFFERENCE)  
C
C       COPYRIGHT       GEODETIC SURVEY DIVISION, 2011.
C                       ALL RIGHTS RESERVED.
C                       ALL TERMS AND CONDITIONS APPLY AS DETAILED IN
C                       " TERMS AND CONDITIONS FOR SOFTWARE " 
C
C       CONTACT         kouba@rogers.com      
C
C       UPDATE HISTORY: Aug. 23, 2011:1996-2008 W. AVERAGES of JPL REPROCESSING
C                                YRATE SOLUTIONS FOR ALL II/IIA CODED IN DATA
C                                STATEMENT, THIS ENABLES REPROCESSING FROM 1996
C                       Sep 26, 2011: 1. Corrected bug causing Block IIF shadow 
C                               CROSSING MANEVURE WITH 0.06 DEG/SEC RATE EVEN 
C                               FOR BETADG > 8 DEG
C                                     2. CORRECTED/improved IIA RECOVERY logic
C                       Dec 18, 2013: 1.corrected IIF night turns (USAF Doc.)
C                                     2.small neg beta IIF and small pos IIA noon turn
C                                    (wrong) directions  for |beta| < 0.9deg
C                                     3. PRN/SVN 23 IIA YBIAS= -0.5 deg up to Feb 2004 
C                                     4. All the above changes labeled "C Dec 12, 2013"
C
C                      Jan 24, 2014:  NOON RECOVERY CORRECTED IF IIA/IIF's HAVE YBIAS=0
C                                    (NOT APPLICABLE CURRENTLY,POSSIBLE FOR FUTURE IIF?)
C                                    & SMALL IIA/IIF BETA IF STATEMENT SIMPLIFIED
C
C                      Jan 16, 2015: the IIA body-x orientation
C                                    (+x=>Sun) and logic is implemented also for
C                                    IIR/IIRM eclipsing (no SANTXYZ reversal )  
C
C                      Mar 09, 2016 - upper limits for GLONASS PRN's -64
C                      
C                      Jan 10, 2017  BEIDOU (PRN: 101-136) ECLIPSING
C                                    GEO(BLK 23, 27) - ON (Orbit normal) YAW
C                                    IGEO(22,26)&MEO(21,25)-ON YAW FOR |BETA|<2DEG
C
C                      Jan 10, 2017 : GALILEO ECLIPSING ACCORDING TO:
C(https://www.gsc-europa.eu/support-to-developers/galileo-iov-satellite-metadata)
C                NOTE: SETTING BETAy=>BETA0=0deg DISABLES (ONLY) GAL/BEI ECLIPSING!
C                ANIGHT = 180+15.0 deg IS SET FOR THE GAL BETAx = 15 DEG
C
C                      Feb 27, 2017 :GPS, GLN, GAL- A POSSIBLE BETA SIGN
C                                    CHANGE DURING NOON OR SHADOW TURNS
C                                    BETAINI- TURN START BETA ANGLE SET WHEN
C                                    |BETA|=< 0.07 DEG (a beta sign change
C                                    possible), OTHERWISE, BETAINI(IPRN) = 0 .
C                                    THEN, WHEN |BETA| =< 0.07 DEG AND
C                                    BETAINI(IPRN).NE.0, BETAINI IS USED
C                                    FOR THE TURN, OTHERWISE BETA IS USED 
C      
C                                    ALSO: IIF YAW BIAS SET TO -0.7 DEG
C                                    AS RECOMMENDED BY KUANG AT AL (2016)
C                                    GPS Solut 
C                                    DOI 10.1007/s10291-016-0562-9
C
C                                    AND ALL GPS IIA WITH BETA (0,+0.5]
C                                    HAVE NOON TURNS IN WRONG DIRECTION
C                                    DUE TO THE YAW BIAS OF +0.5 DEG
C
C     PARAMETERS        DESCRIPTION
C
C        IDIR           DIRECTION OF PROCESSING (1=FORWARD, -1=BACKWARD)
C        IPRN           SV PRN NUMBER (.LE.32 FOR GPS, .GT.32 FOR GLONASS)
C        TTAG           OBSERVATION EPOCH TIME TAG (EG SEC OF GPS WEEK)
C        SVBCOS         SV BETA ANGLE (BETWEEN SV AND SUN RADIUS) COSINE
C        ANOON          SV BETA ANGLE LIMIT (DEG) FOR A NOON TURN MANEVURE 
C        ANIGHT         SV BETA ANGLE LIMIT (DEG) FOR A NIGHT SHADOW CROSSING
C        NECLIPS        NUMBER OF ECLIPSING FOR THE PRN SATELLITE 
C        ECLSTM         ECLIPSE START TIME(EG SEC OF GPS WEEK)
C        ECLETM         ECLIPSE END TIME  ( "         "      )
C        IECLIPS        SV ECLIPSING (0=NO,1, 2=YES(1=night, 2=noon))
C        PI             = PI=3.1415926536D0
C        XSV(3)         SV X, Y, Z (m)(ITRF)
C        SANTXYZ        BODY-X UNIT VECTOR (ITRF)
C                       WARNING: THE IIA BODY-X ORIENTATION EXPECTED FOR THE IIR
C                        THE  BODY-X REVERSED FOR IIR (SEE BELOW) & RETURNED
C        VSVC           SV INERTIAL VELOCIRY VECTOR IN ITRF
C        BETA           90 DEG + THE SUN ANGLE WITH ORBITAL PLANE(IN RAD)
C        IBLK           SV BLOCK  1=I, 2=II, 3=IIA, IIR=(4, 5) IIF=6
C Jan 10, 2017
C                                BEI MEO= 21, 25: IGEO= 22, 26: GEO= 23, 27
C        BETAINI        INITIAL (GPS, GLN, GAL) BETA AT TURN START ONLY WHEN
C                       |BETA| =< 0.07 DEG. (MUST BE EXTERNALLY INITILIZED
C                       TO ZEROS!)
C
C        INTERNAL PARAMETRS DESCRIPTION
C Dec 12, 2013
C        YBIAS       IIA YAW BIAS= 0.5 deg SINCE NOV95,EXCEPT PRN/SVN23?
C Feb 27, 2017
C                        (ALSO USED AS BETA LIMIT FOR IIA WRONG DIR NOON
C                        TURNS)
C                    IIF YAW BIAS=-0.7 deg (USED FOR BETA LIMIT 
C                        OF WRONG DIRECTION NOON TURNS)
C                        YAWBIAS= 0 DISABLES SMALL NEG BETA IIF TURNS!
C        YANGLE         THE NOMINAL YAW ANGLE
C        PHI            THE ECLIPSING YAW ANGLE            
C Jan 10, 2017
C        BETA0       GAL, BEIDOU HARD CODED ECLIPS BETA LIMIT (= 2 DEG)
C                    = 0 DISABLES GAL; BEI MEO(BLK 21, 25) AND
C                    BEI IGEO(BLK 22, 26) ECLIPSING 
C                    (BEI GEO(BLK 23, 27) ALWAYS USE THE ORB NORM YAW PHI=0)
C        BETAE       THE GAL MODEL SHy LIMIT
C
c REMARKS:
c SVBCOS, the COS of the angle between the sv radius vector and the sun 
c radius vector, (the dot product of the above respective unit vectors), 
c SVBCOS is used to test against CNOON=COS(ANOON), if sv is entering 
c a noon maneuverer (i.e., SVBCOS > CNOON). The ANOON limit 
c (typically < 5.7 deg), is determined within the subroutine and depends 
c on the  sv yaw rate (YRATE)and GNSS (GPS or GLONASS). 
c
c ANIGHT , the shadow limit is the "input " in the subroutine 
c call statement, it can be hard coded as it is constant for a GNSS type,
c e.g., 
c 180.D0+-13.25D0 for GPS (IPRN [1, 32]) and 180.D0+-14.20D0 for 
c GLONASS (IPRN [33,64]), resp. CNIGHT=COS(ANIGHT) is used for testing.
c When SVBCOS < CNIGHT (CNIGHT is negative and close to -1), 
c the SV enters, or it is in the shadow. 
C
C Jan 10, 2017
C FOR GAL (IPRN [65,100])  ANIGHT SET INTERNALLY TO 180 + 15 DEG
C         AND ANOON TO 15 DEG TO APPROXIMATE THE BETAx TURN LIMIT
C Feb 27, 2017
C FOR BEIDOU (IPRN [101, 136]) 180 + 2 DEG (USED FOR ECLIPS REPORT ONLY)
c
C *********************************************************************


C ----------------------------------------------------------------------
C Modified Version of ECLIPS.f
C ----------------------------------------------------------------------
C  Dr. Thomas Papanikolaou
C  Cooperative Research Centre for Spatial Information, Australia
C  July 2017
C ----------------------------------------------------------------------
C The current version has been modified accordingly to the modifications 
C implemented in the ECLIPS (V. Dec 2013) that has been used by 
C Papanikolaou and Melachroinos (2016). 
C The modifications impact is given as follows:
C - GPS IIA night turn and shadow exit recovery 
C - GPS IIR night turn
C - GPS noon turn
C ----------------------------------------------------------------------


      IMPLICIT NONE
C
C     MAXSAT - MAXIMUM NUMBER OF SATELLITES, CHANGED IF REQUIRED
C      
      INTEGER MAXSAT
C Jan 10, 2017
C     PARAMETER (MAXSAT=64)
      PARAMETER (MAXSAT=136)
      REAL*8    BETAINI(*)
C
      INTEGER*4 IDIR, IPRN
      INTEGER*4 IECLIPS, NECLIPS(*)
C
      REAL*8    TTAG, TWOHR, HALFHR
      REAL*8    SVBCOS, PI
      REAL*8    ECLSTM(MAXSAT,*)
      REAL*8    ECLETM(MAXSAT,*)
      REAL*8    ANOON, ANIGHT
      REAL*8    CNOON, CNIGHT
      REAL*8    DTR, DTTAG
      REAL*8    XSV(*), SANTXYZ(*), VSVC(*), BETA, MURATE, YANGLE, DET,
C Jan 10, 2017
C    &          YRATE(64), BETADG, PHI, SANTX, SANTY,        v(3),r(3)
     &          YRATE(136),BETADG, PHI, SANTX, SANTY, v(3),r(3), BETA0,
C    &          SMTH, BETAE
     &          BETAE
      REAL*8    YAWEND
C Dec 12, 2013
      REAL*8 YBIAS
      INTEGER*4 IBLK(*), J, I
C
      LOGICAL   NOON, NIGHT
	  
C 22 March 2017
      REAL*8 yaw_angle(2)
	  
	  
C MAX YAW RATES OF CURRENT&PAST BLOCK II/IIA's,(AVER'D 1996-2008 JPL SOLUTIONS)  
C CHANGE IF REQUIRED OR INPUT IF ESTIMATED 
C PRN                 01     02      03      04     05       06     07
      DATA YRATE /.1211d0,.1339d0,.123d0,.1233d0,.1180d0,.1266d0,.1269d0
C             08      09     10      11      12      13      14      15
     & ,.1033d0,.1278d0,.0978d0,0.200d0,0.199d0,0.200d0,0.0815d0,.1303d0
C PRN          16     17     18      19      20      21      22     23
     & ,.0838d0,.1401d0,.1069d0, .098d0, .103d0,0.1366d0,.1025d0,.1140d0
C PRN          24     25      26      27     28      29      30     31
     & ,.1089d0,.1001d0,.1227d0,.1194d0,.1260d0, .1228d0,.1165d0,.0969d0
C PRN         32  33-64: GLONASS RATES (DILSSNER 2010)                          
C
C Jan 10, 2017 - start: GALILEO ANOON of 2 DEG => YAW RATE of .203 DEG/s
C                Note: THE BEIDOU HARDWARE RATES NOT NEEDED
C    &  ,.1140d0, 32*0.250d0/
     &  ,.1140d0, 32*0.250d0, 36*0.203d0, 36*0.203d0/
C Jan 10, 2017
C     GAL  BETAy => BETA0
C     GAL  BETAx => ANOON=15DEG:  (ANIGHT-180); ANIGHT =180 + 15 DEG
C NOTE:BETA0= 0 DISABLES GALILEO AND/OR BEIDOU IGEO/MEO ECLIPSING!
       BETA0= 2.0D0
C Jan 10, 2017
C      SMTH = 258.D0
C
C  CHECK FOR BLOCK IIR AND FIX TO NOMINAL YAW RATE
      IF( IPRN .LE. 32 .AND. IBLK(IPRN).GE.4 ) YRATE(IPRN)=0.2D0
C THE NEW GPS BLK IIF YAW RATES ( DILSSNER (2010) INSIDE GNSS)
      IF( IPRN .LE. 32 .AND. IBLK(IPRN).GT.5 ) YRATE(IPRN)=0.11D0
C Dec 12, 2013
C  YBIAS=-0.5 FOR IIA (IBLK<4) PRN23 (SVN23 UP TO FEB2004,AFTER  IIR SVN60
C Feb 27, 2017
C   AND NOT USED), IIF (IBLK=6)=-0.7, USED FOR SMALL NEG BETA NOON TURNS ONLY!
      YBIAS=0.0D0
      IF(IBLK(IPRN).LE.3) YBIAS= 0.5D0
C Feb 27, 2017
C     IF(IPRN.EQ.23.OR.IBLK(IPRN).EQ.6) YBIAS=-0.5D0
C YBIAS=0 DISABLES SMALL NEGATIVE BETA IIF TURNS
C IF REQUIRED, CHANGE IIF YBIAS (BETA) LIMIT 
      IF(IBLK(IPRN).EQ.6) YBIAS=-0.7D0
C Jan 10, 2017 - start
C GAL BETAX => ANIGHT
      IF(IPRN.GT.64.AND.IPRN.LE.100)ANIGHT= 180.D0 +15.D0
C BEIDOU NIGHT TURN => BETA0 DEG (FOR REPORTING ONLY) 
      IF(IPRN.GT.100.AND.IPRN.LE.136)ANIGHT= 180.D0 + BETA0
C Jan 10, 2017 -end
C Init PHI
      PHI = PI/2.d0
C
      IECLIPS=0
      TWOHR = 7200.D0
      HALFHR= 1800.D0
C Jan 10, 2017 - no 1/2h extension for GALILEO
        IF(IPRN .GT.64.AND.IPRN.LE.100) HALFHR= 0.D0
      DTR=PI/180.D0
C compute the noon beta angle limit (beta zero) FOR A NOON TURN from YRATEs
C & THE ACTUAL SAT ORBIT ANGLE RATE (MURATE) (~0.00836 FOR GPS; ~ 0.00888 GLNS)
       MURATE= sqrt((VSVC(1)**2+VSVC(2)**2+VSVC(3)**2)/
     & (xsv(1)**2+xsv(2)**2+xsv(3)**2))/DTR
      ANOON=ATAN(MURATE/YRATE(IPRN))/DTR
C Jan 10, 2017
C  GAL & BEI NOON TURN LIMITS
C     IF(IPRN.GT.64.AND.IPRN.LE.100) ANOON=ANIGHT-180.d0
      IF(IPRN.GT.64.AND.IPRN.LE.136) ANOON=ANIGHT-180.d0

C July 2017
C CNOON limit extension
C      CNOON=DCOS(ANOON*DTR)
      CNOON=DCOS( (ANOON + HALFHR * MURATE) * DTR)    
	  
      CNIGHT=DCOS(ANIGHT*DTR)

C July 2017
C CNIGHT limit expansion for GPS IIA shadow exit recovery
 	  If ( IBLK(IPRN).LE.3 ) Then
		CNIGHT = DCOS( (ANIGHT + HALFHR * MURATE) * DTR)
	  End If
	  
C
      NOON=.FALSE.
      NIGHT=.FALSE.
      BETADG = beta/DTR - 90.d0 
C Mar 09, 2016
C      IF(IPRN.GT.32.AND.ABS(BETADG).LT.ANOON) THEN
       IF(IPRN.GT.32.AND.IPRN.LE.64.AND.ABS(BETADG).LT.ANOON) THEN
C GLONASS NOON TURN MODE ACORDING TO DILSSNER 2010 
         YAWEND=75.D0
C  ITERATION FOR YAWEND OF THE GLONASS  NOON TURN
         DO J=1,3
           YAWEND=ABS(ATAN2(-TAN(BETADG*DTR),SIN(PI-
     &       DTR*MURATE*YAWEND/YRATE(IPRN)))/DTR -      
     &       ATAN2(-TAN(BETADG*DTR),SIN(PI+
     &       DTR*MURATE*YAWEND/YRATE(IPRN)))/DTR)/2.D0 
         END DO
C UPDATE ANOON, CNOON FOR NEW GLONASS NOON TURN LIMITS
          ANOON= MURATE*YAWEND/YRATE(IPRN)
          CNOON= DCOS(ANOON*DTR)
       ENDIF 
C BLK IIR'S
C Jan 10, 2017 - ADD GAL & BEI
C       IF(IBLK(IPRN).EQ.4 .OR. IBLK(IPRN).EQ.5) THEN
        IF(IBLK(IPRN).EQ.4 .OR. IBLK(IPRN).EQ.5.OR.
C    &             (IPRN.GT.64.AND.IPRN.LE.100)) THEN
     &             (IPRN.GT.64.AND.IPRN.LE.136)) THEN
	 
C July 2017  
C Night turn analogous to Noon turn has been deactivated
C         CNIGHT=DCOS((ANOON+180.d0)*DTR)
		 
C Jan 16, 2015
C NO LOMGER THE IGS STANDARD - NO IIR BODY-X REVERSAL!
C      DO J=1,3
C BODY-X U VECTOR REVERSAL FOR IIR ONLY
c       SANTXYZ(J)=-SANTXYZ(J)
C        END DO
        END IF
C
      IF (SVBCOS .LT. CNIGHT) THEN
        NIGHT=.TRUE.
      END IF
      IF (SVBCOS .GT. CNOON) THEN
        NOON=.TRUE.
      END IF
C Jan 10, 2017 - IGNORE GAL ECLIPSING FOR BETA >= BETA0!
      IF(IPRN.GT.64.AND.IPRN.LE.100.AND.ABS(BETADG).GE.BETA0 ) THEN
       NIGHT = .FALSE.
       NOON  = .FALSE.
      END IF
C
C     IF SV IN NIGHT SHADOW OR NOON TURN DURING FORWARD PASS
C     STORE START AND END TIME OF YAW MANEUVRE (FOR THE BACKWARD RUN)
C
C YAW ANLGE
      YANGLE=   acos((santxyz(1)*vsvc(1) +
     &santxyz(2)*vsvc(2)+santxyz(3)*vsvc(3))/sqrt(vsvc(1)**2+vsvc(2)**2+
     & vsvc(3)**2))/DTR                          
C IIR REVERSAL NO LONGER APPLICABLE/USED
C IIR YANGLE has the same sign as beta, II/IIA has the opposite sign
C Jan 16, 2015
C      IF(BETADG.LT.0.d0.AND.IBLK(IPRN).GE.4.AND.IBLK(IPRN).LE.5)
C    &            YANGLE=-YANGLE
c      IF(BETADG.GT.0.d0.AND.IBLK(IPRN).NE.4.AND.IBLK(IPRN).NE.5)
       IF(BETADG.GT.0.d0)
     &            YANGLE=-YANGLE
C
C Jan 10, 2017 - Start
C BEIDOU ON YAW(=0) FOR GEO ALWAYS and FOR IGSO & MEO when BETADG < 2 DEG
      IF(IPRN.GT.100.AND.IPRN.LE.136) THEN
       IF(IBLK(IPRN).EQ.23.OR.IBLK(IPRN).EQ.27.OR.ABS(BETADG).LE.BETA0)
     &     THEN
         DO J=1,3
C BODY-X ON(ORBIT NORMAL YAW) FOR BEIDOU GEO(BLK 23,28) OR FOR
C |BETA| =< BETA0  (BODYX =>  SAT VELOCITY (YAW PHI=0))
           SANTXYZ(J)= VSVC(J)/sqrt(VSVC(1)**2+VSVC(2)**2+VSVC(3)**2)
          ENDDO
          PHI =  0.D0                       
        IF (NIGHT) THEN
C BEI NIGHT ECLIPS REPORTING
c              write(*,*)"R",IPRN,TTAG,YANGLE, PHI,DET,
c     &        BETADG 
             IECLIPS=1
             NECLIPS(IPRN)=1
        ENDIF
        IF(NOON) THEN
C BEI NOON ECLIPS REPORTING
c              write(*,*)"S",IPRN,TTAG,YANGLE, PHI,DET,
c     &        BETADG                 
             IECLIPS=2
             NECLIPS(IPRN)=1
        ENDIF
       END IF
       GOTO 1
      END IF
C Jan 10, 2017  end
      IF ( (NIGHT .OR. NOON)) THEN
       DET=SQRT((180.d0-acos(svbcos)/DTR)**2-BETADG**2)
       PHI = PI/2.d0
C Check if already after a midnight or noon
       IF(NIGHT) THEN
C Jan 16, 2015 - Start (NO IIR X-REVERSAL!)
c        IF(IBLK(IPRN).EQ.4.OR.IBLK(IPRN).EQ.5) THEN
c         IF(DABS(YANGLE).GT.90.d0) DET=-DET
c         IF(DET.NE.0.d0)PHI=ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))/DTR
c        ELSE
C BLK IIA & GLONASS & IIR & IIF TOO !
          IF(DABS(YANGLE).LT.90.d0) DET=-DET
          IF(DET.NE.0.d0)PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
c        END IF
C Jan 16, 2015- end
       END IF 
       IF(NOON) THEN
       DET=SQRT((acos(svbcos)*180./pi)**2-BETADG**2)
C Jan 16, 2015- start (NO IIR X-REVERSAL!)
c        IF(IBLK(IPRN).EQ.4.OR.IBLK(IPRN).EQ.5) THEN
c         IF(DABS(YANGLE).LT.90.d0) DET=-DET
c         IF(DET.NE.0.d0)PHI=ATAN2(TAN(BETADG*DTR),-SIN(PI-DET*DTR))/DTR
c        ELSE
C BLK IIA & GLONASS !
          IF(DABS(YANGLE).GT.90.d0) DET=-DET
          IF(DET.NE.0.d0)PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
c        END IF
C Jan 16, 2015 - end
       END IF 
	   
	  
C July 2017  
C Replace YANGLE value with the PHI value
      YANGLE = PHI	   
	   
	   
C ONLY FORWARD
       IF (IDIR .GT. 0 ) THEN
C
C       INITIALIZE ECLIPSE START AND TIME TAG ARRAYS  
C
        IF ( NECLIPS(IPRN) .EQ. 0 ) THEN
        NECLIPS(IPRN)=NECLIPS(IPRN)+1
          ECLSTM(IPRN,NECLIPS(IPRN))=TTAG+DET/MURATE
C IIR MIDNIGHT/NOON TURN or II/IIA NOON TURN START
C for IIR/GLONAS NIGHT (turn) only makes sense when |BETADG| < ANOON!
C For IIA it gets here only when NOON is true and that happens only when |BETADG|< ANOON!
          YAWEND=ATAN(MURATE/YRATE(IPRN))/DTR
          IF((IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5.OR.NOON).AND.
     &       ABS(BETADG).LT.YAWEND) THEN
C GLONASS
C Mar 09, 2016
C           IF(     IPRN .GT.32) THEN
C jan 10, 2017       - ALSO GAL NOON TURN TIMES(ANOON=ANIGHT-180deg)
C           IF(     IPRN .GT.32.AND.IPRN.LE.64) THEN
            IF(     IPRN .GT.32.AND.IPRN.LE.100) THEN
C GLONASS NOON TURN MODE, ANOON ACORDING TO DILSSNER ET AL 2010 
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &        ANOON/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &        2.D0*ANOON/MURATE
            ELSE
C GPS IIA/IIR/IIF NOON OR IIR MIDNIGHT TURNs
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &        ABS(BETADG)*sqrt(ANOON/ABS(BETADG)-1.d0)/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &        2*ABS(BETADG)*sqrt(ANOON/ABS(BETADG)-1.d0)/MURATE
            ENDIF
          END IF
C     II/IIA SHADOW START & END TIMES
C Jan 10, 2017 - ADD GALILEO NIGHT
C         IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5).AND.NIGHT) THEN
          IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5
     &         .OR.(IPRN .GT.64.AND.IPRN.LE.100)).AND.NIGHT) THEN
           ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &      SQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
           ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &      2.d0*SQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
          END IF
      END IF
C
C       UPDATE SV COSINE AND TIME TAG ARRAYS
C       (TO BE USED DURING BACKWARDS RUN)
C
        IF( (NIGHT .AND. SVBCOS .LT. CNIGHT)
     &       .OR. (NOON .AND. SVBCOS .GT. CNOON) ) THEN
          DTTAG= ABS(TTAG-ECLSTM(IPRN,NECLIPS(IPRN)))
C
C         ECLIPSE TIME IS MORE THAN 2 HOURS, THIS IS A NEW ECLIPSE!
C
C Feb 27, 2017
C SET GPS, GLN, GAL BETAINI IF APPLICABLE, I.E. FWD & |BETA| =< 0.07 DEG
            IF(IDIR.GT.0.AND.IPRN.LE.100.AND.ABS(BETADG).LE.0.07D0
     &         .AND.BETAINI(IPRN).EQ.0.D0) BETAINI(IPRN)= BETADG
         IF( DTTAG .GT. TWOHR ) THEN
          NECLIPS(IPRN)=NECLIPS(IPRN)+1
            ECLSTM(IPRN,NECLIPS(IPRN))=TTAG+DET/MURATE
C IIR MIDNIGHT/NOON TURN  or II/IIA NOON TURN START
C                                  AND GLONASS NOON
            IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5.OR.NOON) THEN
C GLONASS
C Mar 09, 2016
C            IF(IPRN.GT.32) THEN
C Jan 10, 2017 - include  ALSO GAL
C            IF(IPRN.GT.32.AND.IPRN.LE.64) THEN
             IF(IPRN.GT.32.AND.IPRN.LE.100) THEN
C GLONASS NOON TURN MODE ACORDING TO DILSSNER ET AL 2010 
              ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &          ANOON/MURATE
              ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &         2.D0*ANOON/MURATE
             ELSE
C GPS TURNS ONLY
              ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &         ABS(BETADG)*sqrt(ANOON/ABS(BETADG)-1.d0)/MURATE
              ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &         2*ABS(BETADG)*sqrt(ANOON/ABS(BETADG)-1.d0)/MURATE
C Jan 10, 2017 - GALILEO NOON - start
              IF(IPRN .GT.64.AND.IPRN.LE.100)THEN
           ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &      SQRT(ANOON**2-BETADG**2)/MURATE
           ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &      2.d0*SQRT(ANOON**2-BETADG**2)/MURATE
              END IF
             ENDIF
            END IF
C     II/IIA SHADOW START & END TIMES
C   & GLONASS & IIF AS WELL !
C Jan 10, 2017 - ADD GAL NIGHT
C           IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5).AND.NIGHT) THEN
            IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5
     &           .OR.(IPRN .GT.64.AND.IPRN.LE.100)).AND.NIGHT) THEN
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &       SQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &       2.d0*SQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
            END IF
        END IF
        ENDIF
C  END OF FORWARD (FWD) LOOP (IDIR = 1)
       ENDIF
      ENDIF
C
C     BOTH FWD (IDIR= 1) OR BWD (IDIR=-1)
C     SET ECLIPSE FLAG (1=NIGHT SHADOW, 2=NOON TURN) 
C
      IF ( NECLIPS(IPRN) .NE. 0 ) THEN
C CHECK IF IPRN IS ECLIPSING AND WHICH SEQ NO (I)
        I=0
        DO J=1, NECLIPS(IPRN)
         IF( TTAG.GE.ECLSTM(IPRN,J).AND.TTAG.LE.(ECLETM(IPRN,J)+HALFHR))
     &   I= J
        END DO
C CURRENTLY NOT ECLIPSING (i=0)
        IF(I.EQ.0) GO TO 1
        IF ( TTAG .GE. ECLSTM(IPRN,I) .AND.
     &            TTAG .LE.(ECLETM(IPRN,I)+HALFHR) ) THEN
C Feb 27, 2017 
C FOR GPS & GLN ONLY! USE BETAINI FOR ALL TURNS WHEN |BETADG|.LE. 0.07 DEG
C TO ENSURE NO BETA SIGN CHANGE AND IIF |START-END YAW DIFF| < 180 DEG
            IF(ABS(BETADG).LE.0.07D0.AND.IPRN.LE.64
     &         .AND.BETAINI(IPRN).NE.0.0D0) BETADG = BETAINI(IPRN)
C velocity & radius unit vectors V & R
             DO J=1,3
              V(J)=VSVC(J)/SQRT(VSVC(1)**2+VSVC(2)**2+VSVC(3)**2)
              R(J)=XSV(J)/SQRT(XSV(1)**2+XSV(2)**2+XSV(3)**2) 
             END DO
C ORBIT ANGLE MU AT ECLIPSE/TURN START
             DET= MURATE*(ECLETM(IPRN,I)-
     &                        ECLSTM(IPRN,I))/2.d0
C Dec 12, 2013 - start
C YAWEND HERE - IIF SHADOW YAW RATE
C HERE |YAW start-YAW end| ALWAYS < 180 DEG DUE TO POSSIBLE USE OF BETAINI
                  YAWEND=(ATAN2(-TAN(BETADG*DTR), SIN( DET*DTR))- 
     &             ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR)))/DTR/
     &                  (ECLETM(IPRN,I)-ECLSTM(IPRN,I))
c Dec 12, 2013 - end
           IF (SVBCOS .LT. 0) THEN
C SHADOW CROSSING
C BLK IIA/IIF SHADOW CROSSING
                IF(IPRN.LE.32.AND.(IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5))
     &             THEN
                 IF(TTAG.LE.ECLETM(IPRN,I)) THEN
C IIA NIGHT TURN
                  IF(IBLK(IPRN).LE.3) 
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR

C July 2017
C Yaw-bias is replaced by the beta angle.  Case: GPS IIA for beta<0 
C     &           +SIGN(YRATE(IPRN),YBIAS)*(TTAG-ECLSTM(IPRN,I)) 
     &         +SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))     				    
	 
c               write(*,*) iprn, phi, betadg, betaini(iprn)
			   
C Dec 12, 2013
c    &           +SIGN(YRATE(IPRN),0.5d0)*(TTAG-ECLSTM(IPRN,I)) 
C IIF NIGHT TURN (DILSSNER  2010)
                   IF(IBLK(IPRN).GT.5)
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
C Dec 12, 2013- start
C Correct IIF  NIGHT CROSSING USING COMPUTING YAW RATE (YAWEND)
C ACCORDING TO THE USAF IIF DOCUMENT
C    &           +SIGN(0.06D0, BETADG)*(TTAG-ECLSTM(IPRN,I)) 
     &           +  YAWEND            *(TTAG-ECLSTM(IPRN,I)) 
C Dec 12, 2013 - end
                 ELSE
C **** WARNING
C IIA SHADOW EXIT RECOVERY: USING THE IIA DATA  DURING
C THE IIA RECOVERY (UP TO 30 MIN) IS NOT RECOMMENDED!
C **** WARNING
C GPS IIA  AT SHADOW EXIT
                   IF(IBLK(IPRN).LE.3)
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
C Dec 12, 2013
C    &          +SIGN(YRATE(IPRN),0.5d0)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))

C July 2017
C Yaw-bias is replaced by the beta angle.  Case: GPS IIA for beta<0 
C     &          +SIGN(YRATE(IPRN),YBIAS)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))
     &      +SIGN(YRATE(IPRN),BETADG)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))  
	 
C GPS IIF AT SHADOW EXIT
C Dec 12, 2013
C NO NEED FOR IIF (IBLK=6) RECOVERY, ALREADY AT THE EXIT YAW!
                   IF(IBLK(IPRN).GT.5)GO TO 1
c                  IF(IBLK(IPRN).GT.5)
c    &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
C    &          +SIGN(0.06D0, BETADG)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I)) 
C YAWEND- HERE THE ACTUAL YAW DIFFERENCE  AT THE SHADOW EXIT
                   YAWEND= YANGLE- PHI  

C July 2017 
C Angle reduction has been deactivated in order to avoid opposite SIGN				   				   
C                   YAWEND=DMOD(YAWEND, 360.D0)
C                   IF(ABS(YAWEND).GT.180.D0) YAWEND= YAWEND-360.D0*
C     &               YAWEND/ABS(YAWEND)
	 
                   PHI=PHI
     &                   +SIGN(YRATE(IPRN),YAWEND)*(TTAG-ECLETM(IPRN,I))
C SANTX- THE CURRENT ANGLE DIFF, CONSISTENT WITH YAWEND
                   SANTX= YANGLE-PHI

C July 2017 
C Angle reduction has been deactivated in order to avoid opposite SIGN				   				   
C                   SANTX =DMOD(SANTX , 360.D0)
C                   IF(ABS(SANTX).GT.180.D0) SANTX = SANTX -360.D0*
C     &               SANTX /ABS(SANTX )
	 
C STOP! THE NOMINAL YAW (YANGLE) REACHED!
                  IF(ABS(SANTX).GT.ABS(YAWEND)) GOTO 1
                  IF(YAWEND.NE.0.D0.AND.((SANTX)/YAWEND).LT.0.D0) GOTO 1

C July 2017 
C Angle reduction has been deactivated;
C abs(Phi)>180 may occurs during initial epochs of GPS IIA exit recovery				   
C SET PHI [-180,+180]
C                   PHI= DMOD(PHI, 360.D0)
C                   IF(ABS(PHI).GT.180.D0) PHI= PHI-360.D0*PHI/ABS(PHI)

                 ENDIF
                ENDIF
C GLONASS
C Mar 09, 2016
C               IF(IPRN.GT.32) THEN
                IF(IPRN.GT.32.AND.IPRN.LE.64) THEN
C GLONASS      NIGHT TURN (DILSSNER AT AL 2010 )
                 IF(TTAG.GT.ECLETM(IPRN,I)) GOTO 1
                 YAWEND=YRATE(IPRN) 
                 PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
     &      +SIGN(YAWEND     ,BETADG)*(TTAG-ECLSTM(IPRN,I)) 
C YAWEND -YAW ANGLE AT THE (GLONASS) SHADOW EXIT
                  YAWEND=ATAN2(-TAN(BETADG*DTR), SIN( DET*DTR))/DTR
                 IF((YAWEND/PHI).GE.1.d0.OR.(PHI/YAWEND).LT.0.d0) THEN  
                  PHI = YAWEND
                 ENDIF
                ENDIF
C Dec 12, 2013 - start
c                  IF(IPRN.LE.32.AND.IBLK(IPRN).GT.5) THEN
C GPS BLK IIF NIGHT YAW RATE(DILSSNER 2010):
c                   IF(ABS(BETADG).GT.8.D0) GO TO 1
c                  ENDIF
C Jan 10, 2017
C              IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5) THEN
              IF(IPRN.LE.32.AND.IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5)THEN
C BLK II R SHADOW (MIDNIGHT TURN) CROSSING
C Jan 16, 2015
c               PHI=ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))/DTR
                PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
     &      +SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I)) 
C Jan 16, 2015
C               IF((PHI/YANGLE).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0) GO TO 1
                IF((YANGLE/PHI).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0) GO TO 1
               END IF
C jan 10, 2017 - GALILEO night - start
               IF(IPRN.GT.64.AND.IPRN.LE.100) THEN
C  SIN ORB ANGLE MU => YAWEND
               YAWEND=
     &         SIN((ttag-(ECLSTM(IPRN,I)+ECLETM(IPRN,I))/2)*MURATE*DTR)
C Jan 10, 2017 - start
C GAL ECLIPS MODEL: Shy => BETAE
               BETAE=0.5*(-SIGN(SIN(BETA0*DTR),BETADG)-SIN(BETADG*DTR))
     &         +0.5D0*(-SIGN(SIN(BETA0*DTR),BETADG)+SIN(BETADG*DTR))*
     &         COS(PI*ABS(YAWEND*COS(BETADG*DTR))/SIN(ANOON*DTR))
                 IF(ABS(BETADG).LT.0.07D0) THEN
C USE BETAINI IF NOT ZERO, SINCE BETA SIGN CHANGE IS POSSIBLE DURING THE
C MODEL TURN
                  IF(BETAINI(IPRN).NE.0.D0)
     &BETAE=0.5*(-SIGN(SIN(BETA0*DTR),BETAINI(IPRN))-SIN(BETADG*DTR))
     &+0.5D0*(-SIGN(SIN(BETA0*DTR),BETAINI(IPRN))+SIN(BETADG*DTR))*
     &         COS(PI*ABS(YAWEND*COS(BETADG*DTR))/SIN(ANOON*DTR))
                 ENDIF
               PHI   = ATAN2(BETAE, YAWEND*COS(BETADG*DTR))/DTR
               ENDIF
C  GALILEO night - ends 
C Jan 10, 2017 - end
c              write(*,*)"R",IPRN,TTAG,YANGLE, PHI,DET,
c     & BETADG, ECLETM(IPRN,I),I
             IECLIPS=1
           ELSE
C NOON TURNS 
           PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
     &      -SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))
C Dec 12, 2013 -start
C SMALL NEGATIVE BETA IIF OR SMALL POSITIVE IIA NOON TURN PROBLEM
C Jan 24, 2014
C               IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).EQ.6).AND.   
C APPLY WRONG DIRECTIONs TO ALL IIA (IBLK.LE.3) NOON WITH BETA (0,0.5DEG) AND
C IIF (IBLK=6) NNON TURNS WITH BETA (-0.7,0) DEG
C FOR ONLY IIF WRONG NOONS, USE:
C               IF(IPRN.LE.32.AND.IBLK(IPRN).EQ.6.AND.
                IF(IPRN.LE.32.AND.   
C Feb 27, 2017
C USE YBIAS AS  BETA LIMIT FOR WRONG NOON TURN DIRECTION
C    &             (BETADG*SIGN(1.D0,YBIAS)).LE.0.9D0  .AND.

C July 2017
C Deactivation of the limit of ABS(YBIAS) when close to beta sign change 
C     &             (BETADG*SIGN(1.D0,YBIAS)).LE.ABS(YBIAS).AND.
     &             (BETADG*SIGN(1.D0,YBIAS)).LE.0.1D0 .AND.        
	 
     &             (BETADG*YBIAS).GT.0.0D0)                  THEN
                    PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
     &              +SIGN(YRATE(IPRN),YBIAS )*(TTAG-ECLSTM(IPRN,I))
                 ENDIF
C Dec 12, 2013 - end
                IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5) THEN
C BLK IIR NOON TURNS ONLY
C Jan 16, 2015
c                PHI=ATAN2( TAN(BETADG*DTR),-SIN(PI-DET*DTR))/DTR
c    &      -SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))
C IIR END TURN CHECK
C                IF((YANGLE/PHI).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0) GOTO 1
                 IF((PHI/YANGLE).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0) GOTO 1
                ELSE
C GLONASS END TURN CHECK
C Mar 09, 2016
C                IF(     IPRN .GT.32.AND.TTAG.GT.ECLETM(IPRN,I)) GOTO 1
C Jan 10, 2017
C        IF(IPRN.GT.32.AND.IPRN.LE.64.AND.TTAG.GT.ECLETM(IPRN,I)) GOTO 1
         IF(IPRN.GT.32.AND.IPRN.LE.100.AND.TTAG.GT.ECLETM(IPRN,I))GOTO 1
C IIA OR IIF END TURN CHECK
C Dec 12, 2013 -start
C Feb 27, 2017
C                IF(IPRN.LE.32.AND.BETADG*SIGN(1.D0,YBIAS).LE.0.9D0.AND.
                 IF(IPRN.LE.32.AND.BETADG*SIGN(1.D0,YBIAS).LE.ABS(YBIAS)
     &              .AND.
C Jan 24, 2014
C    &             BETADG*SIGN(1.D0,YBIAS).GT.0.0D0.AND.
     &             BETADG*YBIAS.GT.0.0D0.AND.
     &            (((PHI-SIGN(1.d0,YBIAS)*360.D0)/YANGLE).LE.1.d0.OR.
     &            ((PHI-SIGN(1.D0,YBIAS)*360.D0)/YANGLE).LT.0.d0))GOTO 1
C                IF(IPRN.LE.32.AND.
                 IF(IPRN.LE.32.AND.(BETADG*SIGN(1.D0,YBIAS).GT.0.7D0.OR.
C Jan 24, 2014
C    &              BETADG*SIGN(1.D0,YBIAS).LE.0.0D0).AND.
     &              BETADG*YBIAS.LE.0.0D0).AND.
     &            ((PHI/YANGLE).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0)) GOTO 1
                ENDIF
				
C July 2017
C GPS IIA/IIF Noon turn end check        			 	 
	  IF ( IBLK(IPRN)<=3 .OR. IBLK(IPRN)==6 ) THEN        
         IF (ABS(YANGLE)>170.D0 .AND. ABS(YANGLE/PHI)<1.0D0)  GOTO 1
      END IF 	 				
				
C Jan 10, 2017 - GALILEO noon  - start
               IF(IPRN.GT.64.AND.IPRN.LE.100) THEN
C  SIN ORB ANGLE MU => YAWEND
               YAWEND= SIN(
     &         PI +(TTAG-(ECLSTM(IPRN,I)+ECLETM(IPRN,I))/2)*MURATE*DTR)
C Jan 10, 2017
C GAL ECLIPS MODEL: Shy => BETAE
               BETAE=0.5*(-SIGN(SIN(BETA0*DTR),BETADG)-SIN(BETADG*DTR))
     &         +0.5D0*(-SIGN(SIN(BETA0*DTR),BETADG)+SIN(BETADG*DTR))*
     &         COS(PI*ABS(YAWEND*COS(BETADG*DTR))/SIN(ANOON*DTR))
                 IF(ABS(BETADG).LT.0.07D0) THEN
C USE BETAINI IF NOT ZERO, SINCE BETA SIGN CHANGE IS POSSIBLE DURING THE
C MODEL TURN
                  IF(BETAINI(IPRN).NE.0.D0)
     &BETAE=0.5*(-SIGN(SIN(BETA0*DTR),BETAINI(IPRN))-SIN(BETADG*DTR))
     &+0.5D0*(-SIGN(SIN(BETA0*DTR),BETAINI(IPRN))+SIN(BETADG*DTR))*
     &         COS(PI*ABS(YAWEND*COS(BETADG*DTR))/SIN(ANOON*DTR))
                 ENDIF
               PHI   = ATAN2(BETAE, YAWEND*COS(BETADG*DTR))/DTR
C Jan 10, 2017 - end
               ENDIF
C Jan 10, 2017 - GALILEO noon  - ends
C
c              write(*,*)"S",IPRN,TTAG,YANGLE, PHI,DET,
c     & BETADG, ECLSTM(IPRN,I)                  
             IECLIPS=2
           END IF
C ROTATE X-VECTOR TO ECLIPSING YAW ANGLE PHI 
C ECLIPSING (II/IIA) NOT TO BE USED  A HALF HR AFTER SHADOW !
       SANTX=(COS((PHI-YANGLE)*DTR)*(V(2)-V(3)*R(2)/R(3))-COS(PHI*
     & DTR)*
     &(SANTXYZ(2)-SANTXYZ(3)*R(2)/R(3)))/(SANTXYZ(1)*V(2)-SANTXYZ(2)*v(1
     &)+((SANTXYZ(2)*V(3)-SANTXYZ(3)*V(2))*R(1)+(SANTXYZ(3)*V(1)-SANTXYZ
     &(1)*V(3))*R(2))/R(3))
       SANTY = (COS(PHI*DTR) - (V(1)-V(3)*R(1)/R(3))*SANTX)/
     & (V(2)-V(3)*R(2)/R(3))
C THE BODY-X UNIT VECTOR ROTATED BY (PHI-YANGLE) RETURNED
          SANTXYZ(1)= SANTX
          SANTXYZ(2)= SANTY
          SANTXYZ(3)= (-R(1)*SANTX-R(2)*SANTY)/R(3)
      END IF
      ENDIF
	  
C July 2017
C The output argument "yaw_angle" has been added for providing the computed 
C values of the nominal yaw angle (YANGLE) and the eclipsing yaw angle (PHI)            
      yaw_angle(1) = YANGLE
      yaw_angle(2) = PHI
	  
      
1     RETURN
C
      END
C2345678901234567890123456789012345678901234567890123456789012345678901234567890
