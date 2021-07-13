C2345678901234567890123456789012345678901234567890123456789012345678901234567890
C
C*************************************************************** ECLIPS
C
      SUBROUTINE ECLIPS ( IDIR, IPRN, TTAG, SVBCOS, ANOON, ANIGHT,
     &                    NECLIPS, ECLSTM, ECLETM, IECLIPS, PI ,
     &                    xsv, santxyz, vsvc, beta, iblk, 
     &                    yaw_angle, MU, MURATE)
C
C       NAME            ECLIPS (version Dec  2013)
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
C       CONTACT         kouba@geod.nrcan.gc.ca
C
C       UPDATE HISTORY: Aug. 23, 2011:1996-2008 W. AVERAGES of JPL REPROCESSING
C                                YRATE SOLUTIONS FOR ALL II/IIA CODED IN DATA
C                                STATEMENT, THIS ENABLES REPROCESSING FROM 1996
C                       Sep 26, 2011: 1. Corrected bug causing Block IIF shadow 
C                               CROSSING MANEVURE WITH 0.06 DEG/SEC RATE EVEN 
C                               FOR BETADG > 8 DEG
C                                     2. CORRECTED/improved IIA RECOVERY logic
C Dec 12, 2013 - start
C                       Dec 18, 2013: 1.corrected IIF night turns (USAF Doc.)
C                                     2.small neg beta IIF and small pos IIA noon turn
C                                    (wrong) directions  for |beta| < 0.9deg
C                                     3. PRN/SVN 23 IIA YBIAS= -0.5 deg up to Feb 2004 
C                                     4. All the above changes labeled "C Dec 12, 2013"
C Dec 12, 2013 - end
C
C                      Jan 24, 2014:  NOON RECOVERY CORRECTED IF IIA/IIF's HAVE YBIAS=0
C                                    (NOT APPLICABLE CURRENTLY,POSSIBLE FOR FUTURE IIF?)
C                                    & SMALL IIA/IIF BETA IF STATEMENT SIMPLIFIED
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
C
C        INTERNAL PARAMETRS DESCRIPTION
C Dec 12, 2013
C        YBIAS       IIA YAW BIAS= 0.5 deg SINCE NOV95,EXCEPT PRN/SVN23?
C        YANGLE         THE NOMINAL YAW ANGLE
C        PHI            THE ECLIPSING YAW ANGLE            
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
c 180.D0+-13.25D0 for GPS (IPRN.GE. 32) and 180.D0+-14.20D0 for 
c GLONASS (IPRN.GT.32), resp.). CNIGHT=COS(ANIGHT) is used for testing.
c When SVBCOS < CNIGHT (CNIGHT is negative and close to -1), 
c the SV enters, or it is in the shadow. 
c
C *********************************************************************
C
      IMPLICIT NONE
C
C     MAXSAT - MAXIMUM NUMBER OF SATELLITES, CHANGED IF REQUIRED
C      
      INTEGER MAXSAT
      PARAMETER (MAXSAT=64)
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
     &          YRATE(64), BETADG, PHI, SANTX, SANTY,        v(3),r(3)
      REAL*8    YAWEND
C Dec 12, 2013
      REAL*8 YBIAS
      INTEGER*4 IBLK(*), J, I
C
      LOGICAL   NOON, NIGHT

C 27 June 2016
      REAL*8 yaw_angle(2)
C 28 July 2016
      REAL*8 MU


	  
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
     &  ,.1140d0, 32*0.250d0/
C  CHECK FOR BLOCK IIR AND FIX TO NOMINAL YAW RATE
      IF( IPRN .LE. 32 .AND. IBLK(IPRN).GE.4 ) YRATE(IPRN)=0.2D0
C THE NEW GPS BLK IIF YAW RATES ( DILSSNER (2010) INSIDE GNSS)
      IF( IPRN .LE. 32 .AND. IBLK(IPRN).GT.5 ) YRATE(IPRN)=0.11D0
C Dec 12, 2013
C  YBIAS=-0.5 FOR IIA (IBLK<4) PRN23 (SVN23 UP TO FEB2004,AFTER  IIR SVN60
C   AND NOT USED), IIF (IBLK=6)=-0.5, USED FOR SMALL NEG BETA NOON TURNS ONLY!
      YBIAS=0.0D0
      IF(IBLK(IPRN).LE.3) YBIAS= 0.5D0
      IF(IPRN.EQ.23.OR.IBLK(IPRN).EQ.6) YBIAS=-0.5D0
C
      IECLIPS=0
      TWOHR = 7200.D0
      HALFHR= 1800.D0    
      DTR=PI/180.D0
C compute the noon beta angle limit (beta zero) FOR A NOON TURN from YRATEs
C & THE ACTUAL SAT ORBIT ANGLE RATE (MURATE) (~0.00836 FOR GPS; ~ 0.00888 GLNS)
       MURATE= sqrt((VSVC(1)**2+VSVC(2)**2+VSVC(3)**2)/
     & (xsv(1)**2+xsv(2)**2+xsv(3)**2))/DTR
      ANOON=ATAN(MURATE/YRATE(IPRN))/DTR
c      CNOON=DCOS(ANOON*DTR)   													C 31/03/2017 CNOON limit extension
      CNOON=DCOS( (ANOON + HALFHR * MURATE) * DTR)
      CNIGHT=DCOS(ANIGHT*DTR)
	  
	  
C 22 August 2016																C Attention: CNIGHT limit expansion for shadow exit recovery
C Expand the CNIGHT limit for shadow exit recovery   
 	  If ( IBLK(IPRN).LE.3 ) Then
		CNIGHT = DCOS( (ANIGHT + HALFHR * MURATE) * DTR)
		! CNIGHT = DCOS(ANIGHT*DTR)
	  End If
	  
C
      NOON=.FALSE.
      NIGHT=.FALSE.
      BETADG = beta/DTR - 90.d0 
	  
	  
C
       IF(IPRN.GT.32.AND.ABS(BETADG).LT.ANOON) THEN
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
        IF(IBLK(IPRN).EQ.4 .OR. IBLK(IPRN).EQ.5) THEN
C         CNIGHT=DCOS((ANOON+180.d0)*DTR) 										C Night turn analogous to Noon has been deactivated
       DO J=1,3
C BODY-X U VECTOR REVERSAL FOR IIR ONLY
        SANTXYZ(J)=-SANTXYZ(J)
         END DO
        END IF
C
      IF (SVBCOS .LT. CNIGHT) THEN
        NIGHT=.TRUE.
      END IF
      IF (SVBCOS .GT. CNOON) THEN
        NOON=.TRUE.
      END IF
	  	  
	  
C
C     IF SV IN NIGHT SHADOW OR NOON TURN DURING FORWARD PASS
C     STORE START AND END TIME OF YAW MANEUVRE (FOR THE BACKWARD RUN)
C
C init PHI
      PHI = PI/2.d0
C YAW ANGLE
      YANGLE=   acos((santxyz(1)*vsvc(1) +
     &santxyz(2)*vsvc(2)+santxyz(3)*vsvc(3))/sqrt(vsvc(1)**2+vsvc(2)**2+
     & vsvc(3)**2))/DTR
 
	 
	 
C IIR YANGLE has the same sign as beta, II/IIA has the opposite sign
       IF(BETADG.LT.0.d0.AND.IBLK(IPRN).GE.4.AND.IBLK(IPRN).LE.5)
     &            YANGLE=-YANGLE
       IF(BETADG.GT.0.d0.AND.IBLK(IPRN).NE.4.AND.IBLK(IPRN).NE.5)
     &            YANGLE=-YANGLE
C
      IF ( (NIGHT .OR. NOON)) THEN
       DET=SQRT((180.d0-acos(svbcos)/DTR)**2-BETADG**2)
       PHI = PI/2.d0
C Check if already after a midnight or noon
       IF(NIGHT) THEN
         IF(IBLK(IPRN).EQ.4.OR.IBLK(IPRN).EQ.5) THEN
          IF(DABS(YANGLE).GT.90.d0) DET=-DET
          IF(DET.NE.0.d0)PHI=ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))/DTR
         ELSE
C BLK IIA & GLONASS TOO !
          IF(DABS(YANGLE).LT.90.d0) DET=-DET
          IF(DET.NE.0.d0)PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
         END IF
       END IF 
       IF(NOON) THEN
	   
C       DET=SQRT((acos(svbcos)*180./pi)**2-BETADG**2)
C 29 July 2016
C 180. changed to 180.D0
       DET=SQRT((acos(svbcos) * 180.D0 /pi)**2-BETADG**2)
	   
         IF(IBLK(IPRN).EQ.4.OR.IBLK(IPRN).EQ.5) THEN
          IF(DABS(YANGLE).LT.90.d0) DET=-DET
          IF(DET.NE.0.d0)PHI=ATAN2(TAN(BETADG*DTR),-SIN(PI-DET*DTR))/DTR
         ELSE
C BLK IIA & GLONASS !
          IF(DABS(YANGLE).GT.90.d0) DET=-DET
          IF(DET.NE.0.d0)PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
         END IF
       END IF 
	  
	  
C 28 July 2016
      MU = DET
	  
C 18 August 2016
C Replace YANGLE with the PHI value (as computed in the directly previous lines of code)
C YANGLE:	Angle of the dot product of the Body-X and Velocity unit vectors
C PHI:		Nominal yaw angle based on the fundamental equation atan2(-tan(beta),sin(MU))
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
C for IIR/GLONAS NIGHT (turn) only makes sense when BETADG < ANOON!
C For IIA it gets here only when NOON is true and that happens  only when BETADG < ANOON!
          YAWEND=ATAN(MURATE/YRATE(IPRN))/DTR
          IF((IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5.OR.NOON).AND.
     &       ABS(BETADG).LT.YAWEND) THEN
C GLONASS
            IF(     IPRN .GT.32) THEN
C GLONASS NOON TURN MODE ACORDING TO DILSSNER ET AL 2010 
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
          IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5).AND.NIGHT) THEN
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
         IF( DTTAG .GT. TWOHR ) THEN
          NECLIPS(IPRN)=NECLIPS(IPRN)+1
            ECLSTM(IPRN,NECLIPS(IPRN))=TTAG+DET/MURATE
C IIR MIDNIGHT/NOON TURN  or II/IIA NOON TURN START
C                                  AND GLONASS NOON
            IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5.OR.NOON) THEN
C GLONASS
             IF(IPRN.GT.32) THEN
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
             ENDIF
            END IF
C     II/IIA SHADOW START & END TIMES
C   & GLONASS & IIF AS WELL !
            IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).GT.5).AND.NIGHT) THEN
             ECLSTM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))-
     &       SQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
             ECLETM(IPRN,NECLIPS(IPRN))= ECLSTM(IPRN,NECLIPS(IPRN))+
     &       2.d0*SQRT((ANIGHT-180.d0)**2-BETADG**2)/MURATE
            END IF
        END IF
        ENDIF
C  END OF FORWARD LOOP (IDIR = 1)
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
C Dec 12, 2013
C    &           +SIGN(YRATE(IPRN),0.5d0)*(TTAG-ECLSTM(IPRN,I)) 
C     &           +SIGN(YRATE(IPRN),YBIAS)*(TTAG-ECLSTM(IPRN,I)) 
     &         +SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))     				    C Rev.12/09/2016: Case: IIA Beta<0 
	 
	 
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
C IIA/IIF SHADOW EXIT RECOVERY: USING THE IIA DATA  DURING
C THE IIA RECOVERY (UP TO 30 MIN) IS NOT RECOMMENDED!
C **** WARNING

	  
	  
C GPS IIA  AT SHADOW EXIT
                   IF(IBLK(IPRN).LE.3)
     &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
C Dec 12, 2013
C    &          +SIGN(YRATE(IPRN),0.5d0)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))
c    &          +SIGN(YRATE(IPRN),YBIAS)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))
     &      +SIGN(YRATE(IPRN),BETADG)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I))   			C Rev.12/09/2016: Case: IIA Beta<0 

	 

	  
C GPS IIF AT SHADOW EXIT
C Dec 12, 2013
C NO NEED FOR IIF RECOVERY ALREADY AT THE EXIT YAW!
                   IF(IBLK(IPRN).GT.5)GO TO 1
c                  IF(IBLK(IPRN).GT.5)
c    &             PHI=ATAN2(-TAN(BETADG*DTR), SIN(-DET*DTR))/DTR
C    &          +SIGN(0.06D0, BETADG)*(ECLETM(IPRN,I)-ECLSTM(IPRN,I)) 


C YAWEND- HERE THE ACTUAL YAW DIFFERENCE  AT THE SHADOW EXIT
                   YAWEND= YANGLE- PHI   
C 23 August 2016																	C Rev.
C Deactivated to avoid error (opposite) SIGN				   
c                   YAWEND=DMOD(YAWEND, 360.D0)
c                   IF(ABS(YAWEND).GT.180.D0) YAWEND= YAWEND-360.D0*
c     &               YAWEND/ABS(YAWEND)

	  
                   PHI=PHI
     &                   +SIGN(YRATE(IPRN),YAWEND)*(TTAG-ECLETM(IPRN,I))

	 
	  
C SANTX- THE CURRENT ANGLE DIFF, CONSISTENT WITH YAWEND
                   SANTX= YANGLE-PHI
C 23 August 2016																	C Rev.
C Deactivated to avoid error (opposite) SIGN				   
c                   SANTX =DMOD(SANTX , 360.D0)
c                   IF(ABS(SANTX).GT.180.D0) SANTX = SANTX -360.D0*
c     &               SANTX /ABS(SANTX )


	  
C STOP! THE NOMINAL YAW (YANGLE) REACHED!
                  IF(ABS(SANTX).GT.ABS(YAWEND)) GOTO 1
                  IF(YAWEND.NE.0.D0.AND.((SANTX)/YAWEND).LT.0.D0) GOTO 1

				  
C 19 September 2016																	C Rev.
C Deactivated during initial epochs of IIA exit recovery where may abs(Phi)>180				   
			  
C SET PHI <-180,+180>
c                   PHI= DMOD(PHI, 360.D0)
c                   IF(ABS(PHI).GT.180.D0) PHI= PHI-360.D0*PHI/ABS(PHI)
				   
				   
                 ENDIF
                ENDIF


				
C GLONASS
                IF(IPRN.GT.32) THEN
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
               IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5) THEN
C BLK II R SHADOW (MIDNIGHT TURN) CROSSING
                PHI=ATAN2( TAN(BETADG*DTR),-SIN(-DET*DTR))/DTR
     &      +SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I)) 
                IF((PHI/YANGLE).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0) GO TO 1				C Deactivate condition for return (GOTO 1)
               END IF
c             write(*,*)"R",IPRN,TTAG,YANGLE, PHI,DET,
c    & BETADG, ECLETM(IPRN,I),I


             IECLIPS=1
			 
			 
           ELSE
C NOON TURNS 
              PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
     &      -SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))

	 
C Dec 12, 2013 -start
C SMALL NEGATIVE BETA IIF OR SMALL POSIT. IIA NOON TURN PROBLEM
C Jan 24, 2014
C               IF((IBLK(IPRN).LE.3.OR.IBLK(IPRN).EQ.6).AND.   
                IF(IPRN.LE.32.AND.   
C CHANGE THE EMPIRICAL BETA LIMIT 0.9 DEG, IF REQ'D  
c     &             (BETADG*SIGN(1.D0,YBIAS)).LE.0.9D0  .AND.
C IN THEORY THE ABOVE LIMIT OF 0.9 DEG SHOULD BE ABS(YBIAS)!
c     &             (BETADG*SIGN(1.D0,YBIAS)).LE.ABS(YBIAS) .AND.        				C Rev.19/09/2016: Beta sign change 
     &             (BETADG*SIGN(1.D0,YBIAS)).LE.0.1D0 .AND.        						C Rev.19/09/2016: Beta sign change 
     &             (BETADG*YBIAS).GT.0.0D0)                  THEN
                    PHI=ATAN2(-TAN(BETADG*DTR),SIN(PI-DET*DTR))/DTR
     &              +SIGN(YRATE(IPRN),YBIAS )*(TTAG-ECLSTM(IPRN,I))
                ENDIF
C Dec 12, 2013 - end
                IF(IBLK(IPRN).GT.3.AND.IBLK(IPRN).LE.5) THEN
C BLK IIR NOON TURNS ONLY
                 PHI=ATAN2( TAN(BETADG*DTR),-SIN(PI-DET*DTR))/DTR
     &      -SIGN(YRATE(IPRN),BETADG)*(TTAG-ECLSTM(IPRN,I))
C IIR END TURN CHECK
                 IF((YANGLE/PHI).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0) GOTO 1
                ELSE
C GLONASS END TURN CHECK
                 IF(     IPRN .GT.32.AND.TTAG.GT.ECLETM(IPRN,I)) GOTO 1
C IIA OR IIF END TURN CHECK
C Dec 12, 2013 -start
                 IF(IPRN.LE.32.AND.BETADG*SIGN(1.D0,YBIAS).LE.0.9D0.AND.
C Jan 24, 2014
C    &             BETADG*SIGN(1.D0,YBIAS).GT.0.0D0.AND.
     &             BETADG*YBIAS.GT.0.0D0.AND.
     &            (((PHI-SIGN(1.d0,YBIAS)*360.D0)/YANGLE).LE.1.d0.OR.
     &            ((PHI-SIGN(1.D0,YBIAS)*360.D0)/YANGLE).LT.0.d0))GOTO 1
C                IF(IPRN.LE.32.AND.
                 IF(IPRN.LE.32.AND.(BETADG*SIGN(1.D0,YBIAS).GT.0.9D0.OR.
C Jan 24, 2014
C    &              BETADG*SIGN(1.D0,YBIAS).LE.0.0D0).AND.
     &              BETADG*YBIAS.LE.0.0D0).AND.
     &            ((PHI/YANGLE).GE.1.d0.OR.(PHI/YANGLE).LT.0.d0)) GOTO 1
                ENDIF
c             write(*,*)"S",IPRN,TTAG,YANGLE, PHI,DET,
c    & BETADG, ECLSTM(IPRN,I)


C Rev.03/04/2017: GPS IIA/IIF Noon turn end check         									C Rev.03/04/2017 			 	 
	  IF ( IBLK(IPRN)<=3 .OR. IBLK(IPRN)==6 ) THEN        
         IF (ABS(YANGLE)>170.D0 .AND. ABS(YANGLE/PHI)<1.0D0)  GOTO 1
      END IF 	 


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
	  

CC	 
C 27 June 2016
C The output argument "yaw_angle" has been added for providing the computed 
C values of the nominal yaw angle (YANGLE) and the eclipsing yaw angle (PHI)            
      yaw_angle(1) = YANGLE
      yaw_angle(2) = PHI
C
C 28 July 2016
C 'DET' and 'MURATE' have been included to the output arguments	  
CC



1     RETURN
C
      END
