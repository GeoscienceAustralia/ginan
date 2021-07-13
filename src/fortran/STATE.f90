      SUBROUTINE STATE(ET2,LIST,PV,PNUT)

!C
!C++++++++++++++++++++++++++++++++
!C
!C THIS SUBROUTINE READS AND INTERPOLATES THE JPL PLANETARY EPHEMERIS FILE
!C
!C     CALLING SEQUENCE PARAMETERS:
!C
!C     INPUT:
!C
!C         ET2   DP 2-WORD JULIAN EPHEMERIS EPOCH AT WHICH INTERPOLATION
!C               IS WANTED.  ANY COMBINATION OF ET2(1)+ET2(2) WHICH FALLS
!C               WITHIN THE TIME SPAN ON THE FILE IS A PERMISSIBLE EPOCH.
!C
!C                A. FOR EASE IN PROGRAMMING, THE USER MAY PUT THE
!C                   ENTIRE EPOCH IN ET2(1) AND SET ET2(2)=0.
!C
!C                B. FOR MAXIMUM INTERPOLATION ACCURACY, SET ET2(1) =
!C                   THE MOST RECENT MIDNIGHT AT OR BEFORE INTERPOLATION
!C                   EPOCH AND SET ET2(2) = FRACTIONAL PART OF A DAY
!C                   ELAPSED BETWEEN ET2(1) AND EPOCH.
!C
!C                C. AS AN ALTERNATIVE, IT MAY PROVE CONVENIENT TO SET
!C                   ET2(1) = SOME FIXED EPOCH, SUCH AS START OF INTEGRATION,
!C                   AND ET2(2) = ELAPSED INTERVAL BETWEEN THEN AND EPOCH.
!C
!C        LIST   12-WORD INTEGER ARRAY SPECIFYING WHAT INTERPOLATION
!C               IS WANTED FOR EACH OF THE BODIES ON THE FILE.
!C
!C                         LIST(I)=0, NO INTERPOLATION FOR BODY I
!C                                =1, POSITION ONLY
!C                                =2, POSITION AND VELOCITY
!C
!C               THE DESIGNATION OF THE ASTRONOMICAL BODIES BY I IS:
!C
!C                         I = 1: MERCURY
!C                           = 2: VENUS
!C                           = 3: EARTH-MOON BARYCENTER
!C                           = 4: MARS
!C                           = 5: JUPITER
!C                           = 6: SATURN
!C                           = 7: URANUS
!C                           = 8: NEPTUNE
!C                           = 9: PLUTO
!C                           =10: GEOCENTRIC MOON
!C                           =11: NUTATIONS IN LONGITUDE AND OBLIQUITY
!C                           =12: LUNAR LIBRATIONS (IF ON FILE)
!C
!C     OUTPUT:
!C
!C          PV   DP 6 X 11 ARRAY THAT WILL CONTAIN REQUESTED INTERPOLATED
!C               QUANTITIES (OTHER THAN NUTATION, STOERD IN PNUT).  
!C               THE BODY SPECIFIED BY LIST(I) WILL HAVE ITS
!C               STATE IN THE ARRAY STARTING AT PV(1,I).  
!C               (ON ANY GIVEN CALL, ONLY THOSE WORDS IN 'PV' WHICH ARE 
!C                AFFECTED BY THE  FIRST 10 'LIST' ENTRIES, AND BY LIST(12)
!C                IF LIBRATIONS ARE ON THE FILE, ARE SET.  
!C                THE REST OF THE 'PV' ARRAYIS UNTOUCHED.)  
!C               THE ORDER OF COMPONENTS STARTING IN PV(1,I) IS: X,Y,Z,DX,DY,DZ.
!C
!C               ALL OUTPUT VECTORS ARE REFERENCED TO THE EARTH MEAN
!C               EQUATOR AND EQUINOX OF J2000 IF THE DE NUMBER IS 200 OR
!C               GREATER; OF B1950 IF THE DE NUMBER IS LESS THAN 200. 
!C
!C               THE MOON STATE IS ALWAYS GEOCENTRIC; THE OTHER NINE STATES 
!C               ARE EITHER HELIOCENTRIC OR SOLAR-SYSTEM BARYCENTRIC, 
!C               DEPENDING ON THE SETTING OF COMMON FLAGS (SEE BELOW).
!C
!C               LUNAR LIBRATIONS, IF ON FILE, ARE PUT INTO PV(K,11) IF
!C               LIST(12) IS 1 OR 2.
!C
!C         NUT   DP 4-WORD ARRAY THAT WILL CONTAIN NUTATIONS AND RATES,
!C               DEPENDING ON THE SETTING OF LIST(11).  THE ORDER OF
!C               QUANTITIES IN NUT IS:
!C
!C                        D PSI  (NUTATION IN LONGITUDE)
!C                        D EPSILON (NUTATION IN OBLIQUITY)
!C                        D PSI DOT
!C                        D EPSILON DOT
!C
!C           *   STATEMENT # FOR ERROR RETURN, IN CASE OF EPOCH OUT OF
!C               RANGE OR I/O ERRORS.
!C
!C     COMMON AREA STCOMX:
!C
!C          KM   LOGICAL FLAG DEFINING PHYSICAL UNITS OF THE OUTPUT
!C               STATES. KM = .TRUE., KM AND KM/SEC
!C                          = .FALSE., AU AND AU/DAY
!C               DEFAULT VALUE = .FALSE.  (KM DETERMINES TIME UNIT
!C               FOR NUTATIONS AND LIBRATIONS.  ANGLE UNIT IS ALWAYS RADIANS.)
!C
!C        BARY   LOGICAL FLAG DEFINING OUTPUT CENTER.
!C               ONLY THE 9 PLANETS ARE AFFECTED.
!C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
!C                             = .FALSE. =\ CENTER IS SUN
!C               DEFAULT VALUE = .FALSE.
!C
!C       PVSUN   DP 6-WORD ARRAY CONTAINING THE BARYCENTRIC POSITION AND
!C               VELOCITY OF THE SUN.
!C
!C

	  
      USE mdl_planets
!      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      IMPLICIT NONE

      SAVE

      INTEGER OLDMAX
      PARAMETER ( OLDMAX = 400)
      INTEGER NMAX
      PARAMETER ( NMAX = 1000)

!C       DIMENSION ET2(2),PV(6,11),PNUT(4),T(2),PJD(4),BUF(1500),
!C      . SS(3),CVAL(NMAX),PVSUN(6)

      INTEGER LIST(12),IPT(3,13)

      LOGICAL FIRST
      DATA FIRST/.TRUE./

      CHARACTER*6 TTL(14,3),CNAM(NMAX)
      CHARACTER*80 NAMFIL

      LOGICAL KM,BARY

! ----------------------------------------------------------------------
      INTEGER IPT_i, IPT_j, SS_i, BUF_i, I, J
      INTEGER NRECL, KSIZE, IRECSZ, NCOEFFS, NRFILE
! ----------------------------------------------------------------------
      INTEGER NCON, NUMDE, NRL, NR
      DOUBLE PRECISION  AU,EMRAT,CVAL(NMAX), PVSUN(6),PV(6,11),PNUT(4)
      DOUBLE PRECISION  SS(3),ET2(2),S,PJD(4),tmp1,tmp2,T(2),AUFAC
      DOUBLE PRECISION  BUF(1500), SS_value                 
! ----------------------------------------------------------------------

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,NUMDE,NCON,IPT
      COMMON/CHRHDR/CNAM,TTL
      COMMON/STCOMX/KM,BARY,PVSUN

	  
! ----------------------------------------------------------------------
! Set Units: AU or KM
!C               STATES. KM = .TRUE., KM AND KM/SEC
!C                          = .FALSE., AU AND AU/DAY
      KM = .TRUE.
!C                        BARY = .TRUE. =\ CENTER IS SOLAR-SYSTEM BARYCENTER
!C                             = .FALSE. =\ CENTER IS SUN
!      BARY = .TRUE.
! ----------------------------------------------------------------------
	  
! ----------------------------------------------------------------------
      TTL = TTL_2
      CNAM = CNAM_2
      NCON = NCON_2
      AU = AU_2
      EMRAT = EMRAT_2
!      IPT = IPT_2      !Attention: False assignment: IPT(3,13), IPT(3,12)
      NUMDE = NUMDE_2
!      LPT = LPT_2
!      RPT = RPT_2
!      TPT = TPT_2
      CVAL = CVAL_2
! ----------------------------------------------------------------------
!      SS = SS_2  
      DO SS_i = 1 , 3
         SS_value = SS_2(SS_i)
         SS(SS_i) = SS_value
      END DO	 
! ----------------------------------------------------------------------  
! IPT_12: 
!       INTEGER  IPT (3,12)                         ! Pointers to number of coefficients for bodies
!       INTEGER  LPT(3)                             ! Pointer to number of coefficients for lunar librations
! Here: IPT(3,13)
      DO IPT_j = 1 , 12
	     DO IPT_i = 1 , 3
            IPT(IPT_i,IPT_j) = IPT_2(IPT_i,IPT_j)
         END DO   
      END DO
	  
      DO IPT_i = 1 , 3
         IPT(IPT_i,13) = LPT_2(IPT_i)
      END DO
! ----------------------------------------------------------------------
	  
!C
!C       ENTRY POINT - 1ST TIME IN, GET POINTER DATA, ETC., FROM EPH FILE
!C
      IF(FIRST) THEN
        FIRST=.FALSE.

!C ************************************************************************
!C ************************************************************************

!C THE USER MUST SELECT ONE OF THE FOLLOWING BY DELETING THE 'C' IN COLUMN 1

!C ************************************************************************

!C        CALL FSIZER1(NRECL,KSIZE,NRFILE,NAMFIL)
!C        CALL FSIZER2(NRECL,KSIZE,NRFILE,NAMFIL)
        CALL FSIZER3(NRECL,KSIZE,NRFILE,NAMFIL)
!      NRECL = 4                                                         
!      KSIZE = 2036 

      IF(NRECL .EQ. 0) WRITE(*,*)'  ***** FSIZER IS NOT WORKING *****'

!C ************************************************************************
!C ************************************************************************

      IRECSZ=NRECL*KSIZE
      NCOEFFS=KSIZE/2

!C         OPEN(NRFILE,
!C      *       FILE=NAMFIL,
!C      *       ACCESS='DIRECT',
!C      *       FORM='UNFORMATTED',
!C      *       RECL=IRECSZ,
!C      *       STATUS='OLD')

!C       READ(NRFILE,REC=1)TTL,(CNAM(K),K=1,OLDMAX),SS,NCON,AU,EMRAT,
!C      & ((IPT(I,J),I=1,3),J=1,12),NUMDE,(IPT(I,13),I=1,3)
!C      & ,(CNAM(L),L=OLDMAX+1,NCON)

!C       IF(NCON .LE. OLDMAX)THEN
!C         READ(NRFILE,REC=2)(CVAL(I),I=1,OLDMAX)
!C       ELSE
!C         READ(NRFILE,REC=2)(CVAL(I),I=1,NCON)
!C       ENDIF

!C ! ----------------------------------------------------------------------
	  
      NRL=0
  
      ENDIF
	  

!C       ********** MAIN ENTRY POINT **********

      IF(ET2(1) .EQ. 0.D0) RETURN

      S=ET2(1)-.5D0
      CALL SPLIT(S,PJD(1))
      CALL SPLIT(ET2(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)+.5D0
      PJD(2)=PJD(2)+PJD(4)
      CALL SPLIT(PJD(2),PJD(3))
      PJD(1)=PJD(1)+PJD(3)

!C       ERROR RETURN FOR EPOCH OUT OF RANGE

      IF(PJD(1)+PJD(4).LT.SS(1) .OR. PJD(1)+PJD(4).GT.SS(2)) GO TO 98

!C       CALCULATE RECORD # AND RELATIVE TIME IN INTERVAL

      NR=IDINT((PJD(1)-SS(1))/SS(3))+3
      IF(PJD(1).EQ.SS(2)) NR=NR-1

        tmp1 = DBLE(NR-3)*SS(3) + SS(1)
        tmp2 = PJD(1) - tmp1
        T(1) = (tmp2 + PJD(4))/SS(3)

!C       READ CORRECT RECORD IF NOT IN CORE

!C       IF(NR.NE.NRL) THEN
!C         NRL=NR
!C !        READ(NRFILE,REC=NR,ERR=99)(BUF(K),K=1,NCOEFFS)
!C         READ(UNIT=UNIT_asc,REC=NR,FMT='(E27.16)',IOSTAT=ios_asc)
!C      &      (BUF(K),K=1,NCOEFFS)                                             !777
!C            PRINT *,"JD Time epoch:PJD(1),SS(1)", PJD(1),SS(1)  
!C            PRINT *,"BUF(K),k=1,2,3", BUF(1),BUF(2),BUF(3)
!C            STOP
!C         IF (ios_asc /= 0) THEN
!C            PRINT *,"Error in reading JPLEPHasc REC:",NR, ios_asc
!C         END IF
!C       ENDIF
! ----------------------------------------------------------------------
!      DO BUF_i = 1 , NCOEFFS
!         BUF(BUF_i) = DB_array(NR-2,BUF_i)
!      END DO 
      BUF(1:NCOEFFS) = DB_array(NR-2,1:NCOEFFS)									!777
! ----------------------------------------------------------------------
	  
      IF(KM) THEN
      T(2)=SS(3)*86400.D0
      AUFAC=1.D0
      ELSE
      T(2)=SS(3)
      AUFAC=1.D0/AU
      ENDIF

!C   INTERPOLATE SSBARY SUN

      CALL INTERP(BUF(IPT(1,11)),T,IPT(2,11),3,IPT(3,11),2,PVSUN)

      DO I=1,6
      PVSUN(I)=PVSUN(I)*AUFAC
      ENDDO

!C   CHECK AND INTERPOLATE WHICHEVER BODIES ARE REQUESTED

      DO 4 I=1,10
      IF(LIST(I).EQ.0) GO TO 4

      CALL INTERP(BUF(IPT(1,I)),T,IPT(2,I),3,IPT(3,I), LIST(I),PV(1,I))

      DO J=1,6
       IF(I.LE.9 .AND. .NOT.BARY) THEN
       PV(J,I)=PV(J,I)*AUFAC-PVSUN(J)
       ELSE
       PV(J,I)=PV(J,I)*AUFAC
       ENDIF
      ENDDO

   4  CONTINUE

!C       DO NUTATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(11).GT.0 .AND. IPT(2,12).GT.0) CALL INTERP(BUF(IPT(1,12)),T,IPT(2,12),2,IPT(3,12),LIST(11),PNUT)

!C       GET LIBRATIONS IF REQUESTED (AND IF ON FILE)

      IF(LIST(12).GT.0 .AND. IPT(2,13).GT.0) CALL INTERP(BUF(IPT(1,13)),T,IPT(2,13),3,IPT(3,13),LIST(12),PV(1,11))

      RETURN

  98  WRITE(*,198)ET2(1)+ET2(2),SS(1),SS(2)
 198  FORMAT(' ***  Requested JED,',f12.2,' not within ephemeris limits,',2f12.2,'  ***')

      STOP

   99 WRITE(*,'(2F12.2,A80)')ET2,'ERROR RETURN IN STATE'

      STOP

      END
!C+++++++++++++++++++++++++++++
!C
