      SUBROUTINE PLEPH ( ET, NTARG, NCENT, RRD )
C
C++++++++++++++++++++++++++
C  NOTE : Over the years, different versions of PLEPH have had a fifth argument:
C  sometimes, an error return statement number; sometimes, a logical denoting
C  whether or not the requested date is covered by the ephemeris.  We apologize
C  for this inconsistency; in this present version, we use only the four necessary 
C  arguments and do the testing outside of the subroutine.
C
C     THIS SUBROUTINE READS THE JPL PLANETARY EPHEMERIS
C     AND GIVES THE POSITION AND VELOCITY OF THE POINT 'NTARG'
C     WITH RESPECT TO 'NCENT'.
C
C     CALLING SEQUENCE PARAMETERS:
C
C       ET = D.P. JULIAN EPHEMERIS DATE AT WHICH INTERPOLATION
C            IS WANTED.
C
C       ** NOTE THE ENTRY DPLEPH FOR A DOUBLY-DIMENSIONED TIME **
C          THE REASON FOR THIS OPTION IS DISCUSSED IN THE 
C          SUBROUTINE STATE
C
C     NTARG = INTEGER NUMBER OF 'TARGET' POINT.
C
C     NCENT = INTEGER NUMBER OF CENTER POINT.
C
C            THE NUMBERING CONVENTION FOR 'NTARG' AND 'NCENT' IS:
C
C                1 = MERCURY           8 = NEPTUNE
C                2 = VENUS             9 = PLUTO
C                3 = EARTH            10 = MOON
C                4 = MARS             11 = SUN
C                5 = JUPITER          12 = SOLAR-SYSTEM BARYCENTER
C                6 = SATURN           13 = EARTH-MOON BARYCENTER
C                7 = URANUS           14 = NUTATIONS (LONGITUDE AND OBLIQ)
C                            15 = LIBRATIONS, IF ON EPH FILE
C
C             (IF NUTATIONS ARE WANTED, SET NTARG = 14. FOR LIBRATIONS,
C              SET NTARG = 15. SET NCENT=0.)
C
C      RRD = OUTPUT 6-WORD D.P. ARRAY CONTAINING POSITION AND VELOCITY
C            OF POINT 'NTARG' RELATIVE TO 'NCENT'. THE UNITS ARE AU AND
C            AU/DAY. FOR LIBRATIONS THE UNITS ARE RADIANS AND RADIANS
C            PER DAY. IN THE CASE OF NUTATIONS THE FIRST FOUR WORDS OF
C            RRD WILL BE SET TO NUTATIONS AND RATES, HAVING UNITS OF
C            RADIANS AND RADIANS/DAY.
C
C            The option is available to have the units in km and km/sec.
C            For this, set km=.true. in the STCOMX common block.
C

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)

      INTEGER NMAX
      PARAMETER (NMAX = 1000)

      DIMENSION RRD(6),ET2Z(2),ET2(2),PV(6,13)
      DIMENSION PVST(6,11),PNUT(4)
      DIMENSION SS(3),CVAL(NMAX),PVSUN(6),ZIPS(2)
      DATA ZIPS/2*0.d0/

      LOGICAL BSAVE,KM,BARY
      LOGICAL FIRST
      DATA FIRST/.TRUE./

      INTEGER LIST(12),IPT(39),DENUM

      COMMON/EPHHDR/CVAL,SS,AU,EMRAT,DENUM,NCON,IPT

      COMMON/STCOMX/KM,BARY,PVSUN

C     INITIALIZE ET2 FOR 'STATE' AND SET UP COMPONENT COUNT
C
      ET2(1)=ET
      ET2(2)=0.D0
      GO TO 11

C     ENTRY POINT 'DPLEPH' FOR DOUBLY-DIMENSIONED TIME ARGUMENT 
C          (SEE THE DISCUSSION IN THE SUBROUTINE STATE)

      ENTRY DPLEPH(ET2Z,NTARG,NCENT,RRD)
      
      ET2(1)=ET2Z(1)
      ET2(2)=ET2Z(2)

  11  IF(FIRST) CALL STATE(ZIPS,LIST,PVST,PNUT)
      FIRST=.FALSE.

  96  IF(NTARG .EQ. NCENT) RETURN
     
      DO I=1,12
        LIST(I)=0
      ENDDO

C     CHECK FOR NUTATION CALL

      IF(NTARG.NE.14) GO TO 97
        IF(IPT(35).GT.0) THEN
          LIST(11)=2
          CALL STATE(ET2,LIST,PVST,PNUT)
          DO I=1,4
            RRD(I)=PNUT(I)
          ENDDO
          RRD(5) = 0.d0
          RRD(6) = 0.d0
          RETURN
        ELSE
          DO I=1,4
            RRD(I)=0.d0
          ENDDO
          WRITE(6,297)
  297     FORMAT(' *****  NO NUTATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF

C     CHECK FOR LIBRATIONS

  97  CONTINUE
      DO I=1,6
        RRD(I)=0.d0
      ENDDO

      IF(NTARG.NE.15) GO TO 98
        IF(IPT(38).GT.0) THEN
          LIST(12)=2
          CALL STATE(ET2,LIST,PVST,PNUT)
          DO I=1,6
            RRD(I)=PVST(I,11)
          ENDDO
          RETURN
        ELSE
          WRITE(6,298)
  298     FORMAT(' *****  NO LIBRATIONS ON THE EPHEMERIS FILE  *****')
          STOP
        ENDIF

C       FORCE BARYCENTRIC OUTPUT BY 'STATE'

  98  BSAVE=BARY
      BARY=.TRUE.

C       SET UP PROPER ENTRIES IN 'LIST' ARRAY FOR STATE CALL

      DO I=1,2
        K=NTARG
        IF(I .EQ. 2) K=NCENT
        IF(K .LE. 10) LIST(K)=2
        IF(K .EQ. 10) LIST(3)=2
        IF(K .EQ. 3) LIST(10)=2
        IF(K .EQ. 13) LIST(3)=2
      ENDDO

C       MAKE CALL TO STATE

      CALL STATE(ET2,LIST,PVST,PNUT)

      DO I=1,10
        DO J = 1,6
          PV(J,I) = PVST(J,I)
        ENDDO
      ENDDO

      IF(NTARG .EQ. 11 .OR. NCENT .EQ. 11) THEN
      DO I=1,6
        PV(I,11)=PVSUN(I)
      ENDDO
      ENDIF

      IF(NTARG .EQ. 12 .OR. NCENT .EQ. 12) THEN
        DO I=1,6
          PV(I,12)=0.D0
        ENDDO
      ENDIF

      IF(NTARG .EQ. 13 .OR. NCENT .EQ. 13) THEN
        DO I=1,6
          PV(I,13) = PVST(I,3)
        ENDDO
      ENDIF

      IF(NTARG*NCENT .EQ. 30 .AND. NTARG+NCENT .EQ. 13) THEN
        DO I=1,6
          PV(I,3)=0.D0
        ENDDO
        GO TO 99
      ENDIF

      IF(LIST(3) .EQ. 2) THEN
        DO I=1,6
          PV(I,3)=PVST(I,3)-PVST(I,10)/(1.D0+EMRAT)
        ENDDO
      ENDIF

      IF(LIST(10) .EQ. 2) THEN
        DO I=1,6
          PV(I,10) = PV(I,3)+PVST(I,10)
        ENDDO
      ENDIF

  99  DO I=1,6
        RRD(I)=PV(I,NTARG)-PV(I,NCENT)
      ENDDO

      BARY=BSAVE

      RETURN
      END
C+++++++++++++++++++++++++++++++++
C
