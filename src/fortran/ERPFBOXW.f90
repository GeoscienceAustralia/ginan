      SUBROUTINE ERPFBOXW(ERM,ANT,GRD,REFF,YSAT,SUN,KAPPA,MONTH, &
                          SVN,MJD,ACCEL)
!! NAME       :  ERPFBOXW
!!
!! PURPOSE    :  COMPUTATION OF EARTH RADIATION PRESSURE ACTING ON A
!!               BOW-WING SATELLITE
!!
!! PARAMETERS :
!!         IN :  ERM       : EARTH RADIATION MODEL
!!                           0 = NONE
!!                           1 = EARTH RADIATION PRESSURE (ANALYTICAL)
!!                           2 = EARTH RADIATION PRESSURE (CERES DATA)
!!               CERES DAT : CERES DATA (EXTERNAL ASCII FILES) -> SET PATH IN DATPATH
!!               ANT       : 0 = NO ANTENNA THRUST
!!                         : 1 = WITH ANTENNA THRUST
!!               GRD       : 1 = 2.5 degree grid
!!                         : 2 = 5.0 degree grid
!!                         : 3 = 10  degree grid
!!               REFF      : ACCELERATION IN REFERENCE FRAME:
!!                           0 = INERTIAL
!!                           1 = BODY FIXED (Z,Y,X)
!!                           2 = SUN FIXED (D,Y,B)
!!                           3 = ORBITAL (RADIAL, ALONG- AND CROSS-TRACK)
!!               YSAT      : SATELLITE POSITION [m] AND VELOCITY [m/s] (INERTIAL), 
!!                           (POSITION,VELOCITY) = (RX,RY,RZ,VX,VY,VZ)
!!               SUN       : SUN POSITION VECTOR [m] (INERTIAL)
!!               KAPPA     : ROTATION MATRIX FROM INERTIAL TO EARTH-FIXED
!!                           REFERENCE FRAME (FOR ERM=2)
!!               MONTH     : MONTH NUMBER (1 ... 12), FOR ERM=2
!!               BLKID     : BLOCK Identifier
!!                             1 = GPS-I
!!                             2 = GPS-II
!!                             3 = GPS-IIA
!!                             4 = GPS-IIR
!!                             5 = GPS-IIR-A
!!                             6 = GPS-IIR-B
!!                             7 = GPS-IIR-M
!!                             8 = GPS-IIF
!!                             9 = GPS-IIIA  (Updated from acc_albedo_propboxw.f)
!!                           101 = GLONASS
!!                           102 = GLONASS-M (Added TAH 190702)
!!                           103 = GLONASS-K (Added TAH 190702)
!!                           201 = Galileo (IOV) (Added from acc_albedo_propboxw.f)
!!                           202 = Galileo (FOC) (Added from acc_albedo_propboxw.f)
!!                           301 = BDS GEO
!!                           302 = BDS IGSO
!!                           303 = BDS MEO
!!                           401 = QZSS-1
!!                           402 = QZSS-2I
!!                           403 = QZSS-2G
!!                           NB: when adding a new block number, check last block in this function!
!!               SVN       : SPACE VEHICLE NUMBER          
!!               MJD       : MODIFIED JULIAN DAY
!!
!!        OUT : ACCEL      : ACCELERATION VECTOR [m/s^2]
!!
!! AUTHOR     : C.J. RODRIGUEZ-SOLANO
!!              rodriguez@bv.tum.de
!!
!! VERSION    : 1.0 (OCT 2010)
!!
!! CREATED    : 2010/10/18
!!
!!
!!
      use mdl_param
      use mdl_num
      IMPLICIT NONE

!!
      INTEGER*4 ERM,ANT,GRD,REFF,SBLK,INDB,MONTH,SVN
      INTEGER*4 IFIRST,II,JJ,K,LFNLOC,IOSTAT,LENPATH
      INTEGER*4 LATK,LONK,GRDCER
      integer*4 i,j
!!
      REAL*8 YSAT(6),SUN(3),FORCE(3),ACCEL(3)
!!
!!    EARTH AND SATELLITE PROPERTIES
      REAL*8 AREAS(4,2),REFLS(4,2),DIFUS(4,2),ABSPS(4,2)
      REAL*8 AREA2S(4,2),REFL2S(4,2),DIFU2S(4,2),ABSP2S(4,2)
      REAL*8 REFLIRS(4,2),DIFUIRS(4,2),ABSPIRS(4,2)
!!
!!    ATTITUDE
      REAL*8 RADVEC(3),ALGVEC(3),CRSVEC(3),ESUN(3)
      REAL*8 RSUN,ABSPOS,ABSCRS,ABSY0V,Y0SAT(3)
      REAL*8 Z_SAT(3),D_SUN(3),Y_SAT(3),B_SUN(3),X_SAT(3)
      REAL*8 ATTSURF(3,4)
!!
      REAL*8 ABSNCFVI,ABSNCFIR,ALBFAC,PHASEVI,PHASEIR
      REAL*8 NCFVEC(3)
      REAL*8 D_ANG,GRDANG,LATIND,LONIND
      REAL*8 D_AREA,LAT_IN,LON_IN,DIST2,ABSDIST
      REAL*8 COSLAT,PSI
      REAL*8 V_NS(3),V_INS(3),V_DIST(3),V_SAT(3)
      REAL*8 COS_IN,COS_RE,PSIDOT,ABSSUN
      REAL*8 REFL_CF,EMIT_CF,E_REFL,E_EMIT
      REAL*8 FORCE_LL(3),FREF(3)
      REAL*8 ANTFORCE,ANTPOW,MJD
      REAL*8 KAPPA(3,3)
      REAL*8 S1

!!Initialization of force vector
      FORCE(1) = 0.D0
      FORCE(2) = 0.D0
      FORCE(3) = 0.D0

      ACCEL(1) = 0.D0
      ACCEL(2) = 0.D0
      ACCEL(3) = 0.D0

!!--------------------------
!!NOMINAL SATELLITE ATTITUDE
!!--------------------------

      ABSPOS = DSQRT(YSAT(1)**2+YSAT(2)**2+YSAT(3)**2)
      DO K=1,3
         RADVEC(K) = YSAT(K)/ABSPOS
      ENDDO
      
      CRSVEC(1) = YSAT(2)*YSAT(6)-YSAT(3)*YSAT(5)
      CRSVEC(2) = YSAT(3)*YSAT(4)-YSAT(1)*YSAT(6)
      CRSVEC(3) = YSAT(1)*YSAT(5)-YSAT(2)*YSAT(4)
      ABSCRS = DSQRT(CRSVEC(1)**2+CRSVEC(2)**2+CRSVEC(3)**2)
      DO K=1,3
         CRSVEC(K) = CRSVEC(K)/ABSCRS
      ENDDO

      ALGVEC(1) = CRSVEC(2)*RADVEC(3)-CRSVEC(3)*RADVEC(2)
      ALGVEC(2) = CRSVEC(3)*RADVEC(1)-CRSVEC(1)*RADVEC(3)
      ALGVEC(3) = CRSVEC(1)*RADVEC(2)-CRSVEC(2)*RADVEC(1) 

      IF(ERM.GT.0)THEN
!!
!!       DISTANCE FROM SATELLITE TO SUN
         RSUN = DSQRT((YSAT(1)-SUN(1))**2+(YSAT(2)-SUN(2))**2+ &
                 (YSAT(3)-SUN(3))**2)

!!       D VECTOR AND Z VECTOR
         DO K=1,3
            D_SUN(K) = (SUN(K)-YSAT(K))/RSUN
            Z_SAT(K) = -YSAT(K)/ABSPOS
         ENDDO

!!       Y VECTOR
         Y0SAT(1) = Z_SAT(2)*D_SUN(3)-Z_SAT(3)*D_SUN(2)
         Y0SAT(2) = Z_SAT(3)*D_SUN(1)-Z_SAT(1)*D_SUN(3)
         Y0SAT(3) = Z_SAT(1)*D_SUN(2)-Z_SAT(2)*D_SUN(1)
         ABSY0V = DSQRT(Y0SAT(1)**2 + Y0SAT(2)**2 + Y0SAT(3)**2)
         DO K=1,3
            Y_SAT(K) = Y0SAT(K)/ABSY0V
         ENDDO

!        B VECTOR
         B_SUN(1) = Y_SAT(2)*D_SUN(3) - Y_SAT(3)*D_SUN(2)
         B_SUN(2) = Y_SAT(3)*D_SUN(1) - Y_SAT(1)*D_SUN(3)
         B_SUN(3) = Y_SAT(1)*D_SUN(2) - Y_SAT(2)*D_SUN(1)

!        X VECTOR
         X_SAT(1) = Y_SAT(2)*Z_SAT(3) - Y_SAT(3)*Z_SAT(2)
         X_SAT(2) = Y_SAT(3)*Z_SAT(1) - Y_SAT(1)*Z_SAT(3)
         X_SAT(3) = Y_SAT(1)*Z_SAT(2) - Y_SAT(2)*Z_SAT(1)

         DO K=1,3
            ATTSURF(K,1) = Z_SAT(K)
            ATTSURF(K,2) = Y_SAT(K)
            ATTSURF(K,3) = X_SAT(K)
            ATTSURF(K,4) = D_SUN(K)
         ENDDO

! =============================================================================
! FORM A ORBIT-NORMAL ATTITUDE FOR BDS GEO SATELLITE (ONLY FOR TESTING PURPOSE).
! BECAUSE THE IMPACT OF EARTH ALBEDO IS MOSTLY ON THE RADIAL DIRECTION,
! THE CONTRIBUTIONS FROM X AND Y SIDES ARE ZERO NO MATTER WHAT THE
! SATELLITE IS OPERATED WITH YS OR ON ATTITUDE. THIS IS BECAUSE THE
! EARTH RADIATION DIRECTION (RADIAL DIRECTION) IS PERPENDICULAR TO THE
! OTHER TWO AXES (Y AND X). THE ACCELERATION IS ONLY
! RESULTED FROM THE Z SIDE AND THE SOLAR PANEL. IN A CASE OF SMALL BETA ANGLES, 
! THE DIFFERENCE IN ACCELERATION AT THE SOLAR PANEL BETWEEN YS AND ON ATTITUDES 
! IS A COSINE FUNCTION OF THE BETA ANGLE. AS A RESULT, THE IMPACT
! OF EARTH ALBEDO ON YAW-STERRING IS SIMILAR TO THAT ON ORBIT-NORMAL AT
! THE SMALL BETA ANGLES. (04-11-2019 Dr. TZUPANG TSENG)

! FOR BDS IGSO ECLIPSING PERIOD (BETA < 4 DEG), THE CONTRIBUTION OF
! COSINE(4DEG) FROM THE SOLAR PANEL IN ON ATTITUDE IS SIMILAR TO THAT IN
! YS ATTITUDE. (05-11-2019 Dr. TZUPANG TSENG)
! =============================================================================
         IF(BLKID == 301)THEN

!        Y VECTOR
         Y_SAT(1) = -CRSVEC(1) 
         Y_SAT(2) = -CRSVEC(2) 
         Y_SAT(3) = -CRSVEC(3)

!        X VECTOR
         X_SAT(1) = Y_SAT(2)*Z_SAT(3) - Y_SAT(3)*Z_SAT(2)
         X_SAT(2) = Y_SAT(3)*Z_SAT(1) - Y_SAT(1)*Z_SAT(3)
         X_SAT(3) = Y_SAT(1)*Z_SAT(2) - Y_SAT(2)*Z_SAT(1)

!        B VECTOR
         B_SUN(1) = Y_SAT(2)*D_SUN(3) - Y_SAT(3)*D_SUN(2)
         B_SUN(2) = Y_SAT(3)*D_SUN(1) - Y_SAT(1)*D_SUN(3)
         B_SUN(3) = Y_SAT(1)*D_SUN(2) - Y_SAT(2)*D_SUN(1)

!        D VECTOR RESULTED FROM ORBIT-NORMAL ATTITUDE
         D_SUN(1) = Y_SAT(2)*B_SUN(3) - Y_SAT(3)*B_SUN(2)
         D_SUN(2) = Y_SAT(3)*B_SUN(1) - Y_SAT(1)*B_SUN(3)
         D_SUN(3) = Y_SAT(1)*B_SUN(2) - Y_SAT(2)*B_SUN(1)

         DO K=1,3
            ATTSURF(K,1) = Z_SAT(K)
            ATTSURF(K,2) = Y_SAT(K)
            ATTSURF(K,3) = X_SAT(K)
            ATTSURF(K,4) = D_SUN(K)
         ENDDO 

         ENDIF
      ENDIF

! ---------------------------- 
! OPTICAL PROPERTIES PER BLOCK
! ----------------------------

      INDB=1

      IF(ERM.GT.0)THEN
         IF(BLKID.LE.10)THEN
            INDB = BLKID
         ELSEIF(BLKID.GT.100 .and. BLKID.lt.200 )THEN
            INDB = BLKID - 90
! MOD TAH 190722: Added Galileo
         ELSEIF(BLKID.gt.200 .and. BLKID.lt.300 ) then
            indb = blkid - 180
! MOD SCM 191219: Added BDS
         ELSEIF(BLKID.gt.300 .and. BLKID.lt.400 ) then
            indb = blkid - 270
! MOD SCM 191219: Added QZSS
         ELSEIF(BLKID.gt.400) then
            indb = blkid - 360
         ENDIF
         DO II = 1,4
            DO JJ = 1,2
               AREAS(II,JJ) = AREA(II,JJ,INDB)
               REFLS(II,JJ) = REFL(II,JJ,INDB)
               DIFUS(II,JJ) = DIFU(II,JJ,INDB)
               ABSPS(II,JJ) = ABSP(II,JJ,INDB)

               AREA2S(II,JJ) = AREA2(II,JJ,INDB)
               REFL2S(II,JJ) = REFL2(II,JJ,INDB)
               DIFU2S(II,JJ) = DIFU2(II,JJ,INDB)
               ABSP2S(II,JJ) = ABSP2(II,JJ,INDB)

               REFLIRS(II,JJ) = REFLIR(II,JJ,INDB)
               DIFUIRS(II,JJ) = DIFUIR(II,JJ,INDB)
               ABSPIRS(II,JJ) = ABSPIR(II,JJ,INDB)
            ENDDO
         ENDDO
      ENDIF

! ----------------------
! EARTH RADIATION MODELS
! ----------------------

      IF(ERM.GT.0)THEN
         ABSSUN = DSQRT(SUN(1)**2 + SUN(2)**2 + SUN(3)**2)
         DO K=1,3
            ESUN(K) = SUN(K)/ABSSUN
         ENDDO

         PSIDOT = ESUN(1)*RADVEC(1)+ESUN(2)*RADVEC(2)+ESUN(3)*RADVEC(3)
         IF(DABS(PSIDOT).GT.(1D0-1D-6))THEN
            PSI = 0D0
         ELSE
            PSI = DACOS(PSIDOT)
         ENDIF
         S1 = S0*(AU/ABSSUN)**2
      ENDIF

!     ANALYTICAL MODEL
      IF(ERM.EQ.1)THEN

         NCFVEC(1) = RADVEC(1)
         NCFVEC(2) = RADVEC(2)
         NCFVEC(3) = RADVEC(3)
         ALBFAC = (PI_global*TOA**2)*(S1/cslight)/(ABSPOS**2)
         PHASEVI = (2*ALB/(3*PI_global**2))*((PI_global-PSI)*DCOS(PSI)+DSIN(PSI))
         PHASEIR = (1-ALB)/(4*PI_global)
         ABSNCFVI = ALBFAC*PHASEVI
         ABSNCFIR = ALBFAC*PHASEIR


         CALL SURFBOXW(AREAS,REFLS,DIFUS,ABSPS, &
                          AREA2S,REFL2S,DIFU2S,ABSP2S, &
                          REFLIRS,DIFUIRS,ABSPIRS, &
                          ABSNCFVI,ABSNCFIR,NCFVEC,ATTSURF,FORCE_LL)
              
         FORCE = FORCE_LL

!d         print *,'In ERPFBOXW blk ',indb
!d         write(*,*) 'areas ', ((AREAS(i,j),i=1,4),j=1,2)
!d         write(*,*) 'refls ', ((REFLS(i,j),i=1,4),j=1,2)
!d         write(*,*) 'difus ', ((DIFUS(i,j),i=1,4),j=1,2)
!d         write(*,*) 'absps ', ((ABSPS(i,j),i=1,4),j=1,2)                     
!d         write(*,*) 'areas2s ',((AREA2S(i,j),i=1,4),j=1,2)
!d         write(*,*) 'refl2s ', ((REFL2S(i,j),i=1,4),j=1,2)
!d         write(*,*) 'difu2s ',  ((DIFU2S(i,j),i=1,4),j=1,2)
!d         write(*,*) 'absp2s',   ((ABSP2S(i,j),i=1,4),j=1,2)
!d         write(*,*) 'reflirs ', ((REFLIRS(i,j),i=1,4),j=1,2)
!d         write(*,*) 'difuirs ', ((DIFUIRS(i,j),i=1,4),j=1,2)
!d         write(*,*) 'abspirs ', (( ABSPIRS(i,j),i=1,4),j=1,2) 
!d         write(*,*) 'absncfvi ',ABSNCFVI
!d         write(*,*) 'absncfir ' ,ABSNCFIR
!d         write(*,*) 'ncfvec ',  ( NCFVEC(i),i=1,3)        
!d         write(*,*) 'attsurf ', (( ATTSURF(i,j),i=1,4),j=1,4)
!d         write(*,*) 'force ', (FORCE(i),i=1,3)

!     NUMERICAL MODEL (CERES DATA)
      ELSEIF(ERM.EQ.2)THEN

         ABSSUN = DSQRT(SUN(1)**2 + SUN(2)**2 + SUN(3)**2)
         DO K=1,3
            ESUN(K) = SUN(K)/ABSSUN
         ENDDO

         DO LATK = 1,LATKMX
            DO LONK = 1,LONKMX

               D_AREA = D_AREA_ALL(LATK,LONK)
               V_NS(1) = V_NS_ALL(LATK,LONK,1)
               V_NS(2) = V_NS_ALL(LATK,LONK,2)
               V_NS(3) = V_NS_ALL(LATK,LONK,3)

               DO II=1,3
                  V_INS(II)=0D0
                  DO JJ=1,3
                     V_INS(II) = V_INS(II) + KAPPA(JJ,II)*V_NS(JJ)
                  ENDDO
               ENDDO

!              Distance and direction from point in the Earth to satellite
               V_DIST(1) = YSAT(1)-TOA*V_INS(1) 
               V_DIST(2) = YSAT(2)-TOA*V_INS(2)
               V_DIST(3) = YSAT(3)-TOA*V_INS(3)
               DIST2 = V_DIST(1)**2 +V_DIST(2)**2 +V_DIST(3)**2
               ABSDIST = DSQRT(DIST2)
               V_SAT(1) = V_DIST(1)/ABSDIST
               V_SAT(2) = V_DIST(2)/ABSDIST
               V_SAT(3) = V_DIST(3)/ABSDIST

!              Cosine of angles of incident and reflected radiation
               COS_IN = ESUN(1)*V_INS(1) + ESUN(2)*V_INS(2) &
                      + ESUN(3)*V_INS(3)
               COS_RE =V_SAT(1)*V_INS(1) +V_SAT(2)*V_INS(2) &
                      +V_SAT(3)*V_INS(3)

               IF(COS_RE.GE.0)THEN

!              Reflectivity and emissivity coefficients
                  REFL_CF = CERGRE(LATK,LONK)
                  EMIT_CF = CERGEM(LATK,LONK)

!                 Reflected Irradiance
                  IF(COS_IN.GE.0)THEN
                     E_REFL=(REFL_CF/(PI_global*DIST2))*COS_RE*COS_IN*S1*D_AREA
                  ELSE
                     E_REFL=0D0
                  ENDIF

!                 Emitted Irradiance
                  E_EMIT = (EMIT_CF/(4*PI_global*DIST2))*COS_RE*S1*D_AREA

!                 Non-conservative force
                  ABSNCFVI = E_REFL/Cslight
                  ABSNCFIR = E_EMIT/Cslight
                  NCFVEC(1) = V_SAT(1)
                  NCFVEC(2) = V_SAT(2)
                  NCFVEC(3) = V_SAT(3)

                  CALL SURFBOXW(AREAS,REFLS,DIFUS,ABSPS, &
                          AREA2S,REFL2S,DIFU2S,ABSP2S, &
                          REFLIRS,DIFUIRS,ABSPIRS, &
                          ABSNCFVI,ABSNCFIR,NCFVEC,ATTSURF,FORCE_LL)

                  FORCE(1) = FORCE(1) + FORCE_LL(1)
                  FORCE(2) = FORCE(2) + FORCE_LL(2)
                  FORCE(3) = FORCE(3) + FORCE_LL(3)
               ENDIF

            ENDDO
         ENDDO

      ENDIF


!     ANTENNA POWER OF GPS SATELLITES (IN WATTS)
!     IGS MODEL (JIM RAY, 2011)

!     NO ANTENNA POWER INFORMATION FOR GLONASS SATELLITES
!     initialisation of ANTPOW
      ANTPOW = 0.d0

!     GPS BLOCK IIA (ASSUMED THE SAME FOR BLOCK I AND II) 
      IF(BLKID.LE.3)THEN
         ANTPOW = 76.D0

!     GPS BLOCK IIR
      ELSEIF((BLKID.GE.4).AND.(BLKID.LE.6))THEN
         ANTPOW = 85.D0

!     GPS BLOCK IIR-M
      ELSEIF(BLKID.EQ.7)THEN
         ANTPOW = 198.D0

!     GPS BLOCK IIF
      ELSEIF(BLKID.EQ.8)THEN
         ANTPOW = 249.D0
         IF((SVN.EQ.62).AND.(MJD.GE.55656.D0))THEN
!           NO M-CODE FOR SVN62/PRN25 STARTING 05APR2011; NANU 2011026
            ANTPOW = 154.D0
         ENDIF
      ENDIF



!     NAVIGATION ANTENNA THRUST (SIMPLE MODEL)
      IF((ANT.EQ.1).AND.(BLKID.LT.100))THEN
         ANTFORCE = ANTPOW/Cslight
         FORCE(1) = FORCE(1) + ANTFORCE*RADVEC(1)
         FORCE(2) = FORCE(2) + ANTFORCE*RADVEC(2)
         FORCE(3) = FORCE(3) + ANTFORCE*RADVEC(3)
      ENDIF

      IF((REFF.GT.0).AND.((ERM.GT.0).OR.(ANT.EQ.1)))THEN
         DO K=1,3
            FREF(K) = 0.D0
         ENDDO

!        FORCE IN BODY-FIXED REFERENCE FRAME
         IF(REFF.EQ.1)THEN
            DO K=1,3
               FREF(1) = FREF(1) + FORCE(K)*Z_SAT(K)
               FREF(2) = FREF(2) + FORCE(K)*Y_SAT(K)
               FREF(3) = FREF(3) + FORCE(K)*X_SAT(K)
            ENDDO

!        FORCE IN SUN-FIXED REFERENCE FRAME
         ELSEIF(REFF.EQ.2)THEN
            DO K=1,3
               FREF(1) = FREF(1) + FORCE(K)*D_SUN(K)
               FREF(2) = FREF(2) + FORCE(K)*Y_SAT(K)
               FREF(3) = FREF(3) + FORCE(K)*B_SUN(K)
            ENDDO

!        FORCE IN ORBITAL REFERENCE FRAME
         ELSEIF(REFF.EQ.3)THEN
            DO K=1,3
               FREF(1) = FREF(1) + FORCE(K)*RADVEC(K)
               FREF(2) = FREF(2) + FORCE(K)*ALGVEC(K)
               FREF(3) = FREF(3) + FORCE(K)*CRSVEC(K)
            ENDDO
         ENDIF

         DO K=1,3
            FORCE(K) = FREF(K)
         ENDDO
      ENDIF

!     CONVERSION TO ACCELERATION
      DO K=1,3
         ACCEL(K) = FORCE(K)/MASS
      ENDDO
                    
!      write(*,*) 'force mass accel ' &
!        ,(force(i),i=1,3),mass,(accel(i),i=1,3)

      END SUBROUTINE
