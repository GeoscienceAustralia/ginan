!
      SUBROUTINE SRPFBOXW(REFF,YSAT,SUN,SVN,ACCEL)
!
! NAME       :  SRPFBOXW
!
! PURPOSE    :  COMPUTATION OF EARTH RADIATION PRESSURE ACTING ON A
!               BOW-WING SATELLITE
!
! PARAMETERS :
!         IN :  
!               REFF      : ACCELERATION IN REFERENCE FRAME:
!                           0 = INERTIAL
!                           1 = BODY FIXED (Z,Y,X)
!                           2 = SUN FIXED (D,Y,B)
!                           3 = ORBITAL (RADIAL, ALONG- AND CROSS-TRACK)
!               YSAT      : SATELLITE POSITION [m] AND VELOCITY [m/s] (INERTIAL), 
!                           (POSITION,VELOCITY) = (RX,RY,RZ,VX,VY,VZ)
!               SUN       : SUN POSITION VECTOR [m] (INERTIAL)
!               BLKID    : BLOCK NUMBER
!                             1 = GPS-I
!                             2 = GPS-II
!                             3 = GPS-IIA
!                             4 = GPS-IIR
!                             5 = GPS-IIR-A
!                             6 = GPS-IIR-B
!                             7 = GPS-IIR-M
!                             8 = GPS-IIF
!                             9 = GPS-IIIA  (Updated from acc_albedo_propboxw.f)
!                           101 = GLONASS
!                           102 = GLONASS-M (Added TAH 190702)
!                           103 = GLONASS-K (Added TAH 190702)
!                           201 = Galileo (IOV) (Added from acc_albedo_propboxw.f)
!                           202 = Galileo (FOC) (Added from acc_albedo_propboxw.f)
!                           301 = BDS GEO
!                           302 = BDS IGSO
!                           303 = BDS MEO
!                           401 = QZSS-1
!                           402 = QZSS-2I
!                           403 = QZSS-2G
!                           NB: when adding a new block number, check last block in this function!
!               SVN       : SPACE VEHICLE NUMBER          
!
!        OUT : ACCEL      : ACCELERATION VECTOR [m/s^2]
!
! AUTHOR     : C.J. RODRIGUEZ-SOLANO
!              rodriguez@bv.tum.de
!
! VERSION    : 1.0 (OCT 2010)
!
! CREATED    : 2010/10/18
!
!
!
      use mdl_param
      use mdl_num
      IMPLICIT NONE

      INTEGER*4 REFF,SBLK,INDB,SVN
      INTEGER*4 JFIRST,II,JJ,K
      INTEGER*4 LATK,LONK
      integer*4 i,j
!
      REAL*8 YSAT(6),SUN(3),FORCE(3),ACCEL(3)
!
!     EARTH AND SATELLITE PROPERTIES
      REAL*8 AREAS(4,2),REFLS(4,2),DIFUS(4,2),ABSPS(4,2)
      REAL*8 AREA2S(4,2),REFL2S(4,2),DIFU2S(4,2),ABSP2S(4,2)
      REAL*8 REFLIRS(4,2),DIFUIRS(4,2),ABSPIRS(4,2)
!
!     ATTITUDE
      REAL*8 RADVEC(3),ALGVEC(3),CRSVEC(3),ESUN(3)
      REAL*8 RSUN,ABSPOS,ABSCRS,ABSY0V,Y0SAT(3)
      REAL*8 Z_SAT(3),D_SUN(3),Y_SAT(3),B_SUN(3),X_SAT(3)
      REAL*8 ATTSURF(3,4)
!
      REAL*8 ABSNCFVI,ABSNCFIR,ALBFAC,PHASEVI,PHASEIR
      REAL*8 NCFVEC(3)
      REAL*8 D_ANG,LATIND,LONIND
      REAL*8 D_AREA,LAT_IN,LON_IN,DIST2,ABSDIST
      REAL*8 COSLAT,PSI
      REAL*8 V_NS(3),V_INS(3),V_DIST(3),V_SAT(3)
      REAL*8 COS_IN,COS_RE,PSIDOT,ABSSUN
      REAL*8 REFL_CF,EMIT_CF,E_REFL,E_EMIT
      REAL*8 FORCE_LL(3),FREF(3)
      REAL*8 ANTFORCE,ANTPOW,MJD
!
      
! Initialization of force vector
      FORCE(1) = 0D0
      FORCE(2) = 0D0
      FORCE(3) = 0D0

      ACCEL(1) = 0D0
      ACCEL(2) = 0D0
      ACCEL(3) = 0D0

! --------------------------
! NOMINAL SATELLITE ATTITUDE
! --------------------------

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



!        DISTANCE FROM SATELLITE TO SUN
         RSUN = DSQRT((YSAT(1)-SUN(1))**2+(YSAT(2)-SUN(2))**2+ &
                 (YSAT(3)-SUN(3))**2)

!        D VECTOR AND Z VECTOR
         DO K=1,3
            D_SUN(K) = (SUN(K)-YSAT(K))/RSUN
            Z_SAT(K) = -YSAT(K)/ABSPOS
         ENDDO

!        Y VECTOR
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


! ---------------------------- 
! OPTICAL PROPERTIES PER BLOCK
! ----------------------------

         INDB = 0
         IF(BLKID.LE.10)THEN
            INDB = BLKID
         ELSEIF(BLKID.GT.100 .and. BLKID.lt.200 )THEN
            INDB = BLKID - 90
! MOD TAH 190722: Added Galileo
         ELSEIF(BLKID.gt.200 .and. BLKID.lt.300) then
            indb = blkid - 180
! MOD SCM 191219 Added BDS & QZSS
         ELSEIF (BLKID.gt.300 .and. BLKID.lt.400) then
            indb = blkid - 270
         ELSEIF (BLKID.gt.400) then
            indb = BLKID - 360
         ENDIF
         if ((indb .ne. 0) .and. (known_blkids(indb) .ne. 0)) then
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
         else
              AREAS = 0.d0
              REFLS = 0.d0
              DIFUS = 0.d0
              ABSPS = 0.d0

              AREA2S = 0.d0
              REFL2S = 0.d0
              DIFU2S = 0.d0
              ABSP2S = 0.d0

              REFLIRS = 0.d0
              DIFUIRS = 0.d0
              ABSPIRS = 0.d0
         endif


! ----------------------
! EARTH RADIATION MODELS (This part is modified for SRP modeling)
! ----------------------

!         NCFVEC(1) = RADVEC(1)
!         NCFVEC(2) = RADVEC(2)
!         NCFVEC(3) = RADVEC(3)
!         ALBFAC = (PI*TOA**2)*(S0/C)/(ABSPOS**2)
!         PHASEVI = (2*ALB/(3*PI**2))*((PI-PSI)*DCOS(PSI)+DSIN(PSI))
!         PHASEIR = (1-ALB)/(4*PI)
!         ABSNCFVI = ALBFAC*PHASEVI
!         ABSNCFIR = ALBFAC*PHASEIR

!     CHANGE THE RADIATION DIRECTION TO SUN->SAT
         NCFVEC(1) = (-1.d0)*D_SUN(1)
         NCFVEC(2) = (-1.d0)*D_SUN(2)
         NCFVEC(3) = (-1.d0)*D_SUN(3)
         ALBFAC = (S0/Cslight)
         ABSNCFVI = ALBFAC*1.d0
         ABSNCFIR = ALBFAC*1.d0


         CALL SURFBOXW(AREAS,REFLS,DIFUS,ABSPS, &
                          AREA2S,REFL2S,DIFU2S,ABSP2S, &
                          REFLIRS,DIFUIRS,ABSPIRS, &
                          ABSNCFVI,ABSNCFIR,NCFVEC,ATTSURF,FORCE)
              
!         print *,'In SRPFBOXW blk ',indb
!         write(*,*) 'areas ',((AREAS(i,j),i=1,4),j=1,2)
!         write(*,*) 'refls ', ((REFLS(i,j),i=1,4),j=1,2)
!         write(*,*) 'difus ', ((DIFUS(i,j),i=1,4),j=1,2)
!         write(*,*) 'absps ', ((ABSPS(i,j),i=1,4),j=1,2)                     
!         write(*,*) 'areas2s ',((AREA2S(i,j),i=1,4),j=1,2)
!         write(*,*) 'albfac, C, S0', ALBFAC, C, S0
!         write(*,*) 'refl2s ', ((REFL2S(i,j),i=1,4),j=1,2)
!         write(*,*) 'difu2s ',  ((DIFU2S(i,j),i=1,4),j=1,2)
!         write(*,*) 'absp2s',   ((ABSP2S(i,j),i=1,4),j=1,2)
!         write(*,*) 'reflirs ', ((REFLIRS(i,j),i=1,4),j=1,2)
!         write(*,*) 'difuirs ', ((DIFUIRS(i,j),i=1,4),j=1,2)
!         write(*,*) 'abspirs ', (( ABSPIRS(i,j),i=1,4),j=1,2) 
!         write(*,*) 'absncfvi ',ABSNCFVI
!         write(*,*) 'absncfir ' ,ABSNCFIR
!         write(*,*) 'ncfvec ',  ( NCFVEC(i),i=1,3)        
!         write(*,*) 'attsurf ', (( ATTSURF(i,j),i=1,4),j=1,4)
!         write(*,*) 'force ', (FORCE(i),i=1,3)


      IF(REFF.GT.0)THEN
         DO K=1,3
            FREF(K) = 0D0
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
