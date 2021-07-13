! ----------------------------------------------------------------------
! SUBROUTINE: BOXWINGINIT
! ----------------------------------------------------------------------
! Purpose:
!   Initialise global variables used in the BOXWING model
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------

      SUBROUTINE BOXWINGINIT(ERM, GRD, MONTH)
      use mdl_param
      use mdl_precision
      use mdl_num
      IMPLICIT NONE

      INTEGER (KIND=4) INDB, II, JJ, SBLK
      !GRD = 1 => 2.5 degree grid
      !GRD = 2 => 5 degree grid
      !GRD = 3 => 10 degree grid
      !ERM = 2 => more stuff gets done!
      INTEGER (KIND=4) ERM, GRD, MONTH
      INTEGER LFNLOC, IOSTAT, LENPATH
      INTEGER LATK, LONK, GRDCER

      REAL (KIND=prec_q) GRDANG, LATIND, LONIND, D_ANG, COSLAT
      REAL (KIND=prec_q) LAT_IN, LON_IN

      REAL (KIND=prec_q) AREAS(4,2), REFLS(4,2)
      REAL (KIND=prec_q) DIFUS(4,2), ABSPS(4,2)
      REAL (KIND=prec_q) AREA2S(4,2), REFL2S(4,2)
      REAL (KIND=prec_q) DIFU2S(4,2), ABSP2S(4,2)
      REAL (KIND=prec_q) REFLIRS(4,2)
      REAL (KIND=prec_q) DIFUIRS(4,2), ABSPIRS(4,2)

      CHARACTER*200  DATPATH, FILREFL, FILEMIT
      CHARACTER*5000 LINE
      CHARACTER*25   ITEM

      DO II = 1, 10
        ! GPS BLKIDS
        if (II /= 10) then
                known_blkids(II) = II
        else
                known_blkids(II) = 0
        endif
        ! GLONASS BLKIDS
        if (II <= 3) then
                known_blkids(II+10) = 100+II
        else
                known_blkids(II+10) = 0
        endif
        !Galilleo BLKIDS
        if (II <= 2) then
                known_blkids(II+20) = 200+II
        else
                known_blkids(II+20) = 0
        endif
        !BDS BLKIDS
        if (II <= 3) then
                known_blkids(II+30) = 300+II
        else
                known_blkids(II+30) = 0
        endif
        !QZSS BLKIDS
        if (II <= 3) then
                known_blkids(II+40) = 400+II
        else
                known_blkids(II+40) = 0
        endif
      enddo

      DO II = 1, 12
        write(F_MONTH(II), '(I2.2)') II
      enddo

      LFNLOC=999

! DATA FILES PATH AND NAME (CHANGE TO LOCAL PATH)
      DATPATH = './CERES/'
            
! COMPLETE FILE NAMES
      II = LEN(DATPATH)
      DO WHILE (DATPATH(II:II).EQ.' ')
         II = II-1
      ENDDO
      LENPATH = II

      FILREFL = DATPATH(1:LENPATH) // 'REFLMO' // F_MONTH(MONTH)
      FILEMIT = DATPATH(1:LENPATH) // 'EMITMO' // F_MONTH(MONTH)

      AREA = 0.d0
      REFL = 0.d0
      DIFU = 0.d0
      ABSP = 0.d0

      AREA2 = 0.d0
      REFL2 = 0.d0
      DIFU2 = 0.d0
      ABSP2 = 0.d0

      REFLIR = 0.d0
      DIFUIR = 0.d0
      ABSPIR = 0.d0

      LATKMX = 0
      LONKMX = 0
      GRDNUM = 1.d0

      write (*,'(1x,a,1x,i2,/)') 'Earth Radiation Model (ERM): ', ERM

!     PROPERTIES FOR ALL SATELLITES BLOCKS
      DO INDB = 1,max_blk
          SBLK = known_blkids(INDB) 
          IF( SBLK.ne.0 )THEN 

             CALL PROPBOXW(SBLK,AREAS,REFLS,DIFUS,ABSPS,AREA2S,REFL2S, &
                           DIFU2S,ABSP2S,REFLIRS,DIFUIRS,ABSPIRS)

            DO II = 1,4
               DO JJ = 1,2
                  AREA(II,JJ,INDB) = AREAS(II,JJ)
                  REFL(II,JJ,INDB) = REFLS(II,JJ)
                  DIFU(II,JJ,INDB) = DIFUS(II,JJ)
                  ABSP(II,JJ,INDB) = ABSPS(II,JJ)

                  AREA2(II,JJ,INDB) = AREA2S(II,JJ)
                  REFL2(II,JJ,INDB) = REFL2S(II,JJ)
                  DIFU2(II,JJ,INDB) = DIFU2S(II,JJ)
                  ABSP2(II,JJ,INDB) = ABSP2S(II,JJ)

                  REFLIR(II,JJ,INDB) = REFLIRS(II,JJ)
                  DIFUIR(II,JJ,INDB) = DIFUIRS(II,JJ)
                  ABSPIR(II,JJ,INDB) = ABSPIRS(II,JJ)
               ENDDO
            ENDDO
            ENDIF
       ENDDO

       IF(ERM.EQ.2)THEN


!     REFLECTIVITY 
            CERES_R = 0
            OPEN(UNIT=LFNLOC,FILE=FILREFL,STATUS='UNKNOWN', &
                 FORM='FORMATTED',IOSTAT=IOSTAT)
            DO II=1,72
               READ(LFNLOC,"(A)")LINE
               DO JJ=1,144
                  ITEM = LINE((JJ-1)*25+1:JJ*25)
                  IF (INDEX(ITEM,'NaN') /= 0) THEN
                     CERES_R(II,JJ) = 0D0
                  ELSE
                     READ(ITEM,*) CERES_R(II,JJ)
                  ENDIF
               ENDDO
            ENDDO
            CLOSE(LFNLOC)
!     EMISSIVITY 
            CERES_E = 0
            OPEN(UNIT=LFNLOC,FILE=FILEMIT,STATUS='UNKNOWN', &
                 FORM='FORMATTED',IOSTAT=IOSTAT)
            DO II=1,72
               READ(LFNLOC,"(A)")LINE
               DO JJ=1,144
                  ITEM = LINE((JJ-1)*25+1:JJ*25)
                  IF (INDEX(ITEM,'NaN') /= 0) THEN
                     CERES_E(II,JJ) = 0D0
                  ELSE
                     READ(ITEM,*) CERES_E(II,JJ)
                  ENDIF
               ENDDO
            ENDDO
            CLOSE(LFNLOC)

!     PRE-INTEGRATION, INDEPENDENT OF SATELLITE POSITION
            GRDCER = 1
            GRDANG = 2.5D0
            LATIND = 36.5D0
            LONIND = 72.5D0
            IF(GRD.EQ.1)THEN
               GRDNUM = 1D0
               LATKMX = 72
               LONKMX = 144
            ELSEIF(GRD.EQ.2)THEN
               GRDNUM = 2D0
               GRDCER = 2
               LATKMX = 36
               LONKMX = 72
            ELSEIF(GRD.EQ.3)THEN
               GRDNUM = 4D0
               GRDCER = 4
               LATKMX = 18
               LONKMX = 36
            ENDIF
 
            GRDANG = GRDANG*GRDNUM
            LATIND = (LATIND-0.5D0)/GRDNUM + 0.5D0
            LONIND = (LONIND-0.5D0)/GRDNUM + 0.5D0
              
            D_ANG = (PI_Global*GRDANG/180.D0)**2

            DO LATK = 1,LATKMX
               DO LONK = 1,LONKMX

                  LAT_IN = (LATK-LATIND)*GRDANG*(PI_Global/180.D0)
                  LON_IN = (LONK-LONIND)*GRDANG*(PI_Global/180.D0)

!                 Sphere normal vector and differential of area
                  COSLAT = DCOS(LAT_IN)
                  D_AREA_ALL(LATK,LONK) = (TOA**2)*COSLAT*D_ANG
                  V_NS_ALL(LATK,LONK,1) = COSLAT*DCOS(LON_IN)
                  V_NS_ALL(LATK,LONK,2) = COSLAT*DSIN(LON_IN)
                  V_NS_ALL(LATK,LONK,3) = DSIN(LAT_IN)

!                 New matrix of Reflectivity and Emissivity
                  CERGRE(LATK,LONK) = 0D0
                  CERGEM(LATK,LONK) = 0D0
                  IF(GRD.EQ.1)THEN
                     CERGRE(LATK,LONK) = CERES_R(LATK,LONK)
                     CERGEM(LATK,LONK) = CERES_E(LATK,LONK)
                  ELSEIF((GRD.EQ.2).OR.(GRD.EQ.3))THEN
                     DO II = 0,(GRDCER-1)
                        DO JJ = 0,(GRDCER-1)
                           CERGRE(LATK,LONK) = CERGRE(LATK,LONK) &
                           + CERES_R(GRDCER*LATK-II,GRDCER*LONK-JJ)
                           CERGEM(LATK,LONK) = CERGEM(LATK,LONK) &
                           + CERES_E(GRDCER*LATK-II,GRDCER*LONK-JJ)
                        ENDDO
                     ENDDO
                     CERGRE(LATK,LONK) = CERGRE(LATK,LONK)/(GRDNUM**2)
                     CERGEM(LATK,LONK) = CERGEM(LATK,LONK)/(GRDNUM**2)
                  ENDIF

               ENDDO
            ENDDO
          ENDIF

          END SUBROUTINE
