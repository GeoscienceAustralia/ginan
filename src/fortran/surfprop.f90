SUBROUTINE surfprop (BLKID,AREA,REFL,DIFU,ABSP)
!
! NAME       : surfprop
!
! PURPOSE    : This subroutine provides the detail of the satellite configuration 
!              and physical information, such as specularity and reflectivity. 
!              The information of the satellite configuration, please refer to
!              http://acc.igs.org/orbits/IIF_SV_DimensionsConfiguration.ppt
!              For the physical information, please refer to 
!              Fliegel et al. (1992),Rodrigue-Solano et al. (2012)
!
! PARAMETERS :
!         IN :  BLKID      : BLOCK NUMBER
!
!        OUT :  area(I)     : area of flat surface facing to the Sun [m^2] 
!               refl(I)     : reflection coefficient
!               difu(I)     : diffusion coefficient 
!               absp(I)     : absorption coefficient 
!               MEANING OF INDEX I AND J:
!                             I = 1 +X (BUS FACE ALWAYS ILLUMINATED
!                                       BY THE SUN)
!                             I = 2 +Y (ALONG SOLAR PANELS BEAMS)
!                             I = 3 +Z (TOWARDS THE EARTH)
!                             I = 4 SOLAR PANELS
!
! Author :	Dr. Tzupang Tseng
!
! Created:	November 2017
! 
! Copyright:    Geoscience Australia
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: X_side(3,4),Y_side(2,4),Z_side(4,4),Solar(2,4)
      REAL (KIND = prec_q) :: TOTAL_X_REFL_AREA, TOTAL_X_DIFU_AREA
      REAL (KIND = prec_q) :: TOTAL_Z_REFL_AREA, TOTAL_Z_DIFU_AREA
      REAL (KIND = prec_q) :: TOTAL_Y_REFL_AREA, TOTAL_Y_DIFU_AREA
      REAL (KIND = prec_q) :: TOTAL_SOLAR_REFL_AREA, TOTAL_SOLAR_DIFU_AREA  
      REAL (KIND = prec_q) :: X_refl(3),X_difu(3)
      REAL (KIND = prec_q) :: Y_refl(2),Y_difu(2)
      REAL (KIND = prec_q) :: Z_refl(2),Z_difu(2)
      REAL (KIND = prec_q) :: Solar_refl(2),Solar_difu(2)

      REAL (KIND = prec_q) :: X_refl_area(3), X_difu_area(3)
      REAL (KIND = prec_q) :: Y_refl_area(2), Y_difu_area(2)
      REAL (KIND = prec_q) :: Z_refl_area(2), Z_difu_area(2)
      REAL (KIND = prec_q) :: Solar_refl_area(2), Solar_difu_area(2)

      REAL (KIND = prec_q) :: area(4), refl(4), difu(4), absp(4)
      REAL (KIND = prec_q) :: G_REFL(4), G_DIFU(4), G_ABSP(4)

      INTEGER              :: BLKID,i
      INTEGER              :: II,JJ
! ----------------------------------------------------------------------

! Identify which sides will be illuminated by the Sun:
! X_side(+X), Y_side, Z_side(+Z) and Solar


! ??_SIDE(I,J) :  I     DIFFERENT SURFACE COMPONENTS, such as plume shield,
!                       w-sensor, solar panel beams and so on
!                 J = 1 AREA
!                 J = 2 SPECULARITY
!                 J = 3 REFLECTIVITY
!                 J = 4 SHAPE: 1 = flat, 2 = cylindrical

      DO JJ = 1,4

        DO II = 1,3
          X_side(II,JJ) = 0D0
        ENDDO

        DO II = 1,2
          Z_side(II,JJ) = 0D0
        ENDDO
        
          Y_side(1,JJ) = 0D0
        
        DO II = 1,2
          Solar(II,JJ) = 0D0
        ENDDO

      ENDDO


! Specify all components for each side: 
! Area,engine, TT&C antenna, TT&C antenna tip, navigation antenna adapter, body aft
! end, engine aft end, forward end, all solar panels, solar panel masts, solar
! panel beams plume shield, antenna shroud, and w-sensor
! Here, only the GPS IIR and IIF are considered because older types were
! decommissioned.
!
! Consider the optical properties for each side in terms of area, specularity,
! reflectivity and shape.
! The optical properties are extracted from Bernese source code and modified
! to fit the ACS SRP program.

!     GPS BLOCK II, IIA
!     -----------------

      IF((BLKID.EQ.2).OR.(BLKID.EQ.3))THEN

!     +X SIDE (including the cylindrical areas )
         X_side(1,1) = 2.607D0
         X_side(1,2) = 0.20D0
         X_side(1,3) = 0.56D0
         X_side(1,4) = 1D0

!     TT&C ANTENNA SIDE
         X_side(2,1) = 0.105D0
         X_side(2,2) = 0.20D0
         X_side(2,3) = 0.28D0
         X_side(2,4) = 2D0

!     EACH NAVIGATIONAL ANTENNA ADAPTER
         X_side(3,1) = 0.181D0
         X_side(3,2) = 0.20D0
         X_side(3,3) = 0.36D0
         X_side(3,4) = 2D0

!     +/- Y SIDE (assumed for all block types)
         Y_side(1,1) = 7.01D0
         Y_side(1,2) = 0.20D0
         Y_side(1,3) = 0.56D0
         Y_side(1,4) = 1D0

!     BODY AFT END (-Z)
         Z_side(1,1) = 2.152D0
         Z_side(1,2) = 0.20D0
         Z_side(1,3) = 0.56D0
         Z_side(1,4) = 1D0

!     ENGINE AFT END (-Z)
         Z_side(2,1) = 0.729D0
         Z_side(2,2) = 0D0
         Z_side(2,3) = 0D0
         Z_side(2,4) = 1D0

!     FORWARD END (+Z)
!         Z_SIDE(3,1) = 2.881D0
!         Z_SIDE(3,2) = 0.20D0
!         Z_SIDE(3,3) = 0.56D0
!         Z_SIDE(3,4) = 1D0

!     ALL SOLAR PANELS
         Solar(1,1) = 10.886D0
         Solar(1,2) = 0.85D0
         Solar(1,3) = 0.23D0
         Solar(1,4) = 1D0

!     SOLAR PANELS MASTS
         Solar(2,1) = 0.985D0
         Solar(2,2) = 0.41D0
         Solar(2,3) = 0.52D0
         Solar(2,4) = 2D0

!  ----------------------------------
!     GPS Block IIR
!  ----------------------------------
!
      ELSEIF((BLKID.GE.4).AND.(BLKID.LE.7))THEN

!     + AND -X FACES
        X_side(1,1) = 3.05D0
        X_side(1,2) = 0D0
        X_side(1,3) = 0.06D0
        X_side(1,4) = 1D0

!     PLUME SHILED
        X_side(2,1) = 0.17D0
        X_side(2,2) = 0D0
        X_side(2,3) = 0.06D0
        X_side(2,4) = 2D0

!     ANTENNA SHROUD
        X_side(3,1) = 0.89D0
        X_side(3,2) = 0D0
        X_side(3,3) = 0.06D0
        X_side(3,4) = 2D0

!     +/- Y SIDE (assumed for all block types)
        Y_side(1,1) = 7.01D0
        Y_side(1,2) = 0.20D0
        Y_side(1,3) = 0.56D0
        Y_side(1,4) = 1D0

!     -Z FACE 
        Z_side(1,1) = 3.75D0
        Z_side(1,2) = 0D0
        Z_side(1,3) = 0.06D0
        Z_side(1,4) = 1D0

!     -Z W-SENSOR
        Z_side(2,1) = 0.50D0
        Z_side(2,2) = 0D0
        Z_side(2,3) = 0.06D0
        Z_side(2,4) = 1D0

!     +Z FACE
!        Z_side(3,1) = 3.75D0
!        Z_side(3,2) = 0D0
!        Z_side(3,3) = 0.06D0
!        Z_side(3,4) = 1D0

!     +Z W-SENSOR
!        Z_side(4,1) = 0.50D0
!        Z_side(4,2) = 0D0
!        Z_side(4,3) = 0.06D0
!        Z_side(4,4) = 1D0

!     ALL SOLAR PANELS
        Solar(1,1) = 13.60D0
        Solar(1,2) = 0.85D0
        Solar(1,3) = 0.28D0
        Solar(1,4) = 1D0

!     SOLAR PANELS BEAMS
        Solar(2,1) = 0.32D0
        Solar(2,2) = 0.85D0
        Solar(2,3) = 0.85D0
        Solar(2,4) = 1D0

!     -------------
!     GPS BLOCK IIF
!     -------------
      ELSEIF(BLKID.EQ.8)THEN
!     +/- X SIDE
        X_side(1,1) = 5.72D0
        X_side(1,2) = 0.20D0
        X_side(1,3) = 0.56D0
        X_side(1,4) = 1D0

!     +/- Y SIDE
        Y_side(1,1) = 7.01D0
        Y_side(1,2) = 0.20D0
        Y_side(1,3) = 0.56D0
        Y_side(1,4) = 1D0

!     -Z SIDE
        Z_side(1,1) = 5.40D0
        Z_side(1,2) = 0D0
        Z_side(1,3) = 0D0
        Z_side(1,4) = 1D0

!     +Z SIDE
!        Z_side(3,1) = 5.40D0
!        Z_side(3,2) = 0D0
!        Z_side(3,3) = 0D0
!        Z_side(3,4) = 1D0

!     SOLAR PANELS
        Solar(1,1) = 22.25D0
        Solar(1,2) = 0.85D0
        Solar(1,3) = 0.23D0
        Solar(1,4) = 1D0

!     -------
!     GLONASS
!     -------
      ELSEIF((BLKID.EQ.101).OR.(BLKID.EQ.102))THEN
!     +/- X SIDE (FLAT)
        X_side(1,1) = 1.258D0
        X_side(1,2) = 0.20D0
        X_side(1,3) = 0.56D0
        X_side(1,4) = 1D0

!     +/- X SIDE (CYLINDRICAL)
        X_side(2,1) = 2.052D0
        X_side(2,2) = 0.20D0
        X_side(2,3) = 0.56D0
        X_side(2,4) = 2D0

!     +/- Y SIDE (FLAT)
        Y_side(1,1) = 2.591D0
        Y_side(1,2) = 0.20D0
        Y_side(1,3) = 0.56D0
        Y_side(1,4) = 1D0

!     +/- Y SIDE (CYLINDRICAL)
        Y_side(2,1) = 2.532D0
        Y_side(2,2) = 0.20D0
        Y_side(2,3) = 0.56D0
        Y_side(2,4) = 2D0

!     -Z SIDE (BUS)
        Z_side(1,1) = 0.877D0
        Z_side(1,2) = 0.20D0
        Z_side(1,3) = 0.56D0
        Z_side(1,4) = 1D0

!     -Z SIDE (APOGEE ENGINE)
        Z_side(2,1) = 0.785D0
        Z_side(2,2) = 0D0
        Z_side(2,3) = 0D0
        Z_side(2,4) = 1D0

!     +Z SIDE (BUS)
        Z_side(3,1) = 1.412D0
        Z_side(3,2) = 0.20D0
        Z_side(3,3) = 0.56D0
        Z_side(3,4) = 1D0

!     +Z SIDE (RETRO REFLECTOR ARRAY)
        Z_side(4,1) = 0.250D0
        Z_side(4,2) = 1D0
        Z_side(4,3) = 1D0
        Z_side(4,4) = 1D0

!     SOLAR PANELS
        Solar(1,1) = 23.616D0
        Solar(1,2) = 0.85D0
        Solar(1,3) = 0.23D0
        Solar(1,4) = 1D0


! This part will be filled in if the further info is obtained.
!============================================================
!     ---------
!     GLONASS-M
!     ---------
!      ELSEIF(BLKID.GT.102)THEN

!     ---------
!     GLONASS-K
!     ---------
!      ELSEIF(BLKID.GT.103)THEN

       ENDIF


       TOTAL_X_REFL_AREA = 0.0d0
       TOTAL_X_DIFU_AREA = 0.0d0
       TOTAL_Z_REFL_AREA = 0.0d0
       TOTAL_Z_DIFU_AREA = 0.0d0
       TOTAL_Y_REFL_AREA = 0.0d0
       TOTAL_Y_DIFU_AREA = 0.0d0
       TOTAL_SOLAR_REFL_AREA = 0.0d0
       TOTAL_SOLAR_DIFU_AREA = 0.0d0


! Compute the fractions of reflected, diffused and absorbed photons using
! spectcularity (u) and reflectivity (v)
! Fraction of absorbed photons  = 1-v
! Fraction of reflected photons = uv
! Fraction of diffused photons  = v(1-u)
! Initialise variables G_REFL, G_ABSP, G_DIFU
G_REFL = 0.d0
G_ABSP = 0.d0
G_DIFU = 0.d0

IF((BLKID.GE.4).AND.(BLKID.LE.7))THEN
!*********
! X_SIDE *
!*********
      do i=1,3
       X_refl(i) = X_side(i,2)*X_side(i,3)
       X_difu(i) = X_side(i,3)*(1-X_side(i,2))
! Multiply the above optical properties with the surface areas
       X_refl_area(i) = X_side(i,1)*X_refl(i)
       X_difu_area(i) = X_side(i,1)*X_difu(i)
       TOTAL_X_REFL_AREA = TOTAL_X_REFL_AREA + X_refl_area(i)
       TOTAL_X_DIFU_AREA = TOTAL_X_DIFU_AREA + X_difu_area(i)
      end do

! Compute the ratio of physical interactions at each side
      G_REFL(1) = TOTAL_X_REFL_AREA/(X_side(1,1)+X_side(2,1)+X_side(3,1))
      G_DIFU(1) = TOTAL_X_DIFU_AREA/(X_side(1,1)+X_side(2,1)+X_side(3,1))
      G_ABSP(1) = 1-(G_REFL(1)+G_DIFU(1))

!**********************************
! Y_SIDE (assumed for all Blocks) *
!**********************************
       Y_refl(1) = Y_side(1,2)*Y_side(1,3)
       Y_difu(1) = Y_side(1,3)*(1-Y_side(1,2))
! Multiply the above optical properties with the surface areas
       Y_refl_area(1) = Y_side(1,1)*Y_refl(1)
       Y_difu_area(1) = Y_side(1,1)*Y_difu(1)
       TOTAL_Y_REFL_AREA = TOTAL_Y_REFL_AREA + Y_refl_area(1)
       TOTAL_Y_DIFU_AREA = TOTAL_Y_DIFU_AREA + Y_difu_area(1)

! Compute the ratio of physical interactions at each side
      G_REFL(2) = TOTAL_Y_REFL_AREA/Y_side(1,1)
      G_DIFU(2) = TOTAL_Y_DIFU_AREA/Y_side(1,1)
      G_ABSP(2) = 1-(G_REFL(2)+G_DIFU(2))

!*********
! Z_SIDE *
!*********
      do i=1,2
       Z_refl(i) = Z_side(i,2)*Z_side(i,3)
       Z_difu(i) = Z_side(i,3)*(1-Z_side(i,2))
! Multiply the above optical properties with the surface areas
       Z_refl_area(i) = Z_side(i,1)*Z_refl(i)
       Z_difu_area(i) = Z_side(i,1)*Z_difu(i)
       TOTAL_Z_REFL_AREA = TOTAL_Z_REFL_AREA + Z_refl_area(i)
       TOTAL_Z_DIFU_AREA = TOTAL_Z_DIFU_AREA + Z_difu_area(i)
      end do

! Compute the ratio of physical interactions at each side
       G_REFL(3)=TOTAL_Z_REFL_AREA/(Z_side(1,1)+Z_side(2,1))
       G_DIFU(3)=TOTAL_Z_DIFU_AREA/(Z_side(1,1)+Z_side(2,1))
! FIXME: Index into G_REFL is 1 - corrected to 3?
       G_ABSP(3)=1-(G_REFL(3)+G_DIFU(3))

!***************
! SOLAR PANELS *
!***************
      do i=1,2
       Solar_refl(i) = Solar(i,2)*Solar(i,3)
       Solar_difu(i) = Solar(i,3)*(1-Solar(i,2))
! Multiply the above optical properties with the surface areas
       Solar_refl_area(i) = Solar(i,1)*Solar_refl(i)
       Solar_difu_area(i) = Solar(i,1)*Solar_difu(i)
       TOTAL_SOLAR_REFL_AREA = TOTAL_SOLAR_REFL_AREA + Solar_refl_area(i)
       TOTAL_SOLAR_DIFU_AREA = TOTAL_SOLAR_DIFU_AREA + Solar_difu_area(i)
      end do

! Compute the ratio of physical interactions at each side
       G_REFL(4) = TOTAL_SOLAR_REFL_AREA/(Solar(1,1)+Solar(2,1))
       G_DIFU(4) = TOTAL_SOLAR_DIFU_AREA/(Solar(1,1)+Solar(2,1))
       G_ABSP(4) = 1-(G_REFL(4)+G_DIFU(4))


ELSEIF(BLKID.EQ.8)THEN

!*********
! X_SIDE *
!*********
       X_refl(1) = X_side(1,2)*X_side(1,3)
       X_difu(1) = X_side(1,3)*(1-X_side(1,2))
! Multiply the above optical properties with the surface areas
       X_refl_area(1) = X_side(1,1)*X_refl(1)
       X_difu_area(1) = X_side(1,1)*X_difu(1)
       TOTAL_X_REFL_AREA = TOTAL_X_REFL_AREA + X_refl_area(1)
       TOTAL_X_DIFU_AREA = TOTAL_X_DIFU_AREA + X_difu_area(1)

! Compute the ratio of physical interactions at each side
      G_REFL(1) = TOTAL_X_REFL_AREA/X_side(1,1)
      G_DIFU(1) = TOTAL_X_DIFU_AREA/X_side(1,1)
      G_ABSP(1) = 1-(G_REFL(1)+G_DIFU(1))

!**********************************
! Y_SIDE (assumed for all Blocks) *
!**********************************
       Y_refl(1) = Y_side(1,2)*Y_side(1,3)
       Y_difu(1) = Y_side(1,3)*(1-Y_side(1,2))
! Multiply the above optical properties with the surface areas
       Y_refl_area(1) = Y_side(1,1)*Y_refl(1)
       Y_difu_area(1) = Y_side(1,1)*Y_difu(1)
       TOTAL_Y_REFL_AREA = TOTAL_Y_REFL_AREA + Y_refl_area(1)
       TOTAL_Y_DIFU_AREA = TOTAL_Y_DIFU_AREA + Y_difu_area(1)

! Compute the ratio of physical interactions at each side
      G_REFL(2) = TOTAL_Y_REFL_AREA/Y_side(1,1)
      G_DIFU(2) = TOTAL_Y_DIFU_AREA/Y_side(1,1)
      G_ABSP(2) = 1-(G_REFL(2)+G_DIFU(2))

!*********
! Z_SIDE *
!*********
       Z_refl(1) = Z_side(1,2)*Z_side(1,3)
       Z_difu(1) = Z_side(1,3)*(1-Z_side(1,2))
! Multiply the above optical properties with the surface areas
       Z_refl_area(1) = Z_side(1,1)*X_refl(1)
       Z_difu_area(1) = Z_side(1,1)*X_difu(1)
       TOTAL_Z_REFL_AREA = TOTAL_Z_REFL_AREA + Z_refl_area(1)
       TOTAL_Z_DIFU_AREA = TOTAL_Z_DIFU_AREA + Z_difu_area(1)

! Compute the ratio of physical interactions at each side
      G_REFL(3) = TOTAL_Z_REFL_AREA/Z_side(1,1)
      G_DIFU(3) = TOTAL_Z_DIFU_AREA/Z_side(1,1)
      G_ABSP(3) = 1-(G_REFL(3)+G_DIFU(3))


!       Z_refl(3) = Z_side(3,2)*Z_side(3,3)
!       Z_difu(3) = Z_side(3,3)*(1-Z_side(3,2))
! Multiply the above optical properties with the surface areas
!       Z_refl_area(3) = Z_side(1,1)*Z_refl(3)
!       Z_difu_area(3) = Z_side(1,1)*Z_difu(3)
!       TOTAL_Z_REFL_AREA(1) = TOTAL_Z_REFL_AREA(1) + Z_refl_area(1)
!       TOTAL_Z_DIFU_AREA(1) = TOTAL_Z_DIFU_AREA(1) + Z_difu_area(1)

!***************
! SOLAR PANELS *
!***************
       Solar_refl(1) = Solar(1,2)*Solar(1,3)
       Solar_difu(1) = Solar(1,3)*(1-Solar(1,2))
! Multiply the above optical properties with the surface areas
       Solar_refl_area(1) = Solar(1,1)*Solar_refl(1)
       Solar_difu_area(1) = Solar(1,1)*Solar_difu(1)
       TOTAL_SOLAR_REFL_AREA = TOTAL_SOLAR_REFL_AREA + SOLAR_refl_area(1)
       TOTAL_SOLAR_DIFU_AREA = TOTAL_SOLAR_DIFU_AREA + SOLAR_difu_area(1)

! Compute the ratio of physical interactions at each side
       G_REFL(4) = TOTAL_SOLAR_REFL_AREA/Solar(1,1)
       G_DIFU(4) = TOTAL_SOLAR_DIFU_AREA/Solar(1,1)
       G_ABSP(4) = 1-(G_REFL(4)+G_DIFU(4))


END IF

! Assign a matrix for those optical properties
       do i=1,4
       area(i) = 0.0d0
       refl(i) = 0.0d0
       difu(i) = 0.0d0
       absp(i) = 0.0d0
       end do

       area(1) = X_side(1,1)+X_side(2,1)+X_side(3,1)
       area(2) = Y_side(1,1)
       area(3) = Z_side(1,1)+Z_side(2,1)
       area(4) = Solar(1,1)+Solar(2,1)

       refl(1) = G_REFL(1)
       refl(2) = G_REFL(2)
       refl(3) = G_REFL(3)
       refl(4) = G_REFL(4)

       difu(1) = G_DIFU(1)
       difu(2) = G_DIFU(2)
       difu(3) = G_DIFU(3)
       difu(4) = G_DIFU(4)

       absp(1) = G_ABSP(1)
       absp(2) = G_ABSP(2)
       absp(3) = G_ABSP(3)
       absp(4) = G_ABSP(4)

END SUBROUTINE
