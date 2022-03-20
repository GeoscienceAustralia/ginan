SUBROUTINE PROPBOXW(BLKID,AREA,REFL,DIFU,ABSP,AREA2,REFL2,DIFU2,ABSP2,REFLIR,DIFUIR,ABSPIR)
!!
!! NAME       :  PROPBOXW
!!
!! PURPOSE    :  SATELLITE DIMENSIONS AND OPTICAL PROPERTIES FROM ROCK MODELS
!!               BOX-WING MODELS FOR GPS, GLONASS, GALILEO, BEIDOU and QZSS SATELLITES
!!
!! REFERENCES :  Fliegel H, Gallini T, Swift E (1992) Global Positioning System Radiation
!!                  Force Model for Geodetic Applications. Journal of Geophysical Research
!!                  97(B1): 559-568
!!               Fliegel H, Gallini T (1996) Solar Force Modelling of Block IIR Global
!!                  Positioning System satellites. Journal of Spacecraft and Rockets
!!                  33(6): 863-866
!!               Ziebart M (2001) High Precision Analytical Solar Radiation Pressure
!!                  Modelling for GNSS Spacecraft. PhD Thesis, University of East London
!!               [IGEXMAIL-0086] GLONASS S/C mass and dimension
!!               [IGSMAIL-5104] GLONASS-M dimensions and center-of-mass correction
!!               http://acc.igs.org/orbits/IIF_SV_DimensionsConfiguration.ppt
!!
!! IN  :  BLKID
!!          1 = GPS-I
!!          2 = GPS-II
!!          3 = GPS-IIA
!!          4 = GPS-IIR
!!          5 = GPS-IIR-A
!!          6 = GPS-IIR-B
!!          7 = GPS-IIR-M
!!          8 = GPS-IIF
!!          9 = GPS-IIIA
!!        101 = GLONASS
!!        102 = GLONASS-M (Added TAH 190702)
!!        103 = GLONASS-K (Added TAH 190702)
!!        201 = Galileo (IOV)
!!        202 = Galileo (FOC)
!!        301 = BDS GEO
!!        302 = BDS IGSO
!!        303 = BDS MEO
!!        401 = QZSS-1
!!        402 = QZSS-2I
!!        403 = QZSS-2G
!!
!! OUT :  AREA(I,J)  : AREAS OF FLAT SURFACES [m^2]
!!        REFL(I,J)  : REFLEXION COEFFICIENT
!!        DIFU(I,J)  : DIFFUSION COEFFICIENT
!!        ABSP(I,J)  : ABSORPTION COEFFICIENT
!!                      I = 1 +Z (TOWARDS THE EARTH)
!!                      I = 2 +Y (ALONG SOLAR PANELS BEAMS)
!!                      I = 3 +X (ALONG BUS DIRECTION ALWAYS ILLUMINATED BY THE SUN)
!!                      I = 4 SOLAR PANELS
!!                      J = 1 POSITIVE DIRECTION
!!                      J = 2 NEGATIVE DIRECTION
!!        ????2(I,J) : INDEX 2 INDICATES AREAS AND OPTICAL PROPERTIES OF CYLINDRICAL
!!                     SURFACES, SAME MEANING OF (I,J) AS BEFORE
!!        ????IR(I,J): OPTICAL PROPERTIES IN THE INFRARED (ASSUMED), NO SEPARATION
!!                     BETWEEN FLAT AND CYLINDRICAL SURFACES
!!
!! CREATED    :  2010/10/18             LAST MODIFIED :  2020/03/20
!!
!! ORIGINAL   :  C.J. RODRIGUEZ-SOLANO   rodriguez@bv.tum.de MODIFIED BY : TOM HERRING
!!
!! CHANGES    :  17-MAR-11 : CR: ADD BOX-WING MODELS FOR GLONASS, GLONASS-M, GPS II-F
!!                  NOV-17 : GFZ: ADD GALILEO IOV & FOC 
!!                  APR-19 : GFZ: ADD GPS BLOCK III
!!            :  02-JUL-19 : Added GLONASS-M and GLONASS-K satellites; Added extra 
!!                           Documentation.
!!                           Four elements of the X_SIDE, Y_SIDE, Z_SIDE and S_SIDE are
!!                           (1) Area (m^2)
!!                           (2) Specularity = rho/(1-alpha)
!!                           (3) Reflecivity = 1-alpha 
!!                           (4) 1 for flat; 2 for cylinder.
!!            :  09-JUL-19 : Changed logical at end of routine to test if we found models.
!!
!! Copyright  :  Geoscience Australia
! ----------------------------------------------------------------------------------------

  USE mdl_precision
  USE mdl_config
  IMPLICIT NONE
!
  INTEGER              :: II,JJ,KK,SS,BLKID
!
  REAL (KIND = prec_q) :: AREA(4,2),REFL(4,2),DIFU(4,2),ABSP(4,2)
  REAL (KIND = prec_q) :: AREA2(4,2),REFL2(4,2),DIFU2(4,2),ABSP2(4,2)
  REAL (KIND = prec_q) :: REFLIR(4,2),DIFUIR(4,2),ABSPIR(4,2)
  REAL (KIND = prec_q) :: X_SIDE(5,4),Z_SIDE(4,4),S_SIDE(2,4),Y_SIDE(2,4)
  REAL (KIND = prec_q) :: SURFALL(13,4)
  REAL (KIND = prec_q) :: ALLREFL(13),ALLDIFU(13),REFL_AREA(13),DIFU_AREA(13)
  REAL (KIND = prec_q) :: G_AREA1(5),G_REFL1(5),G_DIFU1(5),G_ABSP1(5)
  REAL (KIND = prec_q) :: G_AREA2(5),G_REFL2(5),G_DIFU2(5),G_ABSP2(5)
  REAL (KIND = prec_q) :: BUSFAC
  
  CHARACTER (LEN=100)  :: mesg

  LOGICAL blk_found    ! Set false at start and set true when properities are set

! SATELLITE PROPERTIES: VALUES GIVEN FOR +-X, +-Y, +-Z, AND SOLAR PANELS
!
!                       K_SIDE(I,J) :   I = SURFACE COMPONENTS ( K = +-X, +-Y, +-Z, +S etc..)
!                                       J = 1 AREA
!                                       J = 2 SPECULARITY
!                                       J = 3 REFLECTIVITY
!                                       J = 4 SHAPE: 1 = flat, 2 = cylindrical

  DO JJ = 1,4
     DO II = 1,5
        X_SIDE(II,JJ) = 0D0
     ENDDO
     DO II = 1,4
        Z_SIDE(II,JJ) = 0D0
     ENDDO
     DO II = 1,2
        S_SIDE(II,JJ) = 0D0
     ENDDO
     DO II = 1,2
        Y_SIDE(II,JJ) = 0D0
     ENDDO
  ENDDO

! Flag to check if SV Block properties were found

  blk_found = .false.

! -----------
! GPS BLOCK I
! -----------
! SEE FLIEGEL ET AL (1992)
  IF (BLKID.EQ.1) THEN
     blk_found = .true.   

! +X SIDE
     X_SIDE(1,1) = 1.055D0
     X_SIDE(1,2) = 0.80D0
     X_SIDE(1,3) = 0.50D0
     X_SIDE(1,4) = 1D0

! ENGINE SIDE
     X_SIDE(2,1) = 0.570D0
     X_SIDE(2,2) = 0.75D0
     X_SIDE(2,3) = 0.86D0
     X_SIDE(2,4) = 2D0

! TT&C ANTENNA SIDE
     X_SIDE(3,1) = 0.055D0
     X_SIDE(3,2) = 0.05D0
     X_SIDE(3,3) = 0.28D0
     X_SIDE(3,4) = 2D0

! TT&C ANTENNA TIP
     X_SIDE(4,1) = 0.019D0
     X_SIDE(4,2) = 0.85D0
     X_SIDE(4,3) = 0.28D0
     X_SIDE(4,4) = 2D0

! EACH NAVIGATIONAL ANTENNA ADAPTER
     X_SIDE(5,1) = 0.029D0
     X_SIDE(5,2) = 0.75D0
     X_SIDE(5,3) = 0.36D0
     X_SIDE(5,4) = 2D0

! BODY AFT END (-Z)
     Z_SIDE(1,1) = 0.816D0
     Z_SIDE(1,2) = 0.80D0
     Z_SIDE(1,3) = 0.86D0
     Z_SIDE(1,4) = 1D0

! ENGINE AFT END (-Z)                                                         
     Z_SIDE(2,1) = 0.694D0
     Z_SIDE(2,2) = 0D0
     Z_SIDE(2,3) = 0D0
     Z_SIDE(2,4) = 1D0

! FORWARD END (+Z)                                                                     
     Z_SIDE(3,1) = 1.510D0
     Z_SIDE(3,2) = 0.75D0
     Z_SIDE(3,3) = 0.86D0
     Z_SIDE(3,4) = 1D0

! ALL SOLAR PANELS
     S_SIDE(1,1) = 5.583D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

! SOLAR PANELS MASTS                                                                                     
     S_SIDE(2,1) = 0.470D0
     S_SIDE(2,2) = 0.85D0
     S_SIDE(2,3) = 0.85D0
     S_SIDE(2,4) = 2D0

! -----------------
! GPS BLOCK II, IIA
! -----------------
! SEE FLIEGEL ET AL (1992)
  ELSEIF ((BLKID.EQ.2).OR.(BLKID.EQ.3)) THEN
     blk_found = .true.   

! +X SIDE                                                                                           
     X_SIDE(1,1) = 1.553D0
     X_SIDE(1,2) = 0.20D0
     X_SIDE(1,3) = 0.56D0
     X_SIDE(1,4) = 1D0

! ENGINE SIDE (INCLUDES PLUME SHILED 0.22*1.84 m^2)                                            
     X_SIDE(2,1) = 1.054D0
     X_SIDE(2,2) = 0.20D0
     X_SIDE(2,3) = 0.56D0
     X_SIDE(2,4) = 2D0

! TT&C ANTENNA SIDE                                                                       
     X_SIDE(3,1) = 0.105D0
     X_SIDE(3,2) = 0.20D0
     X_SIDE(3,3) = 0.28D0
     X_SIDE(3,4) = 2D0

! EACH NAVIGATIONAL ANTENNA ADAPTER                                                               
     X_SIDE(5,1) = 0.181D0
     X_SIDE(5,2) = 0.20D0
     X_SIDE(5,3) = 0.36D0
     X_SIDE(5,4) = 2D0

! BODY AFT END (-Z)                                                                               
     Z_SIDE(1,1) = 2.152D0
     Z_SIDE(1,2) = 0.20D0
     Z_SIDE(1,3) = 0.56D0
     Z_SIDE(1,4) = 1D0

! ENGINE AFT END (-Z)                                                                          
     Z_SIDE(2,1) = 0.729D0
     Z_SIDE(2,2) = 0D0
     Z_SIDE(2,3) = 0D0
     Z_SIDE(2,4) = 1D0

! FORWARD END (+Z)                                                                                    
     Z_SIDE(3,1) = 2.881D0
     Z_SIDE(3,2) = 0.20D0
     Z_SIDE(3,3) = 0.56D0
     Z_SIDE(3,4) = 1D0

! ALL SOLAR PANELS                                                                                     
     S_SIDE(1,1) = 10.886D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

! SOLAR PANELS MASTS                                                                    
     S_SIDE(2,1) = 0.985D0
     S_SIDE(2,2) = 0.41D0
     S_SIDE(2,3) = 0.52D0
     S_SIDE(2,4) = 2D0

! ----------------------------------
! GPS BLOCK IIR, IIR-A, IIR-B, IIR-M
! ----------------------------------
! SEE FLIEGEL AND GALINI (1996)
  ELSEIF ((BLKID.GE.4).AND.(BLKID.LE.7)) THEN
     blk_found = .true.   

! +X AND -X FACES                                                                                
     X_SIDE(1,1) = 3.05D0
     X_SIDE(1,2) = 0D0
     X_SIDE(1,3) = 0.06D0
     X_SIDE(1,4) = 1D0

! PLUME SHILED                                                                                  
     X_SIDE(2,1) = 0.17D0
     X_SIDE(2,2) = 0D0
     X_SIDE(2,3) = 0.06D0
     X_SIDE(2,4) = 2D0

! ANTENNA SHROUD                                                                              
     X_SIDE(3,1) = 0.89D0
     X_SIDE(3,2) = 0D0
     X_SIDE(3,3) = 0.06D0
     X_SIDE(3,4) = 2D0

! -Z FACE                                                                                        
     Z_SIDE(1,1) = 3.75D0
     Z_SIDE(1,2) = 0D0
     Z_SIDE(1,3) = 0.06D0
     Z_SIDE(1,4) = 1D0

! -Z W-SENSOR
     Z_SIDE(2,1) = 0.50D0
     Z_SIDE(2,2) = 0D0
     Z_SIDE(2,3) = 0.06D0
     Z_SIDE(2,4) = 1D0

! +Z FACE                                                                                              
     Z_SIDE(3,1) = 3.75D0
     Z_SIDE(3,2) = 0D0
     Z_SIDE(3,3) = 0.06D0
     Z_SIDE(3,4) = 1D0

! +Z W-SENSOR                                                                                             
     Z_SIDE(4,1) = 0.50D0
     Z_SIDE(4,2) = 0D0
     Z_SIDE(4,3) = 0.06D0
     Z_SIDE(4,4) = 1D0

! ALL SOLAR PANELS                                                                                
     S_SIDE(1,1) = 13.60D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.28D0
     S_SIDE(1,4) = 1D0

! SOLAR PANELS BEAMS                                                                     
     S_SIDE(2,1) = 0.32D0
     S_SIDE(2,2) = 0.85D0
     S_SIDE(2,3) = 0.85D0
     S_SIDE(2,4) = 1D0

! -------------
! GPS BLOCK IIF
! -------------
! SEE IIF PRESENTATION
! OPTICAL PROPERTIES NOT KNOWN, SAME ASSUMPTIONS AS ZIEBART (2001)
  ELSEIF (BLKID.EQ.8) THEN
     blk_found = .true.   

! +/- X SIDE
     X_SIDE(1,1) = 5.72D0
     X_SIDE(1,2) = 0.20D0
     X_SIDE(1,3) = 0.56D0
     X_SIDE(1,4) = 1D0

! +/- Y SIDE
     Y_SIDE(1,1) = 7.01D0
     Y_SIDE(1,2) = 0.20D0
     Y_SIDE(1,3) = 0.56D0
     Y_SIDE(1,4) = 1D0

! -Z SIDE
     Z_SIDE(1,1) = 5.40D0
     Z_SIDE(1,2) = 0D0
     Z_SIDE(1,3) = 0D0
     Z_SIDE(1,4) = 1D0

! +Z SIDE
     Z_SIDE(3,1) = 5.40D0
     Z_SIDE(3,2) = 0D0
     Z_SIDE(3,3) = 0D0
     Z_SIDE(3,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 22.25D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

! -------------
! GPS BLOCK III
! -------------
! DIMENSIONS ACCORDING TO PETER STEIGENBERGER (IGS-ACS-1219)
! (mass = 2161 kg / wide = 2.46 m deep = 1.78 m high = 3.40 m)
! SOLAR PANEL CAN BE TAKEN FROM (span = 5.33 m)
! http://www.deagel.com/Space-Systems/GPS-Block-III_a000238005.aspx
! BUT SO FAR,SOLAR PANEL BLOCK IIF VALUES ARE USED
! OPTICAL PROPERTIES NOT KNOWN, COPIED FROM IIF
  ELSEIF (BLKID.EQ.9) THEN
     blk_found = .true. 
  
! +/- X SIDE
     X_SIDE(1,1) = 6.05D0
     X_SIDE(1,2) = 0.20D0
     X_SIDE(1,3) = 0.56D0
     X_SIDE(1,4) = 1D0

! +/- Y SIDE
     Y_SIDE(1,1) = 8.36D0
     Y_SIDE(1,2) = 0.20D0
     Y_SIDE(1,3) = 0.56D0
     Y_SIDE(1,4) = 1D0

! -Z SIDE
     Z_SIDE(1,1) = 4.38D0
     Z_SIDE(1,2) = 0D0
     Z_SIDE(1,3) = 0D0
     Z_SIDE(1,4) = 1D0

! +Z SIDE
     Z_SIDE(3,1) = 4.38D0
     Z_SIDE(3,2) = 0D0
     Z_SIDE(3,3) = 0D0
     Z_SIDE(3,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 22.25D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

! -------
! GLONASS
! -------
! SEE ZIEBART (2001)
! OPTICAL PROPERTIES NOT KNOWN, SAME ASSUMPTIONS AS ZIEBART (2001)
! ASSUMED SAME SHAPE FOR GLONASS-M, SEE END OF SUBROUTINE
! ELSEIF((BLKID.EQ.101).OR.(BLKID.EQ.102))THEN
! MOD TAH 190702: Use different values for GLOSNASSM: Values below for
! GLONASS based on conversion of Table 5.6 Rodruguez-Solano Thesis (2013)
  ELSEIF (BLKID.EQ.101 ) THEN
     blk_found = .true.
   
! +/- X SIDE (FLAT) TAH VALUES: +X   1.265   0.200   0.560  1
     X_SIDE(1,1) = 1.258D0
     X_SIDE(1,2) = 0.20D0
     X_SIDE(1,3) = 0.56D0
     X_SIDE(1,4) = 1D0

! +/- X SIDE (CYLINDRICAL) TAH VALUES: +X   2.065   0.200   0.560  2
     X_SIDE(2,1) = 2.052D0
     X_SIDE(2,2) = 0.20D0
     X_SIDE(2,3) = 0.56D0
     X_SIDE(2,4) = 2D0

! +/- Y SIDE (FLAT)  TAH VALUES:  +Y   2.592   0.200   0.560  1
     Y_SIDE(1,1) = 2.591D0
     Y_SIDE(1,2) = 0.20D0
     Y_SIDE(1,3) = 0.56D0
     Y_SIDE(1,4) = 1D0

! +/- Y SIDE (CYLINDRICAL)  TAH VALUES:  +Y   2.531   0.200   0.560  2 
     Y_SIDE(2,1) = 2.532D0
     Y_SIDE(2,2) = 0.20D0
     Y_SIDE(2,3) = 0.56D0
     Y_SIDE(2,4) = 2D0

! -Z SIDE (BUS)   TAH VALUES:  -Z   1.662   0.200   0.295  1
     Z_SIDE(1,1) = 0.877D0
     Z_SIDE(1,2) = 0.20D0
     Z_SIDE(1,3) = 0.56D0
     Z_SIDE(1,4) = 1D0

! -Z SIDE (APOGEE ENGINE)
     Z_SIDE(2,1) = 0.785D0
     Z_SIDE(2,2) = 0D0
     Z_SIDE(2,3) = 0D0
     Z_SIDE(2,4) = 1D0

! +Z SIDE (BUS)   TAH VALUES: +Z   1.662   0.391   0.626  1
     Z_SIDE(3,1) = 1.412D0
     Z_SIDE(3,2) = 0.20D0
     Z_SIDE(3,3) = 0.56D0
     Z_SIDE(3,4) = 1D0

! +Z SIDE (RETRO REFLECTOR ARRAY)
     Z_SIDE(4,1) = 0.250D0
     Z_SIDE(4,2) = 1D0
     Z_SIDE(4,3) = 1D0
     Z_SIDE(4,4) = 1D0

! SOLAR PANELS  TAH VALUES: SP  23.616   0.848   0.230  1
     S_SIDE(1,1) = 23.616D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

!! MOD TAH 190702: Add GLONASS-M (Values from Rodruguez-Solano Thesis (2013) TUM,
!! https://mediatum.ub.tum.de/doc/1188612/719708.pdf

  ELSEIF (BLKID.EQ.102 ) THEN
     blk_found = .true.
   
! +/- X SIDE (FLAT)
     X_SIDE(1,1) = 1.232D0
     X_SIDE(1,2) = 0.20D0
     X_SIDE(1,3) = 0.56D0
     X_SIDE(1,4) = 1D0

! +/- X SIDE (CYLINDRICAL)
     X_SIDE(2,1) = 3.298D0
     X_SIDE(2,2) = 0.20D0
     X_SIDE(2,3) = 0.56D0
     X_SIDE(2,4) = 2D0

! +/- Y SIDE (FLAT)
     Y_SIDE(1,1) = 2.700D0
     Y_SIDE(1,2) = 0.20D0
     Y_SIDE(1,3) = 0.56D0
     Y_SIDE(1,4) = 1D0

! +/- Y SIDE (CYLINDRICAL)
     Y_SIDE(2,1) = 3.330D0
     Y_SIDE(2,2) = 0.20D0
     Y_SIDE(2,3) = 0.56D0
     Y_SIDE(2,4) = 2D0

! -Z SIDE (BUS)
     Z_SIDE(1,1) = 2.120D0
     Z_SIDE(1,2) = 0.20D0
     Z_SIDE(1,3) = 0.21D0
     Z_SIDE(1,4) = 1D0

! -Z SIDE (APOGEE ENGINE)
     Z_SIDE(2,1) = 0.000D0
     Z_SIDE(2,2) = 0D0
     Z_SIDE(2,3) = 0D0
     Z_SIDE(2,4) = 1D0

! +Z SIDE (BUS)
     Z_SIDE(3,1) = 2.12D0
     Z_SIDE(3,2) = 0.30D0
     Z_SIDE(3,3) = 0.59D0
     Z_SIDE(3,4) = 1D0

! +Z SIDE (RETRO REFLECTOR ARRAY)
     Z_SIDE(4,1) = 0.00D0
     Z_SIDE(4,2) = 1D0
     Z_SIDE(4,3) = 1D0
     Z_SIDE(4,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 30.850d0 
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

!! MOD TAH 190702: Add GLONASS-K (Values from Rodruguez-Solano Thesis (2013) TUM, 
!! https://mediatum.ub.tum.de/doc/1188612/719708.pdf
  ELSEIF (BLKID.EQ.103 ) THEN   ! GLONASS-K (Like a IIF with flat bus)
     blk_found = .true. 
  
! +/- X SIDE
     X_SIDE(1,1) = 2.12D0
     X_SIDE(1,2) = 0.20D0
     X_SIDE(1,3) = 0.56D0
     X_SIDE(1,4) = 1D0

! +/- Y SIDE
     Y_SIDE(1,1) = 4.35D0
     Y_SIDE(1,2) = 0.20D0
     Y_SIDE(1,3) = 0.56D0
     Y_SIDE(1,4) = 1D0

! -Z SIDE
     Z_SIDE(1,1) = 1.73D0
     Z_SIDE(1,2) = 0.20D0
     Z_SIDE(1,3) = 0.46D0
     Z_SIDE(1,4) = 1D0

! +Z SIDE
     Z_SIDE(3,1) = 1.73D0
     Z_SIDE(3,2) = 0.32D0
     Z_SIDE(3,3) = 0.60D0
     Z_SIDE(3,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 16.960D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.23D0
     S_SIDE(1,4) = 1D0

!! ========================================================
!! BDS satellites (Temporally using)
!! Information: http://mgex.igs.org/IGS_MGEX_Status_BDS.php
!! The BDS orbit is 30000 km, so the ERP effect could be tiny.
!! ========================================================
  ELSEIF (BLKID.EQ.301.OR.BLKID.EQ.302.OR.BLKID.EQ.303) THEN 
     blk_found = .true. 
  
! +/- X SIDE
     X_SIDE(1,1) = 3.78D0
     X_SIDE(1,2) = 0.20D0 ! guess 
     X_SIDE(1,3) = 0.56D0 ! guess
     X_SIDE(1,4) = 1D0

! +/- Y SIDE
     Y_SIDE(1,1) = 4.40D0
     Y_SIDE(1,2) = 0.20D0 
     Y_SIDE(1,3) = 0.56D0 
     Y_SIDE(1,4) = 1D0

! -Z SIDE
     Z_SIDE(1,1) = 3.44D0
     Z_SIDE(1,2) = 0.0D0 
     Z_SIDE(1,3) = 0.0D0 
     Z_SIDE(1,4) = 1D0

! +Z SIDE
     Z_SIDE(3,1) = 3.44D0
     Z_SIDE(3,2) = 0.0D0
     Z_SIDE(3,3) = 0.13D0 !guess
     Z_SIDE(3,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 22.70D0
     S_SIDE(1,2) = 0.85D0
     S_SIDE(1,3) = 0.28D0
     S_SIDE(1,4) = 1D0

!! ========================================================
!! Cabinet Office; QZS-1 Satellite Information; SPI-QZS1_A;
!! https://qzss.go.jp/en/technical/qzssinfo/khp0mf0000000wuf-att/spi-qzs1_a.pdf                                           
!! Cabinet Office; QZS-2 Satellite Information; SPI-QZS2_C;
!! https://qzss.go.jp/en/technical/qzssinfo/khp0mf0000000wuf-att/spi-qzs2_c.pdf                                           
!! Cabinet Office; QZS-3 Satellite Information; SPI-QZS3_B; 
!! https://qzss.go.jp/en/technical/qzssinfo/khp0mf0000000wuf-att/spi-qzs3_b.pdf                                           
!! Cabinet Office; QZS-4 Satellite Information; SPI-QZS4_C; 
!! https://qzss.go.jp/en/technical/qzssinfo/khp0mf0000000wuf-att/spi-qzs4_c.pdf 
!! The QZSS orbit is 40000 km, so the ERP effect could be very tiny.               
!! ========================================================
  ELSEIF (BLKID.EQ.401) THEN 
     blk_found = .true.

! +/- X SIDE
     X_SIDE(1,1) = 10.10D0
     X_SIDE(1,2) = 0.47D0 
     X_SIDE(1,3) = 0.07D0 
     X_SIDE(1,4) = 1D0

! +/- Y SIDE
     Y_SIDE(1,1) = 10.10D0
     Y_SIDE(1,2) = 0.47D0 
     Y_SIDE(1,3) = 0.07D0 
     Y_SIDE(1,4) = 1D0

! -Z SIDE
     Z_SIDE(1,1) = 5.60D0
     Z_SIDE(1,2) = 0.47D0
     Z_SIDE(1,3) = 0.07D0
     Z_SIDE(1,4) = 1D0

! +Z SIDE
     Z_SIDE(3,1) = 5.60D0
     Z_SIDE(3,2) = 0.47D0
     Z_SIDE(3,3) = 0.07D0
     Z_SIDE(3,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 45.00D0
     S_SIDE(1,2) = 0.88D0
     S_SIDE(1,3) = 0.07D0
     S_SIDE(1,4) = 1D0
      
  ELSEIF (BLKID.EQ.402.OR.BLKID.EQ.403) THEN
     blk_found = .true.

! +/- X SIDE
     X_SIDE(1,1) = 10.10D0
     X_SIDE(1,2) = 0.47D0 
     X_SIDE(1,3) = 0.07D0 
     X_SIDE(1,4) = 1D0

! +/- Y SIDE
     Y_SIDE(1,1) = 10.10D0
     Y_SIDE(1,2) = 0.47D0 
     Y_SIDE(1,3) = 0.07D0 
     Y_SIDE(1,4) = 1D0

! -Z SIDE
     Z_SIDE(1,1) = 5.60D0
     Z_SIDE(1,2) = 0.47D0
     Z_SIDE(1,3) = 0.07D0
     Z_SIDE(1,4) = 1D0

! +Z SIDE
     Z_SIDE(3,1) = 5.60D0
     Z_SIDE(3,2) = 0.47D0
     Z_SIDE(3,3) = 0.07D0
     Z_SIDE(3,4) = 1D0

! SOLAR PANELS
     S_SIDE(1,1) = 29.80D0
     S_SIDE(1,2) = 0.88D0
     S_SIDE(1,3) = 0.07D0
     S_SIDE(1,4) = 1D0


  ENDIF

! ---------------------------
! BOX-WING MODEL APROXIMATION
! ---------------------------

! NAVIGATION ANTENNAS
  X_SIDE(5,1) = X_SIDE(5,1)*12

! MATRIX WITH ALL SURFACES
  DO II = 1,13
     DO JJ = 1,4
        IF(II.LE.5)THEN
           SURFALL(II,JJ) = X_SIDE(II,JJ)
        ELSEIF((II.GE.6).AND.(II.LE.9))THEN
           SURFALL(II,JJ) = Z_SIDE(II-5,JJ)
        ELSEIF((II.GE.10).AND.(II.LE.11))THEN
           SURFALL(II,JJ) = S_SIDE(II-9,JJ)
        ELSEIF(II.GE.12)THEN
           SURFALL(II,JJ) = Y_SIDE(II-11,JJ)
        ENDIF
     ENDDO
  ENDDO
     

! COMPUTATION OF FRACTION OF REFLECTED, DIFFUSSED AND ABSORVED PHOTONS
! FROM SPECULARITY AND REFLECTIVITY
  DO II = 1,13
     ALLREFL(II) = SURFALL(II,2)*SURFALL(II,3)
     ALLDIFU(II) = SURFALL(II,3)*(1-SURFALL(II,2))
  ENDDO

! MULTIPLICATION OF OPTICAL PROPERTIES WITH SURFACE AREA
  DO II = 1,13
     REFL_AREA(II) = ALLREFL(II)*SURFALL(II,1)
     DIFU_AREA(II) = ALLDIFU(II)*SURFALL(II,1)
  ENDDO
  
! AVERAGE PER SURFACE (SEPARATE FOR FLAT AND CYLINDRICAL SURFACES)
  DO SS = 1,5
     G_AREA1(SS) = 0D0
     G_REFL1(SS) = 0D0
     G_DIFU1(SS) = 0D0
     G_ABSP1(SS) = 0D0
     G_AREA2(SS) = 0D0
     G_REFL2(SS) = 0D0
     G_DIFU2(SS) = 0D0
     G_ABSP2(SS) = 0D0
  ENDDO

  DO II = 1,13
!    +X SIDE 
     IF(II.LE.5)THEN
        SS = 1
!    -Z SIDE
     ELSEIF((II.GE.6).AND.(II.LE.7))THEN
        SS = 2
!    +Z SIDE
     ELSEIF((II.GE.8).AND.(II.LE.9))THEN
        SS = 3
!    SOLAR PANELS
     ELSEIF((II.GE.10).AND.(II.LE.11))THEN
        SS = 4
!    +/- Y SIDE
     ELSEIF(II.GE.12)THEN
        SS = 5
     ENDIF

     IF(SURFALL(II,4).EQ.1D0)THEN
        G_AREA1(SS) = G_AREA1(SS) + SURFALL(II,1)
        G_REFL1(SS) = G_REFL1(SS) + REFL_AREA(II)
        G_DIFU1(SS) = G_DIFU1(SS) + DIFU_AREA(II)
     ELSEIF(SURFALL(II,4).EQ.2D0)THEN
        G_AREA2(SS) = G_AREA2(SS) + SURFALL(II,1)
        G_REFL2(SS) = G_REFL2(SS) + REFL_AREA(II)
        G_DIFU2(SS) = G_DIFU2(SS) + DIFU_AREA(II)
     ENDIF
  ENDDO

! AVERAGE OF OPTICAL PROPERTIES ACCORDING TO SURFACES AREA
  DO SS = 1,5
     IF(G_AREA1(SS).GT.0D0)THEN
        G_REFL1(SS) = G_REFL1(SS)/G_AREA1(SS)
        G_DIFU1(SS) = G_DIFU1(SS)/G_AREA1(SS)
        G_ABSP1(SS) = 1D0 - (G_REFL1(SS) + G_DIFU1(SS))
     ENDIF
     IF(G_AREA2(SS).GT.0D0)THEN
        G_REFL2(SS) = G_REFL2(SS)/G_AREA2(SS)
        G_DIFU2(SS) = G_DIFU2(SS)/G_AREA2(SS)
        G_ABSP2(SS) = 1D0 -(G_REFL2(SS) + G_DIFU2(SS))
     ENDIF
  ENDDO

! --------------------------------------------
! ARRAYS OF OPTICAL PROPERTIES AND DIMENSIONS
! --------------------------------------------

  DO SS = 1,4
     DO KK = 1,2
        AREA(SS,KK) = 0D0
        REFL(SS,KK) = 0D0
        DIFU(SS,KK) = 0D0
        ABSP(SS,KK) = 0D0
        AREA2(SS,KK) = 0D0
        REFL2(SS,KK) = 0D0
        DIFU2(SS,KK) = 0D0
        ABSP2(SS,KK) = 0D0
     ENDDO
  ENDDO

! +Z FACE      
  AREA(1,1) = G_AREA1(3)
  REFL(1,1) = G_REFL1(3)
  DIFU(1,1) = G_DIFU1(3)
  ABSP(1,1) = G_ABSP1(3)

! -Z FACE
  AREA(1,2) = G_AREA1(2)
  REFL(1,2) = G_REFL1(2)
  DIFU(1,2) = G_DIFU1(2)
  ABSP(1,2) = G_ABSP1(2)

! +X FACE
  AREA(3,1) = G_AREA1(1)
  REFL(3,1) = G_REFL1(1)
  DIFU(3,1) = G_DIFU1(1)
  ABSP(3,1) = G_ABSP1(1)

! +X CYLINDRICAL
  AREA2(3,1) = G_AREA2(1)
  REFL2(3,1) = G_REFL2(1)
  DIFU2(3,1) = G_DIFU2(1)
  ABSP2(3,1) = G_ABSP2(1)

! -X FACE (ASSUMED FOR ALL BLOCKS)
  AREA(3,2) = G_AREA1(1)
  REFL(3,2) = G_REFL1(1)
  DIFU(3,2) = G_DIFU1(1)
  ABSP(3,2) = G_ABSP1(1)

! -X CYLINDRICAL (ASSUMED FOR ALL BLOCKS)                                               
  AREA2(3,2) = G_AREA2(1)
  REFL2(3,2) = G_REFL2(1)
  DIFU2(3,2) = G_DIFU2(1)
  ABSP2(3,2) = G_ABSP2(1)

! +Y FACE (ASSUMED FOR BLKID = 1...7)
  IF ((BLKID.GE.1).AND.(BLKID.LE.7)) THEN
!    DUE TO BLOCK IIR W-SENSOR
     IF ((BLKID.GE.4).AND.(BLKID.LE.7)) THEN      
        AREA(2,1) = ((AREA(1,1)-0.5D0) + AREA(3,1))/(2D0)
     ELSE
        AREA(2,1) = (AREA(1,1) + AREA(3,1))/(2D0)
     ENDIF
     REFL(2,1) = (REFL(1,1) + REFL(1,2) + REFL(3,1) + REFL(3,2))/4
     DIFU(2,1) = (DIFU(1,1) + DIFU(1,2) + DIFU(3,1) + DIFU(3,2))/4
     ABSP(2,1) = (ABSP(1,1) + ABSP(1,2) + ABSP(3,1) + ABSP(3,2))/4

!    CYLINDRICAL PART EQUAL TO +/- X CYLINDRICAL
     AREA2(2,1) = AREA2(3,1)
     REFL2(2,1) = REFL2(3,1)
     DIFU2(2,1) = DIFU2(3,1)
     ABSP2(2,1) = ABSP2(3,1)

! FOR OTHER SATELLITE BLOCKS
  ELSE
     AREA(2,1) = G_AREA1(5)
     REFL(2,1) = G_REFL1(5)
     DIFU(2,1) = G_DIFU1(5)
     ABSP(2,1) = G_ABSP1(5)

     AREA2(2,1) = G_AREA2(5)
     REFL2(2,1) = G_REFL2(5)
     DIFU2(2,1) = G_DIFU2(5)
     ABSP2(2,1) = G_ABSP2(5)
  ENDIF

! -Y FACE (ASSUMED FOR ALL BLOCKS)
  AREA(2,2) = AREA(2,1)
  REFL(2,2) = REFL(2,1)
  DIFU(2,2) = DIFU(2,1)
  ABSP(2,2) = ABSP(2,1)

! -Y CYLINDRICAL (ASSUMED FOR ALL BLOCKS)
  AREA2(2,2) = AREA2(2,1)
  REFL2(2,2) = REFL2(2,1)
  DIFU2(2,2) = DIFU2(2,1)
  ABSP2(2,2) = ABSP2(2,1)

! FRONT OF SOLAR PANELS
  AREA(4,1) = G_AREA1(4)
  REFL(4,1) = G_REFL1(4)
  DIFU(4,1) = G_DIFU1(4)
  ABSP(4,1) = G_ABSP1(4)

! FRONT OF SOLAR PANELS (CYLINDER)
  AREA2(4,1) = G_AREA2(4)
  REFL2(4,1) = G_REFL2(4)
  DIFU2(4,1) = G_DIFU2(4)
  ABSP2(4,1) = G_ABSP2(4)

! BACK OF SOLAR PANELS (ASSUMED FOR ALL BLOCKS)
  AREA(4,2) = AREA(4,1)
  REFL(4,2) = 0.055D0
  DIFU(4,2) = 0.055D0
  ABSP(4,2) = 0.890D0

!
  IF (BLKID.EQ.201.OR.BLKID.EQ.202) THEN
     REFL(4,2) = 0.00D0
     DIFU(4,2) = 0.19D0
     ABSP(4,2) = 0.81D0
  ENDIF

! BACK OF SOLAR PANELS (CYLINDER)
  AREA2(4,2) = G_AREA2(4)
  REFL2(4,2) = G_REFL2(4)
  DIFU2(4,2) = G_DIFU2(4)
  ABSP2(4,2) = G_ABSP2(4)


! OPTICAL PROPERTIES IN THE INFRARED
! ASSUMED FOR ALL BLOCKS AND ALL SURFACES
  DO SS=1,4
     DO II=1,2
        REFLIR(SS,II) = 0.10D0
        DIFUIR(SS,II) = 0.10D0
        ABSPIR(SS,II) = 0.80D0
     ENDDO
  ENDDO

!
  IF (BLKID.EQ.201) THEN
     blk_found = .true.   

!! The natural order of the next lines 
!! corresponds to an intuitive XYZ order
!! (but still in this function orientation) 
!! as it was previously implemented
!! But the index order (3,2,1) corresponds to 
!! this function effective orientation order as defined by the
!! header (ZYX)
!! 
!! Corection made by PSakic (20171102)
!!
!! X (ALONG TRACK FACES)            
     AREA(3,1) =  1.32D0
     AREA(3,2) =  1.32D0
!! Y (SOLAR PANELS FACES)
     AREA(2,1) =  3.00D0
     AREA(2,2) =  3.00D0
!! Z (TOWARD/OPPOSITE EARTH FACES)
     AREA(1,1) =  3.00D0
     AREA(1,2) =  3.00D0
!! SOLAR PANELS
     AREA(4,1) = 10.82D0
     AREA(4,2) = 10.82D0
     
     
     REFL(3,2) = 0.43D0
     DIFU(3,2) = 0.13D0
     ABSP(3,2) = 0.44D0
     REFL(3,1) = 0.00D0
     DIFU(3,1) = 0.06D0
     ABSP(3,1) = 0.94D0
     
     REFL(2,2) = 0.00D0
     DIFU(2,2) = 0.06D0
     ABSP(2,2) = 0.94D0
     REFL(2,1) = 0.15D0
     DIFU(2,1) = 0.16D0
     ABSP(2,1) = 0.69D0
     
     REFL(1,1) = 0.15D0
     DIFU(1,1) = 0.16D0
     ABSP(1,1) = 0.69D0
     REFL(1,2) = 0.00D0
     DIFU(1,2) = 0.06D0
     ABSP(1,2) = 0.94D0
     
     REFL(4,1) = 0.08D0
     DIFU(4,1) = 0.00D0
     ABSP(4,1) = 0.92D0
     REFL(4,2) = 0.00D0
     DIFU(4,2) = 0.19D0
     ABSP(4,2) = 0.81D0
     
     REFLIR(3,2) = 0.12D0
     DIFUIR(3,2) = 0.06D0
     ABSPIR(3,2) = 0.82D0
     REFLIR(3,1) = 0.00D0
     DIFUIR(3,1) = 0.16D0
     ABSPIR(3,1) = 0.84D0
     REFLIR(2,2) = 0.00D0
     DIFUIR(2,2) = 0.16D0
     ABSPIR(2,2) = 0.84D0
     REFLIR(2,1) = 0.09D0
     DIFUIR(2,1) = 0.14D0
     ABSPIR(2,1) = 0.77D0
     REFLIR(1,1) = 0.09D0
     DIFUIR(1,1) = 0.14D0
     ABSPIR(1,1) = 0.77D0
     REFLIR(1,2) = 0.00D0
     DIFUIR(1,2) = 0.16D0
     ABSPIR(1,2) = 0.84D0
     REFLIR(4,1) = 0.31D0
     DIFUIR(4,1) = 0.00D0
     ABSPIR(4,1) = 0.69D0
     REFLIR(4,2) = 0.00D0
     DIFUIR(4,2) = 0.27D0
     ABSPIR(4,2) = 0.73D0

  ENDIF

  IF (BLKID.EQ.202) THEN
     blk_found = .true.   

!! Implemented by PSakic 20171101
!! Z (TOWARD/OPPOSITE EARTH FACES)
     AREA(1,1) =  3.036D0
     AREA(1,2) =  3.036D0
!! Y (SOLAR PANELS FACES)
     AREA(2,1) =  2.783D0
     AREA(2,2) =  2.783D0
!! X (ALONG TRACK FACES)
     AREA(3,1) =  1.320D0
     AREA(3,2) =  1.320D0
!! SOLAR PANELS
     AREA(4,1) = 10.82D0
     AREA(4,2) = 10.82D0
     
     REFL(1,1) = 0.14D0
     DIFU(1,1) = 0.16D0
     ABSP(1,1) = 0.70D0
     REFL(1,2) = 0.23D0
     DIFU(1,2) = 0.11D0
     ABSP(1,2) = 0.66D0
     
     REFL(2,1) = 0.40D0
     DIFU(2,1) = 0.14D0
     ABSP(2,1) = 0.46D0
     REFL(2,2) = 0.43D0
     DIFU(2,2) = 0.14D0
     ABSP(2,2) = 0.42D0
     
     REFL(3,1) = 0.00D0
     DIFU(3,1) = 0.07D0
     ABSP(3,1) = 0.93D0
     REFL(3,2) = 0.49D0
     DIFU(3,2) = 0.15D0
     ABSP(3,2) = 0.36D0
     
     REFL(4,1) = 0.09D0
     DIFU(4,1) = 0.00D0
     ABSP(4,1) = 0.91D0
     REFL(4,2) = 0.09D0
     DIFU(4,2) = 0.00D0
     ABSP(4,2) = 0.91D0
     
!! IR VALUES ARE BASED ON THE IOV
     REFLIR(3,2) = 0.12D0
     DIFUIR(3,2) = 0.06D0
     ABSPIR(3,2) = 0.82D0
     REFLIR(3,1) = 0.00D0
     DIFUIR(3,1) = 0.16D0
     ABSPIR(3,1) = 0.84D0
     REFLIR(2,2) = 0.00D0
     DIFUIR(2,2) = 0.16D0
     ABSPIR(2,2) = 0.84D0
     REFLIR(2,1) = 0.09D0
     DIFUIR(2,1) = 0.14D0
     ABSPIR(2,1) = 0.77D0
     REFLIR(1,1) = 0.09D0
     DIFUIR(1,1) = 0.14D0
     ABSPIR(1,1) = 0.77D0
     REFLIR(1,2) = 0.00D0
     DIFUIR(1,2) = 0.16D0
     ABSPIR(1,2) = 0.84D0
     REFLIR(4,1) = 0.31D0
     DIFUIR(4,1) = 0.00D0
     ABSPIR(4,1) = 0.69D0
     REFLIR(4,2) = 0.00D0
     DIFUIR(4,2) = 0.27D0
     ABSPIR(4,2) = 0.73D0

! VALUE TABLES IN GALILEO FOC ORIENTATION
!
! NB : THIS FUNCTION ORIENTATION != GALILEO FOC ORIENTATION, SEE HEADER 
! based on :
! https://www.gsc-europa.eu/support-to-developers/galileo-satellite-metadata
!
!			surface	% of surface	Alpha	Rho	Delta	
!Bx	+X	A	0,44	0,33	0,93	0,00	0,07	
!	 	C	0,88	0,67	0,08	0,73	0,19	
!			1,32			0,36	0,49	0,15	<
								
								
!	-X	A	1,32			0,93	0,00	0,07	<
								
!	+Y	A	1,13	0,41	0,93	0,00	0,07	
!	 	C	1,65	0,59	0,08	0,73	0,19	
!			2,78			0,42	0,43	0,14	<
								
!	-Y	A	1,24	0,45	0,93	0,00	0,07	
!	 	C	1,54	0,55	0,08	0,73	0,19	
!			2,78			0,46	0,40	0,14	<
								
!	+Z	A	1,05	0,35	0,93	0,00	0,07	
!	 	B	1,97	0,65	0,57	0,22	0,21	
!			3,02			0,70	0,14	0,16	<
								
!	-Z	A	2,08	0,68	0,93	0,00	0,07	
!	 	C	0,96	0,32	0,08	0,73	0,19	
!			3,04			0,66	0,23	0,11	<
								
!Wi	+SA	E	3,88			0,92	0,08	0,00	
!	 	D	1,53			0,90	0,10	0,00	
!						0,91	0,09	0,00	<
!	-SA	E	3,88	0,72	0,92	0,08	0,00	
!	 	D	1,53	0,28	0,90	0,10	0,00	
!			5,41			0,91	0,09	0,00	<

  ENDIF

! MOD TAH 190709: Reset values to 0 if blk_found is .false.
  IF ( .not. blk_found ) THEN

     DO SS=1,4
        DO II=1,2
           AREA(SS,II) = 0D0
           REFL(SS,II) = 0D0
           DIFU(SS,II) = 0D0
           ABSP(SS,II) = 0D0

           AREA2(SS,II) = 0D0
           REFL2(SS,II) = 0D0
           DIFU2(SS,II) = 0D0
           ABSP2(SS,II) = 0D0

           REFLIR(SS,II) = 0D0
           DIFUIR(SS,II) = 0D0
           ABSPIR(SS,II) = 0D0
        ENDDO
     ENDDO
     
     write(mesg, *) "BOXWING Properties for BLKID: (", BLKID, ") NOT FOUND. "
     call report('WARNING', pgrm_name, 'PROPBOXW', mesg, 'src/fortran/PROPBOXW.f90', 1)     
     
  ENDIF

END SUBROUTINE
