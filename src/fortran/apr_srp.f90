
SUBROUTINE apr_srp (GNSSid, BLKTYP, X_SIDE, Z_SIDE, A_SOLAR, F0)


! ----------------------------------------------------------------------
! SUBROUTINE: apr_srp
! ----------------------------------------------------------------------
! Purpose:
! TBD
! ----------------------------------------------------------------------
! Input arguments:
! - GNSSid       : id of satellite constellation  
! - blktyp       : satellite block type
! 
! Output arguments:
! - X_SIDE       : area of X-sdie in body-fixed frame
! - Z_SIDE       : area of Z-side in body-fixed frame
! - A_SOLAR      : area of solar panel
! - F0           : net force (computed from cannoball) in D direction
! ----------------------------------------------------------------------
! Author :	John Donovan
!
! Created:	04 Feb 2020
!
! Copyright:  GEOSCIENCE AUSTRALIA
! ----------------------------------------------------------------------

      USE mdl_precision
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=1), INTENT(IN) :: GNSSid
      CHARACTER (LEN=10), INTENT(IN) :: BLKTYP
! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Satellite physical informaiton
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), INTENT(OUT) :: X_SIDE, Z_SIDE
      REAL (KIND = prec_q), INTENT(OUT) :: A_SOLAR, F0
! ----------------------------------------------------------------------

! default init
Z_SIDE = 0.d0
X_SIDE = 0.d0
A_SOLAR = 0.d0
F0 = 0.d0

IF (GNSSid == 'G') THEN
! GPS constellation
! -----------------
! I
         if (TRIM(BLKTYP)=='GPS-I')then
         Z_SIDE = 3.020D0
         X_SIDE = 1.728D0
         A_SOLAR= 6.053D0
         F0 = 4.54d-5

! II and IIA
         else if (TRIM(BLKTYP)=='GPS-II' .or. TRIM(BLKTYP)=='GPS-IIA') then
         Z_SIDE = 2.881D0
         X_SIDE = 2.893D0
         A_SOLAR= 11.871D0
         F0 = 8.695d-5
! IIF
         else if(TRIM(BLKTYP)=='GPS-IIF') then
         Z_SIDE = 5.05D0
         X_SIDE = 4.55D0
         A_SOLAR= 22.25D0
         F0 = 16.7d-5
! IIR
         else if (TRIM(BLKTYP)=='GPS-IIR' .or. TRIM(BLKTYP)=='GPS-IIR-A' .or. &
                  TRIM(BLKTYP)=='GPS-IIR-B'.or.TRIM(BLKTYP)=='GPS-IIR-M') then
         Z_SIDE = 4.25D0
         X_SIDE = 4.11D0
         A_SOLAR= 13.92D0
         F0 = 11.15d-5
! III
         else if (TRIM(BLKTYP)=='GPS-IIIA') then
         Z_SIDE = 4.38D0
         X_SIDE = 6.05D0
         A_SOLAR= 22.25D0
         F0 = 11.0d-5

         else
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'G'"
                 STOP     
         end if

ELSE IF (GNSSid == 'R') THEN
! GLONASS constellation
! ---------------------
         if(TRIM(BLKTYP)=='GLO'.or.TRIM(BLKTYP)=='GLO-M'.or.TRIM(BLKTYP)=='GLO-M+'.or.&
            TRIM(BLKTYP)=='GLO-K1A'.or.TRIM(BLKTYP)=='GLO-K1B')then
         Z_SIDE = 1.6620D0
         X_SIDE = 4.200D0
         A_SOLAR= 23.616D0
! GLONASS-K
         if(TRIM(BLKTYP)=='GLO-K1A'.or.TRIM(BLKTYP)=='GLO-K1B') F0 = 10.0d-5
! GLONASS-M
         if(TRIM(BLKTYP)=='GLO'.or.TRIM(BLKTYP)=='GLO-M'.or.TRIM(BLKTYP)=='GLO-M+') F0 = 20.9d-5

         else
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'R'"
                 STOP     
         end if

ELSE IF (GNSSid == 'E') THEN
! GALILEO constellation
! ---------------------
         if (TRIM(BLKTYP)=='GAL-1'.or.TRIM(BLKTYP)=='GAL-2') then
         Z_SIDE = 3.002D0
         X_SIDE = 1.323D0
         A_SOLAR= 11.0D0
         F0 = 8.35d-5
       
         else
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'E'"
                 STOP     
         end if

ELSE IF (GNSSid == 'C') THEN
! BDS constellation
! -----------------
         Z_SIDE = 3.96D0
         X_SIDE = 4.5D0
         A_SOLAR= 22.44D0
! BDS GEO
         if(TRIM(BLKTYP)=='BDS-2G') F0 = 50.1d-5
! BDS MEO
         if(TRIM(BLKTYP)=='BDS-2M') F0 = 8.35d-5
! BDS IGSO
         if(TRIM(BLKTYP)=='BDS-2I') F0 = 17.0d-5
         
         if (BLKTYP(1:3)/='BDS') then
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'C'"
                 STOP     
         end if

ELSE IF (GNSSid == 'J') THEN
! QZSS constellation
! ------------------
! QZSS-1
         if (TRIM(BLKTYP)=='QZS-1') then
         Z_SIDE = 5.6D0
         X_SIDE = 9.0D0
         A_SOLAR= 45.0D0
         F0 = 35.0d-5
! QZSS-2I
         else if (TRIM(BLKTYP)=='QZS-2I') then
         Z_SIDE = 5.6D0
         X_SIDE = 10.1D0
         A_SOLAR= 29.8D0
         F0 = 25.0d-5
! QZSS-2G
         else if (TRIM(BLKTYP)=='QZS-2G') then
         Z_SIDE = 5.6D0
         X_SIDE = 10.1D0
         A_SOLAR= 29.8D0
         F0 = 25.0d-5

         else
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'J'"
                 STOP
         end if

END IF

END SUBROUTINE
