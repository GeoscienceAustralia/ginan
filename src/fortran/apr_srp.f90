
SUBROUTINE apr_srp (GNSSid, BLKID, BLKTYP, X_SIDE, Z_SIDE, A_SOLAR, F0)


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
      INTEGER, INTENT(IN) :: BLKID
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
         if (BLKID == 1)then
         Z_SIDE = 3.020D0
         X_SIDE = 1.728D0
         A_SOLAR= 6.053D0
         F0 = 4.54d-5

! II and IIA
         else if (BLKID == 2 .or. BLKID == 3) then
         Z_SIDE = 2.881D0
         X_SIDE = 2.893D0
         A_SOLAR= 11.871D0
         F0 = 8.695d-5
! IIF
         else if(BLKID == 8) then
         Z_SIDE = 5.05D0
         X_SIDE = 4.55D0
         A_SOLAR= 22.25D0
         F0 = 16.7d-5
! IIR
         else if (BLKID .GE. 4 .and. BLKID .LE. 7) then
         Z_SIDE = 4.25D0
         X_SIDE = 4.11D0
         A_SOLAR= 13.92D0
         F0 = 11.15d-5
! III
         else if (BLKID == 9) then
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
         if(BLKID .GE. 101 .and. BLKID .LE. 103) then
         Z_SIDE = 1.6620D0
         X_SIDE = 4.200D0
         A_SOLAR= 23.616D0
! GLONASS-K
         if(BLKID == 103) F0 = 10.0d-5
! GLONASS-M
         if(BLKID == 101 .or. BLKID == 102) F0 = 20.9d-5

         else
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'R'"
                 STOP     
         end if

ELSE IF (GNSSid == 'E') THEN
! GALILEO constellation
! ---------------------
         if (BLKID == 201 .or. BLKID == 202) then
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
! BDS 2 MEO
         if (BLKID == 301) then
                F0 = 15.5d-5
                Z_SIDE = 3.44D0
                X_SIDE = 3.78D0
                A_SOLAR= 11.35D0
! BDS 2 IGSO
         else if (BLKID == 302) then
                F0 = 16.1d-5
                Z_SIDE = 3.44D0
                X_SIDE = 3.78D0
                A_SOLAR= 11.35D0
! BDS 2 GEO
         else if (BLKID == 303) then
                F0 = 50.1d-5
                Z_SIDE = 3.44D0
                X_SIDE = 3.78D0
                A_SOLAR= 11.35D0
! BDS 3 MEO CAST
         else if (BLKID == 304) then
                F0 = 13.7d-5
                Z_SIDE = 2.18D0
                X_SIDE = 2.86D0
                A_SOLAR= 10.22D0
! BDS 3 IGSO CAST
         else if (BLKID == 305) then
                F0 =  27.6d-5
                Z_SIDE = 4.956D0
                X_SIDE = 8.496D0
                A_SOLAR= 17.70D0
! BDS 3 MEO SECM A or BDS 3 IGSO SECM
         else if (BLKID == 306 .or. BLKID == 307) then
                F0 = 7.8d-5
                Z_SIDE = 2.59D0
                X_SIDE = 1.25D0
                A_SOLAR= 5.40D0
! BDS 3 MEO SECM B
         else if (BLKID == 308) then
                F0 = 8.2d-5
                Z_SIDE = 2.57D0
                X_SIDE = 1.24D0
                A_SOLAR= 5.40D0
! BDS 3 GEO
         else if (BLKID == 309) then
                F0 = 88.5d-5
                Z_SIDE = 4.956D0
                X_SIDE = 8.496D0
                A_SOLAR= 17.70D0
         else
                print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'C'"
                STOP     
         endif

ELSE IF (GNSSid == 'J') THEN
! QZSS constellation
! ------------------
! QZSS-1
         if (BLKID == 401) then
         Z_SIDE = 5.6D0
         X_SIDE = 9.0D0
         A_SOLAR= 45.0D0
         F0 = 35.0d-5
! QZSS-2I
         else if (BLKID == 402) then
         Z_SIDE = 5.6D0
         X_SIDE = 10.1D0
         A_SOLAR= 29.8D0
         F0 = 25.0d-5
! QZSS-2G
         else if (BLKID == 403) then
         Z_SIDE = 5.6D0
         X_SIDE = 10.1D0
         A_SOLAR= 29.8D0
         F0 = 25.0d-5
! QZSS-2A
         else if (BLKID == 404) then
         Z_SIDE = 5.6D0
         X_SIDE = 10.8D0
         A_SOLAR= 29.8D0
         F0 = 25.0d-5

         else
                 print *,'apr_srp - Unknown block type: ',BLKTYP, ", GNSSid = 'J'"
                 STOP
         end if

END IF


END SUBROUTINE
