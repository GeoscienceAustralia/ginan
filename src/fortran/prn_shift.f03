SUBROUTINE prn_shift (GNSSid, PRN_no, PRN_sh)


! ----------------------------------------------------------------------
! SUBROUTINE: prn_shift.f03
! ----------------------------------------------------------------------
! Purpose:
!  This subroutine is used to shift the prn number to internally identify the individual
!  satellite for solar radiation pressure modeling. 
! ----------------------------------------------------------------------
! Input arguments:
! - GNSSid:		ID for multi-GNSS constellations 
!                          G: GPS 
!                          R: GLONASS
!                          E: GALILEO
!                          C: BDS
!                          J: QZSS
! - PRN_no:		The number comes up with the GNSSid from SP3 file
! 
! Output arguments:
! - PRN_sh:		The number shifted by GNSSid
!                          G: GPS     
!                          R: GLONASS + 100
!                          E: GALILEO + 200
!                          C: BDS     + 300
!                          J: QZSS    + 400
!
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
! 
! Copyright: Geoscience Australia, Australia
!
! Created:	17-AUG-2018
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

      CHARACTER (LEN=1) :: GNSSid
      INTEGER (KIND = prec_int4) :: PRN_no, PRN_sh

! ----------------------------------------------------------------------
! Shift PRN number for internally process
! ----------------------------------------------------------------------

         IF (GNSSid == "G") PRN_sh = PRN_no

         IF (GNSSid == "R") PRN_sh = PRN_no + 100

         IF (GNSSid == "E") PRN_sh = PRN_no + 200

         IF (GNSSid == "C") PRN_sh = PRN_no + 300
        
         IF (GNSSid == "J") PRN_sh = PRN_no + 400



End
