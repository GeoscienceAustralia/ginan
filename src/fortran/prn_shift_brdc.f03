SUBROUTINE prn_shift_brdc (GNSSid, PRN_no, PRN_sh)


! ----------------------------------------------------------------------
! SUBROUTINE: prn_shift_brdc.f03
! ----------------------------------------------------------------------
! Purpose:
!  This subroutine is used to shift the prn number to internally identify the individual
!  satellite for broadcast orbit. 
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
!                          R: GLONASS + 50
!                          E: GALILEO + 100
!                          C: BDS     + 150
!                          J: QZSS    + 200
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

         IF (GNSSid == "R") PRN_sh = PRN_no + 50

         IF (GNSSid == "E") PRN_sh = PRN_no + 100

         IF (GNSSid == "C") PRN_sh = PRN_no + 150
        
         IF (GNSSid == "J") PRN_sh = PRN_no + 200



End
