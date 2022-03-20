SUBROUTINE prn2str (PRN_no, STR)


! ----------------------------------------------------------------------
! SUBROUTINE: prn2str.f03
! ----------------------------------------------------------------------
! Purpose:
!  This subroutine is used to convert the prn number to string  
! ----------------------------------------------------------------------
! Input arguments:
! - PRN_no:		ID for multi-GNSS constellations 
!                          G:          GPS    < 50 
!                          R:  50  < GLONASS  < 100 
!                          E: 100  < GALILEO  < 150
!                          C: 150  <   BDS    < 200
!                          J: 200  <  QZSS    < 250
! 
! Output arguments:
! - STR:		
!                          G: GPS     
!                          R: GLONASS 
!                          E: GALILEO 
!                          C: BDS     
!                          J: QZSS    
!
! ----------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng
! 
! Copyright: Geoscience Australia, Australia
!
! Created:	02-07-2019
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

      CHARACTER (LEN=3) :: STR
      INTEGER (KIND = prec_int4) :: PRN_no

! ----------------------------------------------------------------------
! Shift PRN number for internally process
! ----------------------------------------------------------------------

         IF (PRN_no <= 50) THEN
            WRITE(STR(1:1),'(1A)') 'G'
            IF (PRN_no <= 9) THEN
               WRITE(STR(2:2),'(1A)') '0'
               WRITE(STR(3:3),'(I1)') PRN_no
            ELSE
               WRITE(STR(2:3),'(I2)') PRN_no
            END IF
         ELSE IF (PRN_no < 100 .and. PRN_no > 50)THEN
            WRITE(STR(1:1),'(1A)') 'R'
            IF (PRN_no-50 <= 9) THEN
               WRITE(STR(2:2),'(1A)') '0'
               WRITE(STR(3:3),'(I1)') PRN_no-50
            ELSE
               WRITE(STR(2:3),'(I2)') PRN_no-50
            END IF
         ELSE IF (PRN_no < 150 .and. PRN_no > 100)THEN
            WRITE(STR(1:1),'(1A)') 'E'
            IF (PRN_no-100 <= 9) THEN
               WRITE(STR(2:2),'(1A)') '0'
               WRITE(STR(3:3),'(I1)') PRN_no-100
            ELSE
               WRITE(STR(2:3),'(I2)') PRN_no-100
            END IF
         ELSE IF (PRN_no < 200 .and. PRN_no > 150)THEN
            WRITE(STR(1:1),'(1A)') 'C'
            IF (PRN_no-150 <= 9) THEN
               WRITE(STR(2:2),'(1A)') '0'
               WRITE(STR(3:3),'(I1)') PRN_no-150
            ELSE
               WRITE(STR(2:3),'(I2)') PRN_no-150
            END IF
         ELSE IF (PRN_no < 250 .and. PRN_no > 200)THEN
            WRITE(STR(1:1),'(1A)') 'J'
            IF (PRN_no-200 <= 9) THEN
               WRITE(STR(2:2),'(1A)') '0'
               WRITE(STR(3:3),'(I1)') PRN_no-200
            ELSE
               WRITE(STR(2:3),'(I2)') PRN_no-200
            END IF
         END IF         

End
