MODULE m_read_satsnx

! ----------------------------------------------------------------------
! MODULE: m_read_satsnx.f03
! ----------------------------------------------------------------------
! Purpose:
!
!  Module for reading IGS metadat SINEX file and stroing info as global
!  variables 
! 
! ----------------------------------------------------------------------
! Author :	Tzupang Tseng, Geoscience Australia, Australia
! Created:	05-09-2019
! ----------------------------------------------------------------------

      IMPLICIT NONE
!      SAVE		
  	  
Contains
  
SUBROUTINE read_satsnx (satsinex_filename, Iyr, iday, Sec_00, PRN_isat)

! ----------------------------------------------------------------------
! SUBROUTINE: read_satsnx.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read and store satellite metadata information
! ----------------------------------------------------------------------
! Input arguments:
! - satsinex_filename:	Name of satellite SINEX file containing 
!                       satellite prn and svn numbers, transmitting antenna power,
!                       satellite mass and satellite block types
! - Iyr    : 4 digit year
! - iday   : Day of year
! - Sec_00 : Seconds of the day
!
! Ouptut arguments: (None)
! ----------------------------------------------------------------------
! Remarks:
!  
      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config
      USE m_read_svsinex
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy argument declarations
! ----------------------------------------------------------------------
      CHARACTER (LEN=*) :: satsinex_filename
      CHARACTER (LEN=3) :: PRN_isat
      CHARACTER (LEN=50) :: mesg
! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int4) :: idir     ! Direction of PRN->SVN or vice-versa
      INTEGER (KIND = prec_int4) :: UNIT_IN,isat,iyr,iday,ihr,imin
      CHARACTER (LEN=1) :: gnss

      REAL (KIND = prec_d) :: Sec_00
      CHARACTER (LEN=128)  :: cha
      LOGICAL found
! ----------------------------------------------------------------------
      UNIT_IN = 90  												
      idir = -1
      found = .false.

      ihr  = INT(Sec_00/3600.d0)
      Sec_00 = Sec_00 - ihr * 3600.d0
      imin = INT(Sec_00/60.d0) 
      Sec_00 = Sec_00 - imin * 60.d0
      READ(PRN_isat,'(A1,I2)')gnss, isat

      !! Only read the data once. After that skip to lookup
      if (SAT_COUNT == 0) then
! ----------------------------------------------------------------------
! Open IGS metadata file
      OPEN (UNIT = UNIT_IN, FILE = TRIM(satsinex_filename), status='old', IOSTAT = ios)
      
      IF (ios /= 0) THEN
         PRINT *, "Error in opening IGS metadata file:", satsinex_filename
         PRINT *, "OPEN IOSTAT=", ios
         STOP
      END IF
      
      CALL read_sinex_file (UNIT_IN)
 
! ----------------------------------------------------------------------
      CLOSE (UNIT=UNIT_IN)
      endif
      	  
      CALL lookup_sinex (idir,found, iyr,iday,ihr,imin,gnss,isat, &
                         SVNID,BLKTYP,BLKID,MASS,POWER)

      if (.not.found) then
              write (mesg, '(" yr = ", i4, ", doy = ", i4, ", hour = ", i2, ", minute = ", i2)') iyr, iday, ihr, imin
          call report('WARNING', pgrm_name, 'm_read_satsnx', &
              'Could not find satellite metadata: '//PRN_Isat//mesg, ' ', 0)
      endif
      
END SUBROUTINE

END
