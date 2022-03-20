MODULE m_satinfo


      IMPLICIT NONE
      !SAVE


Contains


SUBROUTINE satinfo(mjd,prnnum, satsvn, satblk)

! ------------------------------------------------------------------------------
! Purpose:    This subroutine is used to read the satellite info file satinfo.txt
!             and give back the structure "satellite" and the time domain for
!             the specific satellite operated.
!
! Parameters:
!         in:     mjd
!                 Satellite PRN
!                         
!        out:     Satellite SVN 
!                 Satellite Block type
!
! Remarks:   BLOCK NUMBER (GPS):
!                               1: BLOCK I
!                               2: BLOCK II
!                               3: BLOCK IIA
!                               4: BLOCK IIR
!                               5: BLOCK IIR-A
!                               6: BLOCK IIR-B
!                               7: BLOCK IIR-M
!                               8: BLOCK IIF
!
!            BLOCK NUMBER (GLONASS):
!                               101: GLONASS
!                               102: GLONASS-M
!                               103: GLONASS-K1
!
!            BLOCK NUMBER (BDS): 
!                                21: MEO
!                                22: IGSO
!                                23: GEO
!          
!
! Author:     Tzupang Tseng
!
! Created:    27-09-2018
! 
!
! Changes:    12-10-2018   Tzupang Tseng: fixed the bugs related to the st_time
!                                         bigger than the end_time
!
! Copyright:  Geoscience Australia, Australia
! ------------------------------------------------------------------------------

  USE mdl_precision
  use mdl_config
  !USE mdl_param, ONLY: satinfoplace ! Temporary deactivated
  IMPLICIT NONE

! variables
! ---------------
  REAL (KIND = prec_q)       :: mjd
  INTEGER(KIND = 4)          :: prnnum
  INTEGER(KIND = 4)          :: satsvn
  INTEGER(KIND = 4)          :: satblk
!
! Local Variables
! ---------------
  CHARACTER(LEN=512)         :: line
  CHARACTER(LEN=100)         :: mesg

  INTEGER(KIND=4)               :: nlin
  INTEGER(KIND=4)               :: icrx
  INTEGER(KIND=4)               :: ii
  INTEGER(KIND=4)               :: ios,J
  INTEGER(KIND=4)               :: iac,ifile
  REAL(KIND = prec_q)           :: st_time,end_time,DJM0

! PART 1: PHYSICAL SATELLITE PARAMETERS
! -------------------------------------
  TYPE sat
    INTEGER(KIND=4)             :: prn     !Satellite number (PRN)
    INTEGER(KIND=4)             :: svn     ! SVN
                                          
    INTEGER(KIND=4)             :: iblock  !Block number
    CHARACTER(LEN=9)            :: cospar  !Cospar ID
    INTEGER(KIND=4)             :: attflag !Attitude flag

    INTEGER(KIND=4)             :: st_YY
    INTEGER(KIND=4)             :: st_MM
    INTEGER(KIND=4)             :: st_DD
    INTEGER(KIND=4)             :: st_HR
    INTEGER(KIND=4)             :: st_MIN
    INTEGER(KIND=4)             :: st_SEC
    INTEGER(KIND=4)             :: end_YY
    INTEGER(KIND=4)             :: end_MM
    INTEGER(KIND=4)             :: end_DD
    INTEGER(KIND=4)             :: end_HR
    INTEGER(KIND=4)             :: end_MIN
    INTEGER(KIND=4)             :: end_SEC

    REAL(KIND = prec_q)         :: mass    !Satellite mass
    REAL(KIND = prec_q)         :: formf   !Form factor
    REAL(KIND = prec_q)         :: radpres !Radiation pressure coeff. C0
    INTEGER(KIND=4)             :: admodel !Air drag model
    REAL(KIND = prec_q)         :: adrag   !Air drag coeff.
    CHARACTER(LEN=3)            :: plane   !plane

  END TYPE sat

  TYPE(sat), DIMENSION(:), POINTER :: satellite

!
! Create the satellite information file
! -----------------------------------

  ifile  = 88
  !OPEN (ifile, file= satinfoplace//"satinfo.dat", status = 'old')  ! Temporary deactivated
  OPEN (ifile, file= "satinfo.dat", status = 'old')
!
! Read the content of satinfo.txt
! -------------------------------
  nlin = 0

  Loop_001: DO
  READ (ifile,'(A)') line
     nlin=nlin+1

        IF (line(1:8) == 'PRN  SVN') THEN
          READ (ifile,'(A)')
          nlin = nlin - 2 
        ELSE IF (line == '') THEN
          EXIT Loop_001

        END IF
  END DO Loop_001


!
! Allocate memory for requests
! ----------------------------
!print*,'nlin=',nlin
  ALLOCATE(satellite(nlin),stat=iac)
  if (iac .ne. 0) then
          write(mesg, *) "not enough memory - failed to allocate satellite array, dimenion = ", nlin
          call report('FATAL', pgrm_name, 'satinfo', mesg, 'src/fortran/m_satifno.f90', 1)
  end if

  REWIND(ifile)

! Read PART 1: PHYSICAL SATELLITE PARAMETERS t_satellite
! ------------------------------------------------------
!
 
  READ (ifile,'(A)') ! Skip the first line
  READ (ifile,'(A)') ! Skip the second line

!
! Read satellite parameters
! -------------------------
  DO ii = 1,nlin 
     READ (ifile,'(A)') line
     icrx = ii 

     READ (line, '(I3,2X,I3,I4,3X,A9,I6,5X,I4,5(1X,I2),2X,I4,5(1X,I2),F10.1)')        &
          satellite(icrx)%prn   , satellite(icrx)%svn,      satellite(icrx)%iblock,   &
          satellite(icrx)%cospar, satellite(icrx)%attflag,  satellite(icrx)%st_YY ,   &
          satellite(icrx)%st_MM,  satellite(icrx)%st_DD,    satellite(icrx)%st_HR ,   &
          satellite(icrx)%st_MIN, satellite(icrx)%st_SEC,   satellite(icrx)%end_YY,   &
          satellite(icrx)%end_MM, satellite(icrx)%end_DD,   satellite(icrx)%end_HR,   &
          satellite(icrx)%end_MIN,satellite(icrx)%end_SEC,  satellite(icrx)%mass

!print*, prnnum, satellite(icrx)%prn, satellite(icrx)%svn

!print*, satellite(icrx)%st_YY, satellite(icrx)%st_MM, satellite(icrx)%st_DD
!print*, satellite(icrx)%end_YY, satellite(icrx)%end_MM, satellite(icrx)%end_DD

!
! Transform time information given in Y,M,D,H,M,S into Julian date
! ----------------------------------------------------------------
    CALL iau_CAL2JD (satellite(icrx)%st_YY, satellite(icrx)%st_MM, satellite(icrx)%st_DD, DJM0, st_time, J )
    CALL iau_CAL2JD (satellite(icrx)%end_YY, satellite(icrx)%end_MM, satellite(icrx)%end_DD, DJM0, end_time, J )

!print*, st_time, mjd, end_time

     IF (prnnum .eq. satellite(icrx)%prn) then

        if ( mjd .ge. st_time .and. mjd .le. end_time) then
         satsvn = satellite(icrx)%svn
         satblk = satellite(icrx)%iblock
         exit

        elseif (st_time .gt. end_time .and. mjd .ge. st_time) then
         satsvn = satellite(icrx)%svn
         satblk = satellite(icrx)%iblock
         exit

        end if
  
     END IF

  END DO

 DEALLOCATE(satellite)
 100 CLOSE(UNIT = 88)

 END SUBROUTINE 

END MODULE
