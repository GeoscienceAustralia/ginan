SUBROUTINE eop_c04 (filename,mjd , eop)


! ----------------------------------------------------------------------
! Subroutine:  eop_c04.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read data format of EOP (Earth Orientation Parameters) C04 combined series
!  provided by the Earth Orientation Centre of the IERS (International Earth 
!  Rotation Service and Reference Systems)
! ----------------------------------------------------------------------
! Input arguments:
! - filename:		EOP data file name  e.g. 'eopc04_IAU2000.62-now'
! - mjd:			Modified Julian Day number of the required date (in UTC)
!
! Output arguments:
! - eop:			Array of the EOP data for the input day
!   				eop = [MJD xp yp UT1_UTC LOD dX dY]  1x6 matrix
!   				MJD:     MJD referred to 0h in UTC time scale 
!   				x,y:     Polar motion coordinates (in arcsec) 
!   				UT1_UTC: Difference between UT1 and UTC (in seconds)
!					dX,dY:   Corrections to Precession-Nutation model (arcsec)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia            December 2015
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int8), INTENT(IN) :: mjd
      CHARACTER (LEN=512), INTENT(IN) :: filename
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: eop(EOP_MAX_ARRAY)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_line, ios_data
      INTEGER (KIND = prec_int8) :: i, read_i, len_line
      CHARACTER (LEN=100) :: Format_eop
      CHARACTER (LEN=170) :: line_ith, eop_line 
      CHARACTER :: first
      INTEGER (KIND = prec_int8) :: year,month,day, mjd_day
      REAL (KIND = prec_d) :: xp,yp,UT1_UTC,LOD,dX,dY, xErr,yErr,UT1_UTC_Err,LOD_Err,dX_Err,dY_Err
      LOGICAL found
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      found = .FALSE.
      UNIT_IN = 9
      Format_eop = '(3(I4),I7,2(F11.6),2(F12.7),2(F11.6),2(F11.6),2(F11.7),2F12.6)'
! ----------------------------------------------------------------------
!        just some debug to be deleted
!        print *, "reading EOP data from ", TRIM(filename), " for day ", mjd

! ----------------------------------------------------------------------
! Open data file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (filename), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", filename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read data file
! ----------------------------------------------------------------------
      i = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
	     i = i + 1
!  	     PRINT *, "READ Line (i,ios):", i, ios_line
! ----------------------------------------------------------------------
! End of file condition
! ----------------------------------------------------------------------
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------
! Data - check how much we read
         !skip lines where year does not start with 1 or 2 ...
         first = line_ith(1:1)
         if (first .ne. '1' .and. first .ne. '2') cycle

         len_line = len(line_ith)
         IF (len_line >= 155) THEN
	        READ (line_ith, '(3(I4),I7, 170A)' , IOSTAT=ios_data) year,month,day, mjd_day, eop_line			
            IF (mjd_day == mjd) THEN
	           READ (line_ith, Format_eop, IOSTAT=ios_data) year,month,day, mjd_day, & 
                                                            xp,yp,UT1_UTC,LOD,dX,dY, & 
                                                            xErr,yErr,UT1_UTC_Err,LOD_Err,dX_Err,dY_Err
	           !READ (line_ith, * , IOSTAT=ios_data) year,month,day, mjd_day, xp,yp,UT1_UTC,LOD,dX,dY, xErr,yErr,UT1_UTC_Err,LOD_Err,dX_Err,dY_Err
               eop(EOP_MJD) = mjd_day
               eop(EOP_X) = xp
               eop(EOP_Y) = yp
               eop(EOP_UT1) = UT1_UTC
               eop(EOP_LOD) = LOD
               eop(EOP_DX) = dX
               eop(EOP_DY) = dY
               eop(EOP_X_ERR) = xErr
               eop(EOP_Y_ERR) = yErr
               eop(EOP_UT1_ERR) = UT1_UTC_Err
               eop(EOP_LOD_ERR) = LOD_Err
               found = .TRUE.
               EXIT
            END IF
          END IF           
      END DO

      CLOSE (UNIT=UNIT_IN)
      if (.not. found) then
              ! fill up with some nonsense
              print *, "mjd_day ", mjd, " entry not found in ", TRIM(filename)
              eop = 0.d0
              eop(1) = mjd
      end if

END
