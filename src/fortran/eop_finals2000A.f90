SUBROUTINE eop_finals2000A (filename,mjd , eop)


! ----------------------------------------------------------------------
! Subroutine:  eop_finals2000A.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read data format of EOP (Earth Orientation Parameters) daily solution
!  (finals2000A.daily) provided by the International Earth Rotation 
!  Service and Reference Systems (IERS) Rapid Service/Prediction Center
! ----------------------------------------------------------------------
! Input arguments:
! - filename:		EOP data file name  e.g. 'finals2000A.daily'
! - mjd:			Modified Julian Day number of the required date (in UTC)
!
! Output arguments:
! - eop:			EOP data array (rank 7) for the input day
!   				eop = [MJD xp yp UT1_UTC LOD dX dY]
!   				MJD:     MJD referred to 0h in UTC time scale 
!   				x,y:     Polar motion coordinates (arcsec) 
!   				UT1_UTC: Difference between UT1 and UTC (sec)
!					dX,dY:   Corrections to Precession-Nutation model (arcsec)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia               March 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int8), INTENT(IN) :: mjd
      CHARACTER (LEN=512), INTENT(IN) :: filename
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: eop(7)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_line, ios_data
      INTEGER (KIND = prec_int8) :: i, read_i, len_line
      CHARACTER (LEN=170) :: line_ith, eop_line, Format_eop 
      CHARACTER (LEN=3) :: IPflag
      INTEGER (KIND = prec_int8) :: year,month,day, mjd_day
      REAL (KIND = prec_d) :: mjd_data, xp, yp, xp_er, yp_er
      REAL (KIND = prec_d) :: UT1_UTC, LOD, UT1_UTC_Err, LOD_Err, dX,dY, dX_Err,dY_Err
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
      UNIT_IN = 9
      !Format_eop = '(3(I2),X,F8.2,X,A1,X,2(F9.6),X,2(F9.6),2X,A1,2(F10.7),X,2(F7.4),2X,A1,X,2(F9.3),X,2(F9.3),2(F10.6),F11.7,2(F10.3))'
      Format_eop = '(3(I2),X,F8.2,X,A1,X,2(F9.6),X,2(F9.6),2X,A1,2(F10.7),X,2(F7.4),2X,A1,X,2(F9.3),X,2(F9.3))'
! ----------------------------------------------------------------------

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
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------
! Data
         READ (line_ith, Format_eop, IOSTAT=ios_data) year,month,day, mjd_data, & 
                                                      IPflag, xp,xp_er, yp,yp_er, &
                                                      IPflag, UT1_UTC,UT1_UTC_Err, LOD,LOD_Err, & 
                                                      IPflag, dX,dX_Err, dY,dY_Err

         mjd_day = INT(mjd_data)
         if (mjd_day == mjd) THEN
         !PRINT *,"xp,yp,UT1_UTC,LOD,dX,dY", xp,yp,UT1_UTC,LOD,dX,dY
         !PRINT *,"mjd_day,mjd_data", mjd_day,mjd_data
         !PRINT *,"year,month,day, mjd_data", year,month,day, mjd_data
         !PRINT *,"IPflag, xp,xp_er, yp,yp_er", IPflag, xp,xp_er, yp,yp_er
         !PRINT *,"IPflag, UT1_UTC,UT1_UTC_Err, LOD,LOD_Err", IPflag, UT1_UTC,UT1_UTC_Err, LOD,LOD_Err
         !PRINT *,"IPflag, dX,dX_Err, dY,dY_Err", IPflag, dX,dX_Err, dY,dY_Err
		    eop(1) = mjd_day
            eop(2) = xp
            eop(3) = yp
            eop(4) = UT1_UTC
            eop(5) = LOD
            eop(6) = dX
            eop(7) = dY
            EXIT
         end if
! ----------------------------------------------------------------------
      END DO

      CLOSE (UNIT=UNIT_IN)

END
