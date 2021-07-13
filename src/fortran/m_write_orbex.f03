MODULE m_write_orbex


! ----------------------------------------------------------------------
! MODULE: m_write_orbex.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for write the satellite attitude in ORBEX format 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	10 December 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE write_orbex (ATT_matrix, PRNmatrix, ORBEX_fname)

! ----------------------------------------------------------------------
! SUBROUTINE: write_orbex 
! ----------------------------------------------------------------------
! Purpose:
!  Write satellite attitude in ORBEX format 
! ----------------------------------------------------------------------
! Input arguments:
! - ATT_matrix:			    Orbits and attitude matrix
! - PRNmatrix:       		Satellites' PRN numbers allocatable array
! - ORBEX_fname:       		Output filename of attitude in orbex format 
!
! Output arguments:
!
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou at Geoscience Australia       10 December 2019
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE mdl_config
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(IN) :: PRNmatrix(:)
      CHARACTER (*), INTENT(IN) :: ORBEX_fname
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: ATT_matrix
! OUT

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, i_write
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_ith
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3
      INTEGER (KIND = prec_int2) :: wrt_opt
      INTEGER (KIND = prec_int2) :: FMT_opt
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: RealT
      INTEGER (KIND = prec_int2) :: RealW, RealD
      CHARACTER (LEN=70) :: fmt_wrt, fmt_wrt0, fmt_sz2
      REAL (KIND = prec_q) :: wrtArrayLN 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: vel
      CHARACTER (LEN=100) :: fmt_epoch, fmt_pos, fmt_vel, fmt_line

      REAL (KIND = prec_q) :: MJD_ti, Sec_00
      CHARACTER (LEN=3) :: PRN_ti
      REAL (KIND = prec_q) :: r_ti(3), v_ti(3), cl_ti, Dcl_ti

      DOUBLE PRECISION DJ1, DJ2
      INTEGER IY, IM, ID
      DOUBLE PRECISION FD
      INTEGER J

      !INTEGER (KIND = prec_int4) :: year, month, day, hour_ti, min_ti 
      INTEGER :: year, month, day, hour_ti, min_ti 
      REAL (KIND = prec_q) :: sec_ti 	  
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, Nelem, Nsat
      INTEGER (KIND = prec_int8) :: Nclk_epoch, Nclk_elem, Nclk_sat, iCLK_epoch
      INTEGER (KIND = prec_int8) :: i_sat
      INTEGER (KIND = prec_int2) :: clk_write
! ----------------------------------------------------------------------
      CHARACTER (LEN=3) :: TIMESYS
      CHARACTER (LEN=1) :: orbvector
      CHARACTER (LEN=5) :: REFRAME
      CHARACTER (LEN=3) :: ORBTYPE
      CHARACTER (LEN=4) :: Agency
      CHARACTER(LEN=11) :: PCV_sol
      CHARACTER (LEN=9) :: OceanTide_Load, AtmTide_Load
      CHARACTER (LEN=3) :: ORB_C, CLK_C
! ----------------------------------------------------------------------
      CHARACTER (LEN=3) :: PRN_write
      CHARACTER (LEN=300) :: wrt_line
      CHARACTER (LEN=6) :: wrt_line_0
      INTEGER (KIND = prec_int8) :: GPS_week, GPSweek_mod1024
      REAL (KIND = prec_d) :: GPS_wsec, GPS_day
      REAL (KIND = prec_d) :: Interval
      INTEGER (KIND = prec_int8) :: Nsat_17frac, Nsat_lines, j17
      CHARACTER (LEN=1) :: char1
      INTEGER (KIND = prec_int2) :: num1
! ----------------------------------------------------------------------
      CHARACTER (LEN=4) :: ORBEX_version
      CHARACTER (LEN=5) :: COORD_SYSTEM
      CHARACTER (LEN=4) :: FRAME_TYPE
      REAL (KIND = prec_d), DIMENSION(4) :: quaternions_ti
    CHARACTER(LEN=8)  :: date_mach
    CHARACTER(LEN=10) :: time_mach
    CHARACTER(LEN=5)  :: zone_mach
    INTEGER :: time_values(8)
! ----------------------------------------------------------------------


UNIT_IN = 7  												

ORBEX_version = '0.09' 

! ----------------------------------------------------------------------
! Features to be written in the orbex header
! ----------------------------------------------------------------------
! Time System
TIMESYS = 'GPS'
! Orbit Reference Frame
!COORD_SYSTEM='IGS14'
COORD_SYSTEM='ITRF '
FRAME_TYPE = 'ECEF'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Format definition
! ----------------------------------------------------------------------
!*  2010 12 25  0  0  0.0000 
!PG01  -6582.270015  18452.592641 -17946.851511    -67.214659  6  9  8  87       
!fmt_epoch = '(A3,I5.4,4I3.2,F11.8)'
fmt_epoch = '(A3,I4,A1, I2,A1, I2,A1, I2,A1 I2,A1 , F11.8)'
!fmt_pos = '(A1,A1,I2.2,4F14.6,A)'
!fmt_vel = '(A1,A1,I2.2,4F14.6,A)'
fmt_pos = '(A1,A3,4F14.6)'
fmt_vel = '(A1,A3,4F14.6)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Orbit array dimensions
sz1 = SIZE (ATT_matrix,DIM=1)
sz2 = SIZE (ATT_matrix,DIM=2)
sz3 = SIZE (ATT_matrix,DIM=3)
Nepochs = sz1
Nelem   = sz2
Nsat    = sz3
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT=UNIT_IN,FILE=ORBEX_fname,ACTION="WRITE",POSITION="REWIND", IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", ORBEX_fname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Write Header of the sp3 file
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Initial Epoch 
! ----------------------------------------------------------------------
! MJD of epoch (including fraction of the day)
MJD_ti = ATT_matrix(1,1,1)
! Sec of Day (since 0h)
Sec_00 = ATT_matrix(1,2,1)

! MJD of Epoch (including fraction of the day)
DJ1 = 2400000.5D0
DJ2 = MJD_ti
Call iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )

year  = IY
month = IM
day   = ID

if (1<0) then
hour_ti = INT(FD * 24.0D0) 
min_ti  = INT(FD * 24.0D0 * 60.0D0)
sec_ti  = (FD * 24.0D0 * 60.0D0 * 60.0D0)
else
hour_ti = INT(Sec_00 / (60.0D0 * 60.0D0))
min_ti  = INT(Sec_00/60.0D0 - hour_ti*60.0D0)  
sec_ti  = (Sec_00 - hour_ti*3600.0D0 - min_ti*60.D0)
end if

! GPS Week number and seconds
!CALL time_GPSweek (MJD_ti , GPS_week, GPS_wsec, GPSweek_mod1024)
!CALL time_GPSweek2(MJD_ti , GPS_week, GPS_wsec, GPSweek_mod1024, GPS_day)
CALL time_GPSweek3(MJD_ti, Sec_00, GPS_week, GPS_wsec, GPSweek_mod1024, GPS_day)

! ----------------------------------------------------------------------
! Epochs Interval
Interval = ATT_matrix(2,2,1) - ATT_matrix(1,2,1)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Header Lines
! ----------------------------------------------------------------------
WRITE (UNIT=UNIT_IN,FMT='(A7,2x,A4)',IOSTAT=ios_ith) '%=ORBEX', ORBEX_version  
WRITE (UNIT=UNIT_IN,FMT='(A2)',IOSTAT=ios_ith) '%%'
WRITE (UNIT=UNIT_IN,FMT='(A20)',IOSTAT=ios_ith) '+FILE/DESCRIPTION   '
WRITE (UNIT=UNIT_IN,FMT='(A20,A20)',IOSTAT=ios_ith) ' DESCRIPTION        ', 'Attitude quaternions'
WRITE (UNIT=UNIT_IN,FMT='(A20,A10)',IOSTAT=ios_ith) ' CREATED_BY         ', 'GA ACS/POD'

CALL date_and_time(date_mach,time_mach,zone_mach,time_values)
WRITE (UNIT=UNIT_IN,FMT= '(A,I4,1x,I2,1x,I2,1x, I2,1x,I2,1x,I2,1x)' ,IOSTAT=ios_ith) ' CREATION_DATE      ', &
	   & time_values(1),time_values(2),time_values(3), time_values(5),time_values(6),time_values(7)

WRITE (UNIT=UNIT_IN,FMT='(A20,A6)',IOSTAT=ios_ith) ' INPUT_DATA         ','Orbits'  
WRITE (UNIT=UNIT_IN,FMT='(A20,A3)',IOSTAT=ios_ith) ' TIME_SYSTEM        ', TIMESYS 

WRITE (UNIT=UNIT_IN,FMT='(1x,A10,10x, I4,1x,I2,1x,I2,1x, I2,1x,I2,1x, F15.12)',IOSTAT=ios_ith) 'START_TIME', & 
		& year, month, day, hour_ti, min_ti, sec_ti 
		
! ----------------------------------------------------------------------
! Last epoch
! ----------------------------------------------------------------------
! MJD of epoch (including fraction of the day)
MJD_ti = ATT_matrix(Nepochs,1,1)
! Sec of Day (since 0h)
Sec_00 = ATT_matrix(Nepochs,2,1)
! MJD of Epoch (including fraction of the day)
DJ1 = 2400000.5D0
DJ2 = MJD_ti
Call iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )
year  = IY
month = IM
day   = ID
hour_ti = INT(Sec_00 / (60.0D0 * 60.0D0))
min_ti  = INT(Sec_00/60.0D0 - hour_ti*60.0D0)  
sec_ti  = (Sec_00 - hour_ti*3600.0D0 - min_ti*60.D0)

WRITE (UNIT=UNIT_IN,FMT='(1x,A8,12x, I4,1x,I2,1x,I2,1x, I2,1x,I2,1x, F15.12)',IOSTAT=ios_ith) 'END_TIME', & 
		& year, month, day, hour_ti, min_ti, sec_ti 
! ----------------------------------------------------------------------

WRITE (UNIT=UNIT_IN,FMT='(A20,F10.3)',IOSTAT=ios_ith) ' EPOCH_INTERVAL     ', Interval
WRITE (UNIT=UNIT_IN,FMT='(A20,A5)'  ,IOSTAT=ios_ith) ' COORD_SYSTEM       ', COORD_SYSTEM 
WRITE (UNIT=UNIT_IN,FMT='(A20,A4)'  ,IOSTAT=ios_ith) ' FRAME_TYPE         ', FRAME_TYPE 
WRITE (UNIT=UNIT_IN,FMT='(A20,A3)'  ,IOSTAT=ios_ith) ' LIST_OF_REC_TYPES  ', 'ATT' 
WRITE (UNIT=UNIT_IN,FMT='(A20)',IOSTAT=ios_ith) '-FILE/DESCRIPTION   '

WRITE (UNIT=UNIT_IN,FMT='(A29)',IOSTAT=ios_ith) '+SATELLITE/ID_AND_DESCRIPTION'
DO i_sat = 1 , Nsat
	PRN_write = PRNmatrix(i_sat)
	WRITE (UNIT=UNIT_IN,FMT='(1X,A3)',IOSTAT=ios_ith) PRN_write 
END DO
WRITE (UNIT=UNIT_IN,FMT='(A29)',IOSTAT=ios_ith) '-SATELLITE/ID_AND_DESCRIPTION'

WRITE (UNIT=UNIT_IN,FMT='(A15)',IOSTAT=ios_ith) '+EPHEMERIS/DATA'
WRITE (UNIT=UNIT_IN,FMT='(A103)',IOSTAT=ios_ith) &
& '*ATT RECORDS: TRANSFORMATION FROM TERRESTRIAL FRAME COORDINATES (T) TO SAT. BODY FRAME ONES (B) SUCH AS' 
WRITE (UNIT=UNIT_IN,FMT='(A59)',IOSTAT=ios_ith) &
& '*                                 (0,B) = q.(0,T).trans(q) '
WRITE (UNIT=UNIT_IN,FMT='(A90)',IOSTAT=ios_ith) &
& '*REC ID_ N ___q0_(scalar)_____ ____q1__x__________ ____q2__y__________ ____q3__z__________'



! ----------------------------------------------------------------------
! Write Attityde matrix to the ORBEX file	  
! ----------------------------------------------------------------------
! Epochs loop
DO i_write = 1 , Nepochs       


! Satellites loop
DO i_sat = 1 , Nsat       


! MJD of epoch (including fraction of the day)
MJD_ti = ATT_matrix(i_write,1, i_sat)
! Sec of Day (since 0h)
Sec_00 = ATT_matrix(i_write,2, i_sat)

! Satellite PRN number 
PRN_ti = PRNmatrix(i_sat)

! ----------------------------------------------------------------------
! Attitude in Quaternions per epoch
quaternions_ti(1) = ATT_matrix(i_write,15, i_sat) 
quaternions_ti(2) = ATT_matrix(i_write,16, i_sat)  
quaternions_ti(3) = ATT_matrix(i_write,17, i_sat) 
quaternions_ti(4) = ATT_matrix(i_write,18, i_sat)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
IF (i_sat == 1) THEN
! Write the Epoch line

! MJD of Epoch (including fraction of the day)
DJ1 = 2400000.5D0
DJ2 = MJD_ti
Call iau_JD2CAL ( DJ1, DJ2, IY, IM, ID, FD, J )

year  = IY
month = IM
day   = ID

if (1<0) then
hour_ti = INT(FD * 24.0D0) 
min_ti  = INT(FD * 24.0D0 * 60.0D0)
sec_ti  = (FD * 24.0D0 * 60.0D0 * 60.0D0)
else
hour_ti = INT(Sec_00 / (60.0D0 * 60.0D0))
min_ti  = INT(Sec_00/60.0D0 - hour_ti*60.0D0)  
sec_ti  = (Sec_00 - hour_ti*3600.0D0 - min_ti*60.D0)
end if

! Epoch line
!## 2018 10 21 00 00 0.000000000000 09
WRITE (UNIT=UNIT_IN,FMT='(A2,1x,I4,1x,I2,1x,I2,1x,I2,1x,I2,1x,F15.12,2x,I3) & 
		& ',IOSTAT=ios_ith) '##', year,month,day, hour_ti,min_ti,sec_ti, Nsat

END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Write Satellite Attitude per PRN 	  
! ----------------------------------------------------------------------
! ## 2018 10 21 00 00 0.000000000000 09
! ATT E01              4 0.2796988739859625 0.0767732228075297 0.9535493300680007 -0.0813516273813716
WRITE (UNIT=UNIT_IN,FMT='(1x,A3,1x,A3,14x,I1, 4F20.16)',IOSTAT=ios_ith) 'ATT',PRN_ti, 4, quaternions_ti

IF (ios_ith /= 0) THEN
    PRINT *, "Error in writing to file: ", TRIM (ORBEX_fname)
    PRINT *, "WRITE IOSTAT=", ios_ith
END IF
! ----------------------------------------------------------------------

 END DO
 END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Last Lines 
WRITE (UNIT=UNIT_IN,FMT='(A15)',IOSTAT=ios_ith) '-EPHEMERIS/DATA'
WRITE (UNIT=UNIT_IN,FMT='(A10)',IOSTAT=ios_ith) '%END_ORBEX'
! ----------------------------------------------------------------------

ENDFILE (UNIT = UNIT_IN) 
CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------


END SUBROUTINE


End Module
