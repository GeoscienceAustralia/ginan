MODULE m_write_orb2sp3


! ----------------------------------------------------------------------
! MODULE: m_write_orb2sp3.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for write the POD *.orb format to sp3 orbit format 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia
! Created:	21 February 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE write_orb2sp3 (ORBmatrix, PRNmatrix, sp3_fname, sat_vel, CLKmatrix)

! ----------------------------------------------------------------------
! SUBROUTINE: writesp3_hd 
! ----------------------------------------------------------------------
! Purpose:
!  Write orbit sp3 format header 
! ----------------------------------------------------------------------
! Input arguments:
! - ORBmatrix:       Orbits dynamic allocatable array (3-dimensional)
! - PRNmatrix:       Satellites' PRN numbers allocatable array
!
! Output arguments:
!
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia         21 February 2019
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_config
      use pod_yaml
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: ORBmatrix, CLKmatrix 
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(IN) :: PRNmatrix(:)
      CHARACTER (*), INTENT(IN) :: sp3_fname
      logical, INTENT(IN) :: sat_vel	  
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
      logical :: vel
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
      CHARACTER (LEN=1) :: orbvector
      CHARACTER (LEN=5) :: REFRAME
      CHARACTER (LEN=3) :: TIMESYS
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

      external time_GPSweek3

UNIT_IN = 7  												


! ----------------------------------------------------------------------
! Orbit Features to be written in the sp3 orbit header
! ----------------------------------------------------------------------
! Orbit position or velocity vector Flag
If (.not. sat_vel) Then
	orbvector = 'P'
Else
	orbvector = 'V'
End IF 	

! Orbit Reference Frame
!REFRAME = 'IGS08'
REFRAME = 'ITRF '
! Time System
TIMESYS = 'GPS'

! Orbit Type
IF (yml_pod_mode == MODE_FIT .or. yml_pod_mode == MODE_PREDICT) THEN
	ORBTYPE = 'FIT'
ELSE IF (yml_pod_mode == MODE_EQM_INT .or. yml_pod_mode == MODE_IC_INT) THEN
	ORBTYPE = 'INT'
END IF

Agency  = '  GA'

PCV_sol =        'N/A        '
OceanTide_Load = 'FES2004  '
AtmTide_Load =   'NONE     '
ORB_C = 'CoM'
CLK_C = 'N/A'
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
sz1 = SIZE (ORBmatrix,DIM=1)
sz2 = SIZE (ORBmatrix,DIM=2)
sz3 = SIZE (ORBmatrix,DIM=3)
Nepochs = sz1
Nelem   = sz2
Nsat    = sz3
! ----------------------------------------------------------------------
! Clock array dimensions
sz1 = SIZE (CLKmatrix,DIM=1)
sz2 = SIZE (CLKmatrix,DIM=2)
sz3 = SIZE (CLKmatrix,DIM=3)
Nclk_epoch = sz1
Nclk_elem  = sz2
Nclk_sat   = sz3
IF (Nclk_epoch > 1) THEN
	clk_write = 1
ELSE 
	clk_write = 0
END IF
! ----------------------------------------------------------------------
!print *,"CLKmatrix dims", Nclk_epoch, Nclk_elem, Nclk_sat
!print *,"clk_write",clk_write

! ----------------------------------------------------------------------
! Writing satellite velocity vector (optional)
! ----------------------------------------------------------------------
vel = sat_vel
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT=UNIT_IN,FILE=sp3_fname,ACTION="WRITE",POSITION="REWIND", IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", sp3_fname
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
MJD_ti = ORBmatrix(1,1,1)
! Sec of Day (since 0h)
Sec_00 = ORBmatrix(1,2,1)

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
Interval = ORBmatrix(2,2,1) - ORBmatrix(1,2,1)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Line 1
! ----------------------------------------------------------------------
!fmt_line = '(A2,A1,I4,A1, I2.2,A1, I2.2,A1, I2.2,A1, I2.2,A1, F11.8)'
!WRITE (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_ith) '#d', orbvector, year,' ', month,' ',day,' ', hour_ti,' ', min_ti,' ', sec_ti
!fmt_line = '(A2, I5.4, 4I3.2, F11.8,A1, I7,A1, A5,A1, A5,A1, A4)'
fmt_line = '(A2, A1, I4,A1, I2,A1,I2,A1,I2,A1,I2,A1, F11.8,A1, I7,A1, A5,A1, A5,A1, A3,A1, A4)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '#d', orbvector, year,' ',   & 
       month,' ', day,' ', hour_ti,' ', min_ti,' ', sec_ti,' ', Nepochs,' ', 'ORBIT',' ', REFRAME,' ', ORBTYPE,' ', Agency 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Line 2 :: GPS Week and Seconds, Modified Julian Day Number and fraction of the day
! ----------------------------------------------------------------------
fmt_line = '(A2,A1, I4,A1, F15.8,A1, F14.8,A1, I5,A1, F15.13)'
!WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '##',' ', GPS_week,' ',  GPS_wsec,' ', INT(MJD_ti),' ', (MJD_ti-INT(MJD_ti))    
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '##',' ', &
       GPS_week,' ',  GPS_wsec,' ', Interval,' ', INT(MJD_ti),' ', (MJD_ti-INT(MJD_ti))    
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Lines 3 to 7 (or more if Satellites>85) :: Satellites number and PRNs
! ----------------------------------------------------------------------
Nsat_17frac = INT(Nsat / 17)
If (Nsat > (Nsat_17frac * 17) )  Nsat_17frac = Nsat_17frac + 1

! Nsat_lines: 5 for satellites <=85
If (Nsat_17frac > 5) Then 
	Nsat_lines = Nsat_17frac
Else 
	Nsat_lines = 5
End IF


Do i = 1 , Nsat_lines

WRITE(wrt_line  ,FMT=*,IOSTAT=ios_ith) ''

Do j17 = 1 , 17
	i_sat = (i-1)*17 + j17
	IF (i_sat <= Nsat) THEN	
		PRN_write = PRNmatrix(i_sat)
		!WRITE(wrt_line,FMT=*,IOSTAT=ios_ith) ADJUSTL(TRIM(wrt_line)),PRN_write
	ELSE IF (i_sat > Nsat) THEN
		PRN_write = '  0'
		!WRITE(wrt_line,FMT=*,IOSTAT=ios_ith) ADJUSTL(TRIM(wrt_line)),'  0'
	END IF
	
	If (j17 == 1) Then
		IF (PRN_write == '  0') THEN
			num1 = 0
		Else
			num1 = 1
		END IF
	End IF
	!PRINT *,"num1", num1

	WRITE(wrt_line,FMT=*,IOSTAT=ios_ith) (TRIM(wrt_line)), ADJUSTR(PRN_write)
END DO

! Write Line to sp3 file
if (i==1) then
	fmt_line = '(A2,A1,I3,A3,A)'
	WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '+ ',' ', Nsat,'   ', ADJUSTL(TRIM(wrt_line)) 
ELSE
	!fmt_line = '(A9,A)'
	!WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '+        ', ADJUSTL(TRIM(wrt_line)) 	
	IF (num1 == 0) THEN
		fmt_line = '(A11,A)'
		WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '+          ', ADJUSTL(TRIM(wrt_line)) 	
	Else
		fmt_line = '(A9,A)'
		WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '+        ', ADJUSTL(TRIM(wrt_line)) 		
	END IF
END IF
!print *,"wrt_line final", wrt_line	
End Do
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lines 8-12 (SAT<=85) :: Orbit accuracy (5 or more lines for satellites>85)  
! ----------------------------------------------------------------------
Do i = 1 , Nsat_lines
	WRITE(wrt_line,FMT=*,IOSTAT=ios_ith) '++', '       '
	DO j17 = 1 , 17
		WRITE(wrt_line,FMT=*,IOSTAT=ios_ith) wrt_line, '  0'		
	END DO

! Write Line to sp3 file
!WRITE (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_ith) TRIM(wrt_line) 

fmt_line = '(A60)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) &
       ADJUSTL(TRIM('++         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0'))   
       !'++         0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0'   
	   
END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lines 13-14 (SAT<=85) :: Time System
! ----------------------------------------------------------------------
fmt_line = '(A3,A3,A3,A3,A48)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '%c ','M  ','cc ', TIMESYS,' ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc' 
fmt_line = '(A60)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '%c cc cc ccc ccc cccc cccc cccc cccc ccccc ccccc ccccc ccccc' 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lines 15-16 (SAT<=85) :: Base for Pos/Vel and Base for Clk/Rate  
! ----------------------------------------------------------------------
fmt_line = '(A60)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '%f  1.2500000  1.025000000  0.00000000000  0.000000000000000' 
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '%f  0.0000000  0.000000000  0.00000000000  0.000000000000000' 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lines 17-18 (SAT<=85) :: 
! ----------------------------------------------------------------------
fmt_line = '(A60)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '%i    0    0    0    0      0      0      0      0         0' 
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '%i    0    0    0    0      0      0      0      0         0' 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lines 19-22 (SAT<=85) :: comment lines
! ----------------------------------------------------------------------
fmt_line = '(A60)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '/* GEOSCIENCE AUSTRALIA                                     ' 
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '/* Precise Orbit Determination (POD) Tool                   ' 
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) '/*                                                          ' 

fmt_line = '(A7,A11,A6,A9,A9,A3,A4,A3,A5,A3)'
WRITE (UNIT=UNIT_IN,FMT=fmt_line,IOSTAT=ios_ith) & 
       '/* PCV:', PCV_sol, 'OL/AL:', OceanTide_Load, AtmTide_Load, 'Y  ','ORB:', ORB_C, ' CLK:', CLK_C 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Write Orbits matrix to the sp3 file	  
! ----------------------------------------------------------------------
! Epochs loop
DO i_write = 1 , Nepochs       


! Satellites loop
DO i_sat = 1 , Nsat       


! MJD of epoch (including fraction of the day)
MJD_ti = ORBmatrix(i_write,1, i_sat)
! Sec of Day (since 0h)
Sec_00 = ORBmatrix(i_write,2, i_sat)

! Satellite PRN number 
!PRN_ti = sp3_fname(1:3)
!PRN_ti = sat_prn
PRN_ti = PRNmatrix(i_sat)

! Position vector
r_ti(1) = ORBmatrix(i_write,3, i_sat) !/ 1.D03
r_ti(2) = ORBmatrix(i_write,4, i_sat) !/ 1.D03 
r_ti(3) = ORBmatrix(i_write,5, i_sat) !/ 1.D03
! Unit conversion: m to Km                 
r_ti = r_ti * 1.0D-3

! Velocity vector
if (vel) then
! Velocity vector in dm/sec
v_ti(1) = ORBmatrix(i_write,6, i_sat) 
v_ti(2) = ORBmatrix(i_write,7, i_sat) 
v_ti(3) = ORBmatrix(i_write,8, i_sat) 
! Unit conversion: m/sec to dm/sec                 
v_ti = v_ti * 1.0D1
end if

! Clock
cl_ti  = 999999.999999D0
Dcl_ti = 999999.999999D0
IF (clk_write == 1) THEN 
DO iCLK_epoch = 1 , Nclk_epoch
  IF ( (ABS(CLKmatrix(iCLK_epoch,1, i_sat)) - ABS(MJD_ti) < 1.0D-3) .AND. &
     & (CLKmatrix(iCLK_epoch,2, i_sat) - Sec_00 < 1.0D-12) )THEN
	! Clock record
	cl_ti = CLKmatrix(iCLK_epoch,3, i_sat)
	! Clock correlation (sdev)
	IF (Nclk_elem>3) Dcl_ti = CLKmatrix(iCLK_epoch,4, i_sat)  
	!EXIT  
  END IF
END DO
END IF

!! Clock correlation (sdev)
!if (vel>0) then
!IF (clk_write == 0) THEN 
!Dcl_ti = 999999.999999D0
!ELSE 
!Dcl_ti = CLKmatrix(i_write,4, i_sat) 
!END IF
!end if 

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

!print *,"sec_ti print", sec_ti 
!WRITE (*,FMT='(A6,F17.6)'),"sec_ti", sec_ti
!WRITE (*,FMT='(A6,F17.6)'),"sec_ti", sec_ti

! Epoch line
!*  2010 12 25  0  0  0.0000 
!READ (line_ith, * , IOSTAT=ios_data) char3, year, month, day, hr, minute, sec  
!WRITE (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_ith) '*  ', year,' ', month,' ',day,' ',hour_ti,' ',min_ti,' ',sec_ti,' '
!WRITE (UNIT=UNIT_IN,FMT=fmt_epoch,IOSTAT=ios_ith) '*  ', year, month, day, hour_ti, min_ti, sec_ti
WRITE (UNIT=UNIT_IN,FMT=fmt_epoch,IOSTAT=ios_ith) '*  ', year,' ', month,' ', day,' ', hour_ti,' ', min_ti,' ', sec_ti

END IF
! ----------------------------------------------------------------------


if (1>0) then
! ----------------------------------------------------------------------
! Write Satellite Position and Velocity Vector per PRN 	  
! ----------------------------------------------------------------------
! Position per epoch	  
WRITE (UNIT=UNIT_IN,FMT=fmt_pos,IOSTAT=ios_ith) 'P',PRN_ti, r_ti, cl_ti

! Velocity per epoch	  
if (vel) then
WRITE (UNIT=UNIT_IN,FMT=fmt_vel,IOSTAT=ios_ith) 'V',PRN_ti, v_ti, Dcl_ti
end if
! ----------------------------------------------------------------------
end if


IF (ios_ith /= 0) THEN
    PRINT *, "Error in writing to file: ", TRIM (sp3_fname)
    PRINT *, "WRITE IOSTAT=", ios_ith
END IF
! ----------------------------------------------------------------------

 
 END DO
 END DO
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Last Line :: EOF
WRITE (UNIT=UNIT_IN,FMT='(A3)',IOSTAT=ios_ith) 'EOF'
! ----------------------------------------------------------------------

ENDFILE (UNIT = UNIT_IN) 
CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------


END SUBROUTINE


End Module
