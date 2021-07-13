MODULE m_write_brd2sp3

! ----------------------------------------------------------------------
! MODULE: m_write_brd2sp3.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for write the POD *.orb format to sp3 orbit format 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Geoscience Australia
!               Dr. Tzupang Tseng
!
! Created:	21 February 2019
! ----------------------------------------------------------------------

      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE write_brd2sp3 (ISTR,IPRN,IGNSS,IG,IR,IE,IC,IJ,SATTYPE,SAMPLE,IY,IM,ID,NEWEPOCH,ORBmatrix,sp3_fname,sat_vel)

! ----------------------------------------------------------------------
! SUBROUTINE: write_brd2sp3 
! ----------------------------------------------------------------------
! Purpose:
!  Write orbit sp3 format header 
! ----------------------------------------------------------------------
! Input arguments:
! - ISTR     :       Array containing the processed satellites only used for
!                    header information
! - IPRN     :       Array containing the processed satellites only used for
!                    orbital information
! - IGNSS    :       Number of satellites in a specific constellation
! - IG,IR,IE,IC,IJ:  Number of each satellite constellation (G:GPS, R:GLONASS,
!                                                            E:GALILEO, C:BDS, J:QZSS)
! - SATTYPE  :       The satellite constellation selected for processing
! - SAMPLE   :       Sampling rate in SP3 format
! - IY, IM, ID :     Time line
! - NEWEPOCH :       Number of epochs with satellite positions
! - ORBmatrix:       Orbits dynamic allocatable array (3-dimensional)
! - sp3_fname:       Output filename of SP3 file
! - sat_vel  :       Option for outputing satellite velocity
!
! Output arguments:
!
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia         21 February 2019
!
!Changes: 08-07-2019 Tzupang Tseng: modify this routine that can be used for the
!                                   SP3 file creation from the broadcast orbits
!         12-07-2019 Tzupang Tseng: include multi-GNSS in SP3 file creation,
!                                   except for GLONASS
!         23-07-2019 Tzupang Tseng: modify the header information of PRN number
!                                   consistent with the PRN number in the content
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), DIMENSION(:,:,:),ALLOCATABLE,INTENT(IN) :: ORBmatrix 
      REAL (KIND = prec_d), DIMENSION(:,:),ALLOCATABLE,INTENT(IN) ::NEWEPOCH
      CHARACTER (LEN=3),   INTENT(IN) :: SATTYPE
      CHARACTER (LEN=100), INTENT(IN) :: sp3_fname
      INTEGER (KIND = 4), INTENT(IN) :: sat_vel
      INTEGER (KIND = prec_int4),INTENT(IN) ::IY, IM, ID
      INTEGER (KIND = 4), INTENT(IN) :: SAMPLE
      INTEGER (KIND = 4), INTENT(IN) :: IGNSS,IG,IR,IE,IC,IJ
      INTEGER (KIND = prec_d) :: IPRN(220),ISTR(220)
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
      INTEGER (KIND = prec_int2) :: RealW, RealD
      INTEGER (KIND = prec_int2) :: vel
      CHARACTER (LEN=1) :: RealT
      CHARACTER (LEN=70) :: fmt_wrt, fmt_wrt0, fmt_sz2
      CHARACTER (LEN=100) :: fmt_epoch, fmt_pos, fmt_vel, fmt_line
      CHARACTER (LEN=3) :: PRN_ti
      REAL (KIND = prec_q) :: wrtArrayLN 
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: MJD_ti, Sec_00, MJD0
      REAL (KIND = prec_q) :: r_ti(3), v_ti(3), cl_ti, Dcl_ti

      DOUBLE PRECISION DJ1, DJ2
      DOUBLE PRECISION FD
      INTEGER J

 
      INTEGER :: year, month, day, hour_ti, min_ti 
      REAL (KIND = prec_q) :: sec_ti 	  
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, Nelem, Nsat
      INTEGER (KIND = prec_int8) :: i_sat, i_sat1, II
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
      INTEGER (KIND = prec_int8) ::GPS_week, GPSweek_mod1024
      REAL (KIND = prec_d) :: GPS_wsec
      REAL (KIND = prec_d) :: Interval
      INTEGER (KIND = prec_int8) :: Nsat_17frac, Nsat_lines, j17
      CHARACTER (LEN=1) :: char1
      INTEGER (KIND = prec_int2) :: num1
      INTEGER (KIND = prec_int8) :: ICON
! ----------------------------------------------------------------------


UNIT_IN = 7  												


! ----------------------------------------------------------------------
! Orbit Features to be written in the sp3 orbit header
! ----------------------------------------------------------------------
! Orbit position or velocity vector Flag
If (sat_vel == 0) Then
	orbvector = 'P'
Elseif (sat_vel == 1) Then
	orbvector = 'V'
End IF 	

! Orbit Reference Frame
!REFRAME = 'IGS08'
REFRAME = 'ITRF '
! Time System
TIMESYS = 'GPS'
! Orbit Type
ORBTYPE = 'BRD'
ORBTYPE = 'INT'

Agency  = '  GA'

PCV_sol =        'N/A        '
OceanTide_Load = 'FES2004  '
AtmTide_Load =   'NONE     '
ORB_C = 'CoN'
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
!sz1 = SIZE (ORBmatrix,DIM=1)
sz1 = SIZE(NEWEPOCH,DIM =1)
sz2 = SIZE (ORBmatrix,DIM=2)
sz3 = SIZE (ORBmatrix,DIM=3)
Nepochs = sz1
Nelem   = sz2
Nsat = IGNSS
IF (SATTYPE == 'G') Nsat = IG
IF (SATTYPE == 'R') Nsat = IR
IF (SATTYPE == 'E') Nsat = IE
IF (SATTYPE == 'C') Nsat = IC
IF (SATTYPE == 'J') Nsat = IJ
IF (SATTYPE == 'A') Nsat = IGNSS


!PRINT*,'sz3 =', sz3, 'Nepochs =', Nepochs
! ----------------------------------------------------------------------
! Writing satellite velocity vector (optional)
! ----------------------------------------------------------------------
vel = sat_vel

! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT=UNIT_IN,FILE=sp3_fname,ACTION="WRITE",POSITION="REWIND", IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", sp3_fname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
 
! ----------------------------------------------------------------------
! Write Header of the sp3 file
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Initial Epoch 
! ----------------------------------------------------------------------
! MJD of epoch (including fraction of the day)
! Sec of Day (since 0h)
Sec_00 = MOD(NEWEPOCH(1,1),86400.d0)*86400

year  = IY
month = IM
day   = ID

CALL iau_CAL2JD ( year, month, day, MJD0, MJD_ti, J )

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
CALL time_GPSweek (MJD_ti , GPS_week, GPS_wsec, GPSweek_mod1024)

! ----------------------------------------------------------------------
! Epochs Interval
Interval = SAMPLE

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

! stop compiler warning
i_sat1 = 0

Do i = 1 , Nsat_lines

WRITE(wrt_line  ,FMT=*,IOSTAT=ios_ith) ''

Do j17 = 1 , 17
	i_sat = (i-1)*17 + j17
	IF (i_sat <= Nsat) THEN

! Process the selected GNSS constellation type
       IF (SATTYPE == 'G') THEN
       i_sat1 = i_sat
       ELSE IF (SATTYPE == 'R') THEN
       i_sat1 = i_sat + 50
       ELSE IF (SATTYPE == 'E') THEN 
       i_sat1 = i_sat + 100
       ELSE IF (SATTYPE == 'C') THEN
       i_sat1 = i_sat + 150
       ELSE IF (SATTYPE == 'J') THEN
       i_sat1 = i_sat + 200
       ELSE IF (SATTYPE == 'A') THEN
       IF(i_sat <= IG) i_sat1 = i_sat ! GPS
       IF(i_sat <= IG+IR .AND. i_sat > IG) i_sat1 = i_sat-(IG) + 50 ! GLONASS
       IF(i_sat <= IG+IR+IE .AND. i_sat > IG+IR) i_sat1 = i_sat-(IG+IR) + 100 ! GALILEO
       IF(i_sat <= IG+IR+IE+IC .AND. i_sat > IG+IR+IE) i_sat1 = i_sat-(IG+IR+IE) + 150 ! BDS
       IF(i_sat <= IG+IR+IE+IC+IJ .AND. i_sat > IG+IR+IE+IC) i_sat1 = i_sat-(IG+IR+IE+IC) + 200 ! QZSS
       END IF
	
       CALL prn2str (ISTR(i_sat1), PRN_write)
!       print*,'i_sat=, PRN_write=',IPRN(i_sat1),ISTR(i_sat1),i_sat1, PRN_write
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
min_ti  = 0
hour_ti = 0

! Process the selected GNSS constellation type
IF (SATTYPE == 'G') ICON = 1
IF (SATTYPE == 'R') ICON = 51
IF (SATTYPE == 'E') ICON = 101
IF (SATTYPE == 'C') ICON = 151
IF (SATTYPE == 'J') ICON = 201
IF (SATTYPE == 'A') ICON = 1


! ----------------------------------------------------------------------
! Write Orbits matrix to the sp3 file	  
! ----------------------------------------------------------------------
! Epochs loop
DO i_write = 1 , Nepochs       

!Sec_00 = 900*(i_write-1)

!year  = IY
!month = IM
!day   = ID
!print*, 'year, month, day, Sec_00 =', year, month, day, Sec_00
!if (1<0) then
!hour_ti = INT(FD * 24.0D0)
!min_ti  = INT(FD * 24.0D0 * 60.0D0)
!sec_ti  = (FD * 24.0D0 * 60.0D0 * 60.0D0)
!else
!hour_ti = INT(Sec_00 / (60.0D0 * 60.0D0))
!min_ti  = INT(Sec_00/60.0D0 - hour_ti*60.0D0)
!sec_ti  = (Sec_00 - hour_ti*3600.0D0 - min_ti*60.D0)
!end if
!print*,'time line=',year, month, day, Sec_00, hour_ti, min_ti,sec_ti
!print *,"sec_ti print", sec_ti
!WRITE (*,FMT='(A6,F17.6)'),"sec_ti", sec_ti
!WRITE (*,FMT='(A6,F17.6)'),"sec_ti", sec_ti

! Epoch line
!*  2010 12 25  0  0  0.0000
!READ (line_ith, * , IOSTAT=ios_data) char3, year, month, day, hr, minute, sec
!WRITE (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_ith) '*  ', year,' ', month,' ',day,'
!',hour_ti,' ',min_ti,' ',sec_ti,' '
!WRITE (UNIT=UNIT_IN,FMT=fmt_epoch,IOSTAT=ios_ith) '*  ', year, month, day,
!hour_ti, min_ti, sec_ti
!WRITE (UNIT=UNIT_IN,FMT=fmt_epoch,IOSTAT=ios_ith) '*  ', year,' ', month,' ',day,' ', hour_ti,' ', min_ti,' ', sec_ti

! Satellites loop
!DO i_sat = 1 , Nsat       
DO i_sat = 1 , sz3

IF(i_sat == IPRN(i_sat))THEN                  ! GNSS BRDC          

!Sec_00 = NEWEPOCH(i_write,i_sat) -  NEWEPOCH(1,i_sat)
Sec_00 = 900*(i_write-1)
!IF (ORBmatrix(i_write,1, i_sat) /= 0.d0 )THEN
!print*,'CURRENT EPOCH (GPSWEEK) =', NEWEPOCH(i_write,i_sat), 'REFERENCE EPOCH =', NEWEPOCH(1,i_sat),&
!       'ISAT = ', i_sat,'IPRN(ISAT)=',IPRN(i_sat), 'EXPECTED EPOCH DIFF =', 900*(i_write-1), &
!       'REAL SEC_00=',SEC_00, 'POS =',ORBmatrix(i_write,1, i_sat)
!IF (Sec_00 .LT. 0.d0) EXIT

! Satellite PRN number 
CALL prn2str (i_sat, PRN_ti) 


! Position vector

r_ti(1) = ORBmatrix(i_write,1, i_sat) !/ 1.D03
r_ti(2) = ORBmatrix(i_write,2, i_sat) !/ 1.D03
r_ti(3) = ORBmatrix(i_write,3, i_sat) !/ 1.D03

! Unit conversion: m to Km                 
r_ti = r_ti * 1.0D-3

cl_ti = 999999.999999D0

if (vel>0) then
! Velocity vector in dm/sec
v_ti(1) = ORBmatrix(i_write,6, i_sat) 
v_ti(2) = ORBmatrix(i_write,7, i_sat) 
v_ti(3) = ORBmatrix(i_write,8, i_sat) 
! Unit conversion: m/sec to dm/sec                 
v_ti = v_ti * 1.0D1
end if


Dcl_ti = 999999.999999D0


! ----------------------------------------------------------------------
!IF (i_sat == 1) THEN
IF (i_sat == ICON ) THEN
! Write the Epoch line

year  = IY
month = IM
day   = ID
!print*, 'year, month, day, Sec_00 =', year, month, day, Sec_00
if (1<0) then
hour_ti = INT(FD * 24.0D0) 
min_ti  = INT(FD * 24.0D0 * 60.0D0)
sec_ti  = (FD * 24.0D0 * 60.0D0 * 60.0D0)
else
hour_ti = INT(Sec_00 / (60.0D0 * 60.0D0)) 
min_ti  = INT(Sec_00/60.0D0 - hour_ti*60.0D0)  
sec_ti  = (Sec_00 - hour_ti*3600.0D0 - min_ti*60.D0)
end if
!print*,'time line=',year, month, day, Sec_00, hour_ti, min_ti,sec_ti
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
if (vel>0) then
WRITE (UNIT=UNIT_IN,FMT=fmt_vel,IOSTAT=ios_ith) 'V',PRN_ti, v_ti, Dcl_ti
end if
! ----------------------------------------------------------------------
end if


IF (ios_ith /= 0) THEN
    PRINT *, "Error in writing to file: ", TRIM (sp3_fname)
    PRINT *, "WRITE IOSTAT=", ios_ith
END IF
! ----------------------------------------------------------------------
END IF                    ! GNSS BRDC
!END IF

 END DO
 END DO

! ----------------------------------------------------------------------
! Last Line :: EOF
WRITE (UNIT=UNIT_IN,FMT='(A3)',IOSTAT=ios_ith) 'EOF'
! ----------------------------------------------------------------------

ENDFILE (UNIT = UNIT_IN) 
CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------


END SUBROUTINE


End Module
