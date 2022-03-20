MODULE m_sp3_PRN


! ----------------------------------------------------------------------
! MODULE: m_sp3_PRN.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the subroutine 'sp3_PRN'
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  13 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE                   
        


Contains


SUBROUTINE sp3_PRN (fname, PRNmatrix, year, month, day, Sec_00)

! ----------------------------------------------------------------------
! SUBROUTINE: sp3
! ----------------------------------------------------------------------
! Purpose:
!  Read the input sp3 orbit data file format (including multi-GNSS orbits) 
!  for outputting the satellites PRN numbers
! ----------------------------------------------------------------------
! Input arguments:
! - fname:              sp3 file name
!
! Output allocatable arrays:
! - PRNmatrix:          PRN array of all the satellites included in the sp3 file
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Geoscience Australia, Frontier-SI 
! Created:  28 March 2019
! ----------------------------------------------------------------------

      USE mdl_precision
      USE mdl_num
      use pod_yaml
      use mdl_param
      !USE mdl_arr
      IMPLICIT NONE
        
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname

! OUT
        CHARACTER (LEN=3), ALLOCATABLE, INTENT(OUT) :: PRNmatrix(:)
!year, month, day, sec_day      
      INTEGER :: year, month, day
        REAL (KIND = prec_q), INTENT(OUT) :: Sec_00 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER :: Nyear, Myear, Nmonth, Mmonth, Nday, Mday
      INTEGER :: Mhr, Nhr, Mminute, Nminute
      DOUBLE PRECISION Msec, Nsec
      CHARACTER (LEN=1) :: GNSSid

      CHARACTER (LEN=2) :: ver
      CHARACTER (LEN=1) :: Zvec
      INTEGER (KIND = prec_int8) :: Nepochs
      INTEGER (KIND = prec_int4) :: Ncol, Nvec

      INTEGER :: hr, minute, J_flag, hdiff, hkdiff
      DOUBLE PRECISION sec, DJM0, FD, mjd_i, mjd_j, mjd_k, diff, kdiff, mjd_diff

      REAL (KIND = prec_q) :: r_x, r_y, r_z
        REAL (KIND = prec_q) :: v_x, v_y, v_z
        REAL (KIND = prec_q) :: clock_r, clock_v 
      REAL (KIND = prec_q) :: r(3), v(3)

      INTEGER (KIND = prec_int8) :: i, orb_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      CHARACTER (LEN=7) :: Format1
      CHARACTER (LEN=100) :: fmt_line
      CHARACTER (LEN=500) :: line_ith       
      CHARACTER (LEN=1) :: char1st
      CHARACTER (LEN=1) :: char1
      CHARACTER (LEN=2) :: char2
      CHARACTER (LEN=3) :: char3
      CHARACTER (LEN=200) :: charN 
      CHARACTER (LEN=1) :: GNSSlet
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: space_i, pflag
      INTEGER (KIND = prec_int2) :: AllocateStatus
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nsat
      INTEGER (KIND = prec_int8) :: isat
      CHARACTER (LEN=3) :: PRN_isat
! ----------------------------------------------------------------------
  
       MYear = 0
       Nyear = 0
       Mmonth = 0
       Nmonth = 0
       Mday = 0
       Nday = 0
       Mhr = 0
       Nhr = 0
       Mminute = 0
       Nminute = 0
       Msec = 0
       Nsec = 0

UNIT_IN = 9                                                                         

! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (fname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", fname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read data file
! ----------------------------------------------------------------------
isat = 0
i = 0
orb_i = 0
Format1 = '(A)'
mjd_j = 0.d0
mjd_diff = 0.d0

DO
      READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
      i = i + 1
             
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT        
         END IF
! ----------------------------------------------------------------------

! ---------------------------------------------------------------------- 
! 1st character of line        
         fmt_line = '(A1)'
             READ (line_ith, fmt_line , IOSTAT=ios_data) char1st
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st line : Read Initial Epoch (year, month, day, sec_day)
! ----------------------------------------------------------------------
         If (i == 1) then
                  fmt_line = '(A2,A1,I4)'
            READ (line_ith, fmt_line , IOSTAT=ios_data) ver, Zvec, year
                  READ (line_ith, * , IOSTAT=ios_data) charN, month, day, hr, minute, sec, Nepochs, charN 
                     
            if (Zvec == 'P') then
               Nvec = 3
                  else if (Zvec == 'V') then
                     Nvec = 6
                  end if

! Sec of Day (since 0h)
            Sec_00 = sec + minute * 60D0 + hr * 3600D0
             End if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! 3rd line: Satellites Number ! and PRN numbers
! ----------------------------------------------------------------------
         If (i == 3) then
                  fmt_line = '(A2,A1, I3 ,A)'
            READ (line_ith, fmt_line , IOSTAT=ios_data) char2, char1, Nsat, charN
                  !print *,"Nsat ", Nsat
                  ALLOCATE (PRNmatrix(Nsat), STAT = AllocateStatus)              
             End if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Satellites PRNs :: Check epochs
! ----------------------------------------------------------------------
IF (char1st == "*") THEN

         IF (char1st == "*" .and. yml_interpolate_start > 0.d0) THEN
            !*  2010 12 25  0  0  0.0000 
			READ (line_ith, * , IOSTAT=ios_data) char3, Nyear, Nmonth, Nday, Nhr, Nminute, Nsec  

! MJD of Epoch (including fraction of the day)
            CALL iau_CAL2JD ( Nyear, Nmonth, Nday, DJM0, mjd_i, J_flag )
            mjd_i = mjd_i + (Nsec/86400D0) + (Nhr/24D0) + (Nminute/60D0/24D0)
            if (yml_interpolate_start - mjd_i >= -1.0d-6) then
                    Myear = Nyear
                    Mmonth = Nmonth
                    Mday = Nday
                    Mhr = Nhr
                    Mminute = Nminute
                    Msec = Nsec
            end if
            if (mjd_diff == 0.d0 .and. mjd_j > 0.d0) then
                mjd_diff = mjd_i - mjd_j
             end if
             mjd_j = mjd_i

         end if

! ----------------------------------------------------------------------
! Satellites loop
! ----------------------------------------------------------------------
    DO
        READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
        i = i + 1
        ! 1st character of line            
      fmt_line = '(A1)'
        READ (line_ith, fmt_line , IOSTAT=ios_data) char1st
! ----------------------------------------------------------------------
! End of Loop
If (isat .GE. Nsat) Then
    EXIT          
END IF
! ----------------------------------------------------------------------
! read epoch
         IF (char1st == "*" .and. yml_interpolate_start > 0.d0) THEN
            !*  2010 12 25  0  0  0.0000 
			READ (line_ith, * , IOSTAT=ios_data) char3, Nyear, Nmonth, Nday, Nhr, Nminute, Nsec  

! MJD of Epoch (including fraction of the day)
            CALL iau_CAL2JD ( Nyear, Nmonth, Nday, DJM0, mjd_i, J_flag )
            mjd_i = mjd_i + (Nsec/86400D0) + (Nhr/24D0) + (Nminute/60D0/24D0)
            if (yml_interpolate_start - mjd_i >= -1.0d-6) then
                    Myear = Nyear
                    Mmonth = Nmonth
                    Mday = Nday
                    Mhr = Nhr
                    Mminute = Nminute
                    Msec = Nsec
            end if
            if (mjd_diff == 0.d0 .and. mjd_j > 0.d0) then
                mjd_diff = mjd_i - mjd_j
             end if
             mjd_j = mjd_i

         end if

             ! Position PRN
         IF (char1st == "P") THEN
                  !READ (line_ith, * , IOSTAT=ios_data) char1, PRN_i !, r_x, r_y, r_z, clock_r
                  
                  !fmt_line = '(A1,A1,I2.2,4F14.6,A)'
                  !READ (line_ith, fmt_line , IOSTAT=ios_data) char1, GNSSlet, PRN_i, r_x, r_y, r_z, clock_r, charN

                  !WRITE(PRN_isat, FMT='(A1,I2.2)', IOSTAT=ios_data) GNSSlet, PRN_i
                  !print *,"GNSSlet ", GNSSlet
                  !print *,"PRN_i ", PRN_i
                  !print *,"PRN_isat ", PRN_isat
                  
                  fmt_line = '(A1,A3,A)'
                  READ (line_ith, fmt_line , IOSTAT=ios_data) char1, PRN_isat, charN
                  isat = isat + 1               
                  PRNmatrix(isat) = PRN_isat
                  !print *,"PRN_isat ", PRN_isat
                  !print *,"isat ", isat
                  !CALL SLEEP(1)
             END IF
    END DO  
! ----------------------------------------------------------------------
END IF
! ----------------------------------------------------------------------

END DO
CLOSE (UNIT=UNIT_IN)
! ----------------------------------------------------------------------

if (Myear /= Nyear .or. Mmonth /= Nmonth .or. Mday /= Nday .or. &
    MHr /= Nhr .or. Mminute /= Nminute .or. Msec /= Nsec) then
! MJD of original Epoch (including fraction of the day)
            CALL iau_CAL2JD ( year, month, day, DJM0, mjd_i, J_flag )
            mjd_i = mjd_i + (sec/86400D0) + (hr/24D0) + (minute/60D0/24D0)
      
    year = Myear
    month = Mmonth
    day = Mday
    hr = Mhr
    minute = Mminute
    sec = Msec
    sec_00 = sec + minute * 60.0 + hr * 3600.0

    call iau_CAL2JD(year, month, day, DJM0, mjd_j, J_flag)
    mjd_j = mjd_j + (sec/86400D0) + (hr/24D0) + (minute/60D0/24D0)

    diff = mjd_j - mjd_i
    hdiff = INT(24D0 * diff + 0.00001)

    ! mjd of last time in sp3 file
    call iau_CAL2JD(Nyear, Nmonth, Nday, DJM0, mjd_k, J_flag)
    mjd_k = mjd_k + (Nsec/864000D0) + (Nhr/24D0) + (Nminute/640D0/24D0)

    ! need to add one timestep because sp3 files do not include the end point
    !print *, mjd_k, mjd_j, mjd_diff
    kdiff = mjd_k - mjd_j + (2.d0 * mjd_diff)
    !print *, kdiff
    hkdiff = INT(24D0 * kdiff + 0.000001)

    if (yml_orbit_arc_determination > hkdiff) then
        ! run to min of time remaining in file and current determination arc
        yml_orbit_arc_determination = hkdiff
        orb_est_arc = kdiff * 86400D0 ! it might not be a full hour ...
        print *, "determination arc reduced to ", hkdiff, " hours"
    end if

    yml_determination_arc_corrected = .true.

    !debug: print out reported start time and hdiff
    !print *, year, month, day, hr, minute, sec, " correction = ", hdiff
end if

END SUBROUTINE


End Module


