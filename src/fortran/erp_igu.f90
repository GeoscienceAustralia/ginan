SUBROUTINE erp_igu (filename,mjd_t , erp,  igu_flag)


! ----------------------------------------------------------------------
! Subroutine:  erp_igu.f90
! ----------------------------------------------------------------------
! Purpose:
!  Read ERP (Earth Rotation Parameters) data from the ultra rapid products
!  provided by the IGS (International GNSS Service)
! ----------------------------------------------------------------------
! Input arguments:
! - filename:		IGS ultra-rapid ERP data file name  e.g. igu18861_00.erp
! - mjd_t:			Modified Julian Day number at the required epoch
!					(including fraction of the day)
! - igu_flag:       Logical value for reporting if the input epoch is in/out
!					of the time range covered by the ERP file
!   .TRUE.			Input epoch is within the range         
!   .FALSE.			Input epoch is out of the range        
!
! Output arguments:
! - erp:			Array of the ERP data obtained from the .erp file
!					ERP data are provided for two epochs every 6 hours
!   				erp (:) = [MJD xp yp UT1_UTC LOD]
!   				MJD:     	MJD epoch referred in the .erp file
!   				x,y:     	Polar motion coordinates (arcsec)
!   				UT1_UTC: 	Difference between UT1 and UTC (sec)
!   				LOD: 		Length Of Day (sec)
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou, Geoscience Australia               March 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd_t
      CHARACTER (LEN=512), INTENT(IN) :: filename
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: erp(EOP_MAX_ARRAY)
      LOGICAL, INTENT(OUT)  :: igu_flag
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_line, ios_data
      INTEGER (KIND = prec_int8) :: i, j, erp_i, erp_last, erp_j
      CHARACTER (LEN=100) :: Format_eop
      CHARACTER (LEN=170) :: line_ith  
      CHARACTER (LEN=20) :: char1, char2 
      INTEGER (KIND = prec_int8) :: mjd_day, sz1, sz2
      REAL (KIND = prec_d) :: mjd_ith, Xpole,Ypole, UT1_UTC,LOD, Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
      REAL (KIND = prec_d) :: igu_erp(100,EOP_MAX_ARRAY),first_erp(EOP_MAX_ARRAY)
      LOGICAL first
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
      UNIT_IN = 9
      Format_eop = '(F9,1X, I6,  2(F12.7),2(F11.6),2(F11.6),2(F11.7),2F12.6)'
      first = .false.
      erp_last = 1
! ----------------------------------------------------------------------

      if (.false.) print *, "erp_igu: looking for mjd ", mjd_t

      igu_erp = 0.d0
      erp = 0.d0
      igu_flag = .false.
! ----------------------------------------------------------------------
! Open data file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (filename), STATUS='OLD', IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", filename
         PRINT *, "OPEN IOSTAT=", ios
         STOP
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read data file
! ----------------------------------------------------------------------
      i = 0
      !data_i = 0
      sz1 = size(igu_erp, 1)

      DO
      READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line, END=30) line_ith
      !print*, line_ith
	     i = i + 1
! ----------------------------------------------------------------------
! End of file
         IF (ios_line > 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------
! Data
  	     READ (line_ith, * , IOSTAT=ios_data) char1  ! 1st word
         if (char1 == 'MJD') then
                 !print *, "read mjd header line"
                 ! next line is the units line : skip
                 READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line, END=30) line_ith
                 !print *, line_ith
            erp_j = 1
            DO
            ! read sz1 lines at a time (sz1 - 1 after the first one)
            do erp_i = erp_j , sz1
            READ (UNIT=UNIT_in, FMT='(A)',IOSTAT=ios_line, END=30) line_ith
            !print *, line_ith
               if (ios_line > 0) then
                   exit
               end if
               Xrt = 0.d0
               Yrt = 0.d0
               READ (line_ith,*, ERR=10, END=10) mjd_ith, Xpole, Ypole, UT1_UTC, LOD, &
                                                         Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
               ! ERP matrix for the two epochs provided in the *.erp file			
10             igu_erp (erp_i,EOP_MJD) = mjd_ith
               !print *, "new data line"
               !print *, mjd_ith, Xpole, Ypole, UT1_UTC, LOD, Xsig, Ysig, UTsig, LODsig, Xrt, Yrt
               igu_erp (erp_i,EOP_X) = Xpole * 1.0D-6    ! Conversion to arcsec 
               igu_erp (erp_i,EOP_Y) = Ypole * 1.0D-6    ! Conversion to arcsec 
               igu_erp (erp_i,EOP_UT1) = UT1_UTC * 1.0D-7  ! Conversion to seconds
               igu_erp (erp_i,EOP_LOD) = LOD * 1.0D-7      ! Conversion to seconds
               igu_erp (erp_i,EOP_X_ERR) = Xsig * 1.0D-6 ! conversion to arcsec^M
               igu_erp (erp_i,EOP_Y_ERR) = Ysig * 1.0D-6 ! conversion to arc sec^M
               igu_erp (erp_i,EOP_UT1_ERR) = UTsig * 1.0D-7 ! conversion to seconds^M
               igu_erp (erp_i,EOP_LOD_ERR) = LODsig * 1.0D-7 ! conversion to seconds^M
               igu_erp (erp_i,EOP_DX) = Xrt * 1.0D-6 ! conversion to arcsec^M
               igu_erp (erp_i,EOP_DY) = Yrt * 1.0D-6 ! conversion to arc sec^M
               erp_last = erp_i
               if (.not. first) then
                    first_erp(:) = igu_erp(erp_i, :)
                    first = .true.
               end if
            end do
! ----------------------------------------------------------------------
! Test the time coverage
      !print*,  mjd_t, erp_last
      do i = 1, erp_last - 1
          !print *, igu_erp(i, EOP_MJD)
      if ( mjd_t >= igu_erp(i,EOP_MJD) .and. mjd_t < igu_erp(i+1,EOP_MJD) ) then
         !erp = igu_erp
         igu_flag = .TRUE.
         erp = igu_erp(i,:)
         !PRINT *,"d_mjd:", mjd_t-igu_erp(1,1), mjd_t-igu_erp(2,1)
         !PRINT *,"igu_erp:", igu_erp
         if (.false.) print *, mjd_t, i, erp
         exit
      end if
      end do

! ----------------------------------------------------------------------
        if (igu_flag) goto 30

        igu_erp(1,:) = igu_erp(erp_last,:)
        erp_j = 2

! ----------------------------------------------------------------------
      END DO
      end if
      END DO
30    CLOSE (UNIT=UNIT_IN)


if (.not. igu_flag) then
! ----------------------------------------------------------------------
! Test the time coverage
      !print*,  mjd_t, erp_last
      do i = 1, erp_last - 1
          !print *, igu_erp(i, EOP_MJD)
      if ( mjd_t >= igu_erp(i,EOP_MJD) .and. mjd_t < igu_erp(i+1,EOP_MJD) ) then
         !erp = igu_erp
         igu_flag = .TRUE.
         erp = igu_erp(i, :)
         !PRINT *,"d_mjd:", mjd_t-igu_erp(1,1), mjd_t-igu_erp(2,1)
         !PRINT *,"igu_erp:", igu_erp
         exit
      end if
      end do

end if 
! ----------------------------------------------------------------------

if (.not. igu_flag) then
        ! no coverage of mjd from file. Put the last entries in so can linearly 
        ! interpolate up to 24 hours into future
        if (mjd_t < igu_erp(1, EOP_MJD)) then
             ! mjd is before first entry. put first entry in
             erp = first_erp
             if (mjd_t + 2 < igu_erp(1, EOP_MJD)) then
                 print *, "error: mjd requested < 48 hours behind earliest ERP data, mjd =", mjd_t
                 STOP
             end if
             igu_flag = .true.
        else
            erp = igu_erp(erp_last,:)
            if (igu_erp(erp_last, EOP_MJD) + 2  < mjd_t) then
                print *, "error: mjd requested > 48 hours in front of latest ERP data, mjd = ", mjd_t
                STOP
            end if
            igu_flag = .true.
        end if
end if

END

SUBROUTINE erp_igu_read (ERP_fname, from, to, num_rows, eop_data)

      use mdl_precision
      use mdl_num
      use pod_yaml
      IMPLICIT NONE

      CHARACTER (LEN=*), INTENT(IN) :: ERP_fname
      REAL (KIND=prec_d), INTENT(IN) :: from, to
      ! 3000 is enough to read 1 single day of ERP data at 1 min intervals. Increase as necessary in pod_yaml
      REAL (KIND=prec_d), INTENT(OUT) :: eop_data(MAX_ERP_ROWS, EOP_MAX_ARRAY)
      INTEGER (KIND = prec_int4), INTENT(OUT) :: num_rows

      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_line, ios_data
      INTEGER (KIND = prec_int8) :: i, j, erp_i, erp_last, erp_j
      CHARACTER (LEN=100) :: Format_eop
      CHARACTER (LEN=170) :: line_ith  
      CHARACTER (LEN=20) :: char1, char2 
      INTEGER (KIND = prec_int8) :: mjd_day, sz1, sz2
      REAL (KIND = prec_d) :: mjd_ith, Xpole,Ypole, UT1_UTC,LOD, Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
      REAL (KIND = prec_d) :: erp(EOP_MAX_ARRAY),first_erp(EOP_MAX_ARRAY)

      UNIT_IN = 11
      Format_eop = '(F8.2,2X, I6,  2(F12.7),2(F11.6),2(F11.6),2(F11.7),2F12.6)'
      num_rows = 0
      first_erp = 0.d0
      erp = 0.d0

! ----------------------------------------------------------------------
! Open data file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (ERP_fname), STATUS='OLD', IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", ERP_fname
         PRINT *, "OPEN IOSTAT=", ios
         STOP
      END IF
! ----------------------------------------------------------------------

      DO
          READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line, END=30) line_ith

! ----------------------------------------------------------------------
! End of file
         IF (ios_line > 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT
         END IF
! ----------------------------------------------------------------------
! Data
         READ (line_ith, * , IOSTAT=ios_data) char1  ! 1st word
         if (char1 == 'MJD') then
             !print *, "read mjd header line"
             ! next line is the units line : skip
             READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line, END=30) line_ith
             !print *, line_ith
             do
             READ (UNIT=UNIT_in, FMT='(A)',IOSTAT=ios_line, END=30) line_ith
             !print *, line_ith
                 if (ios_line > 0) then
                     exit
                 end if
                 mjd_ith = 0.d0
                 Xpole = 0.d0
                 Ypole = 0.d0
                 UT1_UTC = 0.d0
                 LOD = 0.d0
                 Xsig = 0.d0
                 Ysig = 09.d0
                 UTsig = 0.d0
                 LODsig = 0.d0
                 Nr = 0.d0
                 Nf = 0.d0
                 Nt = 0.d0
                 Xrt = 0.d0
                 Yrt = 0.d0
                 Xrtsig = 0.d0
                 Yrtsig = 0.d0
                 READ (line_ith,*, ERR=10, END=10) mjd_ith, Xpole, Ypole, UT1_UTC, LOD, &
                                                         Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
                 ! ERP matrix for the two epochs provided in the *.erp file			
10               erp (EOP_MJD) = mjd_ith
                 !print *, "new data line"
                 !print *, mjd_ith, Xpole, Ypole, UT1_UTC, LOD, Xsig, Ysig, UTsig, LODsig, Xrt, Yrt
                 erp (EOP_X) = Xpole * 1.0D-6    ! Conversion to arcsec 
                 erp (EOP_Y) = Ypole * 1.0D-6    ! Conversion to arcsec 
                 erp (EOP_UT1) = UT1_UTC * 1.0D-7  ! Conversion to seconds
                 erp (EOP_LOD) = LOD * 1.0D-7      ! Conversion to seconds
                 erp (EOP_X_ERR) = Xsig * 1.0D-6 ! conversion to arcsec^M
                 erp (EOP_Y_ERR) = Ysig * 1.0D-6 ! conversion to arc sec^M
                 erp (EOP_UT1_ERR) = UTsig * 1.0D-7 ! conversion to seconds^M
                 erp (EOP_LOD_ERR) = LODsig * 1.0D-7 ! conversion to seconds^M
                 erp (EOP_DX) = Xrt * 1.0D-6 ! conversion to arcsec^M
                 erp (EOP_DY) = Yrt * 1.0D-6 ! conversion to arc sec^M

                 if (erp(EOP_MJD) < from) then
                     first_erp = erp
                 else if (.true.) then
                     if (first_erp(EOP_MJD) /= 0.d0) then
                         eop_data(1,:) = first_erp
                         eop_data(2,:) = erp
                         num_rows = 2
                     else
                         eop_data(1,:) = erp
                         num_rows = 1
                     end if
                     exit
                 end if
             end do
             do
             READ (UNIT=UNIT_in, FMT='(A)',IOSTAT=ios_line, END=30) line_ith
             !print *, line_ith
                 if (ios_line > 0) then
                     exit
                 end if
                 mjd_ith = 0.d0
                 Xpole = 0.d0
                 Ypole = 0.d0
                 UT1_UTC = 0.d0
                 LOD = 0.d0
                 Xsig = 0.d0
                 Ysig = 09.d0
                 UTsig = 0.d0
                 LODsig = 0.d0
                 Nr = 0.d0
                 Nf = 0.d0
                 Nt = 0.d0
                 Xrt = 0.d0
                 Yrt = 0.d0
                 Xrtsig = 0.d0
                 Yrtsig = 0.d0
                 READ (line_ith,*, ERR=20, END=20) mjd_ith, Xpole, Ypole, UT1_UTC, LOD, &
                                                         Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
                 ! ERP matrix for the two epochs provided in the *.erp file			
20               erp (EOP_MJD) = mjd_ith
                 !print *, "new data line"
                 !print *, mjd_ith, Xpole, Ypole, UT1_UTC, LOD, Xsig, Ysig, UTsig, LODsig, Xrt, Yrt
                 erp (EOP_X) = Xpole * 1.0D-6    ! Conversion to arcsec 
                 erp (EOP_Y) = Ypole * 1.0D-6    ! Conversion to arcsec 
                 erp (EOP_UT1) = UT1_UTC * 1.0D-7  ! Conversion to seconds
                 erp (EOP_LOD) = LOD * 1.0D-7      ! Conversion to seconds
                 erp (EOP_X_ERR) = Xsig * 1.0D-6 ! conversion to arcsec^M
                 erp (EOP_Y_ERR) = Ysig * 1.0D-6 ! conversion to arc sec^M
                 erp (EOP_UT1_ERR) = UTsig * 1.0D-7 ! conversion to seconds^M
                 erp (EOP_LOD_ERR) = LODsig * 1.0D-7 ! conversion to seconds^M
                 erp (EOP_DX) = Xrt * 1.0D-6 ! conversion to arcsec^M
                 erp (EOP_DY) = Yrt * 1.0D-6 ! conversion to arc sec^M

                 num_rows = num_rows + 1
                 if (num_rows > SIZE (eop_data, DIM=1)) then
                     num_rows = num_rows - 1
                     print *, MAX_ERP_ROWS, " lines of valid ERP data have ben read from your ERP file." 
                     print *, "Increase MAX_ERP_ROWS in m_pod_yaml.F90, recompile and try again"
                     STOP
                 end if
                 eop_data (num_rows, :) = erp

                 if (erp(EOP_MJD) > to) then
                     exit
                 end if
             end do
         end if
    end do

30  close (unit_in)
    if (num_rows == 0) then
        print *, "Warning: no rows of erp data found in the arc requested"
    end if

END
