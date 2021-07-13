SUBROUTINE erp_igu (filename,mjd_t , erp, igu_flag)


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
!   				erp 2x5 matrix
!   				erp (i,1:5) = [MJD xp yp UT1_UTC LOD]
!   				MJD:     	MJD epoch referred in the .erp file
!   				x,y:     	Polar motion coordinates (arcsec)
!   				UT1_UTC: 	Difference between UT1 and UTC (sec)
!   				LOD: 		Length Of Day (sec)
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
      REAL (KIND = prec_d), INTENT(IN) :: mjd_t
      CHARACTER (LEN=512), INTENT(IN) :: filename
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: erp(2,5)
      LOGICAL :: igu_flag
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_line, ios_data
      INTEGER (KIND = prec_int8) :: i, erp_i
      CHARACTER (LEN=100) :: Format_eop
      CHARACTER (LEN=170) :: line_ith  
      CHARACTER (LEN=20) :: char1, char2 
      INTEGER (KIND = prec_int8) :: mjd_day
      REAL (KIND = prec_d) :: mjd_ith, Xpole,Ypole, UT1_UTC,LOD, Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
      REAL (KIND = prec_d) :: igu_erp(2,5)
! ----------------------------------------------------------------------


      igu_flag = .TRUE.

! ----------------------------------------------------------------------
      UNIT_IN = 9
      Format_eop = '(F8.2,2X, I6,  2(F12.7),2(F11.6),2(F11.6),2(F11.7),2F12.6)'
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
      !data_i = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
	     i = i + 1
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------
! Data
  	     READ (line_ith, * , IOSTAT=ios_data) char1  ! 1st word
         if (char1 == 'MJD') then
	        READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
            do erp_i = 1 , 2
	           READ (UNIT=UNIT_IN,FMT=*,IOSTAT=ios_line) mjd_ith, Xpole, Ypole, UT1_UTC, LOD, &
                                                         Xsig,Ysig, UTsig,LODsig, Nr,Nf,Nt, Xrt,Yrt, Xrtsig,Yrtsig
               ! ERP matrix for the two epochs provided in the *.erp file			
               igu_erp (erp_i,1) = mjd_ith
               igu_erp (erp_i,2) = Xpole * 1.0D-6    ! Conversion to arcsec 
               igu_erp (erp_i,3) = Ypole * 1.0D-6    ! Conversion to arcsec 
               igu_erp (erp_i,4) = UT1_UTC * 1.0D-7  ! Conversion to seconds
               igu_erp (erp_i,5) = LOD * 1.0D-7      ! Conversion to seconds
            end do
         end if
! ----------------------------------------------------------------------
      END DO
      CLOSE (UNIT=UNIT_IN)
! ----------------------------------------------------------------------

! Earth Rotation Parameters of .erp file
         erp = igu_erp

! ----------------------------------------------------------------------
! Test the time coverage
      if ( mjd_t >= igu_erp(1,1) .and. mjd_t <= igu_erp(2,1) ) then
         !erp = igu_erp
         igu_flag = .TRUE.
      else 
	     igu_flag = .FALSE.
         !PRINT *,"d_mjd:", mjd_t-igu_erp(1,1), mjd_t-igu_erp(2,1)
         !PRINT *,"igu_erp:", igu_erp
      end if
! ----------------------------------------------------------------------


END
