SUBROUTINE brdcinfo (UNIT_IN, VERSION, ISVN, EPHDAT)


! ----------------------------------------------------------------------
! SUBROUTINE: brdcinfo.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read the orbital parameters of the broadcast ephemeris file  
! ----------------------------------------------------------------------
! Author :	Tzupang Tseng
!
! Copyright:	GEOSCIENCE AUSTRALIA 
!
! Created:	02-06-2019
!
! Changes:         
!
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN) :: PRMfname				
! OUT
      INTEGER (KIND = prec_int2),INTENT(OUT) :: VERSION
      INTEGER (KIND = prec_int8),INTENT(OUT) :: LEAP, NWKUTC
      REAL (KIND = prec_d),DIMENSION(4),INTENT(OUT)::IONALPHA, IONBETA 
      REAL (KIND = prec_d),INTENT(OUT):: A0UTC,A1UTC,ITUTC
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: filename				
      INTEGER (KIND = prec_int2) :: AllocateStatus,DeAllocateStatus
      INTEGER (KIND = prec_int8) :: i,j 
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      CHARACTER (LEN=7) :: Format1
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
! ----------------------------------------------------------------------

IF(VERSION.EQ.1) THEN
   N2=25
ELSE
   N2=29
ENDIF

! Read input file
Format1 = '(A80)'
READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith

! Read the orbital parameters
IF(VERSION.LT.3) THEN
READ(line_ith,'(I2,5I3,F5.1,3D19.12)') ISVN,IYEAR,MONTH,IDAY,IHOUR,MINUTE,SEC,&
                                     (EPHDAT(K),K=2,4)
IYEAR = IYEAR4(IYEAR)

ELSE IF(VERSION.EQ.3) THEN
READ(line_ith,'(A1,I2,I5,5I3,3D19.12)') cSvn,iSvn2,IYEAR,MONTH,IDAY,IHOUR,&
                                     MINUTE,iSec,(EPHDAT(K),K=2,4)
! Convert the cSvn to the ACS internal prn??
ELSE
PRINT*,'THE RINEX VERSION OF BROADCAST EPHEMERIS CANNOT BE RECOGNIZED!'
END IF

DAY=IDAY+IHOUR/24.D0+MINUTE/1440.D0+SEC/86400.D0
TOC=DJUL(IYEAR,MONTH,DAY)


DO  I=5,N2,4
READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
READ(line_ith,'(3X,4D19.12)') (EPHDAT(K),K=I,I+3)
END DO


! ----------------------------------------------------------------------

END
