SUBROUTINE readbrdcheader (UNIT_IN, VERSION, IONALPHA, IONBETA, A0UTC, A1UTC, &
                     ITUTC, NWKUTC, LEAP)


! ----------------------------------------------------------------------
! SUBROUTINE: readbrdcheader.f03
! ----------------------------------------------------------------------
! Purpose: This reoutine is used to read the header of the broadcast ephemeris file.
!          (Currently, GPS only)  
! ----------------------------------------------------------------------
! - INPUT: 
!         UNIT_IN  : Local number for the file reading
!
! - OUTPUT:
!         VERSION  : Index of the RINEX version of the broadcast ephemeris
!         IONALPHA : Ionosphere parameters
!         IONBETA  : Ionosphere parameters
!         A0UTC    : UTC polynomials
!         A1UTC    : 
!         ITUTC    : Reference time for UTC
!         NWKUTC   : Reference week for UTC
!         LEAP     : Leap second
!
! ----------------------------------------------------------------------
!
! Author :    Tzupang Tseng
!
! Copyright:  GEOSCIENCE AUSTRALIA 
!
! Created:    31-05-2019
!
! Changes:    04-06-2019  Tzupang Tseng: The current version is only valid for
!                                        the GPS broadcast ephemeris. 
!
! ----------------------------------------------------------------------
	  
	  
      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      INTEGER (KIND = prec_int4), INTENT(IN) :: UNIT_IN
! OUT
      INTEGER (KIND = prec_int4),INTENT(OUT) :: VERSION
      INTEGER (KIND = prec_int4),INTENT(OUT) :: LEAP, NWKUTC, ITUTC
      REAL (KIND = prec_d),DIMENSION(4),INTENT(OUT)::IONALPHA, IONBETA 
      REAL (KIND = prec_d),INTENT(OUT):: A0UTC,A1UTC
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: filename				
      INTEGER (KIND = prec_int4) :: i,j 
      INTEGER (KIND = prec_int4) :: ios
      INTEGER (KIND = prec_int4) :: ios_line
      CHARACTER (LEN=7) :: Format1
      CHARACTER (LEN=100) :: HEAD
      CHARACTER (LEN=500) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0	  
! ----------------------------------------------------------------------

! Read input file
Format1 = '(A80)'

i = 0
!DO WHILE (.TRUE.)

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1

! Read the first line
IF (i .EQ. 1) THEN
READ (line_ith(1:6),'(I6)') VERSION
PRINT*,'RINEX VERIOSN=',VERSION
READ (line_ith(61:80),'(A20)') HEAD
IF(VERSION .LE. 0) &
PRINT*,'RINEX VERIOSN=',VERSION,'UNEXPECTED VALUE'
IF(HEAD .NE. 'RINEX VERSION / TYPE') &
PRINT*,'THE RINEX VERSION OF BROADCAST EPHEMERIS CANNOT BE IDENTIFIED !!'
END IF

IF (VERSION .LT. 3) THEN
! Read other information from the header
DO WHILE (.TRUE.)
READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
    READ (line_ith(61:80),'(A20)') HEAD
    IF      (HEAD .EQ. 'ION ALPHA           ') THEN
    READ(line_ith(1:50),'(2X,4D12.4)') (IONALPHA(j),j=1,4)
    PRINT*,'ION ALPHA =',IONALPHA(1),IONALPHA(2),IONALPHA(3),IONALPHA(4)
    ELSE IF (HEAD .EQ. 'ION BETA            ') THEN
    READ(line_ith(1:50),'(2X,4D12.4)') (IONBETA(j),j=1,4)
    PRINT*,'ION BETA =',IONBETA(1),IONBETA(2),IONBETA(3),IONBETA(4)
    ELSE IF (HEAD .EQ. 'DELTA-UTC: A0,A1,T,W') THEN
    READ(line_ith(1:59),'(3X,2D19.12,2I9)') A0UTC,A1UTC,ITUTC,NWKUTC
    PRINT*,'DELTA-UTC: A0,A1,T,W',A0UTC,A1UTC,ITUTC,NWKUTC
    ELSE IF (HEAD .EQ. 'LEAP SECONDS        ') THEN
    READ(line_ith,'(I6)') LEAP
    PRINT*,'LEAP SECONDS',LEAP
    ELSE IF (HEAD .EQ. 'END OF HEADER       ') THEN
    PRINT*,'START TO READ THE ORBITAL PARAMETERS'
    RETURN
    END IF
END DO

ELSE IF (VERSION .GE. 3) THEN

DO WHILE (.TRUE.)
READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
     READ (line_ith(61:80),'(A20)') HEAD
!     PRINT*,'HEAD =', HEAD
     IF (HEAD .EQ. 'LEAP SECONDS        ') THEN
     READ(line_ith,'(I6)') LEAP
     PRINT*,'LEAP SECONDS',LEAP
     ELSE IF (HEAD .EQ. 'END OF HEADER       ') THEN
     PRINT*,'START TO READ THE ORBITAL PARAMETERS'
     RETURN
     END IF

END DO

END IF

!END DO


END SUBROUTINE
