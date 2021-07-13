MODULE m_orbitIC


! ----------------------------------------------------------------------
! MODULE: m_orbitIC.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for read orbit's Initial Conditions input file 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	29 August 2019
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE orbitIC (fname, IC_matrix, PRNmatrix)

! ----------------------------------------------------------------------
! SUBROUTINE: orbitIC 
! ----------------------------------------------------------------------
! Purpose:
!  Read orbit initial conditions file 
! ----------------------------------------------------------------------
! Input arguments:
! - filename:       Orbits Initial Conditions file name 
!
! Output arguments:
! - IC_matrix:		Orbits Initial Condtions matrix
! - PRNmatrix:		GNSS satellites PRNs matrix
!
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	29 August 2019
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_config
      USE mdl_param
      use pod_yaml
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=*), INTENT(IN) :: fname	  
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: IC_matrix
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(OUT) :: PRNmatrix(:)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, ipulse
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_ith, ios_data, ios_line
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3
      CHARACTER (LEN=1024) :: line_ith	  
      CHARACTER (LEN=150) :: word1_ln, word_i, healthy
      INTEGER (KIND = prec_int2) :: AllocateStatus	  
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nparam, Nsat, Nparam_isat 
      INTEGER (KIND = prec_int8) :: isat, iparam, jsat
	  CHARACTER (LEN=3) :: PRN_i
	  CHARACTER (LEN=1) :: char1
      INTEGER (KIND = prec_int8) :: mjd_i
      REAL (KIND = prec_q) :: sec00_i, xo, yo, zo, Vxo, Vyo, Vzo	  
! ----------------------------------------------------------------------


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
      i = 0	  
      isat = 0	  
	  Nparam = 0
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
	     i = i + 1
!PRINT *, "ios_line: ", ios_line
		 
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
! ----------------------------------------------------------------------
!PRINT *, "word1_ln: ", word1_ln

! ----------------------------------------------------------------------
! IC values per satellite 
! ----------------------------------------------------------------------
IF (word1_ln == "#IC_INFO") THEN

!#IC_INFO PRN: G01 SVN: 63 BLK_TYP: GPS-IIF    MASS:  1633.0000 SRP:  CBALL  ECOM2 Nparam:  15 - X Y Z XV YV ZV D0 Y0 B0 BC BS D2C D2S D4C D4S
!OR
!#IC_INFO PRN: G04 SVN: 36 ----> UNHEALTHY <---- 
  READ (line_ith, * , IOSTAT=ios_data) word_i, word_i, PRN_i, word_i, word_i , &
     word_i, healthy, word_i, word_i, word_i, word_i, word_i, word_i, Nparam_isat

  if ( trim(healthy) .ne. 'UNHEALTHY') then
    isat = isat + 1
    If (Nparam_isat > Nparam) Nparam = Nparam_isat
  else
    PRINT *, "WARNING - PRN: ", PRN_i, " set ", trim(healthy), " by PEA"
  endif
!PRINT *, "Nparam_isat: ", Nparam_isat
!PRINT *, "Nparam: ", word1_ln

END IF
! ----------------------------------------------------------------------
END DO
CLOSE (UNIT=UNIT_IN)
! ----------------------------------------------------------------------
Nsat = isat
!PRINT *, "Nsat, isat: ", Nsat, isat

! ----------------------------------------------------------------------
! Dynamic memory alloation :: Allocatable arrays PRNmatrix, IC_matrix
	ALLOCATE (PRNmatrix(Nsat), STAT = AllocateStatus)		   
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE orbitIC in module m_orbitIC.f03"
         PRINT *, "Error: Allocatable Array: PRNmatrix"
!         STOP "*** Not enough memory ***"
      END IF  
      PRNMatrix = "";
	ALLOCATE (IC_matrix(Nsat, Nparam+2), STAT = AllocateStatus)		   
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE orbitIC in module m_orbitIC.f03"
         PRINT *, "Error: Allocatable Array: IC_matrix"
!         STOP "*** Not enough memory ***"
      END IF  
      IC_matrix = 0.d0
        ALLOCATE (IC_pulse_matrix_glb(yml_pulse_epoch_number*Nsat,yml_pulse_epoch_number,&
                yml_pulse_parameter_count*yml_pulse_epoch_number+2), STAT = AllocateStatus)
      IF (AllocateStatus /= 0) THEN
         PRINT *, "Error: Not enough memory"
         PRINT *, "Error: SUBROUTINE orbitIC in module m_orbitIC.f03"
         PRINT *, "Error: Allocatable Array: IC_pulse_matrix_glb"
!         STOP "*** Not enough memory ***"
      END IF
      IC_pulse_matrix_glb = 0.d0

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
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
      i = 0	  
      ipulse = 0
      isat = 0	  
      DO
	     READ (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_line) line_ith
	     i = i + 1
!PRINT *, "ios_line: ", ios_line
		 
! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT		
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
! ----------------------------------------------------------------------
!PRINT *, "word1_ln: ", word1_ln

! ----------------------------------------------------------------------
!IF (word1_ln == "#INFO") THEN
IF (1 == 0) THEN

!#INFO    Number of Satellites:               32
READ (line_ith, FMT='(A30)' , IOSTAT=ios_data) word_i
IF (word_i == "#INFO    Number of Satellites:") THEN
	!READ (line_ith, FMT='(A30,I2)' , IOSTAT=ios_data) word_i, Nsat
        !ALLOCATE (PRNmatrix(Nsat), STAT = AllocateStatus)		   
END IF

!#INFO    Number of Parameters per satellite:  15
READ (line_ith, FMT='(A44)' , IOSTAT=ios_data) word_i
IF (word_i == "#INFO    Number of Parameters per satellite:") THEN
	!READ (line_ith, FMT='(A44, I2)' , IOSTAT=ios_data) word_i, Nparam
	!ALLOCATE (IC_matrix(Nsat, Nparam+2), STAT = AllocateStatus)		   
END IF

END IF
! ----------------------------------------------------------------------
!print *,"line_ith ", line_ith


! ----------------------------------------------------------------------
! IC values per satellite 
! ----------------------------------------------------------------------
IF (word1_ln == "#IC_XYZ") THEN
isat = isat + 1

! IC values
!#IC_XYZ  G01 [SVN] [BLK_TYP] ITRF           57841.0000       0.000000 -15082927.9512453451752663   14142716.4518523328006268   16413798.3563747480511665       -714.1089911918757025      -3197.4010676008779228       2111.1774148889130629          0.0000001064401950          0.0000000001126749         -0.0000000053241645          0.0000000031860054         -0.0000000001865469         -0.0000000005529211         -0.0000000027916161          0.0000000051002296         -0.0000000010041421 
!#IC_XYZ  G01 63 GPS-IIF    ICRF                58682       0.000000     1.4226165535956513e+07      2.0021922734854087e+07     -9.8755972053173799e+06     -1.2542766891020451e+03      2.2743003266646274e+03      2.8916623320139997e+03      1.0579865916659674e+00      6.7691004803552588e-05     -5.4338350804146425e-03     -2.3344459476977338e-02      6.0793666980937886e-04      2.4929169079701968e-02     -1.3609115148775948e-02     -3.4963241007821687e-02     -3.9319723496260943e-02 

IF (1>0) then

READ (line_ith, * , IOSTAT=ios_data) word_i, PRN_i, word_i, word_i, word_i, IC_matrix(isat,1:Nparam+2)
PRNmatrix(isat) = PRN_i
! ----------------------------------------------------------------------
ELSE

READ (line_ith, * , IOSTAT=ios_data) word_i, PRN_i, word_i, word_i, word_i, mjd_i, sec00_i, xo, yo, zo, Vxo, Vyo, Vzo, word_i 

PRNmatrix(isat) = PRN_i
IC_matrix(isat,1) = mjd_i
IC_matrix(isat,2) = sec00_i
IC_matrix(isat,3) = xo
IC_matrix(isat,4) = yo
IC_matrix(isat,5) = zo
IC_matrix(isat,6) = Vxo
IC_matrix(isat,7) = Vyo
IC_matrix(isat,8) = Vzo

BACKSPACE (UNIT=UNIT_IN,IOSTAT=ios_data)
READ (UNIT=UNIT_IN,FMT='(A33)',ADVANCE="no",IOSTAT=ios_data) word_i
READ (UNIT=UNIT_IN,FMT='(I9)',ADVANCE="no",IOSTAT=ios_data) mjd_i
READ (UNIT=UNIT_IN,FMT='(F25.10)',ADVANCE="no",IOSTAT=ios_data) sec00_i

DO iparam = 1 , Nparam
	READ (UNIT=UNIT_IN,FMT='(F27.16)',ADVANCE="no",IOSTAT=ios_data) IC_matrix(isat,iparam+2) 
END DO

END IF
! ----------------------------------------------------------------------
word1_ln = ''
line_ith = ''
ipulse = 0
END IF

jsat = 0
! ----------------------------------------------------------------------
! IC pulse values per satellite (01-02-2021 Tzupang Tseng)

IF (word1_ln == "#IC_PULSE_INFO") THEN
        ipulse = ipulse + 1
READ (line_ith, * , IOSTAT=ios_data) word_i, PRN_i
READ (PRN_i,'(1A,I2)') char1, jsat
READ (line_ith, * , IOSTAT=ios_data) word_i, word_i, word_i, word_i, word_i, &
                                  &  word_i, word_i, word_i, IC_pulse_matrix_glb(jsat,ipulse,&
                                  1:yml_pulse_parameter_count*yml_pulse_epoch_number+2)
!print*,'line_ith = ', line_ith
!print*,'IC_PULSE_INFO, PRN , ipulse =', jsat, ipulse, IC_pulse_matrix_glb(jsat,ipulse,1:5)
!if (jsat == 2) stop
END IF

END DO
CLOSE (UNIT=UNIT_IN)
	  
END SUBROUTINE

End Module

