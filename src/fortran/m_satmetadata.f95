MODULE m_satmetadata


! ----------------------------------------------------------------------
! MODULE: m_satmetadata.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the attitude_orb subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 March 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains
	  
	  
SUBROUTINE satmetadata (PRN_array, mjd, satsinex_filename, SVN_array, BLOCK_array, MASS_array, POWER_array)


! ----------------------------------------------------------------------
! SUBROUTINE: satmetadata
! ----------------------------------------------------------------------
! Purpose:
!  Store the GNSS satellite metadata to character allocatable arrays for the input PRN array and day
! ----------------------------------------------------------------------
! Input arguments:
! - PRN_array:				PRN numbers array e.g. G01, .., G32, E01, .., E30
! - mjd              		Modified Julian Day number (including the fraction of the day) 
! - satsinex_filename:		Name of IGS satellite meta data SINEX file   
!
! Output arguments:
! - SVN_array:				SVN numbers array e.g. G037
! - BLOCK_array:			Block type array e.g. GPS-IIA 
! - MASS_array:				Satellite mass array 
! - POWER_array:			Transmitting antenna power array 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 March 2020
! ----------------------------------------------------------------------
 

      USE mdl_precision
!      USE mdl_num
      USE mdl_param
      USE m_read_satsnx
      use pod_yaml
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(IN) :: PRN_array(:)
	  REAL (KIND = prec_d), INTENT(IN) :: mjd
      CHARACTER (LEN=*), INTENT(IN) :: satsinex_filename
! ----------------------------------------------------------------------
! OUT
      !CHARACTER (LEN=4), ALLOCATABLE, INTENT(OUT) :: SVN_array
      INTEGER (KIND = prec_int4), ALLOCATABLE, INTENT(OUT) :: SVN_array(:)
      CHARACTER (LEN=20), ALLOCATABLE, INTENT(OUT) :: BLOCK_array(:)
      REAL (KIND = prec_d), ALLOCATABLE, INTENT(OUT) :: MASS_array(:)
      INTEGER (KIND = prec_int4), ALLOCATABLE, INTENT(OUT) :: POWER_array(:)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, Nparam, Nsat, Natt 
      INTEGER (KIND = prec_int8) :: i_epoch, i_sat
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3
      INTEGER (KIND = prec_int8) :: i, j
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
	  CHARACTER (LEN=3) :: PRN_GNSS, PRN_isat
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: Sec_00, jd0, mjd_1
      !INTEGER (KIND = prec_int4) :: IY, IM, ID
      INTEGER Iyear, Imonth, Iday, J_flag
      DOUBLE PRECISION FD  
      !INTEGER (KIND = prec_int4) :: DOY
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Orbit arrays dimensions
sz1 = SIZE (PRN_array,DIM=1)
!sz2 = SIZE (PRN_array,DIM=2)

!Nepochs = sz1
!Nparam  = sz2
Nsat    = sz1

! Dynamic allocatable arrays
ALLOCATE (SVN_array(Nsat)  , STAT = AllocateStatus)
ALLOCATE (BLOCK_array(Nsat), STAT = AllocateStatus)
ALLOCATE (MASS_array(Nsat) , STAT = AllocateStatus)
ALLOCATE (POWER_array(Nsat), STAT = AllocateStatus)

! ----------------------------------------------------------------------
! Satellite metadata: Block type
! ----------------------------------------------------------------------
DO i_sat = 1 , Nsat
   if (.not. yml_satellites(i_sat)) cycle
   ! Read SINEX file for SVN number, Mass, ..        
   Sec_00 = (mjd - INT(mjd)) * 24.0D0 * 3600.0D0
   jd0 = 2400000.5D0
   CALL iau_JD2CAL ( jd0, mjd, Iyear, Imonth, Iday, FD, J_flag )
   CALL iau_CAL2JD ( Iyear, 1, 1, jd0, mjd_1, J_flag )   
   !DOY = INT(mjd) - (mjd_1-1) 
   DOY = IDNINT(mjd-mjd_1) + 1
   YR = Iyear
   PRN_isat = PRN_array(i_sat)
   CALL read_satsnx (satsinex_filename, Iyear, DOY, Sec_00, PRN_isat)
      
SVN_array(i_sat)   = SVNID   
BLOCK_array(i_sat) = BLKTYP
MASS_array(i_sat)  = MASS
POWER_array(i_sat) = POWER

END DO   
! ----------------------------------------------------------------------

END SUBROUTINE

End

