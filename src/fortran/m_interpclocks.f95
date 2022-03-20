MODULE m_interpclocks


! ----------------------------------------------------------------------
! MODULE: m_interpclocks.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the interp_clocks subroutine used for 
!  GNSS satellite clocks errors interpolation
! 
! Subroutines contained within the module:
! - interp_clocks.f03
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	27 May 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE interp_clocks (ORB_matrix, CLKmatrix, PRNmatrix, CLKmatrix_int)


! ----------------------------------------------------------------------
! SUBROUTINE: interp_clocks.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 clocks interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - ORBmatrix:      Satellite Orbits' allocatable array (3-dimensional)
! - CLKmatrix:		Satellite Clocks' allocatable array (3-dimensional)
!					CLKmatrix(i,j,isat) = [MJD Sec_of_day clock]
!					If clock error (sdev) is available (when Position and Velocity vector are written in orbit sp3 format)
!					CLKmatrix(i,j,isat) = [MJD Sec_of_day clock clock_sdev]
! - PRNmatrix:      Satellites' PRN numbers allocatable array
!
! Output arguments:
! - CLKmatrix_int:  Interpolated Satellite Clocks' allocatable array (3-dimensional)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	27 May 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_sp3
      USE m_lagrange
      !USE m_orb_outlier
      !USE m_interporb_nom
      !USE m_interporb_filt
      USE m_writearray
      USE m_interpclock_nom
      USE pod_yaml
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: ORB_matrix, CLKmatrix 
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(IN) :: PRNmatrix(:)
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:,:), ALLOCATABLE :: CLKmatrix_int
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      INTEGER (KIND = prec_int2) :: first_real_sat
      CHARACTER (LEN=300) :: fname_write
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: clock_matrix
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, interv_in, NPint 
      INTEGER (KIND = prec_int8) :: Nepochs_orb, Nepochs_clk, Nelements_clk, Nsat_clk  
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3  
      INTEGER (KIND = prec_int8) :: orb_rate, clk_rate
 
 
sz1 = size(yml_satellites, DIM = 1)
first_real_sat = 0
do i = 1, sz1
   if (yml_satellites(i)) then
       if (first_real_sat .eq. 0) then
          first_real_sat = i
          exit
       end if
    end if
end do

! remove warning
orb_rate = 0.d0
clk_rate = 0.d0

! ----------------------------------------------------------------------
sz1 = size(ORB_matrix, DIM = 1)
Nepochs_orb = sz1

sz1 = size(CLKmatrix, DIM = 1)
Nepochs_clk = sz1

sz2 = size(CLKmatrix, DIM = 2)
Nelements_clk = sz2

sz3 = size(CLKmatrix, DIM = 3)
Nsat_clk = sz3
! ----------------------------------------------------------------------
 
! ----------------------------------------------------------------------
! Orbits rate 
      DO i = 1 , Nepochs_orb - 1
		if ( INT(ORB_matrix(i+1,1,first_real_sat)) - INT(ORB_matrix(i,1,first_real_sat)) == 0 ) then
			orb_rate = INT( ORB_matrix(i+1,2,first_real_sat) - ORB_matrix(i,2,first_real_sat) )
			EXIT
		end if
	  End Do

! Clocks rate 
      DO i = 1 , Nepochs_clk - 1
		if ( INT(CLKmatrix(i+1,1,first_real_sat)) - INT(CLKmatrix(i,1,first_real_sat)) == 0 ) then
			clk_rate = INT( CLKmatrix(i+1,2,first_real_sat) - CLKmatrix(i,2,first_real_sat) )
			EXIT
		end if
	  End Do	  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
IF (clk_rate == orb_rate) THEN

! Allocatable arrays for the interpolated orbit array
ALLOCATE (CLKmatrix_int(Nepochs_clk, Nelements_clk, Nsat_clk) , STAT = AllocateStatus)

CLKmatrix_int(:,:,:) = CLKmatrix(:,:,:)

ELSE

interv_in = orb_rate
NPint = 12
CALL interpclock_nom (ORB_matrix, CLKmatrix, PRNmatrix, interv_in, NPint, CLKmatrix_int)

END IF
! ----------------------------------------------------------------------

  
End subroutine

END Module
