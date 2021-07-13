MODULE m_interporb


! ----------------------------------------------------------------------
! MODULE: m_interporb.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the interp_orb subroutine used for orbit interpolation
! 
! Subroutines contained within the module:
! - interp_orb.f03
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Cooperative Research Centre for Spatial Information, Australia
! Created:	3 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE interp_orb (fname_sp3, PRN, interv_in, NPint, orbint)


! ----------------------------------------------------------------------
! SUBROUTINE: interp_orb.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 orbit interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - fname_sp3:  	Orbit file in sp3 format which contains only position vector 
! - PRN:			Satellite PRN number
! - interv_in: 		Interval of the interpolation epochs (in seconds)
! - NPint:			Number of data points to be used for interpolation (defines the order of the polynomial)
! - fname_orbint:	Output file name for writing interpolated orbit array  
!
! Output arguments:
! - orbint:   		Interpolated orbit including position and velocity vetors per epoch (allocatable array)
! ----------------------------------------------------------------------
! Note 1:
!  The input orbit file (sp3) is considered to include satellite position vector only
!  as this is the IGS standard product
!  During the orbit interpolation, based on Lagrange polynomials, position 
!  and velocity vectors for the selected satellite (PRN) are computed and
!  provided in the output files.  
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	5 August 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 14 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 adavantages in dynamic memory allocation
! - Dr. Thomas Papanikolaou, 4 May 2020:
! 	Modified for considering case of orbits with outliers (zero values)
! 	Initial orbit interpolation alogorithm moved to the m_interporb_nom.f03 
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_sp3
      USE m_lagrange
      !USE mdl_arr
      !USE mdl_write
      USE m_orb_outlier
      USE m_interporb_nom
      USE m_interporb_filt
      USE m_writearray
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname_sp3
      !CHARACTER (LEN=300), INTENT(IN) :: fname_orbint
      INTEGER (KIND = prec_int8), INTENT(IN) :: interv_in, NPint
	  !INTEGER (KIND = 4) :: PRN
      CHARACTER (LEN=3), INTENT(IN) :: PRN
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbint
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, sz1, sz2, interv 
      INTEGER (KIND = prec_int8) :: data_rate, Nepoch_int, Norb_int, NPint_1, NPint_2, Nlimit 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, ti
	  REAL (KIND = prec_d) :: SecDay_i, MJD_i 
	  REAL (KIND = prec_d) :: MJD_tint, tint, Xint, Xint_1, Yint, Yint_1, Zint, Zint_1
	  REAL (KIND = prec_d) :: X, Y, Z, Vx, Vy, Vz
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: Y_interp 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: clock_matrix
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Noutliers 
	  REAL (KIND = prec_d) :: outlier_value 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3_filt, orb_out
      CHARACTER (LEN=100) :: filename

 
! ----------------------------------------------------------------------
! Read IGS sp3 orbit data file (position only): orbsp3 
Call sp3 (fname_sp3, PRN, orbsp3, clock_matrix)
sz1 = size(orbsp3, DIM = 1)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Test input orbit for detecting outliers (coordinates with zero values):
! ----------------------------------------------------------------------
Noutliers = 0
outlier_value = 0.0D0
CALL orb_outlier (orbsp3, outlier_value, Noutliers, orbsp3_filt, orb_out)
!print *, "Noutliers ", Noutliers

IF (Noutliers == 0) THEN
! Orbit Lagrange interpolation using the input orbit matrix
CALL interp_orb_nom (fname_sp3, PRN, interv_in, NPint, orbint)

ELSE
! Orbit Lagrange interpolation after removing outliers of the input orbit matrix
CALL interp_orb_filt (orbsp3, orbsp3_filt, interv_in, NPint, orbint)

!write (filename, FMT='(A7,A3,A4)') 'orbint_', (PRN), '.out'
!Call writearray (orbint, filename)
END IF
! ----------------------------------------------------------------------
  
End subroutine

END Module
