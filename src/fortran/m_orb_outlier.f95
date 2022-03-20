MODULE m_orb_outlier


! ----------------------------------------------------------------------
! MODULE: m_orb_outlier.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the orb_outlier subroutine used for removing outliers
! (such as zeros entry values) from an orbit array 
! 
! Subroutines contained within the module:
! - orb_outlier.f03
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	5 May 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE orb_outlier (orb_0, outlier_value, Noutliers, orb_1, orb_out)


! ----------------------------------------------------------------------
! SUBROUTINE: orb_outlier.f03
! ----------------------------------------------------------------------
! Purpose:
!  Remove outliers epochs of an input orbit array e.g. Zero entry values 
! ----------------------------------------------------------------------
! Input arguments:
! - orb_0:  		Input orbit array 
! - outlier_value:  Outlier value e.g. zero 
!
! Output arguments:
! - orb_1:  		Output orbit array after removing the outliers e.g. zero values epochs
! - orb_out:  	Orbit array of the epochs of the outliers values 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	5 May 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: orb_0
      REAL (KIND = prec_q), INTENT(IN) :: outlier_value
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orb_1
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orb_out
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs_0, Noutliers, Nelements, sz1, sz2, Nepochs_1
	  INTEGER (KIND = prec_int8) :: i, j, i_outliers, i_pass
	  INTEGER (KIND = prec_int2) :: epoch_outlier
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
	  REAL (KIND = prec_d) :: delta 
! ----------------------------------------------------------------------

 
sz1 = size(orb_0, DIM = 1)
sz2 = size(orb_0, DIM = 2)
Nepochs_0 = sz1
Nelements = sz2

! ----------------------------------------------------------------------
! Detect number of epochs with outliers 
! ----------------------------------------------------------------------
Noutliers = 0
i = 0
j = 0
DO i = 1 , Nepochs_0
	epoch_outlier = 0 
	delta = 999999999.0D0
	DO j = 3 , Nelements	
		delta = ABS( orb_0(i,j) ) - ABS(outlier_value)
		IF ( ABS(delta) < 1.0D-7 ) THEN
			epoch_outlier = 1
		END IF
!IF (i == 1) THEN
!print *,"i, j, delta, orb_0(i,j), epoch_outlier", i, j, delta, orb_0(i,j), epoch_outlier 
!END IF
	END DO
	IF ( epoch_outlier == 1 ) THEN
		Noutliers = Noutliers + 1
	END IF
End DO
! ----------------------------------------------------------------------

! Allocatable arrays
Nepochs_1 = Nepochs_0 - Noutliers
ALLOCATE (orb_1      (Nepochs_1, Nelements), STAT = AllocateStatus)
ALLOCATE (orb_out(Noutliers, Nelements), STAT = AllocateStatus)

!print *,"Noutliers, Nepochs_1, Nepochs_0", Noutliers, Nepochs_1, Nepochs_0

! Special case with no outliers
IF (Noutliers == 0) THEN
orb_1 = orb_0
END IF

! ----------------------------------------------------------------------
! Orbit array after removing outliers
! ----------------------------------------------------------------------
IF (Noutliers > 0) THEN

i = 0
j = 0
i_pass = 0
i_outliers = 0
DO i = 1 , Nepochs_0
	epoch_outlier = 0 
	delta = 999999999.0D0
	DO j = 3 , Nelements	
		delta = ABS( orb_0(i,j) ) - ABS(outlier_value)
		IF ( ABS(delta) < 1.0D-7 ) THEN
			epoch_outlier = 1
		END IF
	END DO
	
	IF ( epoch_outlier == 0 ) THEN
		i_pass = i_pass + 1
		DO j = 1 , Nelements
			orb_1 (i_pass , j) = orb_0(i,j)
		END DO
	ELSE IF ( epoch_outlier == 1 ) THEN
		i_outliers = i_outliers + 1
		DO j = 1 , Nelements
			orb_out (i_outliers , j) = orb_0(i,j)
		END DO
	END IF
End DO

END IF
! ----------------------------------------------------------------------


End subroutine

END Module
