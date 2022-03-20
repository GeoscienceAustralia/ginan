MODULE mdl_timeconfig

! ---------------------------------------------------------------------------
! Purpose:
!  Parametes in commandline source code 
! ---------------------------------------------------------------------------
! Author :	Dr. Tzupang Tseng, Geoscience Australia
!
! Created:	03-04-2020 
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			
	  
! ----------------------------------------------------------------------
! Input and output file names 
! ----------------------------------------------------------------------
      REAL (KIND = prec_d)       :: MJD_IN
      INTEGER (KIND = prec_int8) :: WEEK_IN, WEEKDAY_IN
      INTEGER :: YEAR_IN, MONTH_IN, DAY_IN, DOY_IN

! ----------------------------------------------------------------------


END
