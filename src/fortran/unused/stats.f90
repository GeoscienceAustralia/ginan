SUBROUTINE stats (stat_q)

! ----------------------------------------------------------------------
! SUBROUTINE: stats.f90
! ----------------------------------------------------------------------
! Purpose:
!  Computation of statistical qunatities based on a provided data series
!  Root Mean Square (RMS), Maximum, Minimum, Mean, Standard deviation 
! ----------------------------------------------------------------------
! Input arguments:
! - array1:         Input allocatable array
! - mdl_array:		Module for the input/output allocatable arrays    
!
! Output arguments:
! - stat_q:			Statistics quantities: RMS, Max, Min, Mean, sdev
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia              May 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_arr
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN

! OUT
      REAL (KIND = prec_d) :: stat_q(5)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: min_val, max_val, RMS, Mean, sdev
      REAL (KIND = prec_d) :: sum_dx2, sum_dx, sum_dxx
      INTEGER (KIND = prec_int4) :: sz1, sz2, i
! ----------------------------------------------------------------------


![size_dx size2] = size(dx);
      sz1 = SIZE (array1,DIM=1)
      !sz2 = SIZE (array1,DIM=2)

      min_val = array1(1) 	  
      max_val = array1(1) 	  
      sum_dx2 = 0.0D0
      sum_dx = 0.0D0
	  
      DO i = 1 , sz1	  
	     sum_dx2 = sum_dx2 + array1(i)**2
	     sum_dx = sum_dx + array1(i)
         if ( array1(i) < min_val ) then
		     min_val = array1(i)
		 end if
		 if ( array1(i) > max_val ) then
		     max_val = array1(i)
		 end if
      END DO


! RMS
      RMS = sqrt(sum_dx2 / sz1)	  

! Mean
      Mean = sum_dx / sz1

! ----------------------------------------------------------------------
! Standard Deviation
      sum_dxx = 0.0D0
      DO i = 1 , sz1	  
	     sum_dxx = sum_dxx + (array1(i) - Mean)**2
      END DO

      sdev = sqrt(sum_dxx / sz1)	  
      
      sdev = sqrt(sum_dxx / (sz1 - 1) )	  
! ----------------------------------------------------------------------
  
! ----------------------------------------------------------------------
      stat_q(1) = RMS	  
      stat_q(2) = max_val	  
      stat_q(3) = min_val
      stat_q(4) = Mean
      stat_q(5) = sdev
! ----------------------------------------------------------------------
 
	 
END
