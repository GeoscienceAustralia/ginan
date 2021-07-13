MODULE mdl_arr


! ----------------------------------------------------------------------
! MODULE: mdl_arr.f90
! ----------------------------------------------------------------------
! Purpose:
!  Module for dyanmic allocatable arrays for general use
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia              May 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      IMPLICIT NONE
      SAVE 			

! Allocatable Arrays	  
      REAL (KIND = prec_q), DIMENSION(:)  , ALLOCATABLE :: array1 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: array2
	  
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: R1 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: R2 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: R3 

      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: matrix1 
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: matrix2 
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: matrix3 

      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orb1 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orb2 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orb3 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orb4 
	  
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: Y_interp 
	  
END