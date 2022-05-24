SUBROUTINE orb_sp3 (fname_sp3, PRN, fname_orb)


! ----------------------------------------------------------------------
! SUBROUTINE: orb_sp3.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 orbit;
!  Position vector is obtained by sp3 file 
!  Velocity vector is approximated by the position differences per epoch 
! ----------------------------------------------------------------------
! Input arguments:
! - fname_sp3:  	Orbit file in sp3 format which contains only position vector 
! - PRN:			Satellite PRN number
!
! Output arguments:
!
! Output arguments (via module):
! - orb1:			Orbit array based on the input sp3 file including only the position vector
! - orb2:   		Orbit array including position and velocity vetors per epoch (allocatable array)
! - mdl_arr.f90: 	Module used for the declaration of these variables as allocatable arrays
! ----------------------------------------------------------------------
! Note 1:
!  The input orbit file (sp3) is considered to include only position vector
!  of the GNSS satellites as this is the IGS standard products
! ----------------------------------------------------------------------
! Dr. Thomas D. Papanikolaou, Geoscience Australia         5 August 2016
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_arr
      USE mdl_write
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=300), INTENT(IN) :: fname_sp3, fname_orb
	  !INTEGER (KIND = 4) :: PRN
      CHARACTER (LEN=3), INTENT(IN) :: PRN
! OUT

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, sz1, sz2 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, ti
	  REAL (KIND = prec_d) :: dt, r_t1(3), r_t2(3), Vi(3)
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
	  ! Read sp3 orbit data file (position only)
	  ! write to 'orb1' allocatable array
      Call sp3 (fname_sp3, PRN)
	  sz1 = size(orb1, DIM = 1)
	  sz2 = size(orb1, DIM = 2)
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
	  ! Velocity approximation based on the r differences per epoch 
	  
	  ! orb2: write to 'orb2' allocatable array
      ALLOCATE ( orb2(sz1-1, 8) , STAT = AllocateStatus)
	  
	  epoch_i = 0
      DO epoch_i = 1 , sz1 - 1
	  
         dt = orb1(epoch_i+1 ,2) - orb1(epoch_i ,2)
		 if (dt < 0.0D0) then
			dt = dt + 86400.0D0
		 end if
		 
		 r_t2 = (/ orb1(epoch_i+1,3) , orb1(epoch_i+1,4) , orb1(epoch_i+1,5) /) 
		 r_t1 = (/ orb1(epoch_i  ,3) , orb1(epoch_i  ,4) , orb1(epoch_i  ,5) /) 
		 
		 !V_df(1) = ( orb1(epoch_i+1 , 3) - orb1(epoch_i , 3) ) / dt
		 Vi = (r_t2 - r_t1 ) / dt
		 
	     orb2(epoch_i , 1:5) = orb1(epoch_i , 1:5)
		 orb2(epoch_i , 6:8) = Vi 
		 
if (1<0) then
		 print *,'r_t1',r_t1
		 print *,'r_t2',r_t2
		 print *,'dt',dt	 
		 print *,'Vi',Vi
		 print *," "
		 print *,"orb1",orb1(epoch_i , :)
		 print *,'orb2',orb2(epoch_i , :)
end if

	  End Do
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Write Orbit to output files
	  sz1 = size(orb2, DIM = 1)
	  sz2 = size(orb2, DIM = 2)
      ALLOCATE (wrtArray(sz1,sz2), STAT = AllocateStatus)
      fname_write = fname_orb
      wrtArray = orb2
      CALL write_array (fname_write)  
      DEALLOCATE (wrtArray, STAT = DeAllocateStatus)
	  
	  sz1 = size(orb1, DIM = 1)
	  sz2 = size(orb1, DIM = 2)
      ALLOCATE (wrtArray(sz1,sz2), STAT = AllocateStatus)
      fname_write = 'orb_sp3.out'
      wrtArray = orb1
      CALL write_array (fname_write)  
      DEALLOCATE (wrtArray, STAT = DeAllocateStatus)
! ----------------------------------------------------------------------



End subroutine

