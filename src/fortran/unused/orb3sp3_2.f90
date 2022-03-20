SUBROUTINE orb3sp3_2 (fname_orb_0, fname_orb_1, fname_orb_2, PRN, interv, NPint, fname_orbint)


! ----------------------------------------------------------------------
! SUBROUTINE: orb3sp3_2.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 orbit interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - fname_sp3:  	Orbit file in sp3 format which contains only position vector 
! - PRN:			Satellite PRN number
! - NPint:			Number of data points to be used for interpolation (defines the order of the polynomial)
!
! Output arguments:
!
! Output arguments (via module):
! - orb1:			Orbit array based on the input sp3 file including only the position vector
! - orb2:   		Interpolated orbit including position and velocity vetors per epoch (allocatable array)
! - mdl_arr.f90: 	Module used for the declaration of these variables as allocatable arrays
! ----------------------------------------------------------------------
! Note 1:
!  The input orbit file (sp3) is considered to include only position vector
!  of the GNSS satellites as this is the IGS standard products
!  During the orbit interpolation, based on Lagrange polynomials, position 
!  and velocity vectors for the selected satellite (PRN) are computed and
!  provided in the output files.  
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
      CHARACTER (LEN=150), INTENT(IN) :: fname_orb_0, fname_orb_1, fname_orb_2, fname_orbint
      INTEGER (KIND = prec_int8), INTENT(IN) :: interv, NPint
!	  INTEGER (KIND = 4) :: PRN
      CHARACTER (LEN=3), INTENT(IN) :: PRN
! OUT

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, sz1, sz2, i
      INTEGER (KIND = prec_int4) :: Nsp3 
      INTEGER (KIND = prec_int8) :: data_rate, Nepoch_int, Norb_int, NPint_1, NPint_2 
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbN
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=150) :: fname_write
! ----------------------------------------------------------------------


! orb1 (mdl_arr.f90): Store the orbit data (position only) from the all of the input sp3 files

! Number of sp3 files
Nsp3 = 3

      Do i = 1 , Nsp3
	    if (i==1) then 
			!Call interp_orb(fname_orb_0, PRN, interv, NPint, fname_orbint)
			Call sp3(fname_orb_0, PRN)
	    else if (i==2) then 
			!Call interp_orb(fname_orb_1, PRN, interv, NPint, fname_orbint)
			Call sp3(fname_orb_1, PRN)
	    else if (i==3) then 
			!Call interp_orb(fname_orb_2, PRN, interv, NPint, fname_orbint)
			Call sp3(fname_orb_2, PRN)
		end if
		
		if (i == 1) then
			sz1 = size(orb1, DIM = 1)
			sz2 = size(orb1, DIM = 2)
			! Allocatable array  for the long-arc orbit array
			ALLOCATE (orbN(Nsp3 * sz1, sz2) , STAT = AllocateStatus)
		end if
		
		orbN ( (i-1)*sz1+1 : i*sz1 , :) = orb1(:,:) 
				
		DEALLOCATE (orb1, STAT = DeAllocateStatus)
		
	  End Do
	  
	  
	  sz1 = size(orbN, DIM = 1)
	  sz2 = size(orbN, DIM = 2)
	  
	  ALLOCATE (orb1(sz1,sz2), STAT = AllocateStatus)	  	  
	  orb1 = orbN
	  
	  ! orb2: Final array for the interpolated orbit
	  ! Lagrange Interpolation for the long orbit arc
	  CALL interp_orb2(interv, NPint, fname_orbint)
	  
	  
! ----------------------------------------------------------------------
	  ! long orbit arc: Write array to file  
      ALLOCATE (wrtArray( sz1, sz2), STAT = AllocateStatus)
      fname_write = 'orb_long.out'
      wrtArray = orbN
      CALL write_array (fname_write)  
	  DEALLOCATE (wrtArray, STAT = DeAllocateStatus)

	  DEALLOCATE (orbN, STAT = DeAllocateStatus) 	
! ----------------------------------------------------------------------




End subroutine

