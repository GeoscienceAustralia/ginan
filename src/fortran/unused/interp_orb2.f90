SUBROUTINE interp_orb2(interv, NPint, fname_orbint)


! ----------------------------------------------------------------------
! SUBROUTINE: interp_orb2.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 orbit interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - interv: 		Interval of the interpolation epochs (in seconds)
! - NPint:			Number of data points to be used for interpolation (defines the order of the polynomial)
! - fname_orbint:	Output file name for writing interpolated orbit array  
!
! Input arguments (via module):
! - orb1:			Orbit array including only the position vector (based on multiple sp3 files)
! - mdl_arr.f90: 	Module used for the declaration of these variables as allocatable arrays
!
! Output arguments (via module):
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
      CHARACTER (LEN=150), INTENT(IN) :: fname_orbint
      INTEGER (KIND = prec_int8), INTENT(IN) :: interv, NPint
!	  INTEGER (KIND = 4) :: PRN
! OUT

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, sz1, sz2 
      INTEGER (KIND = prec_int8) :: data_rate, Nepoch_int, Norb_int, NPint_1, NPint_2 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, ti
	  REAL (KIND = prec_d) :: SecDay_i, MJD_i 
	  REAL (KIND = prec_d) :: MJD_tint, tint, Xint, Xint_1, Yint, Yint_1, Zint, Zint_1
	  REAL (KIND = prec_d) :: X, Y, Z, Vx, Vy, Vz
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
! ----------------------------------------------------------------------


 	  
	  ! orb1 via module mdl_arr.f90: Orbit array based on all of the input sp3 files
	  sz1 = size(orb1, DIM = 1)
	  
	  ! Data rate of sp3 (in sec)
      DO i = 1 , sz1-1
		if ( INT(orb1(i+1,1)) - INT(orb1(i,1)) == 0 ) then
			data_rate = INT( orb1(2,2) - orb1(1,2) )
			EXIT
		end if
	  End Do
	  
	  
      ! Number of points to be interpolated per epoch of the sp3 orbit file (15min, 5min)
	  Nepoch_int = data_rate / interv - 1	
	  
	  
      ! Number of epochs of interpolated orbit
      Norb_int = (data_rate / interv) * (sz1-1) + 1 
	  
	  
! Allocatable arrays for Lagrange Interpolation (declared in mdl_arr.f90)
      ALLOCATE (X_interp(NPint), STAT = AllocateStatus)
      ALLOCATE (Y_interp(NPint), STAT = AllocateStatus)


! Allocatable arrays  for the interpolated orbit array
      ALLOCATE (orb2(Norb_int, 8) , STAT = AllocateStatus)
	  
	  
! ----------------------------------------------------------------------
! Number of data points (used for the interpolation) before and after of each epoch of sp3 file	  
      NPint_1 = INT(NPint/2) 
      NPint_2 = NPint - NPint_1 
	  
	  epoch_i = 0
      DO i = 1 , sz1
		epoch_i  = epoch_i + 1
		MJD_i    = orb1(i,1)
		SecDay_i = orb1(i,2)
		
! ----------------------------------------------------------------------
! Form arrays with data points to be used in the interpolation		
		if ( i < NPint_1 ) then
			i1 = i
			i2 = i + (NPint - 1)
			!print *,"sz1, i, epoch_i, i1, i2", sz1, i, epoch_i, i1, i2
		else if ( sz1 - i < NPint_2 ) then
			i2 = sz1
			i1 = i2 - (NPint-1)
			!print *,"sz1, i, epoch_i, i1, i2", sz1, i, epoch_i, i1, i2
		else 
			i1 = i - (NPint_1 - 1)
			i2 = i + NPint_2
			!print *,"i,epoch_i,i1,i2", i, epoch_i, i1, i2
		end if
	

! ----------------------------------------------------------------------
! Time array of the data points used for the interpolation: X_interp
!		ti = 1
!		X_interp(1) = orb1(i1, 2)
!		Do ti = 2 , NPint 
!			X_interp(ti) = X_interp(1) + (ti - 1) * data_rate * 1.D0			
!		End Do

		ti = 0		
		Do j = i1 , i2
		ti = ti + 1		
			X_interp(ti) = SecDay_i + (j-i) * data_rate * 1.D0  !(ti - 1) * data_rate * 1.D0			
		End Do
	    j = 0
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Data Point: Velocity computation based on interpolation
! ----------------------------------------------------------------------
		! Position Vector: value from the IGS sp3 file
		orb2(epoch_i, 1:5) = orb1(i, 1:5)   							
        X = orb1(i,3)
		Y = orb1(i,4)
		Z = orb1(i,5)
		
		! Velocity Vector: computation via Lagrange interpolation
		tint = SecDay_i
		
		! Vx
		Y_interp = orb1(i1:i2, 3)		
		Call interp_lag(tint-1, Xint_1) 
        Vx = X - Xint_1 
		! Vy
		Y_interp = orb1(i1:i2, 4)
		Call interp_lag(tint-1, Yint_1) 
        Vy = Y - Yint_1
		! Vz
		Y_interp = orb1(i1:i2, 5)
		Call interp_lag(tint-1, Zint_1) 
        Vz = Z - Zint_1

		! Write Velocity to array orb2
		orb2(epoch_i, 6) = Vx
		orb2(epoch_i, 7) = Vy
		orb2(epoch_i, 8) = Vz
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Lagrange Interpolation applied for the new points 
! ----------------------------------------------------------------------
If (i < sz1) then
		DO j = 1 , Nepoch_int				
			tint = j * interv * 1.D0 + SecDay_i
			
		!DO j = t(1) , t(NPint) , interv
			!tint = j * interv + orb1(i,2)
		
			! Position and Velocity Vectors

			! X
			Y_interp = orb1(i1:i2, 3)		
			Call interp_lag(tint, Xint)
			! Vx
			Call interp_lag(tint-1, Xint_1) 
            !Vx = Xint_1 - Xint 
            Vx = Xint - Xint_1 
			
			! Y
			Y_interp = orb1(i1:i2, 4)
			Call interp_lag(tint, Yint) 
			! Vy
			Call interp_lag(tint-1, Yint_1) 
            !Vy = Yint_1 - Yint
            Vy = Yint - Yint_1
			
			! Z
			Y_interp = orb1(i1:i2, 5)
			Call interp_lag(tint, Zint) 
			! Vz
			Call interp_lag(tint-1, Zint_1) 
            !Vz = Zint_1 - Zint
            Vz = Zint - Zint_1

			
! Next epoch			
			epoch_i = epoch_i + 1	
			
! MJD epoch
			MJD_tint = INT(MJD_i) * 1.D0 + tint / 86400.D0
			
! Write interpolated state vector in array
			! Time arguments at interpolation epoch
			orb2(epoch_i, 1) = MJD_tint
			
			If (tint > 86400.D0) then
			   tint = tint - 86400.D0
			End If 
			
			orb2(epoch_i, 2) = tint
			
			! Position vector (interpolated)
			orb2(epoch_i, 3) = Xint
			orb2(epoch_i, 4) = Yint
			orb2(epoch_i, 5) = Zint
			
			! Velocity vector (interpolated)		
			orb2(epoch_i, 6) = Vx
			orb2(epoch_i, 7) = Vy
			orb2(epoch_i, 8) = Vz		
		End DO
End IF
! ----------------------------------------------------------------------

      End Do		

	  
	  Deallocate (X_interp)	  
	  Deallocate (Y_interp)	  
	  
	  
! Write Orbit after interpolation 
	  sz1 = size(orb2, DIM = 1)
	  sz2 = size(orb2, DIM = 2)
      ALLOCATE (wrtArray(sz1,sz2), STAT = AllocateStatus)
      !fname_write = 'orb_interp.out'
      fname_write = fname_orbint
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

	  

End subroutine

