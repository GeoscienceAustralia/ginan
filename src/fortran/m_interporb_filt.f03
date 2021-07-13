MODULE m_interporb_filt


! ----------------------------------------------------------------------
! MODULE: m_interporb_filt.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the interp_orb_filt subroutine used for orbit interpolation
! 
! Subroutines contained within the module:
! - interp_orb.f03
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	5 May 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE interp_orb_filt (ORB_matrix, ORB_filt, interv_in, NPint_in, orbint)


! ----------------------------------------------------------------------
! SUBROUTINE: interp_orb_filt.f03
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 orbit interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - ORB_matrix:  	Input orbit matrix (including position vector per epoch)
! - interv_in: 		Interval of the interpolation epochs (in seconds)
! - NPint:			Number of data points to be used for interpolation (defines the order of the Lagrange polynomial)
!
! Output arguments:
! - orbint:   		Interpolated orbit including position and velocity vetors per epoch (allocatable array)
! ----------------------------------------------------------------------
! Note 1:
!  The input orbit matrix is considered to include satellite position vector 
!  only as this is the IGS standard product. 
!  During the orbit interpolation, based on Lagrange polynomials, position 
!  and velocity vectors are computed 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	5 May 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      !USE m_sp3
      USE m_lagrange
      !USE mdl_arr
      !USE mdl_write
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      !CHARACTER (LEN=300), INTENT(IN) :: fname_sp3
      !CHARACTER (LEN=300), INTENT(IN) :: fname_orbint
      INTEGER (KIND = prec_int8), INTENT(IN) :: interv_in, NPint_in
	  !INTEGER (KIND = 4) :: PRN
      !CHARACTER (LEN=3), INTENT(IN) :: PRN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: ORB_matrix, ORB_filt
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbint
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, sz1, sz2, interv 
      INTEGER (KIND = prec_int8) :: Nepochs_0, Nepochs_filt, ORB_filt_epoch, i_filt, NPint
      INTEGER (KIND = prec_int8) :: data_rate, Nepoch_int, Norb_int, NPint_1, NPint_2, Nlimit 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, ti
	  REAL (KIND = prec_d) :: SecDay_i, MJD_i 
	  REAL (KIND = prec_d) :: MJD_tint, tint, Xint, Xint_1, Yint, Yint_1, Zint, Zint_1
	  REAL (KIND = prec_d) :: X, Y, Z, Vx, Vy, Vz
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: Y_interp 
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: clock_matrix
	  REAL (KIND = prec_d) :: delta_min, delta_epoch

 
sz1 = size(ORB_matrix, DIM = 1)
Nepochs_0 = sz1

sz1 = size(ORB_filt, DIM = 1)
Nepochs_filt = sz1

! Test of numerical interpolation order with available data points
IF (NPint_in > Nepochs_filt) THEN
! Reduce the interpolator order in case that input data points are less than the order value
	NPint = Nepochs_filt
ELSE
	NPint = NPint_in	
END IF

! ----------------------------------------------------------------------
! Interpolation epoch limits (24h orbit arc or less):
! 1. Interpolation is applied only within the data points (15 min less than 24h arc):  Set Nlimit to 1
! 2. Interpolation is applied also after the last data point's epoch (23h 45min 00sec) in order to cover a 24h arc:  Set Nlimit to 0
Nlimit = 1
! default initial value?
data_rate = 1
! ----------------------------------------------------------------------
	  
! ----------------------------------------------------------------------
	  ! Data rate of intial orbit matrix (in sec)
      DO i = 1 , Nepochs_0-1
		if ( INT(ORB_matrix(i+1,1)) - INT(ORB_matrix(i,1)) == 0 ) then
			data_rate = INT( ORB_matrix(2,2) - ORB_matrix(1,2) )
			EXIT
		end if
	  End Do
! ----------------------------------------------------------------------
	  
! ----------------------------------------------------------------------
IF (interv_in < 0 .OR. interv_in > data_rate) THEN
	interv = data_rate
ELSE
	interv = interv_in
END IF
! ----------------------------------------------------------------------
	  
      ! Number of points to be interpolated during the span between two epochs of the sp3 file (15min, 5min)
	  Nepoch_int = data_rate / interv - 1
	  	  
      ! Number of epochs of interpolated orbit (excluding extrapolation at the last point i.e. the start of the next day's sp3: 00h 00min 00sec)
      Norb_int = (data_rate / interv) * (Nepochs_0 - Nlimit) + Nlimit
	  
! Allocatable arrays for Lagrange Interpolation (declared in mdl_arr.f90)
      ALLOCATE (X_interp(NPint), STAT = AllocateStatus)
      ALLOCATE (Y_interp(NPint), STAT = AllocateStatus)
! Allocatable arrays  for the interpolated orbit array
      ALLOCATE (orbint(Norb_int, 8) , STAT = AllocateStatus)
	  	  
! Number of data points (used for the interpolation) before and after of each epoch of sp3 file	  
      NPint_1 = INT(NPint/2) 
      NPint_2 = NPint - NPint_1 
	  
! ----------------------------------------------------------------------
! Orbit interpolation loop
	  epoch_i = 0
      DO i = 1 , Nepochs_0
		epoch_i  = epoch_i + 1
		MJD_i    = ORB_matrix(i,1)
		SecDay_i = ORB_matrix(i,2)

! ----------------------------------------------------------------------
! Epoch of orbit matrix filtered (without outliers) closest to interpolation epoch 
delta_min = MJD_i - ORB_filt(1,1)
ORB_filt_epoch = 1
DO i_filt = 1 , Nepochs_filt
	delta_epoch = MJD_i - ORB_filt(i_filt,1)
	IF ( ABS(delta_epoch) < ABS(delta_min) ) THEN
		delta_min = delta_epoch
		ORB_filt_epoch = i_filt
	END IF
END DO
! ----------------------------------------------------------------------
		
! ----------------------------------------------------------------------
! Form arrays with data points to be used in the interpolation		
		!if ( i < NPint_1 ) then
		if (Nepochs_filt == NPint) then
		! Case: Available data points are equal to the interpolation order	
			i1 = 1
			i2 = Nepochs_filt		
		else if ( ORB_filt_epoch < NPint_1 ) then
			i1 = ORB_filt_epoch
			i2 = ORB_filt_epoch + (NPint - 1)
		else if ( Nepochs_filt - ORB_filt_epoch < NPint_2 ) then
			i2 = Nepochs_filt
			i1 = i2 - (NPint-1)
		else 
			i1 = ORB_filt_epoch - (NPint_1 - 1)
			i2 = ORB_filt_epoch + NPint_2
		end if
		!print *,"NPint_1, NPint_2, NPint", NPint_1, NPint_2, NPint
		!print *,"i, epoch_i             ", i, epoch_i
		!print *,"ORB_filt_epoch, i1, i2 ", ORB_filt_epoch, i1, i2
! ----------------------------------------------------------------------
! Time array of the data points used for the interpolation: X_interp
		ti = 0		
		Do j = i1 , i2
		ti = ti + 1		
			X_interp(ti) = ORB_filt(j , 2) + ( INT(ORB_filt(j,1)) - INT(ORB_filt(i1,1)) ) * 86400.0D0
			!X_interp(ti) = ORB_filt(j , 1) 
		End Do
	    j = 0		
! ----------------------------------------------------------------------!
!print *,"X_interp", X_interp
!STOP 

! ----------------------------------------------------------------------
! Lagrange interpolation
! ----------------------------------------------------------------------
		! Lagrange interpolation at interpolation epoch
		tint = SecDay_i

		! Position and Velocity Vectors
		! X
		Y_interp = ORB_filt(i1:i2, 3)		
		Call interp_lag(tint, X_interp, Y_interp, Xint) 
		! Vx
		Call interp_lag(tint-1, X_interp, Y_interp, Xint_1) 
        !Vx = Xint_1 - Xint 
        Vx = Xint - Xint_1 

		! Y
		Y_interp = ORB_filt(i1:i2, 4)
		Call interp_lag(tint, X_interp, Y_interp, Yint) 
		! Vy
		Call interp_lag(tint-1, X_interp, Y_interp, Yint_1) 
        !Vy = Yint_1 - Yint
        Vy = Yint - Yint_1
			
		! Z
		Y_interp = ORB_filt(i1:i2, 5)
		Call interp_lag(tint, X_interp, Y_interp, Zint) 
		! Vz
		Call interp_lag(tint-1, X_interp, Y_interp, Zint_1) 
        !Vz = Zint_1 - Zint
        Vz = Zint - Zint_1
		
! Write interpolated state vector to the matrix 
		! Time arguments
		orbint(epoch_i, 1) = ORB_matrix(i,1)   							
		orbint(epoch_i, 2) = ORB_matrix(i,2)   							
		! Position vector (interpolated)
		orbint(epoch_i, 3) = Xint
		orbint(epoch_i, 4) = Yint
		orbint(epoch_i, 5) = Zint			
		! Velocity vector (interpolated)		
		orbint(epoch_i, 6) = Vx
		orbint(epoch_i, 7) = Vy
		orbint(epoch_i, 8) = Vz		
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lagrange Interpolation applied for the new points 
! ----------------------------------------------------------------------
! When Nlimit=0 & i=Nepochs_0 : Extrapolation is performed after the last data point of the sp3 file (for completing 24h arc)
If (i <= Nepochs_0 - Nlimit) then
		DO j = 1 , Nepoch_int				
		! Lagrange interpolation at interpolation epoch
			tint = j * interv * 1.D0 + SecDay_i
					
		if (1 < 0) then
		!if (i == Nepochs_0 .and. j == Nepoch_int) then
		! Avoid computing the last point since this is the start point of the next day's sp3 file
		PRINT *,"i,j", i,j
		PRINT *,"orbint(epoch_i,:)", orbint(epoch_i,:)
		PRINT *,"orbint(epoch_i-1,:)", orbint(epoch_i-1,:)
		EXIT
		end if
		
		! Position and Velocity Vectors
			! X
			Y_interp = ORB_filt(i1:i2, 3)		
			Call interp_lag(tint, X_interp, Y_interp, Xint) 
			! Vx
			Call interp_lag(tint-1, X_interp, Y_interp, Xint_1) 
            !Vx = Xint_1 - Xint 
            Vx = Xint - Xint_1 
			
			! Y
			Y_interp = ORB_filt(i1:i2, 4)
			Call interp_lag(tint, X_interp, Y_interp, Yint) 
			! Vy
			Call interp_lag(tint-1, X_interp, Y_interp, Yint_1) 
            !Vy = Yint_1 - Yint
            Vy = Yint - Yint_1
			
			! Z
			Y_interp = ORB_filt(i1:i2, 5)
			Call interp_lag(tint, X_interp, Y_interp, Zint) 
			! Vz
			Call interp_lag(tint-1, X_interp, Y_interp, Zint_1) 
            !Vz = Zint_1 - Zint
            Vz = Zint - Zint_1

! Write interpolated values to the output matrix			
! Next epoch			
			epoch_i = epoch_i + 1				
! MJD epoch
			MJD_tint = INT(MJD_i) * 1.D0 + tint / 86400.D0			
			! Write Time arguments of interpolation epoch to the matrix
			orbint(epoch_i, 1) = MJD_tint
			If (tint > 86400.D0) then
			   tint = tint - 86400.D0
			End If 			
			orbint(epoch_i, 2) = tint			
			! Write interpolated state vector to the matrix 
			! Position vector (interpolated)
			orbint(epoch_i, 3) = Xint
			orbint(epoch_i, 4) = Yint
			orbint(epoch_i, 5) = Zint
			! Velocity vector (interpolated)		
			orbint(epoch_i, 6) = Vx
			orbint(epoch_i, 7) = Vy
			orbint(epoch_i, 8) = Vz		
		End DO
End IF
! ----------------------------------------------------------------------
      End Do		  
	  Deallocate (X_interp)	  
	  Deallocate (Y_interp)	  
End subroutine

END Module
