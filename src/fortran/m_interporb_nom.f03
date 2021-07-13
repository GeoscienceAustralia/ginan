MODULE m_interporb_nom


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


SUBROUTINE interp_orb_nom (fname_sp3, PRN, interv_in, NPint, orbint)


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
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	5 August 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 14 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 adavantages in dynamic memory allocation
! - Dr. Thomas Papanikolaou, 4 May 2020:
! 	Renamed as m_interporb_nom.f03 in the frame of upgrading the orbit 
!	interpolation algorithm for case of orbits with outliers (zero values)
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_sp3
      USE m_lagrange
      !USE mdl_arr
      !USE mdl_write
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
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: Y_interp 
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: clock_matrix

 
	  ! Read IGS sp3 orbit data file (position only): orbsp3 
      Call sp3 (fname_sp3, PRN, orbsp3, clock_matrix)
	  sz1 = size(orbsp3, DIM = 1)

! ----------------------------------------------------------------------
! Interpolation epoch limits (24h orbit arc or less):
! 1. Interpolation is applied only within the data points (15 min less than 24h arc):  Set Nlimit to 1
! 2. Interpolation is applied also after the last data point's epoch (23h 45min 00sec) in order to cover a 24h arc:  Set Nlimit to 0
Nlimit = 1
! default initial value?
data_rate = 1
! ----------------------------------------------------------------------
	  
	  ! Data rate of sp3 (in sec)
      DO i = 1 , sz1-1
		if ( INT(orbsp3(i+1,1)) - INT(orbsp3(i,1)) == 0 ) then
			data_rate = INT( orbsp3(2,2) - orbsp3(1,2) )
			EXIT
		end if
	  End Do
	  
! ----------------------------------------------------------------------
IF (interv_in < 0 .OR. interv_in > data_rate) THEN
	interv = data_rate
ELSE
	interv = interv_in
END IF
!print *,"interv", interv
! ----------------------------------------------------------------------
	  
      ! Number of points to be interpolated during the span between two epochs of the sp3 file (15min, 5min)
	  Nepoch_int = data_rate / interv - 1
	  	  
      ! Number of epochs of interpolated orbit (excluding extrapolation at the last point i.e. the start of the next day's sp3: 00h 00min 00sec)
      !Norb_int = (data_rate / interv) * (sz1-1) + 1 
      Norb_int = (data_rate / interv) * (sz1 - Nlimit) + Nlimit
	  !Nlimit=1
      !Norb_int = 1 + (data_rate / interv) * (sz1 - Nlimit)
	  !Nlimit=0	  
      !Norb_int = 1 + (data_rate / interv) * (sz1 - Nlimit) - 1
	  
! Allocatable arrays for Lagrange Interpolation (declared in mdl_arr.f90)
      ALLOCATE (X_interp(NPint), STAT = AllocateStatus)
      ALLOCATE (Y_interp(NPint), STAT = AllocateStatus)
! Allocatable arrays  for the interpolated orbit array
      ALLOCATE (orbint(Norb_int, 8) , STAT = AllocateStatus)
	  	  
! ----------------------------------------------------------------------
! Number of data points (used for the interpolation) before and after of each epoch of sp3 file	  
      NPint_1 = INT(NPint/2) 
      NPint_2 = NPint - NPint_1 
	  
	  epoch_i = 0
      DO i = 1 , sz1
		epoch_i  = epoch_i + 1
		MJD_i    = orbsp3(i,1)
		SecDay_i = orbsp3(i,2)		
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

        ! Time array of the data points used for the interpolation: X_interp
!		ti = 1
!		X_interp(1) = orbsp3(i1, 2)
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
		orbint(epoch_i, 1:5) = orbsp3(i, 1:5)   							
        X = orbsp3(i,3)
		Y = orbsp3(i,4)
		Z = orbsp3(i,5)
		
		! Velocity Vector: computation via Lagrange interpolation
		tint = SecDay_i
		
		! Vx
		Y_interp = orbsp3(i1:i2, 3)		
		!Call interp_lag(tint-1, Xint_1) 
		Call interp_lag(tint-1, X_interp, Y_interp, Xint_1) 
        Vx = X - Xint_1 
		! Vy
		Y_interp = orbsp3(i1:i2, 4)
		!Call interp_lag(tint-1, Yint_1) 
		Call interp_lag(tint-1, X_interp, Y_interp, Yint_1) 
        Vy = Y - Yint_1
		! Vz
		Y_interp = orbsp3(i1:i2, 5)
		!Call interp_lag(tint-1, Zint_1) 
		Call interp_lag(tint-1, X_interp, Y_interp, Zint_1) 
        Vz = Z - Zint_1

		! Write Velocity to array orbint
		orbint(epoch_i, 6) = Vx
		orbint(epoch_i, 7) = Vy
		orbint(epoch_i, 8) = Vz
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Lagrange Interpolation applied for the new points 
! ----------------------------------------------------------------------
! When Nlimit=0 & i=sz1 : Extrapolation is performed after the last data point of the sp3 file (for completing 24h arc)
If (i <= sz1 - Nlimit) then
		DO j = 1 , Nepoch_int				
			tint = j * interv * 1.D0 + SecDay_i
			
		!DO j = t(1) , t(NPint) , interv
			!tint = j * interv + orbsp3(i,2)
		
		if (1 < 0) then
		!if (i == sz1 .and. j == Nepoch_int) then
		! Avoid computing the last point since this is the start point of the next day's sp3 file
		PRINT *,"i,j", i,j
		PRINT *,"orbint(epoch_i,:)", orbint(epoch_i,:)
		PRINT *,"orbint(epoch_i-1,:)", orbint(epoch_i-1,:)
		EXIT
		end if
		
		! Position and Velocity Vectors
			! X
			Y_interp = orbsp3(i1:i2, 3)		
			!Call interp_lag(tint, Xint)
			Call interp_lag(tint, X_interp, Y_interp, Xint) 
			! Vx
			!Call interp_lag(tint-1, Xint_1) 
			Call interp_lag(tint-1, X_interp, Y_interp, Xint_1) 
            !Vx = Xint_1 - Xint 
            Vx = Xint - Xint_1 
			
			! Y
			Y_interp = orbsp3(i1:i2, 4)
			!Call interp_lag(tint, Yint) 
			Call interp_lag(tint, X_interp, Y_interp, Yint) 
			! Vy
			!Call interp_lag(tint-1, Yint_1) 
			Call interp_lag(tint-1, X_interp, Y_interp, Yint_1) 
            !Vy = Yint_1 - Yint
            Vy = Yint - Yint_1
			
			! Z
			Y_interp = orbsp3(i1:i2, 5)
			!Call interp_lag(tint, Zint) 
			Call interp_lag(tint, X_interp, Y_interp, Zint) 
			! Vz
			!Call interp_lag(tint-1, Zint_1) 
			Call interp_lag(tint-1, X_interp, Y_interp, Zint_1) 
            !Vz = Zint_1 - Zint
            Vz = Zint - Zint_1

			
! Next epoch			
			epoch_i = epoch_i + 1	
			
! MJD epoch
			MJD_tint = INT(MJD_i) * 1.D0 + tint / 86400.D0
			
! Write interpolated state vector in array
			! Time arguments at interpolation epoch
			orbint(epoch_i, 1) = MJD_tint
			
			If (tint > 86400.D0) then
			   tint = tint - 86400.D0
			End If 
			
			orbint(epoch_i, 2) = tint
			
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
