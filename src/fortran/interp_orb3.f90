SUBROUTINE interp_orb3(mjd, NPint, rint, vint)


! ----------------------------------------------------------------------
! SUBROUTINE: interp_orb3.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 orbit interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - mjd:			Modified Julian Day number in GPS Time (including the fraction of the day)
! - NPint:			Number of data points to be used for interpolation (defines the order of the polynomial)
!
! Input arguments (via module):
! - orb3:			Orbit array in ITRF including only the position vector per epoch
! - orb4:			Orbit array in ICRF including only the position vector per epoch
! - mdl_arr.f90: 	Module used for the declaration of orb4 variable as allocatable array
!
! Output arguments:
! - rint:   		Position vector at the input epoch based on orbit interpolation 
! - vint:   		Velocity vector at the input epoch based on orbit interpolation 
! ----------------------------------------------------------------------
! Dr. Thomas Papanikolaou
! Cooperative Research Centre for Spatial Information, Australia
! Created: 27 June 2017
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
      REAL (KIND = prec_q), INTENT(IN) :: mjd
      INTEGER (KIND = prec_int8), INTENT(IN) :: NPint
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: rint(3), vint(3)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int8) :: NPint_1, NPint_2 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, i0
	  REAL (KIND = prec_d) :: tepoch, torb_1, torb_max  
	  REAL (KIND = prec_d) :: SecDay_i, MJD_i 
	  REAL (KIND = prec_d) :: dmjd, dsec 
	  REAL (KIND = prec_d) :: MJD_tint, tint, Xint, Xint_1, Yint, Yint_1, Zint, Zint_1
	  REAL (KIND = prec_d) :: X, Y, Z, Vx, Vy, Vz
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbint 


	  
	  
! ----------------------------------------------------------------------
! Orbit array to be applied:
! ----------------------------------------------------------------------
! 'orb3' global variable via module mdl_arr.f90 
sz1 = size(orb3, DIM = 1)
sz2 = size(orb3, DIM = 2)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Allocatable array: Orbit to be applied for the data points required for the numerical interpolation
ALLOCATE (orbint(sz1, sz2) , STAT = AllocateStatus)
! ----------------------------------------------------------------------

orbint = orb3



sz1 = size(orbint, DIM = 1)
sz2 = size(orbint, DIM = 2)


! ----------------------------------------------------------------------
! Conditions epochs
tepoch = mjd
torb_1 = orbint(1,1)
torb_max = orbint(sz1,1)
! ----------------------------------------------------------------------


! Allocatable arrays for Lagrange Interpolation (declared in mdl_arr.f90)
ALLOCATE (X_interp(NPint), STAT = AllocateStatus)
ALLOCATE (Y_interp(NPint), STAT = AllocateStatus)


! ----------------------------------------------------------------------
! Number of data points (used for the interpolation) before and after of the orbit point closest to the computation epoch	  
NPint_1 = INT(NPint/2) 
NPint_2 = NPint - NPint_1 
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Form arrays with data points to be used in the interpolation		
! ----------------------------------------------------------------------
If ( tepoch <= torb_1 ) then
! Extrapolation (backward in time): Computation epoch is before the first epoch of the orbit array

! Interpolation arrays indicators
i1 = 1
i2 = 1 + (NPint - 1)

i0 = i1
!Else If ( tepoch == torb_1 ) then


Else If ( tepoch>torb_1 .and. tepoch <= orbint( NPint_1 ,1) ) Then	
! Interpolation arrays indicators
i1 = 1
i2 = 1 + (NPint - 1)

i0 = i1

Else If ( tepoch > orbint( NPint_1 ,1) .and. tepoch < orbint(sz1-NPint_2 , 1) ) Then	
! Find orbit epoch that is the closest smaller to the interpolation epoch 
    DO j = NPint_1 , sz1-1
       !if (tepoch > orbint(j,1) .and. tepoch < orbint(j+1,1)) then
       if (tepoch > orbint(j,1)) then 
	    IF (tepoch < orbint(j+1,1) ) then
	      i = j

		  ! Interpolation arrays indicators
          i1 = i - (NPint_1 - 1)
          i2 = i + NPint_2
		  
	      EXIT
		end if  
	   end IF
    End Do
	
i0 = i
	
Else If ( tepoch >= orbint(sz1-NPint_2 , 1) ) Then
! Interpolation arrays indicators
i1 = sz1 - (NPint - 1)
i2 = sz1

i0 = i1
	  
Else If ( tepoch > orbint(sz1 , 1) ) Then
! Interpolation arrays indicators
i1 = sz1 - (NPint - 1)
i2 = sz1

i0 = i1

End IF	  
! ----------------------------------------------------------------------

!print *,"i1",i1
!print *,"i2",i2
!print *,"i0",i0
!print *,"tepoch",tepoch
!print *,"torb_1",torb_1
!print *,"orbint( NPint_1 ,1)",orbint( NPint_1 ,1)
!print *,"orbint(sz1-NPint_2 , 1)",orbint(sz1-NPint_2 , 1)


! ----------------------------------------------------------------------
! Time array of the data points used for the interpolation
! ----------------------------------------------------------------------
! Time array in seconds (Sec of Day)
X_interp = orbint(i1:i2, 2)	
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Interpolation epoch 
! ----------------------------------------------------------------------
! Time difference (in days)
!dmjd = (mjd - orbint(i1,1))

! Time difference (in seconds)
!dsec = dmjd *  86400.D0 

! Interpolation epoch in seconds
!tint = X_interp(1) + dsec

! ----------------------------------------------------------------------
! Time difference (in days)
dmjd = (mjd - orbint(i0,1))

! Time difference (in seconds)
dsec = dmjd *  86400.D0 

! Interpolation epoch in seconds
tint = orbint(i0,2) + dsec
! ----------------------------------------------------------------------

!print *,"dsec",dsec
!print *,"tint",tint
!print *,"X_interp",X_interp

		

! ----------------------------------------------------------------------
! Position and Velocity Vectors
! ----------------------------------------------------------------------
! X
Y_interp = orbint(i1:i2, 3)		
Call interp_lag(tint, Xint)

! Vx
Call interp_lag(tint-1.D0, Xint_1) 
!Vx = Xint_1 - Xint 
Vx = Xint - Xint_1 
			
! Y
Y_interp = orbint(i1:i2, 4)
Call interp_lag(tint, Yint) 

! Vy
Call interp_lag(tint-1.D0, Yint_1) 
!Vy = Yint_1 - Yint
Vy = Yint - Yint_1
			
! Z
Y_interp = orbint(i1:i2, 5)
Call interp_lag(tint, Zint) 

! Vz
Call interp_lag(tint-1.D0, Zint_1) 
!Vz = Zint_1 - Zint
Vz = Zint - Zint_1	  
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Write state vector coordinates to output arguments
rint(1) = Xint
rint(2) = Yint
rint(3) = Zint

vint(1) = Vx
vint(2) = Vy
vint(3) = Vz
! ----------------------------------------------------------------------


! Deallocation
DEALLOCATE (orbint, STAT = DeAllocateStatus)
Deallocate (X_interp)	  
Deallocate (Y_interp)	  



End subroutine

