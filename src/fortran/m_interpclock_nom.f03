MODULE m_interpclock_nom


! ----------------------------------------------------------------------
! MODULE: m_interpclock_nom.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the interpclock_nom subroutine used for GNSS satellite clocks interpolation
! 
! Subroutines contained within the module:
! - interpclock_nom.f03
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	27 May 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE interpclock_nom (ORB_matrix, CLKmatrix, PRNmatrix, interv_in, NPint, CLKmatrix_int)


! ----------------------------------------------------------------------
! SUBROUTINE: interpclock_nom.f90
! ----------------------------------------------------------------------
! Purpose:
!  GNSS sp3 clocks interpolation based on Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - CLKmatrix:		Satellite Clocks' allocatable array (3-dimensional)
!					CLKmatrix(i,j,isat) = [MJD Sec_of_day clock]
!					If clock error (sdev) is available (when Position and Velocity vector are written in orbit sp3 format)
!					CLKmatrix(i,j,isat) = [MJD Sec_of_day clock clock_sdev]
! - PRNmatrix:      Satellites' PRN numbers allocatable array
!
! Output arguments:
! - CLKmatrix_int:  Interpolated Satellite Clocks' allocatable array (3-dimensional)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	27 May 2020
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
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: ORB_matrix, CLKmatrix 
	  CHARACTER (LEN=3), ALLOCATABLE, INTENT(IN) :: PRNmatrix(:)
      INTEGER (KIND = prec_int8), INTENT(IN) :: interv_in, NPint
! OUT
      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:,:), ALLOCATABLE :: CLKmatrix_int
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, interv 
      INTEGER (KIND = prec_int8) :: data_rate, Nepoch_int, Norb_int, NPint_1, NPint_2, Nlimit 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, ti
	  REAL (KIND = prec_d) :: SecDay_i, MJD_i 
	  REAL (KIND = prec_d) :: MJD_tint, tint, Xint, Xint_1, Yint, Yint_1, Zint, Zint_1
	  REAL (KIND = prec_d) :: X, Y, Z, Vx, Vy, Vz
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: Y_interp 
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: clock_sat_prn, clock_int
      INTEGER (KIND = prec_int2) :: Clock_sdev_param
      INTEGER (KIND = prec_int8) :: Nepochs_orb, Nepochs_clk, Nelements_clk, Nsat_clk, Nclock_int 
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3  
      INTEGER (KIND = prec_int8) :: i_elements, i_sat, i_clock_epoch, i_CLKmatrix  


! ----------------------------------------------------------------------
sz1 = size(ORB_matrix, DIM = 1)
Nepochs_orb = sz1

sz1 = size(CLKmatrix, DIM = 1)
Nepochs_clk = sz1

sz2 = size(CLKmatrix, DIM = 2)
Nelements_clk = sz2

sz3 = size(CLKmatrix, DIM = 3)
Nsat_clk = sz3
! ----------------------------------------------------------------------

! Allocatable arrays for the interpolated clock array per satellite
!ALLOCATE (CLKmatrix_int(Nepochs_orb, Nelements_clk, Nsat_clk) , STAT = AllocateStatus)

! Check the availability of the clock error sdev
IF (Nelements_clk == 4) THEN
! Clock error (sdev) is available
Clock_sdev_param = 1
ELSE
Clock_sdev_param = 0
END IF
 
! ----------------------------------------------------------------------
! Interpolation epoch limits (24h orbit arc or less):
! 1. Interpolation is applied only within the data points (15 min less than 24h arc):  Set Nlimit to 1
! 2. Interpolation is applied also after the last data point's epoch (23h 45min 00sec) in order to cover a 24h arc:  Set Nlimit to 0
Nlimit = 1
! ----------------------------------------------------------------------

! Data rate of clocks (in sec)
data_rate = 1 ! Initialization
      DO i = 1 , Nepochs_clk - 1
		if ( INT(CLKmatrix(i+1,1,1)) - INT(CLKmatrix(i,1,1)) == 0 ) then
			data_rate = INT( CLKmatrix(i+1,2,1) - CLKmatrix(i,2,1) )
			EXIT
		end if
	  End Do	  
	  	  
! ----------------------------------------------------------------------
IF (interv_in < 0 .OR. interv_in > data_rate) THEN
	interv = data_rate
ELSE
	interv = interv_in
END IF
! ----------------------------------------------------------------------
	  
      ! Number of points to be interpolated during the span between two epochs 
	  Nepoch_int = data_rate / interv - 1
	  	  
      ! Number of epochs of interpolated orbit (excluding extrapolation at the last point i.e. the start of the next day's sp3: 00h 00min 00sec)
      Nclock_int = (data_rate / interv) * (Nepochs_clk - Nlimit) + Nlimit
	  
! Allocatable arrays for Lagrange Interpolation (declared in mdl_arr.f90)
      ALLOCATE (X_interp(NPint), STAT = AllocateStatus)
      ALLOCATE (Y_interp(NPint), STAT = AllocateStatus)
	  	  
! Number of data points (used for the interpolation) before and after of each epoch	  
      NPint_1 = INT(NPint/2) 
      NPint_2 = NPint - NPint_1 

! ----------------------------------------------------------------------
! Allocatable arrays for the interpolated clock arrays
ALLOCATE (CLKmatrix_int(Nclock_int, Nelements_clk, Nsat_clk) , STAT = AllocateStatus)
ALLOCATE (clock_int(Nclock_int, Nelements_clk) , STAT = AllocateStatus)
ALLOCATE (clock_sat_prn(Nepochs_clk, Nelements_clk) , STAT = AllocateStatus)
! ----------------------------------------------------------------------

!print *,"Nepochs_orb, Nepochs_clk, Nclock_int", Nepochs_orb, Nepochs_clk, Nclock_int

!clock_int = 0.0D0	

Do i_sat = 1 , Nsat_clk
i_clock_epoch = 0
Do i_clock_epoch = 1 , Nepochs_clk
Do i_elements = 1 , Nelements_clk	
clock_sat_prn(i_clock_epoch, i_elements) = CLKmatrix(i_clock_epoch, i_elements ,i_sat)	
End Do
End Do	
		
	  epoch_i = 0
      DO i = 1 , Nepochs_clk
		epoch_i  = epoch_i + 1
		MJD_i    = clock_sat_prn(i,1)
		SecDay_i = clock_sat_prn(i,2)		
! ----------------------------------------------------------------------
! Form arrays with data points to be used in the interpolation		
		if ( i < NPint_1 ) then
			i1 = i
			i2 = i + (NPint - 1)
			!print *,"Nepochs_clk, i, epoch_i, i1, i2", Nepochs_clk, i, epoch_i, i1, i2
		else if ( Nepochs_clk - i < NPint_2 ) then
			i2 = Nepochs_clk
			i1 = i2 - (NPint-1)
			!print *,"Nepochs_clk, i, epoch_i, i1, i2", Nepochs_clk, i, epoch_i, i1, i2
		else 
			i1 = i - (NPint_1 - 1)
			i2 = i + NPint_2
			!print *,"i,epoch_i,i1,i2", i, epoch_i, i1, i2
		end if

        ! Time array of the data points used for the interpolation: X_interp
!		ti = 1
!		X_interp(1) = clock_sat_prn(i1, 2)
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
! Data Point - Epoch without interpolation
		clock_int(epoch_i, 1:Nelements_clk) = clock_sat_prn(i, 1:Nelements_clk)		
        !clock_err = clock_sat_prn(i,3)
		!IF ( Clock_sdev_param == 1 ) THEN
		!clock_sdev = clock_sat_prn(i,Nelements_clk)
		!END IF		
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Lagrange Interpolation applied for the new points 
! ----------------------------------------------------------------------
! When Nlimit=0 & i=Nepochs_clk : Extrapolation is performed after the last data point of the sp3 file (for completing 24h arc)
If (i <= Nepochs_clk - Nlimit) then
		DO j = 1 , Nepoch_int				
			tint = j * interv * 1.D0 + SecDay_i
			
		!DO j = t(1) , t(NPint) , interv
			!tint = j * interv + clock_sat_prn(i,2)
				
		! Lagrange Interpolation
			! Clock error X
			Y_interp = clock_sat_prn(i1:i2, 3)		
			Call interp_lag(tint, X_interp, Y_interp, Xint) 
						
			! Clock error standard deviation Y
			IF ( Clock_sdev_param == 1 ) THEN
			Y_interp = clock_sat_prn(i1:i2, Nelements_clk)
			Call interp_lag(tint, X_interp, Y_interp, Yint) 
			END IF		
			
! Next epoch			
			epoch_i = epoch_i + 1				
! MJD epoch
			MJD_tint = INT(MJD_i) * 1.D0 + tint / 86400.D0
			
! Write interpolated values in array
			! Time arguments at interpolation epoch
			clock_int(epoch_i, 1) = MJD_tint
			
			If (tint > 86400.D0) then
			   tint = tint - 86400.D0
			End If 
			
			clock_int(epoch_i, 2) = tint
			
			! Clock error (interpolated)
			clock_int(epoch_i, 3) = Xint
			IF ( Clock_sdev_param == 1 ) THEN
			! Clock sdev (interpolated)		
			clock_int(epoch_i, Nelements_clk) = Yint			
			END IF		
		End DO
End IF
! ----------------------------------------------------------------------
      End Do		  
!print *,"epoch_i", epoch_i
	  
! Write clockint array to the overall CLKmatrix
i_CLKmatrix = 0
i_elements = 0
Do i_CLKmatrix = 1 , Nclock_int
Do i_elements = 1 , Nelements_clk	
CLKmatrix_int(i_CLKmatrix, i_elements ,i_sat) = clock_int(i_CLKmatrix, i_elements)
End Do
End Do	
!clock_int = 0.0D0	

END DO	  	  
	  
	  Deallocate (X_interp)	  
	  Deallocate (Y_interp)	  
End subroutine

END Module
