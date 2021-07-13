MODULE m_sat_ini_vet


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


 SUBROUTINE sat_ini_vet (fname_sp3, ini_vet)

! ----------------------------------------------------------------------
! SUBROUTINE: sat_ini_vet
! ----------------------------------------------------------------------
! Purpose: Generate the initial state vector for multi-GNSS satellites 
!          using the interpolatior of Lagrange polynomials
! ----------------------------------------------------------------------
! Input arguments:
! - fname_sp3:  	Orbit file in sp3 format which contains only position vector 
!
! Output arguments:
! - ini_vet:    	Array to restore the initial state vectors in ITRF
! ----------------------------------------------------------------------
! Note 1:
!  The input orbit file (sp3) is considered to include satellite position vector only
!  as this is the IGS standard product
!  During the orbit interpolation, based on Lagrange polynomials, position 
!  and velocity vectors for the selected satellite (PRN) are computed and
!  provided in the output files.  
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	5 August 2016
! ----------------------------------------------------------------------
! Last modified:
! - Dr. Thomas Papanikolaou, 14 November 2017:
!	Upgraded from Fortran 90 to Fortran 2003 for considering the
!	Fortran 2003 adavantages in dynamic memory allocation
!
! - Dr. Tzupang Tseng, 28 August 2018:
!       Enable the function of processing the multi-GNSS satellites 
!       in order to obtain the initial state vector which will be used for 
!       the further steps, such as the numerical integration or the orbit determination.
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_gnssp3
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
!      INTEGER (KIND = prec_int8), INTENT(IN) :: interv, NPint
	  !INTEGER (KIND = 4) :: PRN
      !CHARACTER (LEN=3), INTENT(IN) :: PRN
! OUT
!      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: orbint
!      REAL (KIND = prec_q), INTENT(OUT), DIMENSION(:,:,:), ALLOCATABLE :: ini_vet
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, epoch_i, sz1, sz2
      INTEGER (KIND = prec_int8) :: data_rate, Nepoch_int, Norb_int, NPint_1, NPint_2, Nlimit 
	  INTEGER (KIND = prec_int8) :: i, j, i1, i2, ti
	  REAL (KIND = prec_d) :: SecDay_i, MJD_i 
	  REAL (KIND = prec_d) :: MJD_tint, tint, Xint, Xint_1, Yint, Yint_1, Zint, Zint_1
	  REAL (KIND = prec_d) :: X, Y, Z, Vx, Vy, Vz
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      CHARACTER (LEN=300) :: fname_write
      INTEGER (KIND = 4) :: PRN,NEPO,zz
      INTEGER (KIND = 4) :: Ofile
      INTEGER (KIND = prec_int8) :: interv, NPint
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbint
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: orbsp3
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: orbeph
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: ini_vet
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: X_interp 
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: Y_interp 
! ----------------------------------------------------------------------

! Initialize varbiles of interv and NPint
! ---------------------------------------
       interv = 900 ! in second
       NPint  = 12 
       Ofile  = 88
      OPEN (unit = 88, file= "gnss-t0-vector.txt")

	  ! Read IGS sp3 orbit data file (position only): orbsp3 
      Call gnssp3 (fname_sp3, orbeph)
	  sz1 = size(orbeph, DIM = 2)

      ALLOCATE ( orbsp3(size(orbeph, DIM = 2), size(orbeph, DIM = 3)), STAT = AllocateStatus)
      ALLOCATE ( ini_vet(size(orbeph, DIM = 1),1, 8), STAT = AllocateStatus)

      DO PRN = 1 , size(orbeph, DIM = 1) ! satellite PRN (BIG LOOP for PROCESSING MULTI-GNSS SATELLITES)

	               DO NEPO = 1, size(orbeph, DIM = 2) ! number of epochs

        IF (orbeph(PRN,NEPO,3)==0 .and.orbeph(PRN,NEPO,4)==0.and. orbeph(PRN,NEPO,5)==0)THEN
                        orbsp3 (NEPO , 1) = 0.0d0
                        orbsp3 (NEPO , 2) = 0.0d0
                        orbsp3 (NEPO , 3) = 0.0d0
                        orbsp3 (NEPO , 4) = 0.0d0
                        orbsp3 (NEPO , 5) = 0.0d0

                        EXIT 
        ELSE
                        orbsp3 (NEPO , 1) = orbeph(PRN,NEPO,1)
                        orbsp3 (NEPO , 2) = orbeph(PRN,NEPO,2)
                        orbsp3 (NEPO , 3) = orbeph(PRN,NEPO,3)
                        orbsp3 (NEPO , 4) = orbeph(PRN,NEPO,4)
                        orbsp3 (NEPO , 5) = orbeph(PRN,NEPO,5)
        END IF
                       End DO
 
! ----------------------------------------------------------------------
! Interpolation epoch limits (24h orbit arc or less):
! 1. Interpolation is applied only within the data points (15 min less than 24h arc):  Set Nlimit to 1
! 2. Interpolation is applied also after the last data point's epoch (23h 45min 00sec) in order to cover a 24h arc:  Set Nlimit to 0
Nlimit = 1
! ----------------------------------------------------------------------
	  
	  ! Data rate of sp3 (in sec)
      DO i = 1 , sz1-1
		if ( INT(orbsp3(i+1,1)) - INT(orbsp3(i,1)) == 0 ) then
			data_rate = INT( orbsp3(2,2) - orbsp3(1,2) )
			EXIT
		end if
	  End Do
	  
	  
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
 IF (orbsp3(i,1)==0 .and. orbsp3(i,2)==0) EXIT
		
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
! Output the initial state vector at the first epoch (added by Tzupang Tseng
! 28-08-2018)
! ----------------------------------------------------------------------

      ini_vet (PRN, 1, 1) = orbint(1, 1)
      ini_vet (PRN, 1, 2) = orbint(1, 2)
      ini_vet (PRN, 1, 3) = orbint(1, 3)
      ini_vet (PRN, 1, 4) = orbint(1, 4)
      ini_vet (PRN, 1, 5) = orbint(1, 5)
      ini_vet (PRN, 1, 6) = orbint(1, 6)
      ini_vet (PRN, 1, 7) = orbint(1, 7)
      ini_vet (PRN, 1, 8) = orbint(1, 8)


      End Do

      WRITE (UNIT = 88, FMT = "(I4,5X,F8.2,5X,F3.1,3(5X,F15.4),3(5X,F14.8))") &
      & PRN, ini_vet (PRN, 1, 1), ini_vet (PRN, 1, 2), ini_vet (PRN, 1, 3), &
      &      ini_vet (PRN, 1, 4), ini_vet (PRN, 1, 5), ini_vet (PRN, 1, 6), &
      &      ini_vet (PRN, 1, 7), ini_vet (PRN, 1, 8)
 

!print*,"PRN=",PRN
!print*, ini_vet (PRN, 1, 1), ini_vet (PRN, 1, 2), ini_vet (PRN, 1, 3), &
!      & ini_vet (PRN, 1, 4), ini_vet (PRN, 1, 5), ini_vet (PRN, 1, 6), &
!      & ini_vet (PRN, 1, 7), ini_vet (PRN, 1, 8)


      END DO ! BIG LOOP for PROCESSING MULTI-GNSS SATELLITES

	  Deallocate (X_interp)
	  Deallocate (Y_interp)

      CLOSE (UNIT = 88)  
End subroutine



END Module
