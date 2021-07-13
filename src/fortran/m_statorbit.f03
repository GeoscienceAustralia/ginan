MODULE m_statorbit


! ----------------------------------------------------------------------
! MODULE: m_statorbit.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling statorbit subroutine 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	6 November 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			


Contains
	  
	  
SUBROUTINE statorbit (ds1, ds2, dorb_XYZ, dorb_RTN, dorb_Kepler, stat_XYZ, stat_RTN, stat_Kepler)


! ----------------------------------------------------------------------
! SUBROUTINE: statorbit.f03
! ----------------------------------------------------------------------
! Purpose:
!  Numerical comparison and statistics between two orbits in inertial frame (ICRF) 
! ----------------------------------------------------------------------
! Input arguments:
! - orb_ref:		1st data series matrix (used as reference)
! - orb_det: 		2nd data series matrix
!
! Output arguments:
! - dorb_XYZ:		Orbit differences in the intertial frame (position and velocity vector differences)
!        			dorb_XYZ = [Time_MJD Time_Seconds00h r(X) r(Y) r(Z) v(X) v(Y) v(Z)] 
! - dorb_RTN:		Orbit differences in the orbital frame (RTN: radial, along-track and cross-track differences)
!        			dorb_RTN = [Time_MJD Time_Seconds00h R(X) T(Y) N(Z) R(vX) T(vY) N(vZ)]
! - dorb_Kepler:	Orbit differences for the Keplerian elements
!        			dorb_Kepler = [Time_MJD Time_Seconds00h a e i Omega omega E]
!   				a: 		semi-major axis  (m)
!   				e: 		eccentricity
!   				i: 		inclination (degrees)
!   				Omega:	right ascension of the ascending node (degrees)
!   				omega:	argument of perigee (degrees)
!   				E: 		eccentric anomaly (degrees)
! - stat_XYZ:		Statistical quantities of dorb_XYZ (see Note 1)					
! - stat_RTN:		Statistical quantities of dorb_RTN (see Note 1)				
! - stat_Kepler:	Statistical quantities of dorb_Kepler (see Note 1)
! ----------------------------------------------------------------------
! Note 1
! The arrays of the statistical quantities of the orbital differences
! are formed as follows:
! stat_XYZ(1,:) : RMS value for each of the orbital elements e.g. X,Y,Z,Vx,Vy,Vz 		
! stat_XYZ(2,:) : Standard Deviation (sigma) for each of the orbital elements e.g. X,Y,Z,Vx,Vy,Vz 		
! stat_XYZ(3,:) : Mean value for each of the orbital elements e.g. X,Y,Z,Vx,Vy,Vz 		
! stat_XYZ(4,:) : MIN value for each of the orbital elements e.g. X,Y,Z,Vx,Vy,Vz 
! stat_XYZ(5,:) : MAX value for each of the orbital elements e.g. X,Y,Z,Vx,Vy,Vz 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	11 October 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE m_statist
      USE mdl_config
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), ALLOCATABLE, DIMENSION(:,:) :: ds1, ds2
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: dorb_XYZ, dorb_RTN, dorb_Kepler  
      ! REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:), ALLOCATABLE :: RMSdsr, MINdsr, MAXdsr  
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(5,6) :: stat_XYZ, stat_RTN, stat_Kepler 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), ALLOCATABLE, DIMENSION(:,:) :: dsr
      REAL (KIND = prec_d), ALLOCATABLE, DIMENSION(:) :: dx
      REAL (KIND = prec_d), DIMENSION(6) :: RMSdsr, Sigmadsr, MEANdsr, MINdsr, MAXdsr  
      REAL (KIND = prec_d) :: RMSdx, Sigmadx, MEANdx, MINdx, MAXdx  
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: delta_t, dt_limit
      INTEGER (KIND = prec_int8) :: Nepochs, Nepochs2, Nelements, Nepochs_delta 
      INTEGER (KIND = prec_int8) :: i, j, j1, k, looptest 
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3, sz4
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------	  
      REAL (KIND = prec_d), DIMENSION(3) :: r1i, v1i, r2i, v2i
      REAL (KIND = prec_d), DIMENSION(3) :: dr, dv, dr_RTN, dv_RTN
      REAL (KIND = prec_d) :: Rrtn(3,3)
      REAL (KIND = prec_d) :: GM  
      REAL (KIND = prec_d) :: kepler1(6), kepler2(6), kepler_9(9) 
      CHARACTER (LEN=100) :: mesg
! ----------------------------------------------------------------------	  



! Set the numerical differences digits limit for the common epochs
dt_limit = 1.D-08


! Gravitational constant
GM = 0.39860044150D+15


sz1 = size(ds1, DIM = 1)
sz2 = size(ds1, DIM = 2)
Nepochs = sz1

sz3 = size(ds2, DIM = 1)
sz4 = size(ds2, DIM = 2)
Nepochs2 = sz3
!print*,'statorbit: sz1, sz2 = ',sz1, sz2
!print*,'statorbit: sz3, sz4 = ',sz3, sz4
! ----------------------------------------------------------------------
! Test columns dimension
If (sz2 .NE. sz4) Then
write(mesg, *) "Input matrices dimension(DIM=2) do not agree. DIM = 2 (", &
        sz2, ",", sz4, ")" 
call report ('WARNING', pgrm_name, 'statorbit', mesg, 'src/m_statorbit.f03', 1)
End If
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Find the number of the common epochs
Nepochs_delta = 0
i = 0
j = 0
Do i = 1 , Nepochs
   ! Test the time argument: 
   Do j = 1 , Nepochs2 
      delta_t = ABS(ds2(j,1) - ds1(i,1))
      IF (delta_t < dt_limit) then
         ! Counter of the common epochs
         Nepochs_delta = Nepochs_delta + 1 
      End if
   End Do
End Do
! ----------------------------------------------------------------------
!print *,"Nepochs_delta", Nepochs_delta
!print *,"Nepochs", Nepochs
!print *,"delta_t", delta_t


! Dynamic allocatable array
! Allocate the array of the numerical orbit comparison
ALLOCATE (dsr(Nepochs_delta,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write (mesg, *) "failed to allocate dsr, dimensions = (", & 
                Nepochs_delta, ",", sz2, ")"
call report ('FATAL', pgrm_name, 'statorbit', mesg, 'src/m_statorbit.f03', 1)
end if
ALLOCATE (dorb_XYZ(Nepochs_delta,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write (mesg, *) "failed to allocate dorb_XYZ, dimensions = (", & 
                Nepochs_delta, ",", sz2, ")"
call report ('FATAL', pgrm_name, 'statorbit', mesg, 'src/m_statorbit.f03', 1)
end if
ALLOCATE (dorb_RTN(Nepochs_delta,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write (mesg, *) "failed to allocate dorb_RTN, dimensions = (", & 
                Nepochs_delta, ",", sz2, ")"
call report ('FATAL', pgrm_name, 'statorbit', mesg, 'src/m_statorbit.f03', 1)
end if
ALLOCATE (dorb_Kepler(Nepochs_delta,sz2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write (mesg, *) "failed to allocate dorb_Kepler, dimensions = (", & 
                Nepochs_delta, ",", sz2, ")"
call report ('FATAL', pgrm_name, 'statorbit', mesg, 'src/m_statorbit.f03', 1)
end if
!initialise
dsr = 0.d0
dorb_XYZ = 0.d0
dorb_RTN = 0.d0
dorb_kepler = 0.d0

! ----------------------------------------------------------------------
! Compute the numerical differences
i = 0
j = 0
k = 0
Do i = 1 , Nepochs
   ! Test the time argument: 
   Do j = 1 , Nepochs2   
      delta_t = ABS(ds2(j,1) - ds1(i,1))
      IF (delta_t < dt_limit) then
         k = k + 1

	     ! State vector numerical differences at common epochs
		 dsr(k,1:2) = ds1(i,1:2)
         dsr(k,3:sz2) = ds2(j,3:sz2) - ds1(i,3:sz2)
IF(dsr(k,3)> 1.D+4.OR.dsr(k,4)> 1.D+4.OR.dsr(k,5)>1.D+4)THEN
PRINT*,'Something wrong with the numerical integration'
PRINT*,'Please adjust the integration step'
STOP
END IF
	     ! Orbital frame numerical differences at common epochs
		 r1i = ds1(i,3:5)
		 v1i = ds1(i,6:8)
		 dr = ds2(j,3:5) - ds1(i,3:5)
		 dv = ds2(j,6:8) - ds1(i,6:8)
		 CALL orb_frame(r1i, v1i, Rrtn)
		 CALL matrix_Rr(Rrtn, dr, dr_RTN)
		 CALL matrix_Rr(Rrtn, dv, dv_RTN)
		 dorb_RTN(k,1:2) = ds1(i,1:2)
         dorb_RTN(k,3:5) = dr_RTN
         dorb_RTN(k,6:8) = dv_RTN

	     ! Kepler elements numerical differences at common epochs
		 CALL kepler_z2k (r1i, v1i, GM, kepler_9)
		 kepler1(1:5) = kepler_9(1:5)		 
		 kepler1(6) = kepler_9(7)		 
		 
		 r2i = ds2(j,3:5)
		 v2i = ds2(j,6:8)
		 CALL kepler_z2k (r2i, v2i, GM, kepler_9)
		 kepler2(1:5) = kepler_9(1:5)		 
		 kepler2(6) = kepler_9(7)		 

		 dorb_Kepler(k,1:2) = ds1(i,1:2)
         dorb_Kepler(k,3:8) = kepler2 - kepler1
		 
	  End IF
   End Do
End Do
! ----------------------------------------------------------------------
!print *,"k", k


dorb_XYZ = dsr
sz1 = size(dsr, DIM = 1)


! ----------------------------------------------------------------------
! Statistics
! ----------------------------------------------------------------------

! Allocate arrays
ALLOCATE (dx(sz1), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write (mesg, *) "failed to allocate dx, dimension = ", & 
                Nepochs_delta 
call report ('FATAL', pgrm_name, 'statorbit', mesg, 'src/m_statorbit.f03', 1)
end if
dx = 0.d0

! Inertial frame
dsr = dorb_XYZ
i = 0
j = 0
Do i = 3 , sz2
   dx(:) = dsr(:,i)
   Call statist(dx, RMSdx, Sigmadx, MEANdx, MINdx, MAXdx)
   j = j + 1
   RMSdsr(j) = RMSdx
   MINdsr(j) = MINdx
   MAXdsr(j) = MAXdx
   Sigmadsr(j) = Sigmadx
   MEANdsr(j) = MEANdx
End Do
stat_XYZ(1,:) = RMSdsr
stat_XYZ(2,:) = Sigmadsr
stat_XYZ(3,:) = MEANdsr
stat_XYZ(4,:) = MINdsr
stat_XYZ(5,:) = MAXdsr


! Orbital frame
dsr = dorb_RTN
i = 0
j = 0
Do i = 3 , sz2
   dx(:) = dsr(:,i)
   Call statist(dx, RMSdx, Sigmadx, MEANdx, MINdx, MAXdx)
   j = j + 1
   RMSdsr(j) = RMSdx
   MINdsr(j) = MINdx
   MAXdsr(j) = MAXdx
   Sigmadsr(j) = Sigmadx
   MEANdsr(j) = MEANdx
End Do
stat_RTN(1,:) = RMSdsr
stat_RTN(2,:) = Sigmadsr
stat_RTN(3,:) = MEANdsr
stat_RTN(4,:) = MINdsr
stat_RTN(5,:) = MAXdsr


! Kepler frame
dsr = dorb_Kepler
i = 0
j = 0
Do i = 3 , sz2
   dx(:) = dsr(:,i)
   Call statist(dx, RMSdx, Sigmadx, MEANdx, MINdx, MAXdx)
   j = j + 1
   RMSdsr(j) = RMSdx
   MINdsr(j) = MINdx
   MAXdsr(j) = MAXdx
   Sigmadsr(j) = Sigmadx
   MEANdsr(j) = MEANdx
End Do
stat_Kepler(1,:) = RMSdsr
stat_Kepler(2,:) = Sigmadsr
stat_Kepler(3,:) = MEANdsr
stat_Kepler(4,:) = MINdsr
stat_Kepler(5,:) = MAXdsr
! ----------------------------------------------------------------------



 100 END SUBROUTINE


End Module

