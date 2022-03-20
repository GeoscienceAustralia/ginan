MODULE m_integrEQM


! ----------------------------------------------------------------------
! MODULE: m_integrEQM.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for numerical integration of the Equation of Motion 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	4 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
	  


Contains


SUBROUTINE integr_EQM (MJDo, tsec_start, ro, vo, arc, integID, step, orbc)


! ----------------------------------------------------------------------
! SUBROUTINE: integr_EQM
! ----------------------------------------------------------------------
! Purpose:
!  Integration of Equation of Motion based on Runge-Kutta methods
! ----------------------------------------------------------------------
! Input arguments:
! - to:			Initial Epoch: Modified Julian Day (MJD) in Terrestrial Time (including the fraction of the day)
! - ro: 		Satellite position vector (m) in ICRF
! - vo: 		Satellite velocity vector (m/sec) in ICRF
! - arc:		Orbit arc lenth (seconds)
! - integID: 	Numerical integration method ID number
! 				RK7 . RKN7(6)8:	Runge-Kutta-Nystrom 7th order (default)   
! 				RK4 . RK4:			Runge-Kutta 4th order
! 				RK8 . RK8(7)13:	Runge-Kutta 8th order
! - step: 		Numerical Integrator Stepsize (seconds)
!
! Output arguments:
! - orbc: 		Satellite orbit array in ICRF including the following per epoch:
!               - Modified Julian Day number in TT (including the fraction of the day) 
!				- Seconds since 00h in TT
!				- Position vector (m)
!				- Velocity vector (m/sec)
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:	18 September 2017
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE pod_yaml
      USE mdl_param
      IMPLICIT NONE

	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: MJDo, tsec_start
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: ro, vo
      REAL (KIND = prec_d), INTENT(IN) :: arc
      INTEGER (KIND = prec_int2), INTENT(IN) :: integID
      REAL (KIND = prec_d), INTENT(IN) :: step
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: orbc  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, i, j 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      REAL (KIND = prec_d) :: lamda_h
      REAL (KIND = prec_d) :: to_sec, tmax, TT, TTo, t
      REAL (KIND = prec_d), DIMENSION(6) :: Zq
      REAL (KIND = prec_d), DIMENSION(3) :: er
      REAL (KIND = prec_d) :: mjd_0, mjd_th, mjdn
      REAL (KIND = prec_d), DIMENSION(3) :: rto, vto, rth, vth  
      REAL (KIND = prec_d), DIMENSION(8) :: Zo 
      REAL (KIND = prec_d), DIMENSION(6) :: yo, yn, ey 
! ----------------------------------------------------------------------	  
      EXTERNAL integr_rk4
      EXTERNAL integr_rkn768
      EXTERNAL integr_rk87
! ----------------------------------------------------------------------	  
      REAL (KIND = prec_d), DIMENSION(3) :: delta_v
	  REAL (KIND = prec_d) :: mjd_ti_pulse, mjd_tj
      INTEGER (KIND = prec_int8) :: N_PULSE_epochs, i1_pulse
! ----------------------------------------------------------------------	  

! ----------------------------------------------------------------------
! Initial Conditions
! ----------------------------------------------------------------------
! Time conversion to seconds (MJD from days to seconds)

! Initial Epoch's Fraction of the day (in seconds)
to_sec = tsec_start !to_sec = (MJDo - INT(MJDo)) * (24.D0 * 3600.D0)

! Final Epoch
tmax = to_sec + arc

! Number of epochs
Nepochs = INT(arc / ABS(step)) + 1


! ----------------------------------------------------------------------
! Dynamic allocatable array 
! ----------------------------------------------------------------------
ALLOCATE (orbc(Nepochs,8), STAT = AllocateStatus)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Initial state vector into the orbit array
! ----------------------------------------------------------------------
orbc(1,1) = MJDo
orbc(1,2) = to_sec
orbc(1,3:5) = ro
orbc(1,6:8) = vo
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Numerical integration methods
! ----------------------------------------------------------------------

!print*,'integID=',integID
! ----------------------------------------------------------------------
! Runge-Kutta-Nystrom RKN7(6)-8 method
! ----------------------------------------------------------------------
If (integID == RK7) Then

! RKN7(6)-8 lamda parameter for stepsize control
lamda_h = 0.001D0

!i = 1
!Do t = to_sec , step , tmax-1

TT = to_sec
Do j = 1 , Nepochs-1
	
    ! Numerical integration for computation of the state vector at next epoch
    Zo(1) = orbc(j,1) 
    Zo(2) = orbc(j,2) 
    Zo(3:8) = orbc(j,3:8) 
	
! ----------------------------------------------------------------------
	
    Call integr_rkn768(Zo, step, lamda_h, Zq, er)
    
	! Next epoch TT (to+h)
    TT = TT + step    
	! Seconds since 00h
	TTo = TT
	If (TT >= 86400.D0) Then
		TTo = TT - INT(TT / 86400.D0) * 86400.D0
	End IF 
	If (TT < 0.D0) Then
		TTo = TT + (INT(ABS(TT)/86400.D0)+1) * 86400.D0
	End IF 

	! MJD and Seconds
	orbc(j+1,1) = INT(MJDo) + TT / (24.0D0 * 3600.0D0) 
	orbc(j+1,2) = TTo
		
	! State vector at the next epoch TT (to+h) in the GCRS
	orbc(j+1,3:8) = Zq
	
!	print *,"orbc t", orbc(j+1,1:2)
!	print *,"orbc r", orbc(j+1,3:5)
	
End DO
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Runge-Kutta RK4 method
! ----------------------------------------------------------------------
Else If (integID == RK4) Then


TT = to_sec
Do j = 1 , Nepochs-1
	! Epoch to	
    mjd_0 = orbc(j,1) 
    yo = orbc(j,3:8) 
	
    ! Numerical integration for computation of the state vector at next epoch
	Call integr_rk4(mjd_0, yo, step, mjdn, yn)
   
	! Next epoch TT (to+h)
    TT = TT + step    

	! Seconds since 00h
	TTo = TT
	If (TT >= 86400.D0) Then
		TTo = TT - INT(TT / 86400.D0) * 86400.D0
	End IF 

	! MJD and Seconds
	orbc(j+1,1) = INT(MJDo) + TT / (24.0D0 * 3600.0D0) 
	orbc(j+1,2) = TTo
		
	! State vector at the next epoch TT (to+h) in the GCRS
	orbc(j+1,3:8) = yn
End DO
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Runge-Kutta RK8(7)-13 method
! ----------------------------------------------------------------------
Else If (integID == RK8) Then


TT = to_sec
Do j = 1 , Nepochs-1
	! Epoch to	
    mjd_0 = orbc(j,1) 
    yo = orbc(j,3:8) 
	
    ! Numerical integration for computation of the state vector at next epoch
	Call integr_rk87 (mjd_0, yo, step, mjdn, yn, ey)
   
	! Next epoch TT (to+h)
    TT = TT + step    

	! Seconds since 00h
	TTo = TT
	If (TT >= 86400.D0) Then
		TTo = TT - INT(TT / 86400.D0) * 86400.D0
	End IF 

	! MJD and Seconds
	orbc(j+1,1) = INT(MJDo) + TT / (24.0D0 * 3600.0D0) 
	orbc(j+1,2) = TTo
		
	! State vector at the next epoch TT (to+h) in the GCRS
	orbc(j+1,3:8) = yn
End DO
! ----------------------------------------------------------------------

End If


End Subroutine



End Module


