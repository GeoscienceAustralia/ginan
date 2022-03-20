MODULE m_integrVEQ


! ----------------------------------------------------------------------
! MODULE: m_integrVEQ.f95
! ----------------------------------------------------------------------
! Purpose:
!  Module for numerical integration of the Variational Equations 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Cooperative Research Centre for Spatial Information, Australia
! Created:  22 January 2018
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE                   
        


Contains


SUBROUTINE integr_VEQ (MJDo, tsec_start, ro, vo, arc, integID, step, Nparam, orbc, Smatrix, Pmatrix)


! ----------------------------------------------------------------------
! SUBROUTINE: integr_VEQ
! ----------------------------------------------------------------------
! Purpose:
!  Variational Equations solution based on Runge-Kutta numerical integration methods
! ----------------------------------------------------------------------
! Input arguments:
! - to:                 Initial Epoch: Modified Julian Day (MJD) in Terrestrial Time (including the fraction of the day)
! - ro:           Satellite position vector (m) in ICRF
! - vo:           Satellite velocity vector (m/sec) in ICRF
! - arc:          Orbit arc lenth (seconds)
! - integID:      Numerical integration method ID number
!                       RK7. RKN7(6)8:    Runge-Kutta-Nystrom 7th order   
!                       RK4. RK4:               Runge-Kutta 4th order
!                       RK8.  RK8(7)13:   Runge-Kutta 8th order
! - step:         Numerical Integrator Stepsize (seconds)
! - Nparam:       Number of parameters to be estimated through the sensitivity matrix
!
! Output arguments:
! - orbc:         Satellite orbit array in ICRF including the following per epoch:
!               - Modified Julian Day number in TT (including the fraction of the day) 
!                       - Seconds since 00h in TT
!                       - Position vector (m)
!                       - Velocity vector (m/sec)
!- Smatrix:       State transition matrix (6*Epochs x 6)
!- Pmatrix:       Sensitivity matrix      (6*Epochs x Np)
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou
!                 Cooperative Research Centre for Spatial Information, Australia
! Created:  22 January 2018
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config
      USE m_veq_rkn768
      USE m_veqC2T
      use pod_yaml
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
        INTEGER (KIND = prec_int8), INTENT(IN) :: Nparam
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: orbc, Smatrix, Pmatrix  
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: Smatrix_T, Pmatrix_T  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs 
      INTEGER (KIND = prec_int8) :: j, iparam, i1
!       ,i, 
!       , j1 
      INTEGER (KIND = prec_int2) :: AllocateStatus
!       , DeAllocateStatus
      REAL (KIND = prec_d) :: lamda_h
      REAL (KIND = prec_d) :: to_sec, tmax, TT, TTo
!       , t
      REAL (KIND = prec_d), DIMENSION(6) :: Zq
      REAL (KIND = prec_d), DIMENSION(3) :: er
      REAL (KIND = prec_d) :: mjd_0, mjd_th
!       , mjdn
!       REAL (KIND = prec_d), DIMENSION(3) :: rto, vto, rth, vth  
      REAL (KIND = prec_d), DIMENSION(8) :: Zo 
      REAL (KIND = prec_d), DIMENSION(6) :: yo, yn
!       , ey 
! ----------------------------------------------------------------------        
      REAL (KIND = prec_d), DIMENSION(6,6) :: veqZo, veqZ, veqZ_T   
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: veqPo, veqP, veqP_T 
      INTEGER (KIND = prec_int2) :: Nveq_rv
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: Smatrix_T, Pmatrix_T  
      CHARACTER (LEN=100) :: mesg
!      LOGICAL stored
! ----------------------------------------------------------------------        
!       REAL (KIND = prec_d), DIMENSION(3) :: delta_v
!         REAL (KIND = prec_d) :: mjd_ti_pulse, mjd_tj
!       INTEGER (KIND = prec_int8) :: N_PULSE_epochs, i1_pulse
! ----------------------------------------------------------------------        


! ----------------------------------------------------------------------
! Matrices (Smatrix, Pmatrix) dimension of Variational Equations solution regarding the velocity vector rows 
Nveq_rv = 6
IF (.not. yml_write_partial_velocities) THEN
      Nveq_rv = 3
END IF
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
! Dynamic allocatable arrays 
! ----------------------------------------------------------------------
ALLOCATE (orbc(Nepochs,8), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate orbc, dimension = (", Nepochs, ",8)"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
!ALLOCATE (Smatrix(Nepochs,6*6+2), STAT = AllocateStatus)
ALLOCATE (Smatrix(Nepochs,6*Nveq_rv+2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enought memory - failed to allocate Smatrix, dimension=(", &
                Nepochs, ",", 6*Nveq_rv+2, ")"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
!ALLOCATE (Pmatrix(Nepochs,Nparam*6+2), STAT = AllocateStatus)
ALLOCATE (Pmatrix(Nepochs,Nparam*Nveq_rv+2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate Pmatrix, dimension=(", &
                Nepochs, ",", Nparam*Nveq_rv+2, ")"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
If (Nparam /= 0) Then
ALLOCATE (veqPo(6,Nparam), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enought memory - failed to allocate veqPo, dimension = (6,", &
                Nparam, ")"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
Else
ALLOCATE (veqPo(6,1), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate veqPo, dimension = (6,1)"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
End IF
Smatrix = 0.0D0
Pmatrix = 0.0D0
! Sensitivity Matrix Initial values: veqPo = 0(6xNp)
veqPo = 0.0D0

ALLOCATE (Smatrix_T(Nepochs,6*Nveq_rv+2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate Smatrix_T, dimension=(", &
                Nepochs, ",", 6*Nveq_rv+2, ")"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
ALLOCATE (Pmatrix_T(Nepochs,Nparam*Nveq_rv+2), STAT = AllocateStatus)
if (AllocateStatus .ne. 0) then
        write(mesg, *) "Not enough memory - failed to allocate Pmatrix_T, dimension=(", &
                Nepochs, ",", Nparam*Nveq_rv+2, ")"
        call report ('FATAL', pgrm_name, 'integr_VEQ', mesg, 'src/fortran/m_integrVEQ.f95', 1)
end if
Smatrix_T = 0.d0
Pmatrix_T = 0.d0

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
! VEQ matrices initial conditions
! ----------------------------------------------------------------------
! State Transition Matrix Initial values: veqZo = I (6x6)
veqZo(1,1:6) = (/ 1.D0, 0.D0, 0.D0, 0.D0, 0.D0, 0.D0 /)
veqZo(2,1:6) = (/ 0.D0, 1.D0, 0.D0, 0.D0, 0.D0, 0.D0 /)
veqZo(3,1:6) = (/ 0.D0, 0.D0, 1.D0, 0.D0, 0.D0, 0.D0 /)
veqZo(4,1:6) = (/ 0.D0, 0.D0, 0.D0, 1.D0, 0.D0, 0.D0 /)
veqZo(5,1:6) = (/ 0.D0, 0.D0, 0.D0, 0.D0, 1.D0, 0.D0 /)
veqZo(6,1:6) = (/ 0.D0, 0.D0, 0.D0, 0.D0, 0.D0, 1.D0 /)

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! VEQ matrices initial epoch values
! ----------------------------------------------------------------------
! State Transition Matrix
Smatrix(1,1) = MJDo
Smatrix(1,2) = to_sec
Smatrix(1,  3:8) = veqZo(1,1:6)
Smatrix(1, 9:14) = veqZo(2,1:6)
Smatrix(1,15:20) = veqZo(3,1:6)
IF (Nveq_rv == 6) Then
Smatrix(1,21:26) = veqZo(4,1:6)
Smatrix(1,27:32) = veqZo(5,1:6)
Smatrix(1,33:38) = veqZo(6,1:6)
End IF

! Sensitivity matrix
Pmatrix(1,1) = MJDo
Pmatrix(1,2) = to_sec
!i = 1
!Pmatrix(1, 2+(i-1)*6+1 : 2+(i-1)*6+6 ) = (/ veqPo(1,i), veqPo(2,i), veqPo(3,i), veqPo(4,i), veqPo(5,i), veqPo(6,i) /)
!Do iparam = 1 , Nparam
!   Pmatrix(1, 2+(iparam-1)*6+1 : 2+(iparam-1)*6+6 ) = & 
!   (/ veqPo(1,iparam), veqPo(2,iparam), veqPo(3,iparam), veqPo(4,iparam), veqPo(5,iparam), veqPo(6,iparam) /)
!End Do
i1 = 0
iparam = 0  
!Do i1 = 1 , 6 
Do i1 = 1 , Nveq_rv 
Do iparam = 1 , Nparam
   Pmatrix(1, 2+(i1-1)*Nparam+iparam) = veqPo(i1,iparam) 
   !(/ veqP(1,iparam), veqP(2,iparam), veqP(3,iparam), veqP(4,iparam), veqP(5,iparam), veqP(6,iparam) /)
End Do
End Do
i1 = 0
!print *,"Pmatrix(1,:)", Pmatrix(1,:)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! VEQ matrices transformation from celestial to terrestrial reference system
! ----------------------------------------------------------------------
! Transformation of state transition matrix and sensitivity matrix at initial epoch
CALL veqC2T (MJDo, to_sec, veqZo, veqPo, veqZ_T, veqP_T)

! State Transition Matrix in itrs
Smatrix_T(1,1) = MJDo
Smatrix_T(1,2) = to_sec
Smatrix_T(1,  3:8) = veqZ_T(1,1:6)
Smatrix_T(1, 9:14) = veqZ_T(2,1:6)
Smatrix_T(1,15:20) = veqZ_T(3,1:6)
IF (Nveq_rv == 6) Then
Smatrix_T(1,21:26) = veqZ_T(4,1:6)
Smatrix_T(1,27:32) = veqZ_T(5,1:6)
Smatrix_T(1,33:38) = veqZ_T(6,1:6)
End IF

! Sensitivity matrix in ITRS
Pmatrix_T(1,1) = MJDo
Pmatrix_T(1,2) = to_sec
i1 = 0
iparam = 0  
!Do i1 = 1 , 6 
Do i1 = 1 , Nveq_rv 
Do iparam = 1 , Nparam
   Pmatrix_T(1, 2+(i1-1)*Nparam+iparam) = veqP_T(i1,iparam) 
End Do
End Do
i1 = 0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Numerical integration methods
! ----------------------------------------------------------------------

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
      
      ! Epoch to  
    Zo(1) = orbc(j,1) 
    Zo(2) = orbc(j,2) 
    Zo(3:8) = orbc(j,3:8) 

! ----------------------------------------------------------------------
!print *,"VEQ INEGR RKN768 Step", TT      
    ! Numerical integration for the next epoch
    !Call integr_rkn768(Zo, step, lamda_h, Zq, er)
      Call veq_rkn768(Zo, veqZo, veqPo, step, lamda_h, Nparam, Zq, er, veqZ, veqP)  
    
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
!     print *,"orbc t", orbc(j+1,1:2)
!     print *,"orbc r", orbc(j+1,3:5)

      ! VEQ matrices at the next epoch TT (to+h)
      ! State Transition Matrix
      Smatrix(j+1,1) = orbc(j+1,1)
      Smatrix(j+1,2) = orbc(j+1,2)
      Smatrix(j+1,  3:8) = veqZ(1,1:6)
      Smatrix(j+1, 9:14) = veqZ(2,1:6)
      Smatrix(j+1,15:20) = veqZ(3,1:6)
IF (Nveq_rv == 6) Then
      Smatrix(j+1,21:26) = veqZ(4,1:6)
      Smatrix(j+1,27:32) = veqZ(5,1:6)
      Smatrix(j+1,33:38) = veqZ(6,1:6)
END IF

      ! Sensitivity matrix
      Pmatrix(j+1,1) = orbc(j+1,1)
      Pmatrix(j+1,2) = orbc(j+1,2)
      !Do iparam = 1 , Nparam
      !   Pmatrix(j+1, 2+(iparam-1)*6+1 : 2+(iparam-1)*6+6 ) = & 
      !   (/ veqP(1,iparam), veqP(2,iparam), veqP(3,iparam), veqP(4,iparam), veqP(5,iparam), veqP(6,iparam) /)
      !End Do

      i1 = 0
      iparam = 0  
      !Do i1 = 1 , 6 
      Do i1 = 1 , Nveq_rv 
      Do iparam = 1 , Nparam
         Pmatrix(j+1, 2+(i1-1)*Nparam+iparam) = veqP(i1,iparam) 
      End Do
      End Do
!print *,"m_integrVEQf03: j,Nepochs",j,Nepochs
!print *,"m_integrVEQf03: Nparam ", Nparam
!print *,"m_integrVEQf03: veqP   ", veqP(1,:)
!print *,"m_integrVEQf03: Pmatrix", Pmatrix(j+1,3:Nparam+2)
!print *,"m_integrVEQf03: veqPo  ", veqPo
!print *,"m_integrVEQf03: veqP   ", veqP
!print *,"m_integrVEQf03: Pmatrix", Pmatrix(j+1,:)

    ! Initial VEQ arrays at next epoch numerical integration 
    veqZo = veqZ
    veqPo = veqP
      
! ----------------------------------------------------------------------
! VEQ matrices transformation from celestial to terrestrial reference system
      mjd_th = INT(MJDo) + TT / (24.0D0 * 3600.0D0)
      CALL veqC2T (mjd_th, TTo, veqZ, veqP, veqZ_T, veqP_T)
! State Transition Matrix in itrs at the next epoch TT (to+h)
      Smatrix_T(j+1,1) = orbc(j+1,1)
      Smatrix_T(j+1,2) = orbc(j+1,2)
      Smatrix_T(j+1,  3:8) = veqZ_T(1,1:6)
      Smatrix_T(j+1, 9:14) = veqZ_T(2,1:6)
      Smatrix_T(j+1,15:20) = veqZ_T(3,1:6)
IF (Nveq_rv == 6) Then
      Smatrix_T(j+1,21:26) = veqZ_T(4,1:6)
      Smatrix_T(j+1,27:32) = veqZ_T(5,1:6)
      Smatrix_T(j+1,33:38) = veqZ_T(6,1:6)
END IF
! Sensitivity matrix in ITRS
      Pmatrix_T(j+1,1) = orbc(j+1,1)
      Pmatrix_T(j+1,2) = orbc(j+1,2)
      i1 = 0
      iparam = 0  
      Do i1 = 1 , Nveq_rv 
      Do iparam = 1 , Nparam
         Pmatrix_T(j+1, 2+(i1-1)*Nparam+iparam) = veqP_T(i1,iparam) 
      End Do
      End Do
! ----------------------------------------------------------------------

End DO
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Runge-Kutta RK8(7)-13 method 
! ----------------------------------------------------------------------
Else If (integID == RK8) Then


TT = to_sec
yn = 0.d0
Do j = 1 , Nepochs-1
      ! Epoch to  
    mjd_0 = orbc(j,1) 
    yo = orbc(j,3:8) 
      
    ! Numerical integration for computation of the state vector at next epoch
    !Call integr_rk87 (mjd_0, yo, step, mjdn, yn, ey)
   
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
      
      ! VEQ matrices at the next epoch TT (to+h)
      ! State Transition Matrix
      Smatrix(j+1,1) = orbc(j+1,1)
      Smatrix(j+1,2) = orbc(j+1,2)
      Smatrix(j+1,  3:8) = veqZ(1,1:6)
      Smatrix(j+1, 9:14) = veqZ(2,1:6)
      Smatrix(j+1,15:20) = veqZ(3,1:6)
IF (Nveq_rv == 6) Then
      Smatrix(j+1,21:26) = veqZ(4,1:6)
      Smatrix(j+1,27:32) = veqZ(5,1:6)
      Smatrix(j+1,33:38) = veqZ(6,1:6)
END IF

      ! Sensitivity matrix
      Pmatrix(j+1,1) = orbc(j+1,1)
      Pmatrix(j+1,2) = orbc(j+1,2)
      !Do iparam = 1 , Nparam
      !   Pmatrix(j+1, 2+(iparam-1)*6+1 : 2+(iparam-1)*6+6 ) = & 
      !   (/ veqP(1,iparam), veqP(2,iparam), veqP(3,iparam), veqP(4,iparam), veqP(5,iparam), veqP(6,iparam) /)
      !End Do
        !stored = .false.
        !if (allocated(veqP)) then 
        !if (size(veqP, 1) == Nveq_rv) then
        !if (size(veqP, 2) == Nparam) then
      !Do i1 = 1 , 6 
      Do i1 = 1 , Nveq_rv 
      Do iparam = 1 , Nparam
         Pmatrix(j+1, 2+(i1-1)*Nparam+iparam) = veqP(i1,iparam) 
      End Do
      End Do
        !stored = .true.
        !End If
        !End If
        !End If
        !if (.not.stored) then
        !        DO i1 = 1, Nveq_rv
        !        Do iparam = 1, Nparam
        !        Pmatrix (j+1, 2+(i1-1)*Nparam+iparam) = 0.d0
        !        End Do
        !        End Do
        !End if
End DO
! ----------------------------------------------------------------------

End If


! ----------------------------------------------------------------------
! VEQ Reference System
IF (yml_veq_refsys == ITRF) THEN
      Smatrix = Smatrix_T
      Pmatrix = Pmatrix_T
END IF
! ----------------------------------------------------------------------


End Subroutine

End Module


