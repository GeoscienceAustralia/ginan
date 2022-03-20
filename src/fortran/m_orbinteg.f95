MODULE m_orbinteg


! ----------------------------------------------------------------------
! MODULE: m_orbinteg.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for orbit integration
!  Numerical integration of Equation of Motion and Variational Equations 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
! Created:  5 October 2017
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE                   
  
        
Contains
        
        
SUBROUTINE orbinteg (INfname, VEQmode, orbC, veqSmatrix, veqPmatrix, is_backwards)


! ----------------------------------------------------------------------
! SUBROUTINE: orbinteg.f03
! ----------------------------------------------------------------------
! Purpose:
!  Orbit Integration: Numerical integration of Equation of Motion and Variational Equations
! ----------------------------------------------------------------------
! Input arguments:
! - INfname:      Input cofiguration file name for the orbit parameterization 
! - VEQmode:      VEQmode = 0 :: Variational Equations integration is not performed
!                       VEQmode = 1 :: Variational Equations integration is performed
!
! Output arguments:
! - orbC:         Satellite orbit array in ICRF including the following per epoch:
!               - Modified Julian Day number (including the fraction of the day) 
!                       - Seconds since 00h 
!                       - Position vector (m)
!                       - Velocity vector (m/sec)
! - veqSmatrix:   State trasnition matrix obtained from the Variational Equations solution based on numerical integration methods
! - veqPmatrix: Sensitivity matrix obtained from the Variational Equations solution based on numerical integration methods
! ----------------------------------------------------------------------
! Note 1:
! The time scale of the 2 first collumns of the orbit arrays (MJD and Seoncds since 00h) 
! refer to the time system defined by the global variable TIME_SCALE in the module mdl_param.f03
! according to the input parameterization file 
! ----------------------------------------------------------------------
! Author :  Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
!
! Created:  5 October 2017
!
! Chnages:  22-05-2019  Tzupang Tseng: compute the beta angle for setting the integration step size for the eclipsed satellites    
! ----------------------------------------------------------------------
        
        
      USE mdl_precision
      USE mdl_num
      USE m_integrEQM
      USE m_integrVEQ
      USE mdl_param
      USE mdl_config
      USE pod_yaml
!      USE m_betainfo
      IMPLICIT NONE

        
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (LEN=100), INTENT(IN)  :: INfname                       
      INTEGER (KIND = prec_int2), INTENT(IN) :: VEQmode 
      logical :: is_backwards
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: orbC, veqSmatrix, veqPmatrix  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: Smatrix, Pmatrix 
      REAL (KIND = prec_d) :: MJDo
      REAL (KIND = prec_d), DIMENSION(3) :: ro, vo
!       REAL (KIND = prec_d), DIMENSION(6) :: Zo_icrf
      REAL (KIND = prec_d) :: arc
      INTEGER (KIND = prec_int2) :: integID
      REAL (KIND = prec_d) :: step
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: to_sec     
      REAL (KIND = prec_d) :: t_sec     
      INTEGER (KIND = prec_int8) :: Nepochs, i
!       , j
      INTEGER (KIND = prec_int8) :: sz1, sz2 
      INTEGER (KIND = prec_int2) :: AllocateStatus
!       , DeAllocateStatus  
        REAL (KIND = prec_d) :: mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
        REAL (KIND = prec_d) :: dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS
!       DOUBLE PRECISION EOP_cr(7)
!       DOUBLE PRECISION CRS2TRS(3,3), TRS2CRS(3,3), d_CRS2TRS(3,3), d_TRS2CRS(3,3)
!       REAL (KIND = prec_d) :: r_TRS(3), v_TRS(3)
!       REAL (KIND = prec_d) :: r_CRS(3), v_CRS(3)
!       REAL (KIND = prec_d) :: v_TRS_1(3), v_TRS_2(3), v_CRS_1(3), v_CRS_2(3)    
! ----------------------------------------------------------------------
!       INTEGER IY, IM, ID, J_flag
!       DOUBLE PRECISION sec, FD, Sec0
!       ,DJM0, 
        INTEGER (KIND = prec_int8) :: Nparam

!       REAL (KIND = prec_d) :: Xmatrix(6)
!       REAL (KIND = prec_d) :: beta0
      logical found

! ----------------------------------------------------------------------
! Read orbit parameterization
Call prm_main (INfname, VeqMode == mVEQ)
!Call prm_read (INfname)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Temp                                                                                                            ! ----------------------------------------------------------------------
SVEC_Zo = SVEC_Zo_ESTIM
!print *, "orbinteg: SVEC_Zo = ", SVEC_Zo
!Bias_accel_glb = Bias_accel_aposteriori
!CPR_CS_glb = CPR_CS_aposteriori
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Orbit integrator input parameters
! ----------------------------------------------------------------------
MJDo = MJD_to
to_sec = SEC_to
ro(1:3) = SVEC_Zo(1:3)
vo(1:3) = SVEC_Zo(4:6)
if (yaml_found) then
    if (VEQmode .eq. mEQM) then
        integID = yml_eqm_integrate_method
        step = yml_eqm_integ_stepsize
        do i=1,prn_override_count
            if (yml_prn_overrides(i)%name .eq. TRIM(PRN) .and. yml_prn_overrides(i)%integ%eqm_enabled) then
                step = yml_prn_overrides(i)%integ%eqm_stepsize
            end if
        end do
    else
        integID = yml_veq_integrate_method
        step = yml_veq_integ_stepsize
        do i=1,prn_override_count
            if (yml_prn_overrides(i)%name .eq. TRIM(PRN) .and. yml_prn_overrides(i)%integ%veq_enabled) then
                step = yml_prn_overrides(i)%integ%veq_stepsize
            end if
        end do
    endif
    if (is_backwards .and. step > 0) then
        step = -1.0D0 * step
    end if
else
    ! Just pick out the parameter file values TODO: to be removed
    integID = integmeth
    step = integstep
endif 

found = .false.
do i=1,prn_override_count
   if (yml_prn_overrides(i)%name .eq. TRIM(PRN) .and. yml_prn_overrides(i)%integ%arc_enabled) then
      arc = yml_prn_overrides(i)%integ%arc_length
      found = .true.
   end if
end do
if (.not.found) arc = orb_est_arc

! ----------------------------------------------------------------------
! compute the beta angle for setting the integration step size for the eclipsed
! satellite
!CALL betainfo (MJDo, ro, vo, beta0)
!IF (ABS(beta0)-14.d0 .lt. 0.d0)THEN
!step = 120
!END IF
!-----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Orbit integration
! ----------------------------------------------------------------------

if (VEQmode == mEQM) then

! ----------------------------------------------------------------------
! Orbit propagation based on numerical integration methods 
! ----------------------------------------------------------------------
! Numerical integration of the Equation of Motion
Call integr_EQM (MJDo, to_sec, ro, vo, arc, integID, step, orbC)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
sz1 = size(orbC, DIM = 1)
sz2 = size(orbC, DIM = 2)
!print *, "integr_EQM, sz1 = ", sz1, ", sz2 = ", sz2
ALLOCATE (veqSmatrix(sz1,sz2), STAT = AllocateStatus)
veqSmatrix = orbC
ALLOCATE (veqPmatrix(sz1,sz2), STAT = AllocateStatus)
veqPmatrix = orbC
! ----------------------------------------------------------------------


Else if (VEQmode == mVEQ) then

! ----------------------------------------------------------------------
! Variational Equations solution based on numerical integration
! ----------------------------------------------------------------------
! Number of estimated parameters (module mdl_param)
Nparam = NPARAM_glb
Call integr_VEQ (MJDo, to_sec, ro, vo, arc, integID, step, Nparam, orbc, Smatrix, Pmatrix)

sz1 = size(Smatrix, DIM = 1)
sz2 = size(Smatrix, DIM = 2)
!print *, "integr_VEQ, SMatrix sz1 = ", sz1, ", sz2 = ", sz2
ALLOCATE (veqSmatrix(sz1,sz2), STAT = AllocateStatus)
veqSmatrix = Smatrix

sz1 = size(Pmatrix, DIM = 1)
sz2 = size(Pmatrix, DIM = 2)
!print *, "integr_VEQ, PMatrix sz1 = ", sz1, ", sz2 = ", sz2
ALLOCATE (veqPmatrix(sz1,sz2), STAT = AllocateStatus)
veqPmatrix = Pmatrix
! ----------------------------------------------------------------------

End If


! ----------------------------------------------------------------------
! Time Scale transformation in orbC matrix
! ----------------------------------------------------------------------
! Time scale change is applied in case that TIME_SCALE .NOT. Terrestrial Time
! TIME_SCALE: global variable in module mdl_param.f03
! ----------------------------------------------------------------------
If (yml_time_scale /= TT_time) Then

sz1 = size(orbC, DIM = 1)
sz2 = size(orbC, DIM = 2)
Nepochs = sz1

Do i = 1 , Nepochs

! MJD in TT Time
mjd = orbC(i,1)
! Seconds since 00h
t_sec = orbC(i,2)
!print *, "t_sec ", t_sec

! Time scale: TT to GPS time
CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
Call time_TT_sec (mjd , dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS)
!print *,"dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS", dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS

! Test the TIME_SCALE global variable in mdl_param.f03      
If (yml_time_scale == GPS_time) then
      mjd = mjd_GPS
      t_sec = t_sec - (dt_TT_TAI + dt_TAI_GPS)
Else if (yml_time_scale == UTC_time) then
      mjd = mjd_UTC           
      t_sec = t_sec - (dt_TT_TAI + dt_TAI_UTC)
Else if (yml_time_scale == TAI_time) then
      mjd = mjd_TAI
      t_sec = t_sec - (dt_TT_TAI)   
End If      
!print *, "TIME_SCALE ", TIME_SCALE
!print *, "(dt_TT_TAI + dt_TAI_GPS) ", (dt_TT_TAI + dt_TAI_GPS)
!print *, "t_sec ", t_sec

! Seconds since 00h
!t_sec = (mjd - INT(mjd)) * (24.D0 * 3600.D0)
! Seconds since 00h
If (t_sec >= 86400.D0) Then
      t_sec = t_sec - INT(t_sec / 86400.D0) * 86400.D0
End IF

! Time scale change in orbit matrix and VEQ matrices 
orbC(i,1) = mjd
orbC(i,2) = t_sec
If (VEQmode == 1) Then
      veqSmatrix(i,1) = mjd
      veqSmatrix(i,2) = t_sec
      veqPmatrix(i,1) = mjd
      veqPmatrix(i,2) = t_sec
End IF
 
End Do

End If
! ----------------------------------------------------------------------


END SUBROUTINE

End

