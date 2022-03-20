MODULE m_pulses_init


! ----------------------------------------------------------------------
! MODULE: m_pulses_init
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the pulses_force subroutine
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 August 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE pulses_init (mjd_t_epoch0, t_sec_epoch0, PULSES_Array_corr)


! ----------------------------------------------------------------------
! SUBROUTINE: pulses_init.f03
! ----------------------------------------------------------------------
! Purpose:
!  Acceleration vector and Partial derivatives of pseudo-stochastic pulses (Velocity changes) 
! ----------------------------------------------------------------------
! Input arguments:
! - rsat:			Satellite Position vector (m)   in inertial frame (ICRF)
! - vsat:			Satellite Velocity vector (m/s) in inertial frame (ICRF)
! - mjd_t:			MJD of current Epoch
! - t_sec:			Seconds since start of day of current Epoch
! - delta_v:		        Pulse value
! - mjd_ti:			MJD of pulse epoch
! - ti_sec:			Seconds since start of the day of pulse' epoch
! - dir:			Pulse' direction e.g. Radial, Tangential, Normal directions, XYZ directions  
! 
! Output arguments:
! - Fpulse:		Acceleration vector cartesian components in inertial frame (ICRF)
! - PDr: 			Partial derivatives matrix of the acceleration w.r.t. the position vector in ICRF
! - PDv: 			Partial derivatives matrix of the acceleration w.r.t. the velocity vector in ICRF
! - PD_param: 		Partial derivatives matrix of the acceleration w.r.t. the (force-related) unknown parameters in ICRF
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 August 2020
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_config
      USE m_pd_pulses
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN) :: mjd_t_epoch0, t_sec_epoch0 
! ----------------------------------------------------------------------
! OUT
     ! REAL (KIND = prec_d), DIMENSION(3), INTENT(OUT) :: SFpulses
      REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: PULSES_Array_corr 
! ----------------------------------------------------------------------

! ---------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      !INTEGER (KIND = prec_int2) :: N_param_pulses
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
	  !REAL (KIND = prec_q) :: delta_v_corr(3)
      !REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: delta_v_corr
      !REAL (KIND = prec_q), DIMENSION(:,:), ALLOCATABLE :: PULSES_Array_corr 
      !INTEGER (KIND = prec_int8) :: PULSE_param, N_PULSE_param
      INTEGER (KIND = prec_int8) :: i_pulse, N_pulse_param_glb
      INTEGER (KIND = prec_int8) :: i1_pulse, i2_pulse
      REAL (KIND = prec_q) :: offset
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Number of pulses directions
!PULSE_dir_glb = PULSE_dir_vec(1) + PULSE_dir_vec(2) + PULSE_dir_vec(3)
PULSE_dir_glb = yml_pulse_parameter_count

! Number of pulses to be estimated
N_PULSE_param_glb = PULSE_dir_glb * yml_pulse_epoch_number
! ----------------------------------------------------------------------

! Allocate arrays of Pulses values
ALLOCATE (PULSES_Array_apriori_glb(yml_pulse_epoch_number, 2+PULSE_dir_glb), STAT = AllocateStatus)
ALLOCATE (PULSES_Array_aposteriori_glb(yml_pulse_epoch_number, 2+PULSE_dir_glb), STAT = AllocateStatus)
ALLOCATE (PULSES_Array_corr(yml_pulse_epoch_number, PULSE_dir_glb), STAT = AllocateStatus)
!ALLOCATE (delta_v_corr(N_PULSE_epochs * PULSE_dir_glb), STAT = AllocateStatus)
PULSES_Array_apriori_glb = 0.0D0
PULSES_Array_aposteriori_glb = 0.0D0
PULSES_Array_corr = 0.0D0
!delta_v_corr = 0.0D0

! ----------------------------------------------------------------------
! Pulses array values initialisation
! ----------------------------------------------------------------------
DO i1_pulse = 1 , yml_pulse_epoch_number
      offset = yml_pulse_offset
!print *,"offset", offset

PULSES_Array_aposteriori_glb(i1_pulse, 1) = mjd_t_epoch0 + offset/86400.0D0 + (i1_pulse-1) * (yml_pulse_interval/86400.0D0)
PULSES_Array_aposteriori_glb(i1_pulse, 2)=(PULSES_Array_aposteriori_glb(i1_pulse,1)-&
                                       & INT(PULSES_Array_aposteriori_glb(i1_pulse, 1)))*86400.d0
!print *,"mjd_t_epoch0, pulse_offest, yml_pulse_interval", mjd_t_epoch0, offset/86400.0D0, (i1_pulse-1)*(yml_pulse_interval/86400.0D0) 
!print*,'i1_pulse, offset, yml_pulse_interval =', i1_pulse, offset, yml_pulse_interval
!print*,'i1_pulse, offset, yml_pulse_interval, mjd_t_epoch0, t_sec_epoch0 =', i1_pulse, offset, yml_pulse_interval, mjd_t_epoch0, t_sec_epoch0
!print*,'mjd_t , t_sec =', PULSES_Array_aposteriori_glb(i1_pulse, 1), PULSES_Array_aposteriori_glb(i1_pulse, 2)
DO i2_pulse = 1 , PULSE_dir_glb
PULSES_Array_aposteriori_glb(i1_pulse,i2_pulse+2) = 1.0D-8
END DO

IF (yml_ic_input_format == IC_FILE) THEN
!        offset = IC_sat_pulse_glb(1,2)
PULSES_Array_aposteriori_glb(i1_pulse, 1) = mjd_t_epoch0 + offset/86400.0D0 + (i1_pulse-1) * (yml_pulse_interval/86400.0D0)
PULSES_Array_aposteriori_glb(i1_pulse, 2) = t_sec_epoch0 + i1_pulse * yml_pulse_interval !offset + (i1_pulse-1) * yml_pulse_interval
        DO i2_pulse = 1 , PULSE_dir_glb
        PULSES_Array_aposteriori_glb(i1_pulse,i2_pulse+2) = IC_sat_pulse_glb(i1_pulse,i2_pulse+2)
        END DO
!        print*,'offset, yml_pulse_interval =', offset, yml_pulse_interval
!        print*,'PULSES_Array_aposteriori_glb, i1_pulse = ', i1_pulse, PULSES_Array_aposteriori_glb(i1_pulse,:)

END IF

END DO
! ----------------------------------------------------------------------



END SUBROUTINE

END Module
