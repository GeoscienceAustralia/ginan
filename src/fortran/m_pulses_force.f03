MODULE m_pulses_force


! ----------------------------------------------------------------------
! MODULE: m_pulses_force
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


SUBROUTINE pulses_force (rsat, vsat, mjd_t, t_sec, integr_stage, SFpulses, PDr, PDv, PD_pulses_param)


! ----------------------------------------------------------------------
! SUBROUTINE: pulses_foece.f03
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
      USE m_pd_pulses
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      !INTEGER (KIND = prec_int8), INTENT(IN) :: PULSE_param 
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: rsat, vsat
      !INTEGER (KIND = prec_int2), INTENT(IN) :: dir
      INTEGER (KIND = prec_int4), INTENT(IN) :: integr_stage
      !REAL (KIND = prec_d), INTENT(IN) :: delta_v(3)
      REAL (KIND = prec_d), INTENT(IN) :: mjd_t, t_sec 
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), DIMENSION(3), INTENT(OUT) :: SFpulses
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE, INTENT(OUT) :: PD_pulses_param
      !REAL (KIND = prec_d), INTENT(OUT) :: Fpulse (3)
      REAL (KIND = prec_d), INTENT(OUT) :: PDr(3,3), PDv(3,3)
      !REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: PD_param
      !REAL (KIND = prec_d), INTENT(OUT), DIMENSION(3,1) :: PD_param
! ----------------------------------------------------------------------

! ---------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      !REAL (KIND = prec_d), DIMENSION(3) :: rsat_icrf, vsat_icrf
      REAL (KIND = prec_q) :: er(3), et(3), en(3), e_unit_dir(3) 
      !REAL (KIND = prec_q) :: Rrtn(3,3), Rrtn_inv(3,3)
      !REAL (KIND = prec_d) :: delta_dirac
      !REAL (KIND = prec_d) :: delta_v_ti
      !INTEGER (KIND = prec_int2) :: N_param_pulses
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      !INTEGER (KIND = prec_int2) :: frame_pulse
!      REAL (KIND = prec_q) :: Rrtn(3,3), Rrtn_inv(3,3)
!      REAL (KIND = prec_d) :: Rcrf_bff(3,3), Rrtn_bff(3,3)
      !INTEGER (KIND = prec_int2) :: delta_t
! ----------------------------------------------------------------------
!      REAL (KIND = prec_d), DIMENSION(3) :: SFpulses
      REAL (KIND = prec_d) :: Fpulse (3)
      REAL (KIND = prec_d) :: PD_pulse_r(3,3), PD_pulse_v(3,3)
      !REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: PD_pulse_param_i
      REAL (KIND = prec_d), DIMENSION(3,1) :: PD_pulse_param_i
!      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: PD_pulses_param
      INTEGER (KIND = prec_int8) :: N_PULSE_param, N_pulse_param_glb
      INTEGER (KIND = prec_int2) :: dir_pulse
      REAL (KIND = prec_d), DIMENSION(3) :: delta_v
      REAL (KIND = prec_d) :: mjd_ti_pulse, ti_sec_pulse
      INTEGER (KIND = prec_int8) :: N1_param, N2_param
      INTEGER (KIND = prec_int8) :: i1, i2, i_dir
      INTEGER (KIND = prec_int8) :: N_PULSE_epochs
      INTEGER (KIND= prec_int2) :: Dir_1, Dir_2, Dir_3

! ----------------------------------------------------------------------

PDr = 0.D0
PDv = 0.D0

N_PULSE_param_glb = yml_pulse_parameter_count * yml_pulse_epoch_number

ALLOCATE (PD_pulses_param(3,N_PULSE_param_glb), STAT = AllocateStatus)
IF (AllocateStatus /= 0) THEN
   PRINT *, "Error: Not enough memory"
   PRINT *, "Error: SUBROUTINE m_pd_force.f03"
   PRINT *, "Error: Allocatable Array: PD_pulses_param"
!         STOP "*** Not enough memory ***"
END IF  
PD_pulses_param = 0.0D0

!SFpulses = (/ 0.0D0, 0.0D0, 0.0D0 /) 
SFpulses(1) = 0.0D0
SFpulses(2) = 0.0D0
SFpulses(3) = 0.0D0

Fpulse(1) = 0.0D0
Fpulse(2) = 0.0D0
Fpulse(3) = 0.0D0

!! FIXME: this is highly dependent on order of directions and grouping for ref frame
if (yml_pulse_ref_frame == ICRF) then
    Dir_1 = DIR_X
    Dir_2 = DIR_Y
    Dir_3 = DIR_Z
else
    DIR_1 = DIR_R
    DIR_2 = DIR_T
    DIR_3 = DIR_N
end if

! Pulses' number of epochs
N_PULSE_epochs = size(PULSES_Array_apriori_glb, DIM = 1)
DO i1 = 1 , N_PULSE_epochs 
   mjd_ti_pulse = PULSES_Array_apriori_glb(i1,1)
   ti_sec_pulse = PULSES_Array_apriori_glb(i1,2)
	
   ! Pulses Vector at epoch ti for the selected directions
   delta_v = 0.0D0
   i_dir = 0
   IF (BTEST(yml_pulse_parameters, DIR_1 - one)) THEN
      i_dir = i_dir + 1
      delta_v(1) = PULSES_Array_apriori_glb(i1, 2+i_dir)
   END IF
   IF (BTEST(yml_pulse_parameters, DIR_2 - one)) THEN
      i_dir = i_dir + 1
      delta_v(2) = PULSES_Array_apriori_glb(i1 , 2+i_dir)
   END IF
   IF (BTEST(yml_pulse_parameters, DIR_3 - one)) THEN
      i_dir = i_dir + 1
      delta_v(3) = PULSES_Array_apriori_glb(i1 , 2+i_dir)
    END IF
        !print *,"delta_v", delta_v 
! ----------------------------------------------------------------------
    ! Pulses acceleration per individual direction and overall summary
    i_dir = 0
    DO i2 = 1 , 3
	dir_pulse = i2	
	!CALL pd_pulses (rsat, vsat, mjd_t, delta_v, mjd_ti, dir, Fpulse, PDr, PDv, PD_param)
        IF (BTEST(yml_pulse_parameters, (DIR_1 - one) + (i2 - one))) THEN
	i_dir = i_dir + 1

        ! Pulse acceleration and partials per direction defined by variable dir_pulse
        CALL pd_pulses (rsat, vsat, mjd_t, t_sec, delta_v, mjd_ti_pulse, ti_sec_pulse, dir_pulse, integr_stage, & 
                        Fpulse, PD_pulse_r, PD_pulse_v, PD_pulse_param_i)

	! Pulses acceleration summary of the directions
        SFpulses = SFpulses + Fpulse

        ! Pulses acceleration' partial derivatives w.r.t. pulses 
	PD_pulses_param(1,i_dir + (i1-1) * yml_pulse_parameter_count) = PD_pulse_param_i(1,1)
	PD_pulses_param(2,i_dir + (i1-1) * yml_pulse_parameter_count) = PD_pulse_param_i(2,1)
	PD_pulses_param(3,i_dir + (i1-1) * yml_pulse_parameter_count) = PD_pulse_param_i(3,1)

	END IF
   END DO 
END DO 	
!print *,"SFpulses", SFpulses
!print *,"PD_pulses_param", PD_pulses_param
	

!Else IF (PULSE_param == 0) Then
!	SFpulses = (/ 0.0D0, 0.0D0, 0.0D0 /)
!End IF
! ----------------------------------------------------------------------

END SUBROUTINE

END Module
