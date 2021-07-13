MODULE m_pd_pulses


! ----------------------------------------------------------------------
! MODULE: m_pd_pulses
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the pd_pulses subroutine
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou at Geoscience Australia
! Created:	17 August 2020
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE pd_pulses (rsat, vsat, mjd_t, t_sec, delta_v, mjd_ti, ti_sec, dir, integr_stage, Fpulse, PDr, PDv, PD_param)


! ----------------------------------------------------------------------
! SUBROUTINE: pd_pulses.f03
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
! - integr_stage:		Numerical integration method intermediate stage of computations 
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
      use pod_yaml
      IMPLICIT NONE

! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(3) :: rsat, vsat
      INTEGER (KIND = prec_int2), INTENT(IN) :: dir
      INTEGER (KIND = prec_int4), INTENT(IN) :: integr_stage
      REAL (KIND = prec_d), INTENT(IN) :: delta_v(3)
      REAL (KIND = prec_d), INTENT(IN) :: mjd_t, mjd_ti, t_sec, ti_sec 
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Fpulse (3)
      REAL (KIND = prec_d), INTENT(OUT) :: PDr(3,3), PDv(3,3)
      !REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: PD_param
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(3,1) :: PD_param
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: rsat_icrf, vsat_icrf
      REAL (KIND = prec_q) :: er(3), et(3), en(3), e_unit_dir(3) 
      REAL (KIND = prec_q) :: Rrtn(3,3), Rrtn_inv(3,3)
      REAL (KIND = prec_d) :: delta_dirac
      REAL (KIND = prec_d) :: delta_v_ti
      INTEGER (KIND = prec_int2) :: N_param_pulses
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
      INTEGER (KIND = prec_int2) :: frame_pulse
!      REAL (KIND = prec_q) :: Rrtn(3,3), Rrtn_inv(3,3)
!      REAL (KIND = prec_d) :: Rcrf_bff(3,3), Rrtn_bff(3,3)
      INTEGER (KIND = prec_int2) :: delta_t
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
!N_param_pulses = N_PULSE_param_glb
!ALLOCATE (PD_param(3,N_param_pulses), STAT = AllocateStatus)
!ALLOCATE (PD_param(3,1), STAT = AllocateStatus)
PD_param = 0.0D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! State Vector in ICRF
rsat_icrf = rsat
vsat_icrf = vsat
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Reference frame of pulse direction
! ----------------------------------------------------------------------
! 0. Intertial frame ICRF (XYZ directions)
! 1. Orbital Frame
!frame_pulse = 1
frame_pulse = yml_pulse_ref_frame
! ----------------------------------------------------------------------
 
! ----------------------------------------------------------------------
! Unit vector of the direction of the pulse:
! ----------------------------------------------------------------------
! 1. Radial direction 
! 2. Along-track
! 3. Cross-track
! ----------------------------------------------------------------------
CALL orb_frame2_unit(rsat_icrf, vsat_icrf, Rrtn, er, et, en)
! Orbital frame to Inertial frame (GCRF) : Inverse matrix
CALL matrix_inv3 (Rrtn, Rrtn_inv)

! remove warning
e_unit_dir = 0.d0
delta_v_ti = 0.d0

if (dir == 1) then
! Radial direction
!e_unit_dir = er
delta_v_ti = delta_v(1)
e_unit_dir(1) = er(1)
e_unit_dir(2) = 0.0D0
e_unit_dir(3) = 0.0D0
if (frame_pulse == ICRF) then
! X direction
e_unit_dir(1) = 1.0D0
e_unit_dir(2) = 0.0D0
e_unit_dir(3) = 0.0D0
end if

elseif (dir == 2) then
! Tangential direction
!e_unit_dir = et
delta_v_ti = delta_v(2)
e_unit_dir(1) = 0.0D0
e_unit_dir(2) = et(2)
e_unit_dir(3) = 0.0D0
if (frame_pulse == ICRF) then
! Y direction
e_unit_dir(1) = 0.0D0
e_unit_dir(2) = 1.0D0
e_unit_dir(3) = 0.0D0
end if

elseif (dir == 3) then
! Normal direction
!e_unit_dir = en
delta_v_ti = delta_v(3)
e_unit_dir(1) = 0.0D0
e_unit_dir(2) = 0.0D0
e_unit_dir(3) = en(3) 
if (frame_pulse == ICRF) then
! Z direction
e_unit_dir(1) = 0.0D0
e_unit_dir(2) = 0.0D0
e_unit_dir(3) = 1.0D0
end if

end if
! ----------------------------------------------------------------------
	

!IF ( abs(mjd_t - mjd_ti) < 1.0D-06 ) THEN
IF ( 1 < 0 ) THEN
print *,""
print *,"pd_pulses delta"
print *,"Direction", dir
print *,"delta_t", delta_t
print *,"(mjd_t),( mjd_ti)", (mjd_t), (mjd_ti) 
print *,"t_sec, ti_sec", t_sec, ti_sec
print *,"t_sec-ti_sec", t_sec - ti_sec
print *,""
print *,"pd_pulses integr_stage", integr_stage
print *,""
END IF

! ----------------------------------------------------------------------
! Dirac's "delta" function at the current epoch t (mjd_t)
! ----------------------------------------------------------------------
delta_dirac = 0.0D0

! Pulse epoch control based on input time arguments
delta_t = 2

! ----------------------------------------------------------------------
! Instant velocity pulses approach (applied only at the start of the integration step)
! ----------------------------------------------------------------------
IF (delta_t == 1) THEN
! Integration step' stage control: Allow pulse only at the initial stage (0)
IF (integr_stage == 0) THEN 
! Time difference comparison
IF ( abs(mjd_t - mjd_ti) < 1.0D-06 ) THEN
	delta_dirac = 1.0D0
END IF
END IF

ELSE IF (delta_t == 2) THEN
! Integration step' stage control: Allow pulse only at the initial stage (0)
IF (integr_stage == 0) THEN 
!IF ( INT(mjd_t) == INT( mjd_ti) ) THEN
!IF ( abs(t_sec - ti_sec) < 1.0D-05 ) THEN
! Time difference comparison
!IF ( INT(mjd_t)==INT( mjd_ti) .AND. abs(t_sec-ti_sec)<1.0D-05 ) THEN
IF ( INT(mjd_t)==INT(mjd_ti).AND.abs(t_sec-(mjd_ti-INT( mjd_ti))*86400.d0) <1.0D-05 )THEN
	delta_dirac = 1.0D0
END IF
END IF

! ----------------------------------------------------------------------
! Constant piece-wise acceleration approach (applied during the overall integration step)
! ----------------------------------------------------------------------
ELSE IF (delta_t == 3) THEN

IF (integr_stage == 0) THEN 
IF ( INT(mjd_t)==INT( mjd_ti) .AND. abs(t_sec-ti_sec)<1.0D-05 ) THEN
	delta_dirac = 1.0D0
END IF
END IF

IF (integr_stage > 0) THEN 
IF ( INT(mjd_t)==INT( mjd_ti) .AND. (t_sec>ti_sec) .AND. (t_sec<=ti_sec+integstep) ) THEN
	delta_dirac = 1.0D0
END IF
END IF

END IF
! ----------------------------------------------------------------------
!IF (delta_dirac==1.0D0) THEN
!print *,"delta_t, delta_dirac, integr_stage, t_sec, ti_sec", & 
!        delta_t,delta_dirac,integr_stage,t_sec,ti_sec
!END IF
! ----------------------------------------------------------------------

	
! ----------------------------------------------------------------------
! Summary of Pseudo-stochastic pulses	
! ----------------------------------------------------------------------
!Fpulse = delta_v_ti * delta_dirac * e_unit_dir
Fpulse(1) = delta_v_ti * delta_dirac * e_unit_dir(1)
Fpulse(2) = delta_v_ti * delta_dirac * e_unit_dir(2)
Fpulse(3) = delta_v_ti * delta_dirac * e_unit_dir(3)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Partial derivatives w.r.t state vector
PDr = 0.0D0
PDv = 0.0D0
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Partial derivatives w.r.t unknown parameters to be estimated
! ----------------------------------------------------------------------
!PD_param = delta_dirac * e_unit_dir
PD_param(1,1) = delta_dirac * e_unit_dir(1)
PD_param(2,1) = delta_dirac * e_unit_dir(2)
PD_param(3,1) = delta_dirac * e_unit_dir(3)
! ----------------------------------------------------------------------


IF (1 > 10) THEN
print *," " 
print *,"m_pd_pulses " 
print *,"delta_dirac ", delta_dirac 
print *,"delta_v_ti ", delta_v_ti 
print *,"dir ", dir 
print *,"e_unit_dir ", e_unit_dir 
print *,"Fpulse ", Fpulse 
print *,"PD_param ", PD_param
print *," "  
END IF

END SUBROUTINE

END Module
