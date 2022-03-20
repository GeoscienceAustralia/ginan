MODULE m_pd_empirical


! ----------------------------------------------------------------------
! MODULE: m_pd_empirical
! ----------------------------------------------------------------------
! Purpose:
!  Module for calling the m_pd_empirical subroutine
! ----------------------------------------------------------------------
! Author : Dr. Tzupang Tseng, Geoscience Australia
!			
! Created: 11-02-2021	
! ----------------------------------------------------------------------


      IMPLICIT NONE
      !SAVE 			
  
	  
Contains


SUBROUTINE pd_empirical (rsat, vsat, GMearth, Yangle, frame, Femp, PDr, PDv, PD_param)


! ----------------------------------------------------------------------
! SUBROUTINE: pd_empirical.f03
! ----------------------------------------------------------------------
! Purpose:
!  Acceleration vector and Partial derivatives of the emprirical force model 
!  This routine is a new version and is ued to replace the old version, 
!  which was created by Dr. T. Papanikolaou (09-2018).
!  In this new version, the empirical parameters can be dynamically set up.
! ----------------------------------------------------------------------
! Input arguments:
! - rsat:			Satellite Position vector (m)   in inertial frame (ICRF)
! - vsat:			Satellite Velocity vector (m/s) in inertial frame (ICRF)
! 
! Output arguments:
! - Femp:			Acceleration vector cartesian components in inertial frame (ICRF)
! - PDr: 			Partial derivatives matrix of the acceleration w.r.t. the position vector in ICRF
! - PDv: 			Partial derivatives matrix of the acceleration w.r.t. the velocity vector in ICRF
! - PD_param: 		Partial derivatives matrix of the acceleration w.r.t. the (force-related) unknown parameters in ICRF
! ----------------------------------------------------------------------
! Author : Dr. Tzupang Tzupang, Geoscience Australia 
! 			
! Created: 11-02-2021
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
      REAL (KIND = prec_q), INTENT(IN) :: GMearth
      REAL (KIND = prec_q), INTENT(IN) :: Yangle
      INTEGER (KIND = prec_int2), INTENT(IN) :: frame
! ----------------------------------------------------------------------
! OUT
      REAL (KIND = prec_d), INTENT(OUT) :: Femp(3)
      REAL (KIND = prec_d), INTENT(OUT) :: PDr(3,3), PDv(3,3)
      REAL (KIND = prec_d), INTENT(OUT), DIMENSION(:,:), ALLOCATABLE :: PD_param
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(3) :: rsat_icrf, vsat_icrf
      REAL (KIND = prec_d) :: fx, fy, fz
      REAL (KIND = prec_q) :: ax, ay, az
      INTEGER (KIND = prec_int8) :: i , j 
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: N_param, PD_Param_ID, Param_Bias, Param_CPR
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: PD_Bias_param, PD_CPR_param 
! ----------------------------------------------------------------------
      REAL (KIND = prec_q) :: kepler(9), u_deg, u_rad
      REAL (KIND = prec_q) :: dxyz, u_deg_dx, u_deg_dy, u_deg_dz, u_rad_dx, u_rad_dy, u_rad_dz
      REAL (KIND = prec_q), DIMENSION(3) :: r_dx, r_dy, r_dz
      REAL (KIND = prec_q), DIMENSION(9) :: kepler_dx, kepler_dy, kepler_dz
! ----------------------------------------------------------------------
      REAL (KIND = prec_q), DIMENSION(:), ALLOCATABLE :: empcoef
      REAL (KIND = prec_q), DIMENSION(3) :: er,et,en,ev
! ----------------------------------------------------------------------

	  
! initialize the SRP force
! -------------------------
     DO i=1,3
         Femp(i)=0.0d0
     END DO

! ----------------------------------------------------------------------
! Partial derivatives w.r.t. unknown parameters
!N_param = NPARAM_glb
N_param = EMPNUM
If (N_param == 0) Then
N_param = 1
End If
ALLOCATE (PD_param(3,N_param), STAT = AllocateStatus)
ALLOCATE (empcoef(N_param), STAT = AllocateStatus)
! init needed
PD_param = 0.d0
empcoef = 0.d0

! Bias partial derivatives matrix allocation
PD_Param_ID = 0
if (BTEST(yml_srp_parameters, EMP_R_BIAS - one)) PD_Param_ID = PD_Param_ID + 1
if (BTEST(yml_srp_parameters, EMP_T_BIAS - one)) PD_Param_ID = PD_Param_ID + 1
if (BTEST(yml_srp_parameters, EMP_N_BIAS - one)) PD_Param_ID = PD_Param_ID + 1
if (BTEST(yml_srp_parameters, EMP_R_CPR - one)) PD_Param_ID = PD_Param_ID + 2
if (BTEST(yml_srp_parameters, EMP_T_CPR - one)) PD_Param_ID = PD_Param_ID + 2
if (BTEST(yml_srp_parameters, EMP_N_CPR - one)) PD_Param_ID = PD_Param_ID + 2

IF (EMPNUM .ne. PD_Param_ID)THEN
PRINT*, 'THE NUMBER OF FORCE PARAMETERS IS NOT CONSISTENT'
PRINT*,           'EMPNUM     =', EMPNUM
PRINT*,           'PD_Param_ID =', PD_Param_ID
PRINT*,'PROGRAM STOP AT m_pd_empirical.f90'
STOP
END IF

! Form unit vector
er(1)=rsat(1)/sqrt(rsat(1)**2+rsat(2)**2+rsat(3)**2)
er(2)=rsat(2)/sqrt(rsat(1)**2+rsat(2)**2+rsat(3)**2)
er(3)=rsat(3)/sqrt(rsat(1)**2+rsat(2)**2+rsat(3)**2)

! the orbit normal vector
!------------------------

ev(1)=vsat(1)/sqrt(vsat(1)**2+vsat(2)**2+vsat(3)**2)
ev(2)=vsat(2)/sqrt(vsat(1)**2+vsat(2)**2+vsat(3)**2)
ev(3)=vsat(3)/sqrt(vsat(1)**2+vsat(2)**2+vsat(3)**2)

CALL productcross (er,ev,en)
CALL productcross (en,er,et)

Call kepler_z2k (rsat, vsat, GMearth, kepler)
u_deg = kepler(9)
u_rad = u_deg * (PI_global / 180.D0)

        PD_Param_ID = 0
        if (BTEST(yml_srp_parameters, EMP_R_BIAS - one)) then
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*er(i)
        PD_param(i,PD_Param_ID) = er(i)
        END DO
!print*,'EMP accelerations'
!print*,'R0'
        End IF
        if (BTEST(yml_srp_parameters, EMP_T_BIAS - one)) then
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*et(i)
        PD_param(i,PD_Param_ID) = et(i)
        END DO
!print*,'T0'
        End IF
        if (BTEST(yml_srp_parameters, EMP_N_BIAS - one)) then
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*en(i)
        PD_param(i,PD_Param_ID) = en(i)
        END DO
!print*,'N0'
        End IF
! radial component
        if (BTEST(yml_srp_parameters, EMP_R_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*DCOS(u_rad)*er(i)
        PD_param(i,PD_Param_ID) = DCOS(u_rad)*er(i)
        END DO
!print*,'RC'
! S term
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*DSIN(u_rad)*er(i)
        PD_param(i,PD_Param_ID) = DSIN(u_rad)*er(i)
        END DO
!print*,'RS'
        End IF
! Along-track component
        if (BTEST(yml_srp_parameters, EMP_T_CPR - one)) then
! C term
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*DCOS(u_rad)*et(i)
        PD_param(i,PD_Param_ID) = DCOS(u_rad)*et(i)
        END DO
!print*,'TC'
! S term
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*DSIN(u_rad)*et(i)
        PD_param(i,PD_Param_ID) = DSIN(u_rad)*et(i)
        END DO
!print*,'TS'
        End IF
! Cross-track component
        if (BTEST(yml_srp_parameters, EMP_N_CPR - one)) then
! C term	
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*DCOS(u_rad)*en(i)
        PD_param(i,PD_Param_ID) = DCOS(u_rad)*en(i)
        END DO
!print*,'NC'
! S term
        PD_Param_ID = PD_Param_ID + 1
        empcoef (PD_Param_ID) = EMP_accel_glb(PD_Param_ID)
        DO i=1,3
        Femp(i) = Femp(i) + empcoef(PD_Param_ID)*DSIN(u_rad)*en(i)
        PD_param(i,PD_Param_ID) = DSIN(u_rad)*en(i)
        END DO
!print*,'NS'
        End If

PDv = 0.d0
PDr = 0.d0
END SUBROUTINE

END Module
