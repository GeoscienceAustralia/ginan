      program main_pod


! ----------------------------------------------------------------------
! Program:	main_pod.f03
! ----------------------------------------------------------------------
! Purpose:
!  Precise Orbit Determination (POD) of GNSS satellites 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, CRC-SI
! Created:	13 September 2017
! ----------------------------------------------------------------------
! POD version major modifications highlights: 
! Last modified  
! - Dr. Thomas Papanikolaou, 3 May 2018
! 	Preliminary version of GNSS dynamic orbit determination	
! - Dr. Thomas Papanikolaou, 25 June 2018
! 	Version with minor revisions
! - Dr. Thomas Papanikolaou, 30 November 2018
! 	Precise Orbit Determination (POD) version: Estimation of empirical forces parameters (bias, cycle-per-rev) that lead to mm-cm level orbital accuracy w.r.t. IGS precise orbits
! - Dr. Thomas Papanikolaou, 30 January 2019
! 	POD version upgrade: Ocean tides effect revision that has significant impact on longer orbit arcs e.g. 3 days 
! - Dr. Thomas Papanikolaou, 29 March 2019
! 	POD version upgrade to a multi-GNSS multi-satellite POD version 
! ----------------------------------------------------------------------


      USE pod_yaml
      USE mdl_precision
      USE mdl_num
      USE mdl_config
      USE mdl_param
      USE m_read_leapsec
      USE m_pod_gnss
      USE m_writeorbit_multi
      USE m_writearray
      USE m_writearray2
      USE m_write_orbres
      USE m_writeorbit
	  USE m_write_orb2sp3
	  USE m_clock_read
	  USE m_attitude_orb
	  USE m_write_orbex	  
	  USE m_satmetadata	  
	  USE m_interpclocks	  
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: CPU_t0, CPU_t1
      CHARACTER (LEN=100) :: EQMfname, VEQfname, PODfname, ORBMODfname				
      CHARACTER (LEN=100) :: EQMfname_initial, VEQfname_initial				
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orb_icrf, orb_itrf  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: veqSmatrix, veqPmatrix
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbits_ics_icrf  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: Vres  
      REAL (KIND = prec_d), DIMENSION(3) :: Vrms 	    
	  !REAL (KIND = prec_d), DIMENSION(5,6) :: stat_XYZ_extC, stat_RTN_extC, stat_Kepler_extC, stat_XYZ_extT
! ----------------------------------------------------------------------
      CHARACTER (LEN=2) :: GNSS_id
	  INTEGER (KIND = prec_int2) :: ORB_mode
! ----------------------------------------------------------------------
	  INTEGER (KIND = prec_int8) :: Nsat, isat
	  INTEGER (KIND = prec_int8) :: iepoch, iparam
	  INTEGER (KIND = prec_int8) :: i
	  INTEGER (KIND = prec_int8) :: sz1, sz2, Nepochs, N2_orb, N2_veqSmatrix, N2_veqPmatrix, N2sum  
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE :: orbits_partials_icrf  
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE :: orbits_partials_itrf  
	  CHARACTER (LEN=3), ALLOCATABLE :: PRNmatrix(:)
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeAllocateStatus  
      INTEGER (KIND = prec_int2) :: sp3_velocity_cfg, partials_velocity_cfg
	  CHARACTER (LEN=3) :: PRN_isat
	  INTEGER :: ios
      CHARACTER (LEN=512) :: orbits_fname, orbits_partials_fname				
      CHARACTER (LEN=100) :: fname_write				
      CHARACTER (LEN=512) :: filename				
! ----------------------------------------------------------------------
      CHARACTER (LEN=512) :: fname_sp3, ORBpseudobs_fname, ORBEXT_fname				
	  INTEGER :: year, month, day
	  INTEGER :: Iyear, Imonth, Iday
      REAL (KIND = prec_d) :: Sec_00 	    
! ----------------------------------------------------------------------
      CHARACTER (LEN=50) :: fname_id				
      CHARACTER (LEN=100) :: param_id				
      CHARACTER (LEN=500) :: param_value				
      REAL (KIND = prec_d) :: Zo(6) 
! ----------------------------------------------------------------------
      CHARACTER (LEN=512) :: ORB2sp3_fname				
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbit_resR  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbit_resT  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbit_resN
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: orbpara_sigma
! ----------------------------------------------------------------------
      REAL (KIND = prec_d) :: mjd
      INTEGER (KIND = prec_int8) :: GPS_week, GPSweek_mod1024
      REAL (KIND = prec_d) :: GPS_wsec, GPS_day
! ----------------------------------------------------------------------
	  INTEGER (KIND = prec_int8) :: Ncommon  
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: dorb_icrf, dorb_itrf 
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: dorb_RTN, dorb_Kepler
! ----------------------------------------------------------------------
      !REAL (KIND = prec_d) :: ORBPRED_ARC_glb
      REAL (KIND = prec_d) :: orbarc_sum
      INTEGER (KIND = prec_int2) :: IC_MODE	  	  
      CHARACTER (LEN=500) :: IC_REF				
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE :: orbdiff2
! ----------------------------------------------------------------------
      LOGICAL :: pod_config_exists
	  CHARACTER (LEN=100) :: pgm_name
! ----------------------------------------------------------------------
      CHARACTER (len=300) :: str
      INTEGER (KIND = prec_int2) :: j, k
      CHARACTER (len=9) :: POD_version
      REAL (KIND = prec_q), DIMENSION(:,:,:), ALLOCATABLE :: CLKmatrix, CLKmatrix_initial 
      CHARACTER (LEN=300) :: CLKfname
      INTEGER (KIND = prec_int2) :: CLKformat
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:,:), ALLOCATABLE :: attitude_array  
      CHARACTER (LEN=100) :: ORBEX_fname				
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int4), ALLOCATABLE :: SVN_array(:)
      CHARACTER (LEN=20), ALLOCATABLE :: BLOCK_array(:)
      REAL (KIND = prec_d), ALLOCATABLE :: MASS_array(:)
      INTEGER (KIND = prec_int4), ALLOCATABLE :: POWER_array(:)
      INTEGER (kind = prec_int2) :: EMP_param, PULSE_param
      CHARACTER (LEN=8) :: VEQ_REFSYS_cfg, PULSES_frame
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: PULSES_dir1, PULSES_dir2, PULSES_dir3

EMP_param = 0
NPARAM_glb = 0

! CPU Times
CALL cpu_time (CPU_t0)

! ----------------------------------------------------------------------
! POD Version:
POD_version = 'v.1.0.1'
! ----------------------------------------------------------------------

! init globals
CALL globals_init()

! ----------------------------------------------------------------------
! POD major configuration file
! ----------------------------------------------------------------------
! Default master POD configureation file
PODfname = 'POD.in'

! Read command line to see if non default master configuration file given
CALL read_cmdline

!only check for default config file if yaml not specified on command line
if (trim(yaml_config) .eq. '') then
! Check if non-default config file given on the command line
If ( trim(POD_fname_cfg) .ne. 'DEFAULT' ) then
	PODfname = trim(POD_fname_cfg)
End If

! Check for existance of POD config file
pod_config_exists = .true.
INQUIRE(FILE=PODfname, EXIST=pod_config_exists)

pgm_name = 'pod'
If ( .not. pod_config_exists) then
	call get_command_argument( 0, pgm_name )
    write(*,'(3a)') 'No Default config file found (POD.in)  - Type: ',trim(pgm_name),' --help'
    write(*,'(3a)') 'If using a non-default config.filename - Type: ',trim(pgm_name),' -c config.filename'
	STOP
End If
else
pgm_name = 'pod'
end if

!call get_command_argument(yaml_config)
if (trim(yaml_config) .ne. "") then
call get_yaml(yaml_config)
FMOD_GRAVFIELD = 0
if (yml_gravity_model > CENTRAL_MODEL) FMOD_GRAVFIELD = 1
end if

pgrm_name = TRIM(pgm_name)
call report ('CLEAR', pgrm_name, ' ', ' ', ' ', 0)

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------  ! 999999999999999999999999999
! ----------------------------------------------------------------------
! Temporary:: manual configuration 
! ----------------------------------------------------------------------
! Beidou case:
! 1. BDSorbtype = 'IGSO'
! 2. BDSorbtype = 'MEO'
BDSorbtype_glb = 'MEO'

! Empirical Forces reference frame:
! 1. Orbital Frame
! 2. Body-fixed frame
Frame_EmpiricalForces_glb = 1

!print *,"Frame_EmpiricalForces_glb ", Frame_EmpiricalForces_glb
!print *,"BDSorbtype_glb            ", BDSorbtype_glb
!print *,"              "
! ----------------------------------------------------------------------
! Temporary :: End of input POD configuration
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------  ! 999999999999999999999999999

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Start :: Read major configuration file POD.in
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! POD Tool mode:
! ----------------------------------------------------------------------
! 1. Orbit Determination (pseudo-observations; orbit fitting)
! 2. Orbit Determination and Prediction
! 3. Orbit Integration (Equation of Motion only)
! 4. Orbit Integration and Partials (Equation of Motion and Variational Equations)
! ----------------------------------------------------------------------
if (.not.yaml_found) then
param_id = 'POD_MODE_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_pod_mode
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Initial Conditions input mode
! ----------------------------------------------------------------------
! 1. Input a-priori orbit in sp3 format (applied as pseudo-observations)
! 2. Input file with Initial Conditions (State Vector and Parameters at initial epoch per satellite) 
! ----------------------------------------------------------------------
param_id = 'IC_input'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_ic_input_format

! Initial Conditions reference frame
param_id = 'IC_refsys'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) IC_REF_cfg

! Initial Conditions file name
param_id = 'IC_filename_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_ic_filename

! ----------------------------------------------------------------------
!print *,"IC_MODE_cfg, IC_REF_cfg", IC_MODE_cfg, IC_REF_cfg
!print *,"IC_filename_cfg", IC_filename_cfg

! ----------------------------------------------------------------------
! Configuration files of Orbit modelling (2 Basic initial files):
! ----------------------------------------------------------------------
! Equation of Motion
param_id = 'EQM_fname_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) EQM_fname_cfg 

! Variational Equations
param_id = 'VEQ_fname_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) VEQ_fname_cfg 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! POD_MODE Cases: 1 or 2  
! ----------------------------------------------------------------------
! A-priori orbit sp3 as pseudo-observations :: sp3 file name
! ----------------------------------------------------------------------
param_id = 'pseudobs_orbit_filename_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_orbit_filename
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! POD_MODE Cases: ALL  (IF orbit_external_opt .NE. 0 see EQM.in configuration file)
! ----------------------------------------------------------------------
! External Orbit Comparison :: sp3 file name
! ----------------------------------------------------------------------
param_id = 'ext_orbit_filename_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_ext_orbit_filename
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Orbit Determination arc length
! ----------------------------------------------------------------------
param_id = 'orbit_determination_arc_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_orbit_arc_determination
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! POD_MODE Cases: 2  
! ----------------------------------------------------------------------
! Orbit Prediction arc length (Seconds)
! ----------------------------------------------------------------------
param_id = 'orbit_prediction_arc_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_orbit_arc_prediction
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Orbit Propagation backwards arc length
! ----------------------------------------------------------------------
param_id = 'orbit_backwards_arc_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_orbit_arc_backwards
! ----------------------------------------------------------------------


! ---------------------------------------------------------------------------
! Earth Orientation Parameters (EOP)
! ---------------------------------------------------------------------------
! EOP data solution options:
! 1. IERS C04 										: EOP_sol=1
! 2. IERS RS/PC Daily 								: EOP_sol=2
! 3. IGS ultra-rapid ERP + IERS RS/PC Daily (dX,dY)	: EOP_sol=3
param_id = 'EOP_solution_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_eop_option

! EOP filename by IERS EOP :: Solutions 1 and 2
param_id = 'EOP_fname_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) EOP_fname_cfg 

! ERP filename (Earth Rotation Parameters by IGS) :: Solution 3
param_id = 'ERP_fname_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) ERP_fname_cfg 

! EOP data interpolation number of points	  
param_id = 'EOP_Nint_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_eop_int_points
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! IAU Precession-Nutation model:
! ---------------------------------------------------------------------------
! 1. IAU2000A:		iau_pn_model = 2000
! 2. IAU2006/2000A:	iau_pn_model = 2006
param_id = 'iau_model_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_iau_model
! ---------------------------------------------------------------------------

! ---------------------------------------------------------------------------
! Orbit Parameter Estimation
! ---------------------------------------------------------------------------
! Number of iterations
param_id = 'Estimator_Iterations_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_estimator_iterations
! ---------------------------------------------------------------------------

! ----------------------------------------------------------------------
! Write to sp3 orbit format: Option for write Satellite Velocity vector 
! ----------------------------------------------------------------------
! 0. sat_vel = 0 :: Do not write Velocity vector to sp3 orbit
! 1. sat_vel > 0 :: Write Velocity vector to sp3 orbit
param_id = 'sp3_velocity_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) sp3_velocity_cfg 
if (sp3_velocity_cfg > 0) yml_write_sp3_velocities = .true.
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Write partials of the velocity vector w.r.t. parameters into the orbits_partials output file 
! ----------------------------------------------------------------------
! 0. partials_velocity_cfg = 0 :: Do not write Velocity vector's partials elements
! 1. partials_velocity_cfg > 0 :: Write Velocity vector's partials elements
param_id = 'partials_velocity_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) partials_velocity_cfg 
if (partials_velocity_cfg > 0) yml_write_partial_velocities = .true.
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Leap Second file name:
! ----------------------------------------------------------------------
param_id = 'leapsec_filename_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_leapsecond_filename
! ----------------------------------------------------------------------

! Read Satellite infromation from SINEX file
! ----------------------------------------------------------------------
param_id = 'satsinex_filename_cfg'
Call readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_satsinex_filename
PRINT*,'satsinex_filename =', yml_satsinex_filename

!----------------------------------------------------------------------

! Flag of different models for A priori SRP value 
! ---------------------------------------------------------------------
param_id = 'SRP_MOD_arp'
Call readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_apriori_srp
IF (yml_apriori_srp == SRP_NONE) PRINT*,'no a priori SRP model'
IF (yml_apriori_srp == SRP_CANNONBALL) PRINT*,'use cannonball f0 model as a priori SRP model'
IF (yml_apriori_srp == SRP_SIMPLE_BW) PRINT*,'use simple box-wing model as a priori SRP model'
IF (yml_apriori_srp == SRP_FULL_BW) PRINT*,'use box-wing model from repro3 routines as a priori SRP model'

! Estimable SRP models
! ---------------------------------------------------------------------
! ECOM_param = 1 (ECOM1), forces estimated in D,Y,B directions
! ECOM_param = 2 (ECOM2), forces estimated in D,Y,B directions
! ECOM_param =12 (ECOM12), a hybrid ECOM1+ECOM2 model
! ECOM_param = 3 (SBOXW), forces estimated in D,Y,B,X,Z directions
! ECOM_param = 0, no parameters are estimated

param_id = 'ECOM_param'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) yml_ECOM_mode

! Estimable EMP model
! ---------------------------------------------------------------------
! EMP_param = 1, forces estimated in radial,along-track and cross-track directions
! EMP_param = 0, no parameters are estimated

param_id = 'EMP_param'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT = * , IOSTAT=ios_key ) EMP_param
    yml_EMP_mode = (EMP_param > 0)


! ----------------------------------------------------------------------
! Reference System of Variational Equations' Partials & Parameter Estimation 
! ----------------------------------------------------------------------
! 1. Celestial Reference System :: ICRS
! 2. Terrestrial Reference System :: ITRS
! ----------------------------------------------------------------------
param_id = 'VEQ_REFSYS_cfg'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT ='(A4)', IOSTAT=ios_key ) VEQ_REFSYS_cfg 
if (TRIM(VEQ_REFSYS_cfg) == "ITRS") yml_veq_refsys = ITRF
if (TRIM(VEQ_REFSYS_cfg) == "ICRS") yml_veq_refsys = ICRF
if (yml_veq_refsys == NO_REFSYS) then
    print *, "Could not interpret ", trim(VEQ_REFSYS_cfg), " as a reference system"
    STOP
end if
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Pseudo-Stochastic pulses 
! ----------------------------------------------------------------------
! Pulses parameters on/off 
param_id = 'PULSES_param'
yml_pulses = .false.
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) PULSE_param 
if (PULSE_param /= 0) yml_pulses =.true.

! Number of pulses' epochs 
param_id = 'PULSES_epochs_number'
yml_pulse_epoch_number = -1
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) yml_pulse_epoch_number

! Pulses reference frame 
param_id = 'PULSES_reference_frame'
yml_pulse_ref_frame = NO_REFSYS
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) PULSES_frame 
PULSES_frame = trim(PULSES_frame)
if (PULSES_frame == "ICRF") yml_pulse_ref_frame = ICRF
if (PULSES_frame == "ORBITAL") yml_pulse_ref_frame = ORBITAL

! Pulses direction 1 
param_id = 'PULSES_direction_1'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) PULSES_dir1 
if (PULSES_dir1 /= 0) then
   if (yml_pulse_ref_frame == ICRF) yml_pulse_parameters = IBSET(yml_pulse_parameters, DIR_X - one)
   if (yml_pulse_ref_frame == ORBITAL) yml_pulse_parameters = IBSET(yml_pulse_parameters, DIR_R - one)
   yml_pulse_parameter_count = yml_pulse_parameter_count + 1
end if


! Pulses direction 2 
param_id = 'PULSES_direction_2'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) PULSES_dir2
if (PULSES_dir2 /= 0) then
   if (yml_pulse_ref_frame == ICRF) yml_pulse_parameters = IBSET(yml_pulse_parameters, DIR_Y - one)
   if (yml_pulse_ref_frame == ORBITAL) yml_pulse_parameters = IBSET(yml_pulse_parameters, DIR_T - one)
   yml_pulse_parameter_count = yml_pulse_parameter_count + 1
end if

! Pulses direction 3 
param_id = 'PULSES_direction_3'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) PULSES_dir3 
if (PULSES_dir3 /= 0) then
   if (yml_pulse_ref_frame == ICRF) yml_pulse_parameters = IBSET(yml_pulse_parameters, DIR_Z - one)
   if (yml_pulse_ref_frame == ORBITAL) yml_pulse_parameters = IBSET(yml_pulse_parameters, DIR_N - one)
   yml_pulse_parameter_count = yml_pulse_parameter_count + 1
end if

! Pulses time interval 
param_id = 'PULSES_interval'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) yml_pulse_interval

! Pulses time offset 
param_id = 'PULSES_offset'
CALL readparam (PODfname, param_id, param_value)
READ ( param_value, FMT =*, IOSTAT=ios_key ) yml_pulse_offset
! ----------------------------------------------------------------------

else
    EOP_fname_cfg = yml_eop_filename
    ERP_fname_cfg = yml_erp_filename
end if

! ----------------------------------------------------------------------
! End :: Read major configuration file POD.in
! ----------------------------------------------------------------------
!
! ----------------------------------------------------------------------
! Read command line (again) to overwrite POD.in/yaml configfile options
CALL read_cmdline
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Major configuration parameters via "read_cmdline routine", "POD.in configuration file" and "Module mdl_config.f03" 
! ----------------------------------------------------------------------
! Debug
if (.false.) then
print*, 'Master configuration parameters:'
print*, 'POD_MODE_cfg: '               ,yml_pod_mode
print*, 'EQM_fname_cfg: '              ,trim(EQM_fname_cfg)
print*, 'VEQ_fname_cfg: '              ,trim(VEQ_fname_cfg)
print*, 'pseudobs_orbit_filename_cfg: ',trim(yml_orbit_filename) 
print*, 'ext_orbit_filename_cfg: '     ,trim(yml_ext_orbit_filename)
print*, 'orbit_determination_arc_cfg: ',yml_orbit_arc_determination
print*, 'orbit_prediction_arc_cfg: '   ,yml_orbit_arc_prediction
print*, 'EOP_solution_cfg: '           ,yml_eop_option
print*, 'EOP_fname_cfg: '              ,trim(EOP_fname_cfg)
print*, 'ERP_fname_cfg: '              ,trim(ERP_fname_cfg)
print*, 'EOP_Nint_cfg: '               ,yml_eop_int_points
print*, 'iau_model_cfg: '              ,yml_iau_model
print*, 'Estimator_Iterations_cfg: '   ,yml_estimator_iterations
print*, 'sp3_velocity_cfg: '           ,yml_write_sp3_velocities
print*, 'leapsec_filename_cfg: '       ,yml_leapsecond_filename
end if
! ----------------------------------------------------------------------

orb_est_arc       = yml_orbit_arc_determination * 3600.D0
ORBPRED_ARC_glb   = yml_orbit_arc_prediction * 3600.D0

! Debug
if (1<0) then
print *, "orb_est_arc", orb_est_arc
print *, "ORBPRED_ARC_glb", ORBPRED_ARC_glb
Print *," "
end if

! POD mode
If      (yml_pod_mode == MODE_FIT) then
Print *,"POD Tool mode: 1 :: Orbit Determination"
Else IF (yml_pod_mode == MODE_PREDICT) then 
Print *,"POD Tool mode: 2 :: Orbit Determination and Prediction"
Else IF (yml_pod_mode == MODE_EQM_INT) then 
Print *,"POD Tool mode: 3 :: Orbit Integration"
Else IF (yml_pod_mode == MODE_IC_INT) then 
Print *,"POD Tool mode: 4 :: Orbit Integration and Partials"
ELSE
Print *,"POD mode not selected, program stop"
STOP
End IF
Print *," "
! IC mode
If      (yml_ic_input_format == SP3_FILE) then
if (yml_orbit_filename == "") then
        print *,"Pseudo Observations file not set"
        STOP
end if
Print *,"Initial Conditions mode: 1 :: A-priori orbit input file: ", TRIM(yml_orbit_filename)
Else IF (yml_ic_input_format == IC_FILE) then 
if (yml_ic_filename == "") then
        print *,"initial Conditions file not set"
        STOP
end if
Print *,"Initial Conditions mode: 2 :: Initial Conditions input file: ", TRIM(yml_ic_filename)
END IF
Print *," "

! ----------------------------------------------------------------------
! Read Leap Second File
CALL read_leapsec(yml_leapsecond_filename)
!Print*,'NDAT,IDAT,DATS: ',NDAT,IDAT,DATS

! ----------------------------------------------------------------------
! Form (rewrite) the two orbit integration configuration files for 
! Equation of Motion and Variational Equations: EQM.in and VEQ.in 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Copy Initial Configuration files 
! TODO: Read the epoch start from sp3 file and update first
! ----------------------------------------------------------------------
fname_id = '0'
CALL write_prmfile2 (EQM_fname_cfg, fname_id, EQMfname)
CALL write_prmfile2 (VEQ_fname_cfg, fname_id, VEQfname)
! ----------------------------------------------------------------------
!print *,"EQMfname ", EQMfname
!print *,"VEQfname ", VEQfname
!print *,"              "

! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Start :: Rewrite configuration files
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Rewrite Configuration files :: Set POD_MODE
! ----------------------------------------------------------------------
fname_id = '1'
IF (yml_pod_mode == MODE_FIT .OR. yml_pod_mode == MODE_PREDICT) THEN
param_id = 'VEQ_integration'
param_value = '1'
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
yml_veq_integration = .true.

param_id = 'Estimator_procedure'
param_value = '1'
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
yml_estimator_procedure = 1

ELSE IF (yml_pod_mode == MODE_EQM_INT) THEN
param_id = 'VEQ_integration'
param_value = '0'
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
yml_veq_integration = .false.

param_id = 'Estimator_procedure'
param_value = '0'
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
yml_estimator_procedure = 0

ELSE IF (yml_pod_mode == MODE_IC_INT) THEN
param_id = 'VEQ_integration'
param_value = '1'
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
yml_veq_integration = .true.

param_id = 'Estimator_procedure'
param_value = '0'
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
yml_estimator_procedure = 0

END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Rewrite Configuration files :: Set orbit estimation arc length
! We don't have a PRN yet, so this must be put into a global
! ----------------------------------------------------------------------
fname_id = '1'
param_id = 'Orbit_arc_length'
write (param_value, *) orb_est_arc
!Call write_prmfile (EQMfname, fname_id, param_id, param_value)
!Call write_prmfile (VEQfname, fname_id, param_id, param_value)
orbarc = orb_est_arc

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Rewrite Configuration files :: Set a-priori orbit (pseudo-observations; orbit comparison)
! ----------------------------------------------------------------------
fname_id = '1'
param_id = 'pseudobs_filename'
param_value = yml_orbit_filename
!Call write_prmfile (EQMfname, fname_id, param_id, param_value)
!Call write_prmfile (VEQfname, fname_id, param_id, param_value)

param_id = 'orbit_filename'
param_value = yml_orbit_filename
!Call write_prmfile (EQMfname, fname_id, param_id, param_value)
!Call write_prmfile (VEQfname, fname_id, param_id, param_value)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Rewrite Configuration files :: Set Earth Orientation Modelling 
! ----------------------------------------------------------------------
fname_id = '1'
if (.not. yaml_found) then
param_id = 'EOP_data_sol'
write (param_value, *) yml_eop_option
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)

param_id = 'EOP_filename'
write (param_value, *) EOP_fname_cfg
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)

param_id = 'ERP_filename'
write (param_value, *) ERP_fname_cfg
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)

param_id = 'EOP_interpolation_points'
write (param_value, *) yml_eop_int_points
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
end if

param_id = 'iau_pn_model'
write (param_value, *) yml_iau_model
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Rewrite Configuration files :: Set Orbit Parameter Estimator number of iterations 
! ----------------------------------------------------------------------
fname_id = '1'
param_id = 'Estimator_Iterations'
write (param_value, *) yml_estimator_iterations
Call write_prmfile (EQMfname, fname_id, param_id, param_value)
Call write_prmfile (VEQfname, fname_id, param_id, param_value)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! End :: Rewrite configuration files
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! POD of the GNSS satellite constellations
! ----------------------------------------------------------------------
CALL pod_gnss (EQMfname, VEQfname, PRNmatrix, orbpara_sigma, orbits_partials_icrf, orbits_partials_itrf, &
               orbits_ics_icrf,orbit_resR, orbit_resT, orbit_resN, orbdiff2)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Clocks read from input file for passing to the write out 
! ----------------------------------------------------------------------
IF (yml_ic_input_format == SP3_FILE) THEN 
CLKformat = 1
CLKfname  = yml_orbit_filename
ELSE
CLKformat = 0
CLKfname = ''
END IF 
!print *, "calling clocks from read of ", CLKfname
CALL clock_read (CLKfname,CLKformat, PRNmatrix, orbits_partials_itrf, CLKmatrix_initial)

! Satellite clocks interpolation based on orbit integration step
!CALL interp_clocks (ORBmatrix, CLKmatrix, PRNmatrix, CLKmatrix_int)
CALL interp_clocks (orbits_partials_icrf, CLKmatrix_initial, PRNmatrix, CLKmatrix)
! ---------------------------------------------------------------------- 
!print *,"CLKformat, CLKfname ",CLKformat,CLKfname

! ----------------------------------------------------------------------
! Output filenames prefix
! ----------------------------------------------------------------------
!mjd = orbits_partials_icrf(1,1,1)
!mjd = orbits_partials_itrf(2,1,1)
mjd = orbits_ics_icrf(1,1) 
CALL time_GPSweek  (mjd, GPS_week, GPS_wsec, GPSweek_mod1024)
!CALL time_GPSweek2 (mjd, GPS_week, GPS_wsec, GPSweek_mod1024, GPS_day)
GPS_day = ( GPS_wsec/86400.0D0 )  
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Write satellite orbits and partial derivatives to one .orb output file (internal format)
! ----------------------------------------------------------------------
!orbits_partials_fname = 'orbits_partials_icrf.orb'
write (orbits_partials_fname, FMT='(A,A,A3,I4,I1,A20)') trim(yml_output_dir), "/", 'gag', (GPS_week), &
        INT(GPS_day) ,'_orbits_partials.out'
!CALL writeorbit_multi (orbits_partials_icrf, PRNmatrix, orbits_partials_fname)
CALL writeorbit_multi (orbits_partials_icrf, orbits_partials_itrf, orbits_ics_icrf, PRNmatrix, orbpara_sigma, & 
                       orbits_partials_fname, EQMfname, VEQfname, POD_version)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Write satellite orbits to sp3 format
! ----------------------------------------------------------------------
! Orbit sp3 filename
write (ORB2sp3_fname, FMT='(A,A,A3,I4,I1,A4)') trim(yml_output_dir), "/", 'gag', (GPS_week), &
        INT(GPS_day) ,'.sp3'
! ICRF
!CALL write_orb2sp3 (orbits_partials_icrf, PRNmatrix, ORB2sp3_fname, sat_vel, CLKmatrix)
! ITRF
CALL write_orb2sp3 (orbits_partials_itrf, PRNmatrix, ORB2sp3_fname, yml_write_sp3_velocities, CLKmatrix)
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Extract residual filename tag from input .sp3 filename
! ----------------------------------------------------------------------
str = trim(adjustl(yml_orbit_filename))
i = index(str, '.sp3')
k = index(str, '/', .true.) + 1
j = len(str(k:i-1))

if (yml_ext_orbit_opt > TYPE_NONE) then
! ----------------------------------------------------------------------
! Write Orbit residuals
! ----------------------------------------------------------------------
! Radial
write (filename, FMT='(A,A,A3,I4,I1,a1,a,A16)') trim(yml_output_dir), "/", 'gag', (GPS_week), INT(GPS_day), '_', &
        str(k:k+j-1), '_orbitstat_R.out'
Call writearray (orbit_resR, filename)
! Transverse
write (filename, FMT='(A,A,A3,I4,I1,a1,a,A16)') trim(yml_output_dir), "/", 'gag', (GPS_week), INT(GPS_day), '_', &
        str(k:k+j-1) ,'_orbitstat_T.out'
Call writearray (orbit_resT, filename)
! Normal
write (filename, FMT='(A,A,A3,I4,I1,a1,a,A16)') trim(yml_output_dir), "/", 'gag', (GPS_week), INT(GPS_day), '_', &
        str(k:k+j-1) ,'_orbitstat_N.out'
Call writearray (orbit_resN, filename)
! ----------------------------------------------------------------------
! Write combined orbit residuals file (RTN)
write (filename, FMT='(A,A,A3,I4,I1,a1,a,A16)') trim(yml_output_dir), "/", 'gag', (GPS_week), INT(GPS_day), '_', &
        str(k:k+j-1) ,'_orbdiff_rtn.out'
Call write_orbres (orbdiff2, filename)
! ----------------------------------------------------------------------
end if


! ---------------------------------------------------------------------- 
! Satellite Attitude
! ---------------------------------------------------------------------- 
! Satellite metadata: Block type
mjd = orbits_ics_icrf(1,1)
CALL satmetadata (PRNmatrix, mjd, yml_satsinex_filename, SVN_array, BLOCK_array, MASS_array, POWER_array)

! Satellite attitude matrix
CALL attitude_orb (orbits_partials_itrf, orbits_partials_icrf, PRNmatrix, BLOCK_array, attitude_array)

! Write satellite attitude to orbex format
! Orbex filename
write (ORBEX_fname, FMT='(A,A,A3,I4,I1,A4)') trim(yml_output_dir), "/", 'gag', (GPS_week), INT(GPS_day) ,'.obx'
! Write attitude matrix to orbex format
CALL write_orbex (attitude_array, PRNmatrix, ORBEX_fname)

! Write satellite attitude per epoch to out ascii file 
write (filename, FMT='(A,A,A3,I4,I1,A13)') trim(yml_output_dir), "/", 'gag', (GPS_week), INT(GPS_day), '_attitude.out'
Call writearray2 (attitude_array, filename)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Attitude comparison via ORBEX files: 
! ----------------------------------------------------------------------
! Read ORBEX data file 1
!CALL read_orbex (ORBEX_fname  , PRNmatrix_1, ATTmatrix_1)
! Read ORBEX data file 2
!CALL read_orbex (ORBEX_fname_2, PRNmatrix_2, ATTmatrix_2)
! Attitude numerical comparison (quaternions numerical differences) between the two ORBEX data files over the common satellites (PRN) and epochs
!CALL orbex_delta(Attitude_array1, Attitude_array2, PRN_common, att_delta, att_stat)

! Write attitude quaternions' numerical differences into ascii file
!write (filename, FMT='(A3,I4,I1,A14)') 'gag', (GPS_week), INT(GPS_day), '_att_delta.out'
!Call writearray2 (att_delta, filename)

! Write attitude quaternions comparison' statistics into ascii file
!write (filename, FMT='(A3,I4,I1,A13)') 'gag', (GPS_week), INT(GPS_day), '_att_stat.out'
!Call writearray (att_delta, filename)
! ----------------------------------------------------------------------


if (allocated(attitude_array)) Deallocate(attitude_array, stat=DeAllocateStatus)
if (allocated(CLKmatrix)) Deallocate(CLKmatrix, stat=DeAllocateStatus)
if (allocated(orbits_partials_icrf)) Deallocate(orbits_partials_icrf, stat=DeAllocateStatus)
if (allocated(orbits_partials_itrf)) Deallocate(orbits_partials_itrf, stat=DeAllocateStatus)
if (allocated(orbits_ics_icrf)) Deallocate(orbits_ics_icrf, stat=DeAllocateStatus)
if (allocated(orbit_resN)) Deallocate(orbit_resN, stat=DeAllocateStatus)
if (allocated(orbit_resR)) Deallocate(orbit_resR, stat=DeAllocateStatus)
if (allocated(orbit_resT)) Deallocate(orbit_resT, stat=DeAllocateStatus)
if (allocated(orbdiff2)) Deallocate(orbdiff2, stat=DeAllocateStatus)
if (allocated(PRNmatrix)) Deallocate(PRNmatrix, stat=DeAllocateStatus)
call globals_fini()

CALL cpu_time (CPU_t1)
PRINT *,"CPU Time (sec)", CPU_t1-CPU_t0

End Program

