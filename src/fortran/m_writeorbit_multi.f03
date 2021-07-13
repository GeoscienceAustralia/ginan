MODULE m_writeorbit_multi


! ----------------------------------------------------------------------
! MODULE: m_writeorbit_multi.f03
! ----------------------------------------------------------------------
! Purpose:
!  Module for write orbit array data to output (ascii) files 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	21 March 2019
! Modified:     26 Feb 2020 Simon McClusky
! ----------------------------------------------------------------------


      use pod_yaml
      IMPLICIT NONE
      !SAVE 			

	  
Contains


SUBROUTINE writeorbit_multi (orbitsmatrix_crf,orbitsmatrix_trf,orbits_ics_icrf,PRN_array,orbpara_sigma,&
                             filename,EQMfname,VEQfname,POD_version)

! ----------------------------------------------------------------------
! SUBROUTINE: writeorbit_multi 
! ----------------------------------------------------------------------
! Purpose:
!  Write orbit and partial derivatives matrices to an output ascii file
!
!   writeorbit.f03 subroutine has been modified in order to write the 
!	estimated orbits and partial derivatives (solution of the Variational Equations) 
!   to output file (ascii) based on an internal adopted orbit format:
!   {MJD Sec_00h r(XYZ) v(XYZ) State_Transition_Matrix Sensitivity_matrix} 
! ----------------------------------------------------------------------
! Input arguments:
! - wrtArray:       Input allocatable array
! - filename:       Orbits & Partials file name to be used for writing out the orbits/partials matrices 
! - EQMfname: 		Input configuration file name for the orbit integration of the Equation of Motion  
! - VEQfname: 		Input configuration file name for the orbit integration of the Variational Equations
!
! Output arguments:
!
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou
!			Geoscience Australia, Frontier-SI
! Created:	21 March 2019
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_config
      USE mdl_param
      USE m_read_satsnx
      IMPLICIT NONE
	  
! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: orbitsmatrix_crf
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:,:), ALLOCATABLE :: orbitsmatrix_trf
      REAL (KIND = prec_q), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: orbits_ics_icrf
      REAL (KIND = prec_d), INTENT(IN), DIMENSION(:,:), ALLOCATABLE :: orbpara_sigma
      CHARACTER (LEN=3), ALLOCATABLE :: PRN_array(:)
      CHARACTER (*), INTENT(IN) :: filename
      CHARACTER (LEN=100), INTENT(IN) :: EQMfname, VEQfname
      CHARACTER (len=9), INTENT(IN) :: POD_version	  
! OUT
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, i_write
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios, ios_ith, ios_key
      INTEGER (KIND = prec_int8) :: sz1, sz2, sz3
      INTEGER (KIND = prec_int2) :: wrt_opt
      INTEGER (KIND = prec_int2) :: FMT_opt
! ----------------------------------------------------------------------
      CHARACTER (LEN=1) :: RealT
      INTEGER (KIND = prec_int2) :: RealW, RealD
      CHARACTER (LEN=70) :: fmt_wrt, fmt_wrt0, fmt_sz2
      REAL (KIND = prec_q) :: wrtArrayLN 
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: Nepochs, Nparam, Nsat, Norbits_ics_icrf 
      INTEGER (KIND = prec_int8) :: i_epoch, i_sat, N_PULSE_Param_glb
! ----------------------------------------------------------------------
      CHARACTER (LEN=100) :: param_id				
      CHARACTER (LEN=500) :: param_value				
      CHARACTER (LEN=100) :: gravity_model_filename, iau_pn_model, DE_fname_data, ocean_tides_model_file
      CHARACTER (LEN=100) :: orbit_num_integrator, EOP_sol, SE_Tides, Pole_Tide 
      CHARACTER (LEN=512) :: EOP_data
      CHARACTER (LEN=5) :: EPH_name
      INTEGER (KIND = prec_int4) :: ln
      INTEGER (KIND = prec_int2) :: Ftides
! ----------------------------------------------------------------------
      CHARACTER (LEN=3) :: PRN_isat
      !INTEGER (KIND = prec_int4) :: IY, IM, ID
      INTEGER Iyear, Imonth, Iday, J_flag
      DOUBLE PRECISION FD  
      REAL (KIND = prec_d) :: Sec_00, mjd, mjd_1, jd0
      !INTEGER (KIND = prec_int4) :: DOY
      CHARACTER (LEN=10) :: srp_model, apr_srp_model
! ----------------------------------------------------------------------
    CHARACTER(LEN=8)  :: date_mach
    CHARACTER(LEN=10) :: time_mach
    CHARACTER(LEN=5)  :: zone_mach
    INTEGER :: time_values(8)
    REAL (KIND = prec_q) :: kepler_ic(9),r_ic(3),v_ic(3)
    CHARACTER(LEN=500) :: ic_param_list
    CHARACTER(LEN=20)  :: STR1,STR2,STR3,STR4,STR5,STR6,STR7,STR8,STR9
    CHARACTER(LEN=20)  :: STR12,STR13,STR14,STR15,STR16,STR17
    character(len=3) :: time_scale
    CHARACTER(LEN=500) :: STR99
    CHARACTER(LEN=500)  :: PNUM
! ----------------------------------------------------------------------


UNIT_IN = 7  												
N_pulse_param_glb = yml_pulse_parameter_count * yml_pulse_epoch_number

! ----------------------------------------------------------------------
! Orbit arrays dimensions
sz1 = SIZE (orbitsmatrix_crf,DIM=1)
sz2 = SIZE (orbitsmatrix_crf,DIM=2)
sz3 = SIZE (orbitsmatrix_crf,DIM=3)

Nepochs = sz1
Nparam  = sz2
Nsat    = sz3
   
! PRN
Nsat = SIZE (PRN_array,DIM=1)

! orbits_ics_icrf
Norbits_ics_icrf = SIZE (orbits_ics_icrf,DIM=1)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Format definition
! ----------------------------------------------------------------------
! Orbit format: {MJD Sec_00h r(XYZ) v(XYZ)} 
!fmt_wrt = '(F25.15,F25.9,3F25.3,3F25.7)'
! Orbit-VEQ format: {PRN MJD Sec_00h r(XYZ) v(XYZ) VEQ-Z VEQ-P} 
!fmt_wrt = '(5A3,F25.12,F25.9,3F25.4,3F25.9, F25)'
fmt_wrt = '(A3,A1,F25.12,F25.9,3F25.4,3F25.9, A)'
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Open file
      OPEN (UNIT=UNIT_IN,FILE=filename,ACTION="WRITE",POSITION="REWIND", IOSTAT=ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", filename
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Write file header information
! ----------------------------------------------------------------------
WRITE (UNIT=UNIT_IN,FMT='(A,17x,A)',IOSTAT=ios_ith) '#INFO    POD Tool version :',TRIM(POD_version)
WRITE (UNIT=UNIT_IN,FMT='(A,17x,A)',IOSTAT=ios_ith) '#INFO    POD Tool output  :','Orbits & Partial Derivatives file'
CALL date_and_time(date_mach,time_mach,zone_mach,time_values)
WRITE (UNIT=UNIT_IN,FMT='(A,2x,I4,1x,I2,1x,I2,1x, I2,1x,I2,1x,I2,1x)',IOSTAT=ios_ith) &
       &'#INFO    File creation date (y/m/d/h/m/s):', &
       & time_values(1),time_values(2),time_values(3), time_values(5),time_values(6),time_values(7)      
	
! POD Tool mode
IF (yml_pod_mode == MODE_FIT) WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) &
        '#INFO    POD Tool mode:                     ', '1. Orbit Determination (using pseudo-observations)'
IF (yml_pod_mode == MODE_PREDICT) WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) &
        '#INFO    POD Tool mode:                     ', '2. Orbit Determination and Prediction'
IF (yml_pod_mode == MODE_EQM_INT) WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) &
        '#INFO    POD Tool mode:                     ', '3. Orbit Numerical Integration'
IF (yml_pod_mode == MODE_IC_INT) WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) &
        '#INFO    POD Tool mode:                     ', '4. Orbit & Partials numerical integration '
! Initial Conditions
IF (yml_ic_input_format == SP3_FILE) WRITE (UNIT=UNIT_IN,FMT='(A,A,A)',IOSTAT=ios_ith) &
        '#INFO    Initial Conditions input mode:     ','1. a-priori .sp3 orbit: ',TRIM(yml_orbit_filename)
IF (yml_ic_input_format == IC_FILE) WRITE (UNIT=UNIT_IN,FMT='(A,A,A)',IOSTAT=ios_ith) &
        '#INFO    Initial Conditions input mode:     ','2. Initial Conditions input file: ',TRIM(yml_IC_filename) 

WRITE (UNIT=UNIT_IN,FMT='(a,I9,F25.10)' ,IOSTAT=ios_ith) '#INFO    Epoch initial conditions:          ', &
                                                         INT(orbits_ics_icrf(1,1)),orbits_ics_icrf(2,1) !INT(orbitsmatrix_crf(1,1,1)),orbitsmatrix_crf(1,2,1)
WRITE (UNIT=UNIT_IN,FMT='(a,I9,F25.10)' ,IOSTAT=ios_ith) '#INFO    Epoch Start:                       ', & 
                                                         INT(orbitsmatrix_crf(1,1,1)),orbitsmatrix_crf(1,2,1) 
WRITE (UNIT=UNIT_IN,FMT='(a,I9,F25.10)' ,IOSTAT=ios_ith) '#INFO    Epoch End:                         ', &
                                                         INT(orbitsmatrix_crf(Nepochs,1,1)),orbitsmatrix_crf(Nepochs,2,1) 
WRITE (UNIT=UNIT_IN,FMT='(a,I9)'        ,IOSTAT=ios_ith) '#INFO    Tabular interval (sec):            ', abs(INT(integstep))
WRITE (UNIT=UNIT_IN,FMT='(a,i5)'        ,IOSTAT=ios_ith) '#INFO    Number of Epochs:                  ',Nepochs

! Orbit arc length (in hours) 
WRITE (UNIT=UNIT_IN,FMT='(A,A,I3,A,I3,A,I3)',IOSTAT=ios_ith) '#INFO    Orbit arc length (hours):         ',& 
								  'Orbit Determination arc: ', INT(yml_orbit_arc_determination), &
								  ' | Orbit Prediction arc: ', INT(yml_orbit_arc_prediction), &
								  ' | Backwards orbit integration arc: ', INT(yml_orbit_arc_backwards)

! Numerical Integration methods
IF (integmeth == RK7) orbit_num_integrator = 'RKN7(6)8 Runge-Kutta-Nystrom 7th order method'
IF (integmeth == RK4) orbit_num_integrator = 'Runge-Kutta 4th order'
IF (integmeth == RK8) orbit_num_integrator = 'RK8(7)13 Runge-Kutta 8th order'
WRITE (UNIT=UNIT_IN,FMT='(2A)',IOSTAT=ios_ith)   '#INFO    Numerical Integration Method:     ', orbit_num_integrator 
WRITE (UNIT=UNIT_IN,FMT='(A,I7)',IOSTAT=ios_ith) '#INFO    Numerical Integration step (sec): ', abs(INT(integstep)) 

! Satellite Dynamics model
!WRITE (UNIT=UNIT_IN,FMT='(a)'              ,IOSTAT=ios_ith) '#INFO    Model information:           [TIME_SYS] [GRAV_MODEL]&
!                                           &[PN_MODEL] [EPH_MODEL] [ALBEDO_MODEL]'
if (yml_time_scale == TT_time) time_scale = 'TT'
if (yml_time_scale == GPS_time) time_scale = 'GPS'
if (yml_time_scale == UTC_time) time_scale = 'UTC'
if (yml_time_scale == TAI_time) time_scale = 'TAI'
WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) '#INFO    Time System:                       ', time_scale
gravity_model_filename = yml_gravity_filename
!READ ( TRIM(ADJUSTR(param_value)), FMT = * , IOSTAT=ios_key ) DE_fname_data
DE_fname_data =  TRIM(yml_ephemeris_data_file) 
ln = LEN_TRIM (DE_fname_data)
WRITE (EPH_name, FMT = '(A2,A3)' , IOSTAT=ios_key ) 'DE', DE_fname_data(ln-2:ln)

! General orbit parameterization											
! FIXME: eventually we will omit this call with full yaml
Call prm_main (EQMfname, .false.)

IF (yml_tidal_effects_enabled .and. BTEST(yml_tidal_effects, solid_nonfreq - one)) THEN
SE_Tides = 'IERS 2010'
ELSE
SE_Tides = '-'
END IF
IF (yml_tidal_effects_enabled .and. BTEST(yml_tidal_effects, ocean - one)) THEN
param_id = 'ocean_tides_model_file'
ocean_tides_model_file = yml_ocean_tides_file
ELSE
ocean_tides_model_file = '-'
END IF
IF (yml_tidal_effects_enabled .and. BTEST(yml_tidal_effects, solid_pole - one)) THEN
Pole_Tide = 'IERS 2010'
ELSE
Pole_Tide = '-'
END IF
WRITE (UNIT=UNIT_IN,FMT='(A)',IOSTAT=ios_ith)   '#INFO    Satellite Dynamics Model::         ' 
WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) '#INFO    Gravity field model:               ', TRIM(gravity_model_filename)
WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) '#INFO    Planetary Ephemeris:               ', TRIM(EPH_name)
WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) '#INFO    Solid Earth Tides  :               ', TRIM(SE_Tides)
WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) '#INFO    Ocean Tides        :               ', TRIM(ocean_tides_model_file)
WRITE (UNIT=UNIT_IN,FMT='(A,A)',IOSTAT=ios_ith) '#INFO    Pole Tide          :               ', TRIM(Pole_Tide)

! Earth Orientation modelling
IF (yml_eop_option == EOP_C04T) THEN 
	EOP_sol = 'IERS C04'
	EOP_data = EOP_fname_cfg
ELSE IF (yml_eop_option == EOP_FAST) THEN
	EOP_sol = 'IERS RS/PC Daily'
	EOP_data = EOP_fname_cfg
ELSE IF (yml_eop_option == EOP_SUPER_FAST) THEN
	EOP_sol = 'IGS ultra-rapid ERP'
	EOP_data = ERP_fname_cfg
END IF
if (yml_iau_model .eq. NO_IAU) then
    iau_pn_model = "None"
else if (yml_iau_model .eq. IAU2000) then
    iau_pn_model = "IAU2000"
else
    iau_pn_model = "IAU2006A"
end if
!param_id = 'iau_pn_model'
!CALL readparam (EQM_fname_cfg, param_id, param_value)
!READ ( param_value, FMT = * , IOSTAT=ios_key ) iau_pn_model 
WRITE (UNIT=UNIT_IN,FMT='(10A)'            ,IOSTAT=ios_ith) '#INFO    Earth Orientation modelling:       ', &
											& '[EOP solution: ',trim(EOP_sol),'] ', &
											& '[EOP data file: ',trim(EOP_data),'] ', &
											& '[Precession-Nutation model: ',TRIM(iau_pn_model),']'
      
WRITE (UNIT=UNIT_IN,FMT='(a,i3)'           ,IOSTAT=ios_ith) '#INFO    Number of Satellites:              ',Nsat
WRITE (UNIT=UNIT_IN,FMT='(a,i4)'           ,IOSTAT=ios_ith) '#INFO    Number of Parameters per satellite:',NPARAM_glb+6
WRITE (UNIT=UNIT_IN,FMT='(a,i4)'           ,IOSTAT=ios_ith) '#INFO    Number of Partials:                ',Nparam-8
if (yml_veq_refsys == ITRF) then
WRITE (UNIT=UNIT_IN,FMT='(a,a4)'           ,IOSTAT=ios_ith) '#INFO    Partials Reference System:         ','ITRF'
else if (yml_veq_refsys == ICRF) then
WRITE (UNIT=UNIT_IN,FMT='(a,a4)'           ,IOSTAT=ios_ith) '#INFO    Partials Reference System:         ','ICRF'
end if
WRITE (UNIT=UNIT_IN,FMT='(a)'              ,IOSTAT=ios_ith) '#INFO    Satellite ICS:                     '
!DO i_sat = 1 , 1
DO i_sat = 1 , Nsat
   ! Read SINEX file for SVN number, Mass, ..  
   !mjd0   = INT(orbits_ics_icrf(1,i_sat))
   Sec_00 = orbits_ics_icrf(2,i_sat)
   mjd = orbits_ics_icrf(1,i_sat)
   jd0 = 2400000.5D0
   CALL iau_JD2CAL ( jd0, mjd, Iyear, Imonth, Iday, FD, J_flag )
   CALL iau_CAL2JD ( Iyear, 1, 1, jd0, mjd_1, J_flag )   
   !DOY = INT(mjd) - (mjd_1-1) 
   DOY = IDNINT(mjd-mjd_1) + 1
   YR = Iyear
   PRN_isat = PRN_array(i_sat)
   CALL read_satsnx (yml_satsinex_filename, Iyear, DOY, Sec_00, PRN_isat) 

! SRP model and SRP parameters
!---------------------------------------------------------------------------------
! A priori SRP model
! MOD
   apr_srp_model    = 'UNKNOWN'                                                                                        
   IF      (yml_apriori_srp == SRP_NONE) then
      apr_srp_model = 'NONE   '
   ELSE IF (yml_apriori_srp == SRP_CANNONBALL) then
      apr_srp_model = 'CBALL  '
   ELSE IF (yml_apriori_srp == SRP_SIMPLE_BW) then
      apr_srp_model = 'SBOXW  '
   ELSE IF (yml_apriori_srp == SRP_FULL_BW) then
      apr_srp_model = 'FBOXW  '
   END IF

   STR1 = 'X Y Z XV YV ZV'
   STR2= ''
   STR3= ''
   STR4= ''
   STR5= ''
   STR6= ''
   STR7= ''
   STR8= ''
   STR9= ''
   STR12= ''
   STR13= ''
   STR14= ''
   STR15= ''
   STR16= ''
   STR17= ''

! Setup parameterization string
    If (BTEST(yml_srp_parameters, ECOM_D_BIAS - one)) STR2 = ' D0 '
    If (BTEST(yml_srp_parameters, ECOM_Y_BIAS - one)) STR3 = ' Y0 '
    If (BTEST(yml_srp_parameters, ECOM_B_BIAS - one)) STR4 = ' B0 '
    If (BTEST(yml_srp_parameters, ECOM_D_CPR - one)) STR5 = ' DC DS '
    If (BTEST(yml_srp_parameters, ECOM_Y_CPR - one)) STR6 = ' YC YS '
    If (BTEST(yml_srp_parameters, ECOM_B_CPR - one)) STR7 = ' BC BS '
    If (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)) STR8 = ' D2C D2S '
    If (BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)) STR9 = ' D4C D4S '

    If (BTEST(yml_srp_parameters, EMP_R_BIAS - one)) STR12 = ' RB '
    If (BTEST(yml_srp_parameters, EMP_T_BIAS - one)) STR13 = ' TB '
    If (BTEST(yml_srp_parameters, EMP_N_BIAS - one)) STR14 = ' NB '
    If (BTEST(yml_srp_parameters, EMP_R_CPR - one)) STR15 = ' C1R S1R '
    If (BTEST(yml_srp_parameters, EMP_T_CPR - one)) STR16 = ' C1T S1T '
    If (BTEST(yml_srp_parameters, EMP_N_CPR - one)) STR17 = ' C1N S1N '

    STR99=''
    IF (yml_pulses) THEN
       if (yml_pulse_ref_frame == ICRF) then
       WRITE(PNUM, '(1X,A2,L1,1X,A2,L1,1X,A2,L1)') &
               'PX',BTEST(yml_pulse_parameters, DIR_X - one),&
               'PY',BTEST(yml_pulse_parameters, DIR_Y - one),&
               'PZ',BTEST(yml_pulse_parameters, DIR_Z - one)
       else if (yml_pulse_ref_frame == ORBITAL) then
       WRITE(PNUM, '(1X,A2,L1,1X,A2,L1,1X,A2,L1)') &
               'PR',BTEST(yml_pulse_parameters, DIR_R - one),&
               'PT',BTEST(yml_pulse_parameters, DIR_T - one),&
               'PN',BTEST(yml_pulse_parameters, DIR_N - one)
       end if
       STR99 = trim(STR99)//trim(PNUM)
       print*,'STR99 =', STR99
    END IF

! Estimated SRP (adjusted) parameters
   srp_model = 'NONE'
   ic_param_list = trim(STR1)
!   ic_param_list =  ''
   IF (yml_ECOM_mode /= ECOM_NONE .AND. .not. yml_EMP_mode) THEN
     IF      (yml_ECOM_mode == ECOM1) THEN
       srp_model = 'ECOM1  '
       ic_param_list = trim(STR1)//trim(STR2)//trim(STR3)//trim(STR4)//trim(STR5)//trim(STR6)//trim(STR7)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV D0 Y0 B0 DC DS YC YS BC BS'
       
     ELSE IF (yml_ECOM_mode == ECOM2) THEN
       srp_model = 'ECOM2  '   
       ic_param_list = trim(STR1)//trim(STR2)//trim(STR3)//trim(STR4)//trim(STR7)//trim(STR8)//trim(STR9)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV D0 Y0 B0 BC BS D2C D2S D4C D4S'

     ELSE IF (yml_ECOM_mode == ECOM_HYBRID) THEN
       srp_model = 'ECOM12  '
       ic_param_list = trim(STR1)//trim(STR2)//trim(STR3)//trim(STR4)//trim(STR5)//trim(STR6)//trim(STR7)//&
                     & trim(STR8)//trim(STR9)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV D0 Y0 B0 DC DS YC YS BC BS D2C D2S D4C D4S'
       
     ELSE IF (yml_ECOM_mode == SBOXW) THEN
       srp_model = 'SBOXW  '
       ic_param_list =  'X Y Z XV YV ZV DX DZ DSP Y0 B0 BC BS'
     END IF
   END IF
      
   IF (yml_EMP_mode .AND. yml_ECOM_mode == ECOM_NONE) THEN
       srp_model = 'EMPRCL '
       ic_param_list = trim(STR1)//trim(STR12)//trim(STR13)//trim(STR14)//trim(STR15)//trim(STR16)//trim(STR17)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV RB TB NB C1R S1R C1T S1T C1N S1N'
   END IF

   IF (yml_ECOM_mode /= ECOM_NONE .AND. yml_EMP_mode) THEN
      srp_model = 'ECOM + EMPRCL'
      IF      (yml_ECOM_mode == ECOM1) THEN
       ic_param_list = trim(STR1)//trim(STR12)//trim(STR13)//trim(STR14)//trim(STR15)//trim(STR16)//trim(STR17)// &
                     & trim(STR2)//trim(STR3)//trim(STR4)//trim(STR5)//trim(STR6)//trim(STR7)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV RB TB NB C1R S1R C1T S1T C1N S1N D0 Y0 B0 DC DS YC YS BC BS'

     ELSE IF (yml_ECOM_mode == ECOM2) THEN
       ic_param_list = trim(STR1)//trim(STR12)//trim(STR13)//trim(STR14)//trim(STR15)//trim(STR16)//trim(STR17)// &
                     & trim(STR2)//trim(STR3)//trim(STR4)//trim(STR7)//trim(STR8)//trim(STR9)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV RB TB NB C1R S1R C1T S1T C1N S1N D0 Y0 B0 BC BS D2C D2S D4C D4S'

     ELSE IF (yml_ECOM_mode == ECOM_HYBRID) THEN
       ic_param_list = trim(STR1)//trim(STR12)//trim(STR13)//trim(STR14)//trim(STR15)//trim(STR16)//trim(STR17)// &
                     & trim(STR2)//trim(STR3)//trim(STR4)//trim(STR5)//trim(STR6)//trim(STR7)//trim(STR8)//trim(STR9)//trim(STR99)
!       ic_param_list =  'X Y Z XV YV ZV RB TB NB C1R S1R C1T S1T C1N S1N D0 Y0 B0 DC DS YC YS BC BS D2C D2S D4C D4S'

     ELSE IF (yml_ECOM_mode == SBOXW) THEN
       ic_param_list =  'X Y Z XV YV ZV RB TB NB C1R S1R C1T S1T C1N S1N DX DZ DSPR Y0 B0 BC BS'
     END IF

   END IF

!---------------------------------------------------------------------------------

! IC INFO   
   WRITE (UNIT=UNIT_IN,FMT='(a,a,1x,a3,1x,a,1x,i3,1x,a,1x,a,1x,a,1x,F10.5,1x,a,1x,a,1x,a,1x,a,1x,i3,1x,a,1x,a)',IOSTAT=ios_ith) & 
          &'#IC_INFO ','PRN:',PRN_array(i_sat),'SVN:',SVNID,'BLK_TYP:',TRIM(BLKTYP),' MASS:',MASS, &
          &'SRP:', TRIM(apr_srp_model), TRIM(srp_model), 'Nparam:', NPARAM_glb+6, '-', trim(ic_param_list)

! IC 
   WRITE (UNIT=UNIT_IN,FMT='(a,a3,1x,I3,1x,a,1x,a,2x)',ADVANCE="no",IOSTAT=ios_ith) &
          &'#IC_XYZ  ',PRN_array(i_sat),SVNID,TRIM(BLKTYP),'ICRF'
   WRITE (UNIT=UNIT_IN,FMT='(I5,F19.10)',ADVANCE="no",IOSTAT=ios_ith) INT(orbits_ics_icrf(1,i_sat)), orbits_ics_icrf(2,i_sat)

  IF (yml_pulses) THEN
   WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) orbits_ics_icrf(3:Norbits_ics_icrf-N_PULSE_param_glb,i_sat) &
          &, PULSES_Array_sat_glb(i_sat,:,3:5) 
  ELSE
   WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) orbits_ics_icrf(3:Norbits_ics_icrf,i_sat)
  END IF

! IC PARAMETER SIGMA
   WRITE (UNIT=UNIT_IN,FMT='(a,1x,a3,1x,I3,1x,a,2x)',ADVANCE="no",IOSTAT=ios_ith)'#IC_XYZ_SIGMA',PRN_array(i_sat),SVNID,TRIM(BLKTYP)
   WRITE (UNIT=UNIT_IN,FMT='(I5,F19.10)',ADVANCE="no",IOSTAT=ios_ith)INT(orbits_ics_icrf(1,i_sat)), orbits_ics_icrf(2,i_sat)       
   WRITE (UNIT=UNIT_IN,FMT=* ,IOSTAT=ios_ith) orbpara_sigma(i_sat,1:NPARAM_glb+6)

! IC PULSE RTN
! FIXME: what if only two directions are on?
  IF (yml_pulses) THEN
     DO i = 1, N_PULSE_param_glb/yml_pulse_parameter_count
     IF (i <= 9) THEN
       STR99=''
       WRITE(PNUM, '(1X,A2,I1,1X,A2,I1,1X,A2,I1)')'PR',i,'PT',i,'PN',i
       STR99 = trim(STR99)//trim(PNUM)
       WRITE (UNIT=UNIT_IN,FMT='(a,a3,1x,I3,1x,a,1x,a,1x,a15,2x)',ADVANCE="no",IOSTAT=ios_ith) &
             &'#IC_PULSE_INFO  ',PRN_array(i_sat),SVNID,TRIM(BLKTYP),'ICRF', trim(STR99)
       WRITE (UNIT=UNIT_IN,FMT='(I5,F19.10)',ADVANCE="no",IOSTAT=ios_ith) INT(orbits_ics_icrf(1,i_sat)),&
             &orbits_ics_icrf(2,i_sat)+yml_PULSE_offset+(i-1)*yml_PULSE_interval
       WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) PULSES_Array_sat_glb(i_sat,i,3:5),&
             &orbpara_sigma(i_sat,NPARAM_glb+6-N_PULSE_param_glb+(i-1)*3+1:NPARAM_glb+6-N_PULSE_param_glb+i*3)
     ELSE 
      STR99=''
       WRITE(PNUM, '(1X,A2,I2,1X,A2,I2,1X,A2,I2)')'PR',i,'PT',i,'PN',i
       STR99 = trim(STR99)//trim(PNUM)
       WRITE (UNIT=UNIT_IN,FMT='(a,a3,1x,I3,1x,a,1x,a,1x,a15,2x)',ADVANCE="no",IOSTAT=ios_ith) &
             &'#IC_PULSE_INFO  ',PRN_array(i_sat),SVNID,TRIM(BLKTYP),'ICRF', trim(STR99)
       WRITE (UNIT=UNIT_IN,FMT='(I5,F19.10)',ADVANCE="no",IOSTAT=ios_ith) INT(orbits_ics_icrf(1,i_sat)),&
             &orbits_ics_icrf(2,i_sat)+yml_PULSE_offset+(i-1)*yml_PULSE_interval
       WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) PULSES_Array_sat_glb(i_sat,i,3:5),&
             &orbpara_sigma(i_sat,NPARAM_glb+6-N_PULSE_param_glb+(i-1)*3+1:NPARAM_glb+6-N_PULSE_param_glb+i*3)
     END IF
     END DO
  END IF

! IC Kepler
   r_ic = orbits_ics_icrf(3:5,i_sat)
   v_ic = orbits_ics_icrf(6:8,i_sat)
   CALL kepler_z2k(r_ic, v_ic, GM_global, kepler_ic)
   WRITE (UNIT=UNIT_IN,FMT='(a,a3,1x, a,2x)',ADVANCE="no",IOSTAT=ios_ith) &
          &'#IC_Kepler ',PRN_array(i_sat),'Kepler elements &
		  &[Semi-major axis(m) Eccentricity Inclination(deg) Omega_Asc.node(deg) omega_perigee(deg) True-anomaly(deg)]'	  
   WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) kepler_ic(1:6)    
END DO 
IF (yml_write_partial_velocities) THEN
!WRITE (UNIT=UNIT_IN,FMT='(a)'              ,IOSTAT=ios_ith) '#INFO PRN MJD SOD, ICRF [X Y Z ZD YD ZD], ITRF [X Y Z XD YD ZD], &
!                                           &Partials [dx/dX dx/dY dx/dZ dx/dXD dx/dYD dx/dZD dY/dX dY/dY dY/dZ &
!                                           &dY/dXD dY/dYD dY/dZD ... dx/dRAD1,dx/dRAD1,dx/dRAD3,dx/dRAD3 dx/dRAD4 &
!                                           &... dx/dRADN ... dy/dRAD1,dx/dRAD2 ... dy/dRADN ... ]'   
  WRITE (UNIT=UNIT_IN,FMT='(a)'            ,IOSTAT=ios_ith) '#INFO PRN MJD SOD, ICRF [X Y Z ZD YD ZD], ITRF [X Y Z XD YD ZD], &
                                           &Partials &
                                           &[dx/dXo dy/dXo dz/dXo   dx/dYo dy/dYo dz/dYo   &
                                           &dx/dZo dy/dZo dz/dZo   dx/dvXo dy/dvXo dz/dvXo   & 
                                           &dx/dvYo dy/dvYo dz/dvYo   dx/dvZo dy/dvZo dz/dvZo   & 
					   &dx/dRAD1 dy/dRAD1 dz/dRAD1   dx/dRAD2 dy/dRAD2 dz/dRAD2   &
                                           &dx/dRAD3 dy/dRAD3 dz/dRAD3   ...   dx/dRADN dy/dRADN dz/dRADN   & 
                                           &dvx/dXo dvy/dXo dvz/dXo   dvx/dYo dvy/dYo dvz/dYo   &
                                           &dvx/dZo dvy/dZo dvz/dZo   dvx/dvXo dvy/dvXo dvz/dvXo   & 
                                           &dvx/dvYo dvy/dvYo dvz/dvYo   dvx/dvZo dvy/dvZo dvz/dvZo   & 
					   &dvx/dRAD1 dvy/dRAD1 dvz/dRAD1   dvx/dRAD2 dvy/dRAD2 dvz/dRAD2   &
                                           &dvx/dRAD3 dvy/dRAD3 dvz/dRAD3   ...   dvx/dRADN dvy/dRADN dvz/dRADN]' 
ELSE
!WRITE (UNIT=UNIT_IN,FMT='(a)'              ,IOSTAT=ios_ith) '#INFO PRN MJD SOD, ICRF [X Y Z ZD YD ZD], ITRF [X Y Z XD YD ZD], &
!                                           &Partials [dx/dXo dx/dYo dx/dZo dx/dVxo dx/dVyo dx/dVzo &
!                                           &          dy/dXo dy/dYo dy/dZo dy/dVxo dy/dVyo dy/dVzo & 
!                                           &          dz/dXo dz/dYo dz/dZo dz/dVxo dz/dVyo dz/dVzo & 
!										   &          dx/dRAD1  dx/dRAD2  dx/dRAD3 .....  dx/dRADN &
!										   &          dy/dRAD1  dy/dRAD2  dy/dRAD3 .....  dy/dRADN &
!										   &          dz/dRAD1  dz/dRAD2  dz/dRAD3 .....  dz/dRADN ]' 

!WRITE (UNIT=UNIT_IN,FMT='(a)'              ,IOSTAT=ios_ith) '#INFO PRN MJD SOD, ICRF [X Y Z ZD YD ZD], ITRF [X Y Z XD YD ZD], &
!                                           &Partials [dx/dXo dx/dYo dx/dZo dx/dVxo dx/dVyo dx/dVzo &
!					   &          dx/dRAD1  dx/dRAD2  dx/dRAD3 .....  dx/dRADN &
!                                           &          dy/dXo dy/dYo dy/dZo dy/dVxo dy/dVyo dy/dVzo & 
!					   &          dy/dRAD1  dy/dRAD2  dy/dRAD3 .....  dy/dRADN &
!                                           &          dz/dXo dz/dYo dz/dZo dz/dVxo dz/dVyo dz/dVzo & 
!				           &          dz/dRAD1  dz/dRAD2  dz/dRAD3 .....  dz/dRADN ]' 
  WRITE (UNIT=UNIT_IN,FMT='(a)'            ,IOSTAT=ios_ith) '#INFO PRN MJD SOD, ICRF [X Y Z ZD YD ZD], ITRF [X Y Z XD YD ZD], &
                                           &Partials &
                                           &[dx/dXo dy/dXo dz/dXo   dx/dYo dy/dYo dz/dYo   &
                                           &dx/dZo dy/dZo dz/dZo   dx/dvXo dy/dvXo dz/dvXo   & 
                                           &dx/dvYo dy/dvYo dz/dvYo   dx/dvZo dy/dvZo dz/dvZo   & 
					   &dx/dRAD1 dy/dRAD1 dz/dRAD1   dx/dRAD2 dy/dRAD2 dz/dRAD2   &
                                           &dx/dRAD3 dy/dRAD3 dz/dRAD3   ...   dx/dRADN dy/dRADN dz/dRADN]' 
END IF 
                                                      
WRITE (UNIT=UNIT_IN,FMT= '(A13)' ,IOSTAT=ios_ith) 'End_of_Header'
! ----------------------------------------------------------------------
! Write data to file | Line by line	  
! ----------------------------------------------------------------------
! Write orbit-partials matrix per epoch per satellite
! ----------------------------------------------------------------------
DO i_epoch = 1 , Nepochs
	DO i_sat = 1 , Nsat

!print *,"PRN_array(i_sat)", PRN_array(i_sat)	
!print *,"orbitsmatrix_crf(i_epoch,:,i_sat)", orbitsmatrix_crf(i_epoch,:,i_sat)
	
! Based on the format definition by fmt_wrt 
!WRITE (UNIT=UNIT_IN,FMT=fmt_wrt,IOSTAT=ios_ith) PRN_array(i_sat),' ', orbitsmatrix_crf(i_epoch,:,i_sat)
!WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) PRN_array(i_sat),' ', orbitsmatrix_crf(i_epoch,1:8,i_sat), &
!                                            orbitsmatrix_trf(i_epoch,3:8,i_sat),orbitsmatrix_crf(i_epoch,9:NParam,i_sat)
WRITE (UNIT=UNIT_IN,FMT='(A3,2x)',ADVANCE="no",IOSTAT=ios_ith) PRN_array(i_sat)
WRITE (UNIT=UNIT_IN,FMT='(I5)',ADVANCE="no",IOSTAT=ios_ith)  INT(orbitsmatrix_crf(i_epoch,1,i_sat))
WRITE (UNIT=UNIT_IN,FMT='(F19.10)',ADVANCE="no",IOSTAT=ios_ith) orbitsmatrix_crf(i_epoch,2,i_sat)
WRITE (UNIT=UNIT_IN,FMT= * ,IOSTAT=ios_ith) orbitsmatrix_crf(i_epoch,3:8,i_sat), orbitsmatrix_trf(i_epoch,3:8,i_sat), & 
											orbitsmatrix_crf(i_epoch,9:NParam,i_sat)
											
IF (ios_ith /= 0) THEN
   PRINT *, "Error in writing to file: ", TRIM (filename)
   PRINT *, "WRITE IOSTAT=", ios_ith
END IF
	
	END DO
END DO


ENDFILE (UNIT = UNIT_IN) 
CLOSE (UNIT = UNIT_IN)
! ----------------------------------------------------------------------



END SUBROUTINE



End Module

