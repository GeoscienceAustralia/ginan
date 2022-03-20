SUBROUTINE prm_main (PRMfname, isVeq)


! ----------------------------------------------------------------------
! SUBROUTINE: prm_main.f03
! ----------------------------------------------------------------------
! Purpose:
!  Read the orbit parameterization based on the input configuration file 
! ----------------------------------------------------------------------
! Author :	Dr. Thomas Papanikolaou, Cooperative Research Centre for Spatial Information, Australia
!
! Created:	04 October 2017
!
! Changes       07-12-2018 Tzupang Tseng: added a condition for the NPARAM_glb when the SRP estimation
!                                             is switched on.
!               21-02-2019 Tzupang Tseng: added a function capable of switching
!                                         on and off some parameters in ECOM models
!               03-12-2019 Tzupang Tseng: added a function of estimating the
!                                         parameters in simple box wing model
! 				17-08-2020 Dr. Thomas Papanikolaou: pseudo-stachastic pulses added  
! ----------------------------------------------------------------------


      USE mdl_precision
      USE mdl_num
      USE mdl_param
      USE mdl_eop
      USE mdl_arr
      USE m_eop_data
      USE mdl_config
      USE pod_yaml
      IMPLICIT NONE


! ----------------------------------------------------------------------
! Dummy arguments declaration
! ----------------------------------------------------------------------
! IN
      CHARACTER (*), INTENT(IN) :: PRMfname 
      logical, intent(in) :: isVeq
! OUT

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Local variables declaration
! ----------------------------------------------------------------------
      CHARACTER (LEN=50) :: REF_Zo, time_system, Sat_PRN
      INTEGER (KIND = prec_int4) :: time_in
      CHARACTER (LEN=1) :: GNSSid
      INTEGER (KIND = 4) :: PRN_sp3
      INTEGER (KIND = 4) :: PRN_eclips

      CHARACTER (LEN=7) :: integrator

      REAL (KIND = prec_d) :: Zo(6), ro(3), vo(3) 
      INTEGER IY, IM, ID, J_flag
      DOUBLE PRECISION DJM0, Sec, FD
      REAL (KIND = prec_d) :: mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC
      REAL (KIND = prec_d) :: t_sec     
	  REAL (KIND = prec_d) :: dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS

      REAL (KIND = prec_d) :: r_TRS(3), v_TRS(3)
      REAL (KIND = prec_d) :: r_CRS(3), v_CRS(3), v_CRS_1(3), v_CRS_2(3)

      REAL (KIND = prec_d) :: EOP_cr(7)
      REAL (KIND = prec_d) :: CRS2TRS(3,3), TRS2CRS(3,3)
      REAL (KIND = prec_d) :: d_CRS2TRS(3,3), d_TRS2CRS(3,3)

      REAL (KIND = prec_d) :: GM

      CHARACTER (LEN=100) :: fmt_line
      INTEGER (KIND = prec_int8) :: IEMP, IECOM
! ----------------------------------------------------------------------
      INTEGER (KIND = prec_int8) :: i, read_i
      INTEGER (KIND = prec_int2) :: UNIT_IN, ios
      INTEGER (KIND = prec_int2) :: ios_line, ios_key, ios_data
      INTEGER (KIND = prec_int2) :: space_i, VEQ_integration_glb
      INTEGER (KIND = prec_int2) :: AllocateStatus, DeallocateStatus
      CHARACTER (LEN=7) :: Format1, Format2, Format3
      CHARACTER (LEN=500) :: line_ith  
      CHARACTER (LEN=150) :: word1_ln, word_i, t0   
! ----------------------------------------------------------------------
      REAL (KIND = prec_d), DIMENSION(:,:), ALLOCATABLE :: EOP_days   
      LOGICAL found

! ----------------------------------------------------------------------
! Orbit parameterization INPUT file read:
! ----------------------------------------------------------------------

! Init time_in and time_system to nonsense value
time_in = NO_time
time_system = ""

! Init Integrator and integmeth to nonsense value
integrator = ""
integmeth = 0

! ----------------------------------------------------------------------
      UNIT_IN = 9 
      Format1 = '(A)'
      Format2 = '(F)'
      Format3 = '(I100)'
! ----------------------------------------------------------------------
if (.not. yaml_found) then

! ----------------------------------------------------------------------
! Open .in file
      OPEN (UNIT = UNIT_IN, FILE = TRIM (PRMfname), IOSTAT = ios)
      IF (ios /= 0) THEN
         PRINT *, "Error in opening file:", PRMfname
         PRINT *, "OPEN IOSTAT=", ios
      END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Read input file
i = 0
DO

READ (UNIT=UNIT_IN,FMT=Format1,IOSTAT=ios_line) line_ith
i = i + 1
! PRINT *, "READ Line (i,ios):", i, ios_line

! ----------------------------------------------------------------------
! End of file
         IF (ios_line < 0) THEN
!            PRINT *, "End of file, i=", i
            EXIT 
         END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! 1st Word of Line ith
READ (line_ith, * , IOSTAT=ios_data) word1_ln  ! 1st word
!READ (line_ith, * , IOSTAT=ios_data) word1_ln, charN 
! ----------------------------------------------------------------------
!PRINT *, "word1_ln: ", word1_ln


! ----------------------------------------------------------------------
! Parameters Keywords read 
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Reference Frame of the input initial state vector
! 1. ITRF
! 2. ICRF
! 3. Kepler Elements (in ICRF)
If (yml_ic_input_format == IC_FILE) then
        IF (word1_ln == "Reference_frame") THEN
            READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, REF_Zo 
        END IF
        if (REF_Zo == 'ICRF') then
            yml_ic_input_refsys = ICRF
        else if (REF_Zo == 'ITRF') then
            yml_ic_input_refsys = ITRF
        else if (REF_Zo == 'Kepler') then
            yml_ic_input_refsys = KEPLERRF
        end if
else
    !SP3 file coded as if ITRF format
    yml_ic_input_refsys = ITRF
end if
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Time System of the input initial epoch:
! 1. TT
! 2. GPS time
! 3. UTC
! 4. TAI
IF (word1_ln == "Time_scale") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, time_system 

If (time_system == 'TT') Then
	time_in = TT_time
Else if (time_system == 'GPS') then
	time_in = GPS_time
Else if (time_system == 'UTC') then
	time_in = UTC_time
Else if (time_system == 'TAI') then
	time_in = TAI_time
End If	

! TIME_System: Global variable in module mdl_param.f03
! defining the time scale of the ouput files of the computed orbits
yml_time_scale = time_in
END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! GNSS Satellite PRN number 
! ----------------------------------------------------------------------
! GNSS PRN number: Constellation ID + Number e.g. G04
! ----------------------------------------------------------------------
! GNSSid:	GNSS constellation id letter
! 			G: GPS
! 			R: GLONASS
! 			E: Galileo
! 			C: BDS (BeiDou)
! 			J: QZSS
! 			S: SBAS
!
! PRN_sp3:	PRN numbering as adopted in the sp3 format (Numerical value following the GNSS constellation id letter)
! ----------------------------------------------------------------------
IF (word1_ln == "Satellite_PRN") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Sat_PRN 
PRN = TRIM(Sat_PRN)
END IF

!fmt_line = '(A1,I2.2)'
!READ (PRN, fmt_line , IOSTAT=ios) GNSSid, PRN_sp3
!print *, "GNSSid ", GNSSid
!print *, "PRN_sp3 ", PRN_sp3
! ----------------------------------------------------------------------
		 
! ----------------------------------------------------------------------
! Orbit arc length (sec)
!IF (word1_ln == "Orbit_arc_length") THEN
!   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, orbarc 
!END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Initial Conditions

! Intial Epoch
IF (word1_ln == "Year") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, IY 
END IF

IF (word1_ln == "Month") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, IM 
END IF

IF (word1_ln == "Day") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ID 
END IF

IF (word1_ln == "Seconds") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Sec 
END IF

! Initial State vector (m)      
IF (word1_ln == "state_vector") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, Zo 
END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Earth Orientation                                                       
! ----------------------------------------------------------------------
! EOP settings are saved as global variables in the module mdl_EOP.f90 (to be used by the subroutine EOP.f90)

! EOP data solution options
! 1. IERS C04
! 2. IERS RS/PC Daily (finals2000A.daily)
! 3. IGS ultra-rapid ERP + IERS RS/PC Daily (dX,dY)
if (.not. toption_on_command_line) then
IF (word1_ln == "EOP_data_sol") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, EOP_sol 
END IF
end if

! EOP filename
if (.not. roption_on_command_line) then
IF (word1_ln == "EOP_filename") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, EOP_fname 
END IF
else
    EOP_fname = yml_EOP_filename
end if

! ERP filename
IF (word1_ln == "ERP_filename") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, ERP_fname 
END IF

! EOP interpolation points
IF (word1_ln == "EOP_interpolation_points") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, EOP_Nint 
END IF

! IAU Precession-Nutation model:
! 2. IAU2000A:			iau_model = 2000
! 3. IAU2006/2000A:		iau_model = 2006
if (.not. noption_on_command_line) then
IF (word1_ln == "iau_pn_model") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, yml_iau_model 
END IF
endif 

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Numerical Integrator                                                    
! ----------------------------------------------------------------------
! Numerical Integration methods
! 1. RKN7(6)8:	Runge-Kutta-Nystrom 7th order   
! 2. RK4:		Runge-Kutta 4th order
! 3. RK8(7)13:	Runge-Kutta 8th order
IF (word1_ln == "integrator_meth") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, integrator 

If (integrator =='RKN7') Then
	integmeth = RK7
Else if (integrator =='RK4') then
	integmeth = RK4
Else if (integrator =='RK8') then
	integmeth = RK8
End IF
end if


! Numerical Integration Stepsize (in sec)
IF (word1_ln == "integrator_step") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, integstep 
END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Numerical Integration of Variational Equations
! ----------------------------------------------------------------------
IF (word1_ln == "VEQ_integration") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, VEQ_integration_glb 
   if (VEQ_integration_glb  /= 0) then
       yml_veq_integration = .true.
   else 
       yml_veq_integration = .false.
   end if
END IF
! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Orbit parameters estimation
! ----------------------------------------------------------------------
IF (word1_ln == "Estimator_procedure") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, yml_estimator_procedure
END IF

IF (word1_ln == "Estimator_Iterations") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, yml_estimator_iterations
END IF
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! External Orbit Comparison :: Optional
IF (word1_ln == "orbit_external_opt") THEN
   READ ( line_ith, FMT = * , IOSTAT=ios_key ) word_i, yml_ext_orbit_opt
END IF

! ----------------------------------------------------------------------

END DO
CLOSE (UNIT=UNIT_IN)
! Close of input parameterization file
! ----------------------------------------------------------------------
iau_model = yml_iau_model

else if (yaml_found) then
If (yml_ic_input_format == SP3_FILE) then
    !SP3 file coded as if ITRF format
    yml_ic_input_refsys = ITRF
end if
if (yml_ic_input_refsys == ITRF) Ref_Zo = "ITRF"
if (yml_ic_input_refsys == ICRF) Ref_Zo = "ICRF"
if (yml_ic_input_refsys == KEPLERRF) Ref_Zo = "Kepler"
    IY = yml_pod_data_initial_year
    IM = yml_pod_data_initial_month
    ID = yml_pod_data_initial_day
    Sec = yml_pod_data_initial_seconds
if (isVeq) then
    integmeth = yml_veq_integrate_method
    integstep = yml_veq_integ_stepsize
else
    integmeth = yml_eqm_integrate_method
    integstep = yml_eqm_integ_stepsize
endif
    if (integmeth == RK4) integrator = "RK4"
    if (integmeth == RK7) integrator = "RKN7"
    if (integmeth == RK8) integrator = "RK8"
    ERP_fname = yml_erp_filename
    EOP_fname = yml_eop_filename
    EOP_Nint = yml_eop_int_points
    EOP_sol = yml_eop_option
    iau_model = yml_iau_model
    found = .false.
    do i=1, prn_override_count
        if (yml_prn_overrides(i)%name == PRN) then
            if (.not. yml_prn_overrides(i)%integ%state_vector_enabled) then
                 !TODO: call report(FATAL)
            endif
            found = .true.
            Zo(1) = yml_prn_overrides(i)%integ%xpos
            Zo(2) = yml_prn_overrides(i)%integ%ypos
            Zo(3) = yml_prn_overrides(i)%integ%zpos
            Zo(4) = yml_prn_overrides(i)%integ%xvel
            Zo(5) = yml_prn_overrides(i)%integ%yvel
            Zo(6) = yml_prn_overrides(i)%integ%zvel
        end if
    end do
    if (.not. found) then
            !TODO: call report(FATAL)
    end if
end if

!debug: print out stuff
if (.false.) then
   print *, "integmeth = ", integrator
   print *, "integstep = ", integstep
   print *, "time_system = ", yml_time_scale
   print *, "precession model = ", yml_iau_model
   print *, "ref frame = ", Ref_Zo
   print *, "PRN = ", PRN
   print *, "Veq integration = ", yml_Veq_integration
   print *, "orbext_opt = ", yml_ext_orbit_opt
end if

! ----------------------------------------------------------------------
! Initial Conditions: Time & Reference frame transformation
! ----------------------------------------------------------------------
! MJD of input initial epoch including fraction of the day	  
CALL iau_CAL2JD ( IY, IM, ID, DJM0, mjd, J_flag )
mjd = mjd + Sec / 86400.D0	  

If (yml_time_scale == TT_time) Then

! ----------------------------------------------------------------------
! Initial Epoch in TT
MJD_to = mjd
! Seconds since 00h
SEC_to = Sec
! ----------------------------------------------------------------------

Else
t_sec = Sec
! ----------------------------------------------------------------------
! "Time Systems" transformation											 
      IF (yml_time_scale == TT_time) THEN
	     CALL time_TT (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
      ELSE IF (yml_time_scale == GPS_time) THEN 
	     CALL time_GPS (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
		 Call time_TT_sec (mjd_TT , dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS)
		 t_sec = t_sec + (dt_TT_TAI + dt_TAI_GPS)		 
      ELSE IF (yml_time_scale == UTC_time) THEN 
	     CALL time_UTC (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
		 Call time_TT_sec (mjd_TT , dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS)
		 t_sec = t_sec + (dt_TT_TAI + dt_TAI_UTC)
      ELSE IF (yml_time_scale == TAI_time) THEN 
         CALL time_TAI (mjd , mjd_TT, mjd_GPS, mjd_TAI, mjd_UTC)
		 Call time_TT_sec (mjd_TT , dt_TT_TAI, dt_TAI_UTC, dt_TAI_GPS)
		 t_sec = t_sec + (dt_TT_TAI)	
      END IF 
! ----------------------------------------------------------------------
! GPS week and Seconds of GPS week
! CALL time_GPSweek (mjd_GPS , GPS_week, GPS_wsec, GPSweek_mod1024)
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Initial Epoch in TT
MJD_to = mjd_TT
! Seconds since 00h
SEC_to = t_sec !SEC_to = (MJD_to - INT(MJD_to)) * (24.D0 * 3600.D0)
! ----------------------------------------------------------------------
!print *,"SEC_to", SEC_to, t_sec 
End If

! ----------------------------------------------------------------------
! EOP data reading and save global variables to module mdl_eop.f90
! ----------------------------------------------------------------------
CALL eop_data (mjd_TT, EOP_fname, EOP_sol, EOP_Nint , EOP_day_glb)

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Reference frame of initial state vector
! ----------------------------------------------------------------------
If (yml_ic_input_refsys == ICRF) Then

    SVEC_Zo = Zo
!print *, "1. ICRF(prm_main): fname=", PRMfname, " SVEC_Zo=", SVEC_Zo
	
Else If (yml_ic_input_refsys == ITRF) Then

r_TRS(1:3) = Zo(1:3)  
v_TRS(1:3) = Zo(4:6)  

! ICRF-ITRF transformation matrix (including derivatives) based on EOP data
CALL EOP (mjd_TT, EOP_cr, CRS2TRS, TRS2CRS, d_CRS2TRS, d_TRS2CRS)

! ITRF to ICRF 
! r_CRS = TRS2CRS * r_TRS
      CALL matrix_Rr (TRS2CRS,r_TRS , r_CRS)	  
! v_CRS = TRS2CRS * v_TRS + d_TRS2CRS * r_TRS
      CALL matrix_Rr (TRS2CRS,v_TRS , v_CRS_1)
      CALL matrix_Rr (d_TRS2CRS,r_TRS , v_CRS_2)
      v_CRS = v_CRS_1 + v_CRS_2
	  
	  SVEC_Zo(1:3) = r_CRS(1:3)	
	  SVEC_Zo(4:6) = v_CRS(1:3)	
!print *, "2. ITRF(prm_main): fname=", PRMfname, " SVEC_Zo=", SVEC_Zo

Else If (yml_ic_input_refsys == KEPLERRF) Then

! GM global value via module mdl_num.f90
	 GM = GM_global

	 !Call kepler_k2z (kepler, GM , r,v)
	 Call kepler_k2z (Zo, GM , ro, vo)
	 SVEC_Zo(1:3) = ro
	 SVEC_Zo(4:6) = vo
!print *, "3. Kepler(prm_main): fname=", PRMfname, " SVEC_Zo=", SVEC_Zo

End If
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------



! ----------------------------------------------------------------------
! Forces model settings
! ----------------------------------------------------------------------
! Gravitational Effects
Call prm_grav (PRMfname, isVeq)

! Non-gravitational Effects
Call prm_nongrav (PRMfname, isVeq)

if (.not. yaml_found) then
        yml_srp_parameters = 0
end if

! Empirical parameters/accelerations
Call prm_emp (PRMfname, isVeq)

if (.false.) then
print *, "after prm_emp yml_srp_parameters: ", yml_srp_parameters
print *, "EMP_R_Bias : ", BTEST(yml_srp_parameters, EMP_R_Bias - one)
print *, "EMP_T_Bias : ", BTEST(yml_srp_parameters, EMP_T_Bias - one)
print *, "EMP_N_Bias : ", BTEST(yml_srp_parameters, EMP_N_Bias - one)
print *, "EMP_R_CPR : ", BTEST(yml_srp_parameters, EMP_R_CPR - one)
print *, "EMP_T_CPR : ", BTEST(yml_srp_parameters, EMP_T_CPR - one)
print *, "EMP_N_CPR : ", BTEST(yml_srp_parameters, EMP_N_CPR - one)
end if 
! Solar radiation pressure parameters
Call prm_srp (PRMfname, isVeq)

if (.false.) then
print *, "after prm_srp yml_srp_parameters =", yml_srp_parameters
print *, "ECOM_D_Bias : ", BTEST(yml_srp_parameters, ECOM_D_Bias - one)
print *, "ECOM_Y_Bias : ", BTEST(yml_srp_parameters, ECOM_Y_Bias - one)
print *, "ECOM_B_Bias : ", BTEST(yml_srp_parameters, ECOM_B_Bias - one)
print *, "ECOM_D_CPR : ", BTEST(yml_srp_parameters, ECOM_D_CPR - one)
print *, "ECOM_Y_CPR : ", BTEST(yml_srp_parameters, ECOM_D_CPR - one)
print *, "ECOM_B_CPR : ", BTEST(yml_srp_parameters, ECOM_D_CPR - one)
print *, "ECOM_D_2_CPR : ", BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)
print *, "ECOM_D_4_CPR : ", BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)
end if

! ----------------------------------------------------------------------


! ----------------------------------------------------------------------
! Observation model                                                       
! ----------------------------------------------------------------------

! ----------------------------------------------------------------------

! ----------------------------------------------------------------------
! Unknow parameters to be estimated
! ----------------------------------------------------------------------
! Number of parameters to be estimated
NPARAM_glb = 0

! ----------------------------------------------------------------------
! Empirical Forces parameters
! ----------------------------------------------------------------------
IEMP = 0
IECOM = 0
! Empirical parameters
If (yml_EMP_mode) Then
! Bias parameters
        if (BTEST(yml_srp_parameters, EMP_R_BIAS - one)) then
		IEMP = IEMP + 1
	End If
        if (BTEST(yml_srp_parameters, EMP_T_BIAS - one)) then
		IEMP = IEMP + 1
	End If
        if (BTEST(yml_srp_parameters, EMP_N_BIAS - one)) then
		IEMP = IEMP + 1
	End If
! Cycle-per-revolution parameters
        if (BTEST(yml_srp_parameters, EMP_R_CPR - one)) then
		IEMP = IEMP + 2
	End If
        if (BTEST(yml_srp_parameters, EMP_T_CPR - one)) then
		IEMP = IEMP + 2
	End If
        if (BTEST(yml_srp_parameters, EMP_N_CPR - one)) then
		IEMP = IEMP + 2
	End If
EMPNUM = IEMP
NPARAM_glb = IEMP
End	If

! --------------------------------------------------------------------
! Solar radiation pressure model
! -------------------------------------------------------------------
If (yml_ECOM_mode/=ECOM_NONE .and.yml_ECOM_mode/=SBOXW) Then
! Bias parameters
        if (BTEST(yml_srp_parameters, ECOM_D_Bias - one)) then
                IECOM = IECOM + 1
        End If
        if (BTEST(yml_srp_parameters, ECOM_Y_Bias - one)) then
                IECOM = IECOM + 1
        End If
        if (BTEST(yml_srp_parameters, ECOM_B_Bias - one)) then
                IECOM = IECOM + 1
        End If
! Cycle-per-revolution parameters
        if (BTEST(yml_srp_parameters, ECOM_D_CPR - one)) then
                IECOM = IECOM + 2
        End If
        if (BTEST(yml_srp_parameters, ECOM_Y_CPR - one)) then
                IECOM = IECOM + 2
        End If
        if (BTEST(yml_srp_parameters, ECOM_B_CPR - one)) then
                IECOM = IECOM + 2
        End If
        if (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)) then
                IECOM = IECOM + 2
        End If
        if (BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)) then
                IECOM = IECOM + 2
        End If

        ECOMNUM = IECOM
        NPARAM_glb = IECOM

        IF (yml_ECOM_mode<=ECOM2.AND.ECOMNUM>9)THEN
        PRINT*,'PLEASE CHECK THE PARAMETERIZATIONS OF THE SELECTED SRP MODEL'
        PRINT*,'ECOM SRP MODEL INDEX =', yml_ECOM_mode
        PRINT*,'ECOM1=1, ECOM2=2'
        PRINT*,'ECOM1:D0,Y0,B0,DC,DS,YC,YS,BC,BS'
        PRINT*,'ECOM2:D0,Y0,B0,BC,BS,D2C,D2S,D4C,D4S'
        PRINT*,'PROGRAM:prm_main.f03'
        STOP
        
        ELSE IF (yml_ECOM_mode==ECOM1.AND.&
                (BTEST(yml_srp_parameters, ECOM_D_2_CPR - one).OR.&
                 BTEST(yml_srp_parameters, ECOM_D_4_CPR - one))) THEN
        PRINT*,'THIS IS NOT ECOM1 PARAMETERIZATIONS'
        PRINT*,'THE SELECTED PARAMETERS ARE RELATED TO ECOM2'
        PRINT*,'PLEASE CHECK THE PARAMETERIZATIONS OF THE SELECTED SRP MODEL'
        PRINT*,'ECOM SRP MODEL INDEX =', yml_ECOM_mode
        PRINT*,'ECOM_D_2_CPR =', BTEST(yml_srp_parameters, ECOM_D_2_CPR - one)
        PRINT*,'ECOM_D_4_CPR =', BTEST(yml_srp_parameters, ECOM_D_4_CPR - one)
        PRINT*,'ECOM1=1, ECOM2=2'
        PRINT*,'ECOM1:D0,Y0,B0,DC,DS,YC,YS,BC,BS'
        PRINT*,'ECOM2:D0,Y0,B0,BC,BS,D2C,D2S,D4C,D4S'
        PRINT*,'PROGRAM:prm_main.f03'
        STOP

        ELSE IF (yml_ECOM_mode==ECOM2.AND.&
                 (BTEST(yml_srp_parameters, ECOM_D_CPR - one) .or.&
                  BTEST(yml_srp_parameters, ECOM_Y_CPR - one))) then
        PRINT*,'THIS IS NOT ECOM2 PARAMETERIZATIONS'
        PRINT*,'THE SELECTED PARAMETERS ARE RELATED TO ECOM1'
        PRINT*,'PLEASE CHECK THE PARAMETERIZATIONS OF THE SELECTED SRP MODEL'
        PRINT*,'ECOM SRP MODEL INDEX =', yml_ECOM_mode
        PRINT*,'ECOM_D_CPR =', BTEST(yml_srp_parameters, ECOM_D_CPR - one)
        PRINT*,'ECOM_Y_CPR =', BTEST(yml_srp_parameters, ECOM_Y_CPR - one)
        PRINT*,'ECOM1=1, ECOM2=2'
        PRINT*,'ECOM1:D0,Y0,B0,DC,DS,YC,YS,BC,BS'
        PRINT*,'ECOM2:D0,Y0,B0,BC,BS,D2C,D2S,D4C,D4S'
        PRINT*,'PROGRAM:prm_main.f03'
        STOP

        END IF

ELSEIF (yml_ECOM_mode == SBOXW) THEN

        ECOMNUM = 7
        NPARAM_glb = 7

ELSEIF (yml_ECOM_mode < ECOM_NONE) THEN
        PRINT*,'PLEASE CHECK THE SRP MODEL SETTING IN CONFIGURATION FILE !'
        PRINT*,'PROGRAM STOP !'
        STOP
End If

IF (yml_ECOM_mode /= ECOM_NONE .AND. yml_EMP_mode) THEN
NPARAM_glb = ECOMNUM + EMPNUM
!print*,'ECOM and EMP models are used together'
!print*,'Number of empirical parameters =', EMPNUM
!print*,'Number of ECOM parameters =', ECOMNUM
!print*,'The total number (NPARAM_glb) of force-related parameters =',NPARAM_glb

END IF

! ----------------------------------------------------------------------

NPARAM_EMP_ECOM_glb = NPARAM_glb

! ----------------------------------------------------------------------
! Pseudo-stochastic pulses parameters
! ----------------------------------------------------------------------
If (yml_pulses) Then
		!NPARAM_EMP_ECOM_glb = NPARAM_glb
		NPARAM_glb = NPARAM_glb + yml_pulse_parameter_count * yml_pulse_epoch_number
END IF

! ----------------------------------------------------------------------


if (1<0) then
!if (0 .eq. 0) then
! ----------------------------------------------------------------------
PRINT *, "--------------------- INPUT ----------------------------"
PRINT *, "Reference Frame:               ", REF_Zo
PRINT *, "Time System:                   ", time_system
PRINT *, "GNSS Satellite PRN:            ", PRN
PRINT *, "Orbit arc length:              ", orbarc
PRINT *, "Epoch:                         ", IY, IM, ID, Sec
PRINT *, "ro:                 		     ", Zo(1:3)
PRINT *, "vo:                 		     ", Zo(4:6)

PRINT *, "EOP data solution ID:          ", EOP_sol
PRINT *, "EOP data file name:            ", TRIM(EOP_fname)
PRINT *, "ERP data file name:            ", TRIM(ERP_fname) 
PRINT *, "EOP interpolation points       ", EOP_Nint
PRINT *, "IAU Precession-Nutation model: ", yml_iau_model

PRINT *, "Numerical Integrator method    ", integrator
PRINT *, "Numerical Integrator ID        ", integmeth
PRINT *, "Numerical Integration step     ", integstep
PRINT *, "--------------------------------------------------------"
PRINT *, " "
! ----------------------------------------------------------------------
end if

if (allocated(EOP_days)) Deallocate(EOP_days, stat=DeallocateStatus)


END
