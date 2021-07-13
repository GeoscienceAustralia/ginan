! This file is part of Fortran-YAML: a lightweight YAML parser written in
! object-oriented Fortran.
!
! Official repository: https://github.com/BoldingBruggeman/fortran-yaml
!
! Copyright 2013-2016 Bolding & Bruggeman ApS.
!
! This is free software: you can redistribute it and/or modify it under
! the terms of the GNU General Public License as published by the Free Software
! Foundation (https://www.gnu.org/licenses/gpl.html). It is distributed in the
! hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
! implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
! A copy of the license is provided in the COPYING file.
! -----------------------------------------------------------------------------

module pod_yaml

   use yaml_version, only: yaml_commit_id=>git_commit_id, &
                           yaml_branch_name=>git_branch_name
   use yaml
   use yaml_types
   use, intrinsic :: iso_fortran_env

   implicit none

   character(error_length) :: error
   character(256) :: path
   class (type_node), pointer :: root
   type (type_error), target :: my_error
   type (type_error), pointer :: my_error_p
   type (type_dictionary), pointer :: root_dict, pod_data_dict, pod_options_dict, eqm_options_dict, veq_options_dict
   type (type_dictionary), pointer :: srp_dict, srp_parameters_dict, integ_dict, time_scale_dict, srp_mode_dict
   type (type_dictionary), pointer :: gravity_dict, gravity_model_dict, planets_dict, tides_dict, rel_dict, non_grav_dict
   type (type_dictionary), pointer :: overrides_dict, pulse_dict
   logical yml_ext_orbit_enabled, yml_estimate_params, yml_write_sp3_velocities, yml_write_partial_velocities
   logical yml_veq_integration, yml_pulses
   integer*4 yml_estimator_procedure, yml_orbit_arc_length
   integer*2 yml_pod_mode
   integer*2 yml_ic_input_format
   integer*2 yml_ic_input_refsys, yml_veq_refsys
   integer*2 yml_ext_orbit_opt
   integer*2 yml_ext_orbit_frame
   integer*2 yml_iau_model
   integer*2 yml_time_scale
   integer*2 yml_ECOM_mode
   logical   yml_EMP_mode
   integer*2 yml_apriori_srp
   integer*2 ios_keyy
   integer*2 yml_pulse_epoch_number, yml_pulse_parameter_count, yml_pulse_ref_frame

   ! srp_parameters are a bitfield type for all variables, 3 (DYB) ECOM biases, 5 (DYB,D2,D4) ECOM cprs, 3 (RTN) EMP biases, 
   ! 3 (RTN) EMP cprs.
   ! see parameters just below
   integer*2 yml_srp_parameters !current operational set
   integer*2 yml_pulse_parameters !current operational set
   integer*2 yml_emp_cpr_count

   ! this group is the current effective set. EQM and VEQ both provide the options for the last two
   integer*2 yml_gravity_model, yml_gravity_max_degree, yml_gravity_time_max_degree 

   ! tidal effects are a bitfield type, 1 = solid_nonfreq, 2 = solid_freq, 3 = ocean, 4 = solid_pole, 5 = ocean_pole
   integer*2 yml_tidal_effects, yml_tides_max_degree

   ! non grav effects are a bitfield type, 1 = thrust, 2 = earth, 3 = solar
   integer*2 yml_non_grav_effects
   logical   yml_planetary_perturbations_enabled, yml_tidal_effects_enabled, yml_rel_effects_enabled
   logical   yml_non_grav_effects_enabled, yml_gravity_enabled
  
   ! this is the current set of eqm variables
   integer*2 yml_eqm_gravity_max_degree, yml_eqm_gravity_time_max_degree 
   integer*2 yml_eqm_ecomnum, yml_eqm_empnum
   integer*2 yml_eqm_tidal_effects, yml_eqm_tides_max_degree, yml_eqm_integrate_method
   integer*2 yml_eqm_non_grav_effects, yml_eqm_srp_parameters, yml_eqm_emp_cpr_count
   integer*4 yml_eqm_integ_stepsize, yml_eqm_srp_parameters_defined
   logical   yml_eqm_planetary_perturbations_enabled, yml_eqm_tidal_effects_enabled, yml_eqm_rel_effects_enabled
   logical   yml_eqm_non_grav_effects_enabled, yml_eqm_gravity_enabled


   ! this is the current set of veq variables
   integer*2 yml_veq_gravity_max_degree, yml_veq_gravity_time_max_degree 
   integer*2 yml_veq_ecomnum, yml_veq_empnum
   integer*2 yml_veq_tidal_effects, yml_veq_tides_max_degree, yml_veq_integrate_method
   integer*2 yml_veq_non_grav_effects, yml_veq_srp_parameters, yml_veq_emp_cpr_count
   integer*4 yml_veq_integ_stepsize, yml_veq_srp_parameters_defined
   logical   yml_veq_planetary_perturbations_enabled, yml_veq_tidal_effects_enabled, yml_veq_rel_effects_enabled
   logical   yml_veq_non_grav_effects_enabled, yml_veq_gravity_enabled
   
   logical   soption_on_command_line, ooption_on_command_line, roption_on_command_line
   logical   moption_on_command_line, noption_on_command_line, toption_on_command_line

   character(4) yml_pod_data_prn
   integer*2    yml_pod_data_ref_frame
   character(20) yml_pod_data_initial_epoch
   integer*4     yml_pod_data_initial_year
   integer*2     yml_pod_data_initial_month, yml_pod_data_initial_day
   real*8        yml_pod_data_initial_seconds !include fractional seconds
   real*8        yml_Zo(6), yml_pulse_offset, yml_pulse_interval
   character(512) yml_pod_data_state_vector

   character(512) yml_orbit_filename, yml_ext_orbit_filename, yml_satsinex_filename, yml_leapsecond_filename, yml_eop_filename
   character(512) yml_gravity_filename, yml_ephemeris_header, yml_ephemeris_data_file, yml_ocean_tides_file, yml_erp_filename
   character(512) yml_ic_filename, yml_output_dir, cmd
   integer        dir_status

   integer*4 yml_orbit_steps, yml_orbit_points, yml_orbit_arc_determination, yml_orbit_arc_prediction, yml_orbit_arc_backwards
   integer*4 yml_ext_orbit_steps, yml_ext_orbit_points, yml_eop_option, yml_eop_int_points, yml_estimator_iterations

   type :: srp_override
      integer*4 parameters, parameters_defined
      integer*2 apriori_model
      integer*2 ecom_mode
      logical   emp_mode
      integer*2 emp_cpr_count
   end type

   integer*2 max_ecom_parameters, max_emp_parameters
   ! change these values if they ever grow!!
   parameter (max_ecom_parameters=13, max_emp_parameters=9)

   type :: integration_override
      real*8  veq_stepsize, veq_iterations, eqm_stepsize, eqm_iterations
      real*8  arc_length                 ! arc_length to integrate
      real*8  xpos, ypos, zpos           ! state vector: ITRF position
      real*8  xvel, yvel, zvel           ! state vector: ITRF velocity
      real*8  ecom_init_values(max_ecom_parameters)
      real*8  emp_init_values(max_emp_parameters)
      integer*2 ecom_parameters_used, emp_parameters_used
      logical veq_enabled, eqm_enabled, arc_enabled, state_vector_enabled
   end type

   type :: override
      character(20) name ! system or block or prn
      type (srp_override) ::  srp
      type (integration_override) ::  integ
   end type

   integer*4 max_sys_overrides, max_block_overrides, max_prn_overrides

   parameter (max_sys_overrides = 8)
   parameter (max_block_overrides = 256)
   parameter (max_prn_overrides = 2048)

   integer*4 sys_override_count, block_override_count, prn_override_count

   type (override) :: yml_sys_overrides (max_sys_overrides), yml_block_overrides (max_block_overrides)
   type (override) :: yml_prn_overrides (max_prn_overrides)

   ! srp parameters bitfield definition
   integer*2 ECOM_D_bias, ECOM_Y_bias, ECOM_B_bias, EMP_R_bias, EMP_T_bias, EMP_N_bias
   integer*2 ECOM_D_cpr, ECOM_Y_cpr, ECOM_B_cpr, ECOM_D_2_cpr, ECOM_D_4_cpr, EMP_R_cpr, EMP_T_cpr, EMP_N_cpr

   parameter (ECOM_D_bias = 1, ECOM_Y_bias = 2, ECOM_B_bias = 3)
   parameter (ECOM_D_cpr = 4, ECOM_Y_cpr = 5, ECOM_B_cpr = 6)
   parameter (ECOM_D_2_cpr = 7, ECOM_D_4_cpr = 8)
   parameter (EMP_R_bias = 9, EMP_T_bias = 10, EMP_N_bias = 11)
   parameter (EMP_R_cpr = 12, EMP_T_cpr = 13, EMP_N_cpr = 14)

   ! eop parameter choices
   integer*2 EOP_NONE, EOP_C04T, EOP_FAST, EOP_SUPER_FAST
   parameter (EOP_NONE = 0, EOP_C04T = 1, EOP_FAST = 2, EOP_SUPER_FAST = 3)
   ! integration methods definition
   integer*2 RK4, RK7, RK8, RK0
   parameter (RK0 = 0, RK4 = 4, RK7 = 7, RK8 = 8)

   ! pulse directions 
   integer*2 DIR_X, DIR_Y, DIR_Z, DIR_R, DIR_T, DIR_N
   parameter (DIR_X = 1, DIR_Y = 2, DIR_Z = 3, DIR_R = 4, DIR_T = 5, DIR_N = 6)

   ! reference frames definition (not all valid in all situations)
   integer*2 ITRF, ICRF, KEPLERRF, NO_REFSYS, ORBITAL
   parameter (NO_REFSYS = 0, ITRF = 1, ICRF = 2, KEPLERRF = 3, ORBITAL = 4)

   ! initial conditions definition
   integer*2 NOT_SPECIFIED, SP3_FILE, IC_FILE
   parameter (NOT_SPECIFIED = 0, SP3_FILE = 1, IC_FILE = 2)

   ! pod modes definition
   integer*2 NO_MODE, MODE_FIT, MODE_PREDICT, MODE_EQM_INT, MODE_IC_INT
   parameter (NO_MODE = 0, MODE_FIT = 1, MODE_PREDICT = 2, MODE_EQM_INT = 3, MODE_IC_INT = 4)

   ! tidal effects bitfield definition
   integer*2 solid_nonfreq, solid_freq, solid_pole, ocean_pole, ocean
   parameter (solid_nonfreq = 1, solid_freq = 2, solid_pole = 4, ocean_pole = 5, ocean = 3)

   ! gravity models definition
   integer*2 CENTRAL_MODEL, STATIC_MODEL, TIME_MODEL, IERS_MODEL, NO_GRAVITY_MODEL
   parameter (NO_GRAVITY_MODEL = -1, CENTRAL_MODEL = 0, STATIC_MODEL = 1, TIME_MODEL = 2, IERS_MODEL = 3)

   ! non-grav effects
   integer*2 SOLARNG, EARTHNG, ANTENNANG
   parameter (SOLARNG = 1, EARTHNG = 2, ANTENNANG = 3)
   ! time system
   integer*2 TT_time, UTC_time, GPS_time, TAI_time, NO_time
   parameter (NO_time = 0, TT_time = 1, GPS_time = 2, UTC_time = 3, TAI_time = 4)

   ! nutation model
   integer*2 NO_IAU, IAU2000, IAU2006
   parameter (NO_IAU = 0, IAU2000 = 2000, IAU2006 = 2006)

   ! ext orbit types
   integer*2 TYPE_SP3, TYPE_KEPLER, TYPE_INTERP, TYPE_NONE, TYPE_3LINTERP, TYPE_POSSP3
   parameter (TYPE_NONE = 0, TYPE_SP3 = 1, TYPE_INTERP = 2, TYPE_KEPLER = 3, TYPE_3LINTERP = 4, TYPE_POSSP3 = 5)

   ! srp model types
   integer*2 SRP_MISSING, SRP_NONE, SRP_CANNONBALL, SRP_SIMPLE_BW, SRP_FULL_BW
   parameter (SRP_MISSING = -1, SRP_NONE = 0, SRP_CANNONBALL = 1, SRP_SIMPLE_BW = 2, SRP_FULL_BW = 3)

   ! ECOM types
   integer*2 ECOM1, ECOM2, ECOM_HYBRID, SBOXW, ECOM_NONE
   parameter (ECOM1 = 1, ECOM2 = 2, ECOM_HYBRID = 12, SBOXW = 3, ECOM_NONE = 0)

   ! needed because integer subtraction is by default integer*4 ...
   integer*2 one
   parameter (one = 1)

   SAVE

   contains

subroutine get_yaml(yaml_filepath)

   character (*) yaml_filepath
   logical   show_mesg

   nullify(root_dict)
   nullify(pod_data_dict)
   nullify(pod_options_dict)
   nullify(eqm_options_dict)
   nullify(veq_options_dict)
   nullify(srp_dict)
   nullify(srp_parameters_dict)
   nullify(gravity_dict)
   nullify(gravity_model_dict)
   nullify(planets_dict)
   nullify(tides_dict)
   nullify(rel_dict)
   nullify(non_grav_dict)
   nullify(overrides_dict)
   nullify(integ_dict)
   nullify(time_scale_dict)
   nullify(pulse_dict)

   my_error_p => my_error
   yml_pod_mode = NO_MODE
   yml_ic_input_format = NOT_SPECIFIED
   yml_ic_input_refsys = NO_REFSYS
   yml_pod_data_ref_frame = NO_REFSYS
   yml_veq_refsys = NO_REFSYS
   yml_time_scale = NO_time

   sys_override_count = 0
   block_override_count = 0
   prn_override_count = 0

   write(*,*) 'YAML version:   ',yaml_commit_id,' (',yaml_branch_name,' branch)'

   root => parse(yaml_filepath,unit=100,error=error)
   if (error/='') then
      write (*,*) 'PARSE ERROR: '//trim(error)
      stop 1
   end if
   !call root%dump(unit=output_unit,indent=0)

   if (associated(root)) then
      select type (root)
      type is (type_dictionary)
         root_dict => root
      class default
         ! do nothing
      end select 
   end if

   !TODO: check for errors each step of the way
   if (.not.associated(root_dict)) then
        write(*,*) "could not find any data in YAML config"
        STOP
   end if
   pod_data_dict = root_dict%get_dictionary("pod_data", .true., my_error_p)
   if (.not.associated(pod_data_dict)) then
      write(*,*) "could not find pod_data label in YAML config"
      STOP
   else
      call get_pod_data(pod_data_dict, my_error, yml_pod_data_prn, yml_pod_data_ref_frame,&
              yml_pod_data_initial_epoch, yml_pod_data_state_vector)  
      ! parse initial_epoch to get year, month, day, secs
      if (len_trim(yml_pod_data_initial_epoch) .ge. 18) then
          read(yml_pod_data_initial_epoch, '(I41XI21XI21XD7.4)', IOSTAT=ios_keyy) yml_pod_data_initial_year,&
                  yml_pod_data_initial_month, yml_pod_data_initial_day, yml_pod_data_initial_seconds
      else
          ! dummy for now - it gets filled later anyway
          yml_pod_data_initial_year = 0
          yml_pod_data_initial_month = 0
          yml_pod_data_initial_day = 0
          yml_pod_data_initial_seconds = 0.0d0
      end if
      call new_prn_override(yml_pod_data_prn)
      read(yml_pod_data_state_vector, *, IOSTAT=ios_keyy) yml_Zo
      yml_prn_overrides(prn_override_count)%integ%xpos = yml_Zo(1)
      yml_prn_overrides(prn_override_count)%integ%ypos = yml_Zo(2)
      yml_prn_overrides(prn_override_count)%integ%zpos = yml_Zo(3)
      yml_prn_overrides(prn_override_count)%integ%xvel = yml_Zo(4)
      yml_prn_overrides(prn_override_count)%integ%yvel = yml_Zo(5)
      yml_prn_overrides(prn_override_count)%integ%zvel = yml_Zo(6)
      yml_prn_overrides(prn_override_count)%integ%state_vector_enabled = .true.
   end if

   pod_options_dict = root_dict%get_dictionary("pod_options", .true., my_error_p);
   if (.not.associated(pod_options_dict)) then
      write(*,*) "could not find pod_options label in YAML config"
      STOP
   else
      ! if output directory not specified, default it to '.'
      yml_output_dir = pod_options_dict%get_string("output_directory", ".", my_error_p)
      if (yml_output_dir .ne. '.') then
          cmd = "mkdir -p " // trim(yml_output_dir)
          call system(trim(cmd), dir_status)
          if (dir_status .ne. 0) then
              write (*,*) "could not create output directory ", yml_output_dir
              STOP
          end if
      end if
      yml_pod_mode = get_pod_mode(pod_options_dict, my_error)
      yml_ic_input_format = get_input_format(pod_options_dict, yml_ic_filename, my_error)
      yml_ic_input_refsys = get_input_refsys(pod_options_dict, my_error)
      yml_veq_refsys = get_veq_refsys(pod_options_dict, my_error)
      call get_pseudoobs(pod_options_dict, my_error, yml_orbit_filename, yml_orbit_steps, yml_orbit_points);
      call get_orbitarcs(pod_options_dict, my_error, yml_orbit_arc_determination, yml_orbit_arc_prediction,&
              yml_orbit_arc_backwards)
      yml_ext_orbit_enabled = pod_options_dict%get_logical("ext_orbit_enabled", .false., my_error_p)
      if (yml_ext_orbit_enabled) then
         yml_ext_orbit_opt = get_ext_orbit_opt(pod_options_dict, my_error)
         yml_ext_orbit_frame = get_ext_orbit_frame(pod_options_dict, my_error)
         call get_extorbit_comp(pod_options_dict, my_error, yml_ext_orbit_filename, yml_ext_orbit_steps,&
                 yml_ext_orbit_points)
      end if
      yml_iau_model = get_iau_model(pod_options_dict, my_error)
      call get_earth_orientation_params(pod_options_dict, my_error, yml_eop_option,&
              yml_eop_filename, yml_erp_filename, yml_eop_int_points)
      yml_satsinex_filename = pod_options_dict%get_string("satsinex_filename", "", my_error_p)
      yml_leapsecond_filename = pod_options_dict%get_string("leapsec_filename", "", my_error_p)
      yml_gravity_filename = pod_options_dict%get_string("gravity_model_file", "", my_error_p)
      yml_ephemeris_header = pod_options_dict%get_string("DE_fname_header", "", my_error_p)
      yml_ephemeris_data_file = pod_options_dict%get_string("DE_fname_data", "", my_error_p)
      yml_ocean_tides_file = pod_options_dict%get_string("ocean_tides_model_file", "", my_error_p)
      yml_write_sp3_velocities = pod_options_dict%get_logical("sp3_velocity", .false., my_error_p)
      yml_write_partial_velocities = pod_options_dict%get_logical("partials_velocity", .false., my_error_p)
      yml_estimator_iterations = pod_options_dict%get_integer("estimator_iterations", -1, my_error_p)
      srp_dict = pod_options_dict%get_dictionary("srp_apriori_model", .true., my_error_p)
      if (.not.associated(srp_dict)) then
         write (*,*) "cannot find srp_apriori_model label in pod_options"
         STOP
      endif
      yml_apriori_srp = get_apriori_srp(srp_dict, my_error)
      srp_mode_dict = pod_options_dict%get_dictionary("srp_modes", .true., my_error_p)
      if (.not.associated(srp_mode_dict)) then
         write (*,*) "cannot find srp_modes label in pod_options"
         STOP
      end if
      yml_ECOM_mode = get_ECOM_mode(srp_mode_dict, my_error)
      yml_EMP_mode = srp_mode_dict%get_logical("EMPirical", .false., my_error_p)

      ! the next two are actually set by pod mode and perhaps not required?
      yml_estimator_procedure = pod_options_dict%get_integer("estimator_procedure", -1, my_error_p)
      yml_veq_integration = pod_options_dict%get_logical("veq_integration", .false., my_error_p)

      time_scale_dict = pod_options_dict%get_dictionary("time_scale", .true., my_error_p)

      if (.not.associated(time_scale_dict)) then
         write (*,*) "Could not find time_scale in pod_options"
         STOP
      endif
      yml_time_scale = get_time_scale(time_scale_dict, my_error)

      gravity_model_dict = pod_options_dict%get_dictionary("gravity_model", .true., my_error_p)
      if (.not.associated(gravity_model_dict)) then
         write (*,*) "could not find gravity_model label in pod_options"
         STOP
      end if
      yml_gravity_model = get_gravity_model(gravity_model_dict, my_error)

      pulse_dict = pod_options_dict%get_dictionary("pulse", .true., my_error_p) 
      if (.not. associated(pulse_dict)) then
          write (*,*) "could not find pulse label in pod_options"
          STOP
      end if
      yml_pulse_parameter_count = get_yaml_pulses(pulse_dict, my_error, yml_pulses, yml_pulse_ref_frame,&
          yml_pulse_offset, yml_pulse_interval, yml_pulse_epoch_number, yml_pulse_parameters)

   end if

   eqm_options_dict = root_dict%get_dictionary("eqm_options", .true., my_error_p)
   if (.not.associated(eqm_options_dict)) then
      write(*,*) "could not find eqm_options label in YAML config"
      STOP
   else
      integ_dict = eqm_options_dict%get_dictionary("integration_options", .true., my_error_p)
      if (.not.associated(integ_dict)) then
         write (*,*) "could not find integration_options label in eqm_options"
         STOP
      end if

      yml_eqm_integrate_method = get_integrator_method(integ_dict, my_error, yml_eqm_integ_stepsize)

      gravity_dict = eqm_options_dict%get_dictionary("gravity_field", .true., my_error_p)
      if (.not.associated(gravity_dict)) then
         write (*,*) "could not find gravity_field label in eqm_options"
         STOP
      end if
      planets_dict = eqm_options_dict%get_dictionary("planetary_perturbations", .true., my_error_p)
      if (.not.associated(planets_dict)) then
         write (*,*) "could not find planetary_perturbations label in eqm_options"
         STOP
      end if
      tides_dict = eqm_options_dict%get_dictionary("tidal_effects", .true., my_error_p)
      if (.not.associated(tides_dict)) then
         write (*,*) "could not find tidal_effects label in eqm_options"
         STOP
      end if
      rel_dict = eqm_options_dict%get_dictionary("relativistic_effects", .true., my_error_p)
      if (.not.associated(rel_dict)) then
         write (*,*) "could not find relativistic_effects label in eqm_options"
         STOP
      end if
      non_grav_dict = eqm_options_dict%get_dictionary("non_gravitational_effects", .true., my_error_p)
      if (.not.associated(non_grav_dict)) then
         write (*,*) "could not find non_gravitational_effects label in eqm_options"
         STOP
      end if
      yml_eqm_gravity_enabled = gravity_dict%get_logical("enabled", .false., my_error_p)
      yml_eqm_gravity_max_degree = gravity_dict%get_integer("gravity_degree_max", -1, my_error_p)
      yml_eqm_gravity_time_max_degree = gravity_dict%get_integer("timevar_degree_max", -1, my_error_p)
      yml_eqm_planetary_perturbations_enabled = planets_dict%get_logical("enabled", .false., my_error_p)
      yml_eqm_tidal_effects_enabled = tides_dict%get_logical("enabled", .false., my_error_p)

      yml_eqm_tidal_effects = get_tidal_effects(tides_dict, my_error)
      yml_eqm_tides_max_degree = tides_dict%get_integer("ocean_tides_degree_max", -1, my_error_p)
      yml_eqm_rel_effects_enabled = rel_dict%get_logical("enabled", .false., my_error_p)
      yml_eqm_non_grav_effects_enabled = non_grav_dict%get_logical("enabled", .false., my_error_p)
      yml_eqm_non_grav_effects = get_non_grav_effects(non_grav_dict, my_error, "eqm")

      srp_parameters_dict = eqm_options_dict%get_dictionary("srp_parameters", .true., my_error_p)
      if (.not.associated(srp_parameters_dict)) then
         write (*,*) "could not find srp_parameters label in eqm_options"
         STOP
      end if

      yml_eqm_emp_cpr_count = srp_parameters_dict%get_integer("cpr_count", 0, my_error_p)
      yml_eqm_srp_parameters = get_srp_parameters(srp_parameters_dict, .true., yml_eqm_srp_parameters_defined, my_error)
      ! calculate ecomnum and empnum for the supplied parameters

      yml_eqm_ecomnum = 0
      if (BTEST(yml_eqm_srp_parameters, ECOM_D_BIAS - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+1
      if (BTEST(yml_eqm_srp_parameters, ECOM_Y_BIAS - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+1
      if (BTEST(yml_eqm_srp_parameters, ECOM_B_BIAS - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+1
      if (BTEST(yml_eqm_srp_parameters, ECOM_D_CPR - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+2
      if (BTEST(yml_eqm_srp_parameters, ECOM_Y_CPR - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+2
      if (BTEST(yml_eqm_srp_parameters, ECOM_B_CPR - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+2
      if (BTEST(yml_eqm_srp_parameters, ECOM_D_2_CPR - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+2
      if (BTEST(yml_eqm_srp_parameters, ECOM_D_4_CPR - one)) yml_eqm_ecomnum = yml_eqm_ecomnum+2
      yml_eqm_empnum = 0
      if (BTEST(yml_eqm_srp_parameters, EMP_R_BIAS - one)) yml_eqm_empnum = yml_eqm_empnum+1
      if (BTEST(yml_eqm_srp_parameters, EMP_T_BIAS - one)) yml_eqm_empnum = yml_eqm_empnum+1
      if (BTEST(yml_eqm_srp_parameters, EMP_N_BIAS - one)) yml_eqm_empnum = yml_eqm_empnum+1
      if (BTEST(yml_eqm_srp_parameters, EMP_R_CPR - one)) yml_eqm_empnum = yml_eqm_empnum+2
      if (BTEST(yml_eqm_srp_parameters, EMP_T_CPR - one)) yml_eqm_empnum = yml_eqm_empnum+2
      if (BTEST(yml_eqm_srp_parameters, EMP_N_CPR - one)) yml_eqm_empnum = yml_eqm_empnum+2
   end if

   nullify(gravity_dict)
   nullify(gravity_model_dict)
   nullify(planets_dict)
   nullify(tides_dict)
   nullify(rel_dict)
   nullify(non_grav_dict)
   nullify(srp_parameters_dict)

   veq_options_dict = root_dict%get_dictionary("veq_options", .true., my_error_p)
   if (.not.associated(veq_options_dict)) then
      write(*,*) "could not find veq_options label in YAML config"
      STOP
   else
      integ_dict = veq_options_dict%get_dictionary("integration_options", .true., my_error_p)
      if (.not.associated(integ_dict)) then
         write (*,*) "could not find integration_options label in veq_options"
         STOP
      end if
      yml_veq_integrate_method = get_integrator_method(integ_dict, my_error, yml_veq_integ_stepsize)

      gravity_dict = veq_options_dict%get_dictionary("gravity_field", .true., my_error_p)
      if (.not.associated(gravity_dict)) then
         write (*,*) "could not find gravity_field label in veq_options"
         STOP
      end if
      planets_dict = veq_options_dict%get_dictionary("planetary_perturbations", .true., my_error_p)
      if (.not.associated(planets_dict)) then
         write (*,*) "could not find planetary_perturbations label in veq_options"
         STOP
      end if
      tides_dict = veq_options_dict%get_dictionary("tidal_effects", .true., my_error_p)
      if (.not.associated(tides_dict)) then
         write (*,*) "could not find tidal_effects label in veq_options"
         STOP
      end if
      rel_dict = veq_options_dict%get_dictionary("relativistic_effects", .true., my_error_p)
      if (.not.associated(rel_dict)) then
         write (*,*) "could not find relativistic_effects label in veq_options"
         STOP
      end if
      non_grav_dict = veq_options_dict%get_dictionary("non_gravitational_effects", .true., my_error_p)
      if (.not.associated(non_grav_dict)) then
         write (*,*) "could not find non_gravitational_effects label in veq_options"
         STOP
      end if
      yml_veq_gravity_enabled = gravity_dict%get_logical("enabled", .false., my_error_p)
      yml_veq_gravity_max_degree = gravity_dict%get_integer("gravity_degree_max", -1, my_error_p)
      yml_veq_gravity_time_max_degree = gravity_dict%get_integer("timevar_degree_max", -1, my_error_p)
      yml_veq_planetary_perturbations_enabled = planets_dict%get_logical("enabled", .false., my_error_p)
      yml_veq_tidal_effects_enabled = tides_dict%get_logical("enabled", .false., my_error_p)
      yml_veq_tidal_effects = get_tidal_effects(tides_dict, my_error)
      yml_veq_tides_max_degree = tides_dict%get_integer("ocean_tides_degree_max", -1, my_error_p)
      yml_veq_rel_effects_enabled = rel_dict%get_logical("enabled", .false., my_error_p)
      yml_veq_non_grav_effects_enabled = non_grav_dict%get_logical("enabled", .false., my_error_p)
      yml_veq_non_grav_effects = get_non_grav_effects(non_grav_dict, my_error, "veq")

      srp_parameters_dict = veq_options_dict%get_dictionary("srp_parameters", .true., my_error_p)
      if (.not.associated(srp_parameters_dict)) then
         write (*,*) "could not find srp_parameters label in veq_options"
         STOP
      end if

      yml_veq_emp_cpr_count = srp_parameters_dict%get_integer("cpr_count", 0, my_error_p)
      yml_veq_srp_parameters = get_srp_parameters(srp_parameters_dict, .true., yml_veq_srp_parameters_defined, my_error)
      ! calculate ecomnum and empnum for the supplied parameters

      yml_veq_ecomnum = 0
      if (BTEST(yml_veq_srp_parameters, ECOM_D_BIAS - one)) yml_veq_ecomnum = yml_veq_ecomnum+1
      if (BTEST(yml_veq_srp_parameters, ECOM_Y_BIAS - one)) yml_veq_ecomnum = yml_veq_ecomnum+1
      if (BTEST(yml_veq_srp_parameters, ECOM_B_BIAS - one)) yml_veq_ecomnum = yml_veq_ecomnum+1
      if (BTEST(yml_veq_srp_parameters, ECOM_D_CPR - one)) yml_veq_ecomnum = yml_veq_ecomnum+2
      if (BTEST(yml_veq_srp_parameters, ECOM_Y_CPR - one)) yml_veq_ecomnum = yml_veq_ecomnum+2
      if (BTEST(yml_veq_srp_parameters, ECOM_B_CPR - one)) yml_veq_ecomnum = yml_veq_ecomnum+2
      if (BTEST(yml_veq_srp_parameters, ECOM_D_2_CPR - one)) yml_veq_ecomnum = yml_veq_ecomnum+2
      if (BTEST(yml_veq_srp_parameters, ECOM_D_4_CPR - one)) yml_veq_ecomnum = yml_veq_ecomnum+2
      yml_veq_empnum = 0
      if (BTEST(yml_veq_srp_parameters, EMP_R_BIAS - one)) yml_veq_empnum = yml_veq_empnum+1
      if (BTEST(yml_veq_srp_parameters, EMP_T_BIAS - one)) yml_veq_empnum = yml_veq_empnum+1
      if (BTEST(yml_veq_srp_parameters, EMP_N_BIAS - one)) yml_veq_empnum = yml_veq_empnum+1
      if (BTEST(yml_veq_srp_parameters, EMP_R_CPR - one)) yml_veq_empnum = yml_veq_empnum+2
      if (BTEST(yml_veq_srp_parameters, EMP_T_CPR - one)) yml_veq_empnum = yml_veq_empnum+2
      if (BTEST(yml_veq_srp_parameters, EMP_N_CPR - one)) yml_veq_empnum = yml_veq_empnum+2

   end if

   if (yml_veq_srp_parameters /= yml_eqm_srp_parameters) then
       write (*,*) "srp parameters in eqm and veq sections must be identical"
       STOP
   end if

   if (.not. yml_EMP_mode .and. yml_veq_empnum > 0) then
       write (*,*) "Empirical model is off but you are asking for empirical parameters to be estimated." // &
              " Turning them off"
       yml_veq_empnum = 0
       yml_eqm_empnum = 0
       if (BTEST(yml_veq_srp_parameters, EMP_R_BIAS - one)) yml_veq_srp_parameters = yml_veq_srp_parameters - &
              pow (2, EMP_R_BIAS - one)
       if (BTEST(yml_veq_srp_parameters, EMP_T_BIAS - one)) yml_veq_srp_parameters = yml_veq_srp_parameters - &
              pow (2, EMP_T_BIAS - one)
       if (BTEST(yml_veq_srp_parameters, EMP_N_BIAS - one)) yml_veq_srp_parameters = yml_veq_srp_parameters - &
              pow (2, EMP_N_BIAS - one)
       if (BTEST(yml_veq_srp_parameters, EMP_R_CPR - one)) yml_veq_srp_parameters = yml_veq_srp_parameters - &
              pow (2, EMP_R_CPR - one)
       if (BTEST(yml_veq_srp_parameters, EMP_T_CPR - one)) yml_veq_srp_parameters = yml_veq_srp_parameters - &
              pow (2, EMP_T_CPR - one)
       if (BTEST(yml_veq_srp_parameters, EMP_N_CPR - one)) yml_veq_srp_parameters = yml_veq_srp_parameters - &
              pow (2, EMP_N_CPR - one)

      yml_eqm_srp_parameters = yml_veq_srp_parameters
   end if

   if (yml_ECOM_mode .eq. ECOM1) then
       show_mesg = .false.
       if (BTEST(yml_veq_srp_parameters, ECOM_D_2_CPR - one)) then
               yml_veq_srp_parameters = yml_veq_srp_parameters - pow (2, ECOM_D_2_CPR - one)
               show_mesg = .true.
               yml_veq_ecomnum = yml_veq_ecomnum - 2
       endif 
       if (BTEST(yml_veq_srp_parameters, ECOM_D_4_CPR - one)) then
               yml_veq_srp_parameters = yml_veq_srp_parameters - pow (2, ECOM_D_4_CPR - one)
               yml_veq_ecomnum = yml_veq_ecomnum - 2
               show_mesg = .true.
       endif 
       if (show_mesg) write(*,*) "ECOM1 model selected - turning off D2_CPR and D4_CPR parameters"
       yml_eqm_srp_parameters = yml_veq_srp_parameters
       yml_eqm_ecomnum = yml_veq_ecomnum
   end if

   if (yml_ECOM_mode .eq. ECOM2) then
       show_mesg = .false.
       if (BTEST(yml_veq_srp_parameters, ECOM_D_CPR - one)) then
               yml_veq_srp_parameters = yml_veq_srp_parameters - pow (2, ECOM_D_CPR - one)
               show_mesg = .true.
               yml_veq_ecomnum = yml_veq_ecomnum - 2
       endif 
       if (BTEST(yml_veq_srp_parameters, ECOM_Y_CPR - one)) then
               yml_veq_srp_parameters = yml_veq_srp_parameters - pow (2, ECOM_Y_CPR - one)
               yml_veq_ecomnum = yml_veq_ecomnum - 2
               show_mesg = .true.
       endif 
       if (show_mesg) write(*,*) "ECOM2 model selected - turning off D_CPR and Y_CPR parameters"
       yml_eqm_srp_parameters = yml_veq_srp_parameters
       yml_eqm_ecomnum = yml_veq_ecomnum
   end if

   if ((yml_eqm_tidal_effects_enabled .or. yml_veq_tidal_effects_enabled) .and. yml_ocean_tides_file == "") then
       write (*,*) "tidal effects enabled and no tidal effects file specified"
       STOP
   end if

end subroutine get_yaml

function get_yaml_pulses(dict, error, yml_pulses, yml_pulse_ref_frame,& 
          yml_pulse_offset, yml_pulse_interval, yml_pulse_epoch_number, yml_pulse_parameters)
   type (type_dictionary) :: dict
   type (type_dictionary), pointer :: ref_dict, parms_dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical yml_pulses
   real*8    yml_pulse_offset, yml_pulse_interval
   integer*2 yml_pulse_epoch_number
   integer*2 yml_pulse_parameters
   integer*2 get_yaml_pulses
   integer*2 yml_pulse_ref_frame

   nullify(e)
   get_yaml_pulses = 0;

   yml_pulse_offset = dict%get_real("offset", -1.d0, e)
   yml_pulse_interval = dict%get_real("interval", -1.d0, e)
   yml_pulse_epoch_number = dict%get_integer("epoch_number", -1, e)
   yml_pulses = dict%get_logical("enabled", .false., e)

   ! if pulses off no need to set anything else?
   if (.not. yml_pulses) then
       yml_pulse_offset = -1.d0
       yml_pulse_interval = -1.d0
       yml_pulse_epoch_number = -1
       yml_pulse_ref_frame = NO_REFSYS
       return
   end if

   ref_dict = dict%get_dictionary("reference_frame", .true., e)
   if (.not. associated(ref_dict)) then
       error = e
       error%message = "cannot find reference_frame label in pulses config"
       STOP
   endif
   yml_pulse_ref_frame = get_reference_system (ref_dict, e)

   parms_dict = dict%get_dictionary("parameters", .true., e)
   if (.not. associated(parms_dict)) then
       error = e
       error%message = "cannot find parameters label in pulses config"
       STOP
   endif
   yml_pulse_parameters = get_pulse_parms(parms_dict, yml_pulse_ref_frame, error)

   if (BTEST(yml_pulse_parameters, DIR_X - one)) get_yaml_pulses = get_yaml_pulses+1
   if (BTEST(yml_pulse_parameters, DIR_Y - one)) get_yaml_pulses = get_yaml_pulses+1
   if (BTEST(yml_pulse_parameters, DIR_Z - one)) get_yaml_pulses = get_yaml_pulses+1
   if (BTEST(yml_pulse_parameters, DIR_R - one)) get_yaml_pulses = get_yaml_pulses+1
   if (BTEST(yml_pulse_parameters, DIR_T - one)) get_yaml_pulses = get_yaml_pulses+1
   if (BTEST(yml_pulse_parameters, DIR_N - one)) get_yaml_pulses = get_yaml_pulses+1

   if (associated(e)) then
       error = e
       error%message = "error reading pulses config"
   end if

   return
end function get_yaml_pulses

function get_pulse_parms(dict, ref_frame, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical direction_x, direction_y, direction_z
   logical direction_r, direction_t, direction_n
   integer*2 get_pulse_parms, ref_frame

   get_pulse_parms = 0

   direction_x = dict%get_logical("direction_x", .false., e)
   direction_y = dict%get_logical("direction_y", .false., e)
   direction_z = dict%get_logical("direction_z", .false., e)
   direction_r = dict%get_logical("direction_r", .false., e)
   direction_t = dict%get_logical("direction_t", .false., e)
   direction_n = dict%get_logical("direction_n", .false., e)

   if (direction_x) get_pulse_parms = get_pulse_parms + pow (2, DIR_X - one)
   if (direction_y) get_pulse_parms = get_pulse_parms + pow (2, DIR_Y - one)
   if (direction_z) get_pulse_parms = get_pulse_parms + pow (2, DIR_Z - one)
   if (direction_r) get_pulse_parms = get_pulse_parms + pow (2, DIR_R - one)
   if (direction_t) get_pulse_parms = get_pulse_parms + pow (2, DIR_T - one)
   if (direction_n) get_pulse_parms = get_pulse_parms + pow (2, DIR_N - one)

   if ((BTEST(get_pulse_parms, DIR_X - one) .or.&
        BTEST(get_pulse_parms, DIR_Y - one) .or.&
        BTEST(get_pulse_parms, DIR_Z - one)) .and. ref_frame == ORBITAL) then
        error%message = "Cannot select pulse X,Y,Z directions in ORBITAL frame"
        get_pulse_parms = 0
   end if

   if ((BTEST(get_pulse_parms, DIR_R - one) .or.&
        BTEST(get_pulse_parms, DIR_T - one) .or.&
        BTEST(get_pulse_parms, DIR_N - one)) .and. ref_frame == ICRF) then
        error%message = "Cannot select pulse R,T,N directions in ICRF frame"
        get_pulse_parms = 0
   end if

   return
end function get_pulse_parms

function get_non_grav_effects(dict, error, label)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical solar, earth, thrust
   character (*) label
   integer*2 get_non_grav_effects

   nullify (e)
   get_non_grav_effects = 0;

   solar = dict%get_logical("solar_radiation", .false., e)
   earth = dict%get_logical("earth_radiation", .false., e)
   thrust = dict%get_logical("antenna_thrust", .false., e)

   if (associated(e)) then
      error = e
      error%message = label // ':' // error%message
   end if

   if (solar) then
      get_non_grav_effects = pow(2, SOLARNG - one)
   end if
   if (earth) then
      get_non_grav_effects = get_non_grav_effects + pow(2, EARTHNG - one)
   end if
   if (thrust) then
      get_non_grav_effects = get_non_grav_effects + pow(2, ANTENNANG - one)
   end if

   return
end function get_non_grav_effects

function get_tidal_effects(dict, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical solid_nonfreq_t, solid_freq_t, solid_pole_t, ocean_pole_t, ocean_t
   integer*2 get_tidal_effects

   nullify (e)
   get_tidal_effects = 0;

   solid_nonfreq_t = dict%get_logical("solid_tides_nonfreq", .false., e)
   solid_freq_t = dict%get_logical("solid_tides_freq", .false., e)
   solid_pole_t = dict%get_logical("solid_earth_pole_tides", .false., e)
   ocean_pole_t = dict%get_logical("ocean_pole_tide", .false., e)
   ocean_t = dict%get_logical("ocean_tides", .false., e)

   if (associated(e)) then
      error = e
      !error%message = label // ':' // error%message
   end if

   if (solid_nonfreq_t) then
      get_tidal_effects = pow (2, solid_nonfreq - one)
   end if
   if (solid_freq_t) then
      get_tidal_effects = get_tidal_effects + pow (2, solid_freq - one)
   end if
   if (solid_pole_t) then
      get_tidal_effects = get_tidal_effects + pow (2, solid_pole - one)
   end if
   if (ocean_pole_t) then
      get_tidal_effects = get_tidal_effects + pow(2, ocean_pole - one)
   end if
   if (ocean_t) then
      get_tidal_effects = get_tidal_effects + pow (2, ocean - one)
   end if

   return
end function get_tidal_effects

function get_srp_parameters(dict, listall, defined, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   logical listall
   integer*4 defined
   integer*4 get_srp_parameters

   defined = 0
   get_srp_parameters = 0

   call get_logical_parameter(dict, error, "ECOM_D_bias", listall, get_srp_parameters, ECOM_D_bias, defined)
   call get_logical_parameter(dict, error, "ECOM_Y_bias", listall, get_srp_parameters, ECOM_Y_bias, defined)
   call get_logical_parameter(dict, error, "ECOM_B_bias", listall, get_srp_parameters, ECOM_B_bias, defined)
   call get_logical_parameter(dict, error, "ECOM_D_cpr", listall, get_srp_parameters, ECOM_D_cpr, defined)
   call get_logical_parameter(dict, error, "ECOM_Y_cpr", listall, get_srp_parameters, ECOM_Y_cpr, defined)
   call get_logical_parameter(dict, error, "ECOM_B_cpr", listall, get_srp_parameters, ECOM_B_cpr, defined)
   call get_logical_parameter(dict, error, "ECOM_D_2_cpr", listall, get_srp_parameters, ECOM_D_2_cpr, defined)
   call get_logical_parameter(dict, error, "ECOM_D_4_cpr", listall, get_srp_parameters, ECOM_D_4_cpr, defined)
   call get_logical_parameter(dict, error, "EMP_R_bias", listall, get_srp_parameters, EMP_R_bias, defined)
   call get_logical_parameter(dict, error, "EMP_T_bias", listall, get_srp_parameters, EMP_T_bias, defined)
   call get_logical_parameter(dict, error, "EMP_N_bias", listall, get_srp_parameters, EMP_N_bias, defined)
   call get_logical_parameter(dict, error, "EMP_R_cpr", listall, get_srp_parameters, EMP_R_cpr, defined)
   call get_logical_parameter(dict, error, "EMP_T_cpr", listall, get_srp_parameters, EMP_T_cpr, defined)
   call get_logical_parameter(dict, error, "EMP_N_cpr", listall, get_srp_parameters, EMP_N_cpr, defined)

   return
end function get_srp_parameters

! temp: use the math library version instead
function pow(base, idx)
   integer*4 base
   integer*2 idx

   integer*4 pow
   integer*2 i

   pow = 1

   if (idx .eq. 0) then
      return
   end if

   do i=1, idx
      pow = pow * base
   end do

   return
end function pow

subroutine get_logical_parameter(dict, error, label, listall, parameters, idx, defined)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   type (type_scalar), pointer :: s
   logical listall
   integer*4 parameters, defined
   integer*2 idx, idx_minus_1
   character(*) label
   logical value
   integer*4 abit

   nullify(e)
   nullify(s)

   s = dict%get_scalar(label, listall, e)
   if (associated(e)) then
      if (listall) then
         write (*,*) "Must supply a default " // label // " srp parameter"
         error = e
         STOP
      else
         return
      end if
   end if

   value = dict%get_logical(label, .false., e)
   if (associated(e)) then
      error = e
      write (*,*) "label "//label//" found but cannot interpret as logical value"
      STOP
   endif
   idx_minus_1 = idx - one
   abit = pow(2, idx_minus_1)
   defined = defined + abit

   if (value) then
      parameters = parameters + abit
   end if

   return
end subroutine get_logical_parameter

function get_gravity_model(dict, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical central, static_mdl, time_mdl, iers
   integer*2 get_gravity_model

   nullify (e)

   get_gravity_model = NO_GRAVITY_MODEL

   central = dict%get_logical("central_force", .false., e)
   static_mdl = dict%get_logical("static_gravity_model", .false., e)
   time_mdl = dict%get_logical("time_variable_model", .false., e)
   iers = dict%get_logical("iers_geopotential_model", .false., e)

   if (central) then
      if (static_mdl .or. time_mdl .or. iers) then
         write (*,*) "Can only have one gravity model selected"
         STOP
      end if
      get_gravity_model = CENTRAL_MODEL
   else if (static_mdl) then
      if (time_mdl .or. iers) then
         write (*,*) "Can only have one gravity model selected"
         STOP
      end if
      get_gravity_model = STATIC_MODEL
   else if (time_mdl) then
      if (iers) then
         write (*,*) "Can only have one gravity model selected"
         STOP
      end if
      get_gravity_model = TIME_MODEL
   else if (iers) then
      get_gravity_model = IERS_MODEL
   end if

   if (get_gravity_model < 0) then
      write (*,*) "Must choose a gravity model "
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   return
end function get_gravity_model

function get_apriori_srp(dict, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical cannonball, simple_bw, full_bw, no_model
   integer*2 get_apriori_srp

   nullify (e)

   get_apriori_srp = SRP_MISSING

   cannonball = dict%get_logical("cannon_ball_model", .false., e)
   simple_bw = dict%get_logical("simple_boxwing_model", .false., e)
   full_bw = dict%get_logical("full_boxwing_model", .false., e)
   no_model = dict%get_logical("no_model", .false., e)

   if (no_model) then
      if (cannonball .or. simple_bw .or. full_bw) then
         write (*,*) "Can only have one srp_apriori_model selected"
         STOP
      end if
      get_apriori_srp = SRP_NONE
   else if (cannonball) then
      if (simple_bw .or. full_bw) then
         write (*,*) "Can only have one srp_apriori_model selected"
         STOP
      end if
      get_apriori_srp = SRP_CANNONBALL
   else if (simple_bw) then
      if (full_bw) then
         write (*,*) "Can only have one srp_apriori_model selected"
         STOP
      end if
      get_apriori_srp = SRP_SIMPLE_BW 
   else if (full_bw) then
      get_apriori_srp = SRP_FULL_BW
   end if

   if (get_apriori_srp < 0) then
      write (*,*) "Must choose a default srp apriori model (even if its 'no_model')"
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   return
end function get_apriori_srp

function get_ECOM_mode(dict, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical ecom1_t, ecom2_t, hybrid_t, sboxw_t
   integer*2 get_ECOM_mode

   nullify (e)

   ecom1_t = dict%get_logical("ECOM1", .false., e)
   ecom2_t = dict%get_logical("ECOM2", .false., e)
   hybrid_t = dict%get_logical("hybrid", .false., e)
   sboxw_t = dict%get_logical("SBOXW", .false., e)

   get_ECOM_mode = ECOM_NONE
   if (ecom1_t) then
      if (ecom2_t .or. hybrid_t .or. sboxw_t) then
         write (*,*) "Can choose at most 1 ECOM mode"
         STOP
      end if
      get_ECOM_mode = ECOM1
   else if (ecom2_t) then
      if (hybrid_t .or. sboxw_t) then
         write (*,*) "Can choose at most 1 ECOM mode"
         STOP
      end if
      get_ECOM_mode = ECOM2
   else if (hybrid_t) then
      if (sboxw_t) then
         write (*,*) "Can choose at most 1 ECOM mode"
         STOP
      end if
      get_ECOM_mode = ECOM_HYBRID 
   else if (sboxw_t) then
      get_ECOM_mode = SBOXW     
   end if

   if (associated(e)) then
      error = e
   end if

   return
end function get_ECOM_mode

subroutine get_pod_data(pod_data_dict, error, prn, ref_frame,&
                init_epoch, state_vector)
   type (type_dictionary), pointer :: pod_data_dict, ref_frame_dict
   type (type_error) :: error
   type (type_error), pointer :: e

   character(*) prn, init_epoch, state_vector
   integer*8 orbit_arc_secs
   integer*2 ref_frame

   nullify (e)
   nullify (time_scale_dict)

   prn = pod_data_dict%get_string("satellite_PRN", "", e)
   init_epoch = pod_data_dict%get_string("initial_epoch", "", e)
   state_vector = pod_data_dict%get_string("state_vector", "", e)

   ref_frame_dict = pod_data_dict%get_dictionary("reference_frame", .true., e)
   if (.not.associated(ref_frame_dict)) then
       write (*,*) "Could not find reference_frame in pod_data"
       STOP
   endif
   ref_frame = get_reference_system(ref_frame_dict, error)

if (1<0) then
!debug only
   if (prn /= "") then
      write(*,*) "pod_data(prn) is "//prn
   else
      write(*,*) "pod_data(prn) is missing"
   end if
   if (ref_frame == NO_REFSYS) then
      write(*,*) "pod_data(reference frame) is ", ref_frame
   else
      write(*,*) "pod+data(reference frame) is missing"
   end if
   if (init_epoch /= "") then
      write(*,*) "pod_data(initial epoch) is "//init_epoch
   else
      write(*,*) "pod_data(initial epoch) is missing"
   end if
   if (state_vector /= "") then
      write(*,*) "pod_data(state vector) is " //trim(state_vector)
   else
      write(*,*) "pod_data(state vector) is missing"
   end if
end if

   if (associated(e)) then
      error = e
   end if

   return
end subroutine get_pod_data

function get_reference_system(dict, error)
   type (type_dictionary), pointer :: dict
   type (type_error) :: error
   type (type_error), pointer :: e

   logical refsys_itrf, refsys_icrf, refsys_kepler, refsys_itrs, refsys_icrs
   logical refsys_orbital
   integer*2 get_reference_system

   nullify(e)

   ! allow to use both ITRF/ICRF and ITRS/ICRS interchangeably
   get_reference_system = NO_REFSYS
   refsys_itrf = dict%get_logical("itrf", .false., e)
   refsys_icrf = dict%get_logical("icrf", .false., e)
   refsys_kepler= dict%get_logical("kepler", .false., e)
   refsys_itrs = dict%get_logical("itrs", .false., e)
   refsys_icrs = dict%get_logical("icrs", .false., e)
   refsys_orbital = dict%get_logical("orbital", .false., e)

   if (refsys_itrf .or. refsys_itrs) then
      if (refsys_icrf .or. refsys_icrs .or. refsys_kepler .or. refsys_orbital) then
         write (*,*)  "Can only choose one reference system"
         STOP
      end if
      get_reference_system = ITRF
   end if

   if (refsys_icrf .or. refsys_icrs) then
      if (refsys_kepler .or. refsys_orbital) then
         write (*,*)  "Can only choose one reference system"
         STOP
      end if
      get_reference_system = ICRF
   end if

   if (refsys_kepler) then
      if (refsys_orbital) then
         write (*,*)  "Can only choose one reference system"
         STOP
      end if
      get_reference_system = KEPLERRF
   end if

   if (refsys_orbital) then
      get_reference_system = ORBITAL
   end if

   if (get_reference_system == NO_REFSYS) then
      write (*,*) "Must specify a reference system/frame option"
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   return
end function get_reference_system

function get_time_scale(dict, error)
   type (type_dictionary), pointer :: dict
   type (type_error) :: error
   type (type_error), pointer :: e

   logical TT_time_t, UTC_time_t, GPS_time_t, TAI_time_t
   integer*2 get_time_scale

   nullify(e)
   get_time_scale = NO_time

   TT_time_t = dict%get_logical("TT_time", .false., e)
   UTC_time_t = dict%get_logical("UTC_time", .false., e)
   GPS_time_t = dict%get_logical("GPS_time", .false., e)
   TAI_time_t = dict%get_logical("TAI_time", .false., e)

   if (TT_time_t) then
      if (UTC_time_t .or. GPS_time_t .or. TAI_time_t) then
         write (*,*) "Can only choose one time_scale option"
         STOP
      end if
      get_time_scale = TT_time
   else if (UTC_time_t) then
      if (GPS_time_t .or. TAI_time_t) then
         write (*,*) "Can only choose one time_scale option"
         STOP
      end if
      get_time_scale = UTC_time
   else if (GPS_time_t) then
      if (TAI_time_t) then
         write (*,*) "Can only choose one time_scale option"
         STOP
      end if
      get_time_scale = GPS_time
   else if (TAI_time_t) then
      get_time_scale = TAI_time
   end if

   if (get_time_scale .eq. NO_time) then
      write (*,*) "Must specify a time_scale option"
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   return
end function get_time_scale
   
subroutine get_earth_orientation_params(dict, error, eop_option, eop_filename, erp_filename, npoints)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   character(*) eop_filename, erp_filename
   integer*4 npoints, eop_option
   logical EOP_c4, EOP_rapid, EOP_ultra_rapid

   nullify(e)

   EOP_c4 = dict%get_logical("EOP_soln_c04", .false., e)
   EOP_rapid = dict%get_logical("EOP_soln_rapid", .false., e)
   EOP_ultra_rapid = dict%get_logical("EOP_soln_igs", .false., e)

   npoints = dict%get_integer("EOP_soln_interp_points", -1, e)

   eop_option = EOP_NONE
   if (EOP_c4) then
      eop_option = EOP_C04T 
      eop_filename = dict%get_string("EOP_soln_c04_file", "", e)
      if (EOP_rapid .or. EOP_ultra_rapid) then
         write (*,*) "Can only choose one EOP option"
         STOP
      end if
   end if
   if (EOP_rapid) then
      eop_option = EOP_FAST
      eop_filename = dict%get_string("EOP_soln_rapid_file", "", e)
      if (EOP_ultra_rapid) then
         write (*,*) "Can only choose one EOP option"
         STOP
      end if
   end if
   ! for ultra rapid you need both rapid filename and ultra-rapid filename
   if (EOP_ultra_rapid) then
      eop_option = EOP_SUPER_FAST
      eop_filename = dict%get_string("EOP_soln_rapid_file", "", e)
   end if
   erp_filename = dict%get_string("ERP_soln_igs_file", "", e)
   if (eop_option == EOP_NONE) then
      write (*,*) "Must choose an EOP option"
      STOP
   end if
   if (eop_filename == "") then
      write (*,*) "Must specify EOP file relevant to your choice of option"
      STOP
   end if
   if (EOP_ultra_rapid .and. erp_filename == "") then
      write (*,*) "Must specify ERP file for ultra rapid parameters"
      STOP
   end if


   if (associated(e)) then
      error = e
   end if

end subroutine get_earth_orientation_params

subroutine get_extorbit_comp(dict, error, filename, steps, points)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   integer*4 :: steps, points
   character(*) filename

   nullify(e)

   filename = dict%get_string("ext_orbit_filename", "", e);
   steps = dict%get_integer("ext_orbit_interp_step", -1, e)
   points = dict%get_integer("ext_orbit_interp_points", -1, e)

   if (associated(e)) then
      error = e
   end if

   return
end subroutine get_extorbit_comp

integer*2 function get_iau_model(dict, error)
   use yaml_types
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical iau_model_2000, iau_model_2006
   
   nullify(e)
   get_iau_model = NO_IAU

   iau_model_2000 = dict%get_logical("iau_model_2000", .false., e);
   iau_model_2006 = dict%get_logical("iau_model_2006", .false., e)

   if (associated(e)) then
      error = e
   endif

   if (iau_model_2000) then
      get_iau_model = IAU2000
      if (iau_model_2006) then
         write(*,*) "Can only select 1 nutation model"
         STOP
      end if
   else if (iau_model_2006) then
      get_iau_model = IAU2006
   end if

   if (get_iau_model == NO_IAU) then
      write (*,*) "Must select a nutation model"
      STOP
   end if

   return
end function get_iau_model

integer*2 function get_integrator_method(dict, error, stepsize)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical r4, r7, r8
   integer*4 stepsize

   nullify(e)
   get_integrator_method = 0

   r4 = dict%get_logical("RK4_integrator_method", .false., e)
   r7 = dict%get_logical("RKN7_integrator_method", .false., e)
   r8 = dict%get_logical("RK8_integrator_method", .false., e)

   stepsize = dict%get_integer("integrator_step", -1, e)

   if (associated(e)) then
      error = e
   end if

   if (r4) then
      get_integrator_method = RK4
      if (r7 .or. r8) then
         write (*,*) "Can only choose 1 integrator method"
         STOP
      endif
   else if (r7) then
      get_integrator_method = RK7
      if (r8) then
         write (*,*) "Can only choose 1 integrator method"
         STOP
      end if
   else if (r8) then
      get_integrator_method = RK8
   end if

   if (get_integrator_method == 0) then
      write (*,*) "Must choose an integration method"
      STOP
   end if

   if (stepsize == -1) then
      write (*,*) "Must set the integrator stepsize"
      STOP
   end if

   return
end function get_integrator_method

integer*2 function get_veq_refsys(dict, error)
   type(type_dictionary) :: dict
   type(type_dictionary), pointer :: refsys_dict
   type (type_error) :: error
   type (type_error), pointer :: e

   nullify(refsys_dict)
   nullify(e)

   refsys_dict = dict%get_dictionary("veq_refsys", .true., e)
   if (.not.associated(refsys_dict)) then
      write (*,*) "Cannot find veq_refsys: tag in pod_options"
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   get_veq_refsys = get_reference_system(refsys_dict, error)

   return
end function get_veq_refsys


integer*2 function get_ext_orbit_frame(dict, error)
   type(type_dictionary) :: dict
   type(type_dictionary), pointer :: frame_dict
   type (type_error) :: error
   type (type_error), pointer :: e

   nullify(frame_dict)
   nullify(e)

   frame_dict = dict%get_dictionary("ext_orbit_frame", .true., e)
   if (.not.associated(frame_dict)) then
      write (*,*) "Cannot find ext_orbit_frame: tag in pod_options"
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   get_ext_orbit_frame = get_reference_system(frame_dict, error)

   return
end function get_ext_orbit_frame

integer*2 function get_ext_orbit_opt(dict, error)
   type (type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical ext_orbit_sp3, ext_orbit_interp, ext_orbit_kepler, ext_orbit_lagrange, ext_orbit_position_sp3

   nullify(e)
   get_ext_orbit_opt = TYPE_NONE

   ext_orbit_sp3 = dict%get_logical("ext_orbit_type_sp3", .false., e)
   ext_orbit_interp = dict%get_logical("ext_orbit_type_interp", .false., e)
   ext_orbit_kepler = dict%get_logical("ext_orbit_type_kepler", .false., e)
   ext_orbit_lagrange = dict%get_logical("ext_orbit_type_lagrange", .false., e)
   ext_orbit_position_sp3 = dict%get_logical("ext_orbit_type_position_sp3", .false., e)

   if (associated(e)) then
      error = e
   end if

   if (ext_orbit_sp3) then
           get_ext_orbit_opt = TYPE_SP3
   end if
   if (ext_orbit_interp) then
           if (get_ext_orbit_opt > TYPE_NONE) then
         write (*,*) "Cannot select more than one external orbit type"
         STOP
      end if
      get_ext_orbit_opt = TYPE_INTERP
   end if
   if (ext_orbit_kepler) then
           if (get_ext_orbit_opt > TYPE_NONE) then
         write (*,*) "Cannot select more than one external orbit type"
         STOP
      end if
      get_ext_orbit_opt = TYPE_KEPLER 
   end if
   if (ext_orbit_lagrange) then
           if (get_ext_orbit_opt > TYPE_NONE) then
         write (*,*) "Cannot select more than one external orbit type"
         STOP
      end if
      get_ext_orbit_opt = TYPE_3LINTERP 
   end if
   if (ext_orbit_position_sp3) then
           if (get_ext_orbit_opt > TYPE_NONE) then
         write (*,*) "Cannot select more than one external orbit type"
         STOP
      end if
      get_ext_orbit_opt = TYPE_POSSP3
   end if
   if (get_ext_orbit_opt == TYPE_NONE) then
      write (*,*) "Must select an external orbit type"
      STOP
   end if

   return
end function get_ext_orbit_opt

integer*2 function get_pod_mode(dict, error)
   type(type_dictionary) :: dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical pod_mode_fit, pod_mode_predict, pod_mode_eqm_int, pod_mode_ic_int

   nullify(e)
   get_pod_mode = -1

   pod_mode_fit = dict%get_logical("pod_mode_fit", .false., e)
   !TODO check for error
   pod_mode_predict = dict%get_logical("pod_mode_predict", .false., e)
   !TODO check for error
   pod_mode_eqm_int = dict%get_logical("pod_mode_eqm_int", .false., e)
   !TODO check for error
   pod_mode_ic_int = dict%get_logical("pod_mode_ic_int", .false., e)
   !TODO check for error

   if (associated(e)) then
      error = e
   end if

   if (pod_mode_fit) then
      get_pod_mode = MODE_FIT
   end if
   if (pod_mode_predict) then
      if (get_pod_mode > 0) then
         write (*,*) "Cannot select more than one pod mode"
         STOP
      end if
      get_pod_mode = MODE_PREDICT
   end if
   if (pod_mode_eqm_int) then
      if (get_pod_mode > 0) then
         write (*,*) "Cannot select more than one pod mode"
         STOP
      end if
      get_pod_mode = MODE_EQM_INT
   end if
   if (pod_mode_ic_int) then
      if (get_pod_mode > 0) then
         write (*,*) "Cannot select more than one pod mode"
         STOP
      end if
      get_pod_mode = MODE_IC_INT
   end if
   if (get_pod_mode == NO_MODE) then
      write (*,*) "Must select a pod mode"
      STOP
   end if

   return
end function get_pod_mode

integer*2 function get_input_format(dict, filename, error)
   type(type_dictionary) :: dict
   type(type_dictionary), pointer :: format_dict
   type (type_error) :: error
   type (type_error), pointer :: e
   logical ic_input_sp3, ic_input_icf
   character(*) filename

   nullify(e)
   nullify(format_dict)

   get_input_format = NOT_SPECIFIED
   format_dict = dict%get_dictionary("ic_input_format", .true., e)
   if (.not.associated(format_dict)) then
       write (*,*) "Need ic_input_format: tag in pod_options"
       STOP
   end if
   ic_input_sp3 = format_dict%get_logical("sp3", .false., e)
   !TODO check for error
   ic_input_icf = format_dict%get_logical("icf", .false., e)
   !TODO check for error

   filename = format_dict%get_string("ic_filename", "", e)

   if (associated(e)) then
      error = e
   end if

   if (ic_input_sp3) then
      get_input_format = SP3_FILE
   end if
   if (ic_input_icf) then
      if (get_input_format > NOT_SPECIFIED) then
         write (*,*) "Cannot select more than one input format type"
         STOP
      else
         get_input_format = IC_FILE
      end if
   end if
   if (get_input_format == NOT_SPECIFIED) then
      write (*,*) "Must select an input format type"
      STOP
   end if

   return
end function get_input_format

integer*2 function get_input_refsys(dict, error)
   type(type_dictionary) :: dict
   type(type_dictionary), pointer :: refsys_dict
   type (type_error) :: error
   type (type_error), pointer :: e

   nullify(refsys_dict)
   nullify(e)

   refsys_dict = dict%get_dictionary("ic_input_refsys", .true., e)
   if (.not.associated(refsys_dict)) then
      write (*,*) "Cannot find ic_input_refsys: tag in pod_options"
      STOP
   end if

   if (associated(e)) then
      error = e
   end if

   get_input_refsys = get_reference_system(refsys_dict, error)

   return
end function get_input_refsys

subroutine get_pseudoobs(dict, error, filename, step, points)
   type(type_dictionary) :: dict
   type (type_error) :: error
   character(*) filename
   integer*4 step
   integer*4 points
   type (type_error), pointer :: e

   nullify(e)

   filename = dict%get_string("pseudobs_orbit_filename", "", e);
   step = dict%get_integer("pseudobs_interp_step", -1, e)
   points = dict%get_integer("pseudobs_interp_points", -1, e)

   if (associated(e)) then
      error = e
   end if

   return
end subroutine get_pseudoobs

subroutine get_orbitarcs(dict, error, determination, prediction, backwards)
   type (type_dictionary) :: dict
   type (type_error) :: error
   integer*4 determination, prediction, backwards
   type (type_error), pointer :: e

   nullify(e)

   determination=dict%get_integer("orbit_arc_determination", -1, e)
   prediction=dict%get_integer("orbit_arc_prediction", -1, e)
   backwards=dict%get_integer("orbit_arc_backwards", -1, e)

   if (associated(e)) then
      error = e
   end if

   return
end subroutine get_orbitarcs

subroutine new_prn_override(prn)
   character(*) prn
   integer mutex_success, thread_get_mutex
   ! actually a pointer to a C mutex object
   integer*8 :: mutex

   external thread_get_mutex

   ! TODO: Check for error return value of 0
   mutex_success = thread_get_mutex(mutex)
   call thread_lock(mutex)

   prn_override_count = prn_override_count+1
   yml_prn_overrides(prn_override_count)%name = TRIM(prn)
   yml_prn_overrides(prn_override_count)%integ%eqm_enabled = .false.
   yml_prn_overrides(prn_override_count)%integ%veq_enabled = .false.
   yml_prn_overrides(prn_override_count)%integ%arc_enabled = .false.
   yml_prn_overrides(prn_override_count)%integ%state_vector_enabled = .false.
   yml_prn_overrides(prn_override_count)%integ%emp_parameters_used = 0
   yml_prn_overrides(prn_override_count)%integ%ecom_parameters_used = 0

   call thread_unlock(mutex)
   call thread_release_mutex(mutex)
end subroutine new_prn_override

end module pod_yaml
