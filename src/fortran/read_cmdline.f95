! ----------------------------------------------------------------------
! SUBROUTINE: read_cmdline
! ----------------------------------------------------------------------
! Purpose:
! This subroutine reads command line options and returns variables defined in mdl_config
! Uses getopt command line parsing functions from f90getopt.F90
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------


subroutine read_cmdline
USE f90getopt
USE mdl_config
use pod_yaml

! ----------------------------------------------------------------------
! Command line local variables
integer            :: len_optarg
character (LEN=80) :: pgm_name

integer            :: EMP_param

! Set number of long command line options available
type(option_s) :: opts(19)

! Current pod_yaml variable options
! ----------------------------------------------------------------------
! POD_fname_cfg                -c
! yml_pod_mode                 -m
! EQM_fname_cfg                -e
! VEQ_fname_cfg                -v
! yml_orbit_filename           -s
! yml_ext_orbit_filename       -o
! yml_orbit_arc_determination  -a
! yml_orbit_arc_prediction     -p
! EOP_solution_cfg             -t
! EOP_fname_cfg                -r
!! ERP_fname_cfg               -- not implemented on cmdline 
!! EOP_Nint_cfg                -- not implemented on cmdline 
! yml_iau_model                -n
! yml_Estimator_Iterations     -i
! sp3_velocity_cfg             -u
! yml_ic_input_format          -q
! ECOM_param_glb               -k
! EMP_param_glb                -w
! HELP                         -h
! gbl_debug (verbosity)        -d
! yaml_config                  -y
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Read the Command line
! ----------------------------------------------------------------------
! Define command line options
!                    long_opt    argument  short_opt
opts(1)  = option_s( "config",   .true.,      'c' )
opts(2)  = option_s( "podmode",  .true.,      'm' )
opts(3)  = option_s( "pobs",     .true.,      's' )
opts(4)  = option_s( "cobs",     .true.,      'o' )
opts(5)  = option_s( "eqm",      .true.,      'e' )
opts(6)  = option_s( "veq",      .true.,      'v' )
opts(7)  = option_s( "arclen",   .true.,      'a' )
opts(8)  = option_s( "predlen",  .true.,      'p' )
opts(9)  = option_s( "eopf",     .true.,      'r' )
opts(10) = option_s( "eopsol",   .true.,      't' )
opts(11) = option_s( "nutpre",   .true.,      'n' )
opts(12) = option_s( "estiter",  .true.,      'i' )
opts(13) = option_s( "sp3vel",   .false.,     'u' )
opts(14) = option_s( "icmodel",  .true.,      'q' )
opts(15) = option_s( "srpmodel", .true.,      'k' )
opts(16) = option_s( "empmodel", .true.,      'w' )
opts(17) = option_s( "help",     .false.,     'h' )
opts(18) = option_s( "verbosity", .true.,     'd' )
opts(19) = option_s( "config",    .true.,     'y' )

! Get the program name
call get_command_argument( 0, pgm_name )

! Read Command line from beginning
optind=1

! If no comand line options given provide some help [By default run the POD with Default POD.in file available]
!if (command_argument_count() .eq. 0 ) then
!   print*, trim(pgm_name),' -h or ',trim(pgm_name),' --help for command line help'
!   stop
!end if

! Set master configuration file name to be the default defined name (set in main_pod.f03)
POD_fname_cfg = 'DEFAULT'

! Process options given sequentially
do
   select case(getopt("c:m:s:o:e:v:a:p:r:t:n:i:u:q:k:w:d:y:h",opts))
      case( char(0) )
         exit
      case( 'c' )
!         print *, 'option config/c=', optarg
   	      POD_fname_cfg = trim(optarg)
      case( 'm' )
!         print *, 'option podmode/m=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i2)')	yml_pod_mode
          moption_on_command_line = .true.
      case( 's' )
!          print *, 'option pobs/s=', optarg
	      yml_orbit_filename = trim(optarg)
              soption_on_command_line = .true.
      case( 'o' )
!         print *, 'option cobs/o=', optarg
	      yml_ext_orbit_filename = trim(optarg)
              ooption_on_command_line = .true.
      case( 'e' )
!         print *, 'option eqm/e=', optarg
	      EQM_fname_cfg = trim(optarg)
      case( 'v' )
!         print *, 'option veq/v=', optarg
	      VEQ_fname_cfg = trim(optarg)
      case( 'a' )
!         print *, 'option arclen/a=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(f14.6)') yml_orbit_arc_determination
      case( 'p' )
!         print *, 'option predlen/p=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(f14.6)') yml_orbit_arc_prediction
      case( 'r' )
!         print *, 'option eopf/r=', optarg
	      EOP_fname_cfg = trim(optarg)
              yml_eop_filename = EOP_fname_cfg
              roption_on_command_line = .true.
      case( 't' )
!         print *, 'option eopsol/t=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i3)') yml_eop_option
          toption_on_command_line = .true.
      case( 'n' )
!      print *, 'option nutpre/n=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)') yml_iau_model
          noption_on_command_line = .true.
      case( 'i' )
!         print *, 'option estiter/i=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i2)') yml_estimator_iterations
      case( 'u' )
!      print *, 'option sp3vel/u=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)') sp3_velocity_cfg
          yml_write_sp3_velocities = (sp3_velocity_cfg > 0)
      case( 'd' )
!      print *, 'option verbosity/d=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i2)') gbl_debug
      case( 'q' )
!      print *, 'option icmode/u=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)') yml_ic_input_format
      case( 'k' )
!      print *, 'option srpmodel/k=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)') yml_ECOM_mode
          ! TODO: set srp bitfield and srp_apriori_model?
      case( 'w' )
!      print *, 'option empmodel/w=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)') EMP_param
          yml_EMP_mode = (EMP_param > 0)
      case( 'y' )
!      print *, 'option yaml/k=', optarg
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(a200)') yaml_config
          yaml_found = .true.
      case( 'h' )
              print*,'Default master POD (if not using yaml) config file = POD.in'
		  print*,'To run from default config file: ',trim(pgm_name),' or ',trim(pgm_name),' -c POD.in'
          print*,''
		  print*,'POD.in/yaml config file options by default can be overridden on the command line'
          print*,''
          print*,'Command line: ',trim(pgm_name),' -m -s -o -a -p -r -t -n -i -u -q -k -w -y -h '
          print*,''
          print*,'Where: '
          print*,'      -m --podmode = POD Mode:'
          print*,'				1 - Orbit Determination (pseudo-observations; orbit fitting)'
          print*,'				2 - Orbit Determination and Prediction'
          print*,'				3 - Orbit Integration (Equation of Motion only)'
          print*,'				4 - Orbit Integration and Partials (Equation of Motion and Variational Equations)'
          print*,'      -s --pobs    = Pseudo observations orbit .sp3 file name'
          print*,'      -o --cobs    = Comparison orbit .sp3 file name'
          print*,'      -a --arclen  = Orbit Estimation Arc length (hours)'
          print*,'      -p --predlen = Orbit Prediction Arc length (hours)'
	      print*,'      -r --eopf    = Earth Orientation Paramaeter (EOP) values file'
          print*,'      -t --eopsol  = Earth Orientation Paramaeter file type: (1,2,3)'
          print*,'				1 - IERS C04 EOP'
          print*,'				2 - IERS RS/PC Daily EOP'
          print*,'				3 - IGS RP + IERS RS/PC Daily (dX,dY)'
          print*,'      -n --nutpre  = IAU Precession / Nutation model'
          print*,'				2000 - IAU2000A'
          print*,'				2006 - IAU2006/2000A'
          print*,'      -i --estiter = Orbit Estimatimation Iterations (1 or greater)'
          print*,'      -u --sp3vel  = Output .sp3 file with velocities'
		  print*,'				0 - Do not write Velocity vector to sp3 orbit'
		  print*,'				1 - Write Velocity vector to sp3 orbit'  
          print*,'      -q --icmode  = Initial condition from parameter estimation procedure'
          print*,'      -k --srpmodel= 1: ECOM1, 2:ECOM2, 12:ECOM12, 3:SBOX'
          print*,'      -w --empmodel= 1: activated, 0: no estimation'
          print*,'      -d --verbosity = output verbosity level [Default: 0]'
          print*,'      -y --yaml = yaml config file'
          print*,'      -h --help.   = Print program help'
          print*,''
          print*,'Examples:'
          print*,''
          print*,'       ',trim(pgm_name),' -m 1 -q 1 -k 1 -w 0 -s igs16403.sp3 -o igs16403.sp3 -y ex1.yaml'		  
          print*,'       ',trim(pgm_name),' -m 2 -q 1 -k 1 -w 0 -s igs16403.sp3 -p 12 -y ex2.yaml'
          print*,''
          print*,'For orbit updates using Parameter Estimation Algorithm (PEA):'
          print*,'       ', trim(pgm_name),' -m 4 -q 2 -k 1 -w 0 -s igs16403.sp3 -o igs16403.sp3 -y ex3.yaml'
          stop
   end select
end do

return
end
