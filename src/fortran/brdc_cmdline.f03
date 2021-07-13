! ----------------------------------------------------------------------
! SUBROUTINE : brdc_cmdline
! ----------------------------------------------------------------------
! Purpose:
!  This subroutine reads commnad line options and returns variables defined in mdl_brdconfig
!  Uses getopt command line parsing functions from f90getopt.F90
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------
subroutine brdc_cmdline

USE f90getopt
USE mdl_brdconfig
USE mdl_config
! ----------------------------------------------------------------------
! Command line local variables
integer            :: len_optarg
character (LEN=80) :: pgm_name

! Set number of long command line options available
type(option_s) :: opts(14)

! Current mdl_brdconfig varaible options
! ----------------------------------------------------------------------
! Input file name              -f
! Output file name             -o
! Satellite constellation      -c
! leap second                  -l
! Information                  -h
! ----------------------------------------------------------------------
! ----------------------------------------------------------------------
! Read the Command line
! ----------------------------------------------------------------------
! Define command line options
!                    long_opt    argument  short_opt

opts(1)  = option_s( "inputfile",     .true.,      'f' )
opts(2)  = option_s( "outputfile",    .true.,      'o' )
opts(3)  = option_s( "constellation", .true.,      'c' )
opts(4)  = option_s( "leap second",   .true.,      'l' )
opts(5)  = option_s( "EOP file",      .true.,      'r' )
opts(6)  = option_s( "help",          .true.,      'h' )


! Get the program name
call get_command_argument( 0, pgm_name )

! Read Command line from beginning
optind=1

! If no comand line options given provide some help [By default run the POD with Default POD.in file available]
!if (command_argument_count() .eq. 0 ) then
!   print*, trim(pgm_name),' -h or ',trim(pgm_name),' --help for command line help'
!   stop
!end if


! Process options given sequentially
do
   select case(getopt("f:o:c:l:r:h",opts))
      case( char(0) )
         exit
      case( 'f' )
!         print *, 'option INPUT FILE=', optarg
   	      INFILENAME  = trim(optarg)
      case( 'o' )
!         print *, 'option OUTPUT FILE=', optarg
	      OUTFILENAME = trim(optarg)
      case( 'c' )
!         print *, 'option GNSS constellation=', optarg
              GNSSTYPE    = trim(optarg)
      case( 'l' )
!         print *, 'option leap second=', optarg
              leapsec_filename_cfg  = trim(optarg)
      case( 'r' )
!         print *, 'option EOP file=', optarg
              EOP_fname_cfg = trim(optarg)
      case( 'h' )
          print*,' Conversion from broadcast dynamic elements to earth-center earth-fixed (ecef) coordinates '
          print*,' The program can work for multi-GNSS constellations, except for GLONASS that will be handled soon.'
	  print*,'To run main program : ',trim(pgm_name)
          print*,''
          print*,'Command line: ',trim(pgm_name),' -f -o -c -h '
          print*,''
          print*,'Where: '
          print*,'      -f --inputfile  = input file name '
          print*,'      -o --outputfile = output file name'
          print*,'      -c --constellation = GNSS TYPE, e.g., A: All GNSS; G: GPS; R: GLONASS'
          print*,'                                            E: GALILEO;  C: BDS; J: QZSS'
          print*,'      -l --leap second file = leap.second'
          print*,'      -r --EOP file'
          print*,'      -h --help.   = Print program help'
          print*,''
          print*,'Examples:'
          print*,''
          print*,'       ',trim(pgm_name),' -f BRDC00IGS_R_20163210000_01D_MN.rnx -o brdc16321.sp3 -c A -l leap.second &
                                            -r finals2000A.daily'
          stop
   end select
end do

return
end
