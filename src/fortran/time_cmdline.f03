subroutine time_cmdline

! This subroutine reads commnad line options and returns time variables defined in mdl_timeconfig
! Uses getopt command line parsing functions from f90getopt.F90

USE f90getopt
USE mdl_timeconfig
! ----------------------------------------------------------------------
! Command line local variables
integer            :: len_optarg
character (LEN=80) :: pgm_name

! Set number of long command line options available
type(option_s) :: opts(14)

! Current mdl_timeconfig varaible options
! ----------------------------------------------------------------------
! Input time variables: 
! MJD systm
!                       -j : MJD 
! ----------------------------------------------------------------------
! DOY system
!                       -i : day of year
!                       -y : year
! ----------------------------------------------------------------------
! Canlendar year system
!                       -y : year (4 digits)
!                       -m : month
!                       -d : day of month
! ----------------------------------------------------------------------
! GPS week system
!                       -w : GPS week
!                       -k : day of GPS week
! ----------------------------------------------------------------------
! Read the Command line
! ----------------------------------------------------------------------
! Define command line options
!                    long_opt    argument  short_opt

opts(1)  = option_s( "MJD_IN",     .true.,      'j' )
opts(2)  = option_s( "DOY_IN",     .true.,      'i' )
opts(3)  = option_s( "YEAR_IN",    .true.,      'y' )
opts(4)  = option_s( "MONTH_IN",   .true.,      'm' )
opts(5)  = option_s( "DAY_IN",     .true.,      'd' )
opts(6)  = option_s( "WEEK_IN",    .true.,      'w' )
opts(7)  = option_s( "WEEKDAY_IN", .true.,      'k' )
opts(8)  = option_s( "help",      .false.,      'h' )

! Get the program name
call get_command_argument( 0, pgm_name )

! Read Command line from beginning
optind=1

!if (command_argument_count() .eq. 0 ) then
!   print*, trim(pgm_name),' -h or ',trim(pgm_name),' --help for command line help'
!   stop
!end if


! Process options given sequentially
do
   select case(getopt("j:i:y:m:d:w:k:h",opts))
      case( char(0) )
         exit
      case( 'j' )
!         print *, 'option INPUT MID=', optarg
!   	  MJD_IN     = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(f8.2)') MJD_IN 
      case( 'i' )
!         print *, 'option INPUT DOY=', optarg
!	  DOY_IN     = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)')  DOY_IN
      case( 'y' )
!         print *, 'option INPUT YEAR=', optarg
!          YEAR_IN    = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i4)') YEAR_IN
      case( 'm' )
!         print *, 'option INPUT MONTH=', optarg
!          MONTH_IN   = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i3)') MONTH_IN
      case( 'd' )
!         print *, 'option INPUT DAY OF MONTH=', optarg
!          DAY_IN     = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i3)') DAY_IN
      case( 'w' )
!         print *, 'option INPUT GPS WEEK=', optarg
!          WEEK_IN    = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i5)') WEEK_IN
      case( 'k' )
!         print *, 'option INPUT DAY OF GPS WEEK=', optarg
!          WEEKDAY_IN = trim(optarg)
          len_optarg = len_trim(optarg)
          read(optarg(1:len_optarg),'(i2)') WEEKDAY_IN
      case( 'h' )
          print*,' Conversion among different time systems, including MJD, DOY, canlendar year and GPS week.'
          print*,''
	  print*,'To run main program : ',trim(pgm_name)
          print*,''
          print*,'Command line: ',trim(pgm_name),' -j -i -y -m -d -w -k -h'
          print*,''
          print*,' Input anyone of the following time systems'
          print*,'Where: '
          print*,'      -j --MJD (must use the float number)'
          print*,'======================================='
          print*,'      -i --DAY OF YEAR '
          print*,'      -y --YEAR (4 digits) e.g., 2020 '
          print*,'======================================='
          print*,'      -y --YEAR (4 digits) e.g., 2020 '
          print*,'      -m --MONTH'
          print*,'      -d --DAY OF MONTH'
          print*,'======================================='
          print*,'      -w --GPS WEEK'
          print*,'      -k --DAY OF GPS WEEK (0-6)'
          print*,'======================================='
          print*,'      -h --help.   = Print program help'
          print*,''
          print*,'Examples:'
          print*,''
          print*,'       ',trim(pgm_name),' -j 58887.0'
          print*,' OR '
          print*,'       ',trim(pgm_name),' -i 39 -y 2020'
          print*,' OR '
          print*,'       ',trim(pgm_name),' -d 8 -m 2 -y 2020'
          print*,' OR '
          print*,'       ',trim(pgm_name),' -w 2091 -k 6'
         
          stop
   end select
end do

return
end
