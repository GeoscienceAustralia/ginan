! ----------------------------------------------------------------------
! MODULE: f90getopt
! ----------------------------------------------------------------------
! Purpose:
!   get options from command line
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------
module f90getopt

    implicit none
    
    ! Portable declaration of stderr, stdin, stdout
#ifdef f2003
    use, intrinsic :: iso_fortran_env, only : input_unit=>stdin, &
        output_unit=>stdout, &
        error_unit=>stderr
#else
#define stdin  5
#define stdout 6
#define stderr 0
#endif

    character(len=80):: optarg        ! Option's value
    character        :: optopt        ! Option's character
    integer          :: optind=1      ! Index of the next argument to process
    logical          :: opterr=.true. ! Errors are printed by default. Set opterr=.false. to suppress them

    type option_s
        character(len=80) :: name     ! Name of the option
        logical           :: has_arg  ! Option has an argument (.true./.false.)
        character         :: short    ! Option's short character equal to optopt
    end type option_s

    ! grpind is index of next option within group; always >= 2
    integer, private:: grpind=2

contains

    ! ----------------------------------------
    ! Return str(i:j) if 1 <= i <= j <= len(str),
    ! else return empty string.
    ! This is needed because Fortran standard allows but doesn't *require* short-circuited
    ! logical AND and OR operators. So this sometimes fails:
    !     if ( i < len(str) .and. str(i+1:i+1) == ':' ) then
    ! but this works:
    !     if ( substr(str, i+1, i+1) == ':' ) then

    character function substr( str, i, j )
        ! arguments
        character(len=*), intent(in):: str
        integer, intent(in):: i, j

        if ( 1 <= i .and. i <= j .and. j <= len(str)) then
            substr = str(i:j)
        else
            substr = ''
        endif
    end function substr


    ! ----------------------------------------
    character function getopt( optstring, longopts )
        ! arguments
        character(len=*), intent(in):: optstring
        type(option_s),   intent(in), optional:: longopts(:)

        ! local variables
        character(len=80):: arg
        optarg = ''
        if ( optind > command_argument_count()) then
            getopt = char(0)
        endif

        call get_command_argument( optind, arg )
        if ( present( longopts ) .and. arg(1:2) == '--' ) then
            getopt = process_long( longopts, arg )
        elseif ( arg(1:1) == '-' ) then
            getopt = process_short( optstring, arg )
        else
            getopt = char(0)
        endif
    end function getopt


    ! ----------------------------------------
    character function process_long( longopts, arg )
        ! arguments
        type(option_s),   intent(in):: longopts(:)
        character(len=*), intent(in):: arg

        ! local variables
        integer :: i = 0
        integer :: j = 0
        integer :: len_arg = 0             ! length of arg
        logical :: has_equalsign = .false. ! arg contains equal sign?

        len_arg = len_trim(arg)

        ! search for equal sign in arg and set flag "has_equalsign" and
        ! length of arg (till equal sign)
        do j=1, len_arg
            if (arg(j:j) == "=") then
                has_equalsign = .true.
                len_arg = j-1
                exit
            endif
        enddo

        ! search for matching long option

        if (.not. has_equalsign) then
            optind = optind + 1
        endif

        do i = 1, size(longopts)
            if ( arg(3:len_arg) == longopts(i)%name ) then
                optopt = longopts(i)%short
                process_long = optopt
                if ( longopts(i)%has_arg ) then
                    if (has_equalsign) then ! long option has equal sign between value and option
                        if (arg(len_arg+2:) == '') then ! no value (len_arg+2 value after "="
                            write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
                            process_long=char(0) ! Option not valid
                        else
                            call get_command_argument(optind, optarg)
                            optarg = optarg(len_arg+2:)
                            optind = optind + 1
                        endif
                    else ! long option has no equal sign between value and option
                        if ( optind <= command_argument_count()) then
                            call get_command_argument( optind, optarg )
                            optind = optind + 1
                        elseif ( opterr ) then
                            write(stderr, '(a,a,a)') "ERROR: Option '", trim(arg), "' requires a value"
                            process_long=char(0) ! Option not valid
                        endif
                    endif
                endif
                return
            endif
        end do
        ! else not found
        process_long = char(0)
        optopt='?'
        if ( opterr ) then
            write(stderr, '(a,a,a)') "ERROR: Unrecognized option '", arg(1:len_arg), "'"
        endif
        return
    end function process_long


    ! ----------------------------------------
    character function process_short( optstring, arg )
        ! arguments
        character(len=*), intent(in):: optstring, arg

        ! local variables
        integer:: i, arglen

        arglen = len( trim( arg ))
        optopt = arg(grpind:grpind)
        process_short = optopt

        i = index( optstring, optopt )
        if ( i == 0 ) then
            ! unrecognized option
            process_short = '?'
            if ( opterr ) then
                write(stderr, '(a,a,a)') "ERROR: Unrecognized option '-", optopt, "'"
            endif
        endif
        if ( i > 0 .and. substr( optstring, i+1, i+1 ) == ':' ) then
            ! required argument
            optind = optind + 1
            if ( arglen > grpind ) then
                ! -xarg, return remainder of arg
                optarg = arg(grpind+1:arglen)
            elseif ( optind <= command_argument_count()) then
                ! -x arg, return next arg
                call get_command_argument( optind, optarg )
                optind = optind + 1
            elseif ( opterr ) then
                write(stderr, '(a,a,a)') "ERROR: Option '-", optopt, "' requires a value"
                process_short = char(0) ! Option not valid
            endif
            grpind = 2
        elseif ( arglen > grpind ) then
            ! no argument (or unrecognized), go to next option in argument (-xyz)
            grpind = grpind + 1
        else
            ! no argument (or unrecognized), go to next argument
            grpind = 2
            optind = optind + 1
        endif
    end function process_short

end module f90getopt
