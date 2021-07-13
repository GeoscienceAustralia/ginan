! ----------------------------------------------------------------------
! SUBROUTINE: report (output debug, etc)
! ----------------------------------------------------------------------
! Purpose:
!     Routine to report on the process status
!     The routine reports status messages which indicate where in the
!     processing we are; warnings which are are not fatal to the processing
!     but could be used as diagnostic of later problems; and fatal errors
!     which cause the program to stop running.
 
!     For all reports, the messsages are appended to files named
!     'prog' .status, .warn or .fatal depending on the type of error
!     These files are opened with append status and closed before the
!     routine ends (the later is to ensure that the UNIX file buffers
!     flushed so that the user can see the current status).
 
!     Standard examples of the use of this routine would be:
!     STATUS:
!     call report('STATUS','POD','model','Beginning process',' ',0)
!     WARNING:
!     call report('WARNING','POD','wrtout',
!    .                  'Site radius too large',' ',0)
!     FATAL:
!     call report('FATAL','POD','invert',
!    .                   'Normal equations non-positive definite',' ',0)
!     CLEAR:
!     call report('CLEAR','POD',' ',' ',' ',0)
! ----------------------------------------------------------------------
! Author :	John Donovan, Geoscience Australia
! Created:	26 March 2020
! ----------------------------------------------------------------------

!*************************************************************************************
! utility subroutines called within

!*************************************************************************************
 
      subroutine rep_open( prog, extent, rep_file, rep_unit )
 
!     This routine will open the status/warning/fatal file with access
!     append.  The unit number is searched to find an available unit
!     and if no unit can be found the unit is returned as 6 so that
!     stdout will be used.
 
! PASSED VARIABLES
 
!   rep_unit    - Unit number for the file.  Returned by this
!               - routine
 
      use pod_yaml
      integer*4 rep_unit
 
!   prog      - Name of the prog calling report_stat
!   extent      - extent to be given to file name (status, warning
!               - or fatal).
!   rep_file        - Name given to output file (returned).
 
      character*(*) prog, extent, rep_file
 
! LOCAL VARIABLES
 
!   ierr        - IOSTAT error when file opened.
!   trimlen     - Length of non-blank portion of string
 
      integer*4 ierr, trimlen
 
!   unit_open   - Set true by inquire if unit is already in use.
 
      logical unit_open

      character*8 access

 
!***  First generate the file name
 
      rep_file = TRIM (yml_output_dir) // "/" // TRIM(prog) // extent
 
!     Now find an available unit.  Units 100-499 are searched
      unit_open = .true.
      rep_unit  =    100
      do while ( unit_open .and. rep_unit.lt.500 )
          inquire( unit=rep_unit, opened=unit_open)
 
!         If the unit is in use, try the next one
          if( unit_open ) rep_unit = rep_unit + 1
      end do
 
!**   If the unit is still open, then we have a major problem.   We can
!     find a unit number for output so warning user and set the unit
!     to 6
      if( unit_open ) then
          write(*,120) extent(2:)
 120      format('WARNING: Cannot find available unit number for ',a, &
                 ' reports.  Output to stdout')
          rep_unit = 6
      else
 
!         Found unit number, so open file
          access = 'append'
! for IBM AIX
          open(rep_unit, file=rep_file, status='unknown',position=access, iostat=ierr)
          if( ierr.ne.0 ) then
 
!             Error opening the output file.  Tell user of problem
              write(*,140) ierr, extent(2:), &
                     TRIM(rep_file), rep_unit
 140          format('WARNING: IOSTAT error ',i4,' occurred opening ', &
                    a,' file ',a,' on unit ',i3)
              rep_unit = 6
          end if
      end if
 
!**** Thats all
      return
      end
 
!****************************************************************************************

      subroutine casefold( str )

      character*(*) str
      character*(200) ucstr
      integer i, c

      ucstr = TRIM(str)
      do i=1, len(ucstr)
          c = ichar(ucstr(i:i))
          if (c .ge. 97 .and. c .le.122) ucstr(i:i) = achar(c-32)
      enddo
       
      str = ucstr
      end

!****************************************************************************************

      subroutine caseunfold( str )

      character*(*) str
      character*(200) lcstr
      integer i, c

      lcstr = TRIM(str)
      do i=1, len(lcstr)
          c = ichar(lcstr(i:i))
          if (c .ge. 65 .and. c .le.90) lcstr(i:i) = achar(c+32)
      enddo
       
      str = lcstr
       
      end

!****************************************************************************************

      subroutine check_ascii( str )

      character*(*) str
      character*(200) astr
      integer i, c

      astr = TRIM(str)
      do i=1, len(astr)
          c = ichar(astr(i:i))
          if (c .le. 31 .or. c .ge.127) astr(i:i) = achar(46) !'.'
      enddo

      str = astr
      end

!****************************************************************************************
  subroutine report( typep, progp, routinep, messagep, filep, ierr )

!
! PASSED Variables
 
!   ierr    - Error number associated with report.  This value is
!           - printed for warnings and fatal errors when non-zero.
 
      integer*4 ierr
 
!   type    - Type of message:  Only three types are allowed, any
!           - other type will cause this routine to print a warning
!           - message.  The types are (only first four characters
!           - checked:
!           - status  -- General reporting of status for progress
!           -       information
!           - warning -- Non-fatal error in the processing, could be
!           -       diagonistic of later fatal errors
!           - fatal   -- Fatal error, If ierr=0 then only the message
!           -       is printed, otherwize the error number is
!           -       printed with the message.
!           - clear   -- Removes the status, warning and fatal files
!                   for a given prog name.
 
!   prog  - Name of prog (main program) (e.g. pod)
!   subroutine  - Name of the subroutine from which the report is
!           - made.
!   file    - Name of the file or string being manipulated.  This is
!             printed whenever a non-zero length string is passed.
!             It is included with (Name xxxxx) at the end of the line.
!   message - Message to be printed
                                              
      character*(*) typep, progp, routinep, filep, messagep
      character*(256) etype, prog, routine, srcfile, message

! LOCAL VARIABLES
 
!   rep_date(5) - Yr, mon, day, hr, min that the report is made
!   TRIM        - Library routine to return the non-blank portion
!               - of a string
 
!   jerr        - IOSTAT error writing to the status file
!   kerr        - IOSTAT error writing to stdout
!   cerr        - IOSTAT error closing the status file
 
!   rep_unit    - Unit number assigned to the report file.  If we have
!               - trouble with the opening the unit, rep_unit is set
!               - 6 so that output will go to to stdout.

!   i           - Counter used to find / or start of string in the prog
!                 name

      integer    my_date(8), rep_unit

      integer*4  rep_date(5), jerr, kerr, cerr, i, proglen
 
!   rep_sec     - Seconds that the report is made
 
      real*8 rep_sec
 
!   rep_file    - Name of the report file to which the messages are
!               - written.  Generated from the prog name with an an
!               - extent based on the type.  (NOTE: Module is upper
!               - case when name generated).
!   mod_name - Names of the prog for creating the file name
!                 (Removes the full information if present)
 
 
      character*256 rep_file
      character*64 prog_name
         
!**** Make a copy of all passed variables.  Stops us overwritting
!     a fixed string.
      etype    = typep
      prog  = progp
      routine = routinep
      srcfile    = filep
      message = messagep

!***  Start: First get the system time so that we know when the report
!     was made
 
      call date_and_time( values=my_date )

!     Remove the century portion of the date so that all architectures look the
!     same
      rep_date(1) = my_date(1) - int(my_date(1)/100)*100
      rep_date(2) = my_date(2)
      rep_date(3) = my_date(3)
      rep_date(4) = my_date(5)
      rep_date(5) = my_date(6)
      rep_sec = my_date(7) + real(my_date(8))/1000.d0
 
!     Make sure all the strings contain ascii characters. The routines below also trim strings
!     to minimum length
      call check_ascii( prog )
      call check_ascii( routine )
      call check_ascii( message )
      call check_ascii( srcfile )
      call check_ascii( etype )
 
!     See what type of report we are to make (this saves
!     unnecessary opening of files).  Casefold the type in case user
!     calls with the wrong case.  Module and subroutine are set case for
!     "nice" looking output.
      call casefold( etype )
      call casefold( prog )
      call caseunfold( routine )
 
!     Get the lengths of the strings.  (1 is minumum length to stop fortran
!     error if string(1:0) is attempted.)

      if( ichar(prog(1:1))   .eq. 0 ) prog(1:1)  = ' '
      if( ichar(routine(1:1)).eq. 0 ) routine(1:1) = ' '
      if( ichar(message(1:1)).eq. 0 ) message(1:1) = ' '
      if( ichar(srcfile(1:1)).eq. 0 ) srcfile(1:1)    = ' '
      if( ichar(etype(1:1))  .eq. 0 ) etype(1:1)    = ' '

      proglen = len(prog)
!
!     Now strip out the prog name from the end the prog string.
!     (Look for /'s in the name in case full name passed)
      i = proglen
      do while ( i.gt.1 .and. prog(i:i).ne.'/' )
          i = i - 1
      end do
      if( prog(i:i).eq.'/' ) i = i + 1
      prog_name = prog(i:proglen) 
      
!     Set the error flags for errors in this routine to zero.  At end we
!     check these are still zero.
      jerr = 0
      kerr = 0
      cerr = 0
 
!****  Report the appropriate type of messages.
!     STATUS report
      if( etype(1:4).eq.'STAT' ) then
 
!         Generate the name the file to write the status to and get
!         an available unit number to open the file with status append.
          call rep_open( prog_name, '.status', rep_file, rep_unit)
 
!         Now write the status message
          if( len(trim(srcfile)).le.1 ) then
              write(rep_unit, 120, iostat=jerr) rep_date, rep_sec, &
                    trim(prog),trim(routine), trim(message)
              write(*, 120, iostat=kerr) rep_date, rep_sec, &
                    trim(prog), trim(routine), trim(message)
 
 120  format('STATUS :',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a)
           else
!             if srcfile name passed print this as well)           
              write(rep_unit, 140, iostat=jerr) rep_date, rep_sec, &
                    trim(prog), trim(routine), trim(message),trim(srcfile)
              write(*, 140, iostat=kerr) rep_date, rep_sec, &
                    trim(prog), trim(routine), trim(message), trim(srcfile)
 
 140  format('STATUS :',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a,' (Name ',a,')')
           end if
           
!***  WARNING messages that are not fatal (can be for information or to
!     report problems with file access and/or string reading.
      else if ( etype(1:4).eq.'WARN' ) then
 
!         Generate file name and open
          call rep_open( prog_name, '.warning', rep_file, rep_unit)
 
!         If the error flag is zero, assume the file name is not
!         needed
          if( ierr.eq. 0 ) then

!             If the file name is zero or 1 charcater long, just
!             print the message, otherwize output the file name
!             as well.
              if( len(trim(srcfile)).le.1 ) then
                  write(rep_unit, 220, iostat=jerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message)
                  write(*, 220, iostat=kerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message)
 
 220  format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a)
               else
!                 Print the file name as well.
                  write(rep_unit, 230, iostat=jerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message), trim(srcfile)
                  write(*, 230, iostat=kerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message), trim(srcfile)
 
 230  format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a,' (Name ',a,')')
              end if
 
          else
!             Report the messsage and the file name and the error number
              write(rep_unit, 240, iostat=jerr) rep_date, rep_sec, &
                    trim(prog), trim(routine), trim(message), trim(srcfile), ierr
              write(*, 240, iostat=kerr) rep_date, rep_sec, &
                    trim(prog), trim(routine), trim(message), trim(srcfile), ierr
 240  format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a,1x,a,' ERROR ',i5)
 
          end if
 
!**** FATAL error messages:  These are processed the same as warnings
!     but will cause the program to stop running.
      else if ( etype(1:4).eq.'FATA' ) then
 
!         Generate file name and open
          call rep_open( prog_name, '.fatal', rep_file, rep_unit)
 
!         If the error flag is zero, assume the file name is not
!         needed
          if( ierr.eq. 0 ) then

!             Again check to see if the file name has been passed
              if(len(trim(srcfile)).le.1 ) then
                  write(rep_unit, 320, iostat=jerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message)
                  write(*, 320, iostat=kerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message)
 
 320  format('FATAL  :',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a)
              else

!                 Print the file name as well
                  write(rep_unit, 330, iostat=jerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message), trim(srcfile)
                  write(*, 330, iostat=kerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message), trim(srcfile)
 
 330  format('FATAL  :',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a,' (Name ',a,')')
              end if         
          else
!             Report the messsage and the file name and the error number
              write(rep_unit, 340, iostat=jerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message), trim(srcfile), ierr
              write(*, 340, iostat=kerr) rep_date, rep_sec, &
                        trim(prog), trim(routine), trim(message), trim(srcfile), ierr
 340  format('FATAL  :',3(i2.2),':',2(i2.2),':',f4.1,1x,a,'/',a,': ',a,1x,a,' ERROR ',i5)  
          end if
 
!         For the fatal errors we close the rep_file and stop the program
!         running
          close( rep_unit, iostat=cerr )
          stop 'FATAL Error: Stop from report'
 
!**** See if we clearing the files
      else if( etype(1:4).eq.'CLEA' ) then

!         Open each of the types of files and delete on closing
          if( proglen.gt.1 ) then
              call rep_open( prog_name, '.status', rep_file, rep_unit)
              if (rep_unit .ne. 6) close(rep_unit, iostat=cerr, status='delete')
              call rep_open( prog_name, '.warning', rep_file, rep_unit)
              if (rep_unit .ne. 6) close(rep_unit, iostat=cerr, status='delete')
              call rep_open( prog_name, '.fatal', rep_file, rep_unit)
              if (rep_unit .ne. 6) close(rep_unit, iostat=cerr, status='delete')
          end if
      else
 
!         An unknown type of report has been called.  First warn the user
!         that an unknown type has been attempted, and then print out the
!         message information as if it were a warning.
!         Generate file name and open
          call rep_open( prog_name, '.warning', rep_file, rep_unit)
          write(rep_unit, 420, iostat=jerr) rep_date, rep_sec, &
                  prog, routine, etype
          write(*, 420, iostat=kerr) rep_date, rep_sec, &
                  prog, routine, etype
 420      format('WARNING:',3(i2.2),':',2(i2.2),':',f4.1,1x, &
                 a,'/',a,': Unknown type of status. Error in call to ', &
                'report_stat -- ',a)
 
!         Now print the standard warning format.
          if( ierr.eq. 0 ) then

!             See if the file name has been passed.
              if(len(trim(srcfile)).le.1 ) then
                  write(rep_unit, 220, iostat=jerr) rep_date, rep_sec, &
                          prog, routine, message
                  write(*, 220, iostat=kerr) rep_date, rep_sec, &
                          prog, routine, message
               else
!                 Print the file name as well.
                  write(rep_unit, 230, iostat=jerr) rep_date, rep_sec, &
                          prog, routine, message, srcfile
                  write(*, 230, iostat=kerr) rep_date, rep_sec, &
                          prog, routine, message, srcfile
              end if
          else
!             Report the messsage and the file name and the error number
              write(rep_unit, 240, iostat=jerr) rep_date, rep_sec, &
                          prog, routine, message, srcfile, ierr
              write(*, 240, iostat=kerr) rep_date, rep_sec, &
                          prog, routine, message, srcfile, ierr
          end if
 
      end if
 
!***  Now clean up before exiting. Close the report file so that it will
!     be updated and check if any errors occurred during the writing of
!     the messages
 
      close( rep_unit, iostat=cerr )
      if( jerr.ne.0 ) then
          write(*,520) jerr, TRIM(rep_file)
 520      format('WARNING: IOSTAT error ',i4,' occurred in ', &
             'report writing the status file ', a)
      end if
      if( kerr.ne.0 ) then
          write(*,540) kerr
 540      format('WARNING: IOSTAT error ',i4,' occurred in ', &
                 'report writing the status report to stdout')
      end if
 
      if( cerr.ne.0 ) then
          write(*,560) cerr, TRIM(rep_file)
 560      format('WARNING: IOSTAT error ',i4,' occurred in ', &
                 'report closing the status file ', a)
      end if
 
!***  Thats all
      return
      end
 
