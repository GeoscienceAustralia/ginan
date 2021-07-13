! -----------------------------------------------------------------------------
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

program test_yaml

   use pod_yaml
   use f90getopt

   implicit none
   character (256) filepath

   call get_command_argument(1, filepath)
   if (filepath=='') then
      write (*,*) 'ERROR: path to YAML file not provided.'
      stop 2
   end if
   call get_yaml(filepath)

end program test_yaml
