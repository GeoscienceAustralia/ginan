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

module yaml_types

   implicit none

   private

   public type_node,type_scalar,type_null,type_error,real_kind
   public type_dictionary,type_key_value_pair
   public type_list,type_list_item

   integer,parameter :: string_length = 1024
   integer,parameter :: real_kind = kind(1.0d0)

   type,abstract :: type_node
      character(len=string_length) :: path = ''
   contains
      procedure (node_dump),deferred :: dump
      procedure                      :: set_path => node_set_path
      procedure                      :: finalize => node_finalize
   end type

   abstract interface
      subroutine node_dump(self,unit,indent)
         import type_node
         class (type_node),intent(in) :: self
         integer,intent(in) :: unit,indent
      end subroutine
   end interface

   type,extends(type_node) :: type_scalar
      character(len=string_length) :: string = ''
   contains
      procedure :: dump       => value_dump
      procedure :: to_logical => scalar_to_logical
      procedure :: to_integer => scalar_to_integer
      procedure :: to_real    => scalar_to_real
   end type

   type,extends(type_node) :: type_null
   contains
      procedure :: dump => null_dump
   end type

   type type_key_value_pair
      character(len=string_length)       :: key   = ''
      class (type_node),         pointer :: value => null()
      logical                            :: accessed = .false.
      type (type_key_value_pair),pointer :: next  => null()
   end type

   type,extends(type_node) :: type_dictionary
      type (type_key_value_pair),pointer :: first => null()
   contains
      procedure :: get            => dictionary_get
      procedure :: get_scalar     => dictionary_get_scalar
      procedure :: get_dictionary => dictionary_get_dictionary
      procedure :: get_list       => dictionary_get_list
      procedure :: get_string     => dictionary_get_string
      procedure :: get_logical    => dictionary_get_logical
      procedure :: get_integer    => dictionary_get_integer
      procedure :: get_real       => dictionary_get_real
      procedure :: set            => dictionary_set
      procedure :: set_string     => dictionary_set_string
      procedure :: dump           => dictionary_dump
      procedure :: flatten        => dictionary_flatten
      procedure :: reset_accessed => dictionary_reset_accessed
      procedure :: set_path       => dictionary_set_path
      procedure :: finalize       => dictionary_finalize
   end type

   type type_list_item
      class (type_node),    pointer :: node => null()
      type (type_list_item),pointer :: next => null()
   end type

   type,extends(type_node) :: type_list
      type (type_list_item),pointer :: first => null()
   contains
      procedure :: append   => list_append
      procedure :: dump     => list_dump
      procedure :: set_path => list_set_path
   end type

   type type_error
      character(len=string_length) :: message
   end type

contains

   subroutine node_finalize(self)
      class (type_node),intent(inout) :: self
   end subroutine

   subroutine dictionary_reset_accessed(self)
      class (type_dictionary),intent(in) :: self
      type (type_key_value_pair),pointer :: pair
      pair => self%first
      do while (associated(pair))
         pair%accessed = .false.
         pair => pair%next
      end do
   end subroutine

   function dictionary_get(self,key) result(value)
      class (type_dictionary),intent(in) :: self
      character(len=*),       intent(in) :: key
      class(type_node),pointer           :: value

      type (type_key_value_pair),pointer :: pair

      nullify(value)
      pair => self%first
      do while (associated(pair))
         if (pair%key==key) exit
         pair => pair%next
      end do
      if (associated(pair)) then
         value => pair%value
         pair%accessed = .true.
      end if
   end function

   subroutine dictionary_set(self,key,value)
      class (type_dictionary),intent(inout) :: self
      character(len=*),       intent(in)    :: key
      class(type_node),pointer              :: value

      type (type_key_value_pair),pointer :: pair

      if (.not.associated(self%first)) then
         ! This will be the first pair.
         allocate(self%first)
         pair => self%first
      else
         ! Try to find a pair with the same key, or failing that, the last pair.
         pair => self%first
         do while (associated(pair%next))
            if (pair%key==key) exit
            pair => pair%next
         end do
         if (.not.pair%key==key) then
            ! Key did not exist yet, which must mean we are operating on the last existing pair.
            ! Append a new pair.
            allocate(pair%next)
            pair => pair%next
         else
            deallocate(pair%value)
         end if
      end if

      ! Store key and value.
      pair%key = key
      pair%value => value
   end subroutine

   subroutine dictionary_set_string(self,key,value)
      class (type_dictionary),intent(inout) :: self
      character(len=*),       intent(in)    :: key,value

      class (type_scalar),pointer :: scalar_node
      class (type_node),  pointer :: node

      allocate(scalar_node)
      scalar_node%string = value
      node => scalar_node
      call self%set(key,node)
   end subroutine

   subroutine value_dump(self,unit,indent)
      class (type_scalar),intent(in) :: self
      integer,            intent(in) :: unit,indent
      write (unit,'(a)') trim(self%string)
   end subroutine

   subroutine null_dump(self,unit,indent)
      class (type_null),intent(in) :: self
      integer,          intent(in) :: unit,indent
      write (unit,'(a)') 'null'
   end subroutine

   recursive subroutine dictionary_dump(self,unit,indent)
      class (type_dictionary),intent(in) :: self
      integer,                intent(in) :: unit,indent
      type (type_key_value_pair),pointer :: pair

      logical :: first

      first = .true.
      pair => self%first
      do while (associated(pair))
         if (first) then
            first = .false.
         else
            write (unit,'(a)',advance='NO') repeat(' ',indent)
         end if

         select type (value=>pair%value)
            class is (type_dictionary)
               write (unit,'(a)') trim(pair%key)//':'
               write (unit,'(a)',advance='NO') repeat(' ',indent+2)
               call value%dump(unit,indent+2)
            class is (type_list)
               write (unit,'(a)') trim(pair%key)//':'
               write (unit,'(a)',advance='NO') repeat(' ',indent+2)
               call value%dump(unit,indent+2)
            class default
               write (unit,'(a)',advance='NO') trim(pair%key)//': '
               call value%dump(unit,indent+len_trim(pair%key)+2)
         end select
         pair => pair%next
      end do
   end subroutine

   recursive subroutine dictionary_flatten(self,target,prefix)
      class (type_dictionary),intent(in)    :: self
      type (type_dictionary), intent(inout) :: target
      character(len=*),       intent(in)    :: prefix

      type (type_key_value_pair),pointer :: pair

      pair => self%first
      do while (associated(pair))
         select type (value=>pair%value)
            class is (type_scalar)
               call target%set_string(prefix//trim(pair%key),value%string)
            class is (type_dictionary)
               call value%flatten(target,prefix=prefix//trim(pair%key)//'/')
         end select
         pair => pair%next
      end do
   end subroutine

   function scalar_to_logical(self,default,success) result(value)
      class (type_scalar),intent(in)  :: self
      logical,            intent(in)  :: default
      logical,optional,   intent(out) :: success
      logical                         :: value

      integer :: ios

      value = default
      read(self%string,*,iostat=ios) value
      if (present(success)) success = (ios == 0)
   end function

   function scalar_to_integer(self,default,success) result(value)
      class (type_scalar),intent(in)  :: self
      integer,            intent(in)  :: default
      logical,optional,   intent(out) :: success
      integer                         :: value

      integer :: ios

      value = default
      read(self%string,*,iostat=ios) value
      if (present(success)) success = (ios == 0)
   end function

   function scalar_to_real(self,default,success) result(value)
      class (type_scalar),intent(in)  :: self
      real(real_kind),    intent(in)  :: default
      logical,optional,   intent(out) :: success
      real(real_kind)                 :: value

      integer :: ios

      value = default
      read(self%string,*,iostat=ios) value
      if (present(success)) success = (ios == 0)
   end function

   recursive subroutine node_set_path(self,path)
      class (type_node),intent(inout) :: self
      character(len=*), intent(in)    :: path
      self%path = path
   end subroutine

   recursive subroutine dictionary_set_path(self,path)
      class (type_dictionary),intent(inout) :: self
      character(len=*),       intent(in)    :: path

      type (type_key_value_pair),pointer :: pair

      self%path = path
      pair => self%first
      do while (associated(pair))
         call pair%value%set_path(trim(self%path)//'/'//trim(pair%key))
         pair => pair%next
      end do
   end subroutine

   function dictionary_get_scalar(self,key,required,error) result(scalar)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      logical,                  intent(in) :: required
      type(type_error),pointer             :: error
      class (type_scalar),pointer          :: scalar

      class (type_node),pointer          :: node

      nullify(error)
      nullify(scalar)
      node => self%get(key)
      if (required.and..not.associated(node)) then
         allocate(error)
         error%message = trim(self%path)//' does not contain key "'//trim(key)//'".'
      end if
      if (associated(node)) then
         select type (node)
            class is (type_scalar)
               scalar => node
            class is (type_null)
               allocate(error)
               error%message = trim(node%path)//' must be set to a scalar value, not to null.'
            class is (type_dictionary)
               allocate(error)
               error%message = trim(node%path)//' must be set to a scalar value, not to a dictionary.'
            class is (type_list)
               allocate(error)
               error%message = trim(node%path)//' must be set to a scalar value, not to a list.'
         end select
      end if
   end function

   function dictionary_get_dictionary(self,key,required,error) result(dictionary)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      logical,                  intent(in) :: required
      type(type_error),pointer             :: error
      class (type_dictionary),pointer      :: dictionary

      class (type_node),pointer :: node

      nullify(error)
      nullify(dictionary)
      node => self%get(key)
      if (required.and..not.associated(node)) then
         allocate(error)
         error%message = trim(self%path)//' does not contain key "'//trim(key)//'".'
      end if
      if (associated(node)) then
         select type (typed_node=>node)
            class is (type_null)
               allocate(dictionary)
               dictionary%path = node%path
            class is (type_dictionary)
               dictionary => typed_node
            class default
               allocate(error)
               error%message = trim(node%path)//' must be a dictionary.'
         end select
      end if
   end function

   function dictionary_get_list(self,key,required,error) result(list)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      logical,                  intent(in) :: required
      type(type_error),pointer             :: error
      class (type_list),pointer            :: list

      class (type_node),pointer :: node

      nullify(error)
      nullify(list)
      node => self%get(key)
      if (required.and..not.associated(node)) then
         allocate(error)
         error%message = trim(self%path)//' does not contain key "'//trim(key)//'".'
      end if
      if (associated(node)) then
         select type (typed_node=>node)
            class is (type_null)
               allocate(list)
            class is (type_list)
               list => typed_node
            class default
               allocate(error)
               error%message = trim(node%path)//' must be a list.'
         end select
      end if
   end function

   function dictionary_get_string(self,key,default,error) result(value)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      character(len=*),optional,intent(in) :: default
      type(type_error),pointer             :: error
      character(len=string_length)         :: value

      class(type_scalar),pointer           :: node

      if (present(default)) value = default
      node => self%get_scalar(key,.not.present(default),error)
      if (associated(node)) value = node%string
   end function

   function dictionary_get_logical(self,key,default,error) result(value)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      logical,         optional,intent(in) :: default
      type(type_error),pointer             :: error
      logical                              :: value

      class (type_scalar),pointer          :: node
      logical                              :: success

      if (present(default)) value = default
      node => self%get_scalar(key,.not.present(default),error)
      if (associated(node)) then
         value = node%to_logical(value,success)
         if (.not.success) then
            allocate(error)
            error%message = trim(node%path)//' is set to "'//trim(node%string) &
                          //'", which cannot be interpreted as a Boolean value.'
         end if
      end if
   end function

   function dictionary_get_integer(self,key,default,error) result(value)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      integer,         optional,intent(in) :: default
      type(type_error),pointer             :: error
      integer                              :: value

      class (type_scalar),pointer          :: node
      logical                              :: success

      if (present(default)) value = default
      node => self%get_scalar(key,.not.present(default),error)
      if (associated(node)) then
         value = node%to_integer(value,success)
         if (.not.success) then
            allocate(error)
            error%message = trim(node%path)//' is set to "'//trim(node%string)//'", which cannot be interpreted as an integer.'
         end if
      end if
   end function

   function dictionary_get_real(self,key,default,error) result(value)
      class (type_dictionary),  intent(in) :: self
      character(len=*),         intent(in) :: key
      real(real_kind), optional,intent(in) :: default
      type(type_error),pointer             :: error
      real(real_kind)                      :: value

      class (type_scalar),pointer          :: node
      logical                              :: success

      if (present(default)) value = default
      node => self%get_scalar(key,.not.present(default),error)
      if (associated(node)) then
         value = node%to_real(value,success)
         if (.not.success) then
            allocate(error)
            error%message = trim(node%path)//' is set to "'//trim(node%string)//'", which cannot be interpreted as a real number.'
         end if
      end if
   end function

   subroutine dictionary_finalize(self)
      class (type_dictionary),intent(inout) :: self

      type (type_key_value_pair),pointer :: pair, next

      pair => self%first
      do while (associated(pair))
         next => pair%next
         call pair%value%finalize()
         deallocate(pair%value)
         deallocate(pair)
         pair => next
      end do
      nullify(self%first)
   end subroutine dictionary_finalize

   subroutine list_append(self,node)
      class (type_list),intent(inout) :: self
      class(type_node),target         :: node

      type (type_list_item),pointer :: item

      if (.not.associated(self%first)) then
         ! This will be the first pair.
         allocate(self%first)
         self%first%node => node
      else
         ! Try to find a pair with the same key, or failing that, the last pair.
         item => self%first
         do while (associated(item%next))
            item => item%next
         end do
         allocate(item%next)
         item%next%node => node
      end if
   end subroutine list_append

   recursive subroutine list_dump(self,unit,indent)
      class (type_list),intent(in) :: self
      integer,          intent(in) :: unit,indent

      type (type_list_item),pointer :: item
      logical :: first

      first = .true.
      item => self%first
      do while (associated(item))
         if (first) then
            first = .false.
         else
            write (unit,'(a)',advance='NO') repeat(' ',indent)
         end if
         write (unit,'(a)',advance='NO') '- '
         call item%node%dump(unit,indent+2)
         item => item%next
      end do
   end subroutine list_dump

   recursive subroutine list_set_path(self,path)
      class (type_list),intent(inout) :: self
      character(len=*), intent(in)    :: path

      type (type_list_item),pointer :: item
      integer :: inode
      character(len=6) :: strindex

      self%path = path
      inode = 0
      item => self%first
      do while (associated(item))
         write (strindex,'(i0)') inode
         call item%node%set_path(trim(self%path)//'['//trim(strindex)//']')
         inode = inode + 1
         item => item%next
      end do
   end subroutine list_set_path

end module yaml_types
