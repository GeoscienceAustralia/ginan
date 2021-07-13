module yaml_settings

   use iso_fortran_env, only: error_unit

   use yaml_types, only: yaml_real_kind => real_kind, type_yaml_node => type_node, type_yaml_null => type_null, &
      type_yaml_scalar => type_scalar, type_yaml_dictionary => type_dictionary, type_yaml_list => type_list, &
      type_yaml_list_item => type_list_item, type_yaml_error => type_error, type_yaml_key_value_pair => type_key_value_pair
   use yaml, only: yaml_parse => parse, yaml_error_length => error_length

   implicit none

   private

   public type_settings, type_option, option, report_error, type_settings_create
   public type_dictionary_populator, type_list_populator, type_settings_node, type_key_value_pair, type_list_item
   public type_real_setting, type_logical_setting, type_integer_setting, type_string_setting
   public type_real_setting_create, type_logical_setting_create, type_integer_setting_create, type_string_setting_create

   integer, parameter :: rk = yaml_real_kind

   real(rk), parameter :: default_minimum_real = -huge(1._rk)
   real(rk), parameter :: default_maximum_real = huge(1._rk)
   integer, parameter :: default_minimum_integer = -huge(1)
   integer, parameter :: default_maximum_integer = huge(1)

   integer, parameter :: yaml_indent = 3
   integer, parameter :: xml_indent = 3

   integer, parameter, public :: display_inherit  = -1
   integer, parameter, public :: display_minimum  = 0
   integer, parameter, public :: display_normal   = 1
   integer, parameter, public :: display_advanced = 2
   integer, parameter, public :: display_hidden  = 3

   type type_value
      character(len=:), allocatable   :: long_name
      character(len=:), allocatable   :: description
      class (type_yaml_node), pointer :: backing_store_node => null()
      character(len=:), allocatable   :: path
      class (type_value), pointer     :: parent => null()
      integer                         :: display = display_inherit
   contains
      procedure :: write_schema      => value_write_schema
      procedure :: write_yaml        => value_write_yaml
      procedure :: get_maximum_depth => value_get_maximum_depth
      procedure :: is_visible        => value_is_visible
      procedure :: get_yaml_style    => value_get_yaml_style
      procedure :: create_child
   end type type_value

   type type_settings_node
      class (type_value), pointer :: value => null()
   contains
      procedure :: set_value => node_set_value
   end type

   type, extends(type_settings_node) :: type_key_value_pair
      character(len=:), allocatable       :: key
      character(len=:), allocatable       :: name
      logical                             :: accessed = .false.
      integer                             :: order = 0
      type (type_key_value_pair), pointer :: next => null()
   end type

   type, extends(type_settings_node) :: type_list_item
      type (type_list_item), pointer :: next => null()
   end type

   type, abstract :: type_dictionary_populator
   contains
      procedure (dictionary_populator_create), deferred :: create
   end type

   type, abstract :: type_list_populator
   contains
      procedure :: set_length => list_populator_set_length
      procedure (list_populator_create), deferred :: create
   end type

   interface
      recursive subroutine list_populator_create(self, index, item)
         import type_list_populator, type_list_item
         class (type_list_populator), intent(inout) :: self
         integer,                     intent(in)    :: index
         type (type_list_item),       intent(inout) :: item
      end subroutine

      recursive subroutine dictionary_populator_create(self, pair)
         import type_dictionary_populator, type_key_value_pair
         class (type_dictionary_populator), intent(inout) :: self
         type (type_key_value_pair),        intent(inout) :: pair
      end subroutine
   end interface

   type, extends(type_value) :: type_settings
      class (type_yaml_dictionary),      pointer :: backing_store => null()
      class (type_dictionary_populator), pointer :: populator     => null()
      type (type_key_value_pair),        pointer :: first         => null()
   contains
      procedure :: write_schema => settings_write_schema
      procedure :: write_yaml => settings_write_yaml
      procedure :: get_maximum_depth => settings_get_maximum_depth
      procedure :: is_visible => settings_is_visible
      procedure :: get_yaml_style => settings_get_yaml_style
      procedure :: load
      procedure :: save
      procedure :: write_schema_file
      procedure :: get_real2
      procedure :: get_integer2
      procedure :: get_logical2
      procedure :: get_string2
      procedure :: get_real
      procedure :: get_integer
      procedure :: get_logical
      procedure :: get_string
      procedure :: get_child
      procedure :: get_list
      procedure :: get_node
      procedure :: check_all_used
      generic :: get => get_real2, get_integer2, get_logical2, get_string2
      procedure :: populate => settings_populate
      procedure :: finalize
   end type type_settings

   type, abstract, extends(type_value) :: type_scalar_value
      character(:),allocatable :: units
      logical                  :: has_default = .false.
   contains
      procedure (scalar_value_as_string),  deferred :: as_string
      procedure (scalar_value_at_default), deferred :: at_default
      procedure :: write_yaml        => scalar_value_write_yaml
      procedure :: get_comment       => scalar_value_get_comment
      procedure :: get_maximum_depth => scalar_value_get_maximum_depth
      procedure :: is_visible        => scalar_value_is_visible
   end type type_scalar_value

   abstract interface
      function scalar_value_as_string(self, use_default) result(string)
         import type_scalar_value
         class (type_scalar_value), intent(in) :: self
         logical,                   intent(in) :: use_default
         character(len=:), allocatable         :: string
      end function

      logical function scalar_value_at_default(self)
         import type_scalar_value
         class (type_scalar_value), intent(in) :: self
      end function
   end interface

   type type_option
      integer                   :: value
      character(:), allocatable :: long_name
      character(:), allocatable :: key
   end type

   type, extends(type_value) :: type_list
      type (type_list_item),       pointer :: first => null()
      class (type_list_populator), pointer :: populator => null()
   contains
      procedure :: write_schema => list_write_schema
      procedure :: write_yaml => list_write_yaml
      procedure :: get_maximum_depth => list_get_maximum_depth
      procedure :: get_yaml_style => list_get_yaml_style
   end type

   type, extends(type_scalar_value) :: type_integer_setting
      integer, pointer :: pvalue => null()
      integer :: value
      integer :: default = 0
      integer :: minimum = default_minimum_integer
      integer :: maximum = default_maximum_integer
      type (type_option), allocatable :: options(:)
   contains
      procedure :: as_string    => integer_as_string
      procedure :: write_schema => integer_write_schema
      procedure :: get_comment  => integer_get_comment
      procedure :: at_default   => integer_at_default
   end type

   type, extends(type_scalar_value) :: type_real_setting
      real(rk), pointer :: pvalue => null()
      real(rk) :: value
      real(rk) :: default = 0.0_rk
      real(rk) :: minimum = default_minimum_real
      real(rk) :: maximum = default_maximum_real
   contains
      procedure, nopass :: create => type_real_setting_create
      procedure :: as_string    => real_as_string
      procedure :: write_schema => real_write_schema
      procedure :: get_comment  => real_get_comment
      procedure :: at_default   => real_at_default
   end type

   type, extends(type_scalar_value) :: type_logical_setting
      logical, pointer :: pvalue => null()
      logical :: value
      logical :: default = .true.
   contains
      procedure, nopass :: create => type_logical_setting_create
      procedure :: as_string    => logical_as_string
      procedure :: write_schema => logical_write_schema
      procedure :: at_default   => logical_at_default
   end type

   type, extends(type_scalar_value) :: type_string_setting
      character(:), pointer :: pvalue => null()
      character(:), allocatable :: value
      character(:), allocatable :: default
   contains
      procedure :: as_string    => string_as_string
      procedure :: write_schema => string_write_schema
      procedure :: at_default   => string_at_default
   end type

contains

   type (type_option) function option(value, long_name, key)
      integer,                    intent(in) :: value
      character(len=*),           intent(in) :: long_name
      character(len=*), optional, intent(in) :: key
      option%value = value
      option%long_name = long_name
      if (present(key)) option%key = key
   end function option

   recursive subroutine value_write_schema(self, unit, name, indent)
      class (type_value), intent(in) :: self
      integer,            intent(in) :: unit, indent
      character(len=*),   intent(in) :: name
      stop 'value_write_schema not overridden'
   end subroutine

   recursive subroutine value_write_yaml(self, unit, indent, comment_depth, header, display)
      class (type_value),  intent(in) :: self
      integer,             intent(in) :: unit
      integer,             intent(in) :: indent
      integer,             intent(in) :: comment_depth
      logical,             intent(in) :: header
      integer,             intent(in) :: display
      stop 'value_write_yaml not overridden'
   end subroutine

   recursive function value_get_maximum_depth(self, name, display) result(maxdepth)
      class (type_value), intent(in) :: self
      character(len=*),   intent(in) :: name
      integer,            intent(in) :: display
      integer                        :: maxdepth
      stop 'value_get_maximum_depth not overridden'
   end function

   recursive function value_is_visible(self, display) result(visible)
      class (type_value), intent(in) :: self
      integer,            intent(in) :: display
      logical                        :: visible
      visible = display >= self%display
   end function

   integer function value_get_yaml_style(self, display)
      class (type_value), intent(in) :: self
      integer,            intent(in) :: display
      value_get_yaml_style = -1
   end function

   integer function settings_get_yaml_style(self, display)
      class (type_settings), intent(in) :: self
      integer,               intent(in) :: display

      type (type_key_value_pair), pointer :: pair

      settings_get_yaml_style = yaml_indent
      pair => self%first
      do while (associated(pair))
         if (pair%value%is_visible(display)) return
         pair => pair%next
      end do

      ! No children are visible - this is an empty block indicated by -2
      settings_get_yaml_style = -2
   end function

   integer function list_get_yaml_style(self, display)
      class (type_list), intent(in) :: self
      integer,           intent(in) :: display
      list_get_yaml_style = 0
      if (.not. associated(self%first)) list_get_yaml_style = -2
   end function

   subroutine load(self, path, unit)
      class (type_settings), intent(inout) :: self
      character(len=*),      intent(in)    :: path
      integer,               intent(in)    :: unit

      class (type_yaml_node),pointer   :: root
      character(len=yaml_error_length) :: error

      root => yaml_parse(path, unit, error)
      if (error /= '') call report_error(error)
      if (.not. allocated(self%path)) self%path = ''
      self%backing_store_node => root
      call settings_set_data(self)
   end subroutine load

   logical function check_all_used(self)
      class (type_settings), intent(in) :: self

      integer :: n

      n = 0
      if (associated(self%backing_store)) call node_check(self%backing_store, n)
      check_all_used = n == 0

   contains

      recursive subroutine node_check(self, n)
         class (type_yaml_node), intent(in)    :: self
         integer,                intent(inout) :: n

         type (type_yaml_list_item),      pointer :: item
         type (type_yaml_key_value_pair), pointer :: pair

         select type (self)
         class is (type_yaml_dictionary)
            pair => self%first
            do while (associated(pair))
               if (.not. pair%accessed) then
                  n = n + 1
                  if (n == 1) write (error_unit,*) 'ERROR: the following setting(s) were not recognized:'
                  write (error_unit,*) '- ' // trim(pair%value%path)
               else
                  call node_check(pair%value, n)
               end if
               pair => pair%next
            end do
         class is (type_yaml_list)
            item => self%first
            do while (associated(item))
               call node_check(item%node, n)
               item => item%next
            end do
         end select
      end subroutine

   end function check_all_used

   subroutine save(self, path, unit, display)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: path
      integer,               intent(in) :: unit
      integer, optional,     intent(in) :: display
      integer :: ios
      integer :: display_
      integer :: comment_depth

      open(unit=unit, file=path, action='write', encoding='UTF-8', status='replace', iostat=ios)
      if (ios /= 0) call report_error('Failed to open '//path//' for writing.')
      display_ = display_hidden
      if (present(display)) display_ = display
      comment_depth = self%get_maximum_depth('', display_) + 1
      call self%write_yaml(unit, 0, comment_depth, header=.false., display=display_)
   end subroutine save

   subroutine write_schema_file(self, path, unit, version)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: path
      integer,               intent(in) :: unit
      character(len=*),      intent(in) :: version

      integer :: ios
      type (type_key_value_pair),pointer :: pair

      open(unit=unit, file=path, action='write', status='replace', iostat=ios)
      if (ios /= 0) call report_error('Failed to open '//path//' for writing.')
      write (unit,'(a)') '<?xml version="1.0" ?>'
      write (unit,'(a,a,a)') '<element name="scenario" label="scenario" version="', version, '" namelistextension=".nml"&
         & xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="../../core/scenario-1.0.xsd">'
      pair => self%first
      do while (associated(pair))
         call pair%value%write_schema(unit, pair%name, xml_indent)
         pair => pair%next
      end do
      write (unit,'(a)') '</element>'
   end subroutine write_schema_file

   recursive function get_node(self, name, treat_as_path, istart, order) result(pair)
      class (type_settings), intent(inout), target :: self
      character(len=*),      intent(in)            :: name
      logical, optional,     intent(in)            :: treat_as_path
      integer, optional,     intent(out)           :: istart
      integer, optional,     intent(in)            :: order
      class (type_key_value_pair), pointer         :: pair

      integer                        :: istart_
      class (type_settings), pointer :: settings
      logical                        :: treat_as_path_
      integer                        :: islash
      integer                        :: order_

      if (self%display == display_inherit) self%display = display_normal

      istart_ = 1
      settings => self
      order_ = 0
      if (present(order)) order_ = order

      treat_as_path_ = .true.
      if (present(treat_as_path)) treat_as_path_ = treat_as_path
      if (treat_as_path_) then
         do
            islash = index(name(istart_:), '/')
            if (islash == 0) exit
            settings => get_child(settings, name(istart_:istart_+islash-2), treat_as_path=.false.)
            istart_ = istart_ + islash
         end do
      end if
      if (present(istart)) istart = istart_

      call get(settings, name(istart_:))

   contains

      subroutine get(self, name)
      class (type_settings), intent(inout), target :: self
      character(len=*),      intent(in)            :: name

      character(len=len(name))            :: key
      type (type_key_value_pair), pointer :: previous

      key = string_lower(name)

      ! First try to find an existing pair with this key.
      previous => null()
      pair => self%first
      do while (associated(pair))
         if (pair%key == key) then
            if (pair%accessed) return
            exit
         end if
         if (pair%accessed .and. order_ >= pair%order) previous => pair
         pair => pair%next
      end do

      if (.not. associated(pair)) then
         ! Key not found - create a new key-setting pair and insert after all previously accessed settings (if any)
         allocate(pair)
         pair%key = key
         allocate(type_value::pair%value)
         pair%value%parent => self
         pair%value%path = self%path//'/'//name
         if (associated(self%backing_store)) pair%value%backing_store_node  => self%backing_store%get(name)
      end if

      pair%name = name
      pair%accessed = .true.
      pair%order = order_
      if (associated(previous)) then
         pair%next => previous%next
         previous%next => pair
      else
         pair%next => self%first
         self%first => pair
      end if
      end subroutine
   end function get_node

   function get_real(self, name, long_name, units, default, minimum, maximum, description, display) result(value)
      class (type_settings),           intent(inout) :: self
      real(rk), target                               :: target
      character(len=*),                intent(in)    :: name
      character(len=*),                intent(in)    :: long_name
      character(len=*),                intent(in)    :: units
      real(rk),        optional,       intent(in)    :: default
      real(rk),        optional,       intent(in)    :: minimum
      real(rk),        optional,       intent(in)    :: maximum
      character(len=*),optional,       intent(in)    :: description
      integer,         optional,       intent(in)    :: display
      real(rk) :: value

      class (type_real_setting),   pointer :: setting
      class (type_key_value_pair), pointer :: pair

      pair => self%get_node(name)      
      setting => type_real_setting_create(pair, long_name, units, &
         default, minimum, maximum, description, display=display)
      value = setting%pvalue
   end function get_real

   subroutine get_real2(self, target, name, long_name, units, default, minimum, maximum, description, display)
      class (type_settings),intent(inout) :: self
      real(rk), target                               :: target
      character(len=*),                intent(in)    :: name
      character(len=*),                intent(in)    :: long_name
      character(len=*),                intent(in)    :: units
      real(rk),        optional,       intent(in)    :: default
      real(rk),        optional,       intent(in)    :: minimum
      real(rk),        optional,       intent(in)    :: maximum
      character(len=*),optional,       intent(in)    :: description
      integer,         optional,       intent(in)    :: display

      class (type_real_setting),   pointer :: setting
      class (type_key_value_pair), pointer :: pair

      pair => self%get_node(name)      
      setting => type_real_setting_create(pair, long_name, units, &
                                          default, minimum, maximum, description, target=target, display=display)
   end subroutine

   function type_real_setting_create(node, long_name, units, &
                                     default, minimum, maximum, description, target, display) result(setting)
      class (type_settings_node),      intent(inout) :: node
      character(len=*),                intent(in)    :: long_name
      character(len=*),                intent(in)    :: units
      real(rk),        optional,       intent(in)    :: default
      real(rk),        optional,       intent(in)    :: minimum
      real(rk),        optional,       intent(in)    :: maximum
      character(len=*),optional,       intent(in)    :: description
      real(rk), target, optional                     :: target
      integer,         optional,       intent(in)    :: display
      class (type_real_setting),  pointer :: setting

      select type (value => node%value)
      class is (type_real_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      if (present(target)) then
         setting%pvalue => target
      else
         setting%pvalue => setting%value
      end if
      setting%long_name = long_name
      if (units /= '') setting%units = units
      if (present(minimum)) setting%minimum = minimum
      if (present(maximum)) setting%maximum = maximum
      if (present(display)) setting%display = display
      if (present(default)) then
         if (default < setting%minimum) call report_error('Default value of setting '//setting%path// &
            ' lies below prescribed minimum.')
         if (default > setting%maximum) call report_error('Default value of setting '//setting%path// &
            ' exceeds prescribed maximum.')
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         call real_set_data(setting, setting%backing_store_node)
      elseif (setting%has_default) then
         setting%pvalue = setting%default
      else
         call report_error('No value specified for setting '//setting%path//'; cannot continue because&
            & this parameter does not have a default value either.')
      end if
   end function type_real_setting_create

   subroutine real_set_data(self, backing_store_node)
      class (type_real_setting), intent(inout) :: self
      class (type_yaml_node),    intent(in)    :: backing_store_node

      logical :: success

      select type (backing_store_node)
      class is (type_yaml_scalar)
         self%pvalue = backing_store_node%to_real(self%pvalue, success)
         if (.not. success) call report_error(self%path//' is set to "'//trim(backing_store_node%string)// &
            '", which cannot be interpreted as a real number.')
      class default
         call report_error('Setting '//self%path//' must be a real number.')
      end select
      if (self%pvalue < self%minimum) call report_error('Value specified for parameter '//self%path// &
         ' lies below prescribed minimum.')
      if (self%pvalue > self%maximum) call report_error('Value specified for parameter '//self%path// &
         ' exceeds prescribed maximum.')
   end subroutine

   function get_integer(self, name, long_name, units, default, minimum, maximum, options, description, display) result(value)
      class (type_settings),       intent(inout) :: self
      character(len=*),            intent(in)    :: name
      character(len=*),            intent(in)    :: long_name
      character(len=*),  optional, intent(in)    :: units
      integer,           optional, intent(in)    :: default
      integer,           optional, intent(in)    :: minimum
      integer,           optional, intent(in)    :: maximum
      type (type_option),optional, intent(in)    :: options(:)
      character(len=*),  optional, intent(in)    :: description
      integer,           optional, intent(in)    :: display
      integer :: value

      class (type_integer_setting), pointer :: setting
      class (type_key_value_pair),  pointer :: pair

      pair => self%get_node(name)
      setting => type_integer_setting_create(pair, long_name, units, default, minimum, maximum, &
         options, description, display=display)
      value = setting%pvalue
   end function

   subroutine get_integer2(self, target, name, long_name, units, default, minimum, maximum, options, description, display)
      class (type_settings),       intent(inout) :: self
      integer, target                            :: target
      character(len=*),            intent(in)    :: name
      character(len=*),            intent(in)    :: long_name
      character(len=*),  optional, intent(in)    :: units
      integer,           optional, intent(in)    :: default
      integer,           optional, intent(in)    :: minimum
      integer,           optional, intent(in)    :: maximum
      type (type_option),optional, intent(in)    :: options(:)
      character(len=*),  optional, intent(in)    :: description
      integer,           optional, intent(in)    :: display

      class (type_integer_setting), pointer :: setting
      class (type_key_value_pair),  pointer :: pair

      pair => self%get_node(name)
      setting => type_integer_setting_create(pair, long_name, units, default, minimum, maximum, &
         options, description, target=target, display=display)
   end subroutine

   function type_integer_setting_create(node, long_name, units, default, minimum, maximum, options, &
      description, target, display) result(setting)
      class (type_settings_node),  intent(inout) :: node
      character(len=*),            intent(in)    :: long_name
      character(len=*),  optional, intent(in)    :: units
      integer,           optional, intent(in)    :: default
      integer,           optional, intent(in)    :: minimum
      integer,           optional, intent(in)    :: maximum
      type (type_option),optional, intent(in)    :: options(:)
      character(len=*),  optional, intent(in)    :: description
      integer, target,   optional                :: target
      integer,           optional, intent(in)    :: display
      class (type_integer_setting), pointer :: setting

      integer                               :: ioption, ioption2, ivalue
      logical                               :: found

      select type (value => node%value)
      class is (type_integer_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select

      if (present(target)) then
         setting%pvalue => target
      else
         setting%pvalue => setting%value
      end if
      setting%long_name = long_name
      if (present(units)) setting%units = units
      if (present(minimum)) setting%minimum = minimum
      if (present(maximum)) setting%maximum = maximum
      if (present(display)) setting%display = display
      if (present(options)) then
         do ioption = 1, size(options)
            do ioption2 = ioption + 1, size(options)
               if (options(ioption)%value == options(ioption2)%value) call report_error( &
                  'Setting '//setting%path//' has multiple options with the same integer value.')
            end do
         end do
         if (allocated(setting%options)) deallocate(setting%options)
         allocate(setting%options(size(options)))

         ! Order options according to value
         ioption = 1
         do ivalue = minval(options(:)%value), maxval(options(:)%value)
            do ioption2 = 1, size(options)
               if (options(ioption2)%value == ivalue) then
                  setting%options(ioption) = options(ioption2)
                  if (allocated(setting%options(ioption)%key)) &
                     setting%options(ioption)%key = string_lower(setting%options(ioption)%key)
                  ioption = ioption + 1
                  exit
               end if
            end do
         end do
      end if
      if (present(default)) then
         if (default < setting%minimum) call report_error('Default value of setting '//setting%path// &
            ' lies below prescribed minimum.')
         if (default > setting%maximum) call report_error('Default value of setting '//setting%path// &
            ' exceeds prescribed maximum.')
         if (allocated(setting%options)) then
            found = .false.
            do ioption = 1, size(setting%options)
               if (default == setting%options(ioption)%value) found = .true.
            end do
            if (.not.found) call report_error('Default value of setting '//setting%path// &
               ' does not correspond to any known option.')
         end if
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         call integer_set_data(setting, setting%backing_store_node)
      elseif (setting%has_default) then
         setting%pvalue = setting%default
      else
         call report_error('No value specified for setting '//setting%path//'; cannot continue because&
            & it does not have a default value either.')
      end if
   end function type_integer_setting_create

   subroutine integer_set_data(self, backing_store_node)
      class (type_integer_setting), intent(inout) :: self
      class (type_yaml_node),       intent(in)    :: backing_store_node

      logical :: success
      integer :: ioption
      character(len=:), allocatable :: strvalue

      select type (backing_store_node)
      class is (type_yaml_scalar)
         self%pvalue = backing_store_node%to_integer(self%pvalue, success)
         if (.not. success .and. allocated(self%options)) then
            strvalue = string_lower(trim(backing_store_node%string))
            do ioption = 1, size(self%options)
               if (strvalue == string_lower(self%options(ioption)%long_name)) then
                  ! Value matches long name of option
                  success = .true.
               elseif (allocated(self%options(ioption)%key)) then
                  ! Option has a key; check if value matches that
                  if (strvalue == self%options(ioption)%key) success = .true.
               end if
               if (success) then
                  self%pvalue = self%options(ioption)%value
                  exit
               end if
            end do
         end if
         if (.not. success) call report_error(self%path//' is set to "'//trim(backing_store_node%string)// &
            '", which cannot be interpreted as an integer number.')
      class default
         call report_error('Setting '//self%path//' must be an integer number.')
      end select
      if (self%pvalue < self%minimum) call report_error('Value specified for setting '//self%path// &
         ' lies below prescribed minimum.')
      if (self%pvalue > self%maximum) call report_error('Value specified for setting '//self%path// &
         ' exceeds prescribed maximum.')
      if (allocated(self%options)) then
         success = .false.
         do ioption = 1, size(self%options)
            if (self%pvalue == self%options(ioption)%value) success = .true.
         end do
         if (.not. success) call report_error('Value specified for setting '//self%path// &
            ' does not correspond to any known option.')
      end if
   end subroutine integer_set_data

   function get_logical(self, name, long_name, default, description, display) result(value)
      class (type_settings),      intent(inout) :: self
      character(len=*),           intent(in)    :: name
      character(len=*),           intent(in)    :: long_name
      logical,          optional, intent(in)    :: default
      character(len=*), optional, intent(in)    :: description
      integer,          optional, intent(in)    :: display
      logical :: value

      class (type_logical_setting), pointer :: setting
      class (type_key_value_pair),  pointer :: pair

      pair => self%get_node(name)
      setting => type_logical_setting_create(pair, long_name, default, description, display=display)
      value = setting%pvalue
   end function get_logical

   subroutine get_logical2(self, target, name, long_name, default, description, display)
      class (type_settings),      intent(inout) :: self
      logical, target                           :: target
      character(len=*),           intent(in)    :: name
      character(len=*),           intent(in)    :: long_name
      logical,          optional, intent(in)    :: default
      character(len=*), optional, intent(in)    :: description
      integer,          optional, intent(in)    :: display

      class (type_logical_setting), pointer :: setting
      class (type_key_value_pair),  pointer :: pair

      pair => self%get_node(name)
      setting => type_logical_setting_create(pair, long_name, default, description, &
         target=target, display=display)
   end subroutine get_logical2

   function type_logical_setting_create(node, long_name, default, description, target, display) result(setting)
      class (type_settings_node), intent(inout) :: node
      character(len=*),           intent(in)    :: long_name
      logical, optional,          intent(in)    :: default
      character(len=*), optional, intent(in)    :: description
      logical, target,  optional                :: target
      integer,          optional, intent(in)    :: display
      class (type_logical_setting), pointer     :: setting

      select type (value => node%value)
      class is (type_logical_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      if (present(target)) then
         setting%pvalue => target
      else
         setting%pvalue => setting%value
      end if
      setting%long_name = long_name
      if (present(display)) setting%display = display
      if (present(default)) then
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         call logical_set_data(setting, setting%backing_store_node)
      elseif (setting%has_default) then
         setting%pvalue = setting%default
      else
         call report_error('No value specified for parameter '//setting%path//'; cannot continue because&
            & this parameter does not have a default value either.')
      end if
   end function type_logical_setting_create
   
   subroutine logical_set_data(self, backing_store_node)
      class (type_logical_setting), intent(inout) :: self
      class (type_yaml_node),       intent(in)    :: backing_store_node

      logical :: success

      select type (backing_store_node)
      class is (type_yaml_scalar)
         self%pvalue = backing_store_node%to_logical(self%pvalue, success)
         if (.not. success) call report_error(self%path//' is set to "'//trim(backing_store_node%string)// &
            '", which cannot be interpreted as logical value (true or false).')
      class default
         call report_error('Setting '//self%path//' must be set to a logical value (true or false).')
      end select
   end subroutine

   function get_string(self, name, long_name, units, default, description, display) result(value)
      class (type_settings),      intent(inout) :: self
      character(len=*),           intent(in)    :: name
      character(len=*),           intent(in)    :: long_name
      character(len=*), optional, intent(in)    :: units
      character(len=*), optional, intent(in)    :: default
      character(len=*), optional, intent(in)    :: description
      integer,          optional, intent(in)    :: display
      character(len=:), allocatable :: value

      class (type_string_setting), pointer :: setting
      class (type_key_value_pair), pointer :: pair

      pair => self%get_node(name)
      setting => type_string_setting_create(pair, long_name, units, default, description, display=display)
      value = setting%pvalue
   end function

   subroutine get_string2(self, target, name, long_name, units, default, description, display)
      class (type_settings),      intent(inout) :: self
      character(len=*), target                  :: target
      character(len=*),           intent(in)    :: name
      character(len=*),           intent(in)    :: long_name
      character(len=*), optional, intent(in)    :: units
      character(len=*), optional, intent(in)    :: default
      character(len=*), optional, intent(in)    :: description
      integer,          optional, intent(in)    :: display

      class (type_string_setting), pointer :: setting
      class (type_key_value_pair), pointer :: pair

      pair => self%get_node(name)
      setting => type_string_setting_create(pair, long_name, units, default, description, &
         target=target, display=display)
   end subroutine get_string2

   function type_string_setting_create(node, long_name, units, default, description, target, display) result(setting)
      class (type_settings_node),      intent(inout) :: node
      character(len=*),                intent(in)    :: long_name
      character(len=*), optional,      intent(in)    :: units
      character(len=*), optional,      intent(in)    :: default
      character(len=*), optional,      intent(in)    :: description
      character(len=*), optional, target             :: target
      integer,          optional,      intent(in)    :: display
      class (type_string_setting), pointer           :: setting

      select type (value => node%value)
      class is (type_string_setting)
         setting => value
      class default
         allocate(setting)
         call node%set_value(setting)
      end select
      if (present(target)) then
         setting%pvalue => target
      else
         setting%pvalue => null()
      end if
      setting%long_name = long_name
      if (present(units)) setting%units = units
      if (present(display)) setting%display = display
      if (present(default)) then
         setting%has_default = .true.
         setting%default = default
      end if
      if (present(description)) setting%description = description
      if (associated(setting%backing_store_node)) then
         select type (yaml_node => setting%backing_store_node)
         class is (type_yaml_null)
            setting%value = ''
         class is (type_yaml_scalar)
            setting%value = trim(yaml_node%string)
         class default
            call report_error(setting%path//' must be be a string or null.')
         end select
      else
         if (setting%has_default) then
            setting%value = setting%default
         else
            call report_error('No value specified for parameter '//setting%path//'; cannot continue because&
               & this parameter does not have a default value either.')
         end if
      end if
      if (associated(setting%pvalue) .and. .not. associated(setting%pvalue, setting%value)) then
         setting%pvalue = setting%value
         deallocate(setting%value)
      else
         setting%pvalue => setting%value
      end if
   end function type_string_setting_create

   recursive function create_child(self) result(child)
      class (type_value), intent(in) :: self
      class (type_settings), pointer   :: child
      if (associated(self%parent)) then
         child => self%parent%create_child()
      else
         allocate(child)
      end if
   end function create_child

   recursive function get_child(self, name, long_name, treat_as_path, populator, display, order) result(child)
      class (type_settings), target, intent(inout) :: self
      character(len=*),              intent(in)    :: name
      character(len=*),optional,     intent(in)    :: long_name
      logical, optional,             intent(in)    :: treat_as_path
      class (type_dictionary_populator), optional, target :: populator
      integer, optional,             intent(in)    :: display
      integer, optional,             intent(in)    :: order
      class (type_settings),  pointer :: child

      class (type_settings_node), pointer :: node

      node => self%get_node(name, treat_as_path=treat_as_path, order=order)
      child => type_settings_create(node, long_name, populator, display)
   end function get_child

   subroutine node_set_value(self, value)
      class (type_settings_node), intent(inout) :: self
      class (type_value), target :: value

      value%parent => self%value%parent
      if (value%display == display_inherit) value%display = value%parent%display
      call move_alloc(self%value%path, value%path)
      value%backing_store_node => self%value%backing_store_node
      deallocate(self%value)
      self%value => value
   end subroutine

   function type_settings_create(node, long_name, populator, display) result(child)
      class (type_settings_node),        optional, intent(inout) :: node
      character(len=*),                  optional, intent(in)    :: long_name
      class (type_dictionary_populator), optional, target        :: populator
      integer, optional,                           intent(in)    :: display
      class (type_settings),  pointer :: child

      logical :: create

      child => null()
      if (present(node)) then
         select type (value => node%value)
         class is (type_settings)
            child => value
         end select
      end if
      create = .not. associated(child)
      if (create) then
         child => node%value%parent%create_child()
         call node%set_value(child)
      end if

      if (present(long_name)) child%long_name = long_name
      if (present(populator)) child%populator => populator
      if (present(display)) child%display = display
      if ((create .or. present(populator)) .and. associated(child%backing_store_node)) &
         call settings_set_data(child)
   end function

   subroutine settings_populate(self, populator)
      class (type_settings), target, intent(inout) :: self
      class (type_dictionary_populator), target :: populator

      self%populator => populator
      if (associated(self%backing_store_node)) call settings_set_data(self)
   end subroutine

   recursive subroutine settings_set_data(self)
      class (type_settings), target, intent(inout) :: self

      type (type_yaml_key_value_pair), pointer :: yaml_pair
      class (type_key_value_pair),     pointer :: pair

      self%backing_store => null()
      if (.not. associated(self%backing_store_node)) return
      select type (backing_store_node => self%backing_store_node)
      class is (type_yaml_dictionary)
         self%backing_store => backing_store_node
         yaml_pair => self%backing_store%first
         do while (associated(yaml_pair))
            if (associated(self%populator)) then
               pair => self%get_node(trim(yaml_pair%key), treat_as_path=.false.)
               call self%populator%create(pair)
            end if
            yaml_pair => yaml_pair%next
         end do
      class is (type_yaml_null)
      class default
         call report_error(self%path//' should be a dictionary')
      end select 
   end subroutine

   recursive subroutine get_list(self, name, populator, long_name, treat_as_path, display)
      class (type_settings), target, intent(inout) :: self
      character(len=*),              intent(in)    :: name
      character(len=*), optional,    intent(in)    :: long_name
      class (type_list_populator), target          :: populator
      logical, optional,             intent(in)    :: treat_as_path
      integer, optional,             intent(in)    :: display

      class (type_settings_node), pointer :: node
      class (type_list),          pointer :: list

      node => self%get_node(name, treat_as_path=treat_as_path)

      select type (value => node%value)
      class is (type_list)
         list => value
      class default
         allocate(list)
         call node%set_value(list)
      end select

      list%populator => populator
      if (present(display)) list%display = display
      if (present(long_name)) list%long_name = long_name
      if (associated(list%backing_store_node)) call list_set_data(list, list%backing_store_node)
   end subroutine get_list

   subroutine list_set_data(self, backing_store_node)
      class (type_list), target, intent(inout) :: self
      class (type_yaml_node), target :: backing_store_node

      type (type_yaml_list_item), pointer :: yaml_item
      type (type_list_item),      pointer :: item, last_item
      integer                             :: i
      character(len=8)                    :: strindex

      select type (backing_store_node)
      class is (type_yaml_list)
         yaml_item => backing_store_node%first
         i = 0
         do while (associated(yaml_item))
            i = i + 1
            yaml_item => yaml_item%next
         end do
         call self%populator%set_length(i)

         last_item => self%first
         yaml_item => backing_store_node%first
         i = 0
         do while (associated(yaml_item))
            i = i + 1
            write (strindex,'(i0)') i
            allocate(item)
            allocate(type_value::item%value)
            item%value%path = self%path//'['//strindex//']'
            item%value%backing_store_node => yaml_item%node
            item%value%parent => self
            if (.not. associated(last_item)) then
               self%first => item
            else
               last_item%next => item
            end if
            last_item => item
            call self%populator%create(i, item)
            yaml_item => yaml_item%next
         end do
      class is (type_yaml_null)
      class default
         call report_error(self%path//' should be a list')
      end select 
   end subroutine

   subroutine finalize(self)
      class (type_settings),intent(inout) :: self

      type (type_key_value_pair),pointer :: current, next

      current => self%first
      do while (associated(current))
         next => current%next
         select type (value => current%value)
         class is (type_settings)
            call value%finalize()
         end select
         deallocate(current%value)
         deallocate(current)
         current => next
      end do
      self%first => null()
   end subroutine finalize

   function string_lower(string) result (lowerstring)
       character(len=*),intent(in) :: string
       character(len=len(string))  :: lowerstring

       integer                     :: i,k

       lowerstring = string
       do i = 1,len(string)
           k = iachar(string(i:i))
           if (k>=iachar('A').and.k<=iachar('Z')) then
               k = k + iachar('a') - iachar('A')
               lowerstring(i:i) = achar(k)
           end if
       end do
   end function string_lower

   subroutine report_error(message)
      character(len=*), intent(in) :: message
      write (error_unit,*) trim(message)
      stop 1
   end subroutine report_error

   recursive subroutine settings_write_yaml(self, unit, indent, comment_depth, header, display)
      class (type_settings), intent(in) :: self
      integer,               intent(in) :: unit
      integer,               intent(in) :: indent
      integer,               intent(in) :: comment_depth
      logical,               intent(in) :: header
      integer,               intent(in) :: display

      logical :: first
      type (type_key_value_pair), pointer  :: pair
      integer :: block_indent

      !if (header) then
      !   write (unit, '()')
      !   write (unit, '("# ",a,a)') repeat(' ', indent), repeat('-', 80)
      !   call write_header(self, name, indent)
      !   write (unit, '("# ",a,a)') repeat(' ', indent), repeat('-', 80)
      !end if

      first = .true.
      pair => self%first
      do while (associated(pair))
         if (pair%value%is_visible(display)) then
            if (.not. first) write (unit, '(a)', advance='no') repeat(' ', indent)
            write (unit, '(a,":")', advance='no') pair%name
            block_indent = pair%value%get_yaml_style(display)
            if (block_indent == -1) then
               ! flow
               write (unit, '(" ")', advance='no')
               call pair%value%write_yaml(unit, indent + len(pair%name) + 2, comment_depth - len(pair%name) - 2, &
                  header=.false., display=display)
            else
               ! block or null
               if (allocated(pair%value%long_name)) write (unit, '(a,"# ",a)', advance='no') &
                  repeat(' ', comment_depth - len(pair%name) - 1), pair%value%long_name
               write (unit, *)
               if (block_indent >= 0) then
                  ! block
                  write (unit, '(a)', advance='no') repeat(' ', indent + block_indent)
                  call pair%value%write_yaml(unit, indent + block_indent, comment_depth - block_indent, &
                     header=.false., display=display)
               end if
            end if
            first = .false.
         end if
         pair => pair%next
      end do

   contains

      recursive subroutine write_header(self, name, indent)
         class (type_value), intent(in) :: self
         character(len=*),           intent(in) :: name
         integer,                    intent(in) :: indent

         type (type_key_value_pair), pointer  :: pair
         integer :: ioption
         logical :: written

         write (unit, '("# ",a,a,": ")', advance='no') repeat(' ', indent), name
         if (allocated(self%long_name)) write (unit, '(a)', advance='no') self%long_name
         write (unit,*)

         select type (self)
         class is (type_settings)
            pair => self%first
            do while (associated(pair))
               call write_header(pair%value, pair%name, indent + yaml_indent)
               pair => pair%next
            end do
         class is (type_scalar_value)
            if (allocated(self%description)) write (unit,'("# ",a,a)') repeat(' ', indent + yaml_indent), self%description
            select type (scalar => self)
            class is (type_real_setting)
               !write (unit,'(" (",a,")")', advance='no') node%units
               written = .false.
               if (scalar%minimum /= default_minimum_real) then
                  write (unit,'("# ",a,a,a)', advance='no') repeat(' ', indent + yaml_indent), 'minimum: ', &
                     format_real(scalar%minimum)
                  written = .true.
               end if
               if (scalar%maximum /= default_maximum_real) then
                  if (written) then
                     write (unit,'(", ")', advance='no')
                  else
                     write (unit,'("# ",a)', advance='no') repeat(' ', indent + yaml_indent)
                  end if
                  write (unit,'(a,a)', advance='no') 'maximum: ', format_real(scalar%maximum)
                  written = .true.
               end if
               if (scalar%has_default) then
                  if (written) then
                     write (unit,'(", ")', advance='no')
                  else
                     write (unit,'("# ",a)', advance='no') repeat(' ', indent + yaml_indent)
                  end if
                  write (unit,'(a,a)', advance='no') 'default: ', format_real(scalar%default)
                  written = .true.
               end if
               if (written) write (unit,*)
            class is (type_integer_setting)
               !if (allocated(node%units)) write (unit,'(" (",a,")")', advance='no') node%units
               if (allocated(scalar%options)) then
                  do ioption=1,size(scalar%options)
                     !if (ioption > 1) write (unit,'(", ")', advance='no')
                     write (unit,'("# ",a,i0,": ",a)') repeat(' ', indent + yaml_indent), scalar%options(ioption)%value, &
                        scalar%options(ioption)%long_name
                  end do
               end if
            end select
         end select
      end subroutine write_header

   end subroutine


   recursive subroutine list_write_yaml(self, unit, indent, comment_depth, header, display)
      class (type_list), intent(in) :: self
      integer,           intent(in) :: unit
      integer,           intent(in) :: indent
      integer,           intent(in) :: comment_depth
      logical,           intent(in) :: header
      integer,           intent(in) :: display

      type (type_list_item), pointer :: item

      item => self%first
      do while (associated(item))
         if (.not. associated(item, self%first)) write (unit, '(a)', advance='no') repeat(' ', indent)
         write (unit, '("- ")', advance='no')
         call item%value%write_yaml(unit, indent + 2, comment_depth - 2, header=.false., display=display)
         item => item%next
      end do
   end subroutine

   recursive subroutine scalar_value_write_yaml(self, unit, indent, comment_depth, header, display)
      class (type_scalar_value), intent(in) :: self
      integer,                   intent(in) :: unit
      integer,                   intent(in) :: indent
      integer,                   intent(in) :: comment_depth
      logical,                   intent(in) :: header
      integer,                   intent(in) :: display

      character(len=:), allocatable :: string, comment

      string = self%as_string(.false.)
      write (unit, '(a,a,"# ",a)', advance='no') string, repeat(' ', comment_depth - len(string)), self%long_name
      if (allocated(self%units)) then
         if (self%units == '-') then
            call append_string(comment, '; ', 'dimensionless')
         elseif (self%units == '1') then
            call append_string(comment, '; ', 'fraction')
         else
            call append_string(comment, '; ', self%units)
         end if
      end if
      call self%get_comment(comment)
      if (self%has_default) call append_string(comment, '; ', 'default=' // self%as_string(.true.))
      if (allocated(comment)) write (unit,'(" [",a,"]")', advance='no') comment
      write (unit,*)
   end subroutine scalar_value_write_yaml

   recursive subroutine scalar_value_get_comment(self, comment)
      class (type_scalar_value),     intent(in)    :: self
      character(len=:), allocatable, intent(inout) :: comment
   end subroutine
   
   recursive function scalar_value_get_maximum_depth(self, name, display) result(maxdepth)
      class (type_scalar_value), intent(in) :: self
      character(len=*),          intent(in) :: name
      integer,                   intent(in) :: display
      integer                               :: maxdepth

      maxdepth = len(name) + 2 + len(self%as_string(.false.))
   end function

   recursive function scalar_value_is_visible(self, display) result(visible)
      class (type_scalar_value), intent(in) :: self
      integer,                   intent(in) :: display
      logical                               :: visible
      visible = .true.
      if (display >= self%display) return
      if (.not. self%has_default) return
      if (.not. self%at_default()) return
      visible = .false.
   end function

   recursive function settings_get_maximum_depth(self, name, display) result(maxdepth)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: name
      integer,               intent(in) :: display
      integer                           :: maxdepth

      type (type_key_value_pair), pointer :: pair

      maxdepth = len(name) + 1
      pair => self%first
      do while (associated(pair))
         if (pair%value%is_visible(display)) &
            maxdepth = max(maxdepth, pair%value%get_maximum_depth(pair%name, display=display) + yaml_indent)
         pair => pair%next
      end do
   end function settings_get_maximum_depth

   recursive function settings_is_visible(self, display) result(visible)
      class (type_settings), intent(in) :: self
      integer,               intent(in) :: display
      logical                           :: visible

      type (type_key_value_pair), pointer :: pair

      visible = .true.
      if (display >= self%display) return
      pair => self%first
      do while (associated(pair))
         if (pair%value%is_visible(display)) return
         pair => pair%next
      end do
      visible = .false.
   end function

   recursive function list_get_maximum_depth(self, name, display) result(maxdepth)
      class (type_list), intent(in) :: self
      character(len=*),  intent(in) :: name
      integer,           intent(in) :: display
      integer                       :: maxdepth

      type (type_list_item), pointer :: item

      maxdepth = len(name) + 1
      item => self%first
      do while (associated(item))
         maxdepth = max(maxdepth, item%value%get_maximum_depth('', display=display) + 2)
         item => item%next
      end do
   end function list_get_maximum_depth

   function real_as_string(self, use_default) result(string)
      class (type_real_setting), intent(in) :: self
      logical,                   intent(in) :: use_default
      character(len=:), allocatable :: string
      if (use_default) then
         string = format_real(self%default)
      else
         string = format_real(self%pvalue)
      end if
   end function real_as_string

   function format_real(value) result(string)
      real(rk), intent(in) :: value
      character(:), allocatable :: string

      integer :: idecimals
      real(rk) :: test
      character(len=15) :: tmp

      idecimals = -1
      if (value < 1.e7_rk) then
         do idecimals = 0, 3
            test = value * 10._rk**idecimals
            if (test == int(test)) exit
         end do
      end if
      select case (idecimals)
      case (0,1)
         write (tmp, '(f15.1)') value
      case (2)
         write (tmp, '(f15.2)') value
      case (3)
         write (tmp, '(f15.3)') value
      case default
         write (tmp, '(es15.8)') value
      end select
      string = trim(adjustl(tmp))
   end function

   recursive subroutine real_get_comment(self, comment)
      class (type_real_setting), intent(in) :: self
      character(len=:),allocatable, intent(inout) :: comment

      if (self%minimum /= default_minimum_real) call append_string(comment, '; ', 'min=' // format_real(self%minimum))
      if (self%maximum /= default_maximum_real) call append_string(comment, '; ', 'max=' // format_real(self%maximum))
   end subroutine

   subroutine append_string(target, infix, string)
      character(len=:),allocatable, intent(inout) :: target
      character(len=*), intent(in) :: infix, string

      if (allocated(target)) then
         target = target // infix // string
      else
         target = string
      end if
   end subroutine

   recursive subroutine integer_get_comment(self, comment)
      class (type_integer_setting), intent(in) :: self
      character(len=:),allocatable, intent(inout) :: comment

      integer :: ioption

      if (allocated(self%options)) then
         do ioption = 1, size(self%options)
            if (allocated(self%options(ioption)%key)) then
               if (self%options(ioption)%key == string_lower(self%options(ioption)%long_name)) then
                  call append_string(comment, ', ', self%options(ioption)%key)
               else
                  call append_string(comment, ', ', self%options(ioption)%key // '=' // self%options(ioption)%long_name)
               end if
            else
               call append_string(comment, ', ', format_integer(self%options(ioption)%value) // '=' // &
                  self%options(ioption)%long_name)
            end if
         end do
      else
         if (self%minimum /= default_minimum_integer) call append_string(comment, '; ', 'min=' // format_integer(self%minimum))
         if (self%maximum /= default_maximum_integer) call append_string(comment, '; ', 'max=' // format_integer(self%maximum))
      end if
   end subroutine

   function format_integer(value) result(string)
      integer, intent(in) :: value
      character(len=:), allocatable :: string

      character(len=8) :: tmp

      write (tmp, '(i0)') value
      string = trim(tmp)
   end function

   function integer_as_string(self, use_default) result(string)
      class (type_integer_setting), intent(in) :: self
      logical, intent(in)                      :: use_default
      character(len=:), allocatable :: string

      integer :: value
      integer :: ioption

      value = self%pvalue
      if (use_default) value = self%default
      if (allocated(self%options)) then
         do ioption = 1, size(self%options)
            if (self%options(ioption)%value == value .and. allocated(self%options(ioption)%key)) then
               string = self%options(ioption)%key
               return
            end if
         end do
      end if
      string = format_integer(value)
   end function integer_as_string

   function logical_as_string(self, use_default) result(string)
      class (type_logical_setting), intent(in) :: self
      logical,                      intent(in) :: use_default
      character(len=:), allocatable :: string

      logical :: value

      value = self%pvalue
      if (use_default) value = self%default
      if (value) then
         string = 'true'
      else
         string = 'false'
      end if
   end function logical_as_string

   function string_as_string(self, use_default) result(string)
      class (type_string_setting), intent(in) :: self
      logical,                     intent(in) :: use_default
      character(len=:), allocatable :: string

      if (use_default) then
         string = self%default
      else
         string = trim(self%pvalue)
      end if
   end function string_as_string

   logical function integer_at_default(self)
      class (type_integer_setting), intent(in) :: self
      integer_at_default = self%pvalue == self%default
   end function

   logical function real_at_default(self)
      class (type_real_setting), intent(in) :: self
      real_at_default = self%pvalue == self%default
   end function

   logical function logical_at_default(self)
      class (type_logical_setting), intent(in) :: self
      logical_at_default = self%pvalue .eqv. self%default
   end function

   logical function string_at_default(self)
      class (type_string_setting), intent(in) :: self
      string_at_default = self%pvalue == self%default
   end function

   recursive subroutine settings_write_schema(self, unit, name, indent)
      class (type_settings), intent(in) :: self
      character(len=*),      intent(in) :: name
      integer,               intent(in) :: unit, indent

      type (type_key_value_pair), pointer  :: pair

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      write (unit, '(a)') '>'
      pair => self%first
      do while (associated(pair))
         call pair%value%write_schema(unit, pair%name, indent + xml_indent)
         pair => pair%next
      end do
      write (unit, '(a,a)') repeat(' ', indent), '</element>'
   end subroutine settings_write_schema

   recursive subroutine list_write_schema(self, unit, name, indent)
      class (type_list), intent(in) :: self
      character(len=*),  intent(in) :: name
      integer,           intent(in) :: unit, indent
   end subroutine list_write_schema

   recursive subroutine integer_write_schema(self, unit, name, indent)
      class (type_integer_setting), intent(in) :: self
      character(len=*),             intent(in) :: name
      integer,                      intent(in) :: unit, indent

      integer :: ioption

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="integer"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      if (self%minimum /= default_minimum_integer) write (unit, '(a,i0,a)', advance='no') ' minInclusive="', self%minimum, '"'
      if (self%maximum /= default_maximum_integer) write (unit, '(a,i0,a)', advance='no') ' maxInclusive="', self%maximum, '"'
      if (allocated(self%options)) then
         write (unit, '(a)') '>'
         write (unit, '(a,a)') repeat(' ', indent + xml_indent), '<options>'
         do ioption=1, size(self%options)
            write (unit,'(a,a,i0,a,a,a)') repeat(' ', indent + 2 * xml_indent), '<option value="', self%options(ioption)%value, &
               '" label="', self%options(ioption)%long_name, '"/>'
         end do
         write (unit, '(a,a)') repeat(' ', indent + xml_indent), '</options>'
         write (unit, '(a,a)') repeat(' ', indent), '</element>'
      else
         write (unit, '("/>")')
      end if
   end subroutine

   recursive subroutine real_write_schema(self, unit, name, indent)
      class (type_real_setting), intent(in) :: self
      character(len=*),          intent(in) :: name
      integer,                   intent(in) :: unit, indent

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="float"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      if (self%minimum /= default_minimum_real) &
         write (unit, '(a,a,a)', advance='no') ' minInclusive="', format_real(self%minimum), '"'
      if (self%maximum /= default_maximum_real) &
         write (unit, '(a,a,a)', advance='no') ' maxInclusive="', format_real(self%maximum), '"'
      write (unit, '("/>")')
   end subroutine

   recursive subroutine logical_write_schema(self, unit, name, indent)
      class (type_logical_setting), intent(in) :: self
      character(len=*),             intent(in) :: name
      integer,                      intent(in) :: unit, indent

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="bool"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      write (unit, '("/>")')
   end subroutine

   recursive subroutine string_write_schema(self, unit, name, indent)
      class (type_string_setting), intent(in) :: self
      character(len=*),            intent(in) :: name
      integer,                     intent(in) :: unit, indent

      write (unit, '(a,a,a,a)', advance='no') repeat(' ', indent), '<element name="', name, '" type="string"'
      if (allocated(self%long_name)) write (unit, '(a,a,a)', advance='no') ' label="', self%long_name, '"'
      write (unit, '("/>")')
   end subroutine

   subroutine list_populator_set_length(self, n)
      class (type_list_populator), intent(inout) :: self
      integer,                     intent(in)    :: n
   end subroutine

end module yaml_settings
