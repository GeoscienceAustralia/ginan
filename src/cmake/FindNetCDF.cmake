# - Find NetCDF
# Find the native NetCDF includes and library
#
#  NETCDF_INCLUDES    - where to find netcdf.h, etc
#  NETCDF_LIBRARIES   - Link these libraries when using NetCDF
#  NETCDF_FOUND       - True if NetCDF found including required interfaces (see below)
#
# Your package can require certain interfaces to be FOUND by setting these
#
#  NETCDF_CXX         - require the C++ interface and link the C++ library
#  NETCDF_F77         - require the F77 interface and link the fortran library
#  NETCDF_F90         - require the F90 interface and link the fortran library
#
# The following are not for general use and are included in
# NETCDF_LIBRARIES if the corresponding option above is set.
#
#  NETCDF_LIBRARIES_C    - Just the C interface
#  NETCDF_LIBRARIES_CXX  - C++ interface, if available
#  NETCDF_LIBRARIES_F77  - Fortran 77 interface, if available
#  NETCDF_LIBRARIES_F90  - Fortran 90 interface, if available
#
# Normal usage would be:
#  set (NETCDF_F90 "YES")
#  find_package (NetCDF REQUIRED)
#  target_link_libraries (uses_f90_interface ${NETCDF_LIBRARIES})
#  target_link_libraries (only_uses_c_interface ${NETCDF_LIBRARIES_C})

if (NETCDF_INCLUDES AND NETCDF_LIBRARIES)
  # Already in cache, be silent
  set (NETCDF_FIND_QUIETLY TRUE)
endif (NETCDF_INCLUDES AND NETCDF_LIBRARIES)

find_path (NETCDF_INCLUDES netcdf.h
        HINTS "${NETCDF_ROOT}/include" "$ENV{NETCDF_ROOT}/include")

string(REGEX REPLACE "/include/?$" "/lib"
        NETCDF_LIB_HINT ${NETCDF_INCLUDES})

find_library (NETCDF_LIBRARIES_C
        NAMES netcdf
        HINTS ${NETCDF_LIB_HINT})
mark_as_advanced(NETCDF_LIBRARIES_C)

if ((NOT NETCDF_LIBRARIES_C) OR (NOT NETCDF_INCLUDES))
  message(STATUS "Trying to find NetCDF using LD_LIBRARY_PATH (we're desperate)...")

  file(TO_CMAKE_PATH "$ENV{LD_LIBRARY_PATH}" LD_LIBRARY_PATH)

  find_library(NETCDF_LIBRARIES_C
          NAMES netcdf
          HINTS ${LD_LIBRARY_PATH})

  if (NETCDF_LIBRARIES_C)
    get_filename_component(NETCDF_LIB_DIR ${NETCDF_LIBRARIES_C} PATH)
    string(REGEX REPLACE "/lib/?$" "/include"
            NETCDF_H_HINT ${NETCDF_LIB_DIR})

    find_path (NETCDF_INCLUDES netcdf.h
            HINTS ${NETCDF_H_HINT}
            DOC "Path to netcdf.h")
  endif()
endif()

set (NetCDF_has_interfaces "YES") # will be set to NO if we're missing any interfaces
set (NetCDF_libs "${NETCDF_LIBRARIES_C}")

get_filename_component (NetCDF_lib_dirs "${NETCDF_LIBRARIES_C}" PATH)

macro (NetCDF_check_interface lang header libs)
  if (NETCDF_${lang})
    find_path (NETCDF_INCLUDES_${lang} NAMES ${header}
            HINTS "${NETCDF_INCLUDES}" NO_DEFAULT_PATH)
    find_library (NETCDF_LIBRARIES_${lang} NAMES ${libs}
            HINTS "${NetCDF_lib_dirs}" NO_DEFAULT_PATH)
    mark_as_advanced (NETCDF_INCLUDES_${lang} NETCDF_LIBRARIES_${lang})
    if (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
      list (INSERT NetCDF_libs 0 ${NETCDF_LIBRARIES_${lang}}) # prepend so that -lnetcdf is last
    else (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
      set (NetCDF_has_interfaces "NO")
      message (STATUS "Failed to find NetCDF interface for ${lang}")
    endif (NETCDF_INCLUDES_${lang} AND NETCDF_LIBRARIES_${lang})
  endif (NETCDF_${lang})
endmacro (NetCDF_check_interface)

if (${APPLE})
    NetCDF_check_interface (CXX netcdf netcdf-cxx4)
else()
    NetCDF_check_interface (CXX netcdf netcdf_c++4)
endif()

NetCDF_check_interface (F77 netcdf.inc  netcdff)
NetCDF_check_interface (F90 netcdf.mod  netcdff)

set (NETCDF_LIBRARIES "${NetCDF_libs}" CACHE STRING "All NetCDF libraries required for interface level")

# handle the QUIETLY and REQUIRED arguments and set NETCDF_FOUND to TRUE if
# all listed variables are TRUE
include (FindPackageHandleStandardArgs)
find_package_handle_standard_args (NetCDF DEFAULT_MSG NETCDF_LIBRARIES NETCDF_INCLUDES NetCDF_has_interfaces)

mark_as_advanced (NETCDF_LIBRARIES NETCDF_INCLUDES)
