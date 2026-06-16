cmake_minimum_required(VERSION 3.22)
cmake_policy(SET CMP0063 NEW)
project(ginan)

# Set C++ standard before any other configuration
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

include(CheckPIESupported)
check_pie_supported()

# Use absolute paths for output directories to support nested build directories
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/../lib")
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/../lib")
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/../bin")

set(CMAKE_MODULE_PATH					"${CMAKE_SOURCE_DIR}/cmake")
set(CMAKE_CXX_VISIBILITY_PRESET     	hidden)
set(CMAKE_VERBOSE_MAKEFILE				OFF)

# Platform-specific compiler flags
if(WIN32)
	# Windows-specific flags
	message(STATUS "Using Windows compiler flags")
	# Note: WIN32_WINNT and LEAN_AND_MEAN are set in toolchain file
	# Disable some warnings on Windows
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unknown-pragmas")
elseif (CMAKE_CXX_COMPILER_ID STREQUAL "AppleClang")
	message(STATUS "Using Clang compliant compiler flags")

	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-shift-overflow")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-string-concatenation")
else ()
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-format-overflow")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-stringop-truncation")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-format-truncation")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-restrict")
endif ()


#	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -ggdb3")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -fpermissive")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -fno-omit-frame-pointer")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wall")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-write-strings")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-narrowing")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-sign-compare")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unused-variable")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-switch")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-dangling-else")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-misleading-indentation")
#	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -fno-var-tracking-assignments")
#	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unknown-warning-option")
	if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
		set(CMAKE_CXX_FLAGS					"${CMAKE_CXX_FLAGS} -Wno-extern-c-compat")
	endif()
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-format-zero-length")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-array-bounds")

#	set(CMAKE_CXX_FLAGS 					"${CMAKE_CXX_FLAGS} -save-temps")
#	set(CMAKE_CXX_FLAGS 					"${CMAKE_CXX_FLAGS} -M")

if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()
if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	set(CMAKE_CXX_FLAGS					"${CMAKE_CXX_FLAGS} -O0 -g")
	set(CMAKE_C_FLAGS					"${CMAKE_C_FLAGS} -O0 -g")
endif ()

option(BUILD_DOC                "BUILD_DOCUMENTATION"       OFF)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	option(ENABLE_PARALLELISATION   "ENABLE_PARALLELISATION"    OFF)
	option(ENABLE_OPTIMISATION      "ENABLE_OPTIMISATION"       OFF)
else ()
	option(ENABLE_PARALLELISATION   "ENABLE_PARALLELISATION"    ON)
	option(ENABLE_OPTIMISATION      "ENABLE_OPTIMISATION"       ON)
endif ()

# Library linking options - allow packagers to override defaults
# Default to static linking for vcpkg/development, but allow dynamic for distributions
option(USE_STATIC_LIBS          "Use static libraries (Boost, yaml-cpp)"    ON)
option(USE_STATIC_BOOST         "Use static Boost libraries"               ${USE_STATIC_LIBS})
option(USE_STATIC_YAML_CPP      "Use static yaml-cpp library"              ${USE_STATIC_LIBS})

find_program(CCACHE_FOUND ccache)
if(CCACHE_FOUND)
	message(STATUS "Setting ccache            on")
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE	ccache)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_LINK		ccache) # Less useful to do it for linking, see edit2
else()
	message(STATUS "Setting ccache            off")
endif(CCACHE_FOUND)


if(ENABLE_OPTIMISATION)
	message(STATUS "Setting optimisation      on")
	set(CMAKE_CXX_FLAGS					"${CMAKE_CXX_FLAGS} -O3")
	set(CMAKE_C_FLAGS					"${CMAKE_C_FLAGS} -O3")
else()
	message(STATUS "Setting optimisation      off")
endif()

if(ENABLE_ARCHITECTURE_OPTIMISATION)
	message(STATUS "Setting arch optimisation on")
	set(CMAKE_CXX_FLAGS					"${CMAKE_CXX_FLAGS} -march=native -mtune=native")
	set(CMAKE_C_FLAGS					"${CMAKE_C_FLAGS} -march=native -mtune=native")
else()
	message(STATUS "Setting arch optimisation off")
endif()

if(ENABLE_PARALLELISATION)
	message(STATUS "Setting parallelisation   on")
	# For cross-compilation, OpenMP tests may fail but the library works
	if(CMAKE_CROSSCOMPILING)
		set(OpenMP_C_FLAGS "-fopenmp -static-libgcc")
		set(OpenMP_CXX_FLAGS "-fopenmp -static-libgcc")
		set(OpenMP_C_LIB_NAMES "gomp")
		set(OpenMP_CXX_LIB_NAMES "gomp")
		set(OpenMP_gomp_LIBRARY "gomp")
		find_package(OpenMP)
		# For Windows cross-compile, link everything statically
		if(WIN32)
			set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static")
		endif()
	else()
		find_package(OpenMP REQUIRED)
	endif()
	set (CMAKE_CXX_FLAGS				"${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
	set (CMAKE_C_FLAGS					"${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
else()
	message(STATUS "Setting parallelisation   off")
endif()


# check if Doxygen is installed
if (BUILD_DOC)
	message(STATUS "Setting docs              on")
	find_package(Doxygen)
	if (DOXYGEN_FOUND)

		# set input and output files
		set(DOXYGEN_IN		../doc_templates/Doxyfile.in)
		set(DOXYGEN_OUT		Doxyfile)
		set(DOXYGEN_LAYIN	../doc_templates/DoxygenLayout.xml.in)
		set(DOXYGEN_LAYOUT	DoxygenLayout.xml)

		# note the option ALL which allows to build the docs together with the application
		add_custom_target(docs
			ALL
			COMMAND				${CMAKE_COMMAND} -DIN=${DOXYGEN_IN} -DOUT=${DOXYGEN_OUT} -P "../cmake/Configure.cmake"
			COMMAND				${CMAKE_COMMAND} -DIN=${DOXYGEN_LAYIN} -DOUT=${DOXYGEN_LAYOUT} -P "../cmake/Configure.cmake"
			COMMAND				${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/${DOXYGEN_OUT}
			COMMAND				cp ${CMAKE_CURRENT_SOURCE_DIR}/doc_templates/css.css ./
			WORKING_DIRECTORY	${CMAKE_BUILD_DIR}
			COMMENT				"Generating API documentation with Doxygen"
			VERBATIM )

		add_custom_target(generateConfigMd
			COMMAND 			pea -Y 1
			COMMAND				mkdir -p ${CMAKE_CURRENT_SOURCE_DIR}/../Docs
			COMMAND				cp defaultConfiguration.md ${CMAKE_CURRENT_SOURCE_DIR}/../Docs
			COMMENT				"Generating config markdown file"
			VERBATIM )

		add_dependencies(docs
			generateConfigMd)

		list(APPEND CTEST_CUSTOM_WARNING_EXCEPTION
		".*warning: ignoring loop annotation.*"
		".*warning: .*Consider increasing DOT_GRAPH_MAX_NODES."
		)

	else (DOXYGEN_FOUND)

		message("Doxygen need to be installed to generate the doxygen documentation, disabling")
		set(BUILD_DOC off CACHE BOOL "" FORCE)
	endif (DOXYGEN_FOUND)
else()
	message(STATUS "Setting docs              off")
endif()




set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
find_package(Threads REQUIRED)
if (CMAKE_USE_PTHREADS_INIT)
	set (CMAKE_CXX_FLAGS				"${CMAKE_CXX_FLAGS} -pthread")
endif()

# Configure yaml-cpp linking
if(USE_STATIC_YAML_CPP)
	set(YAML_CPP_USE_STATIC_LIBS ON)
	message(STATUS "Using static yaml-cpp")
else()
	set(YAML_CPP_USE_STATIC_LIBS OFF)
	message(STATUS "Using dynamic yaml-cpp")
endif()
find_package(YAML_CPP 0.6.2 REQUIRED)

# OpenSSL - use MODULE mode for vcpkg compatibility (wrapper handles it)
find_package(OpenSSL REQUIRED)

# Configure Boost linking
if(USE_STATIC_BOOST)
	set(Boost_USE_STATIC_LIBS ON)
	message(STATUS "Using static Boost libraries")
else()
	set(Boost_USE_STATIC_LIBS OFF)
	message(STATUS "Using dynamic Boost libraries")
endif()

#set(Boost_NO_SYSTEM_PATHS ON)
# Note: Boost system is header-only since 1.67, no longer needed as a component
find_package(Boost 1.75.0 REQUIRED COMPONENTS log log_setup date_time thread program_options serialization timer json unit_test_framework)

if(TARGET Boost::log)
	get_target_property(BOOST_LOG_LIBRARY_TYPE Boost::log TYPE)
	if(BOOST_LOG_LIBRARY_TYPE STREQUAL "SHARED_LIBRARY")
		add_compile_definitions(BOOST_LOG_DYN_LINK)
	elseif(NOT USE_STATIC_BOOST)
		message(STATUS "Shared Boost requested, but the available Boost::log target is ${BOOST_LOG_LIBRARY_TYPE}")
	endif()
elseif(NOT USE_STATIC_BOOST)
	# Legacy FindBoost variables do not expose the selected library type.
	add_compile_definitions(BOOST_LOG_DYN_LINK)
endif()

find_package(Eigen3 CONFIG QUIET)
if(NOT Eigen3_FOUND)
    find_package(Eigen3 REQUIRED)
endif()
if(Eigen3_VERSION VERSION_LESS 3.3 OR Eigen3_VERSION VERSION_GREATER_EQUAL 6)
	message(FATAL_ERROR "Found unsupported Eigen version ${Eigen3_VERSION}; expected Eigen >= 3.3 and < 6")
endif()
if(TARGET Eigen3::Eigen AND NOT EIGEN3_INCLUDE_DIRS)
	get_target_property(EIGEN3_INCLUDE_DIRS Eigen3::Eigen INTERFACE_INCLUDE_DIRECTORIES)
endif()
include_directories(${EIGEN3_INCLUDE_DIRS})

# MongoDB is optional - platforms like MinGW/Windows may not support it
# Users can force it off by setting ENABLE_MONGODB=OFF
# On Windows, MongoDB is disabled by default
if(WIN32)
    option(ENABLE_MONGODB "Enable MongoDB support" OFF)
else()
    option(ENABLE_MONGODB "Enable MongoDB support" ON)
endif()

if(ENABLE_MONGODB)
    # Try CONFIG mode first (for vcpkg), fall back to module mode (for brew/system)
    find_package(mongocxx CONFIG QUIET)
    if(mongocxx_FOUND)
        find_package(bsoncxx CONFIG QUIET)
        if(bsoncxx_FOUND)
            set(ENABLE_MONGODB ON)
            message(STATUS "MongoDB support enabled")
        else()
            message(STATUS "MongoDB bsoncxx not found - building without MongoDB support")
            set(ENABLE_MONGODB OFF)
        endif()
    else()
        message(STATUS "MongoDB mongocxx not found - building without MongoDB support")
        set(ENABLE_MONGODB OFF)
    endif()
else()
    message(STATUS "MongoDB support disabled by user")
endif()

# Using local magic_enum header in 3rdparty directory

# Pre-find dependencies for netCDF to ensure proper propagation of vcpkg variables
find_package(ZLIB REQUIRED)
find_package(CURL CONFIG)

# Find NetCDF libraries using vcpkg's modern CMake config
set(NETCDF_CXX "YES")
find_package(netCDF CONFIG)
find_package(netCDFCxx CONFIG)

if (netCDF_FOUND AND netCDFCxx_FOUND)
    set(NetCDF_FOUND TRUE)
    message(STATUS "Found NetCDF C library via vcpkg")
    message(STATUS "Found NetCDF C++ library via vcpkg")
else()
    # Fallback to legacy FindNetCDF.cmake for system installations
    find_package(NetCDF)
    if (NOT NetCDF_FOUND)
        message(STATUS "NetCDF library not found, skip compiling loading packages.")
    endif()
endif()


if(BLA_VENDOR STREQUAL "OpenBLAS")
    find_package(OpenBLAS CONFIG REQUIRED)
    if(TARGET OpenBLAS::OpenBLAS)
        set(BLAS_LIBRARIES OpenBLAS::OpenBLAS)
    else()
        set(BLAS_LIBRARIES ${OpenBLAS_LIBRARIES})
    endif()
    set(BLAS_INCLUDE_DIRS ${OpenBLAS_INCLUDE_DIRS})

    set(_GINAN_OPENBLAS_LIBRARIES ${BLAS_LIBRARIES})
    set(_GINAN_OPENBLAS_INCLUDE_DIRS ${BLAS_INCLUDE_DIRS})
    find_package(LAPACK REQUIRED)
    if(APPLE)
        find_library(ACCELERATE_FRAMEWORK Accelerate REQUIRED)
        set(LAPACK_LIBRARIES ${ACCELERATE_FRAMEWORK})
    elseif(TARGET LAPACK::LAPACK)
        set(LAPACK_LIBRARIES LAPACK::LAPACK)
    elseif(NOT LAPACK_LIBRARIES)
        # Some OpenBLAS installs include LAPACK symbols directly.
        set(LAPACK_LIBRARIES ${_GINAN_OPENBLAS_LIBRARIES})
    endif()
    set(BLAS_LIBRARIES ${_GINAN_OPENBLAS_LIBRARIES})
    set(BLAS_INCLUDE_DIRS ${_GINAN_OPENBLAS_INCLUDE_DIRS})

    message(STATUS "Using OpenBLAS for BLAS:       ${BLAS_LIBRARIES}")
    message(STATUS "Using LAPACK library:         ${LAPACK_LIBRARIES}")
else()
    find_package(BLAS REQUIRED)
    find_package(LAPACK QUIET)

    if(LAPACK_FOUND)
        message(STATUS "Found BLAS library:       ${BLAS_LIBRARIES}")
        message(STATUS "Found LAPACK library:     ${LAPACK_LIBRARIES}")
    else()
        # OpenBLAS includes LAPACK but FindLAPACK might not detect it.
        message(STATUS "Found BLAS library:       ${BLAS_LIBRARIES}")
        message(STATUS "LAPACK not found separately - assuming BLAS includes LAPACK")
        set(LAPACK_LIBRARIES ${BLAS_LIBRARIES})
    endif()
endif()

if (YAML_CPP_LIB)
	message(STATUS "Found YAML library:       " ${YAML_CPP_LIB})
else()
	message(STATUS "YAML was not found")
endif()

if (NetCDF_FOUND)
	message(STATUS "Found NetCDF library:     " ${NETCDF_LIBRARIES_CXX})
else()
	message(STATUS "NetCDF was not found")
endif()

if (Eigen3_FOUND)
	message(STATUS "Found Eigen version:      " ${Eigen3_VERSION})
else()
	message(STATUS "Eigen was not found")
endif()

if (Boost_FOUND)
	message(STATUS "Found Boost version:      " ${Boost_VERSION})
else()
	message(STATUS "Boost was not found")
endif()

if (mongocxx_FOUND)
	message(STATUS "Found Mongocxx version:   " ${mongocxx_VERSION})
else()
	message(STATUS "Mongocxx was not found")
endif()



message(STATUS "Found C++ compiler:       " ${CMAKE_CXX_COMPILER_ID} " " ${CMAKE_CXX_COMPILER_VERSION})
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaLibVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaLibVersion.h @ONLY)

include(GitVersion)

# Ensure the file with the commit version is generated before compiling
add_custom_target(updateGit ALL
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_SOURCE_DIR}/cmake/GitVersion.cmake
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    COMMENT "Updating git version"
)

# Make sure it regenerates every time the project is built

link_directories(/usr/lib64 ${CMAKE_LIBRARY_OUTPUT_DIRECTORY})

# CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h @ONLY)


add_subdirectory(cpp)
enable_testing()
add_subdirectory(tests)


message(STATUS "CMAKE_CURRENT_BINARY_DIR: ${CMAKE_CURRENT_BINARY_DIR}")
message(STATUS "CMAKE_CURRENT_SOURCE_DIR: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "CMAKE_CXX_FLAGS:         ${CMAKE_CXX_FLAGS}")

macro(print_all_variables)
	message(STATUS "print_all_variables------------------------------------------{")
	get_cmake_property(_variableNames VARIABLES)
	foreach (_variableName ${_variableNames})
		message(STATUS "${_variableName}=${${_variableName}}")
	endforeach()
	message(STATUS "print_all_variables------------------------------------------}")
endmacro()

# print_all_variables()
