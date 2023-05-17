cmake_minimum_required(VERSION 2.8...3.22)
cmake_policy(SET CMP0063 NEW)

project(ginan)


enable_language(Fortran)

set (CMAKE_Fortran_SOURCE_FILE_EXTENSIONS "f95;F90;f90;for;f;F")
set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../lib")
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../lib")
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../bin")

#this bit doesn't work on DARWIN (MAC)
#have to modify cmake configure file directly
if (${CMAKE_SYSTEM_NAME} MATCHES "Darwin") 
	message("found DARWIN")
endif ()
file(GLOB_RECURSE sources fortran/*.f90 fortran/*.F90 fortran/*.f fortran/*.f95 fortran/*.for fortran/*.F)
foreach (file ${sources})
	if ( ${CMAKE_SYSTEM_NAME} MATCHES "Darwin") 
		execute_process(COMMAND /usr/bin/basename    ${file} OUTPUT_VARIABLE fortranfile)
		#message("found DARWIN")
	else()
		execute_process(COMMAND /usr/bin/basename -z ${file} OUTPUT_VARIABLE fortranfile)
	endif()
	set_source_files_properties(fortran/${fortranfile} PROPERTIES LANGUAGE Fortran)
	#set_source_files_properties(fortran/${fortranfile} PROPERTIES COMPILE_FLAGS "-O3 -frecursive")
	#message("set fortran/${fortranfile} to fortran compile")
endforeach (file ${sources})

set(CMAKE_MODULE_PATH					"${CMAKE_SOURCE_DIR}/cmake")
set(CMAKE_CXX_VISIBILITY_PRESET     	hidden)
set(CMAKE_VERBOSE_MAKEFILE				OFF)

set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -std=c++2a")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -ggdb3")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -fpermissive")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wall")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-write-strings")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-deprecated-declarations")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-format-overflow")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-narrowing")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-sign-compare")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-stringop-truncation")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unused-variable")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-switch")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-dangling-else")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-misleading-indentation")
#set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -fno-var-tracking-assignments")
#set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unknown-warning-option")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-format-truncation")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-extern-c-compat")
set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-format-zero-length")

set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -g")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -frecursive")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -fall-intrinsics")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wall")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -fcheck=all")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -fbacktrace")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-unused-dummy-argument")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-conversion")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-unused-variable")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-tabs")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-surprising")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-unused-label")
set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-maybe-uninitialized")




if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE Release)
endif()

	option(ENABLE_UNIT_TESTS        "ENABLE_UNIT_TESTS"         OFF)
	option(BUILD_DOC                "BUILD_DOCUMENTATION"       ON)

if (CMAKE_BUILD_TYPE STREQUAL "Debug")
	option(ENABLE_PARALLELISATION   "ENABLE_PARALLELISATION"    OFF)
	option(ENABLE_OPTIMISATION      "ENABLE_OPTIMISATION"       OFF)
else ()
	option(ENABLE_PARALLELISATION   "ENABLE_PARALLELISATION"    ON)
	option(ENABLE_OPTIMISATION      "ENABLE_OPTIMISATION"       ON)
endif ()




if(ENABLE_UNIT_TESTS)
	message(STATUS "Setting unit tests      on")
endif()

if(ENABLE_OPTIMISATION)
	message(STATUS "Setting optimisation    on")
	set(CMAKE_CXX_FLAGS					"${CMAKE_CXX_FLAGS} -O3")
	set(CMAKE_C_FLAGS					"${CMAKE_C_FLAGS} -O3")
	set(CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -O3")
endif()

if(ENABLE_PARALLELISATION)
	message(STATUS "Setting parallelisation on")
	find_package(OpenMP REQUIRED)
	set (CMAKE_CXX_FLAGS				"${CMAKE_CXX_FLAGS} ${OpenMP_CXX_FLAGS}")
	set (CMAKE_C_FLAGS					"${CMAKE_C_FLAGS} ${OpenMP_C_FLAGS}")
	set (CMAKE_Fortran_FLAGS			"${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")
endif()


# check if Doxygen is installed
if (BUILD_DOC)
	message(STATUS "Setting docs            on")
	find_package(Doxygen)
	if (DOXYGEN_FOUND)

		# set input and output files
		set(DOXYGEN_IN		../doc_templates/Doxyfile.in)
		set(DOXYGEN_OUT		Doxyfile)
		set(DOXYGEN_LAYIN	../doc_templates/DoxygenLayout.xml.in)
		set(DOXYGEN_LAYOUT	DoxygenLayout.xml)

		# note the option ALL which allows to build the docs together with the application
		add_custom_target( docs
			ALL
			COMMAND				${CMAKE_COMMAND} -DIN=${DOXYGEN_IN} -DOUT=${DOXYGEN_OUT} -P "../cmake/Configure.cmake"
			COMMAND				${CMAKE_COMMAND} -DIN=${DOXYGEN_LAYIN} -DOUT=${DOXYGEN_LAYOUT} -P "../cmake/Configure.cmake"
			COMMAND				${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/${DOXYGEN_OUT}
			WORKING_DIRECTORY	${CMAKE_CURRENT_SOURCE_DIR}/build
			COMMENT				"Generating API documentation with Doxygen"
			VERBATIM )
					
		list(APPEND CTEST_CUSTOM_WARNING_EXCEPTION
		".*warning: ignoring loop annotation.*"
		".*warning: .*Consider increasing DOT_GRAPH_MAX_NODES."
		)
		
	else (DOXYGEN_FOUND)

		message("Doxygen need to be installed to generate the doxygen documentation, disabling")
		set(BUILD_DOC off CACHE BOOL "" FORCE)
	endif (DOXYGEN_FOUND)
endif()




set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
find_package(Threads REQUIRED)
if (CMAKE_USE_PTHREADS_INIT)
	set (CMAKE_CXX_FLAGS				"${CMAKE_CXX_FLAGS} -pthread")
	set (CMAKE_Fortran_FLAGS			"${CMAKE_Fortran_FLAGS} -pthread")
endif()

set(YAML_CPP_USE_STATIC_LIBS ON)
find_package(YAML_CPP 0.6.2 REQUIRED)

find_package(OpenSSL REQUIRED)

#set(Boost_NO_SYSTEM_PATHS ON)
set(Boost_USE_STATIC_LIBS ON)
find_package(Boost 1.73.0 REQUIRED COMPONENTS log log_setup date_time filesystem system thread program_options serialization timer)

find_package(Eigen3 3.3.0)
include_directories(${EIGEN3_INCLUDE_DIRS})

find_package(mongocxx	REQUIRED)
find_package(bsoncxx	REQUIRED)

set(OPENBLAS_USE_STATIC_LIBS ON)
set(BLA_VENDOR OpenBLAS)

find_package(BLAS)
if(BLAS_FOUND)
	set(LAPACK_LIBRARIES "")
	message(STATUS "Found BLAS library:       " ${BLA_VENDOR})
else()
	set (BLA_VENDOR "")
	find_package(LAPACK REQUIRED)
	find_package(BLAS   REQUIRED)  
	message(STATUS "Found LAPACK and BLAS")
endif()

if (YAML_CPP_LIB)
	message(STATUS "Found YAML library:       " ${YAML_CPP_LIB})
else()
	message(STATUS "YAML was not found")
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


IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/../.git)
	FIND_PACKAGE(Git)
	IF(GIT_FOUND)

		EXECUTE_PROCESS(
			COMMAND ${GIT_EXECUTABLE} log --pretty=format:'%h' -n 1
			OUTPUT_VARIABLE GINAN_COMMIT_HASH
			ERROR_QUIET
			OUTPUT_STRIP_TRAILING_WHITESPACE
			)
		EXECUTE_PROCESS(
			COMMAND bash -c "git diff --quiet --exit-code || echo -dirty "
			OUTPUT_VARIABLE GINAN_COMMIT_DIFF
			OUTPUT_STRIP_TRAILING_WHITESPACE
			)
		EXECUTE_PROCESS(
			COMMAND ${GIT_EXECUTABLE} describe --exact-match --tags
			OUTPUT_VARIABLE GINAN_COMMIT_TAG ERROR_QUIET
			OUTPUT_STRIP_TRAILING_WHITESPACE
			)
		EXECUTE_PROCESS(
			COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
			OUTPUT_VARIABLE GINAN_BRANCH_NAME
			OUTPUT_STRIP_TRAILING_WHITESPACE
			)
		EXECUTE_PROCESS(
			COMMAND ${GIT_EXECUTABLE} log -1 --format=%cd --date=local
			OUTPUT_VARIABLE "GINAN_COMMIT_DATE"
			OUTPUT_STRIP_TRAILING_WHITESPACE
			)

		if (GINAN_COMMIT_TAG STREQUAL "")
			set(GINAN_COMMIT_TAG "untagged")
		endif()
		string(REGEX REPLACE "'" "" GINAN_COMMIT_HASH "${GINAN_COMMIT_HASH}")

		SET(GINAN_COMMIT_VERSION "${GINAN_COMMIT_TAG}-${GINAN_COMMIT_HASH}${GINAN_COMMIT_DIFF}")
		MESSAGE(STATUS "Git branch tag:           ${GINAN_COMMIT_VERSION}")
		MESSAGE(STATUS "Git branch:               ${GINAN_BRANCH_NAME}")
	ELSE(GIT_FOUND)
		SET(GINAN_COMMIT_VERSION "N/A")
		SET(GINAN_COMMIT_DIFF "N/A")
		SET(GINAN_COMMIT_TAG "N/A")
		SET(GINAN_BRANCH_NAME "N/A")
		MESSAGE( STATUS "Git not found: ${GINAN_COMMIT_VERSION}" )
	ENDIF(GIT_FOUND)
ELSE()
	MESSAGE( STATUS "Git not found in ${CMAKE_CURRENT_SOURCE_DIR}/..")
ENDIF()


link_directories(/usr/lib64 ${CMAKE_BINARY_DIR}/../../lib)

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h @ONLY)


set (NETCDF_CXX "YES")
find_package (NetCDF REQUIRED)

# message(STATUS "...NETCDF     >>>>>>   ${NETCDF_LIBRARIES}     ${NETCDF_INCLUDES}" )
# message(STATUS "...NETCDF_C++ >>>>>>   ${NETCDF_LIBRARIES_CXX} ${NETCDF_INCLUDES_CXX}" )
# find_package(netCDFCxx REQUIRED)

add_subdirectory(cpp)
add_subdirectory(fortran)

					 
message(STATUS "CMAKE_CURRENT_BINARY_DIR: ${CMAKE_CURRENT_BINARY_DIR}")
message(STATUS "CMAKE_CURRENT_SOURCE_DIR: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "CMAKE_CXX_FLAGS:         ${CMAKE_CXX_FLAGS}")
message(STATUS "CMAKE_Fortran_FLAGS:     ${CMAKE_Fortran_FLAGS}")

macro(print_all_variables)
	message(STATUS "print_all_variables------------------------------------------{")
	get_cmake_property(_variableNames VARIABLES)
	foreach (_variableName ${_variableNames})
		message(STATUS "${_variableName}=${${_variableName}}")
	endforeach()
	message(STATUS "print_all_variables------------------------------------------}")
endmacro()

#print_all_variables()
