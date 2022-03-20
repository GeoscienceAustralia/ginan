cmake_minimum_required(VERSION 2.8)
cmake_policy(SET CMP0063 NEW)
cmake_policy(SET CMP0054 OLD)

project(ginan)
macro(print_all_variables)
	message(STATUS "print_all_variables------------------------------------------{")
	get_cmake_property(_variableNames VARIABLES)
	foreach (_variableName ${_variableNames})
		message(STATUS "${_variableName}=${${_variableName}}")
	endforeach()
	message(STATUS "print_all_variables------------------------------------------}")
endmacro()

enable_language(Fortran)
if ( ${CMAKE_SYSTEM_NAME} MATCHES "Darwin") 
	message("found DARWIN")
endif ()

set (CMAKE_Fortran_SOURCE_FILE_EXTENSIONS "f95;F90;f90;for;f;F")
set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../lib")
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../lib")
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../bin")
#this bit doesn't work on DARWIN (MAC)
#have to modify cmake configure file directly
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
# 	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-c++11-narrowing")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unused-but-set-variable")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-sign-compare")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-stringop-truncation")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unused-variable")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-switch")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-dangling-else")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-misleading-indentation")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-var-tracking-assignments")
	set(CMAKE_CXX_FLAGS						"${CMAKE_CXX_FLAGS} -Wno-unknown-warning-option")

	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -g")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -frecursive")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -fall-intrinsics")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wall")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -fcheck=all")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -fbacktrace")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-unused-dummy-argument")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-conversion")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-surprising")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-tabs")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-unused-variable")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-unused-label")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-maybe-uninitialized")
	set (CMAKE_Fortran_FLAGS				"${CMAKE_Fortran_FLAGS} -Wno-ampersand")

# Uncomment these for Linux (e.g. CentOS 7) where boost 1.69.0 is packaged as boost169 (and boost is 1.53.0 for e.g.)
#set(BOOST_INCLUDEDIR /data/work/lib/boost-1.70.0/include)
#set(BOOST_LIBRARYDIR /data/work/lib/boost-1.70.0/lib)
set(Boost_NO_SYSTEM_PATHS ON)
set(Boost_USE_STATIC_LIBS ON)

# first we can indicate the documentation build as an option and set it to ON by default
option(BUILD_DOC                "Build documentation"       OFF)
option(ENABLE_UNIT_TESTS        "ENABLE_UNIT_TESTS"         OFF)
option(ENABLE_MONGODB           "ENABLE_MONGODB"            ON)
option(ENABLE_PARALLELISATION   "ENABLE_PARALLELISATION"    ON)
option(ENABLE_OPTIMISATION      "ENABLE_OPTIMISATION"       ON)

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

if(ENABLE_MONGODB)
	message(STATUS "Setting mongo           on")
	find_package(mongocxx	REQUIRED)
	find_package(bsoncxx	REQUIRED)
endif()

#set debug if nothing else is set.
set(default_build_type "Debug")
if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE ${default_build_type})
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

find_package(Boost 1.73.0 REQUIRED COMPONENTS log log_setup date_time filesystem system thread program_options serialization timer)

find_package(Eigen3 3.3.0)
include_directories(${EIGEN3_INCLUDE_DIRS})


set(OPENBLAS_USE_STATIC_LIBS ON)
set (BLA_VENDOR OpenBLAS)

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

	message(STATUS "Found C++ compiler:       " ${CMAKE_CXX_COMPILER_VERSION})


IF(EXISTS ${CMAKE_CURRENT_SOURCE_DIR}/../.git)
	FIND_PACKAGE(Git)
	IF(GIT_FOUND)
		EXECUTE_PROCESS(
		COMMAND ${GIT_EXECUTABLE} describe --tags --always
		WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/.."
		OUTPUT_VARIABLE "GINAN_COMMIT_VERSION"
			OUTPUT_STRIP_TRAILING_WHITESPACE)

		EXECUTE_PROCESS(
				COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
				WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/.."
			OUTPUT_VARIABLE "GINAN_BRANCH_NAME"
				OUTPUT_STRIP_TRAILING_WHITESPACE)

		EXECUTE_PROCESS(
			COMMAND ${GIT_EXECUTABLE} log -1 --format=%cd --date=local
			WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/.."
		OUTPUT_VARIABLE "GINAN_COMMIT_DATE"
		OUTPUT_STRIP_TRAILING_WHITESPACE)

		MESSAGE(STATUS "Git branch tag:           ${GINAN_COMMIT_VERSION}")
		MESSAGE(STATUS "Git branch:               ${GINAN_BRANCH_NAME}")
	ELSE(GIT_FOUND)
		SET(GINAN_COMMIT_VERSION 0)
		SET(GINAN_BRANCH_NAME "unknown branch")
		MESSAGE( STATUS "Git not found: ${GINAN_COMMIT_VERSION}" )
	ENDIF(GIT_FOUND)
ELSE()
	MESSAGE( STATUS "Git not found in ${CMAKE_CURRENT_SOURCE_DIR}/..")
ENDIF()

CONFIGURE_FILE(${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h @ONLY)


add_library(pod_lib
	fortran/mdl_param.f95
	fortran/mdl_precision.f90
	fortran/mdl_arr.f90
	fortran/mdl_num.f90
	fortran/mdl_config.f95
	fortran/mdl_eop.f90
	fortran/mdl_planets.f90
	fortran/mdl_tides.f90
	fortran/m_matrixRxR.f95
	fortran/m_matrixinv.f95
	fortran/m_matrixreverse.f95
	fortran/m_matrixmerge.f95
	fortran/m_integrEQM.f95
	fortran/m_force_tides.f95
	fortran/m_tides_ocean.f95
	fortran/m_pd_geopotential.f95
	fortran/m_legendre.f95
	fortran/m_legendre1.f95
	fortran/m_legendre2.f95
	fortran/m_force_gfm.f95
	fortran/pd_forceZ.f95
	fortran/m_get_lambda.f
	fortran/m_shadow.f90
	fortran/m_pd_empirical.f95
	fortran/m_emp_init.f95
	fortran/m_pd_ECOM.f90
	fortran/m_pulses_init.f95
	fortran/m_pd_pulses.f95
	fortran/m_pulses_force.f95
	fortran/m_satinfo.f90
	fortran/m_pd_force.f95
	fortran/m_veq_rkn768.f95
	fortran/m_veqC2T.f95
	fortran/m_integrVEQ.f95
	fortran/m_betainfo.f90
	fortran/m_orbinteg.f95
	fortran/m_orb_estimator.f95
	fortran/m_orbC2T.f95
	fortran/m_orbT2C.f95
	fortran/m_statist.f95
	fortran/m_statdelta.f95
	fortran/m_orbinfo.f90
	fortran/m_statorbit.f95
	fortran/m_statorbit2.f95
	fortran/m_writearray.f95
	fortran/m_writearray2.f95
	fortran/m_write_orbres.f95
	fortran/m_orbresize.f95
	fortran/m_orbext.f95
	fortran/m_orbext2.f95
	fortran/m_gfc.f95
	fortran/m_gfc3.f95
	fortran/m_obsorbT2C.f95
	fortran/m_writedata.f95
	fortran/m_writeorbit.f95
	fortran/m_read_svsinex.f95
	fortran/m_read_satsnx.f95
	fortran/m_writeorbit_multi.f95
	fortran/m_write_prmfile_init.f95
	fortran/m_rso.f95
	fortran/m_sp3.f95
	fortran/m_sp3_PRN.f95
	fortran/m_lagrange.f95
	fortran/m_interporb.f95
	fortran/m_interporb_nom.f95
	fortran/m_interporb_filt.f95
	fortran/m_interpclocks.f95
	fortran/m_interpclock_nom.f95
	fortran/m_orb_outlier.f95
	fortran/m_eop_data.f95
	fortran/m_keplerorb.f95
	fortran/m_ecom_init.f95
	fortran/m_orbdet.f95
	fortran/m_orbitmain.f95
	fortran/m_orbitIC.f95
	fortran/m_attitude_orb.f95
	fortran/m_write_orbex.f95
	fortran/m_satmetadata.f95
	)

add_library(iau_math
	fortran/R3.for
	fortran/R1.for
	fortran/jd2cal.for
	fortran/cal2jd.for
	fortran/fad03.for
	fortran/faf03.for
	fortran/fal03.for
	fortran/fama03.for
	fortran/fane03.for 
	fortran/fapa03.for
	fortran/faur03.for
	fortran/fae03.for 
	fortran/faju03.for
	fortran/falp03.for
	fortran/fame03.for
	fortran/faom03.for
	fortran/fasa03.for
	fortran/fave03.for
	fortran/xy06.for
	fortran/s06.for
	fortran/c2ixys.for
	fortran/xys00a.for
	fortran/pnm00a.for
	fortran/bpn2xy.for
	fortran/s00.for
	fortran/pn00a.for
	fortran/nut00a.for
	fortran/pn00.for
	fortran/pr00.for
	fortran/obl80.for
	fortran/bp00.for
	fortran/numat.for
	fortran/bi00.for
	fortran/ir.for
	fortran/rz.for
	fortran/ry.for
	fortran/rx.for
	fortran/anp.for
	fortran/era00.for
	fortran/gmst00.for
	fortran/gmst06.for
	fortran/sp00.for
	fortran/pom00.for
	fortran/taiutc.for
	fortran/rxr.for
	fortran/tr.for
	fortran/cr.for
	fortran/cp.for
	)
   
add_library(iers
	fortran/ORTHO_EOP.F
	fortran/CNMTX.F
	fortran/UTLIBR.F
	fortran/PMSDNUT2.F
	fortran/FUNDARG.F
	fortran/RG_ZONT2.F
	)

#ga_math library requires LAPACK as m_matrixinv uses it
add_library(ga_math
	fortran/m_matrixRxR.f95
	fortran/m_matrixinv.f95
	fortran/m_matrixreverse.f95
	fortran/m_matrixmerge.f95
	fortran/matrix_inv3.f90
	fortran/matrix_Rr.f90
	fortran/matrix_RxR.f90
	fortran/productdot.f90
	fortran/productcross.f90
	fortran/arctan.f90
	fortran/STATE.f90
	fortran/PLEPH.f
	fortran/CONST.f
	fortran/SPLIT.f
	fortran/INTERP.f
	fortran/FSIZER3.f
	fortran/interp_lin.f90
	)

add_library(ga_tz
	fortran/time_TT.f90
	fortran/time_GPS.f90
	fortran/time_UTC.f90
	fortran/time_TAI.f90
	fortran/time_GPSweek.f90
	fortran/time_GPSweek2.f90
	fortran/time_GPSweek3.f90
	fortran/time_TT_sec.f90
	)

add_library(boxwing
	fortran/BOXWINIT.f90
	fortran/ERPFBOXW.f90
	fortran/SURFBOXW.f90
	fortran/PROPBOXW.f90
	fortran/SRPFBOXW.f90
	)

add_library(tides
	fortran/tides_solid1.f90
	fortran/tides_solid2.f90
	fortran/tide_perm.f90
	fortran/tides_fes2004.f90
	fortran/tide_pole_se.f90
	fortran/tide_pole_oc.f90
	)

add_library(ga_gen
	fortran/doy2str.f95
	fortran/f90getopt.F90
	fortran/read_cmdline.f95
	)

add_library(rw_param
	fortran/write_prmfile_init0.f95
	fortran/writeparam.f95
	fortran/writeparam1.f95
	fortran/write_prmfile.f95
	fortran/write_prmfile2.f95
	fortran/readparam.f95 
	fortran/prm_main.f95
	fortran/prm_emp.f95
	fortran/prm_srp.f95
	fortran/prm_grav.f95
	fortran/prm_gravity.f95
	fortran/prm_nongrav.f95
	fortran/prm_ocean.f95
	fortran/prm_orbext.f95
	fortran/prm_planets.f95
	fortran/prm_pseudobs.f95
	)

add_library(force
	fortran/force_gm.f90
	fortran/force_gm3rd.f90
	fortran/force_srp.f90
	fortran/force_erp.f90
	fortran/force_ant.f90
	fortran/force_sum.f95
	fortran/apr_srp.f90
	)

add_library(rel
	fortran/rel_schwarzschild.f90
	fortran/rel_LenseThirring.f90
	fortran/rel_deSitter.f90
	)

add_library(rk
	fortran/integr_rkn768.f95
	fortran/integr_rk87.f95
	fortran/integr_rk4.f95
	)

add_library(yaw
	fortran/yaw_nom.f90
	fortran/yaw_angle.f90
	fortran/yaw_attitude.f90
	fortran/yaw_bds.f90
	fortran/yaw_gal.f90
	fortran/yaw_gal_iov.f95
	fortran/yaw_gal_foc.f95
	fortran/yawdyn.f90
	fortran/att_matrix.f95
	fortran/mat2quater.f95
	fortran/yaw_gal_foc_nom.f95
	fortran/yaw_pred.f95
	fortran/colinearity_angle.f95
	fortran/colinear_pred.f95
	)

add_library(eop
	fortran/EOP.f90
	fortran/eop_rd.f90
	fortran/eop_c04.f90
	fortran/m_eop_cor.f95
	fortran/m_eop_igu.f95
	fortran/eop_finals2000A.f90
	fortran/erp_igu.f90
	)

add_library(kepler
	fortran/kepler_eq.f90
	fortran/kepler_k2z.f90
	fortran/kepler_z2k.f90
	)

add_library(eclipse
	fortran/eclips.f
	fortran/eclips2017.f
	fortran/eclips201707.f
	fortran/eclipse_integstep.f95
	)

add_library(yaml
	fortran/yaml_types.F90
	fortran/yaml_version.F90
	fortran/yaml_settings.F90
	fortran/yaml.F90
	fortran/m_pod_yaml.F90
	fortran/fthread.c
	)

add_executable(pod
	fortran/report.f90
	fortran/gmst_iers.f95
	fortran/interp_iers.f
	fortran/IERS_CMP_2015.F
	fortran/m_read_leapsec.f95
	fortran/m_clock_read.f95
	fortran/m_write_orb2sp3.f95
	fortran/m_pod_gnss.f95
	fortran/gblinit.f95
	fortran/coord_r2sph.f90
	fortran/crs_trs.f90
	fortran/erp_igu.f90
	fortran/era_matrix.f90
	fortran/orb_frame.f90
	fortran/orb_frame2_unit.f90
	fortran/crf_bff.f90
	fortran/GM_de.f90
	fortran/CATfile.f90
	fortran/asc2eph.f90
	fortran/indirectJ2.f90
	fortran/delaunay.f90
	fortran/prn_shift.f95
	fortran/surfprop.f90
	fortran/scan0orb.f95
	fortran/empirical_init.f95
	fortran/empirical_cor.f95
	fortran/beta_angle.f90
	fortran/beta_pred.f90
	fortran/attitude.f95
	fortran/pd_gm.f95
	fortran/pd_forceZ.f95
	fortran/main_pod.f95
	)

add_executable(crs2trs
	fortran/report.f90
	fortran/coord_r2sph.f90
	fortran/interp_iers.f
	fortran/crs_trs.f90
	fortran/main_crs2trs.f95
	)

add_executable(brdc2ecef
	fortran/report.f90
	fortran/m_read_leapsec.f95
	fortran/asc2eph.f90
	fortran/brdc2ecef.f90
	fortran/brdc_cmdline.f95
	fortran/brdc_qc_gal.f90
	fortran/brdc_qc_gps.f90
	fortran/CATfile.f90
	fortran/chkbrdc.f90
	fortran/crs_trs.f90
	fortran/reformbrdc.f90
	fortran/glnacc.f90
	fortran/glnorbint.f90
	fortran/m_antoffset.f90
	fortran/mdl_brdconfig.f95
	fortran/m_meanstd.f90
	fortran/m_write_brd2sp3.f95
	fortran/m_write_orb2sp3.f95
	fortran/prn2str.f95
	fortran/prn_shift_brdc.f95
	fortran/readbrdcheader.f95
	fortran/readbrdcmessg.f90
	fortran/main_brdcorbit.f90
	)

add_executable(timesystem
	fortran/mdl_timeconfig.f95
	fortran/time_cmdline.f95
	fortran/main_time.f95
	)

add_executable(test_yaml
	fortran/test_yaml.F90
	)

add_executable(pea
		cpp/3rdparty/enum.h
		
		cpp/3rdparty/jpl/jpl_eph.cpp
		cpp/3rdparty/jpl/jpl_eph.hpp
		cpp/3rdparty/jpl/jpl_int.hpp	

		cpp/3rdparty/sofa/sofa.cpp
		cpp/3rdparty/sofa/sofa.hpp
		cpp/3rdparty/sofa/sofam.hpp

		cpp/3rdparty/egm96/EGM96.c
		cpp/3rdparty/egm96/EGM96.h
		cpp/3rdparty/egm96/EGM96_data.h
		
		cpp/common/acsConfig.hpp
		cpp/common/acsConfig.cpp
		cpp/common/ntripBroadcast.cpp
		cpp/common/ntripBroadcast.hpp
		cpp/common/acsQC.hpp
		cpp/common/acsQC.cpp
		cpp/common/acsStream.cpp
		cpp/common/acsStream.hpp
		cpp/common/algebra.cpp
		cpp/common/algebra_old.cpp
		cpp/common/algebra.hpp
		cpp/common/algebraTrace.cpp
		cpp/common/algebraTrace.hpp
		cpp/common/antenna.hpp
		cpp/common/antenna.cpp
		cpp/common/biasSINEX.hpp
		cpp/common/biasSINEXread.cpp
		cpp/common/biasSINEXwrite.cpp
		cpp/common/common.hpp
		cpp/common/common.cpp
		cpp/common/constants.hpp
		cpp/common/constants.cpp
		cpp/common/corrections.cpp
		cpp/common/debug.cpp
		cpp/common/debug.hpp
		cpp/common/eigenIncluder.hpp
		cpp/common/ephemeris.cpp
		cpp/common/ephemeris.hpp
		cpp/common/enums.h
		cpp/common/erp.cpp
		cpp/common/erp.hpp
		cpp/common/fileLog.cpp
		cpp/common/fileLog.hpp
		cpp/common/gTime.cpp
		cpp/common/gTime.hpp
		cpp/common/instrument.cpp
		cpp/common/instrument.hpp
		cpp/common/linearCombo.cpp
		cpp/common/linearCombo.hpp
		cpp/common/mongo.cpp
		cpp/common/mongo.hpp
		cpp/common/mongoRead.cpp
		cpp/common/mongoRead.hpp
		cpp/common/mongoWrite.cpp
		cpp/common/mongoWrite.hpp
		cpp/common/navigation.hpp
		cpp/common/observations.hpp
		cpp/common/ntripSocket.cpp
		cpp/common/ntripSocket.hpp
		cpp/common/ntripTrace.cpp
		cpp/common/ntripTrace.hpp
		cpp/common/ntripSourceTable.cpp
		cpp/common/ntripSourceTable.hpp
		cpp/common/orbits.cpp
		cpp/common/orbits.hpp
		cpp/common/preceph.cpp
		cpp/common/rtsSmoothing.cpp
		cpp/common/rtcmDecoder.cpp
		cpp/common/rtcmDecoder.hpp
		cpp/common/rtcmEncoder.cpp
		cpp/common/rtcmEncoder.hpp
		cpp/common/satStat.hpp
		cpp/common/summary.cpp
		cpp/common/summary.hpp
		cpp/common/satSys.cpp
		cpp/common/satSys.hpp
		cpp/common/sinex.hpp
		cpp/common/sinex.cpp
		cpp/common/tropSinex.cpp
		cpp/common/sp3.cpp
		cpp/common/sp3Write.cpp
		cpp/common/sp3Write.hpp		
		cpp/common/ssr.hpp
		cpp/common/station.hpp
		cpp/common/streamTrace.cpp
		cpp/common/streamTrace.hpp
		cpp/common/testUtils.cpp
		cpp/common/testUtils.hpp
		cpp/common/trigPosInterp.cpp
		cpp/common/rinexClkWrite.cpp
		cpp/common/rinexClkWrite.hpp
		cpp/common/rinexNavWrite.cpp
		cpp/common/rinexNavWrite.hpp
		cpp/common/rinexObsWrite.cpp
		cpp/common/rinexObsWrite.hpp

		cpp/common/streamFile.hpp
		cpp/common/streamNav.hpp
		cpp/common/streamNtrip.cpp
		cpp/common/streamNtrip.hpp
		cpp/common/streamObs.hpp
		cpp/common/streamRinex.hpp
		cpp/common/streamRtcm.hpp
		cpp/common/streamSp3.hpp
		
		cpp/iono/ionoMeas.cpp
		cpp/iono/ionoModel.cpp
		cpp/iono/ionoModel.hpp
		cpp/iono/ionoSpherical.cpp
		cpp/iono/ionoSphericalCaps.cpp
		cpp/iono/ionoBSplines.cpp
		cpp/iono/ionexWrite.cpp

		cpp/ambres/GNSSambres.hpp
		cpp/ambres/GNSSambres.cpp
		cpp/ambres/GinARpivot.cpp
		cpp/ambres/GinARWLamb.cpp
		cpp/ambres/GinARNLamb.cpp
		cpp/ambres/GinAR_main.cpp
		cpp/ambres/GinARoutpt.cpp
		
		cpp/orbprop/forceModels.cpp
		cpp/orbprop/forceModels.hpp
		cpp/orbprop/gravity.cpp
		cpp/orbprop/gravity.hpp
		cpp/orbprop/jplEph.cpp
		cpp/orbprop/jplEph.hpp
		cpp/orbprop/forceModels.cpp
		cpp/orbprop/forceModels.hpp
		cpp/orbprop/satRefSys.hpp
		
		cpp/pea/main.cpp
		cpp/pea/minimumConstraints.cpp
		cpp/pea/minimumConstraints.hpp
		cpp/pea/networkEstimator.cpp
		cpp/pea/networkEstimator.hpp
		cpp/pea/pea_snx.cpp
		cpp/pea/ppp.cpp
		cpp/pea/ppppp.cpp
		cpp/pea/ppppp_obs.cpp
		cpp/pea/ppppp_ambres.cpp
		cpp/pea/ppppp_pseudoobs.cpp
		cpp/pea/ppp_user.cpp
		cpp/pea/ppp_network.cpp
		cpp/pea/ppp.hpp
		cpp/pea/spp.cpp

		cpp/rtklib/ionex.cpp
		cpp/rtklib/lambda.cpp
		cpp/rtklib/lambda.h
		cpp/rtklib/readtrop.cpp
		cpp/rtklib/rinex.cpp
		cpp/rtklib/rinex.hpp
		cpp/rtklib/rtkcmn.cpp
		cpp/rtklib/tides.cpp
		cpp/rtklib/tides.hpp
		cpp/rtklib/trop.h
		cpp/rtklib/trop.cpp
		cpp/rtklib/vmf3.h
		cpp/rtklib/vmf3.cpp
		cpp/rtklib/wancorr.cpp
		cpp/rtklib/wancorr.h
		)

target_include_directories(pea PUBLIC
		cpp/3rdparty
		cpp/3rdparty/egm96
		cpp/3rdparty/sofa
		cpp/3rdparty/jpl
		
		cpp/ambres
		cpp/common
		cpp/iono
		cpp/orbprop
		cpp/pea
		cpp/peaUploader
		cpp/rtklib
		
		${EIGEN3_INCLUDE_DIRS}
		${LAPACK_INCLUDE_DIRS}
		${OPENBLAS_INCLUDE_DIRS}
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${LIBMONGOCXX_INCLUDE_DIR}
		${LIBBSONCXX_INCLUDE_DIR}
		"/usr/local/include/mongocxx/v_noabi"
		"/usr/local/include/bsoncxx/v_noabi"
		"/usr/local/include/libmongoc-1.0"
		"/usr/local/include/libbson-1.0"
		"/usr/local/lib"
		)		
		
add_executable(peaUploader
		cpp/3rdparty/enum.h
		cpp/common/acsConfig.hpp
		cpp/common/acsConfig.cpp
		cpp/common/acsStream.cpp
		cpp/common/acsStream.hpp
		cpp/common/common.hpp
		cpp/common/common.cpp
		cpp/common/constants.hpp
		cpp/common/constants.cpp
		cpp/common/fileLog.cpp
		cpp/common/fileLog.hpp
		cpp/common/gTime.cpp
		cpp/common/gTime.hpp
		cpp/common/mongo.cpp
		cpp/common/mongo.hpp
		cpp/common/mongoRead.cpp
		cpp/common/mongoRead.hpp
		cpp/common/ntripSocket.cpp
		cpp/common/ntripSocket.hpp
		cpp/common/ntripTrace.cpp
		cpp/common/ntripTrace.hpp
		cpp/common/rtcmEncoder.cpp
		cpp/common/rtcmEncoder.hpp
		cpp/common/satSys.cpp
		cpp/common/satSys.hpp
		cpp/common/streamTrace.cpp
		cpp/common/streamTrace.hpp
		
		cpp/common/streamNtrip.cpp
		cpp/common/streamNtrip.hpp
		
		cpp/common/rtcmDecoder.cpp
		cpp/common/rtcmDecoder.hpp
		cpp/common/ntripBroadcast.cpp
		cpp/common/ntripBroadcast.hpp
		
		cpp/peaUploader/uploadMain.cpp
		)

target_include_directories(peaUploader PUBLIC
		cpp/pea
		cpp/peaUploader
		cpp/common
		cpp/orbprop
		cpp/3rdparty
		cpp/rtklib
		${EIGEN3_INCLUDE_DIRS}
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${LIBMONGOCXX_INCLUDE_DIR}
		${LIBBSONCXX_INCLUDE_DIR}
		"/usr/local/include/mongocxx/v_noabi"
		"/usr/local/include/bsoncxx/v_noabi"
		"/usr/local/include/libmongoc-1.0"
		"/usr/local/include/libbson-1.0"
		"/usr/local/lib"
		)


add_executable(brdc2sp3
		cpp/brdc2sp3/brdc2sp3_main.cpp
		cpp/common/algebra_old.cpp
		cpp/common/algebra.hpp
		cpp/common/antenna.hpp
		cpp/common/antenna.cpp
		cpp/common/common.hpp
		cpp/common/common.cpp	
		cpp/common/constants.hpp
		cpp/common/constants.cpp
		cpp/common/gTime.cpp
		cpp/common/gTime.hpp
		cpp/common/navigation.hpp
		cpp/common/satSys.cpp
		cpp/common/satSys.hpp
		cpp/common/streamTrace.cpp
		cpp/common/streamTrace.hpp
		cpp/3rdparty/sofa/sofa.cpp		#todo aaron get rid of these
		cpp/rtklib/rinex.cpp
		cpp/rtklib/rinex.hpp
		cpp/rtklib/rtkcmn.cpp
		cpp/rtklib/tides.hpp
		)

target_include_directories(brdc2sp3 PUBLIC
		cpp/pea
		cpp/common
		cpp/3rdparty
		cpp/3rdparty/sofa				#todo aaron get rid of these
		cpp/orbprop
		cpp/iono
		cpp/ambres
		cpp/rtklib
		${EIGEN3_INCLUDE_DIRS}
		${LAPACK_INCLUDE_DIRS}
		${OPENBLAS_INCLUDE_DIRS}
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${LIBMONGOCXX_INCLUDE_DIR}
		${LIBBSONCXX_INCLUDE_DIR}
		"/usr/local/include/bsoncxx/v_noabi"
		"/usr/local/include/libmongoc-1.0"
		"/usr/local/include/libbson-1.0"
		"/usr/local/lib"
		)

if(OpenMP_CXX_FOUND)
	target_link_libraries(pea PUBLIC OpenMP::OpenMP_CXX)
endif()

target_compile_definitions(pea PRIVATE
							EIGEN_USE_BLAS=1
						)

target_compile_definitions(peaUploader PRIVATE
							DEBUGLOM
						)

target_compile_definitions(brdc2sp3 PRIVATE
							EIGEN_USE_BLAS=1
						)

#==================================================
# Ocean tide loading

set (NETCDF_CXX "YES")
find_package (NetCDF REQUIRED)

# message(STATUS "...NETCDF     >>>>>>   ${NETCDF_LIBRARIES}     ${NETCDF_INCLUDES}" )
# message(STATUS "...NETCDF_C++ >>>>>>   ${NETCDF_LIBRARIES_CXX} ${NETCDF_INCLUDES_CXX}" )
# find_package(netCDFCxx REQUIRED)

add_library(otl
		STATIC
		cpp/loading/loading.cpp
		cpp/loading/tide.cpp
		cpp/loading/utils.cpp
		cpp/loading/load_functions.cpp
		cpp/loading/loadgrid.cpp
		)

add_executable(make_otl_blq
		cpp/loading/make_otl_blq.cpp
		)


add_executable(interpolate_loading
		cpp/loading/interpolate_loading.cpp
		)

target_include_directories(otl PUBLIC
		cpp/common
		cpp/loading
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${NETCDF_INCLUDES}
		${NETCDF_INCLUDES_CXX}
		)

target_include_directories(make_otl_blq PUBLIC
		cpp/loading
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${NETCDF_INCLUDES}
		${NETCDF_INCLUDES_CXX}
		)

target_include_directories(interpolate_loading PUBLIC
		cpp/loading
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${NETCDF_INCLUDES}
		${NETCDF_INCLUDES_CXX}
		)


target_link_libraries(make_otl_blq PUBLIC
		otl
		${NETCDF_LIBRARIES_CXX}
		${NETCDF_LIBRARIES}
		Boost::timer
		Boost::program_options
		Boost::log
		Boost::log_setup
		${YAML_CPP_LIBRARIES}
		${YAML_CPP_LIB}
		)

target_link_libraries(interpolate_loading PUBLIC
		otl
		${NETCDF_LIBRARIES_CXX}
		${NETCDF_LIBRARIES}
		Boost::timer
		Boost::program_options
		Boost::log
		Boost::log_setup
		${YAML_CPP_LIBRARIES}
		${YAML_CPP_LIB}
		)


if(OpenMP_CXX_FOUND)
	target_link_libraries(make_otl_blq			PUBLIC OpenMP::OpenMP_CXX)
	target_link_libraries(interpolate_loading	PUBLIC OpenMP::OpenMP_CXX)
endif()

if(ENABLE_UNIT_TESTS)
	target_compile_definitions(pea PRIVATE ENABLE_UNIT_TESTS=1)
endif()


if(ENABLE_MONGODB)
	target_compile_definitions(pea PRIVATE ENABLE_MONGODB=1)
	target_compile_definitions(peaUploader PRIVATE ENABLE_MONGODB=1)	
endif()


if(ENABLE_PARALLELISATION)
	target_compile_definitions(pea PRIVATE ENABLE_PARALLELISATION=1)
endif()


target_link_libraries(pea PUBLIC
						m
						pthread
						${Boost_LIBRARIES}
						${BLAS_LIBRARIES}
						${LAPACK_LIBRARIES}
						${YAML_CPP_LIBRARIES}
						${YAML_CPP_LIB}
						mongo::mongocxx_shared
						${BLAS_LIBRARY_DIRS}
						${OPENSSL_LIBRARY_DIRS}
						${OPENSSL_LIBRARIES}
					)

target_link_libraries(peaUploader PUBLIC
						m
						pthread
						${Boost_LIBRARIES}
						${YAML_CPP_LIBRARIES}
						${YAML_CPP_LIB}
						mongo::mongocxx_shared
						${OPENSSL_LIBRARY_DIRS}
						${OPENSSL_LIBRARIES}
					)

target_link_libraries(brdc2sp3 PUBLIC
						m
						pthread
						${Boost_LIBRARIES}
						${BLAS_LIBRARIES}
						${LAPACK_LIBRARIES}
						${YAML_CPP_LIBRARIES}
						${YAML_CPP_LIB}
						mongo::mongocxx_shared
						${BLAS_LIBRARY_DIRS}
						${OPENSSL_LIBRARY_DIRS}
						${OPENSSL_LIBRARIES}
					)



# check if Doxygen is installed
if (BUILD_DOC)
find_package(Doxygen)
if (DOXYGEN_FOUND)

	# set input and output files
	set(DOXYGEN_IN      ${CMAKE_CURRENT_SOURCE_DIR}/doc_templates/Doxyfile.in)
	set(DOXYGEN_OUT     ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)
	set(DOXYGEN_LAYIN   ${CMAKE_CURRENT_SOURCE_DIR}/doc_templates/DoxygenLayout.xml.in)
	set(DOXYGEN_LAYOUT  ${CMAKE_CURRENT_BINARY_DIR}/DoxygenLayout.xml)

	# request to configure the file
	if (EXISTS "${DOXYGEN_IN}")
		configure_file(${DOXYGEN_IN}		${DOXYGEN_OUT}		@ONLY)
	endif()
	if (EXISTS "${DOXYGEN_LAYIN}")
		configure_file(${DOXYGEN_LAYIN}		${DOXYGEN_LAYOUT}	@ONLY)
	endif()

	# note the option ALL which allows to build the docs together with the application
	add_custom_target( doc_doxygen
	ALL
		COMMAND				${DOXYGEN_EXECUTABLE} ${DOXYGEN_OUT}
		WORKING_DIRECTORY	${CMAKE_CURRENT_SOURCE_DIR}/../docs
		COMMENT				"Generating API documentation with Doxygen"
		VERBATIM )

else (DOXYGEN_FOUND)

	message("Doxygen need to be installed to generate the doxygen documentation")

endif (DOXYGEN_FOUND)
endif()

list(APPEND CTEST_CUSTOM_WARNING_EXCEPTION
".*warning: ignoring loop annotation.*"
".*warning: .*Consider increasing DOT_GRAPH_MAX_NODES."
)

if(ENABLE_MONGODB)
	target_compile_definitions(pea PRIVATE ENABLE_MONGODB=1)
	target_compile_definitions(peaUploader PRIVATE ENABLE_MONGODB=1)
endif()


if(ENABLE_PARALLELISATION)
	target_compile_definitions(pea PRIVATE ENABLE_PARALLELISATION=1)
endif()

link_directories(/usr/lib64 ${CMAKE_BINARY_DIR}/../lib)

target_include_directories(pod PUBLIC
	.
		fortran/
		${LAPACK_INCLUDE_DIRS}
		${BLAS_INCLUDE_DIRS})

#must get all the modules built first to build all the other libraries - so make everything depend on pod_lib & yaml ...
target_link_libraries(rw_param 	pod_lib yaml)
target_link_libraries(force 	pod_lib yaml)
target_link_libraries(eop 		pod_lib yaml)
target_link_libraries(yaw 		pod_lib yaml)
target_link_libraries(tides 	pod_lib yaml)
target_link_libraries(ga_tz 	pod_lib yaml)
target_link_libraries(rk 		pod_lib yaml)
target_link_libraries(ga_math 	pod_lib yaml)
target_link_libraries(iau_math 	pod_lib yaml)
target_link_libraries(ga_gen 	pod_lib yaml)
target_link_libraries(boxwing 	pod_lib yaml)
target_link_libraries(kepler 	pod_lib yaml)
target_link_libraries(rel 		pod_lib yaml)
target_link_libraries(eclipse 	pod_lib yaml)
target_link_libraries(pod_lib 			yaml)

# must have pod_lib first to get all the modules. As rw_param requires pod_lib it must be listed again after that. 
# rk and force depend on eop and boxwing and must precede them in the list, likewise yaw must precede eclipse
target_link_libraries(pod 			pod_lib rw_param pod_lib rel kepler yaw eclipse tides ga_tz rk force eop boxwing ga_math ga_gen iers iau_math yaml ${LAPACK_LIBRARIES} ${BLAS_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(crs2trs 		pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math ga_gen iers iau_math yaml 	${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(brdc2ecef 	pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math ga_gen iers iau_math yaml 	${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(timesystem 	pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math ga_gen iers iau_math 	${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(test_yaml                                                                     ga_gen 	iau_math yaml iau_math	${CMAKE_THREAD_LIBS_INIT}) 

set_target_properties(pod 			PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(crs2trs 		PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(brdc2ecef 	PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(timesystem 	PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(test_yaml 	PROPERTIES LINKER_LANGUAGE Fortran)

if (OpenMP_Fortran_FOUND)
	target_link_libraries(pod PUBLIC OpenMP::OpenMP_Fortran)
endif()

target_compile_definitions(pod 			PRIVATE LAPACK BLAS)
target_compile_definitions(crs2trs 		PRIVATE)
target_compile_definitions(brdc2ecef 	PRIVATE)
target_compile_definitions(timesystem 	PRIVATE)
target_compile_definitions(test_yaml 	PRIVATE)


add_custom_target(peas)
add_dependencies(peas 
					pea
					peaUploader 
					brdc2sp3)

	
message(STATUS "CMAKE_CURRENT_SOURCE_DIR: ${CMAKE_CURRENT_SOURCE_DIR}")
message(STATUS "CMAKE_CXX_FLAGS:         ${CMAKE_CXX_FLAGS}")
message(STATUS "CMAKE_Fortran_FLAGS:     ${CMAKE_Fortran_FLAGS}")

# print_all_variables()
