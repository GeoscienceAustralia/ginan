cmake_minimum_required(VERSION 2.8)
cmake_policy(SET CMP0063 NEW)

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
message("Fortran default extensions = ${CMake_Fortran_SOURCE_FILE_EXTENSIONS}")
if ( ${CMAKE_SYSTEM_NAME} MATCHES "Darwin") 
	message("found DARWIN")
endif ()

set (CMAKE_Fortran_SOURCE_FILE_EXTENSIONS "f03;F90;f90;for;f;F")
set (CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../lib")
set (CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../lib")
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/../../bin")
#this bit doesn't work on DARWIN (MAC)
#have to modify cmake configure file directly
file(GLOB_RECURSE sources fortran/*.f90 fortran/*.F90 fortran/*.f03 fortran/*.f fortran/*.for fortran/*.F)
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

set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_MODULE_PATH                           "${CMAKE_SOURCE_DIR}/cmake")
set(CMAKE_CXX_VISIBILITY_PRESET         hidden)
set(CMAKE_VERBOSE_MAKEFILE                      ON)
set(CMAKE_CXX_FLAGS                             "${CMAKE_CXX_FLAGS} -std=c++1z -ggdb3 -fpermissive -Wno-write-strings -Wno-deprecated-declarations -Wno-format-overflow -Wno-switch -Wno-dangling-else")
#set(CMAKE_CXX_FLAGS                             "${CMAKE_CXX_FLAGS} -std=c++1z -ggdb3 -fpermissive -Wno-write-strings -Wno-deprecated-declarations -Wno-format-overflow")
set (CMAKE_Fortran_FLAGS "-g -frecursive -fall-intrinsics -fcheck=all -fbacktrace")
#set (CMAKE_Fortran_FLAGS "-g -frecursive -fall-intrinsics -Wmaybe-uninitialized -Wunused-dummy-argument -fcheck=all -fbacktrace")

# Uncomment these for Linux (e.g. CentOS 7) where boost 1.69.0 is packaged as boost169 (and boost is 1.53.0 for e.g.)
#set(BOOST_INCLUDEDIR /data/work/lib/boost-1.70.0/include)
#set(BOOST_LIBRARYDIR /data/work/lib/boost-1.70.0/lib)
set(Boost_NO_SYSTEM_PATHS ON)
set(Boost_USE_STATIC_LIBS ON)

option(ENABLE_UNIT_TESTS        "ENABLE_UNIT_TESTS"         OFF)
option(ENABLE_MONGODB           "ENABLE_MONGODB"            OFF)
option(ENABLE_PARALLELISATION   "ENABLE_PARALLELISATION"    ON)
option(ENABLE_OPTIMISATION      "ENABLE_OPTIMISATION"       OFF)

if(ENABLE_OPTIMISATION)
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O3")
	set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -O3")
endif()

#set debug if nothing else is set.
set(default_build_type "Debug")
if (NOT CMAKE_BUILD_TYPE)
	set(CMAKE_BUILD_TYPE ${default_build_type})
endif()

set(CMAKE_THREAD_PREFER_PTHREAD TRUE)
find_package(Threads REQUIRED)
if (CMAKE_USE_PTHREADS_INIT)
	set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -pthread")
	set (CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -pthread")
endif()

set(YAML_CPP_USE_STATIC_LIBS ON)
find_package(YAML_CPP 0.6.2 REQUIRED)

find_package(OpenSSL REQUIRED)

find_package(Boost 1.69.0 REQUIRED COMPONENTS log log_setup date_time filesystem system thread program_options serialization timer)

#find_package(OPENBLAS REQUIRED)

find_package(Eigen3 3.3.0)
include_directories(${EIGEN3_INCLUDE_DIRS})

find_package(OpenMP)

if(ENABLE_MONGODB)
	find_package(libmongocxx REQUIRED)
	find_package(libbsoncxx REQUIRED)
endif()

set(OPENBLAS_USE_STATIC_LIBS ON)
set (BLA_VENDOR OpenBLAS)
find_package(BLAS)
if(BLAS_FOUND)
set(LAPACK_LIBRARIES "")
message(STATUS "Found Vendor specific OpenBLAS")
else()
set (BLA_VENDOR "")
find_package(LAPACK required)
find_package(BLAS required)  
message(STATUS "Found LAPACK and BLAS")
endif()

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

	MESSAGE( STATUS "Git version: ${GINAN_COMMIT_VERSION}")
	MESSAGE( STATUS "Git branch: ${GINAN_BRANCH_NAME}")
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
fortran/mdl_param.f03
fortran/mdl_precision.f90 
fortran/mdl_arr.f90 
fortran/mdl_num.f90 
fortran/mdl_config.f03 
fortran/mdl_eop.f90 
fortran/mdl_planets.f90 
fortran/mdl_tides.f90 
fortran/m_matrixRxR.f03 
fortran/m_matrixinv.f03 
fortran/m_matrixreverse.f03 
fortran/m_matrixmerge.f03 
fortran/m_integrEQM.f03 
fortran/m_force_tides.f03 
fortran/m_tides_ocean.f03 
fortran/m_pd_geopotential.f03 
fortran/m_legendre.f03
fortran/m_legendre1.f03
fortran/m_legendre2.f03
fortran/m_force_gfm.f03 
fortran/pd_forceZ.f03 
fortran/m_get_lambda.f 
fortran/m_shadow.f90 
fortran/m_pd_empirical.f03
fortran/m_emp_init.f03
fortran/m_pd_ECOM.f90 
fortran/m_pulses_init.f03
fortran/m_pd_pulses.f03
fortran/m_pulses_force.f03
fortran/m_satinfo.f90 
fortran/m_pd_force.f03 
fortran/m_veq_rkn768.f03 
fortran/m_veqC2T.f03
fortran/m_integrVEQ.f03 
fortran/m_betainfo.f90 
fortran/m_orbinteg.f03 
fortran/m_orb_estimator.f03 
fortran/m_orbC2T.f03 
fortran/m_orbT2C.f03 
fortran/m_statist.f03 
fortran/m_statdelta.f03 
fortran/m_orbinfo.f90 
fortran/m_statorbit.f03 
fortran/m_statorbit2.f03 
fortran/m_writearray.f03 
fortran/m_writearray2.f03 
fortran/m_write_orbres.f03
fortran/m_orbresize.f03 
fortran/m_orbext.f03 
fortran/m_orbext2.f03 
fortran/m_gfc.f03 
fortran/m_gfc3.f03 
fortran/m_obsorbT2C.f03 
fortran/m_writedata.f03 
fortran/m_writeorbit.f03 
fortran/m_read_svsinex.f03
fortran/m_read_satsnx.f03
fortran/m_writeorbit_multi.f03 
fortran/m_write_prmfile_init.f03 
fortran/m_rso.f03 
fortran/m_sp3.f03 
fortran/m_sp3_PRN.f03 
fortran/m_lagrange.f03 
fortran/m_interporb.f03 
fortran/m_interporb_nom.f03
fortran/m_interporb_filt.f03
fortran/m_interpclocks.f03
fortran/m_interpclock_nom.f03
fortran/m_orb_outlier.f03
fortran/m_eop_data.f03 
fortran/m_keplerorb.f03 
fortran/m_ecom_init.f03
fortran/m_orbdet.f03 
fortran/m_orbitmain.f03 
fortran/m_orbitIC.f03
fortran/m_attitude_orb.f03
fortran/m_write_orbex.f03
fortran/m_satmetadata.f03
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
fortran/m_matrixRxR.f03 
fortran/m_matrixinv.f03 
fortran/m_matrixreverse.f03 
fortran/m_matrixmerge.f03 
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
fortran/doy2str.f03
fortran/f90getopt.F90 
fortran/read_cmdline.f03 
)

add_library(rw_param
fortran/write_prmfile_init0.f03 
fortran/writeparam.f03 
fortran/writeparam1.f03 
fortran/write_prmfile.f03 
fortran/write_prmfile2.f03 
fortran/readparam.f03  
fortran/prm_main.f03 
fortran/prm_emp.f03 
fortran/prm_srp.f03 
fortran/prm_grav.f03 
fortran/prm_gravity.f03 
fortran/prm_nongrav.f03 
fortran/prm_ocean.f03 
fortran/prm_orbext.f03 
fortran/prm_planets.f03 
fortran/prm_pseudobs.f03 
)

add_library(force
fortran/force_gm.f90 
fortran/force_gm3rd.f90 
fortran/force_srp.f90 
fortran/force_erp.f90 
fortran/force_ant.f90 
fortran/force_sum.f03 
fortran/apr_srp.f90
)

add_library(rel
fortran/rel_schwarzschild.f90 
fortran/rel_LenseThirring.f90 
fortran/rel_deSitter.f90 
)

add_library(rk
fortran/integr_rkn768.f03 
fortran/integr_rk87.f03 
fortran/integr_rk4.f03 
)

add_library(yaw
fortran/yaw_nom.f90
fortran/yaw_angle.f90 
fortran/yaw_attitude.f90 
fortran/yaw_bds.f90 
fortran/yaw_gal.f90 
fortran/yaw_gal_iov.f03
fortran/yaw_gal_foc.f03
fortran/yawdyn.f90
fortran/att_matrix.f03
fortran/mat2quater.f03
fortran/yaw_gal_foc_nom.f03
fortran/yaw_pred.f03
fortran/colinearity_angle.f03
fortran/colinear_pred.f03
)

add_library(eop
fortran/EOP.f90 
fortran/eop_rd.f90 
fortran/eop_c04.f90 
fortran/m_eop_cor.f03 
fortran/m_eop_igu.f03 
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
fortran/eclipse_integstep.f03 
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
fortran/gmst_iers.f03 
fortran/interp_iers.f 
fortran/IERS_CMP_2015.F 
fortran/m_read_leapsec.f03
fortran/m_clock_read.f03
fortran/m_write_orb2sp3.f03 
fortran/m_pod_gnss.f03 
fortran/gblinit.f03
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
fortran/prn_shift.f03 
fortran/surfprop.f90 
fortran/scan0orb.f03
fortran/empirical_init.f03 
fortran/empirical_cor.f03 
fortran/beta_angle.f90 
fortran/beta_pred.f90 
fortran/attitude.f03 
fortran/pd_gm.f03 
fortran/pd_forceZ.f03 
fortran/main_pod.f03
)

add_executable(crs2trs
fortran/report.f90
fortran/coord_r2sph.f90 
fortran/interp_iers.f 
fortran/crs_trs.f90 
fortran/main_crs2trs.f03
)

add_executable(brdc2ecef
fortran/report.f90
fortran/m_read_leapsec.f03
fortran/asc2eph.f90
fortran/brdc2ecef.f90
fortran/brdc_cmdline.f03
fortran/brdc_qc_gal.f90
fortran/brdc_qc_gps.f90
fortran/CATfile.f90
fortran/chkbrdc.f90
fortran/crs_trs.f90 
fortran/reformbrdc.f90
fortran/glnacc.f90
fortran/glnorbint.f90
fortran/m_antoffset.f90
fortran/mdl_brdconfig.f03
fortran/m_meanstd.f90
fortran/m_write_brd2sp3.f03
fortran/m_write_orb2sp3.f03
fortran/prn2str.f03
fortran/prn_shift_brdc.f03
fortran/readbrdcheader.f03
fortran/readbrdcmessg.f90
fortran/main_brdcorbit.f90
)

add_executable(timesystem
fortran/mdl_timeconfig.f03
fortran/time_cmdline.f03
fortran/main_time.f03
)

add_executable(test_yaml
fortran/test_yaml.F90
)

add_executable(pea
		cpp/3rdparty/enum.h
		cpp/common/acsConfig.hpp
		cpp/common/acsConfig.cpp
		cpp/common/acsQC.hpp
		cpp/common/acsQC.cpp
		cpp/common/acsFileStream.hpp
		cpp/common/acsNtripStream.cpp
		cpp/common/acsNtripStream.hpp
		cpp/common/acsRinexStream.hpp
		cpp/common/acsRtcmStream.hpp
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
		cpp/common/constants.h
		cpp/common/constants.cpp
		cpp/common/corrections.cpp
		cpp/common/debug.cpp
		cpp/common/debug.hpp
		cpp/common/eigenIncluder.hpp
		cpp/common/enums.h
		cpp/common/gTime.cpp
		cpp/common/gTime.hpp
		cpp/common/navigation.hpp
		cpp/common/orbits.cpp
		cpp/common/orbits.hpp
		cpp/common/observations.hpp
		cpp/common/linearCombo.cpp
		cpp/common/linearCombo.hpp
		cpp/common/mongo.cpp
		cpp/common/mongo.hpp
		cpp/common/rtsSmoothing.cpp
		cpp/common/satStat.hpp
		cpp/common/summary.hpp
		cpp/common/satSys.cpp
		cpp/common/satSys.hpp
		cpp/common/snx.hpp
		cpp/common/snx.cpp
		cpp/common/sp3.cpp
		cpp/common/station.hpp
		cpp/common/streamTrace.cpp
		cpp/common/streamTrace.hpp
		cpp/common/testUtils.cpp
		cpp/common/testUtils.hpp
		cpp/common/writeClock.cpp
		cpp/common/writeClock.hpp
		cpp/common/acsNtripBroadcast.cpp
		cpp/common/acsNtripBroadcast.hpp
		cpp/common/acsNtripServer.cpp
		cpp/common/acsNtripServer.hpp
		cpp/common/ntripCasterService.cpp
		cpp/common/ntripCasterService.hpp
		cpp/common/ntripSocket.cpp
		cpp/common/ntripSocket.hpp
		cpp/common/ntripTrace.cpp
		cpp/common/ntripTrace.hpp
		cpp/common/ntripSourceTable.cpp
		cpp/common/ntripSourceTable.hpp
		cpp/common/rtcmEncoder.cpp
		cpp/common/rtcmEncoder.hpp

		cpp/iono/ionoMeas.cpp
		cpp/iono/ionoModel.cpp
		cpp/iono/ionoModel.hpp
		cpp/iono/ionoSpherical.cpp
		cpp/iono/ionoSphericalCaps.cpp
		cpp/iono/ionoBSplines.cpp
		cpp/iono/ionexWrite.cpp

		cpp/ambres/GNSSambres.hpp
		cpp/ambres/GNSSambres.cpp
		cpp/ambres/GNSSpivot.cpp
		cpp/ambres/WLambest.cpp
		cpp/ambres/NLambest.cpp
		cpp/ambres/ARmain.cpp
		cpp/ambres/ARoutput.cpp
		
		cpp/pea/cycleSlip.cpp
		cpp/pea/cycleSlip.hpp
		cpp/pea/main.cpp
		cpp/pea/minimumConstraints.cpp
		cpp/pea/minimumConstraints.hpp
		cpp/pea/networkEstimator.cpp
		cpp/pea/networkEstimator.hpp
		cpp/pea/pea_snx.cpp
		cpp/pea/ppp.cpp
		cpp/pea/ppp_user.cpp
		cpp/pea/ppp_network.cpp
		cpp/pea/ppp.hpp
		cpp/pea/spp.cpp

		cpp/rtklib/ionex.cpp
		cpp/rtklib/lambda.cpp
		cpp/rtklib/lambda.h
		cpp/rtklib/ephemeris.cpp
		cpp/rtklib/preceph.cpp
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
		cpp/pea
		cpp/common
		cpp/3rdparty
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
		"/usr/local/include/mongocxx/v_noabi"
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
							DEBUGLOM
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
	target_link_libraries(make_otl_blq PUBLIC OpenMP::OpenMP_CXX)
	target_link_libraries(interpolate_loading PUBLIC OpenMP::OpenMP_CXX)
endif()


#=================================================
					#add_executable(test_sinex2
					#        cpp/3rdparty/enum.h
					#        cpp/test/sinex/test_sinex2.cpp
					#        cpp/test/include/minunit.h
					#        cpp/common/snx.cpp
					#        cpp/common/gaTime.cpp
					#        cpp/common/gTime.hpp
					#        cpp/common/constants.h
					#        cpp/common/constants.cpp
					#)
#
#target_include_directories(test_sinex2 PUBLIC
#	cpp/pea
#	cpp/3rdparty
#    cpp/common
#	cpp/rtklib
#	cpp/test/include
#	cpp/test/sinex
#        )
#==============================================================

if(ENABLE_UNIT_TESTS)
	target_compile_definitions(pea PRIVATE ENABLE_UNIT_TESTS=1)
endif()


if(ENABLE_MONGODB)
	target_compile_definitions(pea PRIVATE ENABLE_MONGODB=1)
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
						${LIBMONGOCXX_LIBRARIES}
						${LIBBSONCXX_LIBRARIES}
			${BLAS_LIBRARY_DIRS}
						${OPENSSL_LIBRARY_DIRS}
						ssl
						crypto
					)

				#target_link_libraries(test_sinex2 PUBLIC
				#                        m
				#                        pthread
				#                        ${Boost_LIBRARIES}
				#                        ${OpenBLAS_LIB}
				#                        ${LAPACK_LIBRARIES}
				#                        ${YAML_CPP_LIBRARIES}
				#                        ${YAML_CPP_LIB} )


# first we can indicate the documentation build as an option and set it to ON by default
option(BUILD_DOC "Build documentation" ON)

# check if Doxygen is installed
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

list(APPEND CTEST_CUSTOM_WARNING_EXCEPTION
".*warning: ignoring loop annotation.*"
".*warning: .*Consider increasing DOT_GRAPH_MAX_NODES."
)

if(ENABLE_MONGODB)
	target_compile_definitions(pea PRIVATE ENABLE_MONGODB=1)
endif()


if(ENABLE_PARALLELISATION)
	target_compile_definitions(pea PRIVATE ENABLE_PARALLELISATION=1)
endif()

link_directories(/usr/lib64 ${CMAKE_BINARY_DIR}/../../build/lib)

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
target_link_libraries(timesystem 	pod_lib rw_param pod_lib ga_tz rk force eop boxwing ga_math ga_gen iers iau_math 		${CMAKE_THREAD_LIBS_INIT})
target_link_libraries(test_yaml 																ga_gen 				yaml	${CMAKE_THREAD_LIBS_INIT}) 

set_target_properties(pod 			PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(crs2trs 		PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(brdc2ecef 	PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(timesystem 	PROPERTIES LINKER_LANGUAGE Fortran)
set_target_properties(test_yaml 	PROPERTIES LINKER_LANGUAGE Fortran)

target_compile_definitions(pod 			PRIVATE LAPACK BLAS)
target_compile_definitions(crs2trs 		PRIVATE)
target_compile_definitions(brdc2ecef 	PRIVATE)
target_compile_definitions(timesystem 	PRIVATE)
target_compile_definitions(test_yaml 	PRIVATE)

#print_all_variables()
