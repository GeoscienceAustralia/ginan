
add_executable(pea
		3rdparty/enum.h
		
		3rdparty/jpl/jpl_eph.cpp
		3rdparty/jpl/jpl_eph.hpp
		3rdparty/jpl/jpl_int.hpp	

		3rdparty/sofa/sofa.cpp
		3rdparty/sofa/sofa.hpp
		3rdparty/sofa/sofam.hpp

		3rdparty/egm96/EGM96.c
		3rdparty/egm96/EGM96.h
		3rdparty/egm96/EGM96_data.h
		
		common/acsConfig.hpp
		common/acsConfig.cpp
		common/ntripBroadcast.cpp
		common/ntripBroadcast.hpp
		common/acsQC.hpp
		common/acsQC.cpp
		common/acsStream.cpp
		common/acsStream.hpp
		common/algebra.cpp
		common/algebra_old.cpp
		common/algebra.hpp
		common/algebraTrace.cpp
		common/algebraTrace.hpp
		common/antenna.hpp
		common/antenna.cpp
		common/biasSINEX.hpp
		common/biasSINEXread.cpp
		common/biasSINEXwrite.cpp
		common/common.hpp
		common/common.cpp
		common/constants.hpp
		common/constants.cpp
		common/corrections.cpp
		common/debug.cpp
		common/debug.hpp
		common/eigenIncluder.hpp
		common/ephemeris.cpp
		common/ephemeris.hpp
		common/enums.h
		common/erp.cpp
		common/erp.hpp
		common/fileLog.cpp
		common/fileLog.hpp
		common/gpx.cpp
		common/gpx.hpp
		common/gTime.cpp
		common/gTime.hpp
		common/instrument.cpp
		common/instrument.hpp
		common/linearCombo.cpp
		common/linearCombo.hpp
		common/mongo.cpp
		common/mongo.hpp
		common/mongoRead.cpp
		common/mongoRead.hpp
		common/mongoWrite.cpp
		common/mongoWrite.hpp
		common/navigation.hpp
		common/observations.hpp
		common/ntripSocket.cpp
		common/ntripSocket.hpp
		common/ntripTrace.cpp
		common/ntripTrace.hpp
		common/ntripSourceTable.cpp
		common/ntripSourceTable.hpp
		common/orbits.cpp
		common/orbits.hpp
		common/preceph.cpp
		common/rtsSmoothing.cpp
		common/rtcmDecoder.cpp
		common/rtcmDecoder.hpp
		common/rtcmEncoder.cpp
		common/rtcmEncoder.hpp
		common/satStat.hpp
		common/summary.cpp
		common/summary.hpp
		common/satSys.cpp
		common/satSys.hpp
		common/sinex.hpp
		common/sinex.cpp
		common/tropSinex.cpp
		common/sp3.cpp
		common/sp3Write.cpp
		common/sp3Write.hpp		
		common/ssr.hpp
		common/station.hpp
		common/streamTrace.cpp
		common/streamTrace.hpp
		common/testUtils.cpp
		common/testUtils.hpp
		common/trigPosInterp.cpp
		common/rinexClkWrite.cpp
		common/rinexClkWrite.hpp
		common/rinexNavWrite.cpp
		common/rinexNavWrite.hpp
		common/rinexObsWrite.cpp
		common/rinexObsWrite.hpp

		common/streamFile.hpp
		common/streamNav.hpp
		common/streamNtrip.cpp
		common/streamNtrip.hpp
		common/streamObs.hpp
		common/streamRinex.hpp
		common/streamRtcm.hpp
		common/streamSp3.hpp
		
		iono/ionoMeas.cpp
		iono/ionoModel.cpp
		iono/ionoModel.hpp
		iono/ionoSpherical.cpp
		iono/ionoSphericalCaps.cpp
		iono/ionoBSplines.cpp
		iono/ionexWrite.cpp

		ambres/GNSSambres.hpp
		ambres/GNSSambres.cpp
		ambres/GinARpivot.cpp
		ambres/GinARWLamb.cpp
		ambres/GinARNLamb.cpp
		ambres/GinAR_main.cpp
		ambres/GinARoutpt.cpp
		
		orbprop/forceModels.cpp
		orbprop/forceModels.hpp
		orbprop/gravity.cpp
		orbprop/gravity.hpp
		orbprop/jplEph.cpp
		orbprop/jplEph.hpp
		orbprop/forceModels.cpp
		orbprop/forceModels.hpp
		orbprop/satRefSys.hpp
		
		pea/main.cpp
		pea/minimumConstraints.cpp
		pea/minimumConstraints.hpp
		pea/networkEstimator.cpp
		pea/networkEstimator.hpp
		pea/pea_snx.cpp
		pea/ppp.cpp
		pea/ppppp.cpp
		pea/ppppp_obs.cpp
		pea/ppppp_ambres.cpp
		pea/ppppp_pseudoobs.cpp
		pea/ppp_user.cpp
		pea/ppp_network.cpp
		pea/ppp.hpp
		pea/preprocessor.cpp
		pea/spp.cpp

		rtklib/ionex.cpp
		rtklib/lambda.cpp
		rtklib/lambda.h
		rtklib/readtrop.cpp
		rtklib/rinex.cpp
		rtklib/rinex.hpp
		rtklib/rtkcmn.cpp
		rtklib/tides.cpp
		rtklib/tides.hpp
		rtklib/trop.h
		rtklib/trop.cpp
		rtklib/vmf3.h
		rtklib/vmf3.cpp
		rtklib/wancorr.cpp
		rtklib/wancorr.h
		)

target_include_directories(pea PUBLIC
		3rdparty
		3rdparty/egm96
		3rdparty/sofa
		3rdparty/jpl
		
		configurator
		
		ambres
		common
		iono
		orbprop
		pea
		peaUploader
		rtklib
		
		${EIGEN3_INCLUDE_DIRS}
		${LAPACK_INCLUDE_DIRS}
		${OPENBLAS_INCLUDE_DIRS}
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${LIBMONGOCXX_INCLUDE_DIR}
		${LIBBSONCXX_INCLUDE_DIR}
		${OPENSSL_INCLUDE_DIR}
		"/usr/local/include/mongocxx/v_noabi"
		"/usr/local/include/bsoncxx/v_noabi"
		"/usr/local/include/libmongoc-1.0"
		"/usr/local/include/libbson-1.0"
		"/usr/local/lib"
		)		

add_executable(brdc2sp3
		brdc2sp3/brdc2sp3_main.cpp
		common/acsConfig.hpp
		common/acsConfig.cpp
		common/algebra_old.cpp
		common/algebra.hpp
		common/antenna.hpp
		common/antenna.cpp
		common/biasSINEX.hpp
		common/biasSINEXread.cpp
		common/common.hpp
		common/common.cpp	
		common/constants.hpp
		common/constants.cpp
		common/gTime.cpp
		common/gTime.hpp
		common/navigation.hpp
		common/satSys.cpp
		common/satSys.hpp
		common/streamTrace.cpp
		common/streamTrace.hpp
		3rdparty/sofa/sofa.cpp		#todo aaron get rid of these
		rtklib/rinex.cpp
		rtklib/rinex.hpp
		rtklib/rtkcmn.cpp
		rtklib/tides.hpp
		)

target_include_directories(brdc2sp3 PUBLIC
		pea
		configurator
		common
		3rdparty
		3rdparty/sofa				#todo aaron get rid of these
		orbprop
		iono
		ambres
		rtklib
		${EIGEN3_INCLUDE_DIRS}
		${LAPACK_INCLUDE_DIRS}
		${OPENBLAS_INCLUDE_DIRS}
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${LIBMONGOCXX_INCLUDE_DIR}
		${LIBBSONCXX_INCLUDE_DIR}
		${OPENSSL_INCLUDE_DIR}
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

target_compile_definitions(brdc2sp3 PRIVATE
							EIGEN_USE_BLAS=1
						)

#==================================================
# Ocean tide loading

add_library(otl
		STATIC
		loading/loading.cpp
		loading/tide.cpp
		loading/utils.cpp
		loading/load_functions.cpp
		loading/loadgrid.cpp
		)

add_executable(make_otl_blq
		loading/make_otl_blq.cpp
		)


add_executable(interpolate_loading
		loading/interpolate_loading.cpp
		)

target_include_directories(otl PUBLIC
		common
		loading
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${NETCDF_INCLUDES}
		${NETCDF_INCLUDES_CXX}
		)

target_include_directories(make_otl_blq PUBLIC
		loading
		${YAML_INCLUDE_DIRS}
		${Boost_INCLUDE_DIRS}
		${NETCDF_INCLUDES}
		${NETCDF_INCLUDES_CXX}
		)

target_include_directories(interpolate_loading PUBLIC
		loading
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

if(ENABLE_PARALLELISATION)
	target_compile_definitions(pea PRIVATE ENABLE_PARALLELISATION=1)
endif()

add_custom_target(peas)

add_dependencies(peas 
					pea
					brdc2sp3
					)

if(BUILD_DOC)
add_dependencies(peas
					docs
					)
endif()
					
					
