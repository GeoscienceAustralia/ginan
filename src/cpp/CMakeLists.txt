
#set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "time -v")

add_subdirectory(3rdparty/sofa)

add_library(ginan_core

		common/acsConfig.hpp
		common/acsConfig.cpp
		common/acsConfigDocs.cpp

		3rdparty/enum.h
		3rdparty/EigenDenseBaseAddons.h

		3rdparty/jpl/jpl_eph.cpp
		3rdparty/jpl/jpl_eph.hpp
		3rdparty/jpl/jpl_int.hpp

		3rdparty/egm96/EGM96.c
		3rdparty/egm96/EGM96.h
		3rdparty/egm96/EGM96_data.h

		3rdparty/slr/read_crd.c

		3rdparty/iers2010/ch9/fcul_a.cpp
		3rdparty/iers2010/ch9/fcul_zd_hpa.cpp
		3rdparty/iers2010/hardisp/admint.cpp
		3rdparty/iers2010/hardisp/eval.cpp
		3rdparty/iers2010/hardisp/hardisp_impl.cpp
		3rdparty/iers2010/hardisp/recurs.cpp
		3rdparty/iers2010/hardisp/shells.cpp
		3rdparty/iers2010/hardisp/spline.cpp
		3rdparty/iers2010/hardisp/tdfrph.cpp
		3rdparty/iers2010/dehanttideinel/dehanttide_all.cpp

		pea/main.cpp
		pea/minimumConstraints.cpp
		pea/minimumConstraints.hpp
		pea/peaCommitStrings.cpp
		pea/pea_snx.cpp
		pea/ppp.cpp
		pea/ppppp.cpp
		pea/ppp_obs.cpp
		pea/ppp_ambres.cpp
		pea/ppp_callbacks.cpp
		pea/ppp_pseudoobs.cpp
		pea/ppp_slr.cpp
		pea/ppp.hpp
		pea/preprocessor.cpp
		pea/spp.cpp

		common/api.hpp
		common/api.cpp
		common/ntripBroadcast.cpp
		common/ntripBroadcast.hpp
		common/acsQC.hpp
		common/acsQC.cpp
		common/algebra.cpp
		common/algebra_old.cpp
		common/algebra.hpp
		common/algebraTrace.cpp
		common/algebraTrace.hpp
		common/attitude.cpp
		common/compare.cpp
		common/compare.hpp
		common/antenna.hpp
		common/antenna.cpp
		common/biases.cpp
		common/biasSINEXread.cpp
		common/biasSINEXwrite.cpp
		common/binaryStore.cpp
		common/binaryStore.hpp
		common/common.hpp
		common/common.cpp
		common/constants.hpp
		common/constants.cpp
		common/customDecoder.cpp
		common/cost.hpp
		common/cost.cpp
		common/debug.cpp
		common/debug.hpp
		common/eigenIncluder.hpp
		common/ephemeris.cpp
		common/ephemeris.hpp
		common/ephBroadcast.cpp
		common/ephKalman.cpp
		common/ephPrecise.cpp
		common/ephSSR.cpp
		common/enums.h
		common/erp.cpp
		common/erp.hpp
		common/fileLog.cpp
		common/fileLog.hpp
		common/gpx.cpp
		common/gpx.hpp
		common/gTime.cpp
		common/gTime.hpp
		common/icdDecoder.hpp
		common/instrument.cpp
		common/instrument.hpp
		common/ionModels.cpp
		common/linearCombo.cpp
		common/linearCombo.hpp
		#common/mqtt.cpp
		common/navigation.hpp
		common/observations.hpp
		common/ntripSocket.cpp
		common/ntripSocket.hpp
		common/ntripTrace.cpp
		common/ntripTrace.hpp
		common/orbits.cpp
		common/orbits.hpp
		common/packetStatistics.hpp
		common/receiver.cpp
		common/receiver.cpp
		common/rinex.cpp
		common/rinex.hpp
		common/rtsSmoothing.cpp
		common/rtcmDecoder.cpp
		common/rtcmDecoder.hpp
		common/rtcmEncoder.cpp
		common/rtcmEncoder.hpp
		common/rtcmTrace.cpp
		common/rtcmTrace.hpp
		common/satStat.hpp
		common/summary.cpp
		common/summary.hpp
		common/satSys.cpp
		common/satSys.hpp
		common/sinex.hpp
		common/sinex.cpp
		common/sinexParser.cpp
		common/sinexParser.hpp
		common/tropSinex.cpp
		common/sp3.cpp
		common/sp3Write.cpp
		common/sp3Write.hpp
		common/orbex.cpp
		common/orbexWrite.hpp
		common/orbexWrite.cpp
		common/ssr.hpp
		common/trace.cpp
		common/trace.hpp
		common/testUtils.cpp
		common/testUtils.hpp
		common/rinexClkWrite.cpp
		common/rinexClkWrite.hpp
		common/rinexNavWrite.cpp
		common/rinexNavWrite.hpp
		common/rinexObsWrite.cpp
		common/rinexObsWrite.hpp
		common/tides.cpp
		common/tides.hpp
		common/ubxDecoder.cpp
		common/ubxDecoder.hpp
		common/walkthrough.cpp
		common/localAtmosRegion.cpp

		common/streamFile.hpp
		common/streamNtrip.cpp
		common/streamNtrip.hpp
		common/streamCustom.cpp
		common/streamCustom.hpp
		common/streamObs.hpp
		common/streamRinex.hpp
		common/streamRtcm.hpp
		common/streamSerial.cpp
		common/streamSerial.hpp
		common/streamSp3.hpp
		common/streamUbx.cpp
		common/streamUbx.hpp
		common/streamParser.cpp
		common/streamParser.hpp

		iono/geomagField.hpp
		iono/geomagField.cpp
		iono/ionex.cpp
		iono/ionoMeas.cpp
		iono/ionoModel.cpp
		iono/ionoModel.hpp
		iono/ionoSpherical.cpp
		iono/ionoSphericalCaps.cpp
		iono/ionoBSplines.cpp
		iono/ionexWrite.cpp
		iono/ionoLocalSTEC.cpp

		inertial/posProp.cpp

		trop/tropModels.hpp
		trop/tropModels.cpp
		trop/tropSAAS.cpp
		trop/tropSBAS.cpp
		trop/tropGPT2.cpp
		trop/tropVMF3.cpp
		trop/tropCSSR.cpp

		ambres/GNSSambres.hpp
		ambres/GNSSambres.cpp

		slr/slrCom.cpp
		slr/slrObs.cpp
		slr/slrSat.cpp
		slr/slrRec.cpp

		other_ssr/otherSSR.hpp
		other_ssr/prototypeIgsSSRDecode.cpp
		other_ssr/prototypeCmpSSREncode.cpp
		other_ssr/prototypeCmpSSRDecode.cpp
		other_ssr/prototypeIgsSSREncode.cpp

		orbprop/aod.cpp
		orbprop/boxwing.cpp
		orbprop/acceleration.cpp
		orbprop/coordinates.cpp
		orbprop/iers2010.cpp
		orbprop/planets.cpp
		orbprop/tideCoeff.cpp
		orbprop/orbitProp.cpp
		orbprop/staticField.cpp
		orbprop/centerMassCorrections.cpp
		orbprop/oceanPoleTide.cpp

		rtklib/lambda.cpp
		rtklib/lambda.h
		rtklib/rtkcmn.cpp

		common/mongo.cpp
		common/mongo.hpp
		common/mongoRead.cpp
		common/mongoRead.hpp
		common/mongoWrite.cpp
		common/mongoWrite.hpp
)

target_include_directories(ginan_core PUBLIC
		3rdparty
		3rdparty/egm96
		3rdparty/iers2010
		3rdparty/sofa/src
		3rdparty/jpl
		3rdparty/mqtt_cpp/
		3rdparty/slr

		configurator

		ambres
		slr
		common
		iono
		trop
		inertial
		orbprop
		pea
		peaUploader
		rtklib
		other_ssr

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

target_compile_options(ginan_core PRIVATE -fpie)

if(OpenMP_CXX_FOUND)
	target_link_libraries(ginan_core PUBLIC OpenMP::OpenMP_CXX)
endif()

target_compile_definitions(ginan_core PRIVATE
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

if(ENABLE_PARALLELISATION)
	target_compile_definitions(ginan_core PRIVATE ENABLE_PARALLELISATION=1)
endif()


target_link_libraries(ginan_core PUBLIC
						m
						pthread
						sofa_lib
						${Boost_LIBRARIES}
						${BLAS_LIBRARIES}
						${LAPACK_LIBRARIES}
						${YAML_CPP_LIBRARIES}
						${YAML_CPP_LIB}
						mongo::mongocxx_shared
						${BLAS_LIBRARY_DIRS}
						${OPENSSL_LIBRARY_DIRS}
						${OPENSSL_LIBRARIES}
						dl
						backtrace
					)


add_executable(ginan
	gui/ginan.cpp
	gui/anode.cpp
	gui/beast.cpp)

target_include_directories(ginan PUBLIC
	3rdparty/beast/
	)

target_link_libraries(ginan PRIVATE ginan_core)

add_executable(pea
	pea.cpp)

set_property(TARGET pea PROPERTY POSITION_INDEPENDENT_CODE FALSE)

target_include_directories(pea PUBLIC
	)

target_link_libraries(pea PRIVATE ginan_core)


add_custom_target(peas)

add_dependencies(peas
					pea
					ginan
					)


if(BUILD_DOC)
add_dependencies(peas
					docs
					)
endif()


