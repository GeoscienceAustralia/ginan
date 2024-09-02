
#set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE "time -v")

add_subdirectory(3rdparty/sofa)

add_executable(pea

		../Architecture/Ginan.cpp

		common/acsConfig.cpp
		common/acsConfigDocs.cpp

		3rdparty/jpl/jpl_eph.cpp

		3rdparty/egm96/EGM96.c

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
		pea/inputs.cpp
		pea/outputs.cpp
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
		pea/preprocessor.cpp
		pea/spp.cpp

		common/api.cpp
		common/ntripBroadcast.cpp
		common/acsQC.cpp
		common/algebra.cpp
		common/algebra_old.cpp
		common/algebraTrace.cpp
		common/attitude.cpp
		common/compare.cpp
		common/antenna.cpp
		common/biases.cpp
		common/biasSINEXread.cpp
		common/biasSINEXwrite.cpp
		common/common.cpp
		common/constants.cpp
		common/customDecoder.cpp
		common/cost.cpp
		common/debug.cpp
		common/ephemeris.cpp
		common/ephemeris.hpp
		common/ephBroadcast.cpp
		common/ephKalman.cpp
		common/ephPrecise.cpp
		common/ephSSR.cpp
		common/erp.cpp
		common/fileLog.cpp
		common/fileLog.hpp
		common/gpx.cpp
		common/pos.cpp
		common/gTime.cpp
		common/interactiveTerminal.cpp
		common/ionModels.cpp
		common/linearCombo.cpp
		common/mongo.cpp
		common/mongoRead.cpp
		common/mongoWrite.cpp

		common/ntripTrace.cpp
		common/orbits.cpp
		common/receiver.cpp
		common/receiver.cpp
		common/rinex.cpp
		common/rtsSmoothing.cpp
		common/rtcmDecoder.cpp
		common/rtcmEncoder.cpp
		common/rtcmTrace.cpp
		common/summary.cpp
		common/satSys.cpp
		common/sinex.cpp
		common/sinexParser.cpp
		common/tropSinex.cpp
		common/sp3.cpp
		common/sp3Write.cpp
		common/orbex.cpp
		common/orbexWrite.cpp
		common/tcpSocket.cpp
		common/tcpSocket.cpp
		common/trace.cpp
		common/testUtils.cpp
		common/rinexClkWrite.cpp
		common/rinexNavWrite.cpp
		common/rinexObsWrite.cpp
		common/tides.cpp
		common/ubxDecoder.cpp
		common/walkthrough.cpp
		common/localAtmosRegion.cpp

		common/streamNtrip.cpp
		common/streamCustom.cpp
		common/streamSerial.cpp
		common/streamUbx.cpp
		common/streamParser.cpp

		iono/geomagField.cpp
		iono/ionex.cpp
		iono/ionoMeas.cpp
		iono/ionoModel.cpp
		iono/ionoSpherical.cpp
		iono/ionoSphericalCaps.cpp
		iono/ionoBSplines.cpp
		iono/ionexWrite.cpp
		iono/ionoLocalSTEC.cpp

		inertial/posProp.cpp

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
		rtklib/rtkcmn.cpp

		sbas/sisnet.cpp
		sbas/sbas.cpp
)

target_include_directories(pea PUBLIC
		3rdparty
		3rdparty/egm96
		3rdparty/iers2010
		3rdparty/sofa/src
		3rdparty/jpl
		3rdparty/mqtt_cpp/
		3rdparty/slr

		configurator
		../Architecture/

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
		sbas

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

target_compile_options(pea PRIVATE -fpie)

if(OpenMP_CXX_FOUND)
	target_link_libraries(pea PUBLIC OpenMP::OpenMP_CXX)
endif()

target_compile_definitions(pea PRIVATE
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
	target_compile_definitions(pea PRIVATE ENABLE_PARALLELISATION=1)
endif()


target_link_libraries(pea PUBLIC
						m
						pthread
						# openblasp
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
						ncurses
					)

set_property(TARGET pea PROPERTY POSITION_INDEPENDENT_CODE FALSE)

add_custom_target(peas)

add_dependencies(peas
					pea
					)


if(BUILD_DOC)
add_dependencies(peas
					docs
					)
endif()


