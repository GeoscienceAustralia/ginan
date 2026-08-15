set(UNIT_TEST_INCLUDE_DIRS
    ${CMAKE_CURRENT_SOURCE_DIR}/../cpp
    ${EIGEN3_INCLUDE_DIRS}
    ${Boost_INCLUDE_DIRS}
)

function(configure_unit_test target)
    target_include_directories(${target} PRIVATE
        ${UNIT_TEST_INCLUDE_DIRS}
    )

    if(OpenMP_CXX_FOUND)
        target_link_libraries(${target} PRIVATE OpenMP::OpenMP_CXX)
    endif()

    add_test(NAME ${target} COMMAND ${target})
endfunction()

set(RECEIVER_METADATA_TEST_SOURCES
    ../cpp/common/receiverMetadata.cpp
    unit/test_ReceiverMetaData.cpp
)

set(SANITY_CHECKER_TEST_SOURCES
    ../cpp/common/sanityCheckers/ConfigSanityManager.cpp
    ../cpp/common/sanityCheckers/EphemerisTimeDelayChecker.cpp
    ../cpp/common/sanityCheckers/EpochToleranceChecker.cpp
    ../cpp/common/sanityCheckers/IonosphericFreeComboChecker.cpp
    ../cpp/common/sanityCheckers/IonosphericOutageChecker.cpp
    ../cpp/common/sanityCheckers/RequiredSiteEccentricityChecker.cpp
    ../cpp/common/sanityCheckers/SbasSanityChecker.cpp
)

function(add_sanity_checker_test target test_source)
    add_executable(${target}
        ${SANITY_CHECKER_TEST_SOURCES}
        ${test_source}
    )

    target_link_libraries(${target} PRIVATE
        Boost::log
        Boost::log_setup
        Boost::program_options
        Boost::thread
        Boost::unit_test_framework
    )

    configure_unit_test(${target})
endfunction()

add_executable(receiver_metadata_tests
    ${RECEIVER_METADATA_TEST_SOURCES}
)

add_executable(trace_file_cache_tests
    unit/test_TraceFileCache.cpp
)

add_executable(phase_clock_osb_tests
    unit/test_PhaseClockOsb.cpp
)

add_executable(zhang_full_rank_tests
    unit/test_ZhangFullRank.cpp
    unit/test_ZhangCheckpointSupport.cpp
    ../cpp/common/zhangCheckpoint.cpp
)

# Keep the deterministic restart boundary tests in a small target.  Pulling
# these into zhang_full_rank_tests would make failures in cursor/controller
# checkpointing difficult to distinguish from the mathematical regression
# suite, and linking the complete PEA executable would hide missing test
# seams behind unrelated production dependencies.
add_executable(zhang_checkpoint_infra0_tests
    unit/test_ZhangCheckpointInfra0.cpp
    unit/test_ZhangCheckpointSupport.cpp
    unit/test_ZhangInputCheckpointSupport.cpp
    ../cpp/common/zhangCheckpoint.cpp
    ../cpp/pea/zhangInputCheckpoint.cpp
)

add_sanity_checker_test(config_sanity_manager_tests
    unit/sanityCheckers/test_ConfigSanityManager.cpp
)
add_sanity_checker_test(epoch_tolerance_checker_tests
    unit/sanityCheckers/test_EpochToleranceChecker.cpp
)
add_sanity_checker_test(ephemeris_time_delay_checker_tests
    unit/sanityCheckers/test_EphemerisTimeDelayChecker.cpp
)
add_sanity_checker_test(ionospheric_free_combo_checker_tests
    unit/sanityCheckers/test_IonosphericFreeComboChecker.cpp
)
add_sanity_checker_test(ionospheric_outage_checker_tests
    unit/sanityCheckers/test_IonosphericOutageChecker.cpp
)
add_sanity_checker_test(required_site_eccentricity_checker_tests
    unit/sanityCheckers/test_RequiredSiteEccentricityChecker.cpp
)
add_sanity_checker_test(sbas_sanity_checker_tests
    unit/sanityCheckers/test_SbasSanityChecker.cpp
)

add_custom_target(unit_tests
    DEPENDS receiver_metadata_tests
            trace_file_cache_tests
            phase_clock_osb_tests
            zhang_full_rank_tests
            zhang_checkpoint_infra0_tests
            config_sanity_manager_tests
            epoch_tolerance_checker_tests
            ephemeris_time_delay_checker_tests
            ionospheric_free_combo_checker_tests
            ionospheric_outage_checker_tests
            required_site_eccentricity_checker_tests
            sbas_sanity_checker_tests
)

target_link_libraries(receiver_metadata_tests PRIVATE
    Boost::unit_test_framework
)

target_link_libraries(trace_file_cache_tests PRIVATE
    Boost::log
    Boost::unit_test_framework
)

target_link_libraries(phase_clock_osb_tests PRIVATE
    Boost::unit_test_framework
)

target_link_libraries(zhang_full_rank_tests PRIVATE
	Boost::json
    Boost::serialization
    Boost::unit_test_framework
    OpenSSL::Crypto
)

target_link_libraries(zhang_checkpoint_infra0_tests PRIVATE
    Boost::log
    Boost::log_setup
    Boost::serialization
    Boost::unit_test_framework
    OpenSSL::Crypto
)

if(WIN32)
    target_link_libraries(receiver_metadata_tests PRIVATE ws2_32 wsock32)
endif()

configure_unit_test(receiver_metadata_tests)
configure_unit_test(trace_file_cache_tests)
configure_unit_test(phase_clock_osb_tests)
configure_unit_test(zhang_full_rank_tests)
configure_unit_test(zhang_checkpoint_infra0_tests)
