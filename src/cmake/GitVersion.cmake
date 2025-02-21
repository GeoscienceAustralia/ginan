find_package(Git QUIET)
if (GIT_FOUND)
    execute_process(
            COMMAND ${GIT_EXECUTABLE} log --pretty=format:'%h' -n 1
            OUTPUT_VARIABLE GINAN_COMMIT_HASH
            ERROR_QUIET
            OUTPUT_STRIP_TRAILING_WHITESPACE
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
            )
    execute_process(
        COMMAND bash -c "git diff --quiet --exit-code || echo -dirty "
        OUTPUT_VARIABLE GINAN_COMMIT_DIFF
        OUTPUT_STRIP_TRAILING_WHITESPACE
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    execute_process(
        COMMAND ${GIT_EXECUTABLE} describe --exact-match --tags
        OUTPUT_VARIABLE GINAN_COMMIT_TAG ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    execute_process(
        COMMAND ${GIT_EXECUTABLE} rev-parse --abbrev-ref HEAD
        OUTPUT_VARIABLE GINAN_BRANCH_NAME
        OUTPUT_STRIP_TRAILING_WHITESPACE
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
    string(FIND "${GINAN_BRANCH_NAME}" "/" SLASH_POSITION)
    if(SLASH_POSITION GREATER -1)
        string(SUBSTRING "${GINAN_BRANCH_NAME}" ${SLASH_POSITION} -1 GINAN_BRANCH_NAME)
        string(SUBSTRING "${GINAN_BRANCH_NAME}" 1 -1 GINAN_BRANCH_NAME) # Remove the leading '/'
    endif()
    execute_process(
        COMMAND ${GIT_EXECUTABLE} log -1 --format=%cd --date=local
        OUTPUT_VARIABLE "GINAN_COMMIT_DATE"
        OUTPUT_STRIP_TRAILING_WHITESPACE
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )

    if (GINAN_COMMIT_TAG STREQUAL "")
        set(GINAN_COMMIT_TAG "untagged")
    endif()
    string(REGEX REPLACE "'" "" GINAN_COMMIT_HASH "${GINAN_COMMIT_HASH}")
else()
    message(WARNING "Git not found, version will not be updated")
    SET(GINAN_COMMIT_VERSION "N/A")
    SET(GINAN_COMMIT_DIFF "N/A")
    SET(GINAN_COMMIT_TAG "N/A")
    SET(GINAN_BRANCH_NAME "N/A")
endif()


SET(GINAN_COMMIT_VERSION "${GINAN_COMMIT_TAG}-${GINAN_COMMIT_HASH}${GINAN_COMMIT_DIFF}")
MESSAGE(STATUS "Git branch tag:           ${GINAN_COMMIT_VERSION}")
MESSAGE(STATUS "Git branch:               ${GINAN_BRANCH_NAME}")

configure_file(${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h.in ${CMAKE_CURRENT_SOURCE_DIR}/cpp/pea/peaCommitVersion.h @ONLY)
