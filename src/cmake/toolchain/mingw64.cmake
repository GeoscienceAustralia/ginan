# CMake toolchain file for cross-compiling to Windows from Linux using MinGW-w64

set(CMAKE_SYSTEM_NAME Windows)
set(CMAKE_SYSTEM_PROCESSOR AMD64)

# Disable vcpkg applocal.ps1 which requires PowerShell (not available during cross-compile)
set(VCPKG_APPLOCAL_DEPS OFF CACHE BOOL "Disable vcpkg applocal" FORCE)

# Specify the cross compiler
set(CMAKE_C_COMPILER x86_64-w64-mingw32-gcc)
set(CMAKE_CXX_COMPILER x86_64-w64-mingw32-g++)
set(CMAKE_RC_COMPILER x86_64-w64-mingw32-windres)
set(CMAKE_AR x86_64-w64-mingw32-ar)
set(CMAKE_RANLIB x86_64-w64-mingw32-ranlib)

# Where to search for target environment
set(CMAKE_FIND_ROOT_PATH /usr/x86_64-w64-mingw32)

# Add vcpkg installed directory to find root path if available
if(DEFINED _VCPKG_INSTALLED_DIR AND DEFINED VCPKG_TARGET_TRIPLET)
    list(APPEND CMAKE_FIND_ROOT_PATH "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}")
    message(STATUS "Added vcpkg install dir to CMAKE_FIND_ROOT_PATH: ${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}")
endif()

# Adjust the default behavior of the FIND_XXX() commands:
# search programs in the host environment
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)

# search headers and libraries in the target environment
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
# Allow packages from vcpkg to be found
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE BOTH)

# Ensure static linking for MinGW
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -static-libgcc -static-libstdc++")
set(CMAKE_SHARED_LINKER_FLAGS "${CMAKE_SHARED_LINKER_FLAGS} -static-libgcc -static-libstdc++")

# Windows-specific definitions
# Use Windows 8 (0x0602) for Boost atomic operations compatibility
add_definitions(-D_WIN32_WINNT=0x0602)
add_definitions(-DWIN32_LEAN_AND_MEAN)

# Enable large file support (>2GB files) for MinGW
add_definitions(-D_FILE_OFFSET_BITS=64)
add_definitions(-D_LARGEFILE64_SOURCE)

# Help FindOpenSSL locate libraries in vcpkg for cross-compilation
# The vcpkg wrapper uses different variable names for WIN32
if(DEFINED _VCPKG_INSTALLED_DIR AND DEFINED VCPKG_TARGET_TRIPLET)
    set(OPENSSL_ROOT_DIR "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}" CACHE PATH "OpenSSL root directory" FORCE)
    set(OPENSSL_INCLUDE_DIR "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/include" CACHE PATH "OpenSSL include directory" FORCE)
    set(OPENSSL_CRYPTO_LIBRARY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libcrypto.a" CACHE FILEPATH "OpenSSL crypto library" FORCE)
    set(OPENSSL_SSL_LIBRARY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libssl.a" CACHE FILEPATH "OpenSSL SSL library" FORCE)
    set(LIB_EAY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libcrypto.a" CACHE FILEPATH "OpenSSL crypto library (legacy)" FORCE)
    set(SSL_EAY "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libssl.a" CACHE FILEPATH "OpenSSL SSL library (legacy)" FORCE)
    message(STATUS "Set OpenSSL paths for cross-compilation: ${OPENSSL_ROOT_DIR}")
    
    # Help FindBLAS/FindLAPACK locate libraries
    set(BLAS_LIBRARIES "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libopenblas.a" CACHE FILEPATH "BLAS library" FORCE)
    set(LAPACK_LIBRARIES "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/liblapack.a;${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libf2c.a;${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libopenblas.a" CACHE FILEPATH "LAPACK library" FORCE)
    
    # Help FindYAML_CPP locate libraries
    set(YAML_CPP_LIBRARIES "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libyaml-cpp.a" CACHE FILEPATH "YAML-CPP library" FORCE)
    set(YAML_CPP_LIB "${_VCPKG_INSTALLED_DIR}/${VCPKG_TARGET_TRIPLET}/lib/libyaml-cpp.a" CACHE FILEPATH "YAML-CPP library (alternate variable)" FORCE)
endif()
