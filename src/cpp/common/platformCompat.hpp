#pragma once

/**
 * @file platformCompat.hpp
 * @brief Cross-platform compatibility definitions for Windows and POSIX systems
 *
 * This header provides platform-specific abstractions for:
 * - File I/O operations
 * - Socket operations
 * - Threading primitives
 * - Time functions
 */

// Windows-specific definitions
#ifdef _WIN32
#ifndef WIN32_LEAN_AND_MEAN
#define WIN32_LEAN_AND_MEAN
#endif
#include <io.h>
#include <process.h>
#include <windows.h>
#include <winsock2.h>
#include <ws2tcpip.h>

// POSIX compatibility macros for Windows
#ifndef O_RDWR
#define O_RDWR _O_RDWR
#endif
#ifndef O_NONBLOCK
#define O_NONBLOCK 0  // Windows doesn't have direct equivalent
#endif

// Function name compatibility
#define strcasecmp _stricmp
#define strncasecmp _strnicmp
#define popen _popen
#define pclose _pclose
#define fileno _fileno

// Socket compatibility
typedef int socklen_t;

// POSIX systems (Linux, macOS, etc.)
#else
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <netinet/in.h>
#include <pthread.h>
#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>

// Windows compatibility for POSIX
#define INVALID_HANDLE_VALUE -1
#define SOCKET int
#define INVALID_SOCKET -1
#define SOCKET_ERROR -1
#define closesocket close

typedef void* HANDLE;
#endif

// Common cross-platform utilities
namespace PlatformCompat
{
/**
 * @brief Get the last system error code
 * @return Error code (errno on POSIX, GetLastError() on Windows)
 */
inline int GetLastError()
{
#ifdef _WIN32
    return ::GetLastError();
#else
    return errno;
#endif
}

/**
 * @brief Sleep for specified milliseconds
 * @param milliseconds Duration to sleep
 */
inline void SleepMs(unsigned int milliseconds)
{
#ifdef _WIN32
    Sleep(milliseconds);
#else
    usleep(milliseconds * 1000);
#endif
}
}  // namespace PlatformCompat
