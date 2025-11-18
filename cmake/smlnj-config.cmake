# Feature tests for configuring the SML/NJ runtime SYSTEM
#
# COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
# All rights reserved.
#

include(CheckIncludeFile)
include(CheckLibraryExists)
include(CheckSymbolExists)
include(CheckFunctionExists)
include(CheckStructHasMember)
include(CheckTypeExists)
include(CheckCSourceCompiles)

if( UNIX AND NOT APPLE)
  # Used by check_symbol_exists:
  list(APPEND CMAKE_REQUIRED_LIBRARIES "m")
endif()

block(SCOPE_FOR POLICIES)

  # Honor per-config flags in try_compile() source-file signature.
  cmake_policy(SET CMP0066 NEW)
  # Honor language standard in try_compile() source-file signature.
  cmake_policy(SET CMP0067 NEW)

  if ( ${CMAKE_CXX_BYTE_ORDER} STREQUAL "LITTLE_ENDIAN" )
    set(SMLNJ_LITTLE_ENDIAN ON)
  elseif ( ${CMAKE_CXX_BYTE_ORDER} STREQUAL "BIG_ENDIAN" )
    set(SMLNJ_BIG_ENDIAN ON)
  else()
    message(FATAL_ERROR "unknown endianess")
  endif()

  if ( UNIX )
    # include files
    check_include_file(sys/mman.h INCLUDE_SYS_MMAN_H)
    check_include_file(ucontext.h INCLUDE_UCONTEXT_H)
    check_include_file(sys/ucontext.h INCLUDE_SYS_UCONTEXT_H)
    check_include_file(siginfo.h INCLUDE_SIGINFO_H)
    check_include_file(hugetlbfs.h INCLUDE_HUGETLBFS_H)

    # is gethugepagesizes supported?
    if (INCLUDE_HUGETLBFS_H)
      check_symbol_exists (gethugepagesizes "hugetlbfs.h" HAVE_GETHUGEPAGESIZES)
    endif()

    # check various flags for mmap
    if (INCLUDE_SYS_MMAN_H)
      # flag on BSD Unix that allows for specifying alignment of mapped memory
      check_symbol_exists (MAP_ALIGNED "sys/mman.h" HAVE_MAP_ALIGNED)
      # flag on BSD Unix that aligns mapped memory for huge-page use
      check_symbol_exists (MAP_ALIGNED_SUPER "sys/mman.h" HAVE_MAP_ALIGNED_SUPER)
      # flag on Linux that specifies that huge pages should be used
      check_symbol_exists (MAP_HUGETLB "sys/mman.h" HAVE_MAP_HUGETLB)
    endif()

    # is there a mechanism for huge pages?
    if ((MAP_ALIGNED_SUPER OR MAP_HUGETLB) AND (HAVE_GETHUGEPAGESIZES OR PROC_MEMINFO_DIR_EXISTS))
      set (HUGE_PAGE_SUPPORT ON)
    else()
      set (HUGE_PAGE_SUPPORT OFF)
    endif()

    # check cache-size symbols for sysctl
    check_symbol_exists (sysctlbyname "sys/sysctl.h" HAVE_SYSCTLBYNAME)
    if (HAVE_SYSCTLBYNAME)
      check_symbol_exists (HW_L1DCACHESIZE "sys/sysctl.h" HAVE_SYSCTL_CACHE_SIZE)
    endif()

    # check cache-size symbols for sysconf
    check_symbol_exists (sysconf "unistd.h" HAVE_SYSCONF)
    if (HAVE_SYSCONF)
      check_symbol_exists (_SC_LEVEL1_DCACHE_SIZE "unistd.h" HAVE_SC_CACHE_SIZE)
    endif()

    # various types used to represent time values
    check_type_exists ("struct timespec" "sys/time.h" HAVE_STRUCT_TIMESPEC)
    check_type_exists ("struct timeval" "sys/time.h" HAVE_STRUCT_TIMEVAL)

    # different mechanisms for getting the current time
    check_symbol_exists (clock_gettime_nsec_np "time.h" HAVE_CLOCK_GETTIME_NSEC_NP)
    if ( NOT HAVE_CLOCK_GETTIME_NSEC_NP )
      check_symbol_exists (clock_gettime "time.h" HAVE_CLOCK_GETTIME)
      if ( NOT HAVE_CLOCK_GETTIME )
        check_symbol_exists (gettimeofday "sys/time.h" HAVE_GETTIMEOFDAY)
      endif()
    endif()

    # mechanisms for setting the modification time of a file
    check_symbol_exists (utimensat "sys/stat.h" HAVE_UTIMENSAT)
    if ( NOT HAVE_UTIMENSAT )
      check_symbol_exists (utimes "sys/time.h" HAVE_UTIMES)
      if ( NOT HAVE_UTIMES )
        check_symbol_exists (utime "utime.h" HAVE_UTIME)
      endif()
    endif()

    # different mechanisms for generating temporary files
    check_symbol_exists (mktemp "unistd.h" HAVE_MKSTEMP)
    if ( NOT HAVE_MKSTEMP )
      check_symbol_exists (mkstemp "unistd.h" HAVE_MKTEMP)
      if ( NOT HAVE_MKTEMP )
        check_symbol_exists (tmpnam "stdio.h" HAVE_TMPNAM)
      endif()
    endif()

    # check if the `struct stat` type has `struct timespec` file timestamp fields
    check_struct_has_member("struct stat" st_atim "sys/stat.h" HAVE_STRUCT_STAT_WITH_ST_ATIM)
    if ( NOT HAVE_STRUCT_STAT_WITH_ST_ATIM )
      check_struct_has_member("struct stat" st_atimespec "sys/stat.h" HAVE_STRUCT_STAT_WITH_ST_ATIMESPEC)
    endif()

    # different mechanisms for sleeping the process
    check_symbol_exists (nanosleep "time.h" HAVE_NANOSLEEP)
    if ( NOT HAVE_NANOSLEEP )
      check_symbol_exists (usleep "unistd.h" HAVE_USLEEP)
      if ( NOT HAVE_USLEEP )
        check_symbol_exists (sleep "unistd.h" HAVE_SLEEP)
      endif()
    endif()

    # different mechanisms for simple alarms
    check_symbol_exists (ualarm "unistd.h" HAVE_UALARM)

    # interval timers (use clock_getres to get precision when available)
    check_symbol_exists (setitimer "sys/time.h" HAVE_SETITIMER)
    check_symbol_exists (clock_getres "time.h" HAVE_CLOCK_GETRES)

    # getting the resource usage
    check_symbol_exists (getrusage "sys/resource.h" HAVE_GETRUSAGE)

    # mechanisms for polling file descriptors
    check_symbol_exists (select "sys/select.h" HAVE_SELECT)
    if ( NOT HAVE_SELECT )
      check_symbol_exists (poll "poll.h" HAVE_POLL)
    endif()

    # signal handling mechanisms
    if ( INCLUDE_UCONTEXT_H OR INCLUDE_SYS_UCONTEXT_H )
      set (HAVE_UCONTEXT_T 1)
    else()
      check_type_exists ("struct sigcontext" "signal.h" HAVE_STRUCT_SIGCONTEXT)
    endif()
    check_symbol_exists (sigaction "signal.h" HAVE_SIGACTION)
    if ( NOT HAVE_SIGACTION )
      check_symbol_exists (sigvec "signal.h" HAVE_SIGVEC)
    else()
      check_type_exists ("siginfo_t" "signal.h" HAVE_SIGINFO_T)
    endif()

    # check for which Unix signals are defined by the host system
    include (check-unix-signals)

    # do socket addressed have the "sun_len" member?
    check_struct_has_member(
      "struct sockaddr_un" sun_len "sys/socket.h;sys/un.h"
      HAVE_SOCKADDR_SUN_LEN LANGUAGE C)

    # mapping a Unix error code to a string
    check_symbol_exists(strerror_r "string.h" HAVE_STRERROR_R)
    if ( NOT HAVE_STRERROR_R )
      check_symbol_exists(strerror "string.h" HAVE_STRERROR)
      if ( NOT HAVE_STRERROR )
        check_symbol_exists(sys_errlist "stdio.h" HAVE_SYS_ERRLIST)
      endif()
    endif()

    # mechanisms for invalidating the instruction cache
    # note that on Windows, we can use the FlushInstructionCache() function
    check_symbol_exists (__builtin___clear_cache "" HAVE_BUILTIN_CLEAR_CACHE) # GCC
    if ( NOT HAVE_BUILTIN_CLEAR_CACHE )
      check_symbol_exists (cacheflush "sys/cachectl.h" HAVE_CACHEFLUSH) # Linux
      if ( NOT HAVE_CACHEFLUSH )
        # Darwin and BSD systems
        check_symbol_exists (
            sys_icache_invalidate
            "libkern/OSCacheControl.h"
            HAVE_SYS_ICACHE_INVALIDATE)
      endif()
    endif()

    # we assume that all non-windows systems support Unix-domain sockets
    set(HAVE_UNIX_DOMAIN ON)

  else()

    # Windows FILETIME values
    check_type_exists(FILETIME "minwinbase.h" HAVE_FILETIME)

    # windows systems do not support Unix-domain sockets
    set(HAVE_UNIX_DOMAIN OFF BOOL CACHE)

  endif() # NOT PURE_WINDOWS

  # the host architecture and operating system names
  set (SMLNJ_ARCH_NAME "${SMLNJ_ARCH}")
  set (SMLNJ_OPSYS_NAME "${SMLNJ_OPSYS}")

  if(NOT (CMAKE_SIZEOF_VOID_P EQUAL "8"))
    message(FATAL_ERROR "32-bit systems are not supported by this version of SML/NJ")
  endif()

endblock()
