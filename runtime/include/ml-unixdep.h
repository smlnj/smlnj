/*! \file ml-unixdep.h
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://smlnj.org)
 * All rights reserved.
 *
 * This file contains O.S. dependent paths, definitions and feature flags for
 * various UNIX systems.  It should not be included in files that are OS
 * independent.  See the file * "mach-dep/signal-sysdep.h" for machine/O.S.
 * dependencies related to signal handling.
 *
 * Operating system features:
 *
 * The following feature symbols may be defined:
 *
 *   HAVE_GETRUSAGE		if OS provides getrusage(2) call
 *   HAVE_SETITIMER		if OS provides setitimer(2) call
 *   HAVE_CLOCK_GETRES		if OS provides clock_getres(3) call
 *   HAVE_MMAP			if OS provides both mmap(2) and /dev/zero.
 *   HAVE_ANON_MMAP		if OS provides anonymous mmap(2)
 *   HAVE_PARTIAL_MUNMAP        if OS allows unmapping of subranges of a mapped
 *				object
 *   HAVE_SELECT                if OS supports BSD style select(2)
 *   HAVE_POLL			if OS supports SystemV style poll(2)
 *   HAVE_POSIX_SIGS		if OS provides POSIX sigaction signal interface
 *				(including the full sigprocmask interface).
 *   HAVE_BSD_SIGS		if OS provides BSD sigvec interface (including
 *				sigsetmask).
 *   HAVE_STRUCT_SIGCONTEXT     if signal handlers have a struct sigcontext
 *				argument.
 *   INCLUDE_SIGINFO_H		include file that contains siginfo (if needed).
 *   HAVE_STRUCT_UCONTEXT       if signal handlers have a ucontext_t argument.
 *   HAVE_STRERROR		if the system provides the ISO C strerror function.
 *   STAT_HAS_TIMESPEC		if the time fields in the "struct stat" type have
 *				type "struct timespec".
 *   HAVE_NANOSLEEP             if the system provides the nanosleep(2) function.
 *
 * Note that only one of the following sets of symbols should be defined:
 *   { HAVE_MMAP, HAVE_ANON_MMAP }
 *   { HAVE_SELECT, HAVE_POLL }
 *   { HAVE_POSIX_SIGS, HAVE_BSD_SIGS }
 *   { HAVE_STRUCT_SIGCONTEXT, HAVE_STRUCT_UCONTEXT }
 */

#ifndef _ML_UNIXDEP_
#define _ML_UNIXDEP_

/** Include file paths **/
#define INCLUDE_TYPES_H		<sys/types.h>
#define INCLUDE_TIME_H		<sys/time.h>

#if defined(OPSYS_AIX)
#define INCLUDE_FCNTL_H		<fcntl.h>
#else
#define INCLUDE_FCNTL_H		<sys/fcntl.h>
#endif

#if defined(OPSYS_AIX) || defined(OPSYS_LINUX) || defined(OPSYS_FREEBSD) || defined(OPSYS_NETBSD) || defined(OPSYS_OPENBSD) || defined(OPSYS_CYGWIN)
#  define INCLUDE_DIRENT_H	<dirent.h>
#else
#  define INCLUDE_DIRENT_H	<sys/dirent.h>
#endif

#if defined(OPSYS_AIX)  /** AIX 3.2 **/
#  define OS_NAME	"AIX"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_POLL
#  define HAVE_STRUCT_SIGCONTEXT
#  define HAVE_STRERROR

/* These declarations are not in <errno.h> */
extern int	sys_nerr;
extern char	*sys_errlist[];

#elif defined(OPSYS_DARWIN) /** macOS for arm64 and x86_64 **/
#  define OS_NAME       "Darwin"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_ANON_MMAP
#  define HAVE_STRUCT_UCONTEXT
#  define HAVE_STRERROR
#  define HAVE_SELECT
#  define MAP_ANONYMOUS MAP_ANON
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
/* NOTE: macOS added clock_getres in 10.12 (Sierra).  For now, we do not
 * enable it, since we are supporting backward compatability to 10.6 (Snow Leopard).
 */
#  define HAVE_NANOSLEEP

#elif defined(OPSYS_SOLARIS) /** SunOS 5.x **/
#  define OS_NAME	"Solaris"
#  define HAVE_POSIX_SIGS
#  define HAVE_SETITIMER
#  define HAVE_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_POLL
#  define HAVE_STRUCT_UCONTEXT
#  define INCLUDE_SIGINFO_H <siginfo.h>
#  define HAVE_STRERROR
#  define HAS_MKSTEMP

/* These declarations are not in <errno.h> */
extern int	sys_nerr;
extern char	*sys_errlist[];

#elif (defined(ARCH_AMD64) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_CLOCK_GETRES
#  define HAVE_ANON_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_UCONTEXT
#  define HAVE_STRERROR
#  define HAS_MKSTEMP
#  ifndef __USE_GNU
#    define __USE_GNU
#  endif
#  define STAT_HAS_TIMESPEC
#  define HAVE_NANOSLEEP

#include <features.h>

#elif (defined(ARCH_X86) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_CLOCK_GETRES
#  define HAVE_ANON_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_UCONTEXT
#  define HAVE_STRERROR
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
#  define _FILE_OFFSET_BITS 64
#  if _POSIX_C_SOURCE >= 199309L
#    define HAVE_NANOSLEEP
#  endif

#include <features.h>

#elif (defined(ARCH_PPC) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_ANON_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_SELECT
#  define HAVE_STRERROR
#  define HAS_MKSTEMP
#  ifndef __USE_GNU
#    define __USE_GNU
#  endif
#  define STAT_HAS_TIMESPEC
#  if _POSIX_C_SOURCE >= 199309L
#    define HAVE_NANOSLEEP
#  endif

#include <features.h>

#elif (defined(ARCH_ARM64) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_CLOCK_GETRES
#  define HAVE_ANON_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_UCONTEXT
#  define HAVE_STRERROR
#  define HAS_MKSTEMP
#  ifndef __USE_GNU
#    define __USE_GNU
#  endif
#  define STAT_HAS_TIMESPEC
#  define HAVE_NANOSLEEP

#include <features.h>

#elif defined(OPSYS_FREEBSD)
#  define OS_NAME	"BSD"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_CLOCK_GETRES
#  define HAVE_ANON_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_UCONTEXT
#  define HAVE_STRERROR
#  define STAT_HAS_TIMESPEC
#  define HAVE_NANOSLEEP

#elif defined(OPSYS_NETBSD) /* version 3.x */
#  define OS_NAME	"BSD"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_MMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_UCONTEXT
#  define HAVE_STRERROR
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
#  define HAVE_NANOSLEEP

#elif defined(OPSYS_OPENBSD)
#  define OS_NAME	"BSD"
#  define HAVE_BSD_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_MMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_SIGCONTEXT
#  define HAVE_STRERROR
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
#  define HAVE_NANOSLEEP

#elif defined(OPSYS_CYGWIN)
#  define OS_NAME	"Cygwin"
#  define HAVE_POSIX_SIGS
#  define HAVE_GETRUSAGE
#  define HAVE_SETITIMER
#  define HAVE_CLOCK_GETRES
#  define HAVE_MMAP
#  define HAVE_PARTIAL_MUNMAP
#  define HAVE_SELECT
#  define HAVE_STRUCT_SIGCONTEXT
#  define HAVE_STRERROR
#  define STAT_HAS_TIMESPEC
#  define HAVE_NANOSLEEP

#include <features.h>

#endif

#include <unistd.h>
#include <string.h>
#include <errno.h>

#endif /* !_ML_UNIXDEP_ */
