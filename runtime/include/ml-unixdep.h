/*! \file ml-unixdep.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
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
 *   HAS_POSIX_LIBRARIES	if the ML Posix binding is supported.
 *   HAS_GETRUSAGE		if OS provides getrusage(2) call
 *   HAS_SETITIMER		if OS provides setitimer(2) call
 *   HAS_CLOCK_GETRES		if OS provides clock_getres(3) call
 *   HAS_MMAP			if OS provides both mmap(2) and /dev/zero.
 *   HAS_ANON_MMAP		if OS provides anonymous mmap(2)
 *   HAS_PARTIAL_MUNMAP		if OS allows unmapping of subranges of a mapped
 *				object
 *   HAS_VM_ALLOCATE		if OS provides vm_allocate (MACH)
 *   HAS_SELECT			if OS supports BSD style select(2)
 *   HAS_POLL			if OS supports SystemV style poll(2)
 *   HAS_POSIX_SIGS		if OS provides POSIX sigaction signal interface
 *				(including the full sigprocmask interface).
 *   HAS_BSD_SIGS		if OS provides BSD sigvec interface (including
 *				sigsetmask).
 *   HAS_SIGCONTEXT		if signal handlers have a struct sigcontext
 *				argument.
 *   INCLUDE_SIGINFO_H		include file that contains siginfo (if needed).
 *   HAS_UCONTEXT		if signal handlers have a ucontext_t argument.
 *   HAS_STRERROR		if the system provides the ISO C strerror function.
 *   INT_GIDLIST		if the second argument to getgroups is int[].
 *   STAT_HAS_TIMESPEC		if the time fields in the "struct stat" type have
 *				type "struct timespec".
 *   HAS_NANOSLEEP              if the system provides the nanosleep(2) function.
 *
 * Note that only one of the following sets of symbols should be defined:
 *   { HAS_MMAP, HAS_ANON_MMAP, HAS_VM_ALLOCATE }
 *   { HAS_SELECT, HAS_POLL }
 *   { HAS_POSIX_SIGS, HAS_BSD_SIGS }
 *   { HAS_SIGCONTEXT, HAS_UCONTEXT }
 *
 * Some UNIX systems do not support the POSIX libraries (HAS_POSIX_LIBRARIES is
 * not defined), in which case, some of the following feature falgs may be defined:
 *
 *   HAS_ACCESS
 *   HAS_WAITPID		if OS provides waitpid(2) call (POSIX)
 *   HAS_WAIT3			if OS provides the BSD wait3(2) call
 *   HAS_SYMLINKS		if OS supports symbolic links; this includes
 *				the symlink(2) and readlink(2) calls.
 *   HAS_GETCWD			if OS supports getcwd(3) (POSIX)
 *   HAS_GETWD			if OS supports getwd(3) (BSD)
 *   HAS_CHMOD			if OS supports chmod(2) and fchmod(2)
 *   HAS_TRUNCATE		if OS supports truncate(2) and ftruncate(2)
 *   HAS_GETHOSTNAME		if OS supports gethostname(2)
 *   HAS_GETHOSTID		if OS supports gethostid(2)
 *   HAS_SYSINFO		if OS supports SystemV style sysinfo(2)
 *   HAS_UNAME_ID		if OS supports uname(2) with machine ID field
 *
 *   { HAS_GETHOSTID, HAS_SYSINFO, HAS_UNAME_ID }
 *   { HAS_WAITPID, HAS_WAIT3 }
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
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_POLL
#  define HAS_SIGCONTEXT
#  define HAS_STRERROR

/* These declarations are not in <errno.h> */
extern int	sys_nerr;
extern char	*sys_errlist[];

#elif defined(OPSYS_DARWIN) /** macOS for arm64 and x86_64 **/
#  define OS_NAME       "Darwin"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_ANON_MMAP
#  define HAS_UCONTEXT
#  define HAS_STRERROR
#  define HAS_SELECT
#  define MAP_ANONYMOUS MAP_ANON
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
/* NOTE: macOS added clock_getres in 10.12 (Sierra).  For now, we do not
 * enable it, since we are supporting backward compatability to 10.6 (Snow Leopard).
 */
#  define HAS_NANOSLEEP

#elif defined(OPSYS_SOLARIS) /** SunOS 5.x **/
#  define OS_NAME	"Solaris"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_SETITIMER
#  define HAS_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_POLL
#  define HAS_UCONTEXT
#  define INCLUDE_SIGINFO_H <siginfo.h>
#  define HAS_STRERROR
#  define HAS_MKSTEMP

/* These declarations are not in <errno.h> */
extern int	sys_nerr;
extern char	*sys_errlist[];

#elif (defined(ARCH_AMD64) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_CLOCK_GETRES
#  define HAS_ANON_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_SELECT
#  define HAS_UCONTEXT
#  define HAS_STRERROR
#  define HAS_MKSTEMP
#  ifndef __USE_GNU
#    define __USE_GNU
#  endif
#  define STAT_HAS_TIMESPEC
#  define HAS_NANOSLEEP

#include <features.h>

#elif (defined(ARCH_X86) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_CLOCK_GETRES
#  define HAS_ANON_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_SELECT
#  define HAS_UCONTEXT
#  define HAS_STRERROR
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
#  define _FILE_OFFSET_BITS 64
#  if _POSIX_C_SOURCE >= 199309L
#    define HAS_NANOSLEEP
#  endif

#include <features.h>

#elif (defined(ARCH_PPC) && defined(OPSYS_LINUX))
#  define OS_NAME	"Linux"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_ANON_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_SELECT
#  define HAS_STRERROR
#  define HAS_MKSTEMP
#  ifndef __USE_GNU
#    define __USE_GNU
#  endif
#  define STAT_HAS_TIMESPEC
#  if _POSIX_C_SOURCE >= 199309L
#    define HAS_NANOSLEEP
#  endif

#include <features.h>

#elif defined(OPSYS_FREEBSD)
#  define OS_NAME	"BSD"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_CLOCK_GETRES
#  define HAS_ANON_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_SELECT
#  define HAS_UCONTEXT
#  define HAS_STRERROR
#  define STAT_HAS_TIMESPEC
#  define HAS_NANOSLEEP

#elif defined(OPSYS_NETBSD) /* version 3.x */
#  define OS_NAME	"BSD"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_MMAP
#  define HAS_SELECT
#  define HAS_UCONTEXT
#  define HAS_STRERROR
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
#  define HAS_NANOSLEEP

#elif defined(OPSYS_OPENBSD)
#  define OS_NAME	"BSD"
#  define HAS_POSIX_LIBRARIES
#  define HAS_BSD_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_MMAP
#  define HAS_SELECT
#  define HAS_SIGCONTEXT
#  define HAS_STRERROR
#  define HAS_MKSTEMP
#  define STAT_HAS_TIMESPEC
#  define HAS_NANOSLEEP

#elif defined(OPSYS_CYGWIN)
#  define OS_NAME	"Cygwin"
#  define HAS_POSIX_LIBRARIES
#  define HAS_POSIX_SIGS
#  define HAS_GETRUSAGE
#  define HAS_SETITIMER
#  define HAS_CLOCK_GETRES
#  define HAS_MMAP
#  define HAS_PARTIAL_MUNMAP
#  define HAS_SELECT
#  define HAS_SIGCONTEXT
#  define HAS_STRERROR
#  define STAT_HAS_TIMESPEC
#  define HAS_NANOSLEEP

#include <features.h>

#endif

#include <unistd.h>
#include <string.h>
#include <errno.h>

#endif /* !_ML_UNIXDEP_ */
