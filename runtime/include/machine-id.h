/*! \file machine-id.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _MACHINE_ID_
#define _MACHINE_ID_

#ifndef _ML_BASE_
#  include "ml-base.h"
#endif

#if defined(ARCH_AMD64)
#  define MACHINE_ID	"amd64"
#elif defined(ARCH_ARM64)
#  define MACHINE_ID	"arm64"
#else
#  error unknown architecture type
#endif

#if   defined(OPSYS_UNIX)
#  if (defined(OPSYS_DARWIN))
#    define OPSYS_ID    "darwin"
#  elif (defined(OPSYS_FREEBSD) || defined(OPSYS_NETBSD) || defined(OPSYS_NETBSD2) || defined(OPSYS_OPENBSD))
#    define OPSYS_ID	"bsd"
#  elif (defined(OPSYS_LINUX))
#    define OPSYS_ID	"linux"
#  elif (defined(OPSYS_SOLARIS))
#    define OPSYS_ID	"solaris"
#  elif (defined(OPSYS_CYGWIN))
#    define OPSYS_ID    "cygwin"
#  else
#    define OPSYS_ID	"unix"
#  endif
#elif defined(OPSYS_WIN32)
#  define OPSYS_ID	"win32"
#else
#  error unknown operating system
#endif

#endif /* _MACHINE_ID_ */
