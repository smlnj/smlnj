/* sockets-osdep.h
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * O.S. specific dependencies needed by the sockets library.
 */

#ifndef _SOCKETS_OSDEP_
#define _SOCKETS_OSDEP_

#if defined(OPSYS_UNIX)
#  define HAS_UNIX_DOMAIN
#  define INCLUDE_SOCKET_H	<sys/socket.h>
#  define INCLUDE_IN_H		<netinet/in.h>
#  define INCLUDE_TCP_H		<netinet/tcp.h>
#  define INCLUDE_UN_H		<sys/un.h>

#  if defined(OPSYS_SOLARIS)
#    define INCLUDE_RPCENT_H	<rpc/rpcent.h>

typedef char *sockoptval_t;	/* The pointer type used to pass values to */
				/* getsockopt/setsockopt */

#    define BSD_COMP		/* needed to include FION* in ioctl.h */

#  else
typedef void *sockoptval_t;	/* The pointer type used to pass values to */
				/* getsockopt/setsockopt */
#  endif

#  if (defined(OPSYS_AIX))
#    define _SUN		/* to get the rpcent definitions */
#    define SOCKADDR_HAS_LEN	/* socket address has a length field */
#  endif

#  if (defined(OPSYS_FREEBSD) || defined (OPSYS_NETBSD) || defined (OPSYS_NETBSD2))
#    define i386		1	/* to avoid a bug in system header files */
#    define INCLUDE_RPCENT_H	<rpc/rpc.h>
#  endif

#include "ml-unixdep.h"
/* FIXME: The following includes are not needed in every file, yet they
   cannot be moved to where they are used since that would break compilation
   under Windows */
#include INCLUDE_TYPES_H
#include INCLUDE_IN_H
#include INCLUDE_TCP_H
#include <netdb.h>
#include <sys/ioctl.h>
#include <unistd.h>

#elif defined(OPSYS_WIN32) || defined(OPSYS_CYGWIN)
#  define INCLUDE_SOCKET_H      <winsock2.h>

/* This type is not defined in winsock2.h */
typedef int socklen_t;

/* FIXME:  Is ioctlsocket() on Windows really the same as ioctl() on Unix?
   It does seem so, yet the second parameter is of a different type */
#  define ioctl ioctlsocket

typedef char *sockoptval_t;	/* The pointer type used to pass values to */
				/* getsockopt/setsockopt */
#endif

#define MAX_SOCK_ADDR_SZB	1024

#endif /* !_SOCKETS_OSDEP_ */

