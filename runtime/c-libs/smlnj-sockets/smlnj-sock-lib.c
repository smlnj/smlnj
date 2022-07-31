/* smlnj-sock-lib.c
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 */

#include "sockets-osdep.h"
#include INCLUDE_SOCKET_H
#include "ml-base.h"
#include "c-library.h"
#include "cfun-proto-list.h"


/* the table of C functions and ML names */
#define CFUNC(NAME, FUNC, MLTYPE)	CFUNC_BIND(NAME, FUNC, MLTYPE)
PVT cfunc_binding_t CFunTable[] = {
#include "cfun-list.h"
	CFUNC_NULL_BIND
    };
#undef CFUNC


void init_fn(int argc, char **argv)
{
#if defined(OPSYS_WIN32)
  static int nCode = -1;
  if( nCode!=0 )
    {
      WSADATA wsaData;
      nCode = WSAStartup(MAKEWORD(1, 1), &wsaData);
      /* FIXME: what to do if WSAStartup fails (nCode!=0)? */
    }
#endif
}


/* the Sockets library */
c_library_t	    SMLNJ_Sock_Library = {
	CLIB_NAME,
	CLIB_VERSION,
	CLIB_DATE,
        init_fn,
	CFunTable
    };

