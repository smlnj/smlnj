/* cache-flush.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * System dependent includes and macros for flushing the cache.
 */

#ifndef _CACHE_FLUSH_
#define _CACHE_FLUSH_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#if defined(ARCH_X86)
/* 386 & 486 have unified caches and the pentium has hardware consistency */
#  define FlushICache(addr, size)

#elif (defined(ARCH_PPC) && defined(OPSYS_AIX))
#  include <sys/cache.h>
#  define FlushICache(addr, size)	_sync_cache_range((addr), (size))

#elif (defined(ARCH_SPARC) || defined(OPSYS_MKLINUX))
extern FlushICache (void *addr, int nbytes);

#elif (defined(ARCH_PPC) && (defined(OPSYS_LINUX) || defined(OPSYS_DARWIN) ))
extern FlushICache (void *addr, int nbytes);

#elif defined(ARCH_ARM64) && defined(OPSYS_DARWIN)
/* on "Apple Silicon" (aka, arm64), memory that is executable cannot be written
 * (and vice versa) at the same time, so we have to enable/disable write permissions
 * around operations that write to code objects.  See
 *	https://developer.apple.com/documentation/apple-silicon/porting-just-in-time-compilers-to-apple-silicon
 * for details.
 */
#include <pthread.h>
#include <libkern/OSCacheControl.h>
STATIC_INLINE void EnableCodeWrite ()
{
    Say("+++++ CODE WRITE\n");
    pthread_jit_write_protect_np(0);
}
STATIC_INLINE void DisableCodeWrite ()
{
    Say("----- CODE WRITE\n");
    pthread_jit_write_protect_np(1);
}
#define ENABLE_CODE_WRITE	EnableCodeWrite ();
#define DISABLE_CODE_WRITE	DisableCodeWrite ();
//#define ENABLE_CODE_WRITE	pthread_jit_write_protect_np(1);
//#define DISABLE_CODE_WRITE	pthread_jit_write_protect_np(0);
#define FlushICache(ADR, SZ)	sys_icache_invalidate(ADR, SZ)
#else
#  define FlushICache(addr, size)
#endif

#ifndef ENABLE_CODE_WRITE
#  define ENABLE_CODE_WRITE
#  define DISABLE_CODE_WRITE
#endif

#endif /* !_CACHE_FLUSH_ */

