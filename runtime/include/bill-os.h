/*
 * This file provides all the ugly Windows -> POSIX simulation code.
 * For cygwin and mingw only.  On all other platforms this file does nothing.
 */

#ifndef __BILL_OS_H__
#define __BILL_OS_H__

   /* C++/C compatibility */
#if defined(__cplusplus)
#define __BEGIN_CDECLS extern "C"  {
#define __END_CDECLS }
#define __NO_THROW throw ()
#else
#define __BEGIN_CDECLS
#define __END_CDECLS
#define __NO_THROW
#endif 

   /* Beginning of the mess */
#if defined(__CYGWIN__) || defined(__MINGW32__)

#if defined(__CYGWIN__)
#include <netdb.h>
#endif

#if defined(__MINGW32__)
#include <stdio.h>
#endif

__BEGIN_CDECLS

/* 
 * These are missing on Cygwin/Mingw.  The file /etc/networks doesn't
 * exists.
 */

struct netent * getnetbyname(const char * name) __NO_THROW;
struct netent * getnetbyaddr(long net, int type) __NO_THROW;

/*
 * The following functions require simulation on mingw.
 * Simulation includes:
 *   1. symlinks support
 *   2. signals 
 */
#if defined(__MINGW32__)

#include <signal.h>

/* Symlinks sipport */
int open(const char * filename, int, ...) __NO_THROW;
int symlink(const char * , const char *) __NO_THROW;
int readlink(const char * , const char *) __NO_THROW;
int stat(const char * filename, struct stat * buf) __NO_THROW;
int lstat(const char * filename, struct stat * buf) __NO_THROW;
FILE * winduh_fopen(const char * filename, const char * mode) __NO_THROW;
FILE * winduh_freopen(const char * filename, const char * mode, FILE *) 
   __NO_THROW; 

/* Signals */
#ifndef SIGHUP
#define SIGHUP 1
#endif
#ifndef SIGQUIT
#define SIGQUIT 3
#endif
#ifndef SIGALRM 
#define SIGALRM 14
#endif

/* Dlopen simulation */
void * dlopen(const char * filename, int) __NO_THROW; 
void   dlclose(void *) __NO_THROW; 
void * dlsym(void *,const char * name) __NO_THROW; 
const char * dlerror(void) __NO_THROW; 


#endif

__END_CDECLS

#endif

#endif // __BILL_OS_H__
