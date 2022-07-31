/*! \file cygwin-fault.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Special signal handling for cygwin on Windows.
 *
 * Even though cygwin behaves like "unix", its signal handling mechanism
 * is crippled.  I haven't been able to get/set the EIP addresses from
 * the siginfo_t and related data structures.  So here I'm using
 * Windows and some gcc assembly hacks to get things done.
 */

#if defined(__i386__) && defined(__CYGWIN32__) && defined(__GNUC__)

#include "ml-unixdep.h"
#include "signal-sysdep.h"
#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "ml-globals.h"

#include <windows.h>

/******************** from exceptions.h ********************
 *
 * Older versions of Cygwin had a file /usr/include/exceptions.h, but this
 * file has disappeared in more recent versions.  Here is the key bits
 * taken from  ftp://ftp.com.univ-mrs.fr/pub/cygwin/usr/include/exceptions.h
 *
 */

/* exceptions.h

   Copyright 1996, 1997, 1998, 2001 Red Hat, Inc.

This file is part of Cygwin.

This software is a copyrighted work licensed under the terms of the
Cygwin license.  Please consult the file "CYGWIN_LICENSE" for
details. */

#ifndef _EXCEPTIONS_H
#define _EXCEPTIONS_H

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/* Documentation on the innards of exception handling (i.e. from the
   perspective of a compiler implementor) apparently doesn't exist.  Sigh.
   However, the following came from Onno Hovers <onno@stack.urc.tue.nl>

The first pointer to the chain of handlers is in the thread environment block
at FS:[0].  This chain has the following format:

typedef struct __EXCEPTION_FRAME
{
   struct __EXCEPTION_FRAME	*Prev;    /-* pointer to the previous frame *-/
   PEXCEPTION_HANDLER		Handler; /-* handler function *-/
}

You register an exception handler in your compiler with this simple ASM
sequence:
   PUSH _MyExceptionHandler
   PUSH FS:[0]
   MOV  FS:[0],ESP
An exception frame MUST be on the stack! The frame may have more fields and
both Visual C++ and Borland C++ use more fields for themselves.

When an exception occurs the system calls all handlers starting with the
handler at FS:0, and then the previous etc. until one handler returns
ExceptionContinueExecution, which is 0. If a handler does not want to handle
the exception it should just return ExceptionContinueSearch, which is 1.

The handler has the following parameters:
ehandler (
	   PEXCEPTION_RECORD erecord,
	   PEXCEPTION_FRAME myframe,
	   PCONTEXT context,		/-* context before and after *-/
	   PVOID dispatch)		/-* something *-/

When a handler wants to handle the exception, it has some alternatives:

-one is to do do something about the exception condition, like emulating
an invalid instruction, mapping memory where there was a page fault, etc.
If the handler wants to have the context of the thread that causes the
exception changed, it should make that change in the context passed to the
handler.

-the second alternative is to call all exception handlers again, indicating
that you want them to clean up. This way all the __finally blocks get
executed. After doing that you change the context passed to the handler so
the code starts executing in the except block. For this purpose you could
call RtlUnwind. This (undocumented) function calls all exception handlers
up to but not including the exception frame passed to it. If NULL is passed
as exception frame RtlUnwind calls all exception handlers and then exits the
process. The parameters to RtlUnwind are:

RtlUnwind (
   PEXCEPTION_FRAME	endframe,
   PVOID		unusedEip,
   PEXCEPTION_RECORD	erecord,
   DWORD		returnEax)

You should set unusedEip to the address where RtlUnwind should return like
this:
	  PUSH 0
	  PUSH OFFSET ReturnUnwind
	  PUSH 0
	  PUSH 0
	  CALL RtlUnwind
ReturnUnwind:
	  .....

If no EXCEPTION_RECORD is passed, RtlUnwind makes a default exception
record. In any case, the ExceptionFlags part of this record has the
EH_UNWINDING (=2),  flag set. (and EH_EXIT_UNWIND (=4), when NULL is passed as the end
frame.).

The handler for a exception as well as a for unwinds may be executed in the
thread causing the exception, but may also be executed in another (special
exception) thread. So it is not wise to make any assumptions about that!

As an alternative you may consider the SetUnhandledExceptionFilter API
to install your own exception filter. This one is documented.
*/

/* The January 1994 MSJ has an article entitled "Clearer, More Comprehensive
   Error Processing with Win32 Structured Exception Handling".  It goes into
   a teensy bit of detail of the innards of exception handling (i.e. what we
   have to do).  */

typedef int (exception_handler)
     (EXCEPTION_RECORD *, void *, CONTEXT *, void *);

typedef struct _exception_list
{
  struct _exception_list *prev;
  exception_handler *handler;

  /* We're apparently free to add more stuff here.
     At present we don't need any.  */
} exception_list;

void init_exceptions (exception_list *);

#ifdef __cplusplus
};
#endif /* __cplusplus */

#endif /* _EXCEPTIONS_H */

/******************** end exceptions.h ********************/


#define SELF_VPROC      (VProc[0])

/* generic handler for cygwin "signals" such as interrupt, alarm */
/* returns TRUE if the main thread is running ML code */
BOOL cygwin_generic_handler(int code)
{
   vproc_state_t   *vsp = SELF_VPROC;

   vsp->vp_sigCounts[code].nReceived++;
   vsp->vp_totalSigCount.nReceived++;

   vsp->vp_limitPtrMask = 0;

   if (vsp->vp_inMLFlag &&
      (! vsp->vp_handlerPending) &&
      (! vsp->vp_inSigHandler))
   {
      vsp->vp_handlerPending = TRUE;
      SIG_ZeroLimitPtr();
      return TRUE;
   }
   return FALSE;
}

PVT BOOL __stdcall ctrl_c_handler(DWORD type)
{
   switch (type)
   {
      case CTRL_C_EVENT:
      case CTRL_BREAK_EVENT:
         if (cygwin_generic_handler(SIGINT)) {
            return TRUE;
         }
         return FALSE;
      default:
         return FALSE;
   }
}

void InitFaultHandlers(ml_state_t * msp)
{
   /* Install the control-C handler */
   if (! SetConsoleCtrlHandler(ctrl_c_handler, TRUE))
   {
      Die("cygwin:InitFaultHandlers: can't install ctrl-c-handler\n");
   }
   /* Initialize the floating-point unit */
   SIG_InitFPE ();
}

/*
 * This filter catches all exceptions.
 */
PVT int page_fault_handler
   (EXCEPTION_RECORD * exn, void * foo, CONTEXT * c, void * bar)
{
   extern Word_t request_fault[];
   ml_state_t * msp = SELF_VPROC->vp_state;
   int code = exn->ExceptionCode;
   DWORD pc = (DWORD)exn->ExceptionAddress;

   if (! SELF_VPROC->vp_inMLFlag) {
      Die("cygwin:fault_handler: bogus fault not in ML: %#x\n", code);
   }

   switch (code) {
      case EXCEPTION_INT_DIVIDE_BY_ZERO:
      case EXCEPTION_INT_OVERFLOW:
	/* all arithmetic exceptions get mapped to Overflow, since the compiler
	 * generates code to check for divide by zero.
	 */
         /* Say("Overflow at %p\n", pc); */
	msp->ml_faultPC  = pc;
	c->Eip = (DWORD)request_fault;
	break;
      default:
	Die("cygwin:fault_handler: unexpected fault @%#x, code=%#x", pc, code);
   }
   return FALSE;
}

asm (".equ __win32_exception_list,0");
extern exception_list *
   _win32_exception_list asm ("%fs:__win32_exception_list");

/*
 * This overrides the default RunML.
 * It just adds a new exception handler at the very beginning before
 * ML is executed.
 */
void RunML(ml_state_t * msp)
{
   extern void SystemRunML(ml_state_t *);

   exception_list el;
   el.handler = page_fault_handler;
   el.prev    = _win32_exception_list;
   _win32_exception_list = &el;
   return SystemRunML(msp);
}

#endif
