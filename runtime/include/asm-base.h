/*! \file asm-base.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Common definitions for assembly files in the SML/NJ system.
 * Note that we do not include this file in either X86.prim.asm
 * or AMD64.prim.asm; instead we use x86-syntax.h for those files.
 */

#ifndef _ASM_BASE_
#define _ASM_BASE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/* bool_t values for assembly code */
#define FALSE	0
#define TRUE	1

#if (!defined(GLOBALS_HAVE_UNDERSCORE)) && (((defined(OPSYS_FREEBSD) || defined(OPSYS_NETBSD2) || defined(OPSYS_OPENBSD)) && !defined(__ELF__)) || defined(OPSYS_WIN32) || defined(OPSYS_DARWIN) || defined(OPSYS_CYGWIN))
#  define GLOBALS_HAVE_UNDERSCORE
#endif

/* we should probably consider factoring this out into ml-unixdep.h -- JHR */
#ifdef GLOBALS_HAVE_UNDERSCORE
#  define CSYM(ID)	CONCAT(_,ID)
#else
#  define CSYM(ID)	ID
#endif

#if defined(ARCH_SPARC)
#  if defined(OPSYS_SOLARIS)
#    define _ASM
#    include <sys/stack.h>
#    include <sys/trap.h>
#  endif
#  define GLOBAL(ID)	.global	ID
#  define LABEL(ID)	ID:
#  define ALIGN4        .align 4
#  define WORD(W)       .word W
#  define TEXT		.seg "text"
#  define DATA		.seg "data"
#  define BEGIN_PROC(P)
#  define END_PROC(P)

#elif defined(ARCH_PPC)
#  if defined(OPSYS_AIX)
#    define CFUNSYM(ID)	CONCAT(.,ID)
#    define USE_TOC
#    define GLOBAL(ID)	.globl ID
#    define TEXT	.csect [PR]
#    define DATA	.csect [RW]
#    define RO_DATA	.csect [RO]
#    define ALIGN4	.align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)   ID:

#  elif defined(OPSYS_LINUX)
#    define CFUNSYM(ID)	ID
#    define GLOBAL(ID)	.globl ID
#    define TEXT	.section ".text"
#    define DATA	.section ".data"
#    define RO_DATA	.section ".rodata"
#    define ALIGN4	.align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:

#  elif defined(OPSYS_DARWIN)
#    define CFUNSYM(ID) CSYM(ID)
#    define GLOBAL(ID)  .globl  ID
#    define TEXT        .text
#    define DATA        .data
#    define RO_DATA     .data
#    define ALIGN4      .align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:
#    define __SC__      @

#  elif defined(OPSYS_OPENBSD)
#    define CFUNSYM(ID) CSYM(ID)
#    define GLOBAL(ID)  .globl  ID
#    define TEXT        .text
#    define DATA        .data
#    define RO_DATA     .data
#    define ALIGN4      .align 2
#    define ALIGN8	.align 3
#    define DOUBLE(V)	.double V
#    define LABEL(ID)	ID:
#  endif

#  define CENTRY(ID)		\
    .globl CFUNSYM(ID) __SC__	\
    LABEL(CFUNSYM(ID))

#elif defined(ARCH_X86) || defined(ARCH_AMD64)
#  error use x86-syntax.h instead if ml-base.h

#elif defined(ARCH_ARM64)
#  if defined(OPSYS_DARWIN)
#    define CFUNSYM(ID) CSYM(ID)
#    define GLOBAL(ID)  .globl  ID
#    define TEXT        .text
#    define DATA        .data
#    define RO_DATA     .data
#    define ALIGN4      .align 2
#    define ALIGN8	.align 3
#    define LABEL(ID)	ID:
#    define IM(x)	CONCAT(#,x)
#    define __SC__      ;
#  else
#    error missing asm definitions for arm64
#  endif

#else
#  error missing asm definitions

#endif

#ifndef __SC__
#  define __SC__ 	;
#endif

#ifndef ALIGN_CODE
#  define ALIGN_CODE	ALIGN4
#endif

#  define CGLOBAL(ID)	GLOBAL(CSYM(ID))

#define ENTRY(ID)				\
    CGLOBAL(ID) __SC__				\
    LABEL(CSYM(ID))

#define ALIGNED_ENTRY(name)			\
	    CGLOBAL(name) __SC__		\
	    ALIGN4 __SC__			\
    LABEL(CSYM(name))

#define ALIGNED_LABEL(name)			\
	    ALIGN4 __SC__			\
    LABEL(name)

#endif /* !_ASM_BASE_ */

