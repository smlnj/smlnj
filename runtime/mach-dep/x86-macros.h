/*! \file x86-macros.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * These are macros for the X86 and AMD64 assembly code.  They need to be
 * defined after the CPS register names are defined so that the Win32 macros
 * are correct.
 */

#ifndef _X86_MACROS_H_
#define _X86_MACROS_H_

/* jump to the address held in the standard continuation register */
#define CONTINUE	JMP (CODEPTR(stdcont))

/* CHECKLIMIT, ENTRY, and ML_CODE_HDR macros */
#ifdef MASM_ASSEMBLER

CHECKLIMIT_M MACRO
 @@:
	MOVE	(stdlink, temp, pc)
	CMP	(limitptr, allocptr)
	jb	@f
	CALL	(CSYM(saveregs))
	JMP	@b
 @@:
ENDM

ENTRY_M MACRO id
	GLOBAL	(CSYM(&id))
	LABEL	(CSYM(&id))
ENDM

ALIGNED_ENTRY_M MACRO name
	GLOBAL	(CSYM(&name))
	ALIGN_CODE
	LABEL	(CSYM(&name))
ENDM

#define CHECKLIMIT CHECKLIMIT_M
#define ENTRY(id) ENTRY_M id
#define ALIGNED_ENTRY(name) ALIGNED_ENTRY_M name

#elif defined(GNU_ASSEMBLER)

#define CHECKLIMIT				\
 1:;						\
	MOVE	(stdlink, temp, pc);		\
	CMP	(limitptr, allocptr);		\
	JB	(9f);				\
	CALL	(CSYM(saveregs));		\
	JMP	(1b);				\
 9:

#define ENTRY(ID)				\
    CGLOBAL(ID);				\
    LABEL(CSYM(ID))

#define ALIGNED_ENTRY(name)			\
	    CGLOBAL(name);			\
	    ALIGN_CODE;				\
    LABEL(CSYM(name))

#else
#  error must specify either GNU_ASSEMBLER or MASM_ASSEMBLER
#endif /* MASM_ASSEMBLER */

#endif /* !_X86_MACROS_H_ */
