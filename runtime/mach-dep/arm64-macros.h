/*! \file arm64-macros.h
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * These are macros for the Arm64 (aka AArch64) assembly code.  They need to be
 * defined after the CPS register names are defined so that they are expanded
 * correctly.
 */

#ifndef _ARM64_MACROS_H_
#define _ARM64_MACROS_H_

#define TEXT		.section __TEXT,__text,regular,pure_instructions

/* global symbols have a leading "_" */
#  define CSYM(ID)	CONCAT(_,ID)

/* syntax of immediate values */
#define IM(x)	CONCAT(#,x)

/* alignment for code entrypoints */
#define ALIGN_CODE	.p2align 2

/* macro for loading a code address into a register, which requires system-dependent
 * assembly syntax for first loading the page address of the symbol and then loading
 * the page offset.
 */
#if defined(OPSYS_DARWIN)
/* on Darwin, we can include a linker-optimization hint to get the expected single
 * instruction in the object code (see Target/AArch64/AArch64CollectLOH.cpp in LLVM)
 */
.macro	m_load_addr reg:req, addr:req
	L_\addr\()_adrp:	adrp \reg\(), \addr\()@PAGE
	L_\addr\()_adr:		add \reg\(), \reg\(), \addr\()@PAGEOFF
				.loh AdrpAdd L_\addr\()_adrp, L_\addr\()_adr
.endm
#else
#  error unsupported system for Arm64
#endif

/* macro for an aligned local code block */
.macro	m_aligned_label name:req
		ALIGN_CODE
	\name:
.endm

/* macro for an aligned global code entry */
.macro	m_aligned_entry name:req
		.globl CSYM(\name)
		ALIGN_CODE
	CSYM(\name):
.endm

/* macro for global code address */
.macro	m_entry name:req
		.globl CSYM(\name)
	CSYM(\name):
.endm

/* macro for returning from an SML compatible function via a return continuation */
.macro	m_continue
		br	xcont
.endm

#define ENTRY(ID)		m_entry ID
#define ALIGNED_ENTRY(ID)	m_aligned_entry ID
#define ALIGNED_LABEL(ID)	m_aligned_label ID
#define LOAD_ADDR(REG,ADR)	m_load_addr REG, ADR
#define CONTINUE                m_continue

/* reference memory at address `base + offset` */
#define MEM(base,offset)        [base, IM(offset)]
/* reference a stack location */
#define STK(offset)             MEM(sp,offset)
/* pre-increment memory reference; address is base + offset and base := base + offset */
#define PREINC(base,offset)     [base, IM(offset)]!
/* post-increment memory reference; address is base and base := base + offset */
#define POSTINC(base,offset)     [base], IM(offset)

#endif /* !_ARM64_MACROS_H_ */
