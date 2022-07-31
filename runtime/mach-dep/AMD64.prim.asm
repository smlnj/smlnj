/*! \file AMD64.prim.asm
 *
 * COPYRIGHT (c) 2020 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "x86-syntax.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-request.h"
#include "mlstate-offsets.h"	/** this file is generated **/
#include "ml-limits.h"

#if defined(OPSYS_LINUX) && defined(__ELF__)
/* needed to disable the execution bit on the stack pages */
.section .note.GNU-stack,"",%progbits
#endif

/*
 * AMD64 function call conventions (System V ABI):
 *
 * 	Caller save registers: rax, rcx, rdx, rsi, rdi, r8-r11
 * 	Callee save registers: rbx, rbp, r12-15.
 *	Save frame pointer (rbp) first to match standard function prelude
 * 	Floating point state is caller-save.
 * 	The first six integer arguments are passed in registers: rdi, rsi,
 *	    rdx, rcx, r8, and r9.  Additional arguments are passed on the
 *	    stack (rightmost argument pushed first).
 * 	Word-sized result returned in %rax.
 *	The stack frame must be multiple of 16 bytes
 */

/* Registers (see compiler/CodeGen/amd64/amd64CpsRegs.sml): */
#define temp		RAX
#define misc0		RBX     /* callee save */
#define misc1		RCX     /* callee save */
#define misc2		RDX     /* callee save */
#define misc3		R10
#define misc4		R11
#define misc5		R12
#define misc6		R13
#define stdcont		RSI
#define stdarg		RBP
#define	stdlink		R8
#define	stdclos		R9
#define allocptr	RDI
#define limitptr	R14
#define storeptr	R15
#define stackptr        RSP

/* other reg uses */
#define creturn 	RAX

/* Stack frame offsets are w.r.t. the stack pointer.  See
 *
 *	dev-notes/stack-layout.numbers
 *
 * for details.
 */
#define negateSignBit	REGOFF(8264,RSP)
#define signBit		REGOFF(8256,RSP)
#define overflowFn	REGOFF(8248,RSP)
#define start_gc	REGOFF(8240,RSP)	/* holds address of saveregs */
#define varptr		REGOFF(8232,RSP)
#define exncont		REGOFF(8224,RSP)
#define baseptr		REGOFF(8216,RSP)	/* start address of module */
#define tempmem0	REGOFF(8192,RSP)
#define pc		REGOFF(8208,RSP)	/* aka gcLink */
#define mlStatePtr	REGOFF(8200,RSP)

/* space reserved for spilling registers */
#define ML_SPILL_SIZE	8192

/* size of stack-frame region where ML stuff is stored. */
#define ML_AREA_SIZE	80

/* the amount to bump up the frame after the callee save registers have been
 * pushed onto the stack.
 */
#define ML_FRAME_SIZE	(ML_SPILL_SIZE+ML_AREA_SIZE)

/* we put the request code in tempmem before jumping to set_request */
#define request_w	tempmem0

/* NOTE: this include must come after the definition of stdlink, etc. */
#include "x86-macros.h"

/* word-size related immediate operands */
#define WORD_SZB_IM	IM(8)
#define WORD_SHFT_IM	IM(3)

/**********************************************************************/
	TEXT

/* sigh_return:
 */
ALIGNED_ENTRY(sigh_return_a)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	MOV	(IM(REQ_SIG_RETURN), request_w)
	JMP	(CSYM(set_request))

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */
ALIGNED_ENTRY(sigh_resume)
	MOV	(IM(REQ_SIG_RESUME), request_w)
	JMP	(CSYM(set_request))

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ALIGNED_ENTRY(pollh_return_a)
	MOV	(IM(REQ_POLL_RETURN), request_w)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	JMP	(CSYM(set_request))

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ALIGNED_ENTRY(pollh_resume)
	MOV	(IM(REQ_POLL_RESUME), request_w)
	JMP	(CSYM(set_request))

/* handle:
 */
ALIGNED_ENTRY(handle_a)
	MOV	(IM(REQ_EXN), request_w)
	MOVE	(stdlink,temp,pc)
	JMP	(CSYM(set_request))

/* return:
 */
ALIGNED_ENTRY(return_a)
	MOV	(IM(REQ_RETURN), request_w)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	JMP	(CSYM(set_request))

/* Request a fault. */
ALIGNED_ENTRY(request_fault)
	MOV	(IM(REQ_FAULT), request_w)
	MOVE	(stdlink,temp,pc)
	JMP	(CSYM(set_request))

/* Raise the Overflow exception; note that the top of the stack contains an
 * address in the code object containing the raising code.
 */
ALIGNED_ENTRY(raise_overflow)
	POP	(pc)
	MOV	(IM(REQ_RAISE_OVERFLOW), request_w)
	JMP	(CSYM(set_request))

/* bind_cfun : (string * string) -> c_function
 */
ALIGNED_ENTRY(bind_cfun_a)
	CHECKLIMIT
	MOV	(IM(REQ_BIND_CFUN), request_w)
	JMP	(CSYM(set_request))

/* build_literals:
 */
ALIGNED_ENTRY(build_literals_a)
	CHECKLIMIT
	MOV	(IM(REQ_BUILD_LITERALS), request_w)
	JMP	(CSYM(set_request))

/* callc:
 */
ALIGNED_ENTRY(callc_a)
	CHECKLIMIT
	MOV	(IM(REQ_CALLC), request_w)
	JMP	(CSYM(set_request))

/* saveregs:
 * Entry point for GC.  Control is transfered using a `call` instruction,
 * so the return address is on the top of the stack.
 */
ALIGNED_ENTRY(saveregs)
	POP	(pc)
	MOV	(IM(REQ_GC), request_w)
	/* fall into set_request */

/* set_request:
 * common code to switch execution from SML to runtime system.  The request
 * code will be in `tempmem` (on the stack).
 */
ENTRY(set_request)
        /* here the request is in request_w */

	/* Save registers in MLState struct (temp is pointer to struct) */
	MOV	(mlStatePtr, temp)
	MOV	(allocptr, REGOFF(AllocPtrOffMSP,temp))
	MOV	(stdarg, REGOFF(StdArgOffMSP,temp))
	MOV	(stdcont, REGOFF(StdContOffMSP,temp))

#define	temp2 allocptr
	/* note that we have left ML code */
	MOV	(REGOFF(VProcOffMSP,temp), temp2)
	MOV	(IM(0), REGOFF(InMLOffVSP, temp2))

	/* Save stack-allocated CPS registers before the stack frame is popped. */
	MOVE	(exncont, temp2, REGOFF(ExnPtrOffMSP, temp))
	MOVE	(varptr,  temp2, REGOFF(VarPtrOffMSP, temp))
	MOVE	(pc,      temp2, REGOFF(PCOffMSP, temp))
#undef	temp2

	/* Save remaining registers */
	MOV	(limitptr, REGOFF(LimitPtrOffMSP, temp))
	MOV	(storeptr, REGOFF(StorePtrOffMSP, temp))
	MOV	(stdclos,  REGOFF(StdClosOffMSP, temp))
	MOV	(stdlink,  REGOFF(LinkRegOffMSP, temp))
	MOV	(misc0,    REGOFF(Misc0OffMSP, temp))
	MOV	(misc1,    REGOFF(Misc1OffMSP, temp))
	MOV	(misc2,    REGOFF(Misc2OffMSP, temp))

	/* return val of function is request code */
	MOV(request_w,creturn)

	/* Pop the stack frame and return to run_ml(). */
	ADD	(IM(ML_FRAME_SIZE), RSP)

	/* restore C callee-save registers */
	POP	(R15)
	POP	(R14)
	POP	(R13)
	POP	(R12)
	POP	(RBX)
	POP	(RBP)
	RET

/**********************************************************************/

/* restoreregs (ml_state_t *msp):
 *
 * Switch from C to SML.
 */
#ifdef OPSYS_WIN32
/* on Windows, `restoreregs` is a C wrapper around `asm_restoreregs` that
 * handles traps (see `runtime/mach-dep/win32-fault.c`)
 */
ALIGNED_ENTRY(asm_restoreregs)
#else
ALIGNED_ENTRY(restoreregs)
#endif
	/* save C callee-save registers */
	PUSH	(RBP)
	PUSH	(RBX)
	PUSH	(R12)
	PUSH	(R13)
	PUSH	(R14)
	PUSH	(R15)
	/* allocate the rest of the stack frame */
	SUB	(IM(ML_FRAME_SIZE), RSP)

	/* move the argument (MLState ptr) to the temp register */
	MOV	(RDI, temp)

#define temp2	RBX
      /* Initialize the ML stack frame. */
	MOVE	(REGOFF(ExnPtrOffMSP, temp), temp2, exncont)
	MOVE	(REGOFF(VarPtrOffMSP, temp), temp2, varptr)
	MOVE    (REGOFF(PCOffMSP, temp),     temp2, pc)
      /* Store address of code to raise the "Overflow" exception in stack */
	LEA	(CODEADDR(CSYM(saveregs)), temp2)
	MOV	(temp2, start_gc)
	LEA	(CODEADDR(CSYM(raise_overflow)),temp2)
	MOV	(temp2, overflowFn)
      /* Store bitmasks to support floating-point "neg" and "abs" in stack */
	MOV	($0x8000000000000000, temp2)
	MOV	(temp2, signBit)
	MOV	($0x7fffffffffffffff, temp2)
	MOV	(temp2, negateSignBit)
#undef	temp2

	/* Load ML registers. */
	MOV	(temp, mlStatePtr)
	MOV	(REGOFF(AllocPtrOffMSP, temp), allocptr)
	MOV	(REGOFF(LimitPtrOffMSP, temp), limitptr)
	MOV	(REGOFF(StorePtrOffMSP, temp), storeptr)
	MOV	(REGOFF(LinkRegOffMSP, temp),  stdlink)
	MOV	(REGOFF(StdClosOffMSP, temp),  stdclos)
	MOV	(REGOFF(StdContOffMSP, temp),  stdcont)
	MOV	(REGOFF(StdArgOffMSP, temp),   stdarg)
	MOV	(REGOFF(Misc0OffMSP, temp),    misc0)
	MOV	(REGOFF(Misc1OffMSP, temp),    misc1)
	MOV	(REGOFF(Misc2OffMSP, temp),    misc2)

	PUSH	(misc2)			/* free up a register   */
	PUSH	(temp)			/* save msp temporarily */

#define	tmpreg	misc2

	/* note that we are entering ML */
	MOV	(REGOFF(VProcOffMSP,temp), temp)  /* temp is now vsp */
#define vsp	temp
	MOV	(IM(1),REGOFF(InMLOffVSP,vsp))

	/* handle signals */
	MOV	(REGOFF(SigsRecvOffVSP,vsp),RDX)
	CMP	(REGOFF(SigsHandledOffVSP,vsp),RDX)

#undef  tmpreg
	JNE	(pending)

restore_and_jmp_ml:
	POP	(temp)			/* restore temp to msp */
	POP	(misc2)

jmp_ml:
/* FIXME: we may be able to get rid of this CMP with the LLVM code generator */
	CMP	(limitptr, allocptr)
	JMP	(CODEPTR(REGOFF(PCOffMSP,temp)))	/* Jump to ML code. */

pending:
					/* Currently handling signal? */
	CMP	(IM(0), REGOFF(InSigHandlerOffVSP,vsp))
	JNE	(restore_and_jmp_ml)
					/* handler trap is now pending */
	MOV	(IM(1),HandlerPendingOffVSP(vsp))

	/* must restore here because limitptr is on stack */ /* XXX */
	POP	(temp)			/* restore temp to msp */
	POP	(misc2)

	MOV	(allocptr,limitptr)
	JMP	(jmp_ml)			/* Jump to ML code. */
#undef  vsp

/* ----------------------------------------------------------------------
 * array : (int * 'a) -> 'a array
 * Allocate and initialize a new array.	 This can cause GC.
 */
ALIGNED_ENTRY(array_a)
	CHECKLIMIT
	MOV	(REGIND(stdarg),temp)		/* temp := length in words */
	SAR	(IM(1),temp)			/* temp := length untagged */
	CMP	(IM(SMALL_OBJ_SZW),temp)	/* small object? */
	JGE	(L_array_large)
	/* use misc5 and misc6 as temporary registers */
#define temp1 misc5
#define temp2 misc6
	/* build data object descriptor in temp1 */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_arr_data)),temp1)
	/* store descriptor and bump allocation pointer */
	MOV	(temp1,REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)
	/* allocate and initialize data object */
	MOV	(allocptr,temp1)		/* temp1 := array data ptr */
	MOV	(REGOFF(8,stdarg),temp2)	/* temp2 := initial value */
LABEL(L_array_lp)
	MOV	(temp2,REGIND(allocptr))	/* init array */
	ADD	(WORD_SZB_IM,allocptr)
	SUB	(IM(1),temp)
	JNE	(L_array_lp)
	/* Allocate array header */
	MOV	(IM(DESC_polyarr),REGIND(allocptr)) /* descriptor */
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(REGIND(stdarg),temp)		/* temp := length */
	MOV	(allocptr, stdarg)		/* result := header addr */
	MOV	(temp1, REGIND(allocptr))	/* store pointer to data */
	MOV	(temp, REGOFF(8,allocptr))	/* store length */
	ADD	(IM(16),allocptr)
	CONTINUE
#undef temp1
#undef temp2

	/* large arrays are allocated in the runtime system */
LABEL(L_array_large)
	MOV	(stdlink,pc)
	MOV	(IM(REQ_ALLOC_ARRAY),request_w)
	JMP	(CSYM(set_request))


/* create_r : int -> realarray */
ALIGNED_ENTRY(create_r_a)
	CHECKLIMIT
	MOV	(stdarg,temp)		/* temp := length */
	SAR	(IM(1),temp)		/* temp := untagged length in words */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_r_large)

#define temp1 misc3

	/* allocate the data object */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)		/* temp1 := descriptor */
	OR	(IM(MAKE_TAG(DTAG_raw64)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(WORD_SZB_IM,allocptr)		/* allocptr++ */
	MOV	(allocptr,temp1)		/* temp1 := data object */
	SAL	(WORD_SHFT_IM,temp)		/* temp := length in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */

	/* allocate the header object */
	MOV	(IM(DESC_real64arr),REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)		/* allocptr++ */
	MOV	(temp1,REGIND(allocptr))	/* header data */
	MOV	(stdarg,REGOFF(8,allocptr))	/* header length */
	MOV	(allocptr,stdarg)		/* stdarg := header obj */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */

	CONTINUE
#undef temp1

LABEL(L_create_r_large)
	MOV	(stdlink,pc)
	MOV	(IM(REQ_ALLOC_REALDARRAY),request_w)
	JMP	(CSYM(set_request))


/* create_b : int -> bytearray */
ALIGNED_ENTRY(create_b_a)
	CHECKLIMIT
	MOV	(stdarg,temp)			/* temp is tagged length */
	SAR	(IM(1),temp)			/* temp >>= 1; (untag length) */
	ADD	(IM(7),temp)			/* temp += 7; */
	SAR	(WORD_SHFT_IM,temp)		/* temp >>= 3; (length in 8-byte words) */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_b_large)

#define temp1 misc3

	/* allocate the data object */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_raw)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(allocptr,temp1)		/* temp1 is data object */
	SAL	(WORD_SHFT_IM,temp)		/* temp is size in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */

	/* allocate the header object */
	MOV	(IM(DESC_word8arr),REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(temp1,REGIND(allocptr))
	MOV	(stdarg,REGOFF(8,allocptr))
	MOV	(allocptr,stdarg)		/* stdarg := header */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */
	CONTINUE
#undef temp1

LABEL(L_create_b_large)
	MOV	(stdlink,pc)
	MOV	(IM(REQ_ALLOC_BYTEARRAY),request_w)
	JMP	(CSYM(set_request))


/* create_s : int -> string */
ALIGNED_ENTRY(create_s_a)
	CHECKLIMIT
	MOV	(stdarg,temp)
	SAR	(IM(1),temp)			/* untag length */
	ADD	(IM(8),temp)			/* 7 + extra byte */
	SAR	(WORD_SHFT_IM,temp)		/* length in words */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_s_large)

#define temp1 misc3

	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_raw)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(WORD_SZB_IM,allocptr)

	MOV	(allocptr,temp1)		/* temp1 is data obj */
	SAL	(WORD_SHFT_IM,temp)		/* length in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */
	MOV	(IM(0),REGOFF((-8),allocptr))	/* zero out last word */

	/* allocate header obj */
	MOV	(IM(DESC_string),temp)	/* hdr descr */
	MOV	(temp,REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(temp1,REGIND(allocptr))	/* hdr data */
	MOV	(stdarg,REGOFF(8,allocptr))	/* hdr length */
	MOV	(allocptr, stdarg)		/* stdarg is hdr obj */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */

#undef temp1
	CONTINUE

LABEL(L_create_s_large)
	MOVE	(stdlink, temp, pc)
	MOV	(IM(REQ_ALLOC_STRING),request_w)
	JMP	(CSYM(set_request))

/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ALIGNED_ENTRY(create_v_a)
	CHECKLIMIT
	MOV	(REGIND(stdarg),temp)		/* temp = len tagged */
#define temp1 misc3
#define temp2 misc4

	MOV	(temp,temp1)
	SAR	(IM(1),temp1)			/* temp1 = untagged len */
	CMP	(IM(SMALL_OBJ_SZW),temp1)
	JGE	(L_create_v_large)

	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_vec_data)),temp1)
	MOV	(temp1,REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(REGOFF(8,stdarg),temp1)	/* temp1 is list */
	MOV	(allocptr,stdarg)		/* stdarg is vector */

LABEL(L_create_v_lp)
	MOV	(REGIND(temp1),temp2)		/* hd */
	MOV	(temp2,REGIND(allocptr))	/* store into vector */
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(REGOFF(8,temp1),temp1)		/* tl */
	CMP	(IM(ML_nil),temp1)		/* isNull? */
	JNE	L_create_v_lp

	/* allocate header object */
	MOV	(IM(DESC_polyvec),temp1)
	MOV	(temp1,REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(stdarg,REGIND(allocptr))	/* data */
	MOV	(temp,REGOFF(8,allocptr))	/* len */
	MOV	(allocptr,stdarg)		/* result */
	ADD	(IM(16),allocptr)		/* allocptr += 2 */

	CONTINUE
#undef temp1
#undef temp2

LABEL(L_create_v_large)
	MOVE	(stdlink, temp, pc)
	MOV	(IM(REQ_ALLOC_VECTOR),request_w)
	JMP	(CSYM(set_request))

/* MP support is deprecated, but we need to these global symbols for linking */
ALIGNED_ENTRY(try_lock_a)
ALIGNED_ENTRY(unlock_a)
	CONTINUE


/********************* Floating point functions. *********************/

/* rounding modes (see Table 4-14 in the Instruction Set Reference) */
#define	RND_TO_NEGINF	IM(9)
#define RND_TO_POSINF	IM(10)
#define RND_TO_ZERO	IM(11)

	TEXT

/* floor : real -> int
 * Return the nearest integer that is less or equal to the argument.
 * Caller's responsibility to make sure arg is in range.
 */
ALIGNED_ENTRY(floor_a)
	MOVSD		(REGIND(stdarg), XMM0)
	ROUNDSD		(RND_TO_NEGINF, XMM0, XMM0)
	CVTTSD2SI	(XMM0, stdarg)
	SAL		(IM(1),stdarg)	/* convert result to tagged representation */
	INC		(stdarg)
	CONTINUE

/* DEPRECATED, but required for linking */
ALIGNED_ENTRY(logb_a)
	CONTINUE

#define SIGN_MASK	IM(0x8000000000000000)
#define EXP_MASK	IM(0x7ff0000000000000)
#define NOT_EXP_MASK	IM(0x800fffffffffffff)

/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.
 * Note that if we were guaranteed AVX512 support, then we could use
 * the VSCALEFSD instruction, but since we are not, we implement this
 * using integer operations.
 */
ALIGNED_ENTRY(scalb_a)
	CHECKLIMIT
	MOV	(REGOFF(8,stdarg), temp)	/* get second arg */
	SAR	(IM(1), temp)			/* untag second arg */
	MOV	(REGIND(stdarg), stdarg)	/* put pointer to real in stdarg */
#define temp1 misc3
#define temp2 misc4
#define temp3 misc5
	MOV	(REGIND(stdarg), temp1)		/* put bits in temp1 */
	MOV	(EXP_MASK, temp3)               /* temp3 := EXP_MASK */
        MOV     (temp3, temp2)                  /* temp2 := temp3 */
	AND	(temp1, temp2)			/* temp2 has shifted exponent */
	JE	(L_scalb_return)		/* if temp2 == 0 then return first arg */
	SAR	(IM(52), temp2)
	ADD	(temp, temp2)			/* temp2 = exponent + scale */
	JLE	(L_scalb_under)
	CMP	(IM(2047), temp2)
	JGE	(L_scalb_over)
	MOV	(NOT_EXP_MASK, temp)		/* clear exponent field in original number */
	AND	(temp, temp1)
	SAL	(IM(52), temp2)			/* shift exponent into position */
	OR	(temp2, temp1)			/* temp1 := temp1 | temp2 */

L_scalb_alloc:
	MOV	(IM(DESC_reald),temp)		/* hdr descr */
	MOV	(temp,REGIND(allocptr))
	ADD	(WORD_SZB_IM,allocptr)
	MOV	(temp1,REGIND(allocptr))	/* data = temp1 */
	MOV	(allocptr, stdarg)		/* stdarg is result */
	ADD	(WORD_SZB_IM,allocptr)		/* allocptr += 1 */

L_scalb_return:
	CONTINUE

L_scalb_under:
	XOR	(temp1,temp1)			/* temp1 = 0 */
	JMP	(L_scalb_alloc)

L_scalb_over:
        /* here we have an overflow; temp1 contains the bits */
        MOV     (SIGN_MASK, temp2)              /* temp2 := bits & SIGN_MASK */

        AND     (temp2, temp1)                  /* temp1 := sign(bits) */
        OR      (temp3, temp1)                  /* temp1 := temp1 | exponent(2047) */
        JMP	(L_scalb_alloc)
#undef temp1
#undef temp2
#undef temp3

END

/* end of AMD64.prim.asm */
