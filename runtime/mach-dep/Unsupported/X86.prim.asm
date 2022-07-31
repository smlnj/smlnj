/*! \file X86.prim.asm
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
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
 * 386 C function call conventions:
 *  [true for gcc and dynix3 cc; untested for others]
 *
 * 	Caller save registers: eax, ecx, edx
 * 	Callee save registers: ebx, esi, edi, and ebp.
 *	Save frame pointer (ebx) first to match standard function prelude
 * 	Floating point state is caller-save.
 * 	Arguments passed on stack.  Rightmost argument pushed first.
 * 	Word-sized result returned in %eax.
 *	On Darwin, stack frame must be multiple of 16 bytes
 *
 * The 386 registers are used in SML as follows:
 *
 * EAX - temp1 (see the code generator, x86/x86.sml)
 * EBX - misc0
 * ECX - misc1
 * EDX - misc2
 * ESI - standard continuation (ml_cont, see ml_state.h)
 * EBP - standard argument (ml_arg)
 * EDI - free space pointer (ml_allocptr)
 * ESP - stack pointer
 * EIP - program counter (ml_pc)
 */

/* SML registers (see compiler/CodeGen/x86/X86CpsRegs.sml): */
#define temp		EAX
#define misc0		EBX
#define misc1		ECX
#define misc2		EDX
#define stdcont		ESI
#define stdarg		EBP
#define allocptr	EDI
#define stackptr        ESP

/* C function result register */
#define creturn 	EAX

/* SML Stack frame layout: */
#define tempword1	REGOFF_W(0,ESP)
#define tempword2	REGOFF_W(2,ESP)
#define tempmem		REGIND(ESP)
#define baseptr		REGOFF(4,ESP)
#define exncont		REGOFF(8,ESP)
#define limitptr	REGOFF(12,ESP)
#define pc		REGOFF(16,ESP)	/* gcLink */
#define unused_1	REGOFF(20,ESP)
#define storeptr	REGOFF(24,ESP)
#define varptr		REGOFF(28,ESP)
#define start_gc	REGOFF(32,ESP)	/* holds address of saveregs */
#define unused_2	REGOFF(36,ESP)
#define eaxSpill	REGOFF(40,ESP) /* eax=0 */
#define	ecxSpill	REGOFF(44,ESP) /* ecx=1 */
#define	edxSpill	REGOFF(48,ESP) /* edx=2 */
#define	ebxSpill	REGOFF(52,ESP) /* ebx=3 */
#define	espSpill	REGOFF(56,ESP) /* esp=4 */
#define	ebpSpill	REGOFF(60,ESP) /* ebp=5 */
#define	esiSpill	REGOFF(64,ESP) /* esi=6 */
#define	ediSpill	REGOFF(68,ESP) /* edi=7 */
#define stdlink		REGOFF(72,ESP)
#define	stdclos		REGOFF(76,ESP)

#define esp_save	REGOFF(500,ESP)

#define ML_STATE_OFFSET 176
#define mlstate_ptr	REGOFF(ML_STATE_OFFSET,ESP)
#define SpillAreaStart	512	     /* starting offset */
#define ML_FRAME_SIZE	(8192)

/* NOTE: this include must come after the definition of stdlink, etc. */
#include "x86-macros.h"

/**********************************************************************/
#ifdef MASM_ASSEMBLER
	.386
	.MODEL FLAT
#endif

	DATA
	ALIGN4

	GLOBAL(CSYM(ML_X86Frame))
	/* global to hold ptr to the ml frame (gives C access to limitptr) */
	WORD(CSYM(ML_X86Frame))

/**********************************************************************/
	TEXT

/* use tempmem to hold the request word */
#define request_w	tempmem

/* sigh_return:
 */
ALIGNED_ENTRY(sigh_return_a)
	MOV(IM(ML_unit),stdlink)
	MOV(IM(ML_unit),stdclos)
	MOV(IM(ML_unit),pc)
	MOV(IM(REQ_SIG_RETURN),request_w)
	JMP(set_request)

/* sigh_resume:
 * Resume execution at the point at which a handler trap occurred.  This is a
 * standard two-argument function, thus the closure is in ml_cont.
 */
ALIGNED_ENTRY(sigh_resume)
	MOV(IM(REQ_SIG_RESUME),request_w)
	JMP(set_request)

/* pollh_return_a:
 * The return continuation for the ML poll handler.
 */
ALIGNED_ENTRY(pollh_return_a)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	MOV	(IM(REQ_POLL_RETURN),request_w)
	JMP	(set_request)

/* pollh_resume:
 * Resume execution at the point at which a poll event occurred.
 */
ALIGNED_ENTRY(pollh_resume)
	MOV	(IM(REQ_POLL_RESUME),request_w)
	JMP	(set_request)

/* handle:
 */
ALIGNED_ENTRY(handle_a)
	MOVE	(stdlink,temp,pc)
	MOV	(IM(REQ_EXN),request_w)
	JMP	(set_request)

/* return:
 */
ALIGNED_ENTRY(return_a)
	MOV	(IM(ML_unit),stdlink)
	MOV	(IM(ML_unit),stdclos)
	MOV	(IM(ML_unit),pc)
	MOV	(IM(REQ_RETURN),request_w)
	JMP	(set_request)

/* Request a fault.  The floating point coprocessor must be reset
 * (thus trashing the FP registers) since we don't know whether a
 * value has been pushed into the temporary "register".	 This is OK
 * because no floating point registers will be live at the start of
 * the exception handler.
 */
ALIGNED_ENTRY(request_fault)
	CALL	(CSYM(FPEEnable))
	MOVE	(stdlink,temp,pc)
	MOV	(IM(REQ_FAULT),request_w)
	JMP	(set_request)

/* bind_cfun : (string * string) -> c_function
 */
ALIGNED_ENTRY(bind_cfun_a)
	CHECKLIMIT
	MOV	(IM(REQ_BIND_CFUN),request_w)
	JMP	(set_request)

/* build_literals:
 */
ALIGNED_ENTRY(build_literals_a)
	CHECKLIMIT
	MOV	(IM(REQ_BUILD_LITERALS),request_w)
	JMP	(set_request)

/* callc:
 */
ALIGNED_ENTRY(callc_a)
	CHECKLIMIT
	MOV	(IM(REQ_CALLC),request_w)
	JMP	(set_request)

/* saveregs:
 * Entry point for GC.  Control is transfered using a `call` instruction,
 * so the return address is on the top of the stack.
 */
ALIGNED_ENTRY(saveregs)
	POP	(pc)
	MOV	(IM(REQ_GC),request_w)
	/* fall into set_request */

/* set_request:
 * common code to switch execution from SML to runtime system.  The request
 * code will be in `tempmem` (on the stack).
 */
LABEL(set_request)
	/* temp holds mlstate_ptr, valid request in tempmem  */
	/* Save registers */
	MOV	(mlstate_ptr, temp)
	MOV	(allocptr, REGOFF(AllocPtrOffMSP,temp))
	MOV	(stdarg, REGOFF(StdArgOffMSP,temp))
	MOV	(stdcont, REGOFF(StdContOffMSP,temp))

#define	temp2 allocptr
	/* note that we have left ML code */
	MOV	(REGOFF(VProcOffMSP,temp),temp2)
	MOV	(IM(0), REGOFF(InMLOffVSP,temp2))

	MOV	(misc0, REGOFF(Misc0OffMSP,temp))
	MOV	(misc1, REGOFF(Misc1OffMSP,temp))
	MOV	(misc2, REGOFF(Misc2OffMSP,temp))

	/* Save vregs before stack frame is popped. (?? - Blume) */
	MOVE	(limitptr,temp2, REGOFF(LimitPtrOffMSP,temp))
	MOVE	(exncont, temp2, REGOFF(ExnPtrOffMSP,temp))
	MOVE	(stdclos, temp2, REGOFF(StdClosOffMSP,temp))
	MOVE	(stdlink, temp2, REGOFF(LinkRegOffMSP,temp))
	MOVE	(pc,      temp2, REGOFF(PCOffMSP,temp))
	MOVE	(storeptr,temp2, REGOFF(StorePtrOffMSP,temp))
	MOVE	(varptr,  temp2, REGOFF(VarPtrOffMSP,temp))
#undef	temp2

	/* return val of function is request code */
	MOV	(request_w,creturn)

	/* Pop the stack frame */
#if defined(OPSYS_DARWIN)
	LEA	(REGOFF(ML_FRAME_SIZE+12,ESP),ESP)
#else
	MOV	(esp_save, ESP)
#endif
	/* restore C callee-save registers */
	POP	(EDI)
	POP	(ESI)
	POP	(EBX)
	POP	(EBP)
	/* return to run_ml() */
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
	/* put ML state pointer in temp */
	MOV	(REGOFF(4,ESP), temp)
	/* save C callee-save registers */
	PUSH	(EBP)
	PUSH	(EBX)
	PUSH	(ESI)
	PUSH	(EDI)
	/* save stack pointer */
#if defined(OPSYS_DARWIN)
      /* MacOS X frames must be 16-byte aligned.  We have 20 bytes on
       * the stack for the return PC and callee-saves, so we need a
       * 12-byte pad.
       */
	SUB(IM(ML_FRAME_SIZE+12), ESP)
#else
	/* Align sp on 8 byte boundary. Assumes that the stack
	 * starts out being at least word aligned. But who knows ...
	 */
	MOV	(ESP, EBX)
	OR	(IM(4), ESP)
	SUB	(IM(4), ESP)
	/* Allocate and initialize the ML stack frame. */
	SUB	(IM(ML_FRAME_SIZE), ESP)
	MOV	(EBX, esp_save)
#endif

#define temp2 EBX
	/* Initialize the ML stack frame. */
	MOVE	(REGOFF(ExnPtrOffMSP,temp),   temp2, exncont)
	MOVE	(REGOFF(LimitPtrOffMSP,temp), temp2, limitptr)
	MOVE	(REGOFF(StorePtrOffMSP,temp), temp2, storeptr)
	MOVE	(REGOFF(VarPtrOffMSP,temp),   temp2, varptr)
	LEA	(CSYM(saveregs), temp2)
	MOV	(temp2, start_gc)
	MOV	(temp, mlstate_ptr)

	/* vregs */
	MOVE	(REGOFF(LinkRegOffMSP,temp), temp2, stdlink)
	MOVE	(REGOFF(StdClosOffMSP,temp), temp2, stdclos)

	/* PC */
	MOVE	(REGOFF(PCOffMSP,temp), temp2,pc)
#undef temp2

	/* Load ML registers from the ML state */
	MOV	(REGOFF(AllocPtrOffMSP,temp), allocptr)
	MOV	(REGOFF(StdContOffMSP,temp), stdcont)
	MOV	(REGOFF(StdArgOffMSP,temp), stdarg)
	MOV	(REGOFF(Misc0OffMSP,temp), misc0)
	MOV	(REGOFF(Misc1OffMSP,temp), misc1)
	MOV	(REGOFF(Misc2OffMSP,temp), misc2)
	/* put stack pointer somewhere that signal handlers can get it */
	MOV	(ESP, CSYM(ML_X86Frame))

	PUSH	(misc2)			/* free up a register */
	PUSH	(temp)			/* save msp temporarily */

#define tmpreg misc2
	/* note that we're entering ML */
	MOV	(REGOFF(VProcOffMSP,temp),temp)	/* temp is now vsp */
#define vsp temp
	MOV	(IM(1),REGOFF(InMLOffVSP,vsp))

	/* check for any pending signals */
	MOV	(REGOFF(SigsRecvOffVSP,vsp), tmpreg)
	CMP	(REGOFF(SigsHandledOffVSP,vsp), tmpreg)
	JNE	(pending)
#undef tmpreg
	/* here there are no pending signals */
LABEL(restore_and_jmp_ml)
	/* restore temp to msp */
	POP	(temp)
	POP	(misc2)

LABEL(jmp_ml)
	CMP	(limitptr, allocptr)
	JMP	(CODEPTR(REGOFF(PCOffMSP, temp)))	/* jump to ML code */

	/* handle pending signals */
LABEL(pending)
	CMP	(IM(0),REGOFF(InSigHandlerOffVSP,vsp))
	JNE	(restore_and_jmp_ml)

	MOV	(IM(1),REGOFF(HandlerPendingOffVSP,vsp))

	/* must restore here because limitptr is on stack */
	POP	(temp)			/* restore temp to msp */
	POP	(misc2)

	MOV	(allocptr,limitptr)
	JMP	(jmp_ml)
#undef vsp

/**********************************************************************/

/* array : (int * 'a) -> 'a array
 *
 * Allocate and initialize a new array.	 This function can cause GC.
 */
ALIGNED_ENTRY(array_a)
	CHECKLIMIT
	MOV	(REGIND(stdarg),temp)		/* temp := length in words */
	SAR	(IM(1),temp)			/* temp := length untagged */
	CMP	(IM(SMALL_OBJ_SZW),temp)	/* small object? */
	JGE	(L_array_large)
	/* use misc0 and misc1 as temporary registers */
#define temp1 misc0
#define temp2 misc1
	PUSH	(misc0)
	PUSH	(misc1)
	/* build data object descriptor in temp1 */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)	/* build descriptor */
	OR	(IM(MAKE_TAG(DTAG_arr_data)),temp1)
	/* store descriptor and bump allocation pointer */
	MOV	(temp1,REGIND(allocptr))
	ADD	(IM(4),allocptr)
	/* allocate and initialize data object */
	MOV	(allocptr,temp1)		/* temp1 := array data ptr */
	MOV	(REGOFF(4,stdarg),temp2)	/* temp2 := initial value */
LABEL(L_array_lp)
	MOV	(temp2,REGIND(allocptr))	/* init array */
	ADD	(IM(4),allocptr)
	SUB	(IM(1),temp)
	JNE	(L_array_lp)
	/* Allocate array header */
	MOV	(IM(DESC_polyarr),REGIND(allocptr)) /* descriptor */
	ADD	(IM(4),allocptr)
	MOV	(REGIND(stdarg),temp)		/* temp := length */
	MOV	(allocptr, stdarg)		/* result := header addr */
	MOV	(temp1, REGIND(allocptr))	/* store pointer to data */
	MOV	(temp, REGOFF(4,allocptr))	/* store length */
	ADD	(IM(8),allocptr)
	/* restore misc0 and misc1 */
	POP	(misc1)
	POP	(misc0)
	CONTINUE
#undef temp1
#undef temp2

	/* large arrays are allocated in the runtime system */
LABEL(L_array_large)
	MOVE	(stdlink,temp,pc)
	MOV	(IM(REQ_ALLOC_ARRAY),request_w)
	JMP	(set_request)


/* create_r : int -> realarray
 *
 * Alocate an uninitialized packed array of 64-bit reals.
 */
ALIGNED_ENTRY(create_r_a)
	CHECKLIMIT
	MOV	(stdarg,temp)		/* temp := length */
	SAR	(IM(1),temp)		/* temp := untagged length */
	SAL	(IM(1),temp)		/* temp := length in words */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_r_large)

#define temp1 misc0
	PUSH	(misc0)			/* use misc0 as temp1 */

	OR	(IM(4),allocptr)	/* align allocptr on 32-bit x86 */

	/* allocate the data object */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)	/* temp1 := descriptor */
	OR	(IM(MAKE_TAG(DTAG_raw64)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(IM(4),allocptr)		/* allocptr++ */
	MOV	(allocptr,temp1)		/* temp1 := data object */
	SAL	(IM(2),temp)			/* temp := length in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */

	/* allocate the header object */
	MOV	(IM(DESC_real64arr),REGIND(allocptr))
	ADD	(IM(4),allocptr)		/* allocptr++ */
	MOV	(temp1,REGIND(allocptr))	/* header data */
	MOV	(stdarg,REGOFF(4,allocptr))	/* header length */
	MOV	(allocptr,stdarg)		/* stdarg := header obj */
	ADD	(IM(8),allocptr)		/* allocptr += 2 */

	POP	(misc0)
	CONTINUE
#undef temp1

LABEL(L_create_r_large)
	MOVE	(stdlink,temp,pc)
	MOV	(IM(REQ_ALLOC_REALDARRAY),request_w)
	JMP	(set_request)


/* create_b : int -> bytearray
 *
 * Allocate an uninitialized packed array of bytes.
 */
ALIGNED_ENTRY(create_b_a)
	CHECKLIMIT
	MOV	(stdarg,temp)		/* temp is tagged length */
	SAR	(IM(1),temp)		/* temp >>= 1; (untag length) */
	ADD	(IM(3),temp)		/* temp += 3; */
	SAR	(IM(2),temp)		/* temp >>= 2; (length in 4-byte words) */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_b_large)

#define temp1 misc0
	PUSH	(misc0)

	/* allocate the data object */
	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_raw)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(IM(4),allocptr)
	MOV	(allocptr,temp1)		/* temp1 is data object */
	SAL	(IM(2),temp)			/* temp is size in bytes */
	ADD	(temp,allocptr)			/* allocptr += length */

	/* allocate the header object */
	MOV	(IM(DESC_word8arr),REGIND(allocptr))
	ADD	(IM(4),allocptr)
	MOV	(temp1,REGIND(allocptr))
	MOV	(stdarg,REGOFF(4,allocptr))
	MOV	(allocptr,stdarg)		/* stdarg := header */
	ADD	(IM(8),allocptr)		/* allocptr += 2 */
	POP	(misc0)
	CONTINUE
#undef temp1

LABEL(L_create_b_large)
	MOVE	(stdlink,temp,pc)
	MOV	(IM(REQ_ALLOC_BYTEARRAY),request_w)
	JMP	(set_request)


/* create_s : int -> string
 *
 * Allocate an uninitialized packed vector of bytes.
 */
ALIGNED_ENTRY(create_s_a)
	CHECKLIMIT
	MOV	(stdarg,temp)
	SAR	(IM(1),temp)		/* untag */
	ADD	(IM(4),temp)		/* 3 + extra byte */
	SAR	(IM(2),temp)		/* length in words */
	CMP	(IM(SMALL_OBJ_SZW),temp)
	JGE	(L_create_s_large)

	PUSH	(misc0)
#define temp1 misc0

	MOV	(temp,temp1)
	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_raw)),temp1)
	MOV	(temp1,REGIND(allocptr))	/* store descriptor */
	ADD	(IM(4),allocptr)

	MOV	(allocptr,temp1)		/* temp1 is data obj */
	SAL	(IM(2),temp)			/* bytes len */
	ADD	(temp,allocptr)			/* allocptr += length */
	MOV	(IM(0),REGOFF((-4),allocptr)) /* zero out last word */

	/* allocate header obj */
	MOV	(IM(DESC_string),temp)	/* hdr descr */
	MOV	(temp,REGIND(allocptr))
	ADD	(IM(4),allocptr)
	MOV	(temp1,REGIND(allocptr))	/* hdr data */
	MOV	(stdarg,REGOFF(4,allocptr))	/* hdr length */
	MOV	(allocptr, stdarg)		/* stdarg is hdr obj */
	ADD	(IM(8),allocptr)		/* allocptr += 2 */

	POP	(misc0)
#undef temp1
	CONTINUE

LABEL(L_create_s_large)
	MOVE	(stdlink, temp, pc)
	MOV	(IM(REQ_ALLOC_STRING),request_w)
	JMP	(set_request)


/* create_v_a : int * 'a list -> 'a vector
 *	creates a vector with elements taken from a list.
 *	n.b. The frontend ensures that list cannot be nil.
 */
ALIGNED_ENTRY(create_v_a)
	CHECKLIMIT
	MOV	(REGIND(stdarg),temp)		/* temp = len tagged */
	PUSH	(misc0)
	PUSH	(misc1)
#define temp1 misc0
#define temp2 misc1
	MOV	(temp,temp1)
	SAR	(IM(1),temp1)			/* temp1 = untagged len */
	CMP	(IM(SMALL_OBJ_SZW),temp1)
	JGE	(L_create_v_large)


	SAL	(IM(TAG_SHIFTW),temp1)
	OR	(IM(MAKE_TAG(DTAG_vec_data)),temp1)
	MOV	(temp1,REGIND(allocptr))
	ADD	(IM(4),allocptr)
	MOV	(REGOFF(4,stdarg),temp1)	/* temp1 is list */
	MOV	(allocptr,stdarg)		/* stdarg is vector */

LABEL(L_create_v_lp)
	MOV	(REGIND(temp1),temp2)		/* hd */
	MOV	(temp2,REGIND(allocptr))	/* store into vector */
	ADD	(IM(4),allocptr)
	MOV	(REGOFF(4,temp1),temp1)		/* tl */
	CMP	(IM(ML_nil),temp1)		/* isNull */
	JNE	L_create_v_lp

	/* allocate header object */
	MOV	(IM(DESC_polyvec),temp1)
	MOV	(temp1,REGIND(allocptr))
	ADD	(IM(4),allocptr)
	MOV	(stdarg,REGIND(allocptr))	/* data */
	MOV	(temp,REGOFF(4,allocptr))	/* len */
	MOV	(allocptr,stdarg)		/* result */
	ADD	(IM(8),allocptr)		/* allocptr += 2 */

	POP	(misc1)
	POP	(misc0)
	CONTINUE

LABEL(L_create_v_large)
	POP	(misc1)
	POP	(misc0)
	MOVE	(stdlink, temp, pc)
	MOV	(IM(REQ_ALLOC_VECTOR),request_w)
	JMP	(set_request)
#undef temp1
#undef temp2


/* try_lock: spin_lock -> bool.
 * low-level test-and-set style primitive for mutual-exclusion among
 * processors.	For now, we only provide a uni-processor trivial version.
 */
ALIGNED_ENTRY(try_lock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV	((stdarg), temp)	/* Get old value of lock. */
	MOV	(IM(1), (stdarg))	/* Set the lock to ML_false. */
	MOV	(temp, stdarg)		/* Return old value of lock. */
	CONTINUE
#endif

/* unlock : releases a spin lock
 */
ALIGNED_ENTRY(unlock_a)
#if (MAX_PROCS > 1)
#  error multiple processors not supported
#else /* (MAX_PROCS == 1) */
	MOV	(IM(3), (stdarg))	/* Store ML_true into lock. */
	MOV	(IM(1), stdarg)	/* Return unit. */
	CONTINUE
#endif


/********************* Floating point functions. *********************/

/*
 * Initialize the 80387 floating point coprocessor.  First, the floating
 * point control word is initialized (undefined fields are left
 * unchanged).	Rounding control is set to "nearest" (although floor_a
 * needs "toward negative infinity").  Precision control is set to
 * "double".  The precision, underflow, denormal
 * overflow, zero divide, and invalid operation exceptions
 * are masked.  Next, seven of the eight available entries on the
 * floating point register stack are claimed (see x86/x86.sml).
 *
 * NB: this cannot trash any registers because it's called from request_fault.
 */
ALIGNED_ENTRY(FPEEnable)
	FINIT
	/* Temp space.Keep stack aligned. */
	SUB	(IM(4), ESP)
	/* Store FP control word. */
	FSTCW	(REGIND_16(ESP))
	/* Keep undefined fields, clear others. */
	ANDW	(IM(HEXLIT(f0c0)), REGIND(ESP))
	/* Set fields (see above). */
	ORW	(IM(HEXLIT(023f)), REGIND(ESP))
	FLDCW	(REGIND_16(ESP)) /* Install new control word. */
	ADD	(IM(4), ESP)
	RET

/* floor : real -> int
   Return the nearest integer that is less or equal to the argument.
   Caller's responsibility to make argument in range. */

ALIGNED_ENTRY(floor_a)
	/* Get FP control word. */
/* FIXME: use the stack !!! */
	FSTCW	(tempword1)
	MOVW	(tempword1,REG(ax))
	/* Clear rounding field. */
	ANDW	(IM(HEXLIT(f3ff)), REG(ax))
	/* Round towards neg. infinity. */
	ORW	(IM(HEXLIT(0400)), REG(ax))
	MOVW	(REG(ax), tempword2)
	FLDCW	(tempword2)		/* Install new control word. */
	FLD	(REGIND_DBL(stdarg))
	SUB	(IM(4),ESP)
	FISTP	(REGIND(ESP))
	POP	(stdarg)
	SAL	(IM(1),stdarg)
	INC	(stdarg)

	FLDCW	(tempword1)
	CONTINUE


	/* DEPRECATED */
/* logb : real -> int
 * Extract the unbiased exponent pointed to by stdarg.
 * Note: Using fxtract, and fistl does not work for inf's and nan's.
 */
ALIGNED_ENTRY(logb_a)
	MOV	(REGOFF(4,stdarg),temp)	/* msb for little endian arch */
	SAR	(IM(20),temp)		/* throw out 20 bits */
	AND	(IM(HEXLIT(7ff)),temp)	/* clear all but 11 low bits */
	SUB	(IM(1023),temp)		/* unbias */
	SAL	(IM(1),temp)		/* room for tag bit */
	ADD	(IM(1),temp)		/* tag bit */
	MOV	(temp,stdarg)
	CONTINUE


/* scalb : (real * int) -> real
 * Scale the first argument by 2 raised to the second argument.
 * NB: We assume the first floating point "register" is
 * caller-save, so we can use it here (see x86/x86.sml).
 */
ALIGNED_ENTRY(scalb_a)
	CHECKLIMIT
	PUSH	(REGOFF(4,stdarg))		/* Get copy of scalar. */
	SAR	(IM(1),REGIND(ESP))		/* Untag it. */
	FILDL	(REGIND(ESP))			/* Load it ... */
	MOV	(REGIND(stdarg), temp)		/* Get pointer to real. */
	FLD	(REGOFF_DBL(0,temp)) 		/* Load it into temp. */
	FSCALE					/* Multiply exponent by scalar. */
	MOV	(IM(DESC_reald), REGIND(allocptr))
	OR      (IM(4),allocptr)                /* align allocptr on 32-bit x86 */
	FSTPL	(REGOFF_DBL(4,allocptr))	/* Store resulting float. */
	ADD	(IM(4),allocptr)		/* Allocate word for tag. */
	MOV	(allocptr, stdarg)		/* Return a pointer to the float. */
	ADD	(IM(8), allocptr)		/* Allocate room for float. */
	FSTPL	(REGIND_DBL(ESP))
	ADD	(IM(4),ESP)			/* discard copy of scalar */
	CONTINUE

END

/* end of X86.prim.asm */
