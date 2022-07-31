/*! \file gen-offsets.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * This C program generates a header file for the *.prim.asm files,
 * which gives the offset values in the VProc and ML state vectors.
 *
 * Note that we only generate offsets for three miscregs; this is because
 * only the first three miscregs are marked as callee-save by the compiler
 * and are thus live when saveregs is called.  See compiler/CodeGen/main/machspec.sig
 * and compiler/CodeGen/cpscompile/invokegc.sml.
 */

#include "ml-base.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "gen.h"

#define MOFFSET(fld)	(((Addr_t)&(M.s.fld)) - (Addr_t)&(M.b[0]))
#define VOFFSET(fld)	(((Addr_t)&(V.s.fld)) - (Addr_t)&(V.b[0]))

#define PVOFFSET(sym, fld)	\
    fprintf(f, "#define %sOffVSP %ld\n", (sym), (long int) VOFFSET(fld))
#define PMOFFSET(sym, fld)	\
    fprintf(f, "#define %sOffMSP %ld\n", (sym), (long int) MOFFSET(fld))


int main (void)
{
    union {
	vproc_state_t	s;
	char		b[sizeof(vproc_state_t)];
    }		V;
    union {
	ml_state_t	s;
	char		b[sizeof(ml_state_t)];
    }		M;
    FILE	*f;

    f = OpenFile ("mlstate-offsets.h", "_MLSTATE_OFFSETS_");

    PMOFFSET("VProc", ml_vproc);
    PMOFFSET("AllocPtr", ml_allocPtr);
    PMOFFSET("LimitPtr", ml_limitPtr);
    PMOFFSET("StorePtr", ml_storePtr);
    PMOFFSET("ExnPtr", ml_exnCont);
    PMOFFSET("VarPtr", ml_varReg);
    PMOFFSET("LinkReg", ml_linkReg);
    PMOFFSET("StdClos", ml_closure);
    PMOFFSET("StdCont", ml_cont);
    PMOFFSET("Misc0", ml_calleeSave[0]);
    PMOFFSET("Misc1", ml_calleeSave[1]);
    PMOFFSET("Misc2", ml_calleeSave[2]);
    PMOFFSET("StdArg", ml_arg);
    PMOFFSET("PC", ml_pc);
    PVOFFSET("InML", vp_inMLFlag);
    PVOFFSET("HandlerPending", vp_handlerPending);
    PVOFFSET("InSigHandler", vp_inSigHandler);
    PVOFFSET("SigsRecv", vp_totalSigCount.nReceived);
    PVOFFSET("SigsHandled", vp_totalSigCount.nHandled);
    PVOFFSET("LimitPtrMask", vp_limitPtrMask);

    CloseFile (f, "_MLSTATE_OFFSETS_");

    exit (0);
}
