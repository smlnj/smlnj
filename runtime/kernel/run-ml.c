/// \file run-ml.c
///
/// \copyright 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
/// All rights reserved.
///
/// \brief The main dispatch function for running SML code and for
///   servicing requests for runtime-system services
///
/// \author John Reppy
///

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "ml-base.h"
#include "ml-limits.h"
#include "ml-values.h"
#include "vproc-state.h"
#include "ml-state.h"
#include "tags.h"
#include "ml-request.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "ml-signals.h"
#include "c-library.h"
#include "profile.h"
#include "gc.h"

#ifdef ENABLE_CACHEGRIND
#include "valgrind/cachegrind.h"
#endif
#ifdef ENABLE_MEMCHECK
#include "valgrind/memcheck.h"
#endif

/* local functions */
PVT void UncaughtExn (ml_val_t e);


/* ApplyMLFn:
 *
 * Apply the ML closure f to arg and return the result.  If the flag useCont
 * is set, then the ML state has already been initialized with a return
 * continuation (by SaveCState).
 */
ml_val_t ApplyMLFn (ml_state_t *msp, ml_val_t f, ml_val_t arg, bool_t useCont)
{
    InitMLState (msp);

  /* initialize the calling context */
    msp->ml_exnCont     = PTR_CtoML(handle_v+1);
    msp->ml_varReg      = ML_unit;
    msp->ml_arg         = arg;
    if (! useCont)
        msp->ml_cont    = PTR_CtoML(return_c);
    msp->ml_closure     = f;
    msp->ml_pc          =
    msp->ml_linkReg     = GET_CODE_ADDR(f);

    RunML (msp);

    return msp->ml_arg;

} /* end of ApplyMLFn */


/* RaiseMLExn:
 *
 * Modify the ML state, so that the given exception will be raised
 * when ML is resumed.
 */
void RaiseMLExn (ml_state_t *msp, ml_val_t exn)
{
    ml_val_t    kont = msp->ml_exnCont;

/** NOTE: we should have a macro defined in ml-state.h for this **/
    msp->ml_arg         = exn;
    msp->ml_closure     = kont;
    msp->ml_cont        = ML_unit;
    msp->ml_pc          =
    msp->ml_linkReg     = GET_CODE_ADDR(kont);

} /* end of RaiseMLExn. */

extern int restoreregs (ml_state_t *msp);

/* RunML:
 */
#if defined(__CYGWIN32__)
void SystemRunML (ml_state_t *msp)
#else
void RunML (ml_state_t *msp)
#endif
{
    int         request;
    vproc_state_t *vsp = msp->ml_vproc;
    ml_val_t    prevProfIndex = PROF_OTHER;

    for (;;) {

        ASSIGN(ProfCurrent, prevProfIndex);
#ifdef ENABLE_CACHEGRIND
        /* turn on cachegrind instrumentation for SML code */
        CACHEGRIND_START_INSTRUMENTATION;
#endif
#ifdef ENABLE_MEMCHECK
        /* turn off error reporting while executing SML code */
        VALGRIND_DISABLE_ERROR_REPORTING;
#endif
        request = restoreregs(msp);
#ifdef ENABLE_MEMCHECK
        /* turn off error reporting while executing SML code */
        VALGRIND_ENABLE_ERROR_REPORTING;
#endif
#ifdef ENABLE_CACHEGRIND
        /* turn off cachegrind instrumentation for SML code */
        CACHEGRIND_STOP_INSTRUMENTATION;
#endif
        prevProfIndex = DEREF(ProfCurrent);
        ASSIGN(ProfCurrent, PROF_RUNTIME);

        if (request == REQ_GC) {
            if (vsp->vp_handlerPending) { /* this is really a signal */
#ifdef SIGNAL_DEBUG
                SayDebug("RunML: handler pending\n");
#endif
              /* check for GC */
                if (NeedGC (msp, ONE_K*WORD_SZB)) {
                    InvokeGC (msp, 0);
                }
              /* invoke the ML signal handler */
                ChooseSignal (vsp);
                msp->ml_arg             = MakeHandlerArg (msp, sigh_resume);
                msp->ml_cont            = PTR_CtoML(sigh_return_c);
                msp->ml_exnCont         = PTR_CtoML(handle_v+1);
                msp->ml_closure         = DEREF(MLSignalHandler);
                msp->ml_pc              =
                msp->ml_linkReg         = GET_CODE_ADDR(msp->ml_closure);
                vsp->vp_inSigHandler    = TRUE;
                vsp->vp_handlerPending  = FALSE;
            }
            else {
                InvokeGC (msp, 0);
            }
        }
        else {
            switch (request) {
              case REQ_RETURN:
              /* do a minor collection to clear the store list */
                InvokeGC (msp, 0);
                return;

              case REQ_EXN: /* an UncaughtExn exception */
                UncaughtExn (msp->ml_arg);
                return;

              case REQ_FAULT: { /* a hardware overflow trap */
                    ml_val_t    loc, traceStk, exn;
                    char *namestring;
                    if ((namestring = (char *)BO_AddrToCodeObjTag(msp->ml_faultPC)) != NIL(char *))
                    {
                        char    buf2[192];
                        sprintf(buf2, "<file %.184s>", namestring);
                        loc = ML_CString(msp, buf2);
                    }
                    else {
                        loc = ML_CString(msp, "<unknown file>");
                    }
                    LIST_cons(msp, traceStk, loc, LIST_nil);
                    EXN_ALLOC(msp, exn, OverflowId, ML_unit, traceStk);
                    RaiseMLExn (msp, exn);
                } break;

              case REQ_RAISE_OVERFLOW: { /* a request for raising Overflow */
                    ml_val_t    loc, traceStk, exn;
                    char *namestring;
                    SayDebug("RunML: raise Overflow request: pc = %p\n", msp->ml_pc);
                    if ((namestring = (char *)BO_AddrToCodeObjTag((Word_t)(msp->ml_pc))) != NIL(char *))
                    {
                        char    buf2[192];
                        sprintf(buf2, "<file %.184s>", namestring);
                        loc = ML_CString(msp, buf2);
                    }
                    else {
                        loc = ML_CString(msp, "<unknown file>");
                    }
                    LIST_cons(msp, traceStk, loc, LIST_nil);
                    EXN_ALLOC(msp, exn, OverflowId, ML_unit, traceStk);
                    RaiseMLExn (msp, exn);
                } break;

              case REQ_BIND_CFUN:
                msp->ml_arg = BindCFun (
                    STR_MLtoC(REC_SEL(msp->ml_arg, 0)),
                    STR_MLtoC(REC_SEL(msp->ml_arg, 1)));
                SETUP_RETURN(msp);
                break;

              case REQ_CALLC: {
                    ml_val_t    (*f)(), arg;

                    SETUP_RETURN(msp);
                    if (NeedGC (msp, 8*ONE_K))
                        InvokeGC (msp, 0);

#ifdef INDIRECT_CFUNC
                    f = ((cfunc_binding_t *)REC_SELPTR(Word_t, msp->ml_arg, 0))->cfunc;
#  ifdef DEBUG_TRACE_CCALL
                    SayDebug("CALLC: %s (%#x)\n",
                        ((cfunc_binding_t *)REC_SELPTR(Word_t, msp->ml_arg, 0))->name,
                        REC_SEL(msp->ml_arg, 1));
#  endif
#else
                    f = (cfunc_t) REC_SELPTR(Word_t, msp->ml_arg, 0);
#  ifdef DEBUG_TRACE_CCALL
                    SayDebug("CALLC: %#x (%#x)\n", f, REC_SEL(msp->ml_arg, 1));
#  endif
#endif
                    arg = REC_SEL(msp->ml_arg, 1);
                    msp->ml_arg = (*f)(msp, arg);
                } break;

              case REQ_ALLOC_STRING:
                msp->ml_arg = ML_AllocString (msp, INT_MLtoC(msp->ml_arg));
                SETUP_RETURN(msp);
                break;

              case REQ_ALLOC_BYTEARRAY:
                msp->ml_arg = ML_AllocBytearray (msp, INT_MLtoC(msp->ml_arg));
                SETUP_RETURN(msp);
                break;

              case REQ_ALLOC_REALDARRAY:
                msp->ml_arg = ML_AllocRealdarray (msp, INT_MLtoC(msp->ml_arg));
                SETUP_RETURN(msp);
                break;

              case REQ_ALLOC_ARRAY:
                msp->ml_arg = ML_AllocArray (msp,
                    REC_SELINT(msp->ml_arg, 0), REC_SEL(msp->ml_arg, 1));
                SETUP_RETURN(msp);
                break;

              case REQ_ALLOC_VECTOR:
                msp->ml_arg = ML_AllocVector (msp,
                    REC_SELINT(msp->ml_arg, 0), REC_SEL(msp->ml_arg, 1));
                SETUP_RETURN(msp);
                break;

              case REQ_SIG_RETURN:
#ifdef SIGNAL_DEBUG
SayDebug("REQ_SIG_RETURN: arg = %#x, pending = %d, inHandler = %d, nSigs = %d/%d\n",
msp->ml_arg, vsp->vp_handlerPending, vsp->vp_inSigHandler,
vsp->vp_totalSigCount.nHandled, vsp->vp_totalSigCount.nReceived);
#endif
              /* throw to the continuation */
                SETUP_THROW(msp, msp->ml_arg, ML_unit);
              /* note that we are exiting the handler */
                vsp->vp_inSigHandler = FALSE;
                break;

              case REQ_SIG_RESUME:
#ifdef SIGNAL_DEBUG
SayDebug("REQ_SIG_RESUME: arg = %#x\n", msp->ml_arg);
#endif
                LoadResumeState (msp);
                break;

              case REQ_BUILD_LITERALS:
                Die ("BUILD_LITERALS request");
                break;

              default:
                Die ("unknown request code = %d", request);
                break;
            } /* end switch */
        }
    } /* end of while */

} /* end of RunML */


/* UncaughtExn:
 * Handle an uncaught exception.
 */
PVT void UncaughtExn (ml_val_t e)
{
    ml_val_t    name = REC_SEL(REC_SEL(e, 0), 0);
    ml_val_t    val = REC_SEL(e, 1);
    ml_val_t    traceBack = REC_SEL(e, 2);
    char        buf[1024];

    if (isUNBOXED(val))
        sprintf (buf, "%ld\n", (long int) INT_MLtoC(val));
    else {
        ml_val_t        desc = OBJ_DESC(val);
        if (desc == DESC_string)
            sprintf (buf, "\"%.*s\"", (int) GET_SEQ_LEN(val), STR_MLtoC(val));
        else
            sprintf (buf, "<unknown>");
    }

    if (traceBack != LIST_nil) {
      /* find the information about where this exception was raised */
        ml_val_t        next = traceBack;
        do {
            traceBack = next;
            next = LIST_tl(traceBack);
        } while (next != LIST_nil);
        val = LIST_hd(traceBack);
        sprintf (buf+strlen(buf), " raised at %.*s",
                 (int) GET_SEQ_LEN(val), STR_MLtoC(val));
    }

    Die ("Uncaught exception %.*s with %s\n",
        GET_SEQ_LEN(name), GET_SEQ_DATAPTR(char, name), buf);

    Exit (1);

} /* end of UncaughtExn */
