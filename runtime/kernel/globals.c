/*! \file globals.c
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#include "ml-base.h"
#include "machine-id.h"
#include "ml-values.h"
#include "tags.h"
#include "ml-globals.h"
#include "ml-objects.h"
#include "ml-limits.h"
#include "c-globals-tbl.h"

typedef struct {
    ml_val_t	desc;
    char	*s;
    ml_val_t	len;
} ml_string_t;

#define ML_STRING(id, s)				\
    ml_string_t id = {					\
	DESC_string,					\
	s,						\
	INT_CtoML(sizeof(s)-1)				\
    }

/* Exceptions are identified by (string ref) values */
#define ML_EXNID(ex,name)				\
    ML_STRING(CONCAT(ex,_s), name);			\
    ml_val_t CONCAT(ex,_id0) [2] = {			\
	DESC_ref,					\
	PTR_CtoML(&(CONCAT(ex,_s).s))			\
    }

#define ASM_CLOSURE(name)				\
    extern ml_val_t CONCAT(name,_a)[];			\
    ml_val_t CONCAT(name,_v)[2] = {			\
	MAKE_DESC(1,DTAG_record),			\
	PTR_CtoML(CONCAT(name,_a))			\
    }

#if (CALLEESAVE > 0)
#define ASM_CONT(name) 							\
    extern ml_val_t CONCAT(name,_a)[];					\
    ml_val_t *CONCAT(name,_c) = (ml_val_t *)(CONCAT(name,_a))
#else
#define ASM_CONT(name)							\
    ASM_CLOSURE(name);							\
    ml_val_t *CONCAT(name,_c) = (ml_val_t *)(CONCAT(name,_v)+1)
#endif

ASM_CLOSURE(array);
ASM_CLOSURE(bind_cfun);
ASM_CLOSURE(callc);
ASM_CLOSURE(create_b);
ASM_CLOSURE(create_r);
ASM_CLOSURE(create_s);
ASM_CLOSURE(create_v);
ASM_CLOSURE(floor);
ASM_CLOSURE(logb);              /* DEPRECATED */
ASM_CLOSURE(scalb);
ASM_CLOSURE(try_lock);          /* DEPRECATED */
ASM_CLOSURE(unlock);            /* DEPRECATED */
ASM_CLOSURE(handle);

ASM_CONT(return);
ASM_CONT(sigh_return);  /* QUESION: maybe this should be ASM_CLOSURE? */
ASM_CONT(pollh_return);         /* DEPRECATED */


/* A ref cell initialized to unit. */
#define REFCELL(z)	ml_val_t z[2] = {DESC_ref, ML_unit}

REFCELL(_ProfCurrent);
REFCELL(_PervStruct);
REFCELL(_MLSignalHandler);
REFCELL(_MLPollHandler);        /* DEPRECATED */
REFCELL(_PollEvent0);           /* DEPRECATED */
REFCELL(_PollFreq0);            /* DEPRECATED */
REFCELL(_ActiveProcs0);         /* DEPRECATED */

ml_val_t		RunTimeCompUnit = ML_unit;
#ifdef ASM_MATH
ml_val_t		MathVec = ML_unit;
#endif

/* aggregate structures of length zero */
const char _ML_string0_data[1]  = {0};
ml_val_t _ML_string0[3]		= {DESC_string, PTR_CtoML(_ML_string0_data), INT_CtoML(0)};
ml_val_t _ML_vector0[3]		= {DESC_polyvec, ML_unit, INT_CtoML(0)};

ML_EXNID(_Div,"Div");           /* DEPRECATED */
ML_EXNID(_Overflow,"Overflow");
ML_EXNID(SysErr, "SysErr");

extern ml_val_t externlist0[];

#ifdef ASM_MATH
ML_EXNID(_Ln,"Ln");
ML_EXNID(_Sqrt,"Sqrt");
#endif


/* A table of pointers to global C variables that are potential roots. */
ml_val_t	*CRoots[MAX_C_ROOTS] = {
    &RunTimeCompUnit,
    _PervStruct+1,
    _MLSignalHandler+1,
#ifdef ASM_MATH
    &MathVec,
#else
    NIL(ml_val_t *),
#endif
    NIL(ml_val_t *), NIL(ml_val_t *), NIL(ml_val_t *)
};
#ifdef ASM_MATH
int		NumCRoots = 4;
#else
int		NumCRoots = 3;
#endif


/* AllocGlobals:
 */
void AllocGlobals (ml_state_t *msp)
{
    ml_val_t	RunVec;
    ml_val_t    CStruct;

  /* allocate the RunVec */
#define RUNVEC_SZ	12
    ML_AllocWrite(msp,  0, MAKE_DESC(RUNVEC_SZ, DTAG_record));
    ML_AllocWrite(msp,  1, PTR_CtoML(array_v+1));
    ML_AllocWrite(msp,  2, PTR_CtoML(bind_cfun_v+1));
    ML_AllocWrite(msp,  3, PTR_CtoML(callc_v+1));
    ML_AllocWrite(msp,  4, PTR_CtoML(create_b_v+1));
    ML_AllocWrite(msp,  5, PTR_CtoML(create_r_v+1));
    ML_AllocWrite(msp,  6, PTR_CtoML(create_s_v+1));
    ML_AllocWrite(msp,  7, PTR_CtoML(create_v_v+1));
    ML_AllocWrite(msp,  8, PTR_CtoML(floor_v+1));
    ML_AllocWrite(msp,  9, ML_unit);			/* logb_v is DEPRECATED */
    ML_AllocWrite(msp, 10, PTR_CtoML(scalb_v+1));
    ML_AllocWrite(msp, 11, ML_unit);			/* try_lock_v is DEPRECATED */
    ML_AllocWrite(msp, 12, ML_unit);			/* unlock_v is DEPRECATED */
    RunVec = ML_Alloc(msp, RUNVEC_SZ);

  /* allocate the CStruct */
#define CSTRUCT_SZ	12
    ML_AllocWrite(msp,  0, MAKE_DESC(CSTRUCT_SZ, DTAG_record));
    ML_AllocWrite(msp,  1, RunVec);
    ML_AllocWrite(msp,  2, DivId);              /* DEPRECATED */
    ML_AllocWrite(msp,  3, OverflowId);
    ML_AllocWrite(msp,  4, SysErrId);
    ML_AllocWrite(msp,  5, ProfCurrent);
    ML_AllocWrite(msp,  6, ML_unit);		/* PollEvent is DEPRECATED */
    ML_AllocWrite(msp,  7, ML_unit);		/* PollFreq is DEPRECATED */
    ML_AllocWrite(msp,  8, ML_unit);		/* MLPollHandler is DEPRECATED */
    ML_AllocWrite(msp,  9, ML_unit);		/* ActiveProcs is DEPRECATED */
    ML_AllocWrite(msp, 10, PervStruct);
    ML_AllocWrite(msp, 11, MLSignalHandler);
    ML_AllocWrite(msp, 12, ML_vector0);
    CStruct = ML_Alloc(msp, CSTRUCT_SZ);

  /* allocate 1-elem SRECORD just containing the CStruct */
    REC_ALLOC1(msp, RunTimeCompUnit, CStruct);

#ifdef ASM_MATH
#define MATHVEC_SZ	8
    ML_AllocWrite(msp,  0, MAKE_DESC(MATHVEC_SZ, DTAG_record));
    ML_AllocWrite(msp,  1, LnId);
    ML_AllocWrite(msp,  2, SqrtId);
    ML_AllocWrite(msp,  3, PTR_CtoML(arctan_v+1));
    ML_AllocWrite(msp,  4, PTR_CtoML(cos_v+1));
    ML_AllocWrite(msp,  5, PTR_CtoML(exp_v+1));
    ML_AllocWrite(msp,  6, PTR_CtoML(ln_v+1));
    ML_AllocWrite(msp,  7, PTR_CtoML(sin_v+1));
    ML_AllocWrite(msp,  8, PTR_CtoML(sqrt_v+1));
    MathVec = ML_Alloc(msp, MATHVEC_SZ);
#endif

} /* end of AllocGlobals */


/* RecordGlobals:
 *
 * Record all global symbols that may be referenced from the ML heap.
 */
void RecordGlobals ()
{
  /* Misc. */
    RecordCSymbol ("nullptr",		PTR_CtoML(0));
    RecordCSymbol ("handle",		PTR_CtoML(handle_v+1));
    RecordCSymbol ("return",		PTR_CtoML(return_c));
#if (CALLEESAVE == 0)
    RecordCSymbol ("return_a",		PTR_CtoML(return_a));
#endif

  /* RunVec */
    RecordCSymbol ("RunVec.array",	PTR_CtoML(array_v+1));
    RecordCSymbol ("RunVec.bind_cfun",	PTR_CtoML(bind_cfun_v+1));
    RecordCSymbol ("RunVec.callc",	PTR_CtoML(callc_v+1));
    RecordCSymbol ("RunVec.create_b",	PTR_CtoML(create_b_v+1));
    RecordCSymbol ("RunVec.create_r",	PTR_CtoML(create_r_v+1));
    RecordCSymbol ("RunVec.create_s",	PTR_CtoML(create_s_v+1));
    RecordCSymbol ("RunVec.create_v",	PTR_CtoML(create_v_v+1));
    RecordCSymbol ("RunVec.floor",	PTR_CtoML(floor_v+1));
    RecordCSymbol ("RunVec.logb",	PTR_CtoML(logb_v+1));           /* DEPRECATED */
    RecordCSymbol ("RunVec.scalb",	PTR_CtoML(scalb_v+1));
    RecordCSymbol ("RunVec.try_lock",	PTR_CtoML(try_lock_v+1));       /* DEPRECATED */
    RecordCSymbol ("RunVec.unlock",	PTR_CtoML(unlock_v+1));         /* DEPRECATED */

  /* CStruct */
    RecordCSymbol ("CStruct.DivId",		DivId); /* FIXME: we can remove this */
    RecordCSymbol ("CStruct.OverflowId",	OverflowId);
    RecordCSymbol ("CStruct.SysErrId",		SysErrId);
    RecordCSymbol ("CStruct.PervStruct",	PervStruct);
    RecordCSymbol ("CStruct.MLSignalHandler",	MLSignalHandler);
    RecordCSymbol ("CStruct.vector0",		ML_vector0);
    RecordCSymbol ("CStruct.profCurrent",	ProfCurrent);
    RecordCSymbol ("CStruct.MLPollHandler",     MLPollHandler); /* DEPRECATED */
    RecordCSymbol ("CStruct.pollEvent",		PollEvent);     /* DEPRECATED */
    RecordCSymbol ("CStruct.pollFreq",		PollFreq);      /* DEPRECATED */
    RecordCSymbol ("CStruct.activeProcs",	ActiveProcs);   /* DEPRECATED */

  /* null string */
    RecordCSymbol ("string0",			ML_string0);

#if defined(ASM_MATH)
  /* MathVec */
    RecordCSymbol ("MathVec.LnId",	LnId);
    RecordCSymbol ("MathVec.SqrtId",	SqrtId);
    RecordCSymbol ("MathVec.arctan",	PTR_CtoML(arctan_v+1));
    RecordCSymbol ("MathVec.cos",	PTR_CtoML(cos_v+1));
    RecordCSymbol ("MathVec.exp",	PTR_CtoML(exp_v+1));
    RecordCSymbol ("MathVec.ln",	PTR_CtoML(ln_v+1));
    RecordCSymbol ("MathVec.sin",	PTR_CtoML(sin_v+1));
    RecordCSymbol ("MathVec.sqrt",	PTR_CtoML(sqrt_v+1));
#endif

} /* end of RecordGlobals. */
