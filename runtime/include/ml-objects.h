/*! \file ml-objects.h
 *
 * Macros and routines for allocating heap objects.
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _ML_OBJECTS_
#define _ML_OBJECTS_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

#ifndef _ML_VALUES_
#include "ml-values.h"
#endif

#ifndef _ML_STATE_
#include "ml-state.h"
#endif

#ifndef _TAGS_
#include "tags.h"
#endif

/* extract info from objects */
#define OBJ_DESC(OBJ)		REC_SEL((OBJ), -1)
#define OBJ_LEN(OBJ)		GET_LEN(OBJ_DESC(OBJ))
#define OBJ_TAG(OBJ)		GET_TAG(OBJ_DESC(OBJ))


/** The size of an ML record in bytes (including descriptor) **/
#define REC_SZB(n)	(((n)+1)*sizeof(ml_val_t))


/** heap allocation macros **/

/* write an ml_val_t value `x` into the allocation space at offset `i`. */
#define ML_AllocWrite(msp, i, x)	((((msp)->ml_allocPtr))[(i)] = (x))

/* write a 32-bit value `x` into the allocation space at offset `i`.  Note that
 * on 64-bit targets, the index is being scaled by 4 bytes (not 8)!
 */
#define ML_AllocWrite32(msp, i, x)	\
	(((Unsigned32_t *)((msp)->ml_allocPtr))[(i)] = (Unsigned32_t)(x))

STATIC_INLINE ml_val_t ML_Alloc (ml_state_t *msp, int n)
{
    ml_val_t obj = PTR_CtoML(msp->ml_allocPtr + 1);
    msp->ml_allocPtr += (n + 1);
    return obj;
}

/* inline allocation functions */
STATIC_INLINE ml_val_t ML_RefAlloc (ml_state_t *msp, ml_val_t a)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = DESC_ref;
    p[1] = a;
    return ML_Alloc(msp, 1);
}
STATIC_INLINE ml_val_t ML_Alloc1 (ml_state_t *msp, ml_val_t a)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(1, DTAG_record);
    p[1] = a;
    return ML_Alloc(msp, 1);
}
STATIC_INLINE ml_val_t ML_Alloc2 (ml_state_t *msp, ml_val_t a, ml_val_t b)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(2, DTAG_record);
    p[1] = a;
    p[2] = b;
    return ML_Alloc(msp, 2);
}
STATIC_INLINE ml_val_t ML_Alloc3 (ml_state_t *msp, ml_val_t a, ml_val_t b, ml_val_t c)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(3, DTAG_record);
    p[1] = a;
    p[2] = b;
    p[3] = c;
    return ML_Alloc(msp, 3);
}
STATIC_INLINE ml_val_t ML_Alloc4 (ml_state_t *msp, ml_val_t a, ml_val_t b, ml_val_t c, ml_val_t d)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(4, DTAG_record);
    p[1] = a;
    p[2] = b;
    p[3] = c;
    p[4] = d;
    return ML_Alloc(msp, 4);
}
STATIC_INLINE ml_val_t ML_Alloc5 (ml_state_t *msp, ml_val_t a, ml_val_t b, ml_val_t c, ml_val_t d, ml_val_t e)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(5, DTAG_record);
    p[1] = a;
    p[2] = b;
    p[3] = c;
    p[4] = d;
    p[5] = e;
    return ML_Alloc(msp, 5);
}
STATIC_INLINE ml_val_t ML_Alloc6 (ml_state_t *msp, ml_val_t a, ml_val_t b, ml_val_t c, ml_val_t d, ml_val_t e, ml_val_t f)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(6, DTAG_record);
    p[1] = a;
    p[2] = b;
    p[3] = c;
    p[4] = d;
    p[5] = e;
    p[6] = f;
    return ML_Alloc(msp, 6);
}
STATIC_INLINE ml_val_t ML_AllocSeqHdr (ml_state_t *msp, ml_val_t desc, ml_val_t data, Int_t len)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = desc;
    p[1] = data;
    p[2] = INT_CtoML(len);
    return ML_Alloc(msp, 2);
}
STATIC_INLINE ml_val_t ML_AllocReal64 (ml_state_t *msp, double d)
{
    ml_val_t *p = msp->ml_allocPtr;
#ifdef ALIGN_REALDS
    p = (ml_val_t *)((Addr_t)p | WORD_SZB);
#endif
    *p++ = DESC_reald;
    *(double *)p = d;
    msp->ml_allocPtr = p + REALD_SZW;
    return PTR_CtoML(p);
}
STATIC_INLINE ml_val_t ML_AllocWord (ml_state_t *msp, Word_t w)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(1, DTAG_raw);
    p[1] = (ml_val_t)w;
    return ML_Alloc(msp, 1);
}

/* support for 32-bit integers and words, which are boxed on 32-bit systems
 * and tagged on 64-bit systems.
 */
STATIC_INLINE ml_val_t INT32_CtoML (ml_state_t *msp, Int32_t n)
{
#ifdef SIZE_64
    return INT_CtoML(n); /* tagged representation on 64-bit systems */
#else /* 32-bit ML values */
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(1, DTAG_raw);
    p[1] = (ml_val_t)n;
    return ML_Alloc(msp, 1);
#endif
}
STATIC_INLINE Int32_t INT32_MLtoC (ml_val_t n)
{
#ifdef SIZE_64
    return INT_MLtoC(n); /* tagged representation on 64-bit systems */
#else /* 32-bit ML values */
    return *PTR_MLtoC(Int32_t, n);
#endif
}
STATIC_INLINE Int32_t REC_SELINT32 (ml_val_t p, int i)
{
#ifdef SIZE_64
    return REC_SELINT(p, i); /* tagged representation on 64-bit systems */
#else /* 32-bit ML values */
    return *REC_SELPTR(Int32_t, p, i);
#endif
}
STATIC_INLINE ml_val_t WORD32_CtoML (ml_state_t *msp, Unsigned32_t n)
{
#ifdef SIZE_64
    return INT_CtoML(n); /* tagged representation on 64-bit systems */
#else /* 32-bit ML values */
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = MAKE_DESC(1, DTAG_raw);
    p[1] = (ml_val_t)n;
    return ML_Alloc(msp, 1);
#endif
}
STATIC_INLINE Unsigned32_t WORD32_MLtoC (ml_val_t n)
{
#ifdef SIZE_64
    return (Unsigned32_t)INT_MLtoC(n); /* tagged representation on 64-bit systems */
#else /* 32-bit ML values */
    return *PTR_MLtoC(Unsigned32_t, n);
#endif
}
STATIC_INLINE Unsigned32_t REC_SELWORD32 (ml_val_t p, int i)
{
#ifdef SIZE_64
    return (Unsigned32_t)REC_SELINT(p, i); /* tagged representation on 64-bit systems */
#else /* 32-bit ML values */
    return *REC_SELPTR(Unsigned32_t, p, i);
#endif
}

/* support for 64-bit integers and words */
STATIC_INLINE ml_val_t ML_AllocInt64 (ml_state_t *msp, Int64_t n)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = DESC_word64;
#ifdef SIZE_64
    p[1] = (ml_val_t)n;
#else /* 32-bit ML values */
    p[1] = (ml_val_t)(Unsigned32_t)((Unsigned64_t)n >> 32);
    p[2] = (ml_val_t)(Unsigned32_t)n;
#endif
    return ML_Alloc(msp, WORD64_SZW);
}
STATIC_INLINE ml_val_t ML_AllocWord64 (ml_state_t *msp, Unsigned64_t w)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = DESC_word64;
#ifdef SIZE_64
    p[1] = (ml_val_t)w;
#else /* 32-bit ML values */
    p[1] = (ml_val_t)(Unsigned32_t)(w >> 32);
    p[2] = (ml_val_t)(Unsigned32_t)w;
#endif
    return ML_Alloc(msp, WORD64_SZW);
}
STATIC_INLINE Int64_t INT64_MLtoC (ml_val_t n)
{
#ifdef SIZE_64
    return *PTR_MLtoC(Unsigned64_t, n);
#else /* 32-bit ML values */
    Unsigned64_t hi = PTR_MLtoC(Unsigned32_t, n)[0];
    Unsigned64_t lo = PTR_MLtoC(Unsigned32_t, n)[1];
    return ((hi << 32) | lo);
#endif
}
STATIC_INLINE Unsigned64_t WORD64_MLtoC (ml_val_t n)
{
#ifdef SIZE_64
    return *PTR_MLtoC(Unsigned64_t, n);
#else /* 32-bit ML values */
    Unsigned64_t hi = PTR_MLtoC(Unsigned32_t, n)[0];
    Unsigned64_t lo = PTR_MLtoC(Unsigned32_t, n)[1];
    return ((hi << 32) | lo);
#endif
}

/* add a store-list entry */
STATIC_INLINE void ML_RecordUpdate (ml_state_t *msp, ml_val_t *addr)
{
    ml_val_t *p = msp->ml_allocPtr;
    p[0] = PTR_CtoML(addr);
    p[1] = msp->ml_storePtr;
    msp->ml_storePtr = PTR_CtoML(p);
    msp->ml_allocPtr += 2;
}

/* allocate a 64-bit integer number of nanoseconds, given seconds and
 * microseconds.
 */
STATIC_INLINE ml_val_t ML_AllocNanoseconds (ml_state_t *msp, int sec, int usec)
{
    Unsigned64_t t = (NS_PER_SEC * (Unsigned64_t)sec) + (1000 * (Unsigned64_t)usec);
    return ML_AllocWord64(msp, t);
}

/* macros that wrap the inline allocation functions; these are for backward
 * compatibility to the old macro-based allocation code.
 */
#define REF_ALLOC(msp, r, a)			{ (r) = ML_RefAlloc((msp), (a)); }
#define REC_ALLOC1(msp, r, a)			{ (r) = ML_Alloc1((msp), (a)); }
#define REC_ALLOC2(msp, r, a, b)		{ (r) = ML_Alloc2((msp), (a), (b)); }
#define REC_ALLOC3(msp, r, a, b, c)		{ (r) = ML_Alloc3((msp), (a), (b), (c)); }
#define REC_ALLOC4(msp, r, a, b, c, d)		{ (r) = ML_Alloc4((msp), (a), (b), (c), (d)); }
#define REC_ALLOC5(msp, r, a, b, c, d, e)	{ (r) = ML_Alloc5((msp), (a), (b), (c), (d), (e)); }
#define REC_ALLOC6(msp, r, a, b, c, d, e, f)	{ (r) = ML_Alloc6((msp), (a), (b), (c), (d), (e), (f)); }
#define SEQHDR_ALLOC(msp, r, desc, data, len)	{ (r) = ML_AllocSeqHdr((msp), (desc), (data), (len)); }
#define REAL64_ALLOC(msp, r, d)			{ (r) = ML_AllocReal64((msp), (d)); }
#define EXN_ALLOC(msp, ex, id, val, where)	REC_ALLOC3(msp, ex, id, val, where)

/** Boxed word values **/
#define WORD_ALLOC(msp, r, w)	{ (r) = ML_AllocWord((msp), (w)); }
#define WORD_MLtoC(w)		(*PTR_MLtoC(Word_t, w))
#define REC_SELWORD(p, i)	(*REC_SELPTR(Word_t, p, i))

/* temporary */
#ifdef SIZE_32
#define INT32_ALLOC(msp, p, i)	WORD_ALLOC(msp, p, i)
#endif

#define INT64_ALLOC(msp, r, i)	{ (r) = ML_AllocInt64((msp), (i)); }
#define WORD64_ALLOC(msp, r, w)	{ (r) = ML_AllocWord64((msp), (w)); }

/** SysWord.word conversions */
#ifdef SIZE_64
typedef Unsigned64_t SysWord_t;
#define SYSWORD_ALLOC(msp, r, w)	WORD64_ALLOC(msp, r, w)
#define SYSWORD_MLtoC(w)		WORD64_MLtoC(w)
#else /* SIZE_32 */
typedef Unsigned64_t SysWord_t;
#define SYSWORD_ALLOC(msp, r, w)	WORD_ALLOC(msp, r, w)
#define SYSWORD_MLtoC(w)		WORD32_MLtoC(w)
#endif

/** ML lists **/
#define LIST_hd(p)		REC_SEL(p, 0)
#define LIST_tl(p)		REC_SEL(p, 1)
#define LIST_nil		INT_CtoML(0)
#define LIST_isNull(p)		((p) == LIST_nil)
#define LIST_cons(msp, r, a, b)	REC_ALLOC2(msp, r, a, b)

/** ML references **/
#define DEREF(r)		REC_SEL(r, 0)
#define ASSIGN(r, x)		(PTR_MLtoC(ml_val_t, r)[0] = (x))

/** ML options **/
#define OPTION_NONE             INT_CtoML(0)
#define OPTION_SOME(msp, r, a)  REC_ALLOC1(msp, r, a)
#define OPTION_get(r)		REC_SEL(r, 0)

/* the HANDLE type is an alias for `void *`, but HANDLE values are
 * actually indices into internal tables in the OS.  We could probably
 * get away with representing them as tagged integers or words, but
 * for now we use a pointer-sized boxed word.
 */
#if defined(_WIN32)
#define HANDLE_MLtoC(h)		((HANDLE)WORD32_MLtoC(h))
#define HANDLE_CtoML(msp, h)	WORD32_CtoML(msp, (Addr_t)h)
#elif defined(_WIN64)
#define HANDLE_MLtoC(h)		((HANDLE)WORD64_MLtoC(h))
#define HANDLE_CtoML(msp, h)	ML_AllocWord64(msp, (Addr_t)h)
#endif

/** external routines **/
extern ml_val_t ML_CString (ml_state_t *msp, const char *v);
extern ml_val_t ML_CStringList (ml_state_t *msp, char **strs);
extern ml_val_t ML_AllocString (ml_state_t *msp, int len);
extern ml_val_t ML_AllocCode (ml_state_t *msp, int len);
extern ml_val_t ML_AllocBytearray (ml_state_t *msp, int len);
extern ml_val_t ML_AllocRealdarray (ml_state_t *msp, int len);
extern ml_val_t ML_AllocArrayData (ml_state_t *msp, int len, ml_val_t initVal);
extern ml_val_t ML_AllocArray (ml_state_t *msp, int len, ml_val_t initVal);
extern ml_val_t ML_AllocVector (ml_state_t *msp, int len, ml_val_t initVal);
extern ml_val_t ML_AllocRaw (ml_state_t *msp, int len);
extern void ML_ShrinkRaw (ml_state_t *msp, ml_val_t v, int nWords);
extern ml_val_t ML_AllocRaw64 (ml_state_t *msp, int len);

extern ml_val_t ML_SysConst (ml_state_t *msp, sysconst_tbl_t *tbl, int id);
extern ml_val_t ML_SysConstList (ml_state_t *msp, sysconst_tbl_t *tbl);
extern ml_val_t ML_AllocCData (ml_state_t *msp, int nbytes);
extern ml_val_t ML_CData (ml_state_t *msp, void *data, int nbytes);

extern ml_val_t BuildLiterals (ml_state_t *msp, Byte_t *lits, int len);

extern ml_val_t _ML_string0[];
extern ml_val_t _ML_vector0[];
#define ML_string0	PTR_CtoML(_ML_string0+1)
#define ML_vector0	PTR_CtoML(_ML_vector0+1)

#endif /* !_ML_OBJECTS_ */
