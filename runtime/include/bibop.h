/*! \file bibop.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The BIBOP maps memory pages to page IDs.  The interpretation of most
 * of these IDs is defined by the GC (see ../gc/arena-id.h), but the
 * IDs for unmapped memory are defined here.
 */

#ifndef _BIBOP_
#define _BIBOP_

typedef Unsigned16_t page_id_t;

#define PAGEID_unmapped	0xffff

#define isUNMAPPED(ID)	((ID) == PAGEID_unmapped)


/** The BIBOP **/

#ifdef SIZE_64

/* for 64-bit ML values, we use a two-level BIBOP to resolve heap pointers to
 * BIBOP pages.  We assume that virtual addresses are < 2^48 (true for current
 * 64-bit hardware).  The top-level (L1) table consists of pointers to L2 tables.
 * We preallocate a special L2 table for unmapped regions.
 *
 * A 64-bit address is logically partitioned into four parts:
 *
 *	|00000000|00000000|aaaaaaaa|aaaaaaaa|bbbbbbbb|bbbbbbcc|cccccccc|cccccccc|
 *
 *	[63..48]	-- assumed to be zero and ignored
 *	[47..32] (a)	-- L1 index (16 bits)
 *	[31..18] (b)	-- L2 index (14 bits)
 *	[17..00] (c)	-- page bits
 *
 * The concatenation of the L1 and L2 indices define the flat BIBOP index,
 * which is 30 bits.
 */

/* we assume that the virtual address space is limited to 48 bits */
#define BIBOP_ADDR_BITS		48
/* the log2 size of the L1 bibop table */
#define BIBOP_L1_BITS		16
/* the log2 size of the L2 bibop table */
#define BIBOP_L2_BITS		14
/* the log2 size of a flat BIBOP index */
#define BIBOP_BITS		(BIBOP_L1_BITS + BIBOP_L2_BITS)
/* the log2 size of a BIBOP page in bytes */
#define BIBOP_PAGE_BITS		(BIBOP_ADDR_BITS - BIBOP_BITS)
/* L1 table size */
#define BIBOP_L1_SZ		(1 << BIBOP_L1_BITS)
/* L2 table size */
#define BIBOP_L2_SZ		(1 << BIBOP_L2_BITS)
/* shift amount to convert address to L1 index */
#define BIBOP_L1_SHIFT		(BIBOP_L2_BITS + BIBOP_PAGE_BITS)
/* shift amount to convert address to L2 index */
#define BIBOP_L2_SHIFT		BIBOP_PAGE_BITS
/* mask for L2 index */
#define BIBOP_L2_MASK		(BIBOP_L2_SZ - 1)

/* convert an address to a flat BIBOP index */
#define BIBOP_ADDR_TO_INDEX(a)		((Addr_t)(a) >> BIBOP_L2_SHIFT)
/* convert an address to its level-1 table index */
#define BIBOP_ADDR_TO_L1_INDEX(a)	((Addr_t)(a) >> BIBOP_L1_SHIFT)
/* convert an address to its level-2 table index */
#define BIBOP_ADDR_TO_L2_INDEX(a)	(BIBOP_ADDR_TO_INDEX(a) & BIBOP_L2_MASK)
/* convert a flat BIBOP index to a L1 table index */
#define BIBOP_INDEX_TO_L1_INDEX(ix)	((ix) >> BIBOP_L2_BITS)
/* convert a flat BIBOP index to a L2 table index */
#define BIBOP_INDEX_TO_L2_INDEX(ix)	((ix) & BIBOP_L2_MASK)
/* convert a flat BIBOP index to a memory address */
#define BIBOP_INDEX_TO_ADDR(i)		((Addr_t)(i) << BIBOP_L2_SHIFT)

typedef struct {
    page_id_t		tbl[BIBOP_L2_SZ];
    Unsigned32_t	numMapped;
} l2_bibop_t;

/* The BIBOP is a L1 table of pointers to L2 tables */
typedef l2_bibop_t **bibop_t;

extern bibop_t		BIBOP;
extern l2_bibop_t	UnmappedL2;

#define UNMAPPED_L2_TBL	&UnmappedL2

#define ADDR_TO_PAGEID(bibop,a)		\
	(bibop[BIBOP_ADDR_TO_L1_INDEX(a)]->tbl[BIBOP_ADDR_TO_L2_INDEX(a)])
#define INDEX_TO_PAGEID(bibop,ix)	\
	(bibop[BIBOP_INDEX_TO_L1_INDEX(ix)]->tbl[BIBOP_INDEX_TO_L2_INDEX(ix)])

/* update a BIBOP entry at the given index */
#define BIBOP_UPDATE(bibop, ix, aid)	\
	do { bibop[BIBOP_INDEX_TO_L1_INDEX(ix)]->tbl[BIBOP_INDEX_TO_L2_INDEX(ix)] = (aid); } while (0)

#else /* SIZE_32 */

#define BIBOP_PAGE_BITS		16		/* log2(BIBOP_PAGE_SZB) */
#define BIBOP_BITS		(BITS_PER_WORD-BIBOP_PAGE_BITS)
#define BIBOP_SZ		(1 << BIBOP_BITS)
#define BIBOP_ADDR_TO_INDEX(a)	((Addr_t)(a) >> BIBOP_PAGE_BITS)

#define BIBOP_INDEX_TO_ADDR(i)	((Addr_t)(i) << BIBOP_PAGE_BITS)
#define BIBOP_NBLKS_TO_SZB(i)	((Addr_t)(i) << BIBOP_PAGE_BITS)

typedef page_id_t *bibop_t;

extern bibop_t		BIBOP;

#define ADDR_TO_PAGEID(bibop,a)		((bibop)[BIBOP_ADDR_TO_INDEX(a)])
#define INDEX_TO_PAGEID(bibop,a)	((bibop)[a])

/* update a BIBOP entry at the given index */
#define BIBOP_UPDATE(bibop, ix, aid)	do { (bibop)[ix] = (aid); } while (0)

#endif /* !SIZE_64 */

/* validate the BIBOP page size */
#if (BIBOP_PAGE_SZB != (1 << BIBOP_PAGE_BITS))
#  error BIBOP_PAGE_SZB in ml-base.h does not equal (1 << BIBOP_PAGE_BITS)
#endif

/* allocate and initialize a Bibop */
extern bibop_t InitBibop ();

/* free a Bibop */
extern void FreeBibop (bibop_t bibop);

#endif /* !_BIBOP_ */
