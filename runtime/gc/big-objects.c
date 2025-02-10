/*! \file big-objects.c
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code for managing big-object regions.  A big-object region consists of a sequence
 * of one or more BIBOP pages and associated data structures.  The memory contained in
 * the BIBOP pages is subdivided into big-object pages; page zero contains a pointer
 * to the region header (`struct bigobj_region`) that describes the allocation of
 * pages in the region to big objects.  The region header must be stored outside of
 * the region memory, since the region memory is executable and thus write protected
 * most of the time (on Arm64).
 */

#include "ml-base.h"
#include "memory.h"
#include "heap.h"
#include "heap-monitor.h"
#include <string.h>

/* cast the address of the start of a big-object region to a pointer to a
 * region-header pointer (the first word of a big-object region is a pointer
 * to the region header for the region).
 */
STATIC_INLINE bigobj_region_t **GetRegionHdrPtr (Addr_t addr)
{
    ASSERT ((addr & (BIBOP_PAGE_SZB-1)) == 0);
    return (bigobj_region_t **)addr;
}

#ifdef BO_DEBUG
/* PrintRegionMap:
 */
void PrintRegionMap (bigobj_region_t *r)
{
    bigobj_desc_t	*dp, *dq;
    int			i;

    SayDebug ("[%d] %d/%d, @%p: ", r->minGen, r->nFree, r->nPages, (void *)(r->firstPage));
    for (i = 0, dq = NIL(bigobj_desc_t *);  i < r->nPages;  i++) {
	dp = r->objMap[i];
	if (dp != dq) {
	    SayDebug ("|");
	    dq = dp;
	}
	if (BO_IS_FREE(dp))
	    SayDebug ("-");
	else
	    SayDebug ("X");
    }
    SayDebug ("|\n");

} /* end of PrintRegionMap */
#endif


/* BO_AllocRegion:
 *
 * Allocate a big object region that is large enough to hold an object of at
 * least reqSzB bytes.  The region is added to the heap's list of big-object
 * regions.  It returns the descriptor for the free big-object that
 * is the region.
 * NOTE: it does not mark the BIBOP entries for the region; this should be
 * done by the caller.
 */
bigobj_desc_t *BO_AllocRegion (heap_t *heap, Addr_t reqSzB)
{
    int		    npages, i;
    Addr_t	    hdrSzB, memObjSzB;
    bigobj_region_t *region;
    mem_obj_t	    *memObj;
    bigobj_desc_t   *desc;

    if (reqSzB < MIN_BOREGION_SZB) {
        reqSzB = MIN_BOREGION_SZB;
    }

    /* number of pages to hold requested size.  Since the first page does not
     * hold data, we need one extra page.
     */
    npages = (ROUNDUP(reqSzB, BIGOBJ_PAGE_SZB) >> BIGOBJ_PAGE_SHIFT) + 1;
    /* the amount of memory we need for the region */
    memObjSzB = ROUNDUP(npages*BIGOBJ_PAGE_SZB, BIBOP_PAGE_SZB);
    /* the actual number of big-object pages in the memory object (not counting
     * the first page.
     */
    ASSERT (npages <= (memObjSzB >> BIGOBJ_PAGE_SHIFT) - 1);
    npages = (memObjSzB >> BIGOBJ_PAGE_SHIFT) - 1;
    /* size of header for npages */
    hdrSzB = BOREGION_HDR_SZB(npages);

    /* allocate memory */
    if (((memObj = MEM_AllocMemObj (memObjSzB, TRUE)) == NIL(mem_obj_t *))
    ||  ((region = (bigobj_region_t *)MALLOC(hdrSzB)) == NIL(bigobj_region_t *)))
    {
	Die ("unable to allocate memory object for bigobject region");
    }

    if ((desc = NEW_OBJ(bigobj_desc_t)) == NIL(bigobj_desc_t *)) {
	Die ("unable to allocate big-object descriptor");
    }

  /* initialize the first page with a pointer to the region structure */
    GetRegionHdrPtr(MEMOBJ_BASE(memObj))[0] = region;

  /* initialize the region header */
    region->firstPage	= MEMOBJ_BASE(memObj) + BIGOBJ_PAGE_SZB;
    region->nPages	= npages;
    region->nFree	= npages;
    region->minGen	= MAX_NUM_GENS;
    region->memObj	= memObj;
    region->next	= heap->bigRegions;
    heap->bigRegions	= region;
    heap->numBORegions++;
    for (i = 0;  i < npages;  i++) {
	region->objMap[i] = desc;
    }

  /* initialize the descriptor for the region's memory */
    desc->obj		= region->firstPage;
    desc->sizeB		= npages * BIGOBJ_PAGE_SZB;
    desc->state		= BO_FREE;
    desc->region	= region;

#ifdef BO_DEBUG
SayDebug ("BO_AllocRegion: %d pages @ %p; desc = %p\n",
npages, (void *)(region->firstPage), desc);
#endif
    return desc;

} /* end of BO_AllocRegion */


/* BO_Alloc:
 *
 * Allocate a big object of the given size.
 */
bigobj_desc_t *BO_Alloc (heap_t *heap, int gen, Addr_t objSzB)
{
    bigobj_desc_t   *hdr, *dp, *newDesc;
    bigobj_region_t *region;
    Addr_t	    totSzB;
    int		    i, npages, firstPage;

    totSzB = ROUNDUP(objSzB, BIGOBJ_PAGE_SZB);
    npages = (totSzB >> BIGOBJ_PAGE_SHIFT);

  /* search for a free object that is big enough (first-fit) */
    hdr = heap->freeBigObjs;
    for (dp = hdr->next;  (dp != hdr) && (dp->sizeB < totSzB);  dp = dp->next) {
	continue;
    }

    if (dp == hdr) {
      /* no free object fits, so allocate a new region */
	dp = BO_AllocRegion (heap, totSzB);
	region = dp->region;
	if (dp->sizeB == totSzB) {
	  /* allocate the whole region to the object */
	    newDesc = dp;
	}
	else {
	  /* split the free object */
	    newDesc		= NEW_OBJ(bigobj_desc_t);
	    newDesc->obj	= dp->obj;
	    newDesc->region	= region;
	    dp->obj		= (Addr_t)(dp->obj) + totSzB;
	    dp->sizeB		-= totSzB;
	    AddBODesc(heap->freeBigObjs, dp);
	    firstPage		= ADDR_TO_BOPAGE(region, newDesc->obj);
	    for (i = 0;  i < npages;  i++) {
		region->objMap[firstPage+i] = newDesc;
	    }
	}
    }
    else if (dp->sizeB == totSzB) {
	RemoveBODesc(dp);
	newDesc = dp;
	region = dp->region;
    }
    else {
	ASSERT(totSzB < dp->sizeB);
      /* split the free object, leaving dp in the free list. */
	region		= dp->region;
	newDesc		= NEW_OBJ(bigobj_desc_t);
	newDesc->obj	= dp->obj;
	newDesc->region	= region;
	dp->obj		= (Addr_t)(dp->obj) + totSzB;
	dp->sizeB	-= totSzB;
	firstPage	= ADDR_TO_BOPAGE(region, newDesc->obj);
	for (i = 0;  i < npages;  i++) {
	    dp->region->objMap[firstPage+i] = newDesc;
	}
    }

    newDesc->sizeB	= objSzB;
    newDesc->state	= BO_YOUNG;
    newDesc->gen	= gen;
    region->nFree	-= npages;

    if (region->minGen > gen) {
        /* update the generation part of the descriptor */
        struct mem_obj_hdr *mObj = (struct mem_obj_hdr *)(region->memObj);
	region->minGen = gen;
	MarkRegion (BIBOP, (ml_val_t *)(mObj->base), mObj->sizeB, AID_BIGOBJ(gen));
	ADDR_TO_PAGEID(BIBOP, mObj->base) = AID_BIGOBJ_HDR(gen);
    }

#ifdef BO_DEBUG
SayDebug ("BO_Alloc: %d bytes @ %p\n", objSzB, (void *)(newDesc->obj));
PrintRegionMap(region);
#endif
    return newDesc;

} /* end of BO_Alloc */


/* BO_Free:
 *
 * Mark a big object as free and add it to the free list.
 */
void BO_Free (heap_t *heap, bigobj_desc_t *desc)
{
    bigobj_region_t *region = desc->region;
    bigobj_desc_t   *dp;
    int		    firstPage, lastPage, i, j;
    Addr_t	    totSzB = ROUNDUP(desc->sizeB, BIGOBJ_PAGE_SZB);

    firstPage = ADDR_TO_BOPAGE(region, desc->obj);
    lastPage = firstPage + (totSzB >> BIGOBJ_PAGE_SHIFT);

#ifdef BO_DEBUG
SayDebug ("BO_Free: @ %#x, bibop gen = %x, gen = %d, state = %d, pages=[%d..%d)\n",
desc->obj, (unsigned)EXTRACT_GEN(ADDR_TO_PAGEID(BIBOP, desc->obj)), desc->gen, desc->state,
firstPage, lastPage);
PrintRegionMap(region);
#endif
    if ((firstPage > 0) && BO_IS_FREE(region->objMap[firstPage-1])) {
      /* coalesce with adjacent free object */
	dp = region->objMap[firstPage-1];
	RemoveBODesc(dp);
	for (i = ADDR_TO_BOPAGE(region, dp->obj); i < firstPage;  i++)
	    region->objMap[i] = desc;
	desc->obj = dp->obj;
	totSzB += dp->sizeB;
	FREE (dp);
    }

    if ((lastPage < region->nPages) && BO_IS_FREE(region->objMap[lastPage])) {
      /* coalesce with adjacent free object */
	dp = region->objMap[lastPage];
	RemoveBODesc(dp);
	for (i = lastPage, j = i+(dp->sizeB >> BIGOBJ_PAGE_SHIFT); i < j;  i++)
	    region->objMap[i] = desc;
	totSzB += dp->sizeB;
	FREE (dp);
    }

    desc->sizeB = totSzB;
    desc->state = BO_FREE;

    region->nFree += (lastPage - firstPage);
    /** what if (region->nFree == region->nPages) ??? **/

  /* add desc to the free list */
    AddBODesc(heap->freeBigObjs, desc);

} /* end of BO_Free */


/* BO_GetDesc:
 *
 * Given an address into a big object, return the object's descriptor.
 */
bigobj_desc_t *BO_GetDesc (ml_val_t addr)
{
    bibop_t	    bibop = BIBOP;
    int		    indx;
    aid_t	    aid;
    bigobj_region_t *rp;

  /* find the beginning of the region containing the code object */
    indx = BIBOP_ADDR_TO_INDEX(addr);
    aid = INDEX_TO_PAGEID(bibop, indx);
    while (! BO_IS_HDR(aid)) {
	--indx;
	aid = INDEX_TO_PAGEID(bibop, indx);
    }

    rp = GetRegionHdrPtr(BIBOP_INDEX_TO_ADDR(indx))[0];

    return ADDR_TO_BODESC(rp, addr);

} /* end of BO_GetDesc */


/* BO_AddrToCodeObjTag:
 *
 * Return the tag of the code object containing the given PC (or else
 * NIL).
 */
Byte_t *BO_AddrToCodeObjTag (Word_t pc)
{
    bigobj_region_t	*rp;
    aid_t		aid;

    aid = ADDR_TO_PAGEID(BIBOP, pc);

    if (IS_BIGOBJ_AID(aid)) {
	int indx = BIBOP_ADDR_TO_INDEX(pc);
	while (!BO_IS_HDR(aid)) {
	    --indx;
	    aid = INDEX_TO_PAGEID(BIBOP,indx);
	    ASSERT(IS_BIGOBJ_AID(aid));
	}
        rp = GetRegionHdrPtr(BIBOP_INDEX_TO_ADDR(indx))[0];
	return BO_GetCodeObjTag (ADDR_TO_BODESC(rp, pc));
    }
    else {
	return NIL(Byte_t *);
    }

} /* end of BO_AddrToCodeObjTag */


/* BO_GetCodeObjTag:
 *
 * Return the tag of the given code object.  See
 *
 *	compiler/CodeGen/cpscompile/smlnj-pseudoOps.sml
 *
 * for details on the tag layout.
 */
Byte_t *BO_GetCodeObjTag (bigobj_desc_t *bdp)
{
    Byte_t		*lastByte;
    int			kx;

    lastByte = (Byte_t *)(bdp->obj) + bdp->sizeB - 1;
    kx = *lastByte * WORD_SZB;

    return lastByte - kx + 1;

} /* end of BO_GetCodeObjTag */

