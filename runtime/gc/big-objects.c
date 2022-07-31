/*! \file big-objects.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Code for managing big-object regions.
 */

#include "ml-base.h"
#include "memory.h"
#include "heap.h"
#include "heap-monitor.h"
#include <string.h>

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
	    SayDebug ("_");
	else
	    SayDebug ("X");
    }
    SayDebug ("|\n");

} /* end of PrintRegionMap */
#endif


/* BO_AllocRegion:
 *
 * Allocate a big object region that is large enough to hold an object of at
 * least reqSzB bytes.  It returns the descriptor for the free big-object that
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

  /* Compute the memory-object size for the region.  A region consists of
   * a header followed by big-object pages.  The size of the header depends
   * on the number of pages and the number of pages should be rounded up
   * to fill the memory object, which will be a multiple of BIBOP_PAGE_SZB
   * bytes.  We use an iterative algorithm to determine the number of pages
   * and the size of the memory object.
   */
    {
	Addr_t szb;
	int hdrSlop, slop;
      /* minimum number of pages to hold requested size */
	npages = ROUNDUP(reqSzB, BIGOBJ_PAGE_SZB) >> BIGOBJ_PAGE_SHIFT;
      /* size of header for npages */
	szb = BOREGION_HDR_SZB(npages);
      /* round up to bigobject page size */
	hdrSzB = ROUNDUP(szb, BIGOBJ_PAGE_SZB);
      /* amount of slop in header (measured in per-page space cost) */
	hdrSlop = (hdrSzB - szb) / sizeof(bigobj_desc_t *);
      /* amount of memory needed to hold the header and pages */
	szb = hdrSzB + npages*BIGOBJ_PAGE_SZB;
      /* round up to BIBOP page size */
	memObjSzB = ROUNDUP(szb, BIBOP_PAGE_SZB);
      /* amount of slop in memory object (measured in per-page space cost) */
	slop = (memObjSzB - szb) / BIGOBJ_PAGE_SZB;
      /* while the page slop is bigger than the header slop, reallocate a page to the header */
	while (hdrSlop < slop) {
	    slop -= 1;
	    hdrSlop += BIGOBJ_PAGE_SZB / sizeof(bigobj_desc_t *);
	}
      /* we can increase the number of pages without increasing the rounded
       * size of the header.
       */
	npages += slop;
      /* recompute the header and request sizes based on the actual number of
       * big-object pages being allocated.
       */
	hdrSzB = ROUNDUP(BOREGION_HDR_SZB(npages), BIGOBJ_PAGE_SZB);
	reqSzB = npages * BIGOBJ_PAGE_SZB;
    }

    if ((memObj = MEM_AllocMemObj (memObjSzB, TRUE)) == NIL(mem_obj_t *)) {
	Die ("unable to allocate memory object for bigobject region");
    }
    region = (bigobj_region_t *)MEMOBJ_BASE(memObj);

    if ((desc = NEW_OBJ(bigobj_desc_t)) == NIL(bigobj_desc_t *)) {
	Die ("unable to allocate big-object descriptor");
    }

  /* initialize the region header */
    region->firstPage	= ((Addr_t)region + hdrSzB);
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
    desc->sizeB		= reqSzB;
    desc->state		= BO_FREE;
    desc->region	= region;

#ifdef BO_DEBUG
SayDebug ("BO_AllocRegion: %d pages @ %p\n", npages, (void *)(region->firstPage));
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
	region->minGen = gen;
	MarkRegion (BIBOP, (ml_val_t *)region, MEMOBJ_SZB(region->memObj),
	    AID_BIGOBJ(gen));
	ADDR_TO_PAGEID(BIBOP, region) = AID_BIGOBJ_HDR(gen);
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
    int		    i;
    aid_t	    aid;
    bigobj_region_t *rp;

  /* find the beginning of the region containing the code object */
    i = BIBOP_ADDR_TO_INDEX(addr);
    aid = INDEX_TO_PAGEID(bibop, i);
    while (! BO_IS_HDR(aid)) {
	--i;
	aid = INDEX_TO_PAGEID(bibop, i);
    }

    rp = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(i);

    return ADDR_TO_BODESC(rp, addr);

} /* end of BO_GetDesc */


/* BO_AddrToCodeObjTag:
 *
 * Return the tag of the code object containing the given PC (or else
 * NIL).
 */
Byte_t *BO_AddrToCodeObjTag (Word_t pc)
{
    bigobj_region_t	*region;
    aid_t		aid;

    aid = ADDR_TO_PAGEID(BIBOP, pc);

    if (IS_BIGOBJ_AID(aid)) {
	int indx = BIBOP_ADDR_TO_INDEX(pc);
	while (!BO_IS_HDR(aid)) {
	    --indx;
	    aid = INDEX_TO_PAGEID(BIBOP,indx);
	    ASSERT(IS_BIGOBJ_AID(aid));
	}
	region = (bigobj_region_t *)BIBOP_INDEX_TO_ADDR(indx);
	return BO_GetCodeObjTag (ADDR_TO_BODESC(region, pc));
    }
    else
	return NIL(Byte_t *);

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

