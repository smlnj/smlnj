/*! \file import-heap.c
 *
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Routines to import an ML heap image.
 */

#include <stdio.h>
#include <string.h>
#include "ml-base.h"
#include "machine-id.h"
#include "memory.h"
#include "cache-flush.h"
#include "ml-state.h"
#include "ml-values.h"
#include "ml-objects.h"
#include "ml-globals.h"
#include "card-map.h"
#include "heap.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "addr-hash.h"
#include "heap-input.h"
#include "heap-io.h"

#if defined(DLOPEN) && !defined(OPSYS_WIN32)
#include <dlfcn.h>
#endif

#ifdef DEBUG
PVT void PrintRelocMap (bo_region_reloc_t *r)
{
    bo_reloc_t		*dp, *dq;
    int			i;

    SayDebug ("region @%#x: |", r->firstPage);
    for (i = 0, dq = r->objMap[0];  i < r->nPages;  i++) {
	dp = r->objMap[i];
	if (dp != dq) {
	    SayDebug ("|");
	    dq = dp;
	}
	if (dp == NIL(bo_reloc_t *))
	    SayDebug ("_");
	else
	    SayDebug ("X");
    }
    SayDebug ("|\n");

} /* end of PrintRelocMap */
#endif


/* local routines */
PVT void ReadHeap (inbuf_t *bp, ml_heap_hdr_t *hdr, ml_state_t *msp, ml_val_t *externs);
PVT bigobj_desc_t *AllocBODesc (bigobj_desc_t *, bigobj_hdr_t *, bo_region_reloc_t *);
PVT void RepairHeap (
	heap_t *, bibop_t, Addr_t [MAX_NUM_GENS][NUM_ARENAS],
	addr_tbl_t *, ml_val_t *);
PVT ml_val_t RepairWord (
	ml_val_t w, bibop_t oldBIBOP, Addr_t addrOffset[MAX_NUM_GENS][NUM_ARENAS],
	addr_tbl_t *boRegionTbl, ml_val_t *externs);
PVT bo_reloc_t *AddrToRelocInfo (bibop_t, addr_tbl_t *, aid_t, Addr_t);

#define READ(bp,obj,msg)	                                        \
    do {                                                                \
        if (HeapIO_ReadBlock(bp, &(obj), sizeof(obj)) == FAILURE) {     \
            Die(msg);                                                   \
        }                                                               \
    } while (0)


/* ImportHeapImage:
 */
ml_state_t *ImportHeapImage (const char *fname, heap_params_t *params)
{
    ml_state_t		*msp;
    ml_image_hdr_t	imHdr;
    ml_heap_hdr_t	heapHdr;
    ml_val_t		*externs;
    ml_vproc_image_t	image;
    inbuf_t		inBuf;

    if (fname != NULL) {
      /* Resolve the name of the image.  If the file exists use it, otherwise try the
       * pathname with the machine ID as an extension.
       */
	if ((inBuf.file = fopen(fname, "rb")) != NULL) {
	    if (! SilentLoad) {
		Say("loading %s ", fname);
	    }
	}
	else {
	    char	buf[1024];

	    if (QualifyImageName(strcpy(buf, fname))
	    && ((inBuf.file = fopen(buf, "rb")) != NULL)) {
		if (! SilentLoad) {
		    Say("loading %s ", buf);
		}
	    }
	    else {
		Die ("unable to open heap image \"%s\"\n", fname);
	    }
	}

	inBuf.needsSwap = FALSE;
	inBuf.buf	    = NIL(Byte_t *);
	inBuf.nbytes    = 0;
    } else {
      /* fname == NULL, so try to find an in-core heap image */
#if defined(DLOPEN) && !defined(OPSYS_WIN32)
	void *lib = dlopen (NULL, RTLD_LAZY);
	void *vimg, *vimglenptr;
	if ((vimg = dlsym(lib, HEAP_IMAGE_SYMBOL)) == NULL) {
	    Die("no in-core heap image found\n");
	}
	if ((vimglenptr = dlsym(lib, HEAP_IMAGE_LEN_SYMBOL)) == NULL) {
	    Die("unable to find length of in-core heap image\n");
	}

	inBuf.file = NULL;
	inBuf.needsSwap = FALSE;
	inBuf.base = vimg;
	inBuf.buf = inBuf.base;
	inBuf.nbytes = *(long *)vimglenptr;
#else
      Die("in-core heap images not implemented\n");
#endif
    }

    READ(&inBuf, imHdr, "failure reading image header\n");
    if (imHdr.byteOrder != ORDER)
	Die ("incorrect byte order in heap image\n");
    if (imHdr.magic != IMAGE_MAGIC)
	Die ("bad magic number (%#x) in heap image\n", imHdr.magic);
    if ((imHdr.kind != EXPORT_HEAP_IMAGE) && (imHdr.kind != EXPORT_FN_IMAGE))
	Die ("bad image kind (%d) in heap image\n", imHdr.kind);
    READ(&inBuf, heapHdr, "failure reading heap header\n");

  /* check for command-line overrides of heap parameters. */
    if (params->allocSz == 0) params->allocSz = heapHdr.allocSzB;
    if (params->numGens < heapHdr.numGens) params->numGens = heapHdr.numGens;
    if (params->cacheGen < 0) params->cacheGen = heapHdr.cacheGen;

    msp = AllocMLState (FALSE, params);

  /* get the run-time pointers into the heap */
    *PTR_MLtoC(ml_val_t, PervStruct) = heapHdr.pervStruct;
    RunTimeCompUnit = heapHdr.runTimeCompUnit;
#ifdef ASM_MATH
    MathVec = heapHdr.mathVec;
#endif

  /* read the externals table */
    externs = HeapIO_ReadExterns (&inBuf);

  /* read and initialize the ML state info */
    READ(&inBuf, image, "failure reading vproc header");
    if (imHdr.kind == EXPORT_HEAP_IMAGE) {
      /* Load the live registers */
	ASSIGN(MLSignalHandler, image.sigHandler);
	msp->ml_arg		= image.stdArg;
	msp->ml_cont		= image.stdCont;
	msp->ml_closure		= image.stdClos;
	msp->ml_pc		= image.pc;
	msp->ml_exnCont		= image.exnCont;
	msp->ml_varReg		= image.varReg;
	msp->ml_calleeSave[0]	= image.calleeSave[0];
	msp->ml_calleeSave[1]	= image.calleeSave[1];
	msp->ml_calleeSave[2]	= image.calleeSave[2];
      /* read the ML heap */
	ReadHeap (&inBuf, &heapHdr, msp, externs);
      /* GC message are on by default for interactive images */
      /* GCMessages = TRUE; */
    }
    else {  /* EXPORT_FN_IMAGE */
	ml_val_t	funct, cmdName, args;
      /* restore the signal handler */
	ASSIGN(MLSignalHandler, image.sigHandler);
      /* read the ML heap */
	msp->ml_arg		= image.stdArg;
	ReadHeap (&inBuf, &heapHdr, msp, externs);
      /* initialize the calling context (taken from ApplyMLFn) */
	funct			= msp->ml_arg;
	msp->ml_exnCont		= PTR_CtoML(handle_v+1);
	msp->ml_varReg		= ML_unit;
	msp->ml_cont		= PTR_CtoML(return_c);
	msp->ml_closure		= funct;
	msp->ml_pc		=
	msp->ml_linkReg		= GET_CODE_ADDR(funct);
      /* setup the arguments to the imported function */
	cmdName = ML_CString(msp, MLCmdName);
	args = ML_CStringList (msp, CmdLineArgs);
	REC_ALLOC2(msp, msp->ml_arg, cmdName, args);
/*
SayDebug("arg = %#x : [%#x, %#x]\n", msp->ml_arg, REC_SEL(msp->ml_arg, 0), REC_SEL(msp->ml_arg, 1));
*/
      /* GC message are off by default for exportFn images */
	GCMessages = FALSE;
    }

    FREE (externs);
    if (inBuf.file != NULL)
      fclose (inBuf.file);

    if (! SilentLoad)
	Say(" done\n");

    return msp;

} /* end of ImportHeapImage */

/* ReadHeap:
 */
PVT void ReadHeap (inbuf_t *bp, ml_heap_hdr_t *hdr, ml_state_t *msp, ml_val_t *externs)
{
    heap_t		*heap = msp->ml_heap;
    heap_arena_hdr_t	*arenaHdrs, *p, *q;
    int			arenaHdrsSize;
    int			i, j, k;
    Addr_t		prevSzB[NUM_ARENAS], sz;
    bibop_t		oldBIBOP;
    Addr_t		addrOffset[MAX_NUM_GENS][NUM_ARENAS];
    bo_region_reloc_t	*boRelocInfo;
    addr_tbl_t		*boRegionTbl;

  /* Allocate a BIBOP for the imported heap image's address space. */
    oldBIBOP = InitBibop();

  /* read in the big-object region descriptors for the old address space */
    {
	size_t			sz;
	bo_region_info_t	*boRgnHdr;

	boRegionTbl = MakeAddrTbl(BIBOP_PAGE_BITS+1, hdr->numBORegions);
	sz = hdr->numBORegions * sizeof(bo_region_info_t);
	boRgnHdr = NEW_VEC(bo_region_info_t, hdr->numBORegions);
	if (HeapIO_ReadBlock (bp, boRgnHdr, sz) == FAILURE) {
            Die("failure to read big-object region info\n");
        }

#ifdef VERBOSE
	SayDebug ("Marking %d regions for imported big objects\n", hdr->numBORegions);
#endif
	boRelocInfo = NEW_VEC(bo_region_reloc_t, hdr->numBORegions);
	for (i = 0;  i < hdr->numBORegions;  i++) {
	  /* mark the big-object region as being in the `MAX_NUM_GENS` generation */
	    MarkRegion(oldBIBOP,
		(ml_val_t *)(boRgnHdr[i].baseAddr),
		RND_MEMOBJ_SZB(boRgnHdr[i].sizeB),
		AID_BIGOBJ(MAX_NUM_GENS));
	    ADDR_TO_PAGEID(oldBIBOP,boRgnHdr[i].baseAddr) = AID_BIGOBJ_HDR(MAX_NUM_GENS);
	  /* set relocation info for the big-object region */
	    boRelocInfo[i].firstPage = boRgnHdr[i].firstPage;
	    boRelocInfo[i].nPages =
		(boRgnHdr[i].sizeB - (boRgnHdr[i].firstPage - boRgnHdr[i].baseAddr))
		    >> BIGOBJ_PAGE_SHIFT;
	    boRelocInfo[i].objMap = NEW_VEC(bo_reloc_t *, boRelocInfo[i].nPages);
	    for (j = 0;  j < boRelocInfo[i].nPages;  j++) {
		boRelocInfo[i].objMap[j] = NIL(bo_reloc_t *);
	    }
	    AddrTblInsert (boRegionTbl, boRgnHdr[i].baseAddr, &(boRelocInfo[i]));
	}
	FREE (boRgnHdr);
    }

  /* read the arena headers. */
    arenaHdrsSize = hdr->numGens * NUM_OBJ_KINDS * sizeof(heap_arena_hdr_t);
    arenaHdrs = (heap_arena_hdr_t *) MALLOC (arenaHdrsSize);
    if (HeapIO_ReadBlock (bp, arenaHdrs, arenaHdrsSize) == FAILURE) {
        Die("failure to read arena info\n");
    }

    for (i = 0;  i < NUM_ARENAS;  i++)
	prevSzB[i] = heap->allocSzB;

  /* allocate the arenas and read in the heap image. */
    for (p = arenaHdrs, i = 0;  i < hdr->numGens;  i++) {
	gen_t	*gen = heap->gen[i];

      /* compute the space required for this generation, and mark the oldBIBOP
       * to reflect the old address space.
       */
	for (q = p, j = 0;  j < NUM_ARENAS;  j++) {
	    MarkRegion (oldBIBOP,
		(ml_val_t *)(q->info.o.baseAddr),
		RND_MEMOBJ_SZB(q->info.o.sizeB),
		gen->arena[j]->id);
	    sz = q->info.o.sizeB + prevSzB[j];
	    if ((j == PAIR_INDX) && (sz > 0))
		sz += 2*WORD_SZB;
	    gen->arena[j]->tospSizeB = RND_MEMOBJ_SZB(sz);
	    prevSzB[j] = q->info.o.sizeB;
	    q++;
	}

      /* Allocate space for the generation */
	if (NewGeneration(gen) == FAILURE)
	    Die ("unable to allocated space for generation %d\n", i+1);
	if (isACTIVE(gen->arena[ARRAY_INDX]))
	    NewDirtyVector (gen);

      /* read in the arenas for this generation and initialize the
       * address offset table.
       */
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t		*ap = gen->arena[j];

	    if (p->info.o.sizeB > 0) {
		addrOffset[i][j] = (Addr_t)(ap->tospBase) - (Addr_t)(p->info.o.baseAddr);
		HeapIO_Seek (bp, (off_t)(p->offset));
                if (HeapIO_ReadBlock(bp, (ap->tospBase), p->info.o.sizeB) == FAILURE) {
                    Die("failure to read heap data; gen = %d, arena = %d\n", i+1, j);
                }
		ap->nextw	= (ml_val_t *)((Addr_t)(ap->tospBase) + p->info.o.sizeB);
		ap->oldTop	= ap->tospBase;
	    }
	    else if (isACTIVE(ap)) {
		ap->oldTop = ap->tospBase;
	    }
	    if (! SilentLoad) {
		Say(".");
	    }
	    p++;
	}
      /* read in the big-object arenas */
	for (j = 0;  j < NUM_BIGOBJ_KINDS;  j++) {
	    Addr_t		totSizeB;
	    bigobj_desc_t	*freeObj, *bdp;
	    bigobj_region_t	*freeRegion;
	    bigobj_hdr_t	*boHdrs;
	    int			boHdrSizeB;
	    Addr_t		indx;
	    bo_region_reloc_t   *region;

	    if (p->info.bo.numBOPages > 0) {
		ENABLE_CODE_WRITE
		totSizeB = p->info.bo.numBOPages << BIGOBJ_PAGE_SHIFT;
		freeObj = BO_AllocRegion (heap, totSizeB);
		freeRegion = freeObj->region;
		freeRegion->minGen = i;
		MarkRegion (BIBOP, (ml_val_t *)freeRegion,
		    MEMOBJ_SZB(freeRegion->memObj), AID_BIGOBJ(i));
		ADDR_TO_PAGEID(BIBOP,freeRegion) = AID_BIGOBJ_HDR(i);

	      /* read in the big-object headers */
		boHdrSizeB = p->info.bo.numBigObjs * sizeof(bigobj_hdr_t);
		boHdrs = (bigobj_hdr_t *) MALLOC (boHdrSizeB);
                if (HeapIO_ReadBlock (bp, boHdrs, boHdrSizeB) == FAILURE) {
                    Die("failure to read big-object headers\n");
                }

	      /* read in the big-objects */
		if (HeapIO_ReadBlock (bp, (void *)(freeObj->obj), totSizeB) == FAILURE) {
                    Die("failure to read big-object data\n");
                }
		if (j == CODE_INDX) {
		    FlushICache ((void *)(freeObj->obj), totSizeB);
		}

	      /* setup the big-object descriptors and per-object relocation info */
		bdp = freeObj;
		for (k = 0;  k < p->info.bo.numBigObjs;  k++) {
		  /* find the region relocation info for the object's region in
		   * the exported heap.
		   */
		    for (indx = BIBOP_ADDR_TO_INDEX(boHdrs[k].baseAddr);
			!BO_IS_HDR(INDEX_TO_PAGEID(oldBIBOP,indx));
			--indx)
		    {
			continue;
		    }
		    region = LookupBORegion (boRegionTbl, indx);
		    ASSERT(region != NIL(bo_region_reloc_t *));
		  /* allocate the big-object descriptor for the object, and
		   * link it into the list of big-objects for its generation.
		   */
		    bdp = AllocBODesc (freeObj, &(boHdrs[k]), region);
		    bdp->next = gen->bigObjs[j];
		    gen->bigObjs[j] = bdp;
		    ASSERT(bdp->gen == i+1);

		    if (DumpObjectStrings && (j == CODE_INDX)) {
		      /* dump the comment string of the code object */
			char           *namestring;
			if ((namestring = (char *)BO_GetCodeObjTag(bdp)) != NIL(char *))
			    SayDebug ("[%6d bytes] %s\n", bdp->sizeB, namestring);
		    }
		}

		if (freeObj != bdp) {
		  /* there was some extra space left in the region */
		    AddBODesc (heap->freeBigObjs, freeObj);
		}

		FREE (boHdrs);
		DISABLE_CODE_WRITE
	    }
	    if (! SilentLoad) {
		Say(".");
	    }
	    p++;
	}
    }

    RepairHeap (heap, oldBIBOP, addrOffset, boRegionTbl, externs);

#ifdef CHECK_HEAP
    SayDebug ("Checking imported heap...\n");
    CheckHeap (heap, hdr->numGens);
#endif

  /* Adjust the run-time globals that point into the heap */
    *PTR_MLtoC(ml_val_t, PervStruct) = RepairWord (
	*PTR_MLtoC(ml_val_t, PervStruct),
	oldBIBOP, addrOffset, boRegionTbl, externs);
    RunTimeCompUnit = RepairWord (
	RunTimeCompUnit, oldBIBOP, addrOffset, boRegionTbl, externs);
#ifdef ASM_MATH
    MathVec = RepairWord (MathVec, oldBIBOP, addrOffset, boRegionTbl, externs);
#endif

  /* Adjust the ML registers to the new address space */
    ASSIGN(MLSignalHandler, RepairWord (
	DEREF(MLSignalHandler), oldBIBOP, addrOffset, boRegionTbl, externs));
    msp->ml_arg = RepairWord (
        msp->ml_arg, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_cont = RepairWord (
        msp->ml_cont, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_closure = RepairWord (
        msp->ml_closure, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_pc = RepairWord (
        msp->ml_pc, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_linkReg = RepairWord (
        msp->ml_linkReg, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_exnCont = RepairWord (
        msp->ml_exnCont, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_varReg = RepairWord (
        msp->ml_varReg, oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_calleeSave[0] = RepairWord (
        msp->ml_calleeSave[0], oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_calleeSave[1] = RepairWord (
        msp->ml_calleeSave[1], oldBIBOP, addrOffset, boRegionTbl, externs);
    msp->ml_calleeSave[2] = RepairWord (
        msp->ml_calleeSave[2], oldBIBOP, addrOffset, boRegionTbl, externs);

  /* release storage */
    for (i = 0; i < hdr->numBORegions;  i++) {
	bo_reloc_t	*p = NIL(bo_reloc_t *);
	int 		nPages = boRelocInfo[i].nPages;
	for (j = 0;  j < nPages;  j++) {
	    if ((boRelocInfo[i].objMap[j] != NIL(bo_reloc_t *))
	    && (boRelocInfo[i].objMap[j] != p)) {
		p = boRelocInfo[i].objMap[j];
	      /* skip over all entries that map to `p` */
		while ((j < nPages) && (boRelocInfo[i].objMap[j] == p)) {
		    j++;
		}
		FREE (p);
	    }
	}
    }
    for (i = 0; i < hdr->numBORegions;  i++) {
        if (boRelocInfo[i].objMap != NIL(bo_reloc_t **)) {
            FREE (boRelocInfo[i].objMap);
        }
    }
    FreeAddrTbl (boRegionTbl, FALSE);
    FREE (boRelocInfo);
    FREE (arenaHdrs);
    FreeBibop (oldBIBOP);

  /* reset the sweep_nextw pointers */
    for (i = 0;  i < heap->numGens;  i++) {
	gen_t	*gen = heap->gen[i];
	for (j = 0;  j < NUM_ARENAS;  j++) {
	    arena_t		*ap = gen->arena[j];
	    if (isACTIVE(ap))
		ap->sweep_nextw = ap->nextw;
	}
    }

} /* end of ReadHeap. */

/* AllocBODesc:
 *
 */
PVT bigobj_desc_t *AllocBODesc (
    bigobj_desc_t   *free,
    bigobj_hdr_t    *objHdr,
    bo_region_reloc_t *oldRegion)
{
    bigobj_region_t *region;
    bigobj_desc_t   *newObj;
    bo_reloc_t	    *relocInfo;
    int	            i, firstPage, npages;
    Addr_t          totSzB;

    totSzB = ROUNDUP(objHdr->sizeB, BIGOBJ_PAGE_SZB);
    npages = (totSzB >> BIGOBJ_PAGE_SHIFT);
    region = free->region;
    if (free->sizeB == totSzB) {
      /* allocate the whole free area to the object */
	newObj = free;
    }
    else {
      /* split the free object */
	newObj		= NEW_OBJ(bigobj_desc_t);
	newObj->obj	= free->obj;
	newObj->region	= region;
	free->obj	= (Addr_t)(free->obj) + totSzB;
	free->sizeB	-= totSzB;
      /* update region's big-object mapping for the new object */
	firstPage	= ADDR_TO_BOPAGE(region, newObj->obj);
        ASSERT(firstPage + npages <= region->nPages);
	for (i = 0;  i < npages;  i++) {
	    region->objMap[firstPage+i] = newObj;
	}
    }

    newObj->sizeB	= objHdr->sizeB;
    newObj->state	= BO_YOUNG;
    newObj->gen		= objHdr->gen;
    newObj->objc	= objHdr->objKind;
    region->nFree	-= npages;

  /* setup the relocation info */
    relocInfo = NEW_OBJ(bo_reloc_t);
    relocInfo->oldAddr = objHdr->baseAddr;
    relocInfo->newObj = newObj;
    firstPage = ADDR_TO_BOPAGE(oldRegion, objHdr->baseAddr);
    ASSERT(firstPage + npages <= oldRegion->nPages);
    for (i = 0;  i < npages;  i++) {
	oldRegion->objMap[firstPage+i] = relocInfo;
    }

    return newObj;

} /* end of AllocBODesc */

/* RepairHeap:
 *
 * Scan the heap, replacing external references with their addresses and
 * adjusting pointers.
 */
PVT void RepairHeap (
    heap_t *heap,
    bibop_t oldBIBOP,
    Addr_t addrOffset[MAX_NUM_GENS][NUM_ARENAS],
    addr_tbl_t *boRegionTbl,
    ml_val_t *externs)
{
    int		i;

    for (i = 0;  i < heap->numGens;  i++) {
	gen_t	*gen = heap->gen[i];
#ifndef BIT_CARDS
#define MARK(cm, p, g)	MARK_CARD(cm, p, g)
#else
#define MARK(cm, p, g)	MARK_CARD(cm, p)
#endif
#define RepairArena(indx)	{						\
	    arena_t		*__ap = gen->arena[(indx)];			\
	    ml_val_t	*__p, *__q;						\
	    __p = __ap->tospBase;						\
	    __q = __ap->nextw;							\
	    while (__p < __q) {							\
		ml_val_t	__w = *__p;					\
		int		__gg, __objc;					\
		if (isBOXED(__w)) {						\
		    Addr_t	__obj = PTR_MLtoADDR(__w);			\
		    aid_t	__aid = ADDR_TO_PAGEID(oldBIBOP, __obj);	\
		    if (IS_BIGOBJ_AID(__aid)) {					\
			bo_reloc_t	*__dp;					\
			__dp = AddrToRelocInfo (oldBIBOP, boRegionTbl,		\
				__aid, __obj);					\
			*__p = PTR_CtoML((__obj - __dp->oldAddr) 		\
				+ __dp->newObj->obj);				\
			__gg = __dp->newObj->gen-1;				\
		    }								\
		    else {							\
			__gg = EXTRACT_GEN(__aid)-1;				\
			__objc = EXTRACT_OBJC(__aid)-1;				\
			*__p = PTR_CtoML(__obj + addrOffset[__gg][__objc]);	\
		    }								\
		    if (((indx) == ARRAY_INDX) && (__gg < i)) {			\
			MARK(gen->dirty, __p, __gg+1);	/** **/			\
		    }								\
		}								\
		else if (isEXTERNTAG(__w)) {					\
		    *__p = externs[EXTERNID(__w)];				\
		}								\
		__p++;								\
	    }									\
	} /* RepairArena */

	RepairArena(RECORD_INDX);
	RepairArena(PAIR_INDX);
	RepairArena(ARRAY_INDX);
    }

} /* end of RepairHeap */

/* RepairWord:
 */
PVT ml_val_t RepairWord (
    ml_val_t w,
    bibop_t oldBIBOP,
    Addr_t addrOffset[MAX_NUM_GENS][NUM_ARENAS],
    addr_tbl_t *boRegionTbl,
    ml_val_t *externs)
{
    if (isBOXED(w)) {
	Addr_t	obj = PTR_MLtoADDR(w);
	aid_t	aid = ADDR_TO_PAGEID(oldBIBOP, obj);
	if (IS_BIGOBJ_AID(aid)) {
	    bo_reloc_t	*dp;
	    dp = AddrToRelocInfo (oldBIBOP, boRegionTbl, aid, obj);
	    return PTR_CtoML((obj - dp->oldAddr) + dp->newObj->obj);
	}
	else {
	    int	g = EXTRACT_GEN(aid)-1;
	    int	objc = EXTRACT_OBJC(aid)-1;
	    return PTR_CtoML(PTR_MLtoC(char, w) + addrOffset[g][objc]);
	}
    }
    else if (isEXTERNTAG(w)) {
	return externs[EXTERNID(w)];
    }
    else
	return w;

} /* end of RepairWord */


/* AddrToRelocInfo:
 */
PVT bo_reloc_t *AddrToRelocInfo (
    bibop_t oldBIBOP,
    addr_tbl_t *boRegionTbl,
    aid_t id,
    Addr_t oldObj)
{
    Addr_t		indx;
    bo_region_reloc_t	*region;

    indx = BIBOP_ADDR_TO_INDEX(oldObj);
    while (!BO_IS_HDR(id)) {
	--indx;
	id = INDEX_TO_PAGEID(oldBIBOP,indx);
    }

  /* find the old region descriptor */
    region = LookupBORegion (boRegionTbl, indx);

    if (region == NIL(bo_region_reloc_t *))
	Die ("unable to map big-object @ %#x; index = %#x, id = %#x\n",
	    oldObj, indx, (unsigned)id);

    return ADDR_TO_BODESC(region, oldObj);

} /* end of AddrToRelocInfo */
