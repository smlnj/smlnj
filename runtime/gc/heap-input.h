/*! \file heap-input.h
 *
 * \author John Reppy
 */

/*
 * COPYRIGHT (c) 2025 The Fellowship of SML/NJ (https://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _HEAP_INPUT_
#define _HEAP_INPUT_

#include <stdio.h>

#ifndef _ADDR_HASH_
#include "addr-hash.h"
#endif

/* An input source for reading heap data.  This abstraction allows both reading
 * from memory and/or a file.  When loading a heap image from the file system,
 * the `nbytes` field will be 0 and all input will come from the file system.
 * For standalone executables, we read from memory, so the `file` field will be
 * `NULL`.  For reading blasted data, we might start from memory and then switch
 * to the file system.
 */
typedef struct {
    bool_t	needsSwap;	/* true, if the input bytes need to be swapped */
    FILE	*file;		/* the file descriptor to read from, once the */
				/* buffered characters are exhausted */
    Byte_t	*base;		/* the start of the buffered characters */
    Byte_t	*buf;		/* the current position in the buffer */
    size_t	nbytes;         /* the number of bytes remaining in the buffer */
} inbuf_t;

#define IS_MEMORY_INBUF(bp)     ((bp)->file == NULL)

/** Big-object relocation info **/

typedef struct {	/* big-object relocation info */
    Addr_t	    oldAddr;	/* address in imported heap */
    bigobj_desc_t   *newObj;	/* corresponding object in the new heap */
} bo_reloc_t;

typedef struct {	/* big-object region relocation info */
    Addr_t	    firstPage;	/* the address of the first page of the region */
    int		    nPages;	/* the number of big-object pages in the region */
    bo_reloc_t	    **objMap;   /* the map from pages to big-object relocation */
				/* info. */
} bo_region_reloc_t;

/* Big-object region hash table interface */
#define LookupBORegion(tbl, bibopIndex)	\
	((bo_region_reloc_t *)AddrTblLookup(tbl, BIBOP_INDEX_TO_ADDR(bibopIndex)))

/* Utility routines */
extern ml_val_t *HeapIO_ReadExterns (inbuf_t *bp);
extern status_t HeapIO_Seek (inbuf_t *bp, off_t offset);
extern status_t HeapIO_ReadBlock (inbuf_t *bp, void *blk, off_t len);

#endif /* !_HEAP_INPUT_ */
