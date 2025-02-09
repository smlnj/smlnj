/*! \file heap-in-util.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * Utility routines to import an ML heap image.
 */

#include "ml-base.h"
#include "heap.h"
#include "ml-values.h"
#include "ml-heap-image.h"
#include "c-globals-tbl.h"
#include "heap-input.h"
#include <string.h>

#ifndef SEEK_SET
#  define SEEK_SET	0
#  define SEEK_END	2
#endif

/* local routines */
PVT status_t ReadBlock (FILE *file, void *blk, size_t len);


/* HeapIO_ReadExterns:
 */
ml_val_t *HeapIO_ReadExterns (inbuf_t *bp)
{
    extern_tbl_hdr_t	hdr;
    ml_val_t		*externs;
    Byte_t		*buf, *cp;
    int			i;

  /* Read the header */
    HeapIO_ReadBlock(bp, &(hdr), sizeof(hdr));

    externs = NEW_VEC(ml_val_t, hdr.numExterns);

  /* Read in the names of the exported symbols */
    buf = NEW_VEC(Byte_t, hdr.externSzB);
    HeapIO_ReadBlock (bp, buf, hdr.externSzB);

  /* map the names of the external symbols to addresses in the run-time system */
    for (cp = buf, i = 0;  i < hdr.numExterns;  i++) {
	if ((externs[i] = ImportCSymbol ((char *)cp)) == ML_unit) {
	    Die ("Run-time system does not provide \"%s\"", cp);
	}
	cp += (strlen((char *)cp) + 1);
    }
    FREE (buf);

    return externs;

} /* end of HeapIO_ReadExterns */


/* HeapIO_Seek:
 *
 * Adjust the next character position to the given position in the
 * input stream.
 */
status_t HeapIO_Seek (inbuf_t *bp, off_t offset)
{
    if (bp->file == NULL) {
      /* the stream is in-memory */
	Byte_t	*newPos = bp->base + offset;
	if (bp->buf + bp->nbytes <= newPos) {
	    return FAILURE;
	}
	else {
	    bp->nbytes -= (newPos - bp->buf);
	    bp->buf = newPos;
	    return SUCCESS;
	}
    }
    else if (fseek (bp->file, offset, SEEK_SET) != 0) {
	Die ("unable to seek on heap image\n");
    }

    bp->nbytes = 0;		/* just in case? */

    return SUCCESS;

} /* end of HeapIO_Seek */


/* HeapIO_ReadBlock:
 */
status_t HeapIO_ReadBlock (inbuf_t *bp, void *blk, off_t len)
{
    status_t	sts = SUCCESS;

  /* FIXME: Why is len signed? Should the API use size_t */
    if (bp->nbytes >= (size_t)len) {
	memcpy (blk, bp->buf, len);
	bp->nbytes -= len;
	bp->buf += len;
    }
    else if (bp->file != NULL) {
	memcpy (blk, bp->buf, bp->nbytes);
	sts = ReadBlock (bp->file, ((Byte_t *)blk) + bp->nbytes, len - bp->nbytes);
	bp->nbytes = 0;
    }
    else {
        Error ("missing data in memory blast object");
        return FAILURE;
    }

    if (bp->needsSwap) {
	Die ("byte-swapping not implemented yet");
    }

    return sts;

} /* end of HeapIO_ReadBlock */

/* ReadBlock:
 */
PVT status_t ReadBlock (FILE *file, void *blk, size_t len)
{
    size_t	sts;
    Byte_t	*bp = (Byte_t *)blk;

    while (len > 0) {
	sts = fread (bp, 1, len, file);
	len -= sts;
	bp += sts;
	if ((sts < len) && (ferror(file) || feof(file))) {
	    Error ("unable to read %d bytes from image\n", len);
	    return FAILURE;
	}
    }

    return SUCCESS;

} /* end of ReadBlock. */
