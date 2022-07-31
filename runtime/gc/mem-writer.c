/*! \file mem-writer.c
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * An implementation of the abstract writers on top of memory regions.
 */

#include "ml-base.h"
#include "writer.h"
#include <string.h>

#ifndef BUFSIZ
#define BUFSIZ 4096
#endif

typedef struct buffer {
    Byte_t	*base;
    Byte_t	*next;
    Byte_t	*top;
} wr_buffer_t;

PVT void Put (writer_t *wr, Word_t w);
PVT void Write (writer_t *wr, const void *data, Addr_t nbytes);
PVT void Flush (writer_t *wr);
PVT off_t Tell (writer_t *wr);
PVT void Seek (writer_t *wr, off_t offset);
PVT void Free (writer_t *wr);

#define BufOf(wr)	((wr_buffer_t *)((wr)->data))

/* WR_OpenMem:
 *
 * Open a file for writing, and make a writer for it.
 */
writer_t *WR_OpenMem (Byte_t *data, Addr_t len)
{
    wr_buffer_t	*bp;
    writer_t	*wr;

    bp = NEW_OBJ(wr_buffer_t);
    bp->base	= data;
    bp->next	= data;
    bp->top	= (Byte_t *)(((Addr_t)data) + len);

    wr = NEW_OBJ(writer_t);
    wr->errFlg	= FALSE;
    wr->data	= (void *)bp;
    wr->putWord	= Put;
    wr->write	= Write;
    wr->flush	= Flush;
    wr->tell	= Tell;
    wr->seek	= Seek;
    wr->free	= Free;

    return wr;

} /* end of WR_OpenMem */

/* Put:
 */
PVT void Put (writer_t *wr, Word_t w)
{
    wr_buffer_t	*bp = BufOf(wr);

    ASSERT(bp->next+WORD_SZB <= bp->top);

    *((Word_t *)(bp->next)) = w;
    bp->next += WORD_SZB;

} /* end of Put */

/* Write:
 */
PVT void Write (writer_t *wr, const void *data, Addr_t nbytes)
{
    wr_buffer_t	*bp = BufOf(wr);

    if (wr->errFlg)
	return;

    ASSERT(bp->next+nbytes <= bp->top);

    memcpy (bp->next, data, nbytes);
    bp->next += nbytes;

} /* end of Write */

/* Flush:
 */
PVT void Flush (writer_t *wr)
{
    wr_buffer_t	*bp = BufOf(wr);

    ASSERT(bp->next <= bp->top);

} /* end of Flush */

/* Tell:
 */
PVT off_t Tell (writer_t *wr)
{
    Die ("Tell not supported on memory writers");

} /* end of Tell */

/* Seek:
 */
PVT void Seek (writer_t *wr, off_t offset)
{
    Die ("Tell not supported on memory writers");

} /* end of Seek */

/* Free:
 */
PVT void Free (writer_t *wr)
{
    wr_buffer_t	*bp = BufOf(wr);

    ASSERT(bp->next == bp->top);

    FREE (BufOf(wr));
    FREE (wr);

} /* end of Free */
