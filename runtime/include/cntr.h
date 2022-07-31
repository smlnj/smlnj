/*! \file cntr.h
 *
 * Large counters for large (> 2^31) values.
 */

/*
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

#ifndef _CNTR_
#define _CNTR_

#define ONE_MILLION	1000000

typedef struct {
    Unsigned64_t	cnt;
} cntr_t;

#define CNTR_INCR(cp, i)	{		\
	cntr_t		*__cp = (cp);		\
	__cp->cnt += (i);			\
    }

#define CNTR_INCR1(cp)	{			\
	cntr_t		*__cp = (cp);		\
	__cp->cnt++;				\
    }

#define CNTR_ZERO(cp)		{		\
	(cp)->cnt = 0;				\
    }

#define CNTR_TO_REAL(cp)			\
    ((double)((cp)->cnt))

/* Add cp2 to cp1 */
#define CNTR_ADD(cp1, cp2)	{		\
	cntr_t		*__cp1 = (cp1);		\
	cntr_t		*__cp2 = (cp2);		\
	__cp1->cnt += __cp2->cnt;		\
    }

#define CNTR_PERCENT(cp1, cp2)	((100.0*CNTR_TO_REAL(cp1)) / CNTR_TO_REAL(cp2))

#define CNTR_FPRINTF(f,cp,wid)	{					\
	cntr_t	*__cp = (cp);						\
	int	__w = (wid);						\
	fprintf ((f), "%*llu", __w, (long long unsigned)(__cp->cnt));	\
    }

#endif /* !_CNTR_ */

