/* gen-unix-signals.h
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 */

typedef struct {
    int		    sig;	/* the UNIX signal code */
    char	    *sigName;	/* the symbolic name of the signal (i.e., */
				/* the #define name). */
    char	    *shortName;	/* the short name of the signal passed to ML */
} sig_desc_t;

typedef struct {
    sig_desc_t	    **sigs;	/* an ordered vector of signal descriptions */
    int		    numSysSigs;	/* the number of system signals */
    int		    numRunSigs; /* the number of runtime signals */
    int		    minSysSig;	/* the minimum system signal number. */
    int		    maxSysSig;	/* the maximum system signal number. */
} sig_info_t;

extern sig_info_t *SortSignalTbl ();

