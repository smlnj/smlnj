/*! \file bin-file.h
 *
 * COPYRIGHT (c) 2021 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * The layout is:
 *   header
 *   import PerIDs (16 bytes each)
 *   export PerIDs (16 bytes each)
 *   CM dependency information
 *   inlinable lambda expression
 *   reserved area 1 (typically empty)
 *   reserved area 2 (typically empty)
 *   code objects
 *     This section contains a sequence of code objects, each of
 *     which is lead by its size.  The individual sizes must sum up to
 *     codeSzB.
 *   pickled static environment
 */

#ifndef _BIN_FILE_
#define _BIN_FILE_

#ifndef _ML_BASE_
#include "ml-base.h"
#endif

/** Persistent IDs **/
#define PERID_LEN	16

typedef struct {	    /* a persistent ID (PerID) */
    Byte_t	bytes[PERID_LEN];
} pers_id_t;

/* the layout of an old binfile */
typedef struct {	    /* The header of a .bin file; note that the fields */
			    /* are in big-endian representation. */
    Byte_t	magic[16];	/* magic number */
    Int32_t	importCnt;	/* the number of imported PerIDs. */
    Int32_t	exportCnt;	/* the number of exported PerIDs. */
    Int32_t	importSzB;	/* size of import tree area */
    Int32_t	cmInfoSzB;	/* the size of the CM dependency information area */
    Int32_t	lambdaSzB;	/* the size of inlinable lambda expressions */
    Int32_t	guidSzB;	/* size of GUID area in bytes */
    Int32_t	pad;	        /* size of padding for code segment alignment */
    Int32_t	codeSzB;	/* the number of bytes of code */
    Int32_t	envSzB;		/* the size of the environment */
} old_binfile_hdr_t;

/* current binfile version ID */
#define BINFILE_VERSION 0x20211123

/* the layout of the new binfile format */
typedef struct {	    /* The header of a .bin file; note that the fields */
			    /* are in big-endian representation. */
    Byte_t      fileKind[8];    /* should be the string "BinFile " */
    Int32_t     version;        /* bin-file format version */
    Byte_t      arch[12];       /* architecture as a space padded string */
    Byte_t      smlnjVersion[12]; /* SML/NJ version number as a space-padded string */
    Int32_t	importCnt;	/* the number of imported PerIDs. */
    Int32_t	exportCnt;	/* the number of exported PerIDs. */
    Int32_t	importSzB;	/* size of import tree area */
    Int32_t	cmInfoSzB;	/* the size of the CM dependency information area */
    Int32_t	guidSzB;	/* size of GUID area in bytes */
    Int32_t	pad;	        /* size of padding for code segment alignment */
    Int32_t	codeSzB;	/* the number of bytes of code */
    Int32_t	envSzB;		/* the size of the environment */
} new_binfile_hdr_t;

/* the useful information contained in the header */
typedef struct {
    Int32_t     version;        /* binfile version; will be 0 for old format files */
    int         hdrSzB;         /* the size of the binfile header in bytes */
    int	        importCnt;	/* the number of imported PerIDs. */
    int	   	exportCnt;	/* the number of exported PerIDs. */
    int	   	importSzB;	/* size of import tree area */
    int	   	cmInfoSzB;	/* the size of the CM dependency information area */
    int 	guidSzB;	/* size of GUID area in bytes */
    int 	pad;	        /* size of padding for code segment alignment */
    int	   	codeSzB;	/* the number of bytes of code */
    int	   	envSzB;		/* the size of the environment */
} binfile_hdr_info_t;

#endif /* !_BIN_FILE_ */
